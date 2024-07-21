-- | provides the @cabal-audit@ plugin which works as follows:
--
-- 1. parse command line arguments to pass on to cabal to build
--    an install plan and parse the advisories database
-- 2. lookup all dependencies in the elaborated plan within the
--    database
-- 3. summarise the found vulnerabilities as a humand readable or
--    otherwise formatted output
module Distribution.Audit (auditMain, buildAdvisories, AuditConfig (..), AuditException (..)) where

import Colourista.Pure (blue, bold, formatWith, green, red, yellow)
import Control.Algebra (Has)
import Control.Carrier.Lift (runM)
import Control.Effect.Pretty (Pretty, PrettyC, pretty, prettyStdErr, runPretty)
import Control.Exception (Exception (displayException), SomeException (SomeException))
import Control.Monad (when)
import Control.Monad.Codensity (Codensity (runCodensity))
import Data.Aeson (KeyValue ((.=)), Value, object)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Distribution.Audit.Parser (AuditConfig (..), OutputFormat (..), auditCommandParser)
import Distribution.Client.NixStyleOptions (NixStyleFlags)
import Distribution.Client.ProjectConfig (ProjectConfig)
import Distribution.Client.ProjectOrchestration
  ( CurrentCommand (OtherCommand)
  , ProjectBaseContext (ProjectBaseContext, cabalDirLayout, distDirLayout, localPackages, projectConfig)
  , commandLineFlagsToProjectConfig
  , establishProjectBaseContext
  )
import Distribution.Client.ProjectPlanning (rebuildInstallPlan)
import Distribution.Client.Setup (defaultGlobalFlags)
import Distribution.Client.Types (PackageSpecifier (SpecificSourcePackage), UnresolvedSourcePackage)
import Distribution.Solver.Modular.Package (PackageIdentifier (pkgName), PackageName, unPackageName)
import Distribution.Solver.Types.SourcePackage (srcpkgDescription, srcpkgPackageId)
import Distribution.Types.BuildInfo (targetBuildDepends)
import Distribution.Types.CondTree (condTreeData)
import Distribution.Types.Dependency (Dependency (Dependency))
import Distribution.Types.GenericPackageDescription (condLibrary)
import Distribution.Types.Library (libBuildInfo)
import Distribution.Verbosity qualified as Verbosity
import Distribution.Version (Version)
import GHC.Generics (Generic)
import Options.Applicative (customExecParser, fullDesc, header, helper, info, prefs, progDesc, showHelpOnEmpty)
import Security.Advisories (Advisory (..), Keyword (..), ParseAdvisoryError (..), printHsecId)
import Security.Advisories.Cabal (ElaboratedPackageInfoAdvised, ElaboratedPackageInfoWith (..), installPlanToLookupTable, matchAdvisoriesForPlan, toMapOn)
import Security.Advisories.Convert.OSV qualified as OSV
import Security.Advisories.Filesystem (listAdvisories)
import Security.Advisories.SBom.Types (prettyVersion, prettyVersionRange)
import System.Exit (exitFailure)
import System.IO (Handle)
import System.Process (callProcess)
import UnliftIO (MonadIO (..), MonadUnliftIO (..), catch, throwIO, withSystemTempDirectory)
import Validation (validation)

pwetty :: Has (Pretty [Text]) sig m => Handle -> Vector ([Text], Text) -> m ()
pwetty = pretty

owo :: Has (Pretty [Text]) sig m => Vector ([Text], Text) -> m ()
owo = prettyStdErr

data AuditException
  = -- | parsing the advisory database failed
    ListAdvisoryValidationError {originalFilePath :: FilePath, parseError :: [ParseAdvisoryError]}
  | -- | to rethrow exceptions thrown by cabal during plan elaboration
    CabalException {reason :: String, cabalException :: SomeException}
  | -- | a library in the package with name 'packageName' is not present
    LibraryNotPresent {packageName :: String}
  deriving stock (Show, Generic)

instance Exception AuditException where
  displayException = \case
    ListAdvisoryValidationError dir errs ->
      mconcat
        [ "Listing the advisories in directory "
        , dir
        , " failed with: \n"
        , mconcat $ displayException <$> errs
        ]
    CabalException ctx (SomeException ex) ->
      "cabal failed while " <> ctx <> ":\n" <> displayException ex
    LibraryNotPresent packageName ->
      "library of package \"" <> packageName <> "\" is not present"

-- | gathers all dependencies of a library in a given package. Does not support sublibraries yet
packageLibraryDepends :: MonadIO m => [PackageSpecifier UnresolvedSourcePackage] -> String -> m [Dependency]
packageLibraryDepends specifiers packageName = do
  let p pkg = do
        SpecificSourcePackage spkg <- pure pkg
        when (unPackageName spkg.srcpkgPackageId.pkgName /= packageName) Nothing
        pure spkg
      buildInfo
        | [pkg] <- mapMaybe p specifiers
        , Just lib <- pkg.srcpkgDescription.condLibrary =
            pure lib.condTreeData.libBuildInfo.targetBuildDepends
        | otherwise = throwIO LibraryNotPresent {packageName}
  buildInfo

-- | the main action to invoke
auditMain :: IO ()
auditMain = do
  (auditConfig, nixStyleFlags) <- customExecParser (prefs showHelpOnEmpty) do
    info (helper <*> auditCommandParser) do
      mconcat
        [ fullDesc
        , progDesc "audit your cabal projects for vulnerabilities"
        , header "Welcome to cabal audit"
        ]
  let interpPretty :: forall m a. PrettyC [Text] m a -> m a
      interpPretty = if auditConfig.noColour then runPretty (const id) else runPretty formatWith

  runM $ interpPretty do
    advisories <-
      ( do
          advisories <- buildAdvisories auditConfig nixStyleFlags
          handleBuiltAdvisories auditConfig.outputHandle auditConfig.outputFormat advisories
          pure advisories
        )
        `catch` \(SomeException ex) -> do
          owo
            [ ([red, bold], "cabal-audit failed :\n")
            , ([red], T.pack $ displayException ex)
            ]
          liftIO exitFailure
    when (auditConfig.failOnWarning && not (null advisories)) $ do
      owo
        [([red], T.pack (show (length advisories)) <> T.pack " advisories found.")]
      liftIO exitFailure

buildAdvisories
  :: (MonadUnliftIO m, Has (Pretty [Text]) sig m)
  => AuditConfig
  -> NixStyleFlags ()
  -> m (M.Map PackageName ElaboratedPackageInfoAdvised)
buildAdvisories MkAuditConfig {advisoriesPathOrURL, verbosity, library} flags = do
  let cliConfig = projectConfigFromFlags flags

  when (verbosity > Verbosity.normal) do
    owo [([blue], "Establishing project base context")]

  ProjectBaseContext {distDirLayout, cabalDirLayout, projectConfig, localPackages} <-
    liftIO do
      establishProjectBaseContext verbosity cliConfig OtherCommand
        `catch` \ex ->
          throwIO $ CabalException {reason = "trying to establish project base context", cabalException = ex}

  when (verbosity > Verbosity.normal) do
    owo [([blue], "...done\nQuerying advisory database")]

  advisories <- do
    let k realPath =
          listAdvisories realPath
            >>= validation (throwIO . ListAdvisoryValidationError realPath) pure
    case advisoriesPathOrURL of
      Left fp -> k fp
      Right url -> withSystemTempDirectory "cabal-audit" \tmp -> do
        owo [([blue], "trying to clone " <> T.pack url)]
        liftIO $ callProcess "git" ["clone", "--depth", "1", url, tmp]
        k tmp

  when (verbosity > Verbosity.normal) do
    owo [([blue], "...done")]

  advisoryMap <- case library of
    Nothing -> do
      when (verbosity > Verbosity.normal) do
        owo [([blue], "Elaborating package install plan")]
      -- the two plans are
      -- 1. the "improved plan" with packages replaced by in-store packages
      -- 2. the "original" elaborated plan
      --
      -- as far as I can tell, for our use case these should be indistinguishable
      (_improvedPlan, plan, _, _, _) <-
        liftIO do
          rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages Nothing
          `catch` \ex -> throwIO $ CabalException {reason = "elaborating the install-plan", cabalException = ex}

      pure $ matchAdvisoriesForPlan (installPlanToLookupTable plan) advisories
    Just lib -> do
      when (verbosity > Verbosity.normal) do
        owo [([blue], "Resolving advisories for library version bounds")]
      deps <- packageLibraryDepends localPackages lib
      liftIO $ print deps
      pure $ matchAdvisoriesForPlan (toMapOn (\(Dependency pn range _) -> (pn, range)) deps) advisories

  when (verbosity > Verbosity.normal) do
    owo [([blue], "...done")]

  pure advisoryMap

-- | provides the built advisories in some consumable form, e.g. as human readable form
--
-- FUTUREWORK(mangoiv): provide output as JSON
handleBuiltAdvisories
  :: (MonadUnliftIO m, Has (Pretty [Text]) sig m)
  => Codensity IO Handle
  -> OutputFormat
  -> M.Map PackageName ElaboratedPackageInfoAdvised
  -> m ()
handleBuiltAdvisories mkHandle = \case
  HumanReadable -> humanReadableHandler mkHandle . M.toList
  Osv -> osvHandler mkHandle

osvHandler :: MonadUnliftIO m => Codensity IO Handle -> M.Map PackageName ElaboratedPackageInfoAdvised -> m ()
osvHandler mkHandle mp =
  withRunCodensityInIO mkHandle \hdl ->
    liftIO . BSL.hPutStr hdl . Aeson.encode @Value . object $
      flip M.foldMapWithKey mp \pn MkElaboratedPackageInfoWith {elaboratedPackageVersionRange, packageAdvisories} ->
        [ fromString (unPackageName pn)
            .= object
              [ "version" .= prettyVersionRange @Text elaboratedPackageVersionRange
              , "advisories" .= map (OSV.convert . fst) (runIdentity packageAdvisories)
              ]
        ]

prettyAdvisory :: Advisory -> Maybe Version -> Vector ([Text], Text)
prettyAdvisory Advisory {advisoryId, advisoryPublished, advisoryKeywords, advisorySummary} mfv =
  let hsecId = T.pack (printHsecId advisoryId)
      indentLine line = [([], "  ")] <> line <> [([], "\n")]
   in foldMap @[]
        indentLine
        [ [([bold, blue], hsecId <> " \"" <> advisorySummary <> "\"")]
        , [([], "published: ") <> ([bold], T.pack $ show advisoryPublished)]
        , [([], "https://haskell.github.io/security-advisories/advisory/" <> hsecId)]
        , fixAvailable
        , [([blue], T.intercalate ", " (coerce advisoryKeywords))]
        ]
 where
  fixAvailable = case mfv of
    Nothing -> [([bold, red], "No fix version available")]
    Just fv -> [([bold, green], "Fix available since version "), ([yellow], prettyVersion fv)]

-- | this is handler is used when displaying to the user
humanReadableHandler
  :: (MonadUnliftIO m, Has (Pretty [Text]) sig m)
  => Codensity IO Handle
  -> [(PackageName, ElaboratedPackageInfoAdvised)]
  -> m ()
humanReadableHandler mkHandle =
  withRunCodensityInIO mkHandle . flip \hdl -> \case
    [] -> pwetty hdl [([green, bold], "No advisories found.")]
    avs -> do
      pwetty hdl [([bold, red], "\n\nFound advisories:\n")]
      for_ avs \(pn, i) -> do
        let verString = ([yellow], prettyVersionRange i.elaboratedPackageVersionRange)
            pkgName = ([yellow], T.pack $ show $ unPackageName pn)
        pwetty hdl [([], "dependency "), pkgName, ([], " at version "), verString, ([], " is vulnerable for:")]
        for_ (runIdentity i.packageAdvisories) (pwetty hdl . uncurry prettyAdvisory)

projectConfigFromFlags :: NixStyleFlags a -> ProjectConfig
projectConfigFromFlags flags = commandLineFlagsToProjectConfig defaultGlobalFlags flags mempty

-- | runs 'Codensity' 'IO' in a monad that is an instance of 'MonadUnliftIO'
withRunCodensityInIO :: MonadUnliftIO m => Codensity IO a -> (a -> m b) -> m b
withRunCodensityInIO cod k = withRunInIO \inIO -> runCodensity cod (inIO . k)
