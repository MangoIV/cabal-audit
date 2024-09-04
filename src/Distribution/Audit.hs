{-# LANGUAGE CPP #-}

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
import Control.Monad.Codensity (Codensity (Codensity, runCodensity))
import Data.Aeson (KeyValue ((.=)), Value, object)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (coerce)
import Data.Foldable (fold, for_)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map qualified as M
import Data.SARIF as Sarif
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Distribution.Client.DistDirLayout (DistDirLayout (distProjectRootDirectory))
import Distribution.Client.NixStyleOptions (NixStyleFlags, defaultNixStyleFlags)
import Distribution.Client.ProjectConfig (ProjectConfig)
import Distribution.Client.ProjectOrchestration
  ( CurrentCommand (OtherCommand)
  , ProjectBaseContext (ProjectBaseContext, cabalDirLayout, distDirLayout, localPackages, projectConfig)
  , commandLineFlagsToProjectConfig
  , establishProjectBaseContext
  )
import Distribution.Client.ProjectPlanning (rebuildInstallPlan)
import Distribution.Client.Setup (defaultGlobalFlags)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Verbosity qualified as Verbosity
import Distribution.Version (Version)
import GHC.Generics (Generic)
import Options.Applicative
import Security.Advisories (Advisory (..), Keyword (..), ParseAdvisoryError (..), printHsecId)
import Security.Advisories.Cabal (ElaboratedPackageInfoAdvised, ElaboratedPackageInfoWith (..), matchAdvisoriesForPlan)
import Security.Advisories.Convert.OSV qualified as OSV
import Security.Advisories.Filesystem (listAdvisories)
import Security.Advisories.SBom.Types (prettyVersion)
import System.Exit (exitFailure)
import System.IO (Handle, IOMode (WriteMode), stdout, withFile)
import System.Process (callProcess)
import UnliftIO (MonadIO (..), MonadUnliftIO (..), catch, throwIO, withSystemTempDirectory)
import Validation (validation)

pwetty :: Has (Pretty [Text]) sig m => Handle -> Vector ([Text], Text) -> m ()
pwetty = pretty

owo :: Has (Pretty [Text]) sig m => Vector ([Text], Text) -> m ()
owo = prettyStdErr

data AuditException
  = -- | parsing the advisory database failed
    ListAdvisoryValidationError {parseError :: [(FilePath, ParseAdvisoryError)]}
  | -- | to rethrow exceptions thrown by cabal during plan elaboration
    CabalException {reason :: String, cabalException :: SomeException}
  deriving stock (Show, Generic)

instance Exception AuditException where
  displayException = \case
    ListAdvisoryValidationError errs ->
      foldMap
        ( \(fp, err) ->
            unlines
              [ "listing advisories in"
              , fp
              , "failed with"
              , displayException err
              ]
        )
        errs
    CabalException ctx (SomeException ex) ->
      "cabal failed while "
        <> ctx
        <> ":\n"
        <> displayException ex

-- | the type of output that is chosen for the command
data OutputFormat
  = -- | write humand readable to stdout
    HumanReadable
  | -- | write as Osv format to the specified file
    Osv
  | -- | write as Sarif format to the specified file (for GitHub Code scanning)
    Sarif

-- | configuration that is specific to the cabal audit command
data AuditConfig = MkAuditConfig
  { advisoriesPathOrURL :: Either FilePath String
  -- ^ path or URL to the advisories
  , verbosity :: Verbosity.Verbosity
  -- ^ verbosity of cabal
  , outputFormat :: OutputFormat
  -- ^ what output format to use
  , outputHandle :: Codensity IO Handle
  -- ^ which handle to write to
  , noColour :: Bool
  -- ^ whether or not to write coloured output
  , failOnWarning :: Bool
  -- ^ whether to exit with a non-success code when advisories are found
  }

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
      interpPretty = if noColour auditConfig then runPretty (const id) else runPretty formatWith

  runM $ interpPretty do
    advisories <-
      ( do
          (advisories, projectBaseContext) <- buildAdvisories auditConfig nixStyleFlags
          handleBuiltAdvisories advisories projectBaseContext (outputHandle auditConfig) (outputFormat auditConfig)
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
  -> m (M.Map PackageName ElaboratedPackageInfoAdvised, ProjectBaseContext)
buildAdvisories MkAuditConfig {advisoriesPathOrURL, verbosity} flags = do
  let cliConfig = projectConfigFromFlags flags

  projectBaseContext@ProjectBaseContext {distDirLayout, cabalDirLayout, projectConfig, localPackages} <-
    liftIO do
      establishProjectBaseContext verbosity cliConfig OtherCommand
        `catch` \ex ->
          throwIO $ CabalException {reason = "trying to establish project base context", cabalException = ex}

  -- the two plans are
  -- 1. the "improved plan" with packages replaced by in-store packages
  -- 2. the "original" elaborated plan
  --
  -- as far as I can tell, for our use case these should be indistinguishable
  (_improvedPlan, plan, _, _, _) <-
    liftIO do
      rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages Nothing
        `catch` \ex -> throwIO $ CabalException {reason = "elaborating the install-plan", cabalException = ex}

  when (verbosity > Verbosity.normal) do
    owo [([blue], "Finished building the cabal install plan, looking for advisories...")]

  advisories <- do
    let k realPath =
          listAdvisories realPath
            >>= validation (throwIO . ListAdvisoryValidationError) pure
    case advisoriesPathOrURL of
      Left fp -> k fp
      Right url -> withSystemTempDirectory "cabal-audit" \tmp -> do
        owo [([blue], "trying to clone " <> T.pack url)]
        liftIO $ callProcess "git" ["clone", "--depth", "1", url, tmp]
        k tmp

  pure (matchAdvisoriesForPlan plan advisories, projectBaseContext)

-- | provides the built advisories in some consumable form, e.g. as human readable form
--
-- FUTUREWORK(mangoiv): provide output as JSON
handleBuiltAdvisories
  :: (MonadUnliftIO m, Has (Pretty [Text]) sig m)
  => M.Map PackageName ElaboratedPackageInfoAdvised
  -> ProjectBaseContext
  -> Codensity IO Handle
  -> OutputFormat
  -> m ()
handleBuiltAdvisories mp pbc mkHandle = \case
  HumanReadable -> humanReadableHandler mkHandle $ M.toList mp
  Osv -> osvHandler mkHandle mp
  Sarif -> sarifHandler mkHandle pbc $ M.toList mp

osvHandler :: MonadUnliftIO m => Codensity IO Handle -> M.Map PackageName ElaboratedPackageInfoAdvised -> m ()
osvHandler mkHandle mp =
  withRunCodensityInIO mkHandle \hdl ->
    liftIO . BSL.hPutStr hdl . Aeson.encode @Value . object $
      flip M.foldMapWithKey mp \pn MkElaboratedPackageInfoWith {elaboratedPackageVersion, packageAdvisories} ->
        [ fromString (unPackageName pn)
            .= object
              [ "version" .= prettyVersion @Text elaboratedPackageVersion
              , "advisories" .= map (OSV.convert . fst) (runIdentity packageAdvisories)
              ]
        ]

sarifHandler
  :: MonadUnliftIO m
  => Codensity IO Handle
  -> ProjectBaseContext
  -> [(PackageName, ElaboratedPackageInfoAdvised)]
  -> m ()
sarifHandler mkHandle projectBaseContext packageAdvisories = do
  let projectRoot = distProjectRootDirectory $ distDirLayout projectBaseContext -- TODO(blackheaven): resolve repository root (in GitHub Action, we get the cloned directory, instead of the directory from repository's root)
  let advisories =
        M.elems $
          M.fromListWith (\(advisory, pkgsInfo) (_, pkgsInfo') -> (advisory, pkgsInfo <> pkgsInfo')) $
            packageAdvisories >>= \(pkgName, pkgInfo) ->
              runIdentity pkgInfo.packageAdvisories <&> \(advisory, fixedAt) ->
                (advisory.advisoryId, (advisory, [(T.pack $ unPackageName pkgName, fixedAt)]))
      run =
        MkRun
          { runTool =
              let tool name version =
                    defaultToolComponent
                      { toolComponentName = Just name
                      , toolComponentVersion = Just version
                      }
               in MkTool
                    { toolExtensions =
                        [ tool "hsec-tools" VERSION_hsec_tools
                        , tool "ghc" $ T.pack __GLASGOW_HASKELL_FULL_VERSION__
                        ]
                    , toolDriver = tool "cabal-audit" VERSION_cabal_audit
                    }
          , runResults =
              advisories <&> \(advisory, concernedInfo) ->
                MkResult
                  { resultRuleId = T.pack $ printHsecId advisory.advisoryId
                  , resultMessage =
                      MkMultiformatMessageString
                        { mmsText = fold $ prettyAdvisory advisory $ prettyTextSummary $ fst <$> concernedInfo
                        , mmsMarkdown = Just $ fold $ prettyAdvisory advisory $ prettyMarkdown $ prettyMultiplePackages concernedInfo
                        }
                  , resultLocations =
                      [ -- TODO(blackheaven) cabal files/lock?
                        MkLocation $
                          Just $
                            MkPhysicalLocation
                              { physicalLocationArtifactLocation = MkArtifactLocation $ T.pack ("file:///" <> projectRoot)
                              , physicalLocationRegion = MkRegion 1 1 2 2 -- TODO(blackheaven): inspect lock file to find exact position
                              }
                      ]
                  , resultLevel = Just Sarif.Error
                  }
          , runArtifacts =
              [ -- TODO(blackheaven) cabal files/lock?
                MkArtifact
                  { artifactLocation = MkArtifactLocation $ T.pack ("file:///" <> projectRoot)
                  , artifactMimeType = Nothing
                  }
              ]
          }
  withRunCodensityInIO mkHandle \hdl ->
    liftIO . BSL.hPutStr hdl . Aeson.encode $ defaultLog {logRuns = [run]}

data Segment = Segment
  { sConsoleColors :: [Text]
  , sMarkdownPrefix :: Text
  , sText :: Text
  }

data Line = Line (Vector Segment) | BlockLine (Vector Segment) | EmptyLine

data PrettyArgs a = PrettyArgs
  { paFixed :: Vector Line
  , paLine :: Line -> Vector a
  , paSubtitle :: Vector Segment
  }

prettyTextSummary :: [Text] -> PrettyArgs Text
prettyTextSummary packageNames =
  PrettyArgs
    { paFixed = mempty
    , paLine =
        \case
          Line xs | filledLine xs -> (sText <$> xs) <> ["\n"]
          BlockLine xs | filledLine xs -> (sText <$> xs) <> ["\n"]
          Line _ -> mempty
          BlockLine _ -> mempty
          EmptyLine -> ["\n"]
    , paSubtitle = [Segment [] "" $ "(" <> T.intercalate ", " packageNames <> ")"]
    }

prettyMultiline :: Vector Segment -> PrettyArgs ([Text], Text)
prettyMultiline fixedLine =
  PrettyArgs
    { paFixed = [BlockLine fixedLine]
    , paLine =
        \case
          Line xs | filledLine xs -> [([], "  ")] <> (mkSegment <$> xs) <> nl
          BlockLine xs | filledLine xs -> [([], "  ")] <> (mkSegment <$> xs) <> nl
          Line _ -> mempty
          BlockLine _ -> mempty
          EmptyLine -> nl
    , paSubtitle = mempty
    }
 where
  mkSegment s = (sConsoleColors s, sText s)
  nl = [([], "\n")]

prettyMarkdown :: Vector Line -> PrettyArgs Text
prettyMarkdown pkgs =
  PrettyArgs
    { paFixed = pkgs
    , paLine =
        \case
          Line xs | filledLine xs -> (mkSegment <$> xs) <> ["\n"]
          BlockLine xs | filledLine xs -> (mkSegment <$> xs) <> ["\n\n"]
          Line _ -> mempty
          BlockLine _ -> mempty
          EmptyLine -> ["\n"]
    , paSubtitle = mempty
    }
 where
  mkSegment s = sMarkdownPrefix s <> sText s

filledLine :: Foldable t => t Segment -> Bool
filledLine = not . all (T.null . sText)

prettySinglePackage :: Maybe Version -> Vector Segment
prettySinglePackage =
  \case
    Nothing ->
      [ Segment [bold, red] "" "No fix version available"
      ]
    Just fv ->
      [ Segment [bold, green] "" "Fix available since version "
      , Segment [yellow] "" $ prettyVersion fv
      ]

prettyMultiplePackages :: [(Text, Maybe Version)] -> Vector Line
prettyMultiplePackages packages =
  [Line [Segment [] "" "Concerned package:"]]
    <> ( V.fromList packages <&> \(pkgName, mfv) ->
          Line $ [Segment [] "" $ "* " <> pkgName <> ": "] <> prettySinglePackage mfv
       )
    <> [EmptyLine]

prettyAdvisory :: Advisory -> PrettyArgs a -> Vector a
prettyAdvisory Advisory {advisoryId, advisoryPublished, advisoryKeywords, advisorySummary} args =
  foldMap @Vector
    (foldMap $ paLine args)
    [ [BlockLine [Segment [bold, blue] "# " $ hsecId <> " \"" <> advisorySummary <> "\""]]
    , [BlockLine $ paSubtitle args]
    , [BlockLine [Segment [] "" "published: ", Segment [bold] "" $ T.pack $ show advisoryPublished]]
    , [BlockLine [Segment [] "" $ "https://haskell.github.io/security-advisories/advisory/" <> hsecId]]
    , paFixed args
    , [BlockLine [Segment [blue] "" $ T.intercalate ", " (coerce advisoryKeywords)]]
    ]
 where
  hsecId = T.pack (printHsecId advisoryId)

withRunCodensityInIO :: MonadUnliftIO m => Codensity IO a -> (a -> m b) -> m b
withRunCodensityInIO cod k = withRunInIO \inIO -> runCodensity cod (inIO . k)

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
        let verString = ([yellow], prettyVersion $ elaboratedPackageVersion i)
            pkgName = ([yellow], T.pack $ show $ unPackageName pn)
        pwetty hdl [([], "dependency "), pkgName, ([], " at version "), verString, ([], " is vulnerable for:")]
        for_
          (runIdentity (packageAdvisories i))
          (pwetty hdl . uncurry prettyAdvisory . fmap (prettyMultiline . prettySinglePackage))

projectConfigFromFlags :: NixStyleFlags a -> ProjectConfig
projectConfigFromFlags flags = commandLineFlagsToProjectConfig defaultGlobalFlags flags mempty

auditCommandParser :: Parser (AuditConfig, NixStyleFlags ())
auditCommandParser =
  (,)
    <$> do
      MkAuditConfig
        <$> do
          Left
            <$> strOption do
              mconcat
                [ long "file-path"
                , short 'p'
                , metavar "FILEPATH"
                , help "the path to the repository containing an advisories directory"
                ]
              <|> Right
            <$> strOption do
              mconcat
                [ long "repository"
                , short 'r'
                , metavar "REPOSITORY"
                , help "the url to the repository containing an advisories directory"
                , value "https://github.com/haskell/security-advisories"
                ]
        <*> flip option (long "verbosity" <> value Verbosity.normal <> showDefaultWith (const "normal")) do
          eitherReader \case
            "silent" -> Right Verbosity.silent
            "normal" -> Right Verbosity.normal
            "verbose" -> Right Verbosity.verbose
            "deafening" -> Right Verbosity.deafening
            _ -> Left "verbosity has to be one of \"silent\", \"normal\", \"verbose\" or \"deafening\""
        <*> ( flag'
                Osv
                ( mconcat
                    [ long "json"
                    , short 'm'
                    , help "whether to format as json mapping package names to osvs that apply"
                    ]
                )
                <|> flag'
                  Sarif
                  ( mconcat
                      [ long "sarif"
                      , help "produce a sarif file (GitHub Code Scanning)"
                      ]
                  )
                <|> pure HumanReadable
            )
        <*> do
          let mkFileHandle fp = Codensity (withFile fp WriteMode)
          mkFileHandle
            <$> strOption do
              mconcat
                [ long "to-file"
                , short 'o'
                , metavar "FILEPATH"
                , help "specify a file to write to, instead of stdout"
                ]
              <|> pure (Codensity \k -> k stdout)
        <*> switch do
          mconcat
            [ long "no-colour"
            , long "no-color"
            , short 'b'
            , help "don't colour the output"
            ]
        <*> switch do
          mconcat
            [ long "fail-on-warning"
            , help "Exits with an error code if any advisories are found in the build plan"
            ]
    -- FUTUREWORK(mangoiv): this will accept cabal flags as an additional argument with something like
    -- --cabal-flags "--some-cabal-flag" and print a helper that just forwards the cabal help text
    <*> pure (defaultNixStyleFlags ())
