{-# LANGUAGE CPP #-}

-- | provides the @cabal-audit@ plugin which works as follows:
--
-- 1. parse command line arguments to pass on to cabal to build
--    an install plan and parse the advisories database
-- 2. lookup all dependencies in the elaborated plan within the
--    database
-- 3. summarise the found vulnerabilities as a humand readable or
--    otherwise formatted output
module Distribution.Audit (auditMain, buildAdvisories, chooseSarifLocationForPackages, AuditConfig (..), AuditException (..)) where

import Colourista.Pure (blue, bold, formatWith, green, red, yellow)
import Control.Algebra (Has)
import Control.Carrier.Lift (runM)
import Control.Effect.Pretty (Pretty, PrettyC, pretty, prettyStdErr, runPretty)
import Control.Exception (Exception (displayException), SomeException (SomeException))
import Control.Monad (filterM, forM, when)
import Control.Monad.Codensity (Codensity (Codensity, runCodensity))
import Data.Aeson (KeyValue ((.=)), Value, object)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isAlphaNum)
import Data.Coerce (coerce)
import Data.Foldable (fold, for_)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (isPrefixOf, nubBy, sortOn)
import Data.Map qualified as M
import Data.SARIF as Sarif
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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
import Distribution.Client.ProjectPlanning (ElaboratedSharedConfig (pkgConfigCompiler), rebuildInstallPlan)
import Distribution.Client.Setup (defaultGlobalFlags)
import Distribution.Compiler (CompilerId (CompilerId))
import Distribution.Simple.Compiler (compilerId)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Verbosity qualified as Verbosity
import Distribution.Version (Version)
import GHC.Generics (Generic)
import Options.Applicative
import Security.Advisories (Advisory (..), Keyword (..), ParseAdvisoryError (..), ghcComponentToText, printHsecId)
import Security.Advisories.Cabal
  ( AuditedComponent (..)
  , ElaboratedPackageInfoAdvised
  , ElaboratedPackageInfoWith (..)
  , matchAdvisoriesForPlan
  )
import Security.Advisories.Convert.OSV qualified as OSV
import Security.Advisories.Filesystem (listAdvisories)
import Security.Advisories.SBom.Types (prettyVersion)
import Security.Advisories.Sync qualified as Sync
import System.Directory (doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, (</>))
import System.IO (Handle, IOMode (WriteMode), stdout, withFile)
import UnliftIO (MonadIO (..), MonadUnliftIO (..), catch, throwIO, withSystemTempDirectory)
import Validation (validation)

pwetty :: Has (Pretty [Text]) sig m => Handle -> Vector ([Text], Text) -> m ()
pwetty = pretty

owo :: Has (Pretty [Text]) sig m => Vector ([Text], Text) -> m ()
owo = prettyStdErr

data AuditException
  = -- | fetching advisories failed
    AdvisoriesFetchingError {fetchingError :: String}
  | -- | parsing the advisory database failed
    ListAdvisoryValidationError {parseError :: [(FilePath, ParseAdvisoryError)]}
  | -- | to rethrow exceptions thrown by cabal during plan elaboration
    CabalException {reason :: String, cabalException :: SomeException}
  deriving stock (Show, Generic)

instance Exception AuditException where
  displayException = \case
    AdvisoriesFetchingError err ->
      mconcat
        [ "Fetching the advisories failed with:\n"
        , err
        ]
    ListAdvisoryValidationError errs ->
      mconcat
        [ "Listing the advisories in "
        , "failed with: \n"
        , foldMap (\(fp, ex) -> mconcat ["* ", fp, ":\n", displayException ex]) errs
        ]
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
  -> m (M.Map AuditedComponent ElaboratedPackageInfoAdvised, ProjectBaseContext)
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
  (_improvedPlan, plan, sharedConfig, _, _) <-
    liftIO do
      rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages Nothing
        `catch` \ex -> throwIO $ CabalException {reason = "elaborating the install-plan", cabalException = ex}
  let CompilerId _ ghcVersion = compilerId (pkgConfigCompiler sharedConfig)
  when (verbosity > Verbosity.normal) do
    owo [([blue], "Finished building the cabal install plan, looking for advisories...")]

  advisories <- do
    let k realPath =
          listAdvisories realPath
            >>= validation (throwIO . ListAdvisoryValidationError) pure
    case advisoriesPathOrURL of
      Left fp -> k fp
      Right url -> withSystemTempDirectory "cabal-audit-" \tmp -> do
        owo [([blue], "trying to fetch " <> T.pack url)]
        eFetched <- liftIO $ Sync.sync $ Sync.githubSnapshot tmp url "generated/snapshot-export"
        case eFetched of
          Left e -> throwIO $ AdvisoriesFetchingError e
          Right _ -> k tmp

  pure (matchAdvisoriesForPlan plan ghcVersion advisories, projectBaseContext)

-- | provides the built advisories in some consumable form, e.g. as human readable form
--
-- FUTUREWORK(mangoiv): provide output as JSON
handleBuiltAdvisories
  :: (MonadUnliftIO m, Has (Pretty [Text]) sig m)
  => M.Map AuditedComponent ElaboratedPackageInfoAdvised
  -> ProjectBaseContext
  -> Codensity IO Handle
  -> OutputFormat
  -> m ()
handleBuiltAdvisories mp pbc mkHandle = \case
  HumanReadable -> humanReadableHandler mkHandle $ M.toList mp
  Osv -> osvHandler mkHandle mp
  Sarif -> sarifHandler mkHandle pbc $ M.toList mp

osvHandler :: MonadUnliftIO m => Codensity IO Handle -> M.Map AuditedComponent ElaboratedPackageInfoAdvised -> m ()
osvHandler mkHandle mp =
  withRunCodensityInIO mkHandle \hdl ->
    liftIO . BSL.hPutStr hdl . Aeson.encode @Value . object $
      flip M.foldMapWithKey mp \pn MkElaboratedPackageInfoWith {elaboratedPackageVersion, packageAdvisories} ->
        [ fromString (T.unpack (renderAuditedComponent pn))
            .= object
              [ "version" .= prettyVersion @Text elaboratedPackageVersion
              , "advisories" .= map (OSV.convert . fst) (runIdentity packageAdvisories)
              ]
        ]

sarifHandler
  :: MonadUnliftIO m
  => Codensity IO Handle
  -> ProjectBaseContext
  -> [(AuditedComponent, ElaboratedPackageInfoAdvised)]
  -> m ()
sarifHandler mkHandle projectBaseContext packageAdvisories = do
  let projectRoot = distProjectRootDirectory $ distDirLayout projectBaseContext -- TODO(blackheaven): resolve repository root (in GitHub Action, we get the cloned directory, instead of the directory from repository's root)
  let advisories =
        M.elems $
          M.fromListWith (\(advisory, pkgsInfo) (_, pkgsInfo') -> (advisory, pkgsInfo <> pkgsInfo')) $
            packageAdvisories >>= \(pkgName, pkgInfo) ->
              runIdentity pkgInfo.packageAdvisories <&> \(advisory, fixedAt) ->
                (advisory.advisoryId, (advisory, [(renderAuditedComponent pkgName, fixedAt)]))
  advisoryLocations <- forM advisories $ \(_, concernedInfo) ->
    liftIO $
      chooseSarifLocationForPackages
        projectRoot
        (fst <$> concernedInfo)
  let advisoriesWithLocations = zip advisories advisoryLocations
  let rules =
        nubBy (\a b -> rdId a == rdId b) $
          map (ruleForAdvisory . fst) advisories
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
                    , toolDriver =
                        let toolCabalAudit = tool "cabal-audit" VERSION_cabal_audit
                         in toolCabalAudit {toolComponentRules = rules}
                    }
          , runResults =
              advisoriesWithLocations <&> \((advisory, concernedInfo), (sarifLocation, sarifRegion)) ->
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
                              { physicalLocationArtifactLocation = MkArtifactLocation $ T.pack sarifLocation
                              , physicalLocationRegion = sarifRegion -- TODO(blackheaven): inspect lock file to find exact position
                              }
                      ]
                  , resultLevel = Just Sarif.Error
                  }
          , runArtifacts =
              -- TODO(blackheaven) cabal files/lock?
              nubBy
                (\a b -> artifactLocation a == artifactLocation b)
                ( advisoriesWithLocations <&> \((_, _), (sarifLocation, _)) ->
                    MkArtifact
                      { artifactLocation = MkArtifactLocation $ T.pack sarifLocation
                      , artifactMimeType = Nothing
                      }
                )
          }
  withRunCodensityInIO mkHandle \hdl ->
    liftIO . BSL.hPutStr hdl . Aeson.encode $ defaultLog {logRuns = [run]}

advisoryUrl :: Advisory -> Text
advisoryUrl advisory =
  "https://haskell.github.io/security-advisories/advisory/"
    <> T.pack (printHsecId advisory.advisoryId)

ruleForAdvisory :: Advisory -> ReportingDescriptor
ruleForAdvisory advisory =
  (defaultReportingDescriptor ruleId)
    { rdName = Just ruleId
    , rdShortDescription =
        Just $
          MkMultiformatMessageString
            { mmsText = advisory.advisorySummary
            , mmsMarkdown = Nothing
            }
    , rdFullDescription =
        Just $
          MkMultiformatMessageString
            { mmsText =
                "Haskell Security Advisory: "
                  <> advisory.advisorySummary
                  <> ". Keywords: "
                  <> T.intercalate ", " (coerce advisory.advisoryKeywords)
            , mmsMarkdown = Nothing
            }
    , rdHelpUri = Just (advisoryUrl advisory)
    , rdDefaultConfiguration =
        Just
          defaultReportingConfiguration
            { rcLevel = Just Sarif.Error
            }
    }
 where
  ruleId = T.pack (printHsecId advisory.advisoryId)

-- search in
-- 1 cabal.project.freeze
-- 2 cabal.project
-- 3 root .cabal file
-- For each candidate file, look for the first affected package name
-- if found, return that file and an exact-ish MkRegion
-- otherwise returs - Region 1:1
chooseSarifLocationForPackages
  :: FilePath
  -> [Text]
  -> IO (FilePath, Region)
chooseSarifLocationForPackages projectRoot packageNames = do
  let freezeFile = "cabal.project.freeze"
      projectFile = "cabal.project"

  rootEntries <- listDirectory projectRoot
  let cabalFiles =
        [ entry
        | entry <- rootEntries
        , takeExtension entry == ".cabal"
        ]

      candidateFiles =
        [freezeFile, projectFile] <> cabalFiles

  existingCandidates <- filterM (doesFileExist . (projectRoot </>)) candidateFiles

  found <- findFirstPackageOccurrence projectRoot existingCandidates packageNames

  case found of
    Just located -> pure located
    Nothing -> do
      fallback <- chooseSarifLocation projectRoot
      pure (fallback, MkRegion 1 1 1 1)

findFirstPackageOccurrence
  :: FilePath
  -> [FilePath]
  -> [Text]
  -> IO (Maybe (FilePath, Region))
findFirstPackageOccurrence _ [] _ = pure Nothing
findFirstPackageOccurrence projectRoot (candidate : rest) packageNames = do
  match <- findPackageOccurrenceInFile (projectRoot </> candidate) packageNames
  case match of
    Just region -> pure $ Just (candidate, region)
    Nothing -> findFirstPackageOccurrence projectRoot rest packageNames

findPackageOccurrenceInFile
  :: FilePath
  -> [Text]
  -> IO (Maybe Region)
findPackageOccurrenceInFile filePath packageNames = do
  contents <- TIO.readFile filePath
  pure $
    firstJust
      [ findPackageOccurrenceInLines (zip [1 ..] $ T.lines contents) packageName
      | packageName <- packageNames
      ]

findPackageOccurrenceInLines
  :: [(Int, Text)]
  -> Text
  -> Maybe Region
findPackageOccurrenceInLines numberedLines packageName =
  firstJust
    [ mkRegionForMatch lineNo lineText packageName
    | (lineNo, lineText) <- numberedLines
    , not (isCommentLine lineText)
    ]

mkRegionForMatch
  :: Int
  -> Text
  -> Text
  -> Maybe Region
mkRegionForMatch lineNo lineText packageName = do
  column0 <- findPackageColumn lineText packageName
  let startColumn = column0 + 1
      endColumn = startColumn + T.length packageName
  pure $ MkRegion lineNo startColumn lineNo endColumn

findPackageColumn :: Text -> Text -> Maybe Int
findPackageColumn lineText packageName =
  findWholeTokenColumnIn (T.unpack packageName) (T.unpack lineText)

findWholeTokenColumnIn :: String -> String -> Maybe Int
findWholeTokenColumnIn needle haystack =
  go 0 haystack
 where
  go _ [] = Nothing
  go n rest
    | needle `isPrefixOf` rest
    , hasTokenBoundaries n needle haystack =
        Just n
    | otherwise =
        case rest of
          _ : xs -> go (n + 1) xs

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x : xs) =
  case x of
    Just _ -> x
    Nothing -> firstJust xs

chooseSarifLocation :: FilePath -> IO FilePath
chooseSarifLocation projectRoot = do
  let freezeFile = projectRoot </> "cabal.project.freeze"
      projectFile = projectRoot </> "cabal.project"

  freezeExists <- doesFileExist freezeFile
  if freezeExists
    then pure "cabal.project.freeze"
    else do
      projectExists <- doesFileExist projectFile
      if projectExists
        then pure "cabal.project"
        else do
          entries <- listDirectory projectRoot
          let cabalFiles =
                sortOn
                  id
                  [ e
                  | e <- entries
                  , takeExtension e == ".cabal"
                  ]
          pure $ case cabalFiles of
            fp : _ -> fp
            [] -> "."

hasTokenBoundaries :: Int -> String -> String -> Bool
hasTokenBoundaries start needle haystack =
  isLeftBoundary leftChar && isRightBoundary rightChar
 where
  leftChar =
    if start == 0
      then Nothing
      else Just (haystack !! (start - 1))

  rightIndex = start + length needle
  rightChar =
    if rightIndex >= length haystack
      then Nothing
      else Just (haystack !! rightIndex)

isLeftBoundary :: Maybe Char -> Bool
isLeftBoundary Nothing = True
isLeftBoundary (Just c) = not (isPackageTokenChar c)

isRightBoundary :: Maybe Char -> Bool
isRightBoundary Nothing = True
isRightBoundary (Just c) = not (isPackageTokenChar c)

isPackageTokenChar :: Char -> Bool
isPackageTokenChar c =
  isAlphaNum c || c == '-' || c == '_'

isCommentLine :: Text -> Bool
isCommentLine lineText =
  case T.dropWhile isHorizontalSpace lineText of
    stripped -> "--" `T.isPrefixOf` stripped

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'

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

renderAuditedComponent :: AuditedComponent -> Text
renderAuditedComponent = \case
  HackageComponent pkg -> T.pack (unPackageName pkg)
  GhcComponent tool -> ghcComponentToText tool

renderAuditedComponentKind :: AuditedComponent -> Text
renderAuditedComponentKind = \case
  HackageComponent _ -> "Hackage package"
  GhcComponent _ -> "GHC tool"

renderAuditedComponentLabel :: AuditedComponent -> Text
renderAuditedComponentLabel component =
  renderAuditedComponentKind component <> " " <> renderAuditedComponent component

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
  -> [(AuditedComponent, ElaboratedPackageInfoAdvised)]
  -> m ()
humanReadableHandler mkHandle =
  withRunCodensityInIO mkHandle . flip \hdl -> \case
    [] -> pwetty hdl [([green, bold], "No advisories found.")]
    avs -> do
      pwetty hdl [([bold, red], "\n\nFound advisories:\n")]
      for_ avs \(component, i) -> do
        let verString = ([yellow], prettyVersion $ elaboratedPackageVersion i)
            componentLabel = ([], renderAuditedComponentLabel component)
        pwetty
          hdl
          [ componentLabel
          , ([], " at version ")
          , verString
          , ([], " is vulnerable for:")
          ]
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
