module Distribution.Audit.Parser (AuditConfig (..), auditCommandParser, OutputFormat (..)) where

import Control.Monad.Codensity (Codensity (Codensity))
import Distribution.Client.NixStyleOptions (NixStyleFlags, defaultNixStyleFlags)
import Distribution.Verbosity qualified as Verbosity
import Options.Applicative
import System.IO (Handle, IOMode (WriteMode), stdout, withFile)

-- | the type of output that is chosen for the command
data OutputFormat
  = -- | write humand readable to stdout
    HumanReadable
  | -- | write as Osv format to the specified file
    Osv

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
  , library :: Maybe String
  -- ^ if we're trying to find issues with the version bounds of a specific library then
  --   its name should be specified
  }

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
        <*> flip option (metavar "VERBOSITY" <> short 'v' <> long "verbosity" <> value Verbosity.normal <> showDefaultWith (const "normal")) do
          eitherReader \case
            "silent" -> Right Verbosity.silent
            "normal" -> Right Verbosity.normal
            "verbose" -> Right Verbosity.verbose
            "deafening" -> Right Verbosity.deafening
            _ -> Left "verbosity has to be one of \"silent\", \"normal\", \"verbose\" or \"deafening\""
        <*> flag HumanReadable Osv do
          mconcat
            [ long "json"
            , short 'm'
            , help "whether to format as json mapping package names to osvs that apply"
            ]
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
        <*> do
          optional
            . strOption
            $ mconcat
              [ long "library"
              , short 'l'
              , metavar "LIBRARY"
              , help
                  "WARNING: this currently only returns the vulnerable direct dependencies of a library.\
                  \n instead of resolving, find advisories for a library in the given cabal project"
              ]
    -- FUTUREWORK(mangoiv): this will accept cabal flags as an additional argument with something like
    -- --cabal-flags "--some-cabal-flag" and print a helper that just forwards the cabal help text
    <*> pure (defaultNixStyleFlags ())
