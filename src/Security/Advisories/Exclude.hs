{-# LANGUAGE StrictData #-}

module Security.Advisories.Exclude
  ( Exclude (..)
  , ExclusionsLoadError (..)
  , loadExclusions
  , applyExclusions
  , displayExclusionsLoadError
  )
where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Security.Advisories (Advisory (advisoryId))
import Security.Advisories.Cabal
  ( AuditedComponent
  , ElaboratedPackageInfoAdvised
  , ElaboratedPackageInfoWith (..)
  )
import Security.Advisories.Core.HsecId (HsecId, parseHsecId)
import System.Directory (doesFileExist)

data Exclude = MkExclude
  { exclude'id :: HsecId
  , exclude'reason :: Text
  , exclude'expires :: Maybe Day
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Exclude where
  parseJSON = withObject "Exclude" \o -> do
    idText :: Text <- o .: "id"
    exclude'id <- maybe (fail ("invalid HSEC id: " <> T.unpack idText)) pure $ parseHsecId (T.unpack idText)
    exclude'reason <- o .: "reason"
    exclude'expires <- o .:? "expires"
    pure MkExclude {exclude'id, exclude'reason, exclude'expires}

data ExclusionsLoadError
  = ExclusionsFileParseError FilePath String
  deriving stock (Eq, Show, Generic)

displayExclusionsLoadError :: ExclusionsLoadError -> String
displayExclusionsLoadError = \case
  ExclusionsFileParseError fp err ->
    "Failed to parse exclusion file " <> fp <> ":\n" <> err

loadExclusions :: FilePath -> IO (Either ExclusionsLoadError [Exclude])
loadExclusions path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right mempty)
    else do
      bs <- BSL.readFile path
      pure $ either (Left . ExclusionsFileParseError path) Right $ Aeson.eitherDecode bs

applyExclusions
  :: Day
  -> [Exclude]
  -> Map AuditedComponent ElaboratedPackageInfoAdvised
  -> Map AuditedComponent ElaboratedPackageInfoAdvised
applyExclusions today exs =
  let activeIds :: Set HsecId
      activeIds =
        Set.fromList [exclude'id e | e <- exs, not (isExpired today e)]

      keepAdvisory (adv, _) = not (Set.member (advisoryId adv) activeIds)

      filterPackage info =
        let kept = filter keepAdvisory (runIdentity (packageAdvisories info))
         in if null kept
              then Nothing
              else Just info {packageAdvisories = Identity kept}
   in Map.mapMaybe filterPackage

isExpired :: Day -> Exclude -> Bool
isExpired today MkExclude {exclude'expires} =
  case exclude'expires of
    Nothing -> False
    Just d -> d < today
