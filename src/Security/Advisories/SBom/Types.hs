{-# LANGUAGE StrictData #-}

module Security.Advisories.SBom.Types (SBomMeta (..), ComponentType (..), prettyComponentType, prettyVersion, prettyLicense, mkPurl, mkBomRef) where

import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Vector (Vector)
import Distribution.Pretty (Pretty (pretty))
import Distribution.SPDX qualified as SPDX
import Distribution.Version (Version, versionNumbers)
import GHC.Generics (Generic)
import Text.PrettyPrint.HughesPJ (Mode (OneLineMode), TextDetails (..), fullRender)

-- | a component type; cabal applications, tests and benchmarks are going to be applications, cabal libraries
--   are going to be libraries
data ComponentType
  = Application
  | Library
  deriving stock (Eq, Ord, Show, Generic)

{-
 - https://hackage.haskell.org/package/Cabal-syntax-3.12.0.0/docs/Distribution-Types-PackageDescription.html
 - https://hackage.haskell.org/package/Cabal-syntax-3.12.0.0/docs/Distribution-Types-PackageId.html#t:PackageIdentifier
 - https://hackage.haskell.org/package/Cabal-syntax-3.12.0.0/docs/Distribution-Types-Library.html#t:Library
 - -}

-- | the component to describe
data SBomMeta = MKSBomMeta
  { sbom'componentName :: Text
  -- ^ the name of the component
  , sbom'componentVersion :: Version
  -- ^ the version of the component
  , sbom'componentType :: ComponentType
  -- ^ the type of the component. If this is not present, then this is no
  -- component but
  , sbom'componentAuthor :: Text
  -- ^ the authors of the component
  , sbom'componentDescription :: Text
  -- ^ the component's description
  , sbom'componentLicense :: SPDX.License
  -- ^ the component's license
  , sbom'supplierName :: Maybe Text
  -- ^ the name of the supplier if present, e.g. github/haskell
  , sbom'componentDependencies :: Vector ComponentDependency
  -- ^ all the dependencies of a component
  }
  deriving stock (Eq, Ord, Show, Generic)

data ComponentDependency = MkComponentDependency
  { dep'type :: ComponentType
  , dep'repo :: Maybe Text
  , dep'name :: Text
  , dep'version :: Version
  , dep'license :: Maybe SPDX.License
  , dep'description :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)

mkPurl
  :: Text
  -- ^ repo
  -> Text
  -- ^ name
  -> Maybe Version
  -- ^ version
  -> Text
mkPurl repo name version = mconcat ["pkg:", repo, "/", name, maybe "" (("@" <>) . prettyVersion) version]
{-# INLINE mkPurl #-}

-- | pretty-prints a `Version`
--
-- >>> import Distribution.Version
-- >>> prettyVersion $ mkVersion [0, 1, 0, 0]
-- "0.1.0.0"
prettyVersion :: IsString s => Version -> s
prettyVersion = fromString . List.intercalate "." . map show . versionNumbers
{-# INLINE prettyVersion #-}

-- | pretty print an SPDX license expression
--
-- >>> import Distribution.SPDX
-- >>> prettyLicense (EAnd (ELicense (ELicenseId AGPL_1_0) Nothing) (ELicense (ELicenseId Beerware) Nothing))
-- "AGPL-1.0 AND Beerware"
prettyLicense :: SPDX.LicenseExpression -> Text
prettyLicense = TL.toStrict . TLB.toLazyText . fullRender OneLineMode 0 0 appendDoc "" . pretty
 where
  appendDoc =
    (<>) . \case
      Chr c -> TLB.singleton c
      Str s -> TLB.fromString s
      PStr s -> TLB.fromString s
{-# INLINE prettyLicense #-}

prettyComponentType :: IsString s => ComponentType -> s
prettyComponentType = \case
  Library -> "library"
  Application -> "application"
{-# INLINE prettyComponentType #-}

-- | builds a bom ref for a component
mkBomRef
  :: Maybe Text
  -- ^ supplier such as a github repository (@github/haskell@), if not specified, @haskell@ is assumed
  -> Text
  -- ^ name of the component
  -> ComponentType
  -- ^ component type
  -> Version
  -- ^ version of the component
  -> Text
mkBomRef msupplier name typ version = T.intercalate ":" [fromMaybe "haskell" msupplier, name, prettyComponentType typ, prettyVersion version]
