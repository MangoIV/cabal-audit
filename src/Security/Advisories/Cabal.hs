{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Security.Advisories.Cabal
  ( matchAdvisoriesForPlan
  , renderGhcComponent
  , AuditedComponent (..)
  , ElaboratedPackageInfoWith (..)
  , ElaboratedPackageInfoAdvised
  , ElaboratedPackageInfo
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Monoid (Alt (Alt, getAlt), Any (Any, getAny))
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
import Distribution.Client.InstallPlan (foldPlanPackage)
import Distribution.Client.InstallPlan qualified as Plan
import Distribution.Client.ProjectPlanning (ElaboratedInstallPlan, elabPkgSourceId)
import Distribution.InstalledPackageInfo (sourcePackageId)
import Distribution.Package (PackageIdentifier (PackageIdentifier, pkgName, pkgVersion), PackageName, mkPackageName)
import Distribution.Version (Version)
import GHC.Generics (Generic)
import Security.Advisories -- .Core.Advisory
  ( Advisory (advisoryAffected)
  , Affected (Affected, affectedComponentIdentifier, affectedVersions)
  , AffectedVersionRange (affectedVersionRangeFixed, affectedVersionRangeIntroduced)
  , ComponentIdentifier (GHC, Hackage)
  , GHCComponent (..)
  )

-- | for a given 'ElaboratedInstallPlan' and a list of advisories, construct a map of advisories
--   and packages within the install plan that are affected by them
matchAdvisoriesForPlan
  :: ElaboratedInstallPlan
  -- ^ the plan as created by cabal
  -> Version
  -- ^ GHC version
  -> [Advisory]
  -- ^ the advisories as discovered in some advisory dir
  -> Map AuditedComponent ElaboratedPackageInfoAdvised
matchAdvisoriesForPlan plan ghcVersion = foldr advise Map.empty
 where
  advise :: Advisory -> Map AuditedComponent ElaboratedPackageInfoAdvised -> Map AuditedComponent ElaboratedPackageInfoAdvised
  advise adv = do
    let versionAffected :: Version -> [AffectedVersionRange] -> Bool
        versionAffected v =
          getAny . foldMap \av -> Any do
            v >= affectedVersionRangeIntroduced av && maybe True (v <) (affectedVersionRangeFixed av)

        fixVersion :: [AffectedVersionRange] -> Maybe Version
        fixVersion = getAlt . foldMap (Alt . affectedVersionRangeFixed)

        lookupTable :: Map PackageName ElaboratedPackageInfo
        lookupTable = installPlanToLookupTable plan

        ghcToolLookup :: Map GHCComponent Version
        ghcToolLookup = mkGhcToolLookup ghcVersion

        advPkgs :: [(AuditedComponent, ElaboratedPackageInfoAdvised)]
        advPkgs = flip mapMaybe (advisoryAffected adv) \Affected {affectedComponentIdentifier, affectedVersions} -> do
          (component, pkgInfo) <- lookupAuditedComponent lookupTable ghcToolLookup affectedComponentIdentifier
          let MkElaboratedPackageInfoWith {elaboratedPackageVersion = elabv} = pkgInfo
          if versionAffected elabv affectedVersions
            then Just (component, pkgInfo {packageAdvisories = Identity [(adv, fixVersion affectedVersions)]})
            else Nothing

    flip
      do foldr . uncurry $ Map.insertWith combinedElaboratedPackageInfos
      advPkgs

  combinedElaboratedPackageInfos
    MkElaboratedPackageInfoWith {elaboratedPackageVersion = ver1, packageAdvisories = advs1}
    MkElaboratedPackageInfoWith {packageAdvisories = advs2} =
      MkElaboratedPackageInfoWith {elaboratedPackageVersion = ver1, packageAdvisories = advs1 <> advs2}

lookupAuditedComponent
  :: Map PackageName ElaboratedPackageInfo
  -> Map GHCComponent Version
  -> ComponentIdentifier
  -> Maybe (AuditedComponent, ElaboratedPackageInfo)
lookupAuditedComponent hackageLookup ghcToolLookup affectedComponentId = case affectedComponentId of
  Hackage t ->
    let pkgName = mkPackageName (T.unpack t)
     in (HackageComponent pkgName,) <$> Map.lookup pkgName hackageLookup
  GHC toolName ->
    ( \version ->
        ( GhcComponent toolName
        , MkElaboratedPackageInfoWith
            { elaboratedPackageVersion = version
            , packageAdvisories = Proxy
            }
        )
    )
      <$> Map.lookup toolName ghcToolLookup

mkGhcToolLookup :: Version -> Map GHCComponent Version
mkGhcToolLookup v =
  Map.fromList
    [ (GHCCompiler, v)
    , (GHCi, v)
    , (GHCRTS, v)
    , (GHCPkg, v)
    , (RunGHC, v)
    , (IServ, v)
    , (HP2PS, v)
    , (HPC, v)
    , (HSC2HS, v)
    , (Haddock, v)
    ]

renderGhcComponent :: GHCComponent -> T.Text
renderGhcComponent = \case
  GHCCompiler -> "ghc"
  GHCi -> "ghci"
  GHCRTS -> "rts"
  GHCPkg -> "ghc-pkg"
  RunGHC -> "runghc"
  IServ -> "ghc-iserv"
  HP2PS -> "hp2ps"
  HPC -> "hpc"
  HSC2HS -> "hsc2hs"
  Haddock -> "haddock"

type ElaboratedPackageInfoAdvised = ElaboratedPackageInfoWith Identity

type ElaboratedPackageInfo = ElaboratedPackageInfoWith Proxy

data AuditedComponent
  = HackageComponent PackageName
  | GhcComponent GHCComponent
  deriving stock (Eq, Ord, Show, Generic)

deriving stock instance Ord GHCComponent

-- | information about the elaborated package that
--   is to be looked up that we want to add  to the
--   information displayed in the advisory
type ElaboratedPackageInfoWith :: (Type -> Type) -> Type
data ElaboratedPackageInfoWith f = MkElaboratedPackageInfoWith
  { elaboratedPackageVersion :: Version
  -- ^ the version of the package that is installed
  , packageAdvisories :: f [(Advisory, Maybe Version)]
  -- ^ the advisories for some package; this is just the () type
  -- (Proxy) as long as the advisories haven't been looked up and a
  -- [Advisory] after looking up the advisories in the DB we also
  -- want to attach the newest fixed version of a given Advisory
  }
  deriving stock Generic

deriving stock instance Eq (f [(Advisory, Maybe Version)]) => (Eq (ElaboratedPackageInfoWith f))

deriving stock instance Ord (f [(Advisory, Maybe Version)]) => (Ord (ElaboratedPackageInfoWith f))

deriving stock instance Show (f [(Advisory, Maybe Version)]) => (Show (ElaboratedPackageInfoWith f))

--   FUTUREWORK(mangoiv): this could probably be done more intelligently by also
--   looking up via the version range but I don't know exacty how

-- | 'Map' to lookup the package name in the install plan that returns information
--   about the package
installPlanToLookupTable :: ElaboratedInstallPlan -> Map PackageName ElaboratedPackageInfo
installPlanToLookupTable = Map.fromList . fmap planPkgToPackageInfo . Plan.toList
 where
  planPkgToPackageInfo pkg = do
    let (PackageIdentifier {pkgName, pkgVersion}) =
          foldPlanPackage
            sourcePackageId
            elabPkgSourceId
            pkg
    (pkgName, MkElaboratedPackageInfoWith {elaboratedPackageVersion = pkgVersion, packageAdvisories = Proxy})
