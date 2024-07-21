{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Security.Advisories.Cabal
  ( matchAdvisoriesForPlan
  , ElaboratedPackageInfo (..)
  , installPlanToLookupTable
  , toMapOn
  , versionAffected
  )
where

import Data.Foldable (Foldable (foldl'))
import Data.Kind (Type)
import Data.Map (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Monoid (Alt (Alt, getAlt))
import Data.Text qualified as T
import Distribution.Client.InstallPlan (foldPlanPackage)
import Distribution.Client.InstallPlan qualified as Plan
import Distribution.Client.ProjectPlanning (ElaboratedInstallPlan, elabPkgSourceId)
import Distribution.InstalledPackageInfo (sourcePackageId)
import Distribution.Package (PackageIdentifier (PackageIdentifier, pkgName, pkgVersion), PackageName, mkPackageName)
import Distribution.Version (Bound (..), LowerBound (LowerBound), UpperBound (UpperBound), Version, VersionInterval (..), VersionRange, asVersionIntervals, thisVersion)
import GHC.Generics (Generic)
import Security.Advisories
  ( Advisory (advisoryAffected)
  , Affected (Affected, affectedPackage, affectedVersions)
  , AffectedVersionRange (..)
  )

toMapOn :: (Ord b, Foldable f) => (a -> (b, c)) -> f a -> Map b c
toMapOn f = foldl' (\mp a -> uncurry Map.insert (f a) mp) Map.empty

-- | for a legal 'VersionRange', is the range affected by and of the
-- 'AffectedVersionRange's
versionAffected :: VersionRange -> [AffectedVersionRange] -> Bool
versionAffected (asVersionIntervals -> intervals) ranges = or do
  AffectedVersionRange introduced mfixed <- ranges

  VersionInterval (LowerBound lv lbound) (UpperBound uv ubound) <- intervals
  let lc = case lbound of ExclusiveBound -> (<); InclusiveBound -> (<=)
      uc = case ubound of ExclusiveBound -> (>); InclusiveBound -> (>=)

  pure $
    lv `lc` introduced && maybe True (uv `uc`) mfixed

-- | for a given 'ElaboratedInstallPlan' and a list of advisories, construct a map of advisories
--   and packages within the install plan that are affected by them
matchAdvisoriesForPlan
  :: Map PackageName VersionRange
  -- ^ package names paired with their legal version range
  -> [Advisory]
  -- ^ the advisories as discovered in some advisory dir
  -> Map PackageName ElaboratedPackageInfo
matchAdvisoriesForPlan plan = foldr advise Map.empty
 where
  advise :: Advisory -> Map PackageName ElaboratedPackageInfo -> Map PackageName ElaboratedPackageInfo
  advise adv = do
    let fixVersion :: [AffectedVersionRange] -> Maybe Version
        fixVersion = getAlt . foldMap (Alt . affectedVersionRangeFixed)

        advPkgs :: [(PackageName, ElaboratedPackageInfo)]
        advPkgs = flip mapMaybe adv.advisoryAffected \Affected {affectedPackage, affectedVersions} -> do
          let pkgn = mkPackageName (T.unpack affectedPackage)
          range <- plan !? pkgn
          if versionAffected range affectedVersions
            then Just (pkgn, MkElaboratedPackageInfo {elaboratedPackageVersionRange = range, packageAdvisories = [(adv, fixVersion affectedVersions)]})
            else Nothing

    flip (foldr . uncurry $ Map.insertWith combinedElaboratedPackageInfos) advPkgs

  combinedElaboratedPackageInfos
    MkElaboratedPackageInfo {elaboratedPackageVersionRange = ver1, packageAdvisories = advs1}
    MkElaboratedPackageInfo {packageAdvisories = advs2} =
      MkElaboratedPackageInfo {elaboratedPackageVersionRange = ver1, packageAdvisories = advs1 <> advs2}

-- | information about the elaborated package that
--   is to be looked up that we want to add  to the
--   information displayed in the advisory
type ElaboratedPackageInfo :: Type
data ElaboratedPackageInfo = MkElaboratedPackageInfo
  { elaboratedPackageVersionRange :: VersionRange
  -- ^ the versions of the package that are allowed
  , packageAdvisories :: [(Advisory, Maybe Version)]
  -- ^ the advisories for some package
  }
  deriving stock (Show, Generic)

--   FUTUREWORK(mangoiv): this could probably be done more intelligently by also
--   looking up via the version range but I don't know exacty how

-- | 'Map' to lookup the package name in the install plan that returns information
--   about the package
installPlanToLookupTable :: ElaboratedInstallPlan -> Map PackageName VersionRange
installPlanToLookupTable = Map.fromList . fmap planPkgToPackageInfo . Plan.toList
 where
  planPkgToPackageInfo pkg =
    let (PackageIdentifier {pkgName, pkgVersion}) =
          foldPlanPackage
            sourcePackageId
            elabPkgSourceId
            pkg
     in (pkgName, thisVersion pkgVersion)
