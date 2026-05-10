module Security.Advisories.SBom.Cabal (planToSBom) where

import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Distribution.Client.InstallPlan qualified as Plan
import Distribution.Client.ProjectPlanning (ElaboratedConfiguredPackage (..), ElaboratedInstallPlan)
import Distribution.InstalledPackageInfo qualified as IPI
import Distribution.License qualified as DL
import Distribution.Package (Package (..), PackageIdentifier (..), unPackageName)
import Distribution.PackageDescription qualified as PD
import Distribution.SPDX qualified as SPDX
import Distribution.Utils.ShortText qualified as ST
import Security.Advisories.SBom.Types

planToSBom :: ElaboratedInstallPlan -> (SBomMeta, V.Vector SBomMeta)
planToSBom plan = (rootSBom, allSBoms)
 where
  allPkgs = Plan.toList plan
  pkgMap = M.fromList [(getPkgId p, p) | p <- allPkgs]

  getPkgId = Plan.foldPlanPackage IPI.installedUnitId (.elabUnitId)

  -- For now, let's take the first package as root, or try to find a local one
  -- A better way would be to identify targets
  isLocal p = case p of
    Plan.Configured ecp -> ecp.elabLocalToProject
    _ -> False

  rootPkg = case filter isLocal allPkgs of
    (p : _) -> p
    [] -> case allPkgs of
      (p : _) -> p
      [] -> error "No packages found in install plan"

  rootSBom = toSBom rootPkg
  allSBoms = V.fromList [toSBom p | p <- allPkgs, getPkgId p /= getPkgId rootPkg]

  toSBom pkg =
    let info = extractInfo pkg
        deps = Plan.depends pkg
        sbomDeps = V.fromList [toDep dId | dId <- deps, M.member dId pkgMap]
     in MKSBomMeta
          { sbom'componentName = T.pack $ unPackageName $ pkgName info.pid
          , sbom'componentVersion = pkgVersion info.pid
          , sbom'componentType = if isLocal pkg then Application else Library
          , sbom'componentAuthor = info.author
          , sbom'componentDescription = info.description
          , sbom'componentLicense = info.license
          , sbom'supplierName = Nothing
          , sbom'componentPurl = mkHackagePurl (T.pack $ unPackageName $ pkgName info.pid) (Just $ pkgVersion info.pid)
          , sbom'componentDependencies = sbomDeps
          }

  toDep dId =
    let pkg = pkgMap M.! dId
        info = extractInfo pkg
     in MkComponentDependency
          { dep'type = if isLocal pkg then Application else Library
          , dep'repo = Nothing
          , dep'name = T.pack $ unPackageName $ pkgName info.pid
          , dep'version = pkgVersion info.pid
          , dep'license = Just info.license
          , dep'description = info.description
          , dep'purl = mkHackagePurl (T.pack $ unPackageName $ pkgName info.pid) (Just $ pkgVersion info.pid)
          }

  extractInfo = Plan.foldPlanPackage extractInstalled extractConfigured

  extractInstalled ipi =
    Info
      { pid = IPI.sourcePackageId ipi
      , author = T.pack $ ST.fromShortText $ IPI.author ipi
      , description = T.pack $ ST.fromShortText $ IPI.description ipi
      , license = extractLicense $ IPI.license ipi
      }

  extractConfigured ecp =
    let pd = elabPkgDescription ecp
     in Info
          { pid = packageId pd
          , author = T.pack $ ST.fromShortText $ PD.author pd
          , description = T.pack $ ST.fromShortText $ PD.description pd
          , license = PD.license pd
          }

data Info = Info
  { pid :: PackageIdentifier
  , author :: Text
  , description :: Text
  , license :: SPDX.License
  }

-- Extract SPDX license from the Either type returned by IPI.license
-- IPI.license returns Either SPDX.License Distribution.License.License
extractLicense :: Either SPDX.License DL.License -> SPDX.License
extractLicense (Left l) = l
extractLicense (Right _) = SPDX.NONE
