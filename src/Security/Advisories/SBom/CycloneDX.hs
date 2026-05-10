module Security.Advisories.SBom.CycloneDX where

import Chronos (Datetime, encodeIso8601)
import Data.Aeson
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Distribution.SPDX (License (..))
import Numeric.Natural (Natural)
import Security.Advisories.SBom.Types

data CycloneDXInfo = MkCycloneDXInfo
  { cyclonedx'sbomVersion :: Natural
  -- ^ the generation of the sbom, this should be the generation of the last generated sbom + 1
  , cyclonedx'freshUUID :: UUID.UUID
  -- ^ the uri of the new cyclone dx SBom
  , cyclonedx'currentTime :: Datetime
  -- ^ the uri of the new cyclone dx SBom
  }

-- | serializeds some SBomMeta to a Value; e.g. for a library of a cabal package
serializeToCycloneDX :: CycloneDXInfo -> SBomMeta -> Vector SBomMeta -> Value
serializeToCycloneDX info root components =
  object
    [ "bomFormat" .= String "CycloneDX"
    , "specVersion" .= String "1.7"
    , "serialNumber" .= String ("urn:uuid:" <> UUID.toText info.cyclonedx'freshUUID)
    , "version" .= Number (fromIntegral info.cyclonedx'sbomVersion)
    , "metadata"
        .= object
          [ "timestamp" .= String (encodeIso8601 info.cyclonedx'currentTime)
          , "component" .= serializeComponent root
          ]
    , "components" .= Array (V.map serializeComponent components)
    , "dependencies" .= Array (V.map serializeDependency (V.cons root components))
    ]

serializeComponent :: SBomMeta -> Value
serializeComponent meta =
  object
    [ "name" .= meta.sbom'componentName
    , "type" .= String (prettyComponentType meta.sbom'componentType)
    , "bom-ref" .= bomRef
    , "authors" .= [object ["name" .= meta.sbom'componentAuthor] | not (T.null meta.sbom'componentAuthor)]
    , "version" .= prettyVersion @T.Text meta.sbom'componentVersion
    , "description" .= meta.sbom'componentDescription
    , "licenses" .= serializeLicense meta.sbom'componentLicense
    , "purl" .= purlText meta.sbom'componentPurl
    ]
 where
  bomRef = mkBomRef meta.sbom'supplierName meta.sbom'componentName meta.sbom'componentType meta.sbom'componentVersion

serializeLicense :: License -> [Value]
serializeLicense NONE = []
serializeLicense (License expr) = [object ["expression" .= prettyLicense expr]]

serializeDependency :: SBomMeta -> Value
serializeDependency meta =
  object
    [ "ref" .= bomRef
    , "dependsOn" .= Array (V.map (toJSON . depRef) meta.sbom'componentDependencies)
    ]
 where
  bomRef = mkBomRef meta.sbom'supplierName meta.sbom'componentName meta.sbom'componentType meta.sbom'componentVersion
  depRef dep = mkBomRef dep.dep'repo dep.dep'name dep.dep'type dep.dep'version
