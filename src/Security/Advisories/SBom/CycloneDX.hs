module Security.Advisories.SBom.CycloneDX where

import Chronos (Datetime, encodeIso8601)
import Data.Aeson
import Data.UUID qualified as UUID
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
serializeToCycloneDX :: CycloneDXInfo -> SBomMeta -> Value
serializeToCycloneDX info meta =
  object
    [ "bomFormat" .= String "CycloneDX"
    , "specVersion" .= String "1.6"
    , "serialNumber" .= String ("urn:uuid:" <> UUID.toText info.cyclonedx'freshUUID)
    , "version" .= Number (fromIntegral info.cyclonedx'sbomVersion)
    , "metadata"
        .= object
          [ "timestamp" .= String (encodeIso8601 info.cyclonedx'currentTime)
          , "component"
              .= object
                [ "name" .= String meta.sbom'componentName
                , "type" .= String (prettyComponentType meta.sbom'componentType)
                , "bom-ref" .= String (mkBomRef meta.sbom'supplierName meta.sbom'componentName meta.sbom'componentType meta.sbom'componentVersion)
                , "authors" .= Array [object ["name" .= meta.sbom'componentAuthor]]
                , "version" .= String (prettyVersion meta.sbom'componentVersion)
                , "description" .= String meta.sbom'componentDescription
                , "licenses" .= Array case meta.sbom'componentLicense of NONE -> []; License expr -> [String (prettyLicense expr)]
                -- FUTUREWORK(mangoiv): it is possible to add externalReferences and render the SourceRepo information of the cabal package heere
                -- https://hackage.haskell.org/package/Cabal-syntax-3.12.0.0/docs/Distribution-Types-SourceRepo.html#t:SourceRepo
                ]
          ]
    , "dependencies" .= Array []
    ]
