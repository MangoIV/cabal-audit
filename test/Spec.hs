module Spec (spec) where

import Data.SARIF qualified as Sarif
import Distribution.Audit (chooseSarifLocationForPackages)
import Test.Hspec

spec :: Spec
spec = do
  describe "chooseSarifLocationForPackages" $ do
    it "finds process in a single cabal file" $ do
      (file, region) <-
        chooseSarifLocationForPackages
          "test/assets/sarif-location/single-package"
          ["process"]
      file `shouldBe` "myTool.cabal"
      region `shouldBe` Sarif.MkRegion 11 7 11 14

    it "prefers cabal.project.freeze when it contains the package" $ do
      (file, region) <-
        chooseSarifLocationForPackages
          "test/assets/sarif-location/freeze-only"
          ["process"]
      file `shouldBe` "cabal.project.freeze"
      region `shouldBe` Sarif.MkRegion 5 7 5 14

    it "uses cabal.project when no freeze file" $ do
      (file, region) <-
        chooseSarifLocationForPackages
          "test/assets/sarif-location/project-only"
          ["process"]
      file `shouldBe` "cabal.project"
      region `shouldBe` Sarif.MkRegion 4 3 4 10

    it "chooses the matching cabal file when multiple cabal files exist" $ do
      (file, region) <-
        chooseSarifLocationForPackages
          "test/assets/sarif-location/multi-package"
          ["process"]
      file `shouldBe` "b.cabal"
      region `shouldBe` Sarif.MkRegion 11 7 11 14

    it "falls back when no explicit package occurrence is found" $ do
      (file, region) <-
        chooseSarifLocationForPackages
          "test/assets/sarif-location/no-match"
          ["cryptonite"]
      file `shouldBe` "transitive-only.cabal"
      region `shouldBe` Sarif.MkRegion 1 1 1 1
