module Spec (spec) where

import Distribution.Audit
import Distribution.Types.PackageName (mkPackageName)
import Security.Advisories.Cabal (AuditedComponent (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "renderAuditedComponent" do
    it "renders Hackage component as bare package name" do
      renderAuditedComponent (HackageComponent (mkPackageName "base"))
        `shouldBe` "base"

    it "renders GHC component as bare tool name" do
      renderAuditedComponent (GhcComponent "ghc")
        `shouldBe` "ghc"

  describe "renderAuditedComponentLabel" do
    it "renders Hackage component as dependency label" do
      renderAuditedComponentLabel (HackageComponent (mkPackageName "base"))
        `shouldBe` "dependency base"

    it "renders GHC component as GHC tool label" do
      renderAuditedComponentLabel (GhcComponent "ghci")
        `shouldBe` "GHC tool ghci"
