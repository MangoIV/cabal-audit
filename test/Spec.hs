module Spec (spec) where

import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
import Distribution.Audit
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Version (Version, mkVersion)
import Security.Advisories.Cabal
  ( AuditedComponent (..)
  , ElaboratedPackageInfoWith (..)
  , lookupAuditedComponent
  , mkGhcToolLookup
  )
import Security.Advisories.Core.Advisory (ComponentIdentifier (..))
import Security.Advisories.Core.Advisory qualified as Advisory
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

  describe "lookupAuditedComponent" do
    it "finds a Hackage package in the install-plan lookup" do
      let pkgName = mkPackageName "bytestring"
          version = mkVersion [0, 11, 5, 4]
          hackageLookup = Map.fromList [(pkgName, pkgInfo version)]
          expected = (HackageComponent pkgName, pkgInfo version)

      lookupAuditedComponent hackageLookup Map.empty (Hackage (T.pack "bytestring"))
        `shouldBe` Just expected

    it "finds a GHC tool in the GHC tool lookup" do
      let toolName = T.pack "ghc"
          version = mkVersion [9, 10, 3]
          ghcLookup = Map.fromList [(toolName, version)]
          expected = (GhcComponent toolName, pkgInfo version)

      lookupAuditedComponent Map.empty ghcLookup (GHC Advisory.GHCCompiler)
        `shouldBe` Just expected

    it "finds GHCi in the GHC tool lookup" do
      let toolName = T.pack "ghci"
          version = mkVersion [9, 10, 3]
          ghcLookup = Map.fromList [(toolName, version)]
          expected = (GhcComponent toolName, pkgInfo version)

      lookupAuditedComponent Map.empty ghcLookup (GHC Advisory.GHCi)
        `shouldBe` Just expected

    it "returns Nothing for a missing Hackage package" do
      lookupAuditedComponent Map.empty Map.empty (Hackage (T.pack "does-not-exist"))
        `shouldBe` Nothing

    it "returns Nothing for a missing GHC tool" do
      lookupAuditedComponent Map.empty Map.empty (GHC Advisory.GHCi)
        `shouldBe` Nothing

    it "does not match mixed-case GHC tool keys" do
      let version = mkVersion [9, 10, 3]
          ghcLookup = Map.fromList [(T.pack "GHC", version)]

      lookupAuditedComponent Map.empty ghcLookup (GHC Advisory.GHCCompiler)
        `shouldBe` Nothing

  describe "mkGhcToolLookup" do
    it "contains GHC and GHCi" do
      let version = mkVersion [9, 10, 3]
      mkGhcToolLookup version
        `shouldBe` Map.fromList
          [ ("ghc", mkVersion [9, 10, 3])
          , ("ghc-iserv", mkVersion [9, 10, 3])
          , ("ghc-pkg", mkVersion [9, 10, 3])
          , ("ghci", mkVersion [9, 10, 3])
          , ("haddock", mkVersion [9, 10, 3])
          , ("hp2ps", mkVersion [9, 10, 3])
          , ("hpc", mkVersion [9, 10, 3])
          , ("hsc2hs", mkVersion [9, 10, 3])
          , ("rts", mkVersion [9, 10, 3])
          , ("runghc", mkVersion [9, 10, 3])
          ]

pkgInfo :: Version -> ElaboratedPackageInfoWith Proxy
pkgInfo version =
  MkElaboratedPackageInfoWith
    { elaboratedPackageVersion = version
    , packageAdvisories = Proxy
    }
