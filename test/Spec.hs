module Spec (spec) where

import Distribution.Simple
import Security.Advisories.Cabal
import Security.Advisories.Core.Advisory
import Test.Hspec

spec :: Spec
spec = describe "version ranges" do
  it "recognises single affected version correctly" do
    versionAffected (earlierVersion (mkVersion [5])) [AffectedVersionRange (mkVersion [4, 19, 1, 0]) Nothing]
  it "recognises single affected version with fix correctly" do
    versionAffected (earlierVersion (mkVersion [5])) [AffectedVersionRange (mkVersion [4, 19, 1, 0]) (Just (mkVersion [4, 20]))]
  it "recognises single affected version with fix correctly as unaffected" do
    not $
      versionAffected (earlierVersion (mkVersion [4, 19])) [AffectedVersionRange (mkVersion [4, 19, 1, 0]) (Just (mkVersion [4, 20]))]
  it "recognises single affected version with fix correctly" do
    versionAffected (earlierVersion (mkVersion [5])) [AffectedVersionRange (mkVersion [4, 19, 1, 0]) (Just (mkVersion [4, 20]))]
