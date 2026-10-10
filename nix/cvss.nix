{
  mkDerivation,
  base,
  containers,
  fetchgit,
  lib,
  tasty,
  tasty-hunit,
  tasty-quickcheck,
  text,
}:
mkDerivation {
  pname = "cvss";
  version = "0.3.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "0xyngq0r6vaa260aw5dy2ijw1vhn1az2rdl4m16hzmnz116hpl3b";
    rev = "57073681929c733854f3222e3fa7d14c05262508";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/cvss/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [base containers text];
  testHaskellDepends = [
    base
    containers
    tasty
    tasty-hunit
    tasty-quickcheck
    text
  ];
  description = "Common Vulnerability Scoring System";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
