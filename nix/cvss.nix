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
    sha256 = "1njqipmhaiyhq2xvpfsm4bl8y1xfyf17nbf81vvqf5ihq5pqnjfl";
    rev = "b44bcbc79aae5df3febfbc1bbe9c760f194612f2";
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
