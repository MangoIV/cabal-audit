{
  mkDerivation,
  aeson,
  base,
  cvss,
  fetchgit,
  lib,
  purl,
  tasty,
  text,
  time,
}:
mkDerivation {
  pname = "osv";
  version = "0.2.0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "1njqipmhaiyhq2xvpfsm4bl8y1xfyf17nbf81vvqf5ihq5pqnjfl";
    rev = "b44bcbc79aae5df3febfbc1bbe9c760f194612f2";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [aeson base cvss purl text time];
  testHaskellDepends = [base tasty];
  description = "Open Source Vulnerability format";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
