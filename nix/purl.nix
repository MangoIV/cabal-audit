{
  mkDerivation,
  aeson,
  base,
  case-insensitive,
  containers,
  fetchgit,
  http-types,
  lib,
  parsec,
  tasty,
  tasty-hunit,
  text,
}:
mkDerivation {
  pname = "purl";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "1njqipmhaiyhq2xvpfsm4bl8y1xfyf17nbf81vvqf5ihq5pqnjfl";
    rev = "b44bcbc79aae5df3febfbc1bbe9c760f194612f2";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/purl/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson
    base
    case-insensitive
    containers
    http-types
    parsec
    text
  ];
  testHaskellDepends = [base containers tasty tasty-hunit text];
  description = "Support for purl (mostly universal package url)";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
