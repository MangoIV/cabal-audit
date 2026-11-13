{
  mkDerivation,
  base,
  Cabal-syntax,
  containers,
  cvss,
  fetchgit,
  lib,
  network-uri,
  osv,
  pandoc-types,
  safe,
  tasty,
  tasty-hunit,
  text,
  time,
}:
mkDerivation {
  pname = "hsec-core";
  version = "0.5.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "1njqipmhaiyhq2xvpfsm4bl8y1xfyf17nbf81vvqf5ihq5pqnjfl";
    rev = "b44bcbc79aae5df3febfbc1bbe9c760f194612f2";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/hsec-core/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base
    Cabal-syntax
    containers
    cvss
    network-uri
    osv
    pandoc-types
    safe
    text
    time
  ];
  testHaskellDepends = [
    base
    Cabal-syntax
    cvss
    tasty
    tasty-hunit
    text
  ];
  description = "Core package representing Haskell advisories";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
