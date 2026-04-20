{
  mkDerivation,
  base,
  Cabal-syntax,
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
  version = "0.3.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "08yxj23q6pn4cfc94dvd3f3jzmxi1kqjbla9gvwak4xh2bg1axrv";
    rev = "eb0b808c0a1fe3a57e9407dd6b91bcb1c8411345";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/hsec-core/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base
    Cabal-syntax
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
  license = lib.licenses.bsd3;
}
