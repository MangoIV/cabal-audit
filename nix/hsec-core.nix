{ mkDerivation
, base
, Cabal-syntax
, cvss
, fetchgit
, lib
, osv
, pandoc-types
, safe
, tasty
, tasty-hunit
, text
, time
}:
mkDerivation {
  pname = "hsec-core";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "0icgccdnfl1zjc9dzish4dvvi83fyazqdpiw3m838hwbq12wkhli";
    rev = "20ba58c5738f5fb9ae5908a08568aafb844f6938";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/hsec-core/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base
    Cabal-syntax
    cvss
    osv
    pandoc-types
    safe
    text
    time
  ];
  testHaskellDepends = [ base cvss tasty tasty-hunit text ];
  description = "Core package representing Haskell advisories";
  license = lib.licenses.bsd3;
}
