{ mkDerivation
, aeson
, base
, cvss
, fetchgit
, lib
, tasty
, tasty-hunit
, text
, time
}:
mkDerivation {
  pname = "osv";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "0icgccdnfl1zjc9dzish4dvvi83fyazqdpiw3m838hwbq12wkhli";
    rev = "20ba58c5738f5fb9ae5908a08568aafb844f6938";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ aeson base cvss text time ];
  testHaskellDepends = [ base tasty tasty-hunit ];
  description = "Open Source Vulnerability format";
  license = lib.licenses.bsd3;
}
