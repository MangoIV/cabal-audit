{ mkDerivation, base, fetchgit, lib, tasty, tasty-hunit, text }:
mkDerivation {
  pname = "cvss";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "0icgccdnfl1zjc9dzish4dvvi83fyazqdpiw3m838hwbq12wkhli";
    rev = "20ba58c5738f5fb9ae5908a08568aafb844f6938";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/cvss/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [ base tasty tasty-hunit text ];
  description = "Common Vulnerability Scoring System";
  license = lib.licenses.bsd3;
}
