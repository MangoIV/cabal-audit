{
  mkDerivation,
  base,
  fetchgit,
  lib,
  tasty,
  tasty-hunit,
  text,
}:
mkDerivation {
  pname = "cvss";
  version = "0.2.0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "08yxj23q6pn4cfc94dvd3f3jzmxi1kqjbla9gvwak4xh2bg1axrv";
    rev = "eb0b808c0a1fe3a57e9407dd6b91bcb1c8411345";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/cvss/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [base text];
  testHaskellDepends = [base tasty tasty-hunit text];
  description = "Common Vulnerability Scoring System";
  license = lib.licenses.bsd3;
}
