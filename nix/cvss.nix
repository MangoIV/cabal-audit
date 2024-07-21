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
  version = "0.2";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "05j68z2p0nw8qfwlm8l7576i8m8qadps3k7445z2yzrm7pcn8qhp";
    rev = "d09058a544bf45cc0814ed9b300cd940bc263617";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/cvss/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [base text];
  testHaskellDepends = [base tasty tasty-hunit text];
  description = "Common Vulnerability Scoring System";
  license = lib.licenses.bsd3;
}
