{
  mkDerivation,
  base,
  lib,
  tasty,
  tasty-hunit,
  text,
}:
mkDerivation {
  pname = "cvss";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "090rlbnlval16xh5ajpn7c3mymqpxhwjfmsbkxvcrk7zn28p6w7p";
    rev = "061e2e466565de1167b5a116cdc1d621f70af268";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/cvss/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [base text];
  testHaskellDepends = [base tasty tasty-hunit text];
  description = "Common Vulnerability Scoring System";
  license = lib.licenses.bsd3;
}
