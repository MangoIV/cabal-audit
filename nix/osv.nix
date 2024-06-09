{
  mkDerivation,
  aeson,
  base,
  cvss,
  lib,
  tasty,
  tasty-hunit,
  text,
  time,
}:
mkDerivation {
  pname = "osv";
  version = "0.1.0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "090rlbnlval16xh5ajpn7c3mymqpxhwjfmsbkxvcrk7zn28p6w7p";
    rev = "061e2e466565de1167b5a116cdc1d621f70af268";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [aeson base cvss text time];
  testHaskellDepends = [base tasty tasty-hunit];
  description = "Open Source Vulnerability format";
  license = lib.licenses.bsd3;
}
