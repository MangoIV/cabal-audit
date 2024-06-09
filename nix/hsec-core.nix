{
  mkDerivation,
  base,
  Cabal-syntax,
  cvss,
  lib,
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
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "090rlbnlval16xh5ajpn7c3mymqpxhwjfmsbkxvcrk7zn28p6w7p";
    rev = "061e2e466565de1167b5a116cdc1d621f70af268";
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
  testHaskellDepends = [base cvss tasty tasty-hunit text];
  description = "Core package representing Haskell advisories";
  license = lib.licenses.bsd3;
}
