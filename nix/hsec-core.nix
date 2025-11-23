{
  mkDerivation,
  base,
  Cabal-syntax,
  cvss,
  fetchgit,
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
  version = "0.2.1.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "114wj60jfdqb95h3fh3k8s0jy2nzya04rchidfdapnn0l5mf3xhn";
    rev = "fc3453aa95edb296b1e4409f53d1c1210b479fc8";
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
