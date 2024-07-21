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
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "05j68z2p0nw8qfwlm8l7576i8m8qadps3k7445z2yzrm7pcn8qhp";
    rev = "d09058a544bf45cc0814ed9b300cd940bc263617";
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
