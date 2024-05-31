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
  sha256 = "036d33f56b0de81e85031eb2bb5357b4f36eaf3c50b22b5214258f1d76dbc679";
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
