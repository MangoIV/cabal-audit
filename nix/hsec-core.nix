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
  version = "0.2.0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "0a78ra4b6mwgzh0apvczdw9nzphkis9xqa0siicfhfb59vj60lp2";
    rev = "add617d5026bd31cad2bdbe8259b5f67381db246";
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
