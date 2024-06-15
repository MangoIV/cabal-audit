{
  mkDerivation,
  alex,
  array,
  base,
  containers,
  happy,
  hspec,
  hspec-discover,
  lib,
  markdown-unlit,
  prettyprinter,
  template-haskell,
  text,
  time,
  transformers,
}:
mkDerivation {
  pname = "toml-parser";
  version = "2.0.1.0";
  sha256 = "03366d727abac9e30055487dbfa07973c321aff571a0de9e504337aa586697e1";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array
    base
    containers
    prettyprinter
    text
    time
    transformers
  ];
  libraryToolDepends = [alex happy];
  testHaskellDepends = [
    base
    containers
    hspec
    template-haskell
    text
    time
  ];
  testToolDepends = [hspec-discover markdown-unlit];
  description = "TOML 1.0.0 parser";
  license = lib.licenses.isc;
}
