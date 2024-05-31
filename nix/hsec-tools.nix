{
  mkDerivation,
  aeson,
  aeson-pretty,
  base,
  bytestring,
  Cabal-syntax,
  commonmark,
  commonmark-pandoc,
  containers,
  cvss,
  directory,
  extra,
  feed,
  filepath,
  hsec-core,
  lib,
  lucid,
  mtl,
  optparse-applicative,
  osv,
  pandoc-types,
  parsec,
  pathwalk,
  pretty-simple,
  process,
  safe,
  tasty,
  tasty-golden,
  tasty-hunit,
  text,
  time,
  toml-parser,
  validation-selective,
}:
mkDerivation {
  pname = "hsec-tools";
  version = "0.1.0.0";
  sha256 = "06c1b1278cbceda0059074a98c3fce2691ccecb25f8fcdcea97218c2d7f388e0";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    Cabal-syntax
    commonmark
    commonmark-pandoc
    containers
    cvss
    directory
    extra
    feed
    filepath
    hsec-core
    lucid
    mtl
    osv
    pandoc-types
    parsec
    pathwalk
    process
    safe
    text
    time
    toml-parser
    validation-selective
  ];
  executableHaskellDepends = [
    aeson
    base
    bytestring
    Cabal-syntax
    filepath
    hsec-core
    optparse-applicative
    text
    validation-selective
  ];
  testHaskellDepends = [
    aeson-pretty
    base
    Cabal-syntax
    cvss
    directory
    hsec-core
    pretty-simple
    tasty
    tasty-golden
    tasty-hunit
    text
    time
  ];
  description = "Tools for working with the Haskell security advisory database";
  license = lib.licenses.bsd3;
  mainProgram = "hsec-tools";
}
