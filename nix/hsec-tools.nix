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
  data-default,
  directory,
  extra,
  feed,
  fetchgit,
  file-embed,
  filepath,
  hedgehog,
  hsec-core,
  lib,
  lucid,
  mtl,
  optparse-applicative,
  osv,
  pandoc,
  pandoc-types,
  parsec,
  pathwalk,
  pretty,
  pretty-simple,
  prettyprinter,
  process,
  safe,
  tasty,
  tasty-golden,
  tasty-hedgehog,
  tasty-hunit,
  template-haskell,
  text,
  time,
  toml-parser,
  transformers,
  validation-selective,
}:
mkDerivation {
  pname = "hsec-tools";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "05j68z2p0nw8qfwlm8l7576i8m8qadps3k7445z2yzrm7pcn8qhp";
    rev = "d09058a544bf45cc0814ed9b300cd940bc263617";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/hsec-tools/; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    Cabal-syntax
    commonmark
    commonmark-pandoc
    containers
    cvss
    data-default
    directory
    extra
    feed
    file-embed
    filepath
    hsec-core
    lucid
    mtl
    osv
    pandoc
    pandoc-types
    parsec
    pathwalk
    pretty
    prettyprinter
    process
    safe
    template-haskell
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
    transformers
    validation-selective
  ];
  testHaskellDepends = [
    aeson-pretty
    base
    Cabal-syntax
    containers
    cvss
    directory
    hedgehog
    hsec-core
    osv
    pretty-simple
    prettyprinter
    tasty
    tasty-golden
    tasty-hedgehog
    tasty-hunit
    text
    time
    toml-parser
  ];
  description = "Tools for working with the Haskell security advisory database";
  license = lib.licenses.bsd3;
  mainProgram = "hsec-tools";
}
