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
  fetchgit,
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
  transformers,
  validation-selective,
}:
mkDerivation {
  pname = "hsec-tools";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "090rlbnlval16xh5ajpn7c3mymqpxhwjfmsbkxvcrk7zn28p6w7p";
    rev = "061e2e466565de1167b5a116cdc1d621f70af268";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/hsec-tools/; echo source root reset to $sourceRoot";
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
    transformers
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
