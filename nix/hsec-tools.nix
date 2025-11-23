{
  mkDerivation,
  aeson,
  aeson-pretty,
  atom-conduit,
  base,
  bytestring,
  Cabal-syntax,
  commonmark,
  commonmark-pandoc,
  conduit,
  conduit-extra,
  containers,
  cvss,
  data-default,
  directory,
  extra,
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
  pretty,
  pretty-simple,
  prettyprinter,
  process,
  refined,
  resourcet,
  tasty,
  tasty-golden,
  tasty-hedgehog,
  template-haskell,
  text,
  time,
  toml-parser,
  transformers,
  uri-bytestring,
  validation-selective,
  xml-conduit,
}:
mkDerivation {
  pname = "hsec-tools";
  version = "0.3.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "114wj60jfdqb95h3fh3k8s0jy2nzya04rchidfdapnn0l5mf3xhn";
    rev = "fc3453aa95edb296b1e4409f53d1c1210b479fc8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/hsec-tools/; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    atom-conduit
    base
    bytestring
    Cabal-syntax
    commonmark
    commonmark-pandoc
    conduit
    conduit-extra
    containers
    cvss
    data-default
    directory
    extra
    file-embed
    filepath
    hsec-core
    lucid
    mtl
    osv
    pandoc
    pandoc-types
    parsec
    pretty
    prettyprinter
    process
    refined
    resourcet
    template-haskell
    text
    time
    toml-parser
    uri-bytestring
    validation-selective
    xml-conduit
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
    text
    time
    toml-parser
  ];
  description = "Tools for working with the Haskell security advisory database";
  license = lib.licenses.bsd3;
  mainProgram = "hsec-tools";
}
