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
  lens,
  lib,
  lucid2,
  mtl,
  network-uri,
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
  tasty-hunit,
  template-haskell,
  temporary,
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
  version = "0.5.0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "1njqipmhaiyhq2xvpfsm4bl8y1xfyf17nbf81vvqf5ihq5pqnjfl";
    rev = "b44bcbc79aae5df3febfbc1bbe9c760f194612f2";
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
    lens
    lucid2
    mtl
    network-uri
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
    directory
    filepath
    hsec-core
    network-uri
    optparse-applicative
    text
    transformers
    validation-selective
  ];
  testHaskellDepends = [
    aeson
    aeson-pretty
    base
    bytestring
    Cabal-syntax
    containers
    cvss
    directory
    filepath
    hedgehog
    hsec-core
    network-uri
    osv
    pretty-simple
    prettyprinter
    process
    tasty
    tasty-golden
    tasty-hedgehog
    tasty-hunit
    temporary
    text
    time
    toml-parser
    validation-selective
  ];
  description = "Tools for working with the Haskell security advisory database";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
  mainProgram = "hsec-tools";
}
