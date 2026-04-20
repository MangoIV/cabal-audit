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
  lucid,
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
  version = "0.3.0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "08yxj23q6pn4cfc94dvd3f3jzmxi1kqjbla9gvwak4xh2bg1axrv";
    rev = "eb0b808c0a1fe3a57e9407dd6b91bcb1c8411345";
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
    lucid
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
    filepath
    hsec-core
    network-uri
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
    network-uri
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
