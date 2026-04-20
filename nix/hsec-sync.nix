{
  mkDerivation,
  base,
  bytestring,
  directory,
  either,
  extra,
  fetchgit,
  filepath,
  http-client,
  lens,
  lib,
  optparse-applicative,
  tar,
  tasty,
  tasty-hunit,
  temporary,
  text,
  transformers,
  wreq,
  zlib,
}:
mkDerivation {
  pname = "hsec-sync";
  version = "0.2.0.2";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "08yxj23q6pn4cfc94dvd3f3jzmxi1kqjbla9gvwak4xh2bg1axrv";
    rev = "eb0b808c0a1fe3a57e9407dd6b91bcb1c8411345";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/hsec-sync/; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    bytestring
    directory
    either
    extra
    filepath
    http-client
    lens
    tar
    temporary
    text
    transformers
    wreq
    zlib
  ];
  executableHaskellDepends = [base optparse-applicative];
  testHaskellDepends = [
    base
    directory
    filepath
    tasty
    tasty-hunit
    temporary
  ];
  description = "Synchronize with the Haskell security advisory database";
  license = lib.licenses.bsd3;
  mainProgram = "hsec-sync";
}
