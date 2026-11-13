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
  version = "0.2.0.3";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "1njqipmhaiyhq2xvpfsm4bl8y1xfyf17nbf81vvqf5ihq5pqnjfl";
    rev = "b44bcbc79aae5df3febfbc1bbe9c760f194612f2";
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
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
  mainProgram = "hsec-sync";
}
