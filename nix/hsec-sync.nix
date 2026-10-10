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
    sha256 = "0xyngq0r6vaa260aw5dy2ijw1vhn1az2rdl4m16hzmnz116hpl3b";
    rev = "57073681929c733854f3222e3fa7d14c05262508";
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
