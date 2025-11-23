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
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "114wj60jfdqb95h3fh3k8s0jy2nzya04rchidfdapnn0l5mf3xhn";
    rev = "fc3453aa95edb296b1e4409f53d1c1210b479fc8";
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
