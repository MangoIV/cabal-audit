{
  mkDerivation,
  aeson,
  base,
  bytestring,
  Cabal,
  cabal-install,
  chronos,
  colourista,
  containers,
  directory,
  filepath,
  fused-effects,
  hsec-core,
  hsec-sync,
  hsec-tools,
  hspec,
  http-client,
  kan-extensions,
  lib,
  optparse-applicative,
  pretty,
  process,
  sarif,
  temporary,
  text,
  transformers,
  unliftio,
  uuid,
  validation-selective,
  vector,
}:
mkDerivation {
  pname = "cabal-audit";
  version = "1.0.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    Cabal
    cabal-install
    chronos
    colourista
    containers
    directory
    filepath
    fused-effects
    hsec-core
    hsec-sync
    hsec-tools
    http-client
    kan-extensions
    optparse-applicative
    pretty
    process
    sarif
    temporary
    text
    transformers
    unliftio
    uuid
    validation-selective
    vector
  ];
  executableHaskellDepends = [base];
  testHaskellDepends = [base hspec sarif];
  description = "Checking a cabal project for security advisories";
  license = lib.licenses.bsd3;
  mainProgram = "cabal-audit";
}
