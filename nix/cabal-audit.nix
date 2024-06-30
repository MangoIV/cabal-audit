{
  mkDerivation,
  aeson,
  base,
  bytestring,
  Cabal,
  cabal-install,
  cabal-install-solver,
  chronos,
  colourista,
  containers,
  fused-effects,
  hsec-tools,
  hspec,
  kan-extensions,
  lib,
  optparse-applicative,
  pretty,
  process,
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
    cabal-install-solver
    chronos
    colourista
    containers
    fused-effects
    hsec-tools
    kan-extensions
    optparse-applicative
    pretty
    process
    text
    transformers
    unliftio
    uuid
    validation-selective
    vector
  ];
  executableHaskellDepends = [base];
  testHaskellDepends = [base hspec];
  description = "Checking a cabal project for security advisories";
  license = lib.licenses.bsd3;
  mainProgram = "cabal-audit";
}
