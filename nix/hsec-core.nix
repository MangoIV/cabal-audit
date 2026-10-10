{
  mkDerivation,
  base,
  Cabal-syntax,
  containers,
  cvss,
  fetchgit,
  lib,
  network-uri,
  osv,
  pandoc-types,
  safe,
  tasty,
  tasty-hunit,
  text,
  time,
}:
mkDerivation {
  pname = "hsec-core";
  version = "0.5.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "0xyngq0r6vaa260aw5dy2ijw1vhn1az2rdl4m16hzmnz116hpl3b";
    rev = "57073681929c733854f3222e3fa7d14c05262508";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/hsec-core/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base
    Cabal-syntax
    containers
    cvss
    network-uri
    osv
    pandoc-types
    safe
    text
    time
  ];
  testHaskellDepends = [
    base
    Cabal-syntax
    cvss
    tasty
    tasty-hunit
    text
  ];
  description = "Core package representing Haskell advisories";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
