{
  mkDerivation,
  aeson,
  base,
  case-insensitive,
  containers,
  fetchgit,
  http-types,
  lib,
  parsec,
  tasty,
  tasty-hunit,
  text,
}:
mkDerivation {
  pname = "purl";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "0xyngq0r6vaa260aw5dy2ijw1vhn1az2rdl4m16hzmnz116hpl3b";
    rev = "57073681929c733854f3222e3fa7d14c05262508";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/purl/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson
    base
    case-insensitive
    containers
    http-types
    parsec
    text
  ];
  testHaskellDepends = [base containers tasty tasty-hunit text];
  description = "Support for purl (mostly universal package url)";
  license = lib.licenses.bsd3;
}
