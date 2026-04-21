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
    sha256 = "08yxj23q6pn4cfc94dvd3f3jzmxi1kqjbla9gvwak4xh2bg1axrv";
    rev = "eb0b808c0a1fe3a57e9407dd6b91bcb1c8411345";
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
