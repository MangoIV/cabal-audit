{
  mkDerivation,
  aeson,
  base,
  cvss,
  fetchgit,
  lib,
  purl,
  tasty,
  text,
  time,
}:
mkDerivation {
  pname = "osv";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "08yxj23q6pn4cfc94dvd3f3jzmxi1kqjbla9gvwak4xh2bg1axrv";
    rev = "eb0b808c0a1fe3a57e9407dd6b91bcb1c8411345";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [aeson base cvss purl text time];
  testHaskellDepends = [base tasty];
  description = "Open Source Vulnerability format";
  license = lib.licenses.bsd3;
}
