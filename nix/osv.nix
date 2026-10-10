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
  version = "0.2.0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories";
    sha256 = "0xyngq0r6vaa260aw5dy2ijw1vhn1az2rdl4m16hzmnz116hpl3b";
    rev = "57073681929c733854f3222e3fa7d14c05262508";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [aeson base cvss purl text time];
  testHaskellDepends = [base tasty];
  description = "Open Source Vulnerability format";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
