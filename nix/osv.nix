{
  mkDerivation,
  aeson,
  base,
  cvss,
  lib,
  tasty,
  tasty-hunit,
  text,
  time,
}:
mkDerivation {
  pname = "osv";
  version = "0.1.0.0";
  sha256 = "0dd33c59a202a060fa295f488be8d89870e2e73521fc57be719a7417cde98287";
  libraryHaskellDepends = [aeson base cvss text time];
  testHaskellDepends = [base tasty tasty-hunit];
  description = "Open Source Vulnerability format";
  license = lib.licenses.bsd3;
}
