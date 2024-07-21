{
  mkDerivation,
  aeson,
  base,
  cvss,
  fetchgit,
  lib,
  tasty,
  tasty-hunit,
  text,
  time,
}:
mkDerivation {
  pname = "osv";
  version = "0.1.0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "05j68z2p0nw8qfwlm8l7576i8m8qadps3k7445z2yzrm7pcn8qhp";
    rev = "d09058a544bf45cc0814ed9b300cd940bc263617";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [aeson base cvss text time];
  testHaskellDepends = [base tasty tasty-hunit];
  description = "Open Source Vulnerability format";
  license = lib.licenses.bsd3;
}
