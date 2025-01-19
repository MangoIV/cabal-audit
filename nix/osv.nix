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
  version = "0.1.0.2";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "1x4zsw56hj13j2pc75vfdkw645r061a9h4rv26c6361j7wrpyr67";
    rev = "ef73a3748f31d8df1557546b26d2d587cdacf459";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [aeson base cvss text time];
  testHaskellDepends = [base tasty tasty-hunit];
  description = "Open Source Vulnerability format";
  license = lib.licenses.bsd3;
}
