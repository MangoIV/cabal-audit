{
  mkDerivation,
  base,
  fetchgit,
  lib,
  tasty,
  tasty-hunit,
  text,
}:
mkDerivation {
  pname = "cvss";
  version = "0.2";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "1x4zsw56hj13j2pc75vfdkw645r061a9h4rv26c6361j7wrpyr67";
    rev = "ef73a3748f31d8df1557546b26d2d587cdacf459";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/cvss/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [base text];
  testHaskellDepends = [base tasty tasty-hunit text];
  description = "Common Vulnerability Scoring System";
  license = lib.licenses.bsd3;
}
