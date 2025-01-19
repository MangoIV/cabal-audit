{
  mkDerivation,
  base,
  Cabal-syntax,
  cvss,
  fetchgit,
  lib,
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
  version = "0.2.0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "1x4zsw56hj13j2pc75vfdkw645r061a9h4rv26c6361j7wrpyr67";
    rev = "ef73a3748f31d8df1557546b26d2d587cdacf459";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/hsec-core/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base
    Cabal-syntax
    cvss
    osv
    pandoc-types
    safe
    text
    time
  ];
  testHaskellDepends = [base cvss tasty tasty-hunit text];
  description = "Core package representing Haskell advisories";
  license = lib.licenses.bsd3;
}
