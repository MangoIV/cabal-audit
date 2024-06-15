{
  mkDerivation,
  base,
  lib,
  tasty,
  tasty-hunit,
  text,
}:
mkDerivation {
  pname = "cvss";
  version = "0.1";
  sha256 = "b9e1b3caa1f22662244c2379f277d7689ed35368cd9de57584a380068a598d0a";
  libraryHaskellDepends = [base text];
  testHaskellDepends = [base tasty tasty-hunit text];
  description = "Common Vulnerability Scoring System";
  license = lib.licenses.bsd3;
}
