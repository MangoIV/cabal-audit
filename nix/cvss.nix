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
    sha256 = "0a78ra4b6mwgzh0apvczdw9nzphkis9xqa0siicfhfb59vj60lp2";
    rev = "add617d5026bd31cad2bdbe8259b5f67381db246";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/cvss/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [base text];
  testHaskellDepends = [base tasty tasty-hunit text];
  description = "Common Vulnerability Scoring System";
  license = lib.licenses.bsd3;
}
