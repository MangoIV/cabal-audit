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
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "064w3hca4kpaysjp7mnfk3ggvf4l11i7a8qqjiw05j96zhs5cgnc";
    rev = "ed728d3aeb69add3f1f62d205cbb5b59a99aa4f0";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/cvss/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [base text];
  testHaskellDepends = [base tasty tasty-hunit text];
  description = "Common Vulnerability Scoring System";
  license = lib.licenses.bsd3;
}
