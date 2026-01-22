{
  mkDerivation,
  aeson,
  base,
  case-insensitive,
  containers,
  fetchgit,
  http-types,
  lib,
  parsec,
  tasty,
  tasty-hunit,
  text,
}:
mkDerivation {
  pname = "purl";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "114wj60jfdqb95h3fh3k8s0jy2nzya04rchidfdapnn0l5mf3xhn";
    rev = "fc3453aa95edb296b1e4409f53d1c1210b479fc8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/purl/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson
    base
    case-insensitive
    containers
    http-types
    parsec
    text
  ];
  testHaskellDepends = [base containers tasty tasty-hunit text];
  description = "Support for purl (mostly universal package url)";
  license = lib.licenses.bsd3;
}
