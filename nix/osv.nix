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
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/haskell/security-advisories.git";
    sha256 = "114wj60jfdqb95h3fh3k8s0jy2nzya04rchidfdapnn0l5mf3xhn";
    rev = "fc3453aa95edb296b1e4409f53d1c1210b479fc8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [aeson base cvss purl text time];
  testHaskellDepends = [base tasty];
  description = "Open Source Vulnerability format";
  license = lib.licenses.bsd3;
}
