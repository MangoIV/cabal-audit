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
    sha256 = "16n0cck9i7f2ws6rmmrlcqw2yp18v0xsg6w56giv29vx90yadqp2";
    rev = "1f8e5699f7bc96dcc069d3675ef6b95710ccfbde";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [aeson base cvss text time];
  testHaskellDepends = [base tasty tasty-hunit];
  description = "Open Source Vulnerability format";
  license = lib.licenses.bsd3;
}
