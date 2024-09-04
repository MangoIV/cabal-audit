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
    sha256 = "0a78ra4b6mwgzh0apvczdw9nzphkis9xqa0siicfhfb59vj60lp2";
    rev = "add617d5026bd31cad2bdbe8259b5f67381db246";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [aeson base cvss text time];
  testHaskellDepends = [base tasty tasty-hunit];
  description = "Open Source Vulnerability format";
  license = lib.licenses.bsd3;
}
