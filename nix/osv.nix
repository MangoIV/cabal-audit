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
    sha256 = "053ibpjkc4mf7a6z34cq4ii4q230njwsahp9v1byk2zf8qfz5czk";
    rev = "3ea57d54282e76866bf4219a6d01737ea51ea132";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/code/osv/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [aeson base cvss text time];
  testHaskellDepends = [base tasty tasty-hunit];
  description = "Open Source Vulnerability format";
  license = lib.licenses.bsd3;
}
