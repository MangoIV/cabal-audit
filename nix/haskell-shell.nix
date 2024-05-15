{ pkgs, hspkgs, ... }: hspkgs.shellFor {
  packages = hps: [ hps.cabal-audit ];

  nativeBuildInputs = [
    pkgs.haskell-language-server
    pkgs.haskellPackages.fourmolu
  ];
}
