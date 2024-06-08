{hspkgs, ...}:
hspkgs.shellFor {
  packages = hps: [hps.cabal-audit];
  nativeBuildInputs = [hspkgs.haskell-language-server hspkgs.fourmolu];
}
