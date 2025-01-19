{
  shellFor,
  haskell-language-server,
  fourmolu,
  ...
}:
shellFor {
  packages = hps: [hps.cabal-audit];
  nativeBuildInputs = [haskell-language-server fourmolu];
}
