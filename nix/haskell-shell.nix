{
  shellFor,
  haskell-language-server,
  fourmolu,
  cabal-audit,
  ...
}:
shellFor {
  packages = hps: [hps.cabal-audit];
  nativeBuildInputs = [haskell-language-server fourmolu];
  inputsFrom = [cabal-audit.env];
}
