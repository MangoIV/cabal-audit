{
  haskell,
  haskellPackages,
  lib,
  upx,
  ...
}: let
  hlib = haskell.lib.compose;
  hspkgs = haskellPackages.override {
    overrides =
      lib.composeExtensions
      (import ./haskell-overlay.nix {inherit hlib;})
      (hself: hsuper: {
        Cabal-syntax = hsuper.Cabal-syntax_3_10_3_0;
        Cabal = hsuper.Cabal_3_10_3_0;
        cabal-install-solver = hself.callHackage "cabal-install-solver" "3.10.3.0" {};
        cabal-install = hself.callHackage "cabal-install" "3.10.3.0" {
          Cabal-QuickCheck = null;
          Cabal-described = null;
          Cabal-tree-diff = null;
        };
      });
  };
in
  (hlib.justStaticExecutables hspkgs.cabal-audit).overrideAttrs (old: {
    postPhases = ["compressionPhase"];
    nativeBuildInputs = old.nativeBuildInputs ++ [upx];
    compressionPhase = ''
      echo "this might take a while.."
      upx --best $out/bin/cabal-audit
    '';
  })
