{
  groff,
  haskell,
  haskellPackages,
  lib,
  makeWrapper,
  perl536,
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
        cabal-install =
          hlib.overrideCabal {
            postInstall = let
              groffOldPerl = groff.override {
                # perl 538 doesn't build in pkgsStatic at the moment
                # https://github.com/NixOS/nixpkgs/issues/295608
                perl = perl536;
              };
            in ''
              mkdir -p "$out/share/man/man1"
              "$out/bin/cabal" man --raw > "$out/share/man/man1/cabal.1"

              wrapProgram "$out/bin/cabal" \
                --prefix PATH : "${lib.makeBinPath [groffOldPerl]}"
            '';
          }
          ((hself.callHackage "cabal-install" "3.10.3.0" {
              Cabal-QuickCheck = null;
              Cabal-described = null;
              Cabal-tree-diff = null;
            })
            .overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs or [] ++ [makeWrapper];
            }));
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
