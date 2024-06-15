{pkgs, ...}: let
  inherit (pkgs) pkgsStatic;
  hlib = pkgsStatic.haskell.lib.compose;
  hspkgs = pkgsStatic.haskellPackages.override {
    overrides =
      pkgs.lib.composeExtensions (import ./haskell-overlay.nix {inherit hlib;})
      (_hself: hsuper: {
        cabal-install =
          hlib.overrideCabal {
            postInstall = let
              groff = pkgsStatic.groff.override {
                # perl 538 doesn't build in pkgsStatic at the moment
                # https://github.com/NixOS/nixpkgs/issues/295608
                perl = pkgsStatic.perl536;
              };
            in ''
              mkdir -p "$out/share/man/man1"
              "$out/bin/cabal" man --raw > "$out/share/man/man1/cabal.1"

              wrapProgram "$out/bin/cabal" \
                --prefix PATH : "${pkgs.lib.makeBinPath [groff]}"
            '';
          }
          hsuper.cabal-install;
      });
  };
in
  hlib.justStaticExecutables hspkgs.cabal-audit
