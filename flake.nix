{
  nixConfig.allow-import-from-derivation = true;
  description = "cabal-audit's flake";
  inputs = {
    # flake inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # flake parts
    parts.url = "github:hercules-ci/flake-parts";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    devshell.url = "github:numtide/devshell";
    # end flake parts
    # end flake inputs
  };
  outputs = inputs:
    inputs.parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
      imports = [inputs.pre-commit-hooks.flakeModule inputs.devshell.flakeModule];

      perSystem = {
        config,
        pkgs,
        lib,
        ...
      }: let
        hlib = pkgs.haskell.lib.compose;
        hspkgs = pkgs.haskellPackages.override {
          overrides = import ./nix/haskell-overlay.nix {inherit hlib;};
        };
      in {
        # this flake module adds two things
        # 1. the pre-commit script which is automatically run when committing
        #    which checks formatting and lints of both Haskell and nix files
        #    the automatically run check can be bypassed with -n or --no-verify
        # 2. an attribute in the checks.<system> attrset which can be run with
        #    nix flake check which checks the same lints as the pre-commit hook
        pre-commit = {
          check.enable = true;
          settings.hooks = {
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;

            alejandra.enable = true;
            statix.enable = true;
            deadnix.enable = true;
          };
        };

        devShells.plain-haskell = hspkgs.callPackage ./nix/haskell-shell.nix {
          inherit (pkgs.haskellPackages) haskell-language-server fourmolu ghc;
        };

        # https://flake.parts/options/devshell for more information; one of the advantages is
        # the beautiful menu this provides where one can add commands that are offered and loaded
        # as part of the devShell
        devshells.default = {
          commands = [
            {
              name = "lint";
              help = "run formatting and linting of haskell and nix files in the entire repository";
              command = "pre-commit run --all";
            }
            {
              name = "regen-nix";
              help = "regenerate nix derivations for haskell packages";
              command =
                builtins.readFile (lib.getExe config.packages.regen-nix);
            }
          ];
          devshell = {
            name = "cabal-audit";
            packagesFrom = [config.devShells.plain-haskell];
            packages = [pkgs.cabal2nix pkgs.alejandra];
            startup.pre-commit.text = config.pre-commit.installationScript;
          };
        };

        packages = {
          inherit (hspkgs) cabal-audit;
          inherit (pkgs) groff;
          default = config.packages.cabal-audit;
          cabal-audit-static = pkgs.pkgsStatic.callPackage ./nix/static.nix {};
          regen-nix = pkgs.writeShellApplication {
            name = "regen-cabal-audit-nix";
            runtimeInputs = [pkgs.cabal2nix pkgs.alejandra];
            text = let
              v = "eb0b808c0a1fe3a57e9407dd6b91bcb1c8411345";
              cmd = pkg: ''
                cabal2nix https://github.com/haskell/security-advisories \
                  --revision ${v} \
                  --subpath code/${pkg}/ > ./${pkg}.nix
              '';
            in ''
              pushd "$PRJ_ROOT"/nix
              ${lib.concatStrings (map cmd ["osv" "purl" "cvss" "hsec-core" "hsec-sync" "hsec-tools"])}
              cabal2nix ../. > ./cabal-audit.nix
              alejandra ./.
              popd
            '';
          };
        };
      };
    };
}
