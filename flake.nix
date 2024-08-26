{
  nixConfig.allow-import-from-derivation = true;
  description = "cabal-audit's flake";
  inputs = {
    # flake inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # flake parts
    parts.url = "github:hercules-ci/flake-parts";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    devshell.url = "github:numtide/devshell";
    # end flake parts
    # end flake inputs

    security-advisories.url = "github:haskell/security-advisories";
    security-advisories.flake = false;
    # end non-flake inputs
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
        hspkgs = pkgs.haskell.packages.ghc98.override {
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

        devShells.plain-haskell = import ./nix/haskell-shell.nix {inherit hspkgs;};

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
          cabal-audit-static = import ./nix/static.nix {inherit pkgs;};
          regen-nix = pkgs.writeShellApplication {
            name = "regen-cabal-audit-nix";
            runtimeInputs = [pkgs.cabal2nix pkgs.alejandra];
            text = let
              v = "d09058a544bf45cc0814ed9b300cd940bc263617";
              cmd = pkg: ''
                cabal2nix https://github.com/haskell/security-advisories.git \
                  --revision ${v} \
                  --subpath code/${pkg}/ > ./${pkg}.nix
              '';
            in ''
              pushd "$PRJ_ROOT"/nix
              ${lib.concatStrings (map cmd ["osv" "cvss" "hsec-core" "hsec-tools"])}
              cabal2nix ../. > ./cabal-audit.nix
              alejandra ./.
              popd
            '';
          };
        };
      };
    };
}
