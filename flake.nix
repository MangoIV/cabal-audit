{
  nixConfig.allow-import-from-derivation = true;
  description = "cabal-audit's flake";
  inputs = {
    # flake inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # flake parts
    parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    devshell.url = "github:numtide/devshell";
    # end flake parts
    # end flake inputs

    # non-flake inputs
    toml-parser.url = "https://hackage.haskell.org/package/toml-parser-2.0.0.0/toml-parser-2.0.0.0.tar.gz";
    toml-parser.flake = false;
    security-advisories.url = "github:haskell/security-advisories";
    security-advisories.flake = false;
    # end non-flake inputs
  };
  outputs = inputs:
    inputs.parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.pre-commit-hooks.flakeModule
        inputs.devshell.flakeModule
      ];

      perSystem =
        { config
        , pkgs
        , ...
        }: {
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
              hlint.enable = true;

              nixpkgs-fmt.enable = true;
              statix.enable = true;
              deadnix.enable = true;
            };
          };

          # this flake module adds a Haskell project by 
          # 1. parsing the packages in the cabal.project file 
          # 2. calling out to callCabal2nix to generate nix package definitions 
          # 3. applying overrides to the nix package set from the nixpkgs input used
          # 4. populating the devShells.<system>.<projectName> (in this case "default") with 
          #    a devShell that contains a built package-db suitable for building 
          #    the cabal project's components with cabal-install; this is later reused to build the 
          #    default devShell
          # 5. populating the packages.<system>.<packageName> with a derivation that 
          #    builds the package with name $packageName
          # 6. populating the apps.<system>.<executableName> with executables as defined by 
          #    the corresponding stanza in the *.cabal files within the cabal project
          # 7. populating the outputs with a haskellFlakeProjectModules attribute-set that 
          #    can be used to easily reuse the generated package definitions in another project 
          #    using haskell-flake
          # 
          # For more information, refer to the official documentation https://flake.parts/options/haskell-flake 
          # and run nix --allow-import-from-derivation flake show in the repository (or as usual
          # by providing a flake url)
          haskellProjects.default = {
            packages = {
              Cabal-syntax.source = "3.10.3.0";
              toml-parser.source = inputs.toml-parser;
              hsec-core.source = "${inputs.security-advisories}/code/hsec-core";
              hsec-tools.source = "${inputs.security-advisories}/code/hsec-tools";
              cvss.source = "${inputs.security-advisories}/code/cvss";
              osv.source = "${inputs.security-advisories}/code/osv";
            };
            settings = {
              cabal-audit.justStaticExecutables = true;
            };
            autoWire = [ "packages" "checks" "apps" ];
          };

          # the default devshell; this has a couple of advantages to using stdenv.mkShell; refer to 
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
            ];
            devshell = {
              name = "cabal-audit";
              packagesFrom = [ config.haskellProjects.default.outputs.devShell ];
              startup.pre-commit.text = config.pre-commit.installationScript;
            };
          };

          packages = { 
            default = config.packages.cabal-audit;
            cabal-audit-docker =  pkgs.dockerTools.buildImage {
              name = "cabal-audit-docker"; 
              tag = "latest";
              contents = [config.packages.cabal-audit pkgs.haskellPackages.ghc pkgs.git pkgs.wget];
              config = { 
                Cmd = [ "/bin/cabal-audit" ];
                WorkingDir = "/workspace";
              };
            };
          };
        };
    };
}
