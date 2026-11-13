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
        # cabal-install-3.16.1.0 needs Cabal/Cabal-syntax 3.16.1.0, which
        # are newer than the reserved nixpkgs packages; pull them from
        # hackage. Setup.hs links the boot Cabal-syntax 3.8 alongside them -
        # harmless, but the builder's strict configure check aborts on it.
        Cabal-syntax =
          hlib.overrideCabal (_: {
            allowInconsistentDependencies = true;
          })
          (
            hself.callHackageDirect {
              pkg = "Cabal-syntax";
              ver = "3.16.1.0";
              sha256 = "sha256-BbUQ4d221OlNLo+BoFzWDaaSZ5f2242xFV4199Iqw78=";
            } {}
          );
        Cabal =
          hlib.overrideCabal (old: {
            allowInconsistentDependencies = true;
            # Cabal declares unix >= 2.8.6 for withFilePath, which base-4.17's
            # System.Posix.Internals already provides; the static GHC 9.4
            # toolchain ships unix-2.7.3.
            preConfigure =
              (old.preConfigure or "")
              + ''
                sed -i 's/>= 2.8.6.0 && < 2.9/>= 2.7.0.1/' Cabal.cabal
              '';
          })
          (
            hself.callHackageDirect {
              pkg = "Cabal";
              ver = "3.16.1.0";
              sha256 = "sha256-d1JZjKk75l22NNyY4cdWjUVnKIvREwBnSvCuwnLfJT4=";
            } {}
          );
        semaphore-compat =
          hlib.overrideCabal (old: {
            preConfigure =
              (old.preConfigure or "")
              + ''
                sed -i 's/semWaitInterruptible/semThreadWait/g' src/System/Semaphore.hs
                # unix-2.7's semThreadWait :: IO (); only the IO Bool site gets wrapped.
                # file has CRLF endings; no $ anchors, indent distinguishes the sites.
                sed -i 's/^\(          Posix\.semThreadWait sem\)/\1 >> pure True/' src/System/Semaphore.hs
                sed -i 's/>= 2.8.1.0 && < 2.9/>= 2.7.1.0/' *.cabal
              '';
          })
          hsuper.semaphore-compat;
        cabal-install-solver =
          hlib.overrideCabal (_: {
            allowInconsistentDependencies = true;
          })
          (
            hself.callHackageDirect {
              pkg = "cabal-install-solver";
              ver = "3.16.1.0";
              sha256 = "sha256-BjRXWha4kiedo/Fi3qr2LXixvuQvXD62XJ4pLZ6yq3c=";
            } {}
          );
        cabal-install =
          hlib.overrideCabal (_: {
            allowInconsistentDependencies = true;
          })
          (
            hself.callHackageDirect {
              pkg = "cabal-install";
              ver = "3.16.1.0";
              sha256 = "sha256-6ssSeM1oeXVCLR5/HPxYEUkcEWU8XxlqZiBl1JHbMbw=";
            } {
              Cabal-QuickCheck = null;
              Cabal-described = null;
              Cabal-tree-diff = null;
              Cabal-tests = null;
            }
          );
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
