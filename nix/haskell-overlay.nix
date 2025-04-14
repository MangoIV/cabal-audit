{hlib, ...}: hfinal: hprev: {
  cabal-audit = hfinal.callPackage ./cabal-audit.nix {};
  osv = hfinal.callPackage ./osv.nix {};
  hsec-core = hlib.doJailbreak (hfinal.callPackage ./hsec-core.nix {Cabal-syntax = hfinal.Cabal-syntax_3_14_1_0;});
  hsec-tools = hlib.doJailbreak (hfinal.callPackage ./hsec-tools.nix {Cabal-syntax = hfinal.Cabal-syntax_3_14_1_0;});
  cvss = hfinal.callPackage ./cvss.nix {};

  extensions = hprev.extensions_0_1_0_2.override {inherit (hfinal) Cabal;};

  toml-parser = hfinal.callHackage "toml-parser" "2.0.1.0" {};

  typst = hfinal.callHackage "typst" "0.5.0.5" {};
  typst-symbols = hfinal.callHackage "typst-symbols" "0.1.6" {};
  texmath = hfinal.callHackage "texmath" "0.12.8.9" {};
}
