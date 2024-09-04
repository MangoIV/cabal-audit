{hlib, ...}: hfinal: hprev: {
  cabal-audit = hfinal.callPackage ./cabal-audit.nix {};
  osv = hfinal.callPackage ./osv.nix {};
  hsec-core = hlib.doJailbreak (hfinal.callPackage ./hsec-core.nix {});
  hsec-tools = hlib.doJailbreak (hfinal.callPackage ./hsec-tools.nix {});
  cvss = hfinal.callPackage ./cvss.nix {};

  Cabal-syntax = hprev.Cabal-syntax_3_12_1_0;
  Cabal = hprev.Cabal_3_12_1_0;

  extensions = hprev.extensions_0_1_0_2.override {inherit (hfinal) Cabal;};

  ormolu = hlib.doJailbreak (hprev.ormolu.override {inherit (hfinal) Cabal-syntax;});
  fourmolu = hlib.doJailbreak (hprev.fourmolu.override {inherit (hfinal) Cabal-syntax;});

  toml-parser = hprev.toml-parser_2_0_1_0;
  sel = hlib.doJailbreak (hlib.markUnbroken hprev.sel);
  typst = hprev.typst_0_5_0_5;
  typst-symbols = hprev.typst-symbols_0_1_6;
  texmath = hprev.texmath_0_12_8_9;
}
