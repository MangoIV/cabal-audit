{hlib, ...}: hfinal: hprev: {
  cabal-audit = hfinal.callPackage ./cabal-audit.nix {};
  osv = hfinal.callPackage ./osv.nix {};
  hsec-core = hfinal.callPackage ./hsec-core.nix {};
  hsec-tools = hfinal.callPackage ./hsec-tools.nix {};
  cvss = hfinal.callPackage ./cvss.nix {};

  Cabal-syntax = hprev.Cabal-syntax_3_10_3_0;
  Cabal = hprev.Cabal_3_10_3_0;

  extensions = hprev.extensions.override {inherit (hfinal) Cabal;};

  toml-parser = hprev.toml-parser_2_0_1_0;
  sel = hlib.doJailbreak (hlib.markUnbroken hprev.sel);
  typst = hprev.typst_0_5_0_4;
  typst-symbols = hprev.typst-symbols_0_1_6;
  texmath = hprev.texmath_0_12_8_9;
}
