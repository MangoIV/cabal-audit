{hlib, ...}: hfinal: hprev: {
  cabal-audit = hfinal.callPackage ./cabal-audit.nix {};
  osv = hfinal.callPackage ./osv.nix {};
  hsec-core = hfinal.callPackage ./hsec-core.nix {};
  hsec-tools = hfinal.callPackage ./hsec-tools.nix {};
  cvss = hfinal.callPackage ./cvss.nix {};

  toml-parser = hfinal.callPackage ./toml-parser.nix {};

  Cabal-syntax = hprev.Cabal-syntax_3_10_3_0;
  Cabal = hprev.Cabal_3_10_3_0;
  sel = hlib.doJailbreak (hlib.markUnbroken hprev.sel);

  extensions = hprev.extensions.override {inherit (hfinal) Cabal;};
}
