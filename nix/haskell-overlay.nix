{hlib, ...}: hfinal: hprev: {
  cabal-audit = hfinal.callPackage ./cabal-audit.nix {};
  osv = hfinal.callPackage ./osv.nix {};
  purl = hfinal.callPackage ./purl.nix {};
  hsec-core = hfinal.callPackage ./hsec-core.nix {};
  hsec-sync = hfinal.callPackage ./hsec-sync.nix {};
  hsec-tools = hlib.doJailbreak (hfinal.callPackage ./hsec-tools.nix {});
  cvss = hfinal.callPackage ./cvss.nix {};

  Cabal-syntax = hprev.Cabal-syntax_3_16_0_0;
  Cabal = hprev.Cabal_3_16_0_0;
}
