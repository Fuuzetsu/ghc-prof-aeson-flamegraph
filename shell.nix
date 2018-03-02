{ nixpkgs ? import ./nixpkgs.nix {}
, ghc ? nixpkgs.haskell.compiler.ghc822
}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "ghc-prof-aeson-flamegraph";
  buildInputs = [ ];
  inherit ghc;
  LANG = "en_US.utf8";
}
