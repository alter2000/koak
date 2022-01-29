let
  pkgs = import (builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs;
      rev = "c7655fa3087cec120b13425f3e16bf378065fd8c";
    }) {};

  hpkgs = pkgs.haskell.packages.ghc8107;

  koak = hpkgs.callCabal2nix "koak" ./. {};
  koak2-static = pkgs.haskell.lib.justStaticExecutables koak;
  koak-static = pkgs.haskell.lib.overrideCabal koak (old: {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
      "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
    ];
  });
in

if pkgs.lib.inNixShell then hpkgs.shellFor {
  packages = p: with hpkgs; [ koak p.haskell-language-server ];
  buildInputs = [ pkgs.stack ];
  withHoogle = true;
} else koak
