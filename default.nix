let
  pkgs = import (builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs;
      ref = "nixos-21.05";
      rev = "0fd9ee1aa36ce865ad273f4f07fdc093adeb5c00";
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
  buildInputs = [ pkgs.stack pkgs.chez ];
  withHoogle = true;
} else koak
