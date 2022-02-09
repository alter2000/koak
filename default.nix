let
  pkgs = import (builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs;
      rev = "521e4d7d13b09bc0a21976b9d19abd197d4e3b1e";
    }) {};

  hpkgs = pkgs.haskell.packages.ghc8107.override {
    overrides = self: super: {
      llvm-hs = super.llvm-hs.override { llvm-config = pkgs.llvm_9; };
    };
  };

  koak = hpkgs.callCabal2nix "koak" ./. {};
  koak-static = pkgs.haskell.lib.justStaticExecutables koak;
in

if pkgs.lib.inNixShell then hpkgs.shellFor {
  packages = p: with hpkgs; [ koak p.haskell-language-server ];
  buildInputs = [ pkgs.stack pkgs.llvm_9 ];
  withHoogle = true;
} else koak
