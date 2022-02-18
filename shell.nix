with import ./default.nix;

let
  pkgs = import (builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs;
      rev = "2128d0aa28edef51fd8fef38b132ffc0155595df";
      ref = "nixos-21.11";
    }) {};

  hpkgs = pkgs.haskell.packages.ghc8107.override {
    overrides = self: super: {
      llvm-hs = super.llvm-hs.override { llvm-config = pkgs.llvm_9; };
    };
  };

  koak = hpkgs.callCabal2nix "koak" ./. {};
  koak-static = pkgs.haskell.lib.justStaticExecutables koak;
in

hpkgs.shellFor {
  packages = p: with hpkgs; [ koak p.haskell-language-server
    p.llvm-hs
  ];
  buildInputs = [ pkgs.stack pkgs.llvm_9 ];
  propagatedBuildInputs = [ hpkgs.llvm-hs  pkgs.llvm_9 ];
  nativeBuildInputs = [ hpkgs.llvm-hs  pkgs.llvm_9 ];
  withHoogle = true;
}
