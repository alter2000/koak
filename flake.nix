{
  description = "Kaleidoscope Compiler";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs?rev=c7655fa3087cec120b13425f3e16bf378065fd8c;
    flake-utils.url = github:numtide/flake-utils;
    easy-hls-src.url = github:jkachmar/easy-hls-nix;
  };

  outputs = { self, nixpkgs, flake-utils, easy-hls-src }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        hpkgs = pkgs.haskell.packages.ghc8107;
        easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ "8.10.7" ]; };
        app = hpkgs.callCabal2nix "koak" ./. {};
        shell = hpkgs.shellFor {
          packages = p: with hpkgs; [ koak p.haskell-language-server p.hlint ];
          buildInputs = [ pkgs.stack ];
          withHoogle = true;
        };
      in
        rec {
          defaultPackage = app;
          defaultApp = flake-utils.lib.mkApp { drv = app; };
          devShell = shell;
        }
    );
}
