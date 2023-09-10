{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    ghc-wasm-meta-src = builtins.fetchTarball "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz";
  };
}
