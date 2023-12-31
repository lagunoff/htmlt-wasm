{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/3e2237222b33d8b5754cc1cc3ee155cadd76770d.tar.gz";
}) {}
}:

let
  inherit (pkgs.haskell.lib) doJailbreak doBenchmark;

  haskellPackages = pkgs.haskell.packages.ghcHEAD.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation ({
        doCheck = false;
        doBenchmark = false;
        doHoogle = true;
        doHaddock = true;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
      } // args);

      htmlt-wasm = doBenchmark (self.callCabal2nix "htmlt-wasm" ./. {});
      semigroupoids = doJailbreak super.semigroupoids;
    };
  };
in {
  shell = {
    javascript = pkgs.mkShell {
      buildInputs = [
        pkgs.pkgsCross.ghcjs.buildPackages.haskell.compiler.ghcHEAD
      ];
    };
    native = pkgs.mkShell {
      inputsFrom = [haskellPackages.htmlt-wasm.env];
      buildInputs = [haskellPackages.ghcid haskellPackages.cabal-install];
    };
  };
}
