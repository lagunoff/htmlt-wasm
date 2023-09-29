{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/3e2237222b33d8b5754cc1cc3ee155cadd76770d.tar.gz";
}) {}
}:

let
  inherit (pkgs.haskell.lib) doJailbreak;

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

      htmlt-wasm = self.callCabal2nix "htmlt-wasm" ./. {};

      semigroupoids = doJailbreak super.semigroupoids;
    };
  };

  cabalCargsScript = ./bin/cabal-cargs.hs;
  cabalFile = ./htmlt-wasm.cabal;

  result = {
    ghc = haskellPackages;
    shell = {
      native = pkgs.mkShell {
        inputsFrom = [haskellPackages.htmlt-wasm.env];
        shellHook = ''
          export CABAL_CARGS=$(ghc ${cabalCargsScript} -e 'printGhcArgs (FromCabalFile "${cabalFile}" (Just [Executable "todomvc"]))')
        '';
      };
    };
  };
in
  result
