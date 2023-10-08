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
        # shellHook = ''
        #   export CABAL_CARGS=$(ghc ${cabalCargsScript} -e 'printGhcArgs (FromCabalFile "${cabalFile}" (Just [Executable "todomvc-server"]))')
        # '';
        shellHook = ''
          export CABAL_CARGS="-isrc -hide-all-packages -iexamples/todomvc -iexamples/todomvc-server -Wall -Wno-missing-signatures -Wno-name-shadowing -Wno-unused-do-bind -Wno-partial-type-signatures -Wno-missing-home-modules -static -XAllowAmbiguousTypes -XBangPatterns -XBlockArguments -XConstraintKinds -XDataKinds -XDefaultSignatures -XDeriveAnyClass -XDeriveFunctor -XDeriveGeneric -XDerivingStrategies -XDerivingVia -XDoAndIfThenElse -XDuplicateRecordFields -XEmptyDataDecls -XFlexibleContexts -XFlexibleInstances -XForeignFunctionInterface -XFunctionalDependencies -XGADTs -XGeneralizedNewtypeDeriving -XImplicitParams -XImportQualifiedPost -XLambdaCase -XNamedFieldPuns -XOverloadedLabels -XOverloadedRecordDot -XOverloadedStrings -XPackageImports -XPartialTypeSignatures -XPatternGuards -XPolyKinds -XQuasiQuotes -XRankNTypes -XRecordWildCards -XRecursiveDo -XScopedTypeVariables -XStandaloneDeriving -XStrictData -XTupleSections -XTypeApplications -XTypeFamilies -XTypeOperators -XUndecidableInstances -XViewPatterns -package=base -package=binary -package=bytestring -package=containers -package=exceptions -package=mtl -package=text -package=wai -package=wai-websockets -package=warp -package=websockets -package=wuss -package=wai-app-static -package=http-types -package=foreign-store"
        '';
      };
    };
  };
in
  result
