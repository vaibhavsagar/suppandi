{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, base-compat, bytestring
      , containers, directory, duffer, servant, servant-server, stdenv
      , string-conversions, text, transformers, wai, warp
      }:
      mkDerivation {
        pname = "suppandi";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base base-compat bytestring containers directory duffer
          servant servant-server string-conversions text transformers wai
          warp
        ];
        executableHaskellDepends = [
          aeson base base-compat bytestring containers directory duffer
          servant servant-server string-conversions text transformers wai
          warp
        ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/vaibhavsagar/suppandi#readme";
        description = "Initial project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      duffer = pkgs.haskell.lib.dontCheck (
        self.callPackage (
          pkgs.fetchFromGitHub {
            owner = "vaibhavsagar";
            repo = "duffer";
            rev = "f8579527eadb13d8ac726cf43d340f5dffd6a161";
            sha256 = "01cfm2fjaji4826z0flnr6vdrgs98j9rfa7ac9ybc134xnsnxglj";
          }
        ) {}
      );
    };
  };

  drv = modifiedHaskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
