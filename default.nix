{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, duffer, duffer-json, servant-server, stdenv
      , text , warp
      }:
      mkDerivation {
        pname = "suppandi";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ aeson base duffer duffer-json servant-server text ];
        executableHaskellDepends = [ base warp ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/vaibhavsagar/suppandi#readme";
        description = "Initial project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  duffer-repo = pkgs.fetchFromGitHub {
    owner = "vaibhavsagar";
    repo = "duffer";
    rev = "818a2ef2624c193f398795bd4bcb63af8214b3c1";
    sha256 = "0lgrh5gyqfipj2ryikr2i9xb8vyazn79i2qcidyxkx064bkxrirx";
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      duffer = pkgs.haskell.lib.dontCheck (
        self.callPackage ( "${duffer-repo}/duffer" ) {}
      );

      duffer-json = pkgs.haskell.lib.dontCheck (
        self.callPackage ( "${duffer-repo}/duffer-json" ) {}
      );
    };
  };

  drv = modifiedHaskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
