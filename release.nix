let
  pkgs = import <nixpkgs> {};
  duffer-repo = pkgs.fetchFromGitHub {
    owner = "vaibhavsagar";
    repo = "duffer";
    rev = "818a2ef2624c193f398795bd4bcb63af8214b3c1";
    sha256 = "0lgrh5gyqfipj2ryikr2i9xb8vyazn79i2qcidyxkx064bkxrirx";
  };
  duffer = pkgs.haskell.lib.dontCheck (
    pkgs.haskellPackages.callPackage ( "${duffer-repo}/duffer" ) {}
  );
  duffer-json = pkgs.haskell.lib.dontCheck (
    pkgs.haskellPackages.callPackage ( "${duffer-repo}/duffer-json" ) { inherit duffer; }
  );

in {
  suppandi = pkgs.haskellPackages.callCabal2nix "suppandi" ./. { inherit duffer duffer-json; };
}
