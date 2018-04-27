let
  pkgs = import <nixpkgs> {};
  duffer-repo = pkgs.fetchFromGitHub {
    owner = "vaibhavsagar";
    repo = "duffer";
    rev = "818a2ef2624c193f398795bd4bcb63af8214b3c1";
    sha256 = "0lgrh5gyqfipj2ryikr2i9xb8vyazn79i2qcidyxkx064bkxrirx";
  };
  haskellPackages = pkgs.haskellPackages.extend (self: super: {
    bytestring-tree-builder = pkgs.haskell.lib.doJailbreak super.bytestring-tree-builder;
    duffer = pkgs.haskell.lib.dontCheck (
      self.callPackage ( "${duffer-repo}/duffer" ) {}
    );
    duffer-json = pkgs.haskell.lib.dontCheck (
      self.callPackage ( "${duffer-repo}/duffer-json" ) {}
    );
  });

in {
  suppandi = let
    drv = haskellPackages.callCabal2nix "suppandi" ./. {};
    in if pkgs.lib.inNixShell then drv.env else drv;
}
