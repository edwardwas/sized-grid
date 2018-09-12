let
  hostPkgs = import <nixpkgs> {};
  pinnedVersion = hostPkgs.lib.importJSON ./nixpkgs-version.json;
    pinnedPkgs = hostPkgs.fetchFromGitHub {
      owner = "NixOs";
      repo = "nixpkgs-channels";
      inherit (pinnedVersion) rev sha256;

    };
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self : super : rec {
          sized-grid = callCabal2nix "sized-grid" ./. {};

          ghc = super.ghc // {withPackages = super.ghc.withHoogle;};
          ghcWithPackages = self.ghc.withPackages;
        };
      };
    };
  };

  pkgs = import pinnedPkgs { inherit config; };
  callCabal2nix = pkgs.haskellPackages.callCabal2nix;
  callHackage = pkgs.haskellPackages.callHackage;
  hLib = pkgs.haskell.lib;

in
  pkgs.haskellPackages.sized-grid
