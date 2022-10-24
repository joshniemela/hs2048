{
  description = "srid/hs2048: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
      ];
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          root = ./.;
          buildTools = hp: {
            inherit (pkgs)
              treefmt;
              
          } // config.treefmt.formatters;
          # overrides = self: super: {}
          hlsCheck.enable = true;
          hlintCheck.enable = true;
        

          #haskellPackages = pkgs.haskellPackages.override {
          #  overrides = self: super: rec {
          #    MonadRandom = pkgs.haskell.lib.dontCheck
          #      (self.callHackage "MonadRandom" "0.5.3" { });
          #  };
          #};
        };
        treefmt.formatters = {
          inherit (pkgs)
            nixpkgs-fmt;
          inherit (pkgs.haskellPackages)
            cabal-fmt
            fourmolu;
        };
      };
    };
}
