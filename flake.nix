{
	description = "Haskell flake";

	inputs = {
		nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem (
			system:
			let 
				name = "hs2048";
				pkgs = import nixpkgs { inherit system; };
				haskellPackages = pkgs.haskell.packages.ghc90;
				cabalStuff = haskellPackages.callCabal2nix name ./. {};
			in rec {
				devShell = haskellPackages.shellFor {
					withHoogle = true;
					packages = p: [ 
						cabalStuff
					];
					buildInputs = with haskellPackages; [
						haskell-language-server
						hlint
						ghcid
						fourmolu
						cabal-install
					];
				};
				

				defaultPackage = cabalStuff;
			}
		);
}
				  
				  



