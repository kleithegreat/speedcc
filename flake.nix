{
  description = "speedcc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskellPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            (hsPkgs.ghcWithPackages (hp: [
              hp.megaparsec
              hp.text
              hp.containers
            ]))
            hsPkgs.cabal-install
          ];
        };
      }
    );
}