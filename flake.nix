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
            hsPkgs.haskell-language-server
          ];
          shellHook = ''
            alias build='cabal build && cp /home/kevin/repos/speedcc/dist-newstyle/build/x86_64-linux/ghc-9.10.3/speedcc-0.1.0.0/x/speedcc/build/speedcc/speedcc .'
          '';
        };
      }
    );
}