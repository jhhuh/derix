{
  description = "Typelevel Package Manager — research project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghc = pkgs.haskellPackages.ghcWithPackages (hp: with hp; [
          containers
          text
          mtl
          prettyprinter
        ]);
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Process management
            overmind
            tmux

            # Haskell
            ghc
            cabal-install
            haskellPackages.haskell-language-server
          ];
        };
      });
}
