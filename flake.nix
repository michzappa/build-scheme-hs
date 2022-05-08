{
  description = "https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, ... }@inputs:
    with inputs;
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs {
          inherit system;
        });
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal-install
            ghc
            haskell-language-server
          ];
        };

        defaultPackage = pkgs.haskellPackages.developPackage {
          root = ./.;
        };
      });
}
