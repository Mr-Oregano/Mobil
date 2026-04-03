{
  description = "Pure Nix OCaml Project with Alcotest";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSystem = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      packages = forEachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3; 
        in
        {
          default = ocamlPackages.buildDunePackage {
            pname = "Mobil";
            version = self.shortRev or self.dirtyShortRev or "dev";
            src = ./.;

            # Build tools
            nativeBuildInputs = with ocamlPackages; [
                findlib 
            ];

            # Build dependencies
            buildInputs = with ocamlPackages; []; 
          };
        });

      devShells = forEachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;
        in
        {
          default = pkgs.mkShell {
            # Use the same inputs from 'packages'
            inputsFrom = [ self.packages.${system}.default ];
            
            # Dev tools
            buildInputs = with ocamlPackages; [
              utop         
              ocamlformat
              ocaml-lsp
              findlib # needed for dune
            ];
          };
        });
    };
}