{
  inputs = {
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
#    flake-utils.follows = "nix-vscode-extensions/flake-utils";
#    nixpkgs.follows = "nix-vscode-extensions/nixpkgs";
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        extensions = inputs.nix-vscode-extensions.extensions.${system}.forVSCodeVersion pkgs.vscodium.version;
        inherit (pkgs) vscode-with-extensions vscodium;

        packages.default = vscode-with-extensions.override {
          vscode = vscodium;
          vscodeExtensions = [
            extensions.vscode-marketplace.bbenoist.nix
            extensions.vscode-marketplace.haskell.haskell
            extensions.vscode-marketplace.justusadam.language-haskell
            extensions.vscode-marketplace.visortelle.haskell-spotlight
            extensions.vscode-marketplace.vscodevim.vim
            extensions.vscode-marketplace.czhang03.unicode-math-input
            extensions.vscode-marketplace.eamodio.gitlens
          ];
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [ packages.default ];
          shellHook = ''
            printf "VSCodium %s with extensions:\n" ${pkgs.vscodium.version}
            codium --list-extensions
          '';
        };
      in
      {
        inherit packages devShells;
      }
    );
}
