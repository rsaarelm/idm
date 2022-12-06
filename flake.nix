{
  inputs = {
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.follows = "rust-overlay/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs:
    with inputs;
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        code = pkgs.callPackage ./. { inherit nixpkgs system rust-overlay; };
      in rec {
        devShell = with pkgs;
          mkShell {
            buildInputs = [
              cargo
              rustc

              rustfmt
              rust-analyzer
              clippy
              cargo-outdated
              cargo-tarpaulin

              tokei
              just
              pre-commit
            ];
          };
      });
}


