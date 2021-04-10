let
  pkgs = import <nixpkgs> {};

  log_level = "info";
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    rustc cargo rustfmt rust-analyzer cargo-outdated clippy

    just  # Utils
  ];

  RUST_BACKTRACE = "1";
  RUST_LOG = "idm=${log_level}";
}
