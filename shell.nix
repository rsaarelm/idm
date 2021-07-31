let
  pkgs = import <nixpkgs> {};

  log_level = "debug";
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    rustc cargo rustfmt rust-analyzer
    cargo-outdated cargo-tarpaulin cargo-watch clippy

    just  # Utils
  ];

  RUST_BACKTRACE = "1";
  RUST_LOG = "idm=${log_level}";
}
