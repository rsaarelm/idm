let
  pkgs = import <nixpkgs> {};

  log_level = "info";
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    rustc cargo rustfmt rust-analyzer
    cargo-outdated cargo-tarpaulin cargo-flamegraph cargo-watch clippy

    # Utils
    just linuxPackages.perf
  ];

  RUST_LOG = "idm=${log_level}";
}
