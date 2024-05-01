# Run unit tests
test:
    cargo test

# Code coverage report
cov:
    cargo tarpaulin -v

# Constantly running coverage monitor
watch-cov:
    cargo watch -x tarpaulin

# Register project-versioned githooks
register-githooks:
    git config --local core.hooksPath githooks/

fuzz:
    cd fuzz
    # FIXME: RUSTFLAGS setting needed because of mismatch with llvm and rustc
    # versions...
    RUSTFLAGS="-Znew-llvm-pass-manager=no" cargo fuzz run from_str
