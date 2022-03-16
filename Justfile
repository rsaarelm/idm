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
