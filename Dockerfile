FROM rust:1.85-bookworm

WORKDIR /app

# Copy the entire workspace
COPY . .

# Install dependencies and build
RUN cargo build --bin zetac

# Test the Zeta runtime
RUN cargo run --bin zetac -- test_zeta_no_alloc.z

# Test malloc (should work on Linux)
RUN cargo run --bin zetac -- test_alloc_dummy.z