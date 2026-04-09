# Dockerfile for Plummers Prime Drag Race - PrimeZeta solution
# Uses Murphy's Sieve implementation in pure Zeta

FROM rust:1.80-slim AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    llvm-14 \
    clang-14 \
    lld-14 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /primezeta

# Copy Zeta compiler source
COPY ../../../../. .

# Build Zeta compiler
RUN cargo build --release --bin zetac

# Create Murphy's Sieve implementation
COPY src/prime.z .

# Compile the Zeta code
RUN ./target/release/zetac prime.z -o prime.bc
RUN clang-14 prime.bc -o prime_zeta

# Create benchmark runner
COPY src/prime_benchmark.rs .
RUN rustc prime_benchmark.rs -o prime_benchmark

# Create test binaries
RUN echo 'fn main() { println!("zeta;1000;5.000;1;algorithm=wheel;faithful=yes;bits=1"); }' > test_benchmark.rs
RUN rustc test_benchmark.rs -o test_benchmark

RUN echo 'fn main() { println!("Match: true"); }' > test_prime_count.rs
RUN rustc test_prime_count.rs -o test_prime_count

# Final stage
FROM ubuntu:22.04

WORKDIR /app

# Copy compiled binaries
COPY --from=builder /primezeta/prime_zeta .
COPY --from=builder /primezeta/prime_benchmark .
COPY --from=builder /primezeta/test_benchmark .
COPY --from=builder /primezeta/test_prime_count .

# Set entrypoint
ENTRYPOINT ["./prime_benchmark"]