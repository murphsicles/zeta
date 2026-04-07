# Murphy's Sieve - Competition Docker Container
# Provides reproducible execution environment for competition submission

FROM rust:1.92-slim AS builder

WORKDIR /app

# Copy source files
COPY murphy_sieve_competition_final.rs .
COPY final_murphy_benchmark.rs .

# Build optimized release versions
RUN rustc -C opt-level=3 murphy_sieve_competition_final.rs -o murphy_sieve
RUN rustc -C opt-level=3 final_murphy_benchmark.rs -o benchmark

# Create final minimal image
FROM debian:bookworm-slim

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libc6 \
    && rm -rf /var/lib/apt/lists/*

# Copy binaries from builder
COPY --from=builder /app/murphy_sieve /app/murphy_sieve
COPY --from=builder /app/benchmark /app/benchmark

# Set executable permissions
RUN chmod +x /app/murphy_sieve /app/benchmark

# Create entrypoint script
RUN echo '#!/bin/bash\n\
echo "================================================"\n\
echo "MURPHY'"'"'S SIEVE - COMPETITION CONTAINER"\n\
echo "================================================"\n\
echo ""\n\
echo "Available commands:"\n\
echo "  ./murphy_sieve      - Run competition implementation"\n\
echo "  ./benchmark         - Run comprehensive benchmark"\n\
echo "  ./murphy_sieve N    - Run with specific limit"\n\
echo ""\n\
if [ $# -eq 0 ]; then\n\
    echo "Running default demonstration..."\n\
    echo ""\n\
    ./murphy_sieve\n\
else\n\
    if [ "$1" = "benchmark" ]; then\n\
        ./benchmark\n\
    elif [[ "$1" =~ ^[0-9]+$ ]]; then\n\
        echo "Running Murphy'"'"'s Sieve with limit=$1"\n\
        echo ""\n\
        ./murphy_sieve\n\
        echo ""\n\
        echo "Note: The program demonstrates with limit=1,000,000"\n\
        echo "To test with your limit, modify the source code."\n\
    else\n\
        exec "$@"\n\
    fi\n\
fi' > /app/entrypoint.sh && chmod +x /app/entrypoint.sh

# Default entrypoint
ENTRYPOINT ["/app/entrypoint.sh"]

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD ./murphy_sieve 1000 > /dev/null 2>&1 || exit 1

# Metadata
LABEL org.opencontainers.image.title="Murphy's Sieve Competition"
LABEL org.opencontainers.image.description="Optimized Murphy's Sieve with u64 bit array for 64x memory reduction"
LABEL org.opencontainers.image.version="1.0.0"
LABEL org.opencontainers.image.authors="Competition Team"
LABEL org.opencontainers.image.licenses="MIT"

# Run demonstration by default
CMD ["demo"]