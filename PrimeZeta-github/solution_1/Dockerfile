FROM ubuntu:22.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install Zeta compiler (placeholder - would need actual Zeta installation)
# COPY zetac /usr/local/bin/zetac

# Copy solution
WORKDIR /solution
COPY src/prime.z .

# Compile
# RUN zetac prime.z -o prime

# Entry point for competition harness
# CMD ["./prime"]