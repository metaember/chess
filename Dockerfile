# Build stage for Rust
FROM rust:1.83-slim-bookworm AS rust-builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Copy source and build
COPY Cargo.toml Cargo.lock ./
COPY src ./src
COPY benches ./benches
RUN cargo build --release --bin web_server

# Build stage for web app
FROM node:22-slim AS web-builder

WORKDIR /build

RUN corepack enable && corepack prepare pnpm@latest --activate

COPY web-app/package.json web-app/pnpm-lock.yaml ./
RUN pnpm install --frozen-lockfile

COPY web-app ./
RUN pnpm build

# Runtime stage
FROM debian:bookworm-slim

WORKDIR /app

# Copy built artifacts
COPY --from=rust-builder /build/target/release/web_server ./web_server
COPY --from=web-builder /web ./web
COPY book/Cerebellum3Merge.bin ./book/Cerebellum3Merge.bin

EXPOSE 3000

CMD ["./web_server"]
