# Development (uses docker-compose.override.yml automatically)
dev:
    docker compose up -d --build

# Production deploy
deploy:
    docker compose -f docker-compose.yml -f docker-compose.prod.yml up -d --build

# Stop containers
down:
    docker compose down

# View logs
logs:
    docker compose logs -f rust-chess

# Rebuild without cache
rebuild:
    docker compose build --no-cache

# Production rebuild
rebuild-prod:
    docker compose -f docker-compose.yml -f docker-compose.prod.yml build --no-cache

# Shell into container
shell:
    docker compose exec rust-chess /bin/sh

# Build web frontend locally
build-web:
    cd web-app && pnpm build

# Run server locally (no docker)
run:
    cargo run --release --bin web_server
