.PHONY: up down build logs clean

up:
	docker compose up -d

down:
	docker compose down

build:
	docker compose up -d --build

logs:
	docker compose logs -f

clean:
	docker compose down -v
