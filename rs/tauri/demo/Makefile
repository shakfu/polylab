.PHONY: install dev build test test-rust test-ts fmt clean

install:
	bun install

dev:
	bun run tauri dev

build:
	bun run tauri build

# Frontend type check plus the Rust unit tests.
test: test-ts test-rust

test-ts:
	bunx tsc --noEmit

test-rust:
	cd src-tauri && cargo test

fmt:
	cd src-tauri && cargo fmt

clean:
	rm -rf dist
	cd src-tauri && cargo clean
