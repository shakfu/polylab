NAME=methods

echo "building using docker"
docker run --rm \
    --user "$(id -u)":"$(id -g)" \
    -v "$PWD":/usr/src/myapp \
    -w /usr/src/myapp \
    rust:latest \
    cargo build --release

echo "copying and stripping executable"
cp target/release/${NAME} .
strip ./${NAME}

echo "cleanup"
rm -rf target
rm Cargo.lock

