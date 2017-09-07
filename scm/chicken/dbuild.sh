
echo "compile $1"
docker run -it --rm \
    -v $(pwd):/src  \
    -w /src         \
    theodesp/chicken-scheme-alpine \
    csc -static $1

