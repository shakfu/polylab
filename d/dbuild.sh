echo "building using docker"
docker run --rm -it -v $(pwd):/src dlanguage/ldc ldc2 $1

