#!/usr/bin/env bash

docker run -it --rm \
  -v `pwd`/circus.ini:/root/circus.ini \
  -p 9999:9999 \
  img-circus
