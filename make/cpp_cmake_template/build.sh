#!/usr/bin/env bash

rm -rf ./build

pushd .
mkdir -p build/release
cd build/release
cmake ../.. -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=1
make all
popd

pushd .
mkdir -p build/debug
cd build/debug
cmake ../.. -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1
make all
popd
