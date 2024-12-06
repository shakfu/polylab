#!/usr/bin/env bash

# build_zpython.sh
# builds minimized python with zipped `site-packages`
# NOTE: need os.py to remain in site-packages or just above it or it will fail

# ----------------------------------------------------------------------------
# VARS

NAME=zpython
VERSION=3.11.7
MAC_DEP_TARGET=10.13

# ----------------------------------------------------------------------------

PWD=$(pwd)
PREFIX=${PWD}/${NAME}
VER=${VERSION%.*}
VERN="${VER//./}"
LIB=${PREFIX}/lib/python${VER}
URL=https://www.python.org/ftp/python/${VERSION}/Python-${VERSION}.tar.xz


get_python() {
    wget $URL
}


remove() {
    echo "removing $1"
    rm -rf $1
}

rm_lib() {
    echo "removing $1"
    rm -rf ${LIB}/$1
}


clean() {
    echo "removing __pycache__ .pyc/o from $1"
    find $1 | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf
}

clean_tests() {
    echo "removing 'test' dirs from $1"
    find $1 | grep -E "(tests|test)" | xargs rm -rf
}

clean_site_packages() {
    echo "removing everything in $LIB/site-packages"
    rm -rf $LIB/site-packages/*
}

rm_ext() {
    echo "removing $LIB/lib-dynload/$1.cpython-${VER}-darwin.so"
    rm -rf $LIB/lib-dynload/$1.cpython-*.so
}

rm_bin() {
    echo "removing $PREFIX/bin/$1"
    rm -rf $PREFIX/bin/$1
}

# ----------------------------------------------------------------------------
# main

main() {
    if ! test -f Python-${VERSION}.tar.xz; then
        wget $URL
    fi
    if test -d Python-${VERSION}; then
        rm -rf Python-${VERSION}
    fi
    tar xvf Python-${VERSION}.tar.xz
    cd Python-${VERSION}

    ./configure MACOSX_DEPLOYMENT_TARGET=${MAC_DEP_TARGET} \
        --prefix=$PREFIX \
        --enable-shared \
        --disable-test-modules \
        --with-ensurepip=no \
        --without-static-libpython

    make install

    clean $PREFIX
    clean_tests $LIB
    clean_site_packages
    remove ${LIB}/site-packages

    # remove what you want here...
    rm_lib config-${VERSION}-darwin
    rm_lib idlelib
    rm_lib lib2to3
    rm_lib tkinter
    rm_lib turtledemo
    rm_lib turtle.py
    rm_lib ensurepip
    rm_lib venv

    remove $LIB/distutils/command/*.exe
    remove $LIB/lib-dynload/_xx*.so
    remove $LIB/lib-dynload/xx*.so
    remove $PREFIX/lib/pkgconfig
    remove $PREFIX/share

    # remove what you want here...
    rm_ext _tkinter


    rm_bin 2to3-${VER}
    rm_bin idle${VER}
    rm_bin idle3
    rm_bin 2to3-${VER}
    rm_bin 2to3


    mv $LIB/lib-dynload $PREFIX
    cp $LIB/os.py $PREFIX
    clean $PREFIX

    echo "zip site-packages"
    python3 -m zipfile -c $PREFIX/lib/python${VERN}.zip $LIB/*
    remove $LIB
    mkdir -p $LIB
    mv $PREFIX/lib-dynload $LIB
    mv $PREFIX/os.py $LIB
    mkdir $LIB/site-packages

    echo "cleanup"
    rm -rf Python-${VERSION}
}

test_zipped_python() {
    ./python/bin/python3 -c "import string; print(string.__file__)"
    ./python/bin/python3 -c "import string; assert string.digits=='0123456789'"
}


# run it
time main

# test it
test_zipped_python



