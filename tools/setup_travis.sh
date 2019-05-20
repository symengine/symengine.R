#!/usr/bin/env bash

## This script is based on `bin/install_travis.sh` and `bin/test_travis.sh` in
## symengine.

# Exit on error
set -e
# Echo each command
set -x

## Ensure we start from the correct directory
if [ ! -f DESCRIPTION ]; then
    echo Wrong directory
    exit 1
fi

export R_SOURCE_DIR=`pwd`

# Setup options to compile and install symengine
export TEST_CPP="no"
export MAKEFLAGS="-j2"
if [ "${TRAVIS_OS_NAME}" == "osx" ]; then
    export CC=clang;
    export CXX=clang++;
fi
if [ "${TRAVIS_OS_NAME}" == "linux" ]; then
    ## Use default gcc and g++
    export CC=`which gcc`;
    export CXX=`which g++`;
    echo $CC
    echo $CXX
    $CC  --version
    $CXX --version
fi

cd $HOME
git clone https://github.com/symengine/symengine symengine-cpp
cd symengine-cpp
export SOURCE_DIR=`pwd`
git checkout `cat $R_SOURCE_DIR/symengine_version.txt`

# Setup travis for C++ library
cd $SOURCE_DIR
source bin/install_travis.sh
set -e
set -x

# Build C++ library
cd $SOURCE_DIR
bin/test_travis.sh

unset MAKEFLAGS

cd $R_SOURCE_DIR

