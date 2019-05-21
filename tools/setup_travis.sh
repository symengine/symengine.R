#!/usr/bin/env bash

## This script is based on `bin/install_travis.sh` and `bin/test_travis.sh` in
## symengine, which use conda install the dependencies and build symengine.
## After running this script, we may need to activate the conda environment with:
##  - export PATH="$HOME/conda_root/bin:$PATH"
##  - source activate $HOME/our_usr

# Exit on error
set -e
# Echo each command
set -x

## Ensure we start from the correct directory
if [ ! -f DESCRIPTION ]; then
    echo Wrong directory
    exit 1
fi

R_SOURCE_DIR=`pwd`

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
SOURCE_DIR=`pwd`

## Checkout the commit specified with environment variable (default: master)
if [ "${SYMENGINE_COMMIT}" == "" ]; then
    SYMENGINE_COMMIT=master
fi
git checkout $SYMENGINE_COMMIT

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

