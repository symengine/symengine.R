#!/usr/bin/env bash

echo ======== BUNDLE SYMENGINE SOURCE ==========

if ! test -f DESCRIPTION; then
    echo 1>&2 "Wrong directory"
    exit 1
fi

PKG_DIR=`pwd`
SYMENGINE_COMMIT=master

rm -r symengine_source 2> /dev/null
mkdir symengine_source

curl -L "https://github.com/symengine/symengine/archive/$SYMENGINE_COMMIT.tar.gz" | \
    tar -xz -C symengine_source/

rm -r src/upstream 2> /dev/null
mv symengine_source/symengine-"$SYMENGINE_COMMIT" src/upstream

rm -r symengine_source

echo ======== BUNDLE SYMENGINE SOURCE DONE ==========
