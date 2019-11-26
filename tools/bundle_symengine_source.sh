#!/usr/bin/env bash

set -e

echo ======== BUNDLE SYMENGINE SOURCE ==========

if ! test -f DESCRIPTION; then
    echo 1>&2 "Wrong directory"
    exit 1
fi

PKG_DIR=`pwd`
SYMENGINE_COMMIT=0139a82d23625f6dde437b25a2e4f43f5a6945fd

echo === Bundle source from commit: $SYMENGINE_COMMIT

test -d symengine_source && rm -r symengine_source || true
mkdir symengine_source

curl -L "https://github.com/symengine/symengine/archive/$SYMENGINE_COMMIT.tar.gz" 2>/dev/null | \
    tar -xz -C symengine_source/

test -d src/upstream && rm -r src/upstream || true
mv symengine_source/symengine-"$SYMENGINE_COMMIT" src/upstream

rm -r symengine_source

echo === Apply patch

diffstat -p0 symengine_patch.diff
patch -p0 <symengine_patch.diff

echo === Remove src/upstream/symengine/utilities/catch directory

rm -r src/upstream/symengine/utilities/catch

echo === diff src/upstream/LICENSE inst/COPYRIGHTS

diff src/upstream/LICENSE inst/COPYRIGHTS 

echo ======== BUNDLE SYMENGINE SOURCE DONE ==========
