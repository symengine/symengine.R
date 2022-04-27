#!/usr/bin/env bash

set -e

echo ======== Cleanup src/upstream[.tar] =======

if [ -f src/upstream.tar ]; then rm src/upstream.tar   ; fi
if [ -d src/upstream/ ];    then rm -rf src/upstream/  ; fi

echo ======== BUNDLE SYMENGINE SOURCE ==========

if ! test -f DESCRIPTION; then
    echo 1>&2 "Wrong directory"
    exit 1
fi

PKG_DIR=`pwd`

SYMENGINE_REPO="symengine/symengine"
SYMENGINE_COMMIT=148d4fa2aaa38afc92d4dfb203b00fdf71f3be2e

echo === Bundle source from commit: $SYMENGINE_COMMIT

test -d symengine_source && rm -r symengine_source || true
mkdir symengine_source

curl -L "https://github.com/$SYMENGINE_REPO/archive/$SYMENGINE_COMMIT.tar.gz" 2>/dev/null | \
    tar -xz -C symengine_source/

test -d src/upstream && rm -r src/upstream || true
mv symengine_source/symengine-"$SYMENGINE_COMMIT" src/upstream

rm -r symengine_source

echo === Apply patch

# diffstat may not be available
diffstat -p0 ./tools/symengine_patch.diff || true
patch -p0 <./tools/symengine_patch.diff

echo === Remove some unnecessary files in src/upstream

set -x

rm -r src/upstream/symengine/utilities/catch
rm -r src/upstream/symengine/tests
rm -r src/upstream/benchmarks
rm -r src/upstream/bin
rm -r src/upstream/docs
rm -r src/upstream/binder
rm    src/upstream/appveyor.yml
rm    src/upstream/codecov.yml

set +x

echo === diff src/upstream/LICENSE inst/COPYRIGHTS

diff src/upstream/LICENSE inst/COPYRIGHTS || true

echo === Create tarball of src/upstream/

cd src/
tar cf upstream.tar upstream/
rm -rf upstream/
cd ../

echo === touch ./tools/SYMENGINE_BUNDLED

touch ./tools/SYMENGINE_BUNDLED

echo ======== BUNDLE SYMENGINE SOURCE DONE ==========
