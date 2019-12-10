#!/usr/bin/env bash

set -e

tput setaf 3; echo ======== BUNDLE SYMENGINE SOURCE ==========; tput sgr0

if ! test -f DESCRIPTION; then
    echo 1>&2 "Wrong directory"
    exit 1
fi

PKG_DIR=`pwd`
SYMENGINE_COMMIT=1db91f876c3fe5d214f97cdcc5c9e46558b250c2

tput setaf 3; echo === Bundle source from commit: $SYMENGINE_COMMIT; tput sgr0

test -d symengine_source && rm -r symengine_source || true
mkdir symengine_source

curl -L "https://github.com/symengine/symengine/archive/$SYMENGINE_COMMIT.tar.gz" 2>/dev/null | \
    tar -xz -C symengine_source/

test -d src/upstream && rm -r src/upstream || true
mv symengine_source/symengine-"$SYMENGINE_COMMIT" src/upstream

rm -r symengine_source

tput setaf 3; echo === Apply patch; tput sgr0

# diffstat may not be available
diffstat -p0 symengine_patch.diff || true
patch -p0 <symengine_patch.diff

tput setaf 3; echo === Remove some unnecessary files in src/upstream; tput sgr0

set -x

rm -r src/upstream/symengine/utilities/catch
rm -r src/upstream/symengine/tests
rm -r src/upstream/benchmarks
rm -r src/upstream/bin
rm -r src/upstream/doc
rm -r src/upstream/notebooks
rm -r src/upstream/binder
rm    src/upstream/appveyor.yml
rm    src/upstream/codecov.yml
rm    src/upstream/.travis.yml

set +x

tput setaf 3; echo === diff src/upstream/LICENSE inst/COPYRIGHTS; tput sgr0

diff src/upstream/LICENSE inst/COPYRIGHTS || true

tput setaf 2; echo ======== BUNDLE SYMENGINE SOURCE DONE ==========; tput sgr0
