#!/usr/bin/env bash

set -e

tput setaf 3; echo ======== BUNDLE SYMENGINE SOURCE ==========; tput sgr0

if ! test -f DESCRIPTION; then
    echo 1>&2 "Wrong directory"
    exit 1
fi

PKG_DIR=`pwd`
SYMENGINE_COMMIT=97e43494cb42821d9616d6db5fe06cb76280f69c

tput setaf 3; echo === Bundle source from commit: $SYMENGINE_COMMIT; tput sgr0

test -d symengine_source && rm -r symengine_source || true
mkdir symengine_source

curl -L "https://github.com/symengine/symengine/archive/$SYMENGINE_COMMIT.tar.gz" 2>/dev/null | \
    tar -xz -C symengine_source/

test -d src/upstream && rm -r src/upstream || true
mv symengine_source/symengine-"$SYMENGINE_COMMIT" src/upstream

rm -r symengine_source

tput setaf 3; echo === Apply patch; tput sgr0

diffstat -p0 symengine_patch.diff
patch -p0 <symengine_patch.diff

tput setaf 3; echo === Remove src/upstream/symengine/utilities/catch directory; tput sgr0

rm -r src/upstream/symengine/utilities/catch

tput setaf 3; echo === diff src/upstream/LICENSE inst/COPYRIGHTS; tput sgr0

diff src/upstream/LICENSE inst/COPYRIGHTS || true

tput setaf 2; echo ======== BUNDLE SYMENGINE SOURCE DONE ==========; tput sgr0
