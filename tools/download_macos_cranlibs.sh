#!/usr/bin/env bash

# Download CRAN MacOS binary dependencies from
#   - https://mac.r-project.org/libs-4/ (x86_64/Intel)
#   - https://mac.r-project.org/libs-arm64/ (arm64/M1)

# This script is for local testing and CI (in future).

set -eu

# Util functions
msg() {
    echo >&2 -e "${1-}"
}

die() {
    local msg=$1
    local code=${2-1} # default exit status 1
    msg "$msg"
    exit "$code"
}

# Directory for placing the libraries
OSARCH=`uname -m`
rm -rf /tmp/r-cranlibs/${OSARCH} || true
export CRANLIBS_ROOT=/tmp/r-cranlibs/${OSARCH}
mkdir -p ${CRANLIBS_ROOT}

download() {
    curl -s "$1" | tar xzf - -C ${CRANLIBS_ROOT}/
}

if [ "$OSARCH" = "x86_64" ]; then
    download https://mac.r-project.org/libs-4/gmp-6.2.1-darwin.17-x86_64.tar.gz
    download https://mac.r-project.org/libs-4/mpfr-4.0.2-darwin.17-x86_64.tar.gz
elif [ "$OSARCH" = "arm64" ]; then
    download https://mac.r-project.org/libs-arm64/gmp-6.2.1-darwin.20-arm64.tar.gz
    download https://mac.r-project.org/libs-arm64/mpfr-4.1.0-darwin.20-arm64.tar.gz
else
    die "Unimplemented for arch ${OSARCH}"
fi

msg "== Libraries have been downloaded to ${CRANLIBS_ROOT}"

if [ "$OSARCH" = "x86_64" ]; then
    prefix="/tmp/r-cranlibs/x86_64/usr/local"
elif [ "$OSARCH" = "arm64" ]; then
    prefix="/tmp/r-cranlibs/arm64/opt/R/arm64"
else
    die "Unexpected: Unimplemented for arch ${OSARCH}"
fi
