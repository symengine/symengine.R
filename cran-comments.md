
## R CMD check results

The package has been checked with `R CMD check --as-cran` on the following systems with
no error/warning:

- win-builder (r-release and r-devel)
- Mac OS with clang
- openSUSE tumbleweed with gcc

## Note on system dependency

The package requires no system dependency to build on Windows.

On Unix or Mac OS, it requires the following system dependencies to build:

- cmake
- gmp, mpfr and mpc libraries

These dependencies can be easily installed via system package manager:

On Ubuntu/Debian:

```
apt install cmake libgmp-dev libmpfr-dev libmpc-dev
```

On Fedora:

```
dnf install cmake gmp-devel mpfr-devel mpc-devel
```

On Mac OS, I found gmp, mpfr and mpc from http://mac.r-project.org/libs/ as static libraries,
which should work as expected. But I am unable to test it on CRAN's machine.
And `cmake` will still be required to build the package on Mac OS.
