
## Version 0.1.1:

0. Fix 'export' issue in configure ('export var=value' is not supported in old version of sh) and use R CMD config and compiler options.

## Previous comments

This is mainly fixing issues building the package on solaris. The following changes are made:

2. In 'configure', fix issue that '!' is not a shell program in solaris.

3. Drop dependency of mpc library. It is not needed for most functionalities of the package.

4. Add a SystemRequirementsNote field to DESCRIPTION noting the 'deb' and 'rpm' package names of gmp and mpfr library that required to be installed. (i.e. libgmp-dev, libmpfr-dev and gmp-devel, mpfr-devel)

## Note:

Currently there some issues compiling the C++ library with Oracle Developer Studio 12.6 (which might be fixed in future). Thus it might require using GCC on solaris to compile the code.
