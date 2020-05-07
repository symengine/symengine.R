
## Version 0.1.3:

This tries to address the build error with Mac OS on CRAN.

It will look for /Applications/CMake.app/Contents/bin/cmake if cmake is not in PATH. As mentioned by Prof Ripley, CRAN MacOS builder uses that path as well.

Additionally it will provide fallback values for PKG_LIBS and PKG_CPPFLAGS if 'cmake --find-package' failed for some reason.
