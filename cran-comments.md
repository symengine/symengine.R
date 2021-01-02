
## Version 0.2.0:

This version will compile the bundled C++ library from source instead of downloading binaries on Windows. It requires Rtools40 toolchains and the following MinGw packages:

 pacman -S mingw-w64-{i686,x86_64}-cmake
 pacman -S mingw-w64-{i686,x86_64}-gmp
 pacman -S mingw-w64-{i686,x86_64}-mpfr
