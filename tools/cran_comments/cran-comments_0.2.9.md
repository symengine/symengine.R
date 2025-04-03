
This version (0.2.9) drops the dependency of odeintr 
and relevant functionalities, which should fix the 
errors in CRAN check results.

In addition, it addresses several other issues on CRAN
check:

1. Replaces the usage of IS_S4_OBJECT and 'rand' in compiled code. 
2. Fixes the -Wdeprecated-literal-operator compiler warning
3. Fixes the package build issue when using CMake 4.0

