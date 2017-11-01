
<!-- README.md is generated from README.Rmd. Please edit that file -->
symengine
=========

[![Travis-CI Build Status](https://travis-ci.org/Marlin-Na/symengine.R.svg?branch=master)](https://travis-ci.org/Marlin-Na/symengine.R)

This is an experiment to provide a R interface to the [SymEngine library](https://github.com/symengine/symengine).

Not fully functional yet, but if you are interested, please contact Jialin Ma <marlin-@gmx.cn> and Isuru Fernando <isuruf@gmail.com>.

This project is expected to be a GSoC 2018 project under the organization of The R Project for Statistical Computing.

Installation
------------

Currently you need to have SymEngine library installed on your computer, please follow the instruction at <https://github.com/symengine/symengine/>, or do the following:

``` sh
git clone git@github.com:symengine/symengine.git
cd symengine
make
sudo make install
```

In the future, the SymEngine library may be contained in the package so that no external libraries will be needed.

Then install the R package from github with:

``` r
# install.packages("devtools")
devtools::install_github("Marlin-Na/symengine.R")
```

If you run into problems installing the package, please let me know.

Example
-------

Here are some examples showing the currently available functionalities.

``` r
library(symengine)
#> SymEngine Version: 0.3.0
#>  _____           _____         _         
#> |   __|_ _ _____|   __|___ ___|_|___ ___ 
#> |__   | | |     |   __|   | . | |   | -_|
#> |_____|_  |_|_|_|_____|_|_|_  |_|_|_|___|
#>       |___|               |___|
S("k*x + b")     # Parse a string to expression
#> (Add)    b + k*x
Integer(2^10)
#> (Integer)    1024
Integer(2^10) ^ 9L
#> (Integer)    1237940039285380274899124224
Integer(2^10) ^ 9L * Symbol("x")
#> (Mul)    1237940039285380274899124224*x
x <- Symbol("x")
y <- Symbol("y")
4L * (x + y) - 2L * x
#> (Add)    -2*x + 4*(x + y)
```

TODO
----
