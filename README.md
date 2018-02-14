
<!-- README.md is generated from README.Rmd. Please edit that file -->
symengine
=========

[![Travis-CI Build Status](https://travis-ci.org/Marlin-Na/symengine.R.svg?branch=master)](https://travis-ci.org/Marlin-Na/symengine.R)

This is an experiment to provide a R interface to the [SymEngine library](https://github.com/symengine/symengine). It is still in progress, but if you are interested, please contact Jialin Ma <marlin-@gmx.cn> and Isuru Fernando <isuruf@gmail.com>.

This project is expected to be a GSoC 2018 project under the organization of The R Project for Statistical Computing.

Installation
------------

Note: It is not supported on Windows yet.

First, ensure you have some prerequisites installed, for example on Debian-based system:

``` sh
sudo apt-get install cmake libgmp-dev libmpfr-dev libmpc-dev
```

Then in R

``` r
devtools::install_github("Marlin-Na/symengine.R")
```

This package has a build-time dependency of the [Rlibsymengine](https://github.com/marlin-na/Rlibsymengine) package, which should be automatically installed using devtools above.

Please report any problem installing the package on your system.

``` r
library(symengine)
#> SymEngine Version: 0.3.0
#>  _____           _____         _         
#> |   __|_ _ _____|   __|___ ___|_|___ ___ 
#> |__   | | |     |   __|   | . | |   | -_|
#> |_____|_  |_|_|_|_____|_|_|_  |_|_|_|___|
#>       |___|               |___|
#> 
#> Attaching package: 'symengine'
#> The following object is masked from 'package:base':
#> 
#>     diff
```

Usage
-----

### Symbol

A symbol or variable can be constructed from character.

``` r
(x <- Symbol("x"))
#> (Symbol) x
(y <- Symbol("y"))
#> (Symbol) y
x ^ y
#> (Pow)    x^y
```

### Integer, RealDouble, RealMPFR

There are explicit constructors for such types:

``` r
Integer(42L)
#> (Integer)    42
RealDouble(base::pi)
#> (RealDouble) 3.14159265358979
```

For large integer and high-precision floating number that R can not hold, you can construct "Integer" or "RealMPFR" from character. For example:

``` r
8615937169318
#> [1] 8.615937e+12
as.integer(8615937169318)
#> Warning: NAs introduced by coercion to integer range
#> [1] NA
Integer("8615937169318")
#> (Integer)    8615937169318
Integer("8615937169318") ^ 9L
#> (Integer)    261651187038033556722865852191251735369739650060120439731902444918211391981554448221540729228593041137656153397421568
```

``` r
# TODO, currently not available
RealMPFR("3.1415926535897932384626433832795028841971693993751058209", bits = 70)
```

Comparing with the `mpfr` function in `Rmpfr` package:

``` r
Rmpfr::mpfr("3.1415926535897932384626433832795028841971693993751058209", precbits = 70)
#> 1 'mpfr' number of precision  187   bits 
#> [1] 3.141592653589793238462643383279502884197169399375105820901
```

Or simply use the SymEngine parser instead of the explicit constructors:

``` r
S("8615937169318")
#> (Integer)    8615937169318
S("3.1415926535897932384626433832795028841971693993751058209")
#> (RealMPFR)   3.1415926535897932384626433832795028841971693993751058209
```

### Complex, ComplexDouble and ComplexMPC

There will be explicit constructors (TODO):

Or use the parser:

``` r
S("6 + 9I")
#> (Complex)    6 + 9*I
```

Or use:

``` r
6L + 9L * Constant("I")
#> (Add)    6 + 9*i
```

The `mpc` library is used for holding complex number with arbitrary precision, similar to `mpfr` library for floating number.

``` r
S("2.3 + 23.9999999999999999999I")
#> (ComplexMPC) 2.29999999999999982236 + 23.9999999999999999999*I
```

### Constants

For example:

``` r
Constant("pi")
#> (Constant)   pi
sin(Constant("pi") / 2L)
#> (Integer)    1
```

### Generic Conversion and Parser

As already showed in the above examples, `S` converts a R object to SymEngine object. When the input is a character, it will parse the expression to produce appropriate object.

``` r
S(6L)
#> (Integer)    6
S(4.2)
#> (RealDouble) 4.2
```

``` r
(x <- S("x"))
#> (Symbol) x
(k <- S(~ k))          # Currently only work with "symbol"
#> (Symbol) k
(b <- Constant("b"))
#> (Constant)   b
S("k * x + b")
#> (Add)    b + k*x
k * x + b
#> (Add)    k*x + b
```

``` r
S("pi")
#> (Constant)   pi
S("sin(pi)")
#> (Integer)    0
```

``` r
S("(tan(x) + sin(x))^2")
#> (Pow)    (sin(x) + tan(x))^2
S("a + 2 >= a")
#> (LessThan)   a <= 2 + a
```

### Substitue a Variable

Substitute a variable with another one:

``` r
x <- S("x")
a <- S("a")
(expr <- (tan(x) + sin(x) + a) ^ 2L)
#> (Pow)    (a + sin(x) + tan(x))^2
```

``` r
subs(expr, "x", "a")
#> (Pow)    (a + sin(a) + tan(a))^2
subs(expr, "x", 3.1415926)
#> (Pow)    (-7.94093388050907e-23 + a)^2
subs(expr, "x", Constant("pi"))
#> (Pow)    a^2
subs(expr, "x", Constant("pi") * 2L/3L)
#> (Pow)    (a + (-1/2)*sqrt(3))^2
```

### Expand an Expression

``` r
expr
#> (Pow)    (a + sin(x) + tan(x))^2
expand(expr)
#> (Add)    2*a*sin(x) + 2*a*tan(x) + 2*sin(x)*tan(x) + a^2 + sin(x)^2 + tan(x)^2
```

### Derivative

``` r
expr
#> (Pow)    (a + sin(x) + tan(x))^2
diff(expr, "x")
#> (Mul)    2*(a + sin(x) + tan(x))*(1 + tan(x)^2 + cos(x))
diff(expr, "a")
#> (Mul)    2*(a + sin(x) + tan(x))
```

### Evaluate an Expression

``` r
evalf(Constant("pi"), bits = 999)
#> (RealMPFR)   3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196442881097566593344612847564823378678316527120190914564856692346034861045432664821339360726024914127
```

### Object Equality and Hash

``` r
Eq(x + y, S("x + y"))
#> [1] TRUE
Neq(x + y, S("x + y"))
#> [1] FALSE
```

``` r
tan(x)
#> (Tan)    tan(x)
sin(x)/cos(x)
#> (Mul)    sin(x)/cos(x)
Eq(tan(x), sin(x)/cos(x)) # Different internal representation
#> [1] FALSE
```

``` r
Hash(tan(x))
#> [1] "5308874006"
Hash(sin(x)/cos(x))
#> [1] "46369110327826722"
```

### N Theory

TODO

### FunctionSymbol

TODO

``` r
S("f(x, y)")
#> (FunctionSymbol) f(x, y)
```

### Lambdify

TODO

### Vector

TODO

### Matrix (DenseMatrix and SparseMatrix)

TODO

Under the Hood
--------------

The SymEngine objects are implemented with "externalptr":

``` r
x <- Symbol(~ x)
str(x)
#> Formal class 'Basic' [package "symengine"] with 1 slot
#>   ..@ .xData:<externalptr>
```

See "src/interface.c" for the C code that wraps the symengine api.

Related R Packages
------------------

-   There are several functions in base R for defferentiation, integration, solving system of equations, etc. E.g. `solve`, `stats::D`, `stats::deriv`, `stats::integrate`, `stats::numericDeriv`.

-   R package [`Deriv`](https://github.com/sgsokol/Deriv) for symbolic differentiation, it allows user to supply custom rules for differentiation.
-   R package `numDeriv` for calculating numerical approximations to derivatives.

-   R package `gmp` and `Rmpfr` provide multiple precision arithmetic and floating point operations. They also include some special functions, e.g. `Rmpfr::integrateR` for numerical integration.

-   R package `mpc` available at [R forge](http://mpc.r-forge.r-project.org/). It provides multiple precision arithmetic for complex numbers.

-   R package [`rSymPy`](https://cran.r-project.org/web/packages/rSymPy/index.html) provides an interface to 'SymPy' library in python via rJava.
-   R package [`Ryacas`](https://cran.r-project.org/web/packages/Ryacas/index.html) provides an interface to the 'Yacas' computer algebra system. It is easier to install compared to `rSymPy`.

Notes on some dependencies
--------------------------

The SymEngine library can optionally depend on some external libraries, which is configured by CMake, see the list of CMake options in [README of SymEngine](https://github.com/symengine/symengine/README.md) and the [configure script](https://github.com/Marlin-Na/Rlibsymengine/blob/master/configure) of Rlibsymengine.

A few notes:

1.  `GMP` (GNU Multiple Precision Arithmetic Library) is a C library that can be used to store and do arithmetic calculation with big integers and rationals. It has an R interface ([gmp](https://github.com/cran/gmp/blob/master/DESCRIPTION) package).

2.  `mpfr` (Multiple Precision Floating-Point Reliable) is a C library that depends on the `GMP` library and is used for arbitrary precision floating number arithmetic calculations. It has an R interface ([Rmpfr](https://github.com/cran/Rmpfr) package). This is an optional dependency for SymEngine.

3.  `mpc` () is a C library that extends the `mpfr` library for the arithmetic of complex numbers with arbitrarily precision. There is a R package `mpc` which is not on CRAN, but [available](http://mpc.r-forge.r-project.org/) at R forge.
