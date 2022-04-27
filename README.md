
<!-- README.md is generated from README.Rmd. Please edit that file -->

# symengine

<!-- badges: start -->

[![R-CMD-check](https://github.com/symengine/symengine.R/workflows/R-CMD-check/badge.svg)](https://github.com/symengine/symengine.R/actions)
[![Travis-CI Build
Status](https://travis-ci.org/symengine/symengine.R.svg?branch=master)](https://travis-ci.org/symengine/symengine.R)
[![AppVeyor Build
status](https://ci.appveyor.com/api/projects/status/rr0tdh8ykvs04qg2?svg=true)](https://ci.appveyor.com/project/symengine/symengine-r)
<!-- badges: end -->

`symengine` is an R interface to the [SymEngine C++
library](https://github.com/symengine/symengine) for symbolic
computation.

## Installation

There are some dependencies needed on Unix systems. You may install them
with

    zypper install cmake gmp-devel mpfr-devel mpc-devel    ## openSUSE
    dnf    install cmake gmp-devel mpfr-devel libmpc-devel ## Fedora
    apt    install cmake libgmp-dev libmpfr-dev libmpc-dev ## Debian
    brew   install cmake gmp mpfr libmpc                   ## Mac OS

Then you can install the R package with

``` r
devtools::install_github("symengine/symengine.R")
```

On Windows, you will need to install
[Rtools42](https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html)
for building the package from source.

Please report any problem installing the package on your system.

``` r
library(symengine)
#> SymEngine Version: 0.9.0
#>  _____           _____         _         
#> |   __|_ _ _____|   __|___ ___|_|___ ___ 
#> |__   | | |     |   __|   | . | |   | -_|
#> |_____|_  |_|_|_|_____|_|_|_  |_|_|_|___|
#>       |___|               |___|
```

## Usage

Also check the documentation site with built vignettes and help pages at
<http://symengine.marlin.pub>.

### Manipulating Symbolic Expressions

``` r
use_vars(x, y, z)
#> Initializing 'x', 'y', 'z'
expr <- (x + y + z) ^ 2L - 42L
expand(expr)
#> (Add)    -42 + 2*x*y + 2*x*z + 2*y*z + x^2 + y^2 + z^2
```

Substitue `z` as `a` and `y` as `x^2`.

``` r
a <- S("a")
expr <- subs(expr, z, a)
expr <- subs(expr, y, x^2L)
expr
#> (Add)    -42 + (a + x + x^2)^2
```

Second derivative of `expr` with regards to `x`:

``` r
d1_expr <- D(expr, "x")
d2_expr <- D(d1_expr, "x")
expand(d2_expr)
#> (Add)    2 + 4*a + 12*x + 12*x^2
```

Solve the equation of `d2_expr == 0` with regards to `x`.

``` r
solutions <- solve(d2_expr, "x")
solutions
#> VecBasic of length 2
#> V( -1/2 + (-1/2)*sqrt(1 + (-1/3)*(2 + 4*a)), -1/2 + (1/2)*sqrt(1 + (-1/3)*(2 + 4*a)) )
```

### Numerically Evaluate Symbolic Expressions

For the two solutions above, we can convert them into a function that
gives numeric output with regards to given input.

``` r
func <- as.function(solutions)
ans <- func(a = -100:-95)
colnames(ans) <- c("Solution1", "Solution2")
ans
#>      Solution1 Solution2
#> [1,] -6.280715  5.280715
#> [2,] -6.251811  5.251811
#> [3,] -6.222762  5.222762
#> [4,] -6.193564  5.193564
#> [5,] -6.164215  5.164215
#> [6,] -6.134714  5.134714
```

### Numbers

The next prime number greater than 2^400.

``` r
n <- nextprime(S(~ 2 ^ 400))
n
#> (Integer)    2582249878086908589655919172003011874329705792829223512830659356540647622016841194629645353280137831435903171972747493557
```

The greatest common divisor between the prime number and 42.

``` r
GCD(n, 42)
#> (Integer)    1
```

The binomial coefficient `(2^30 ¦ 5)`.

``` r
choose(S(~ 2^30), 5L)
#> (Integer)    11893730661780666387808571314613824587300864
```

Pi “computed” to 400-bit precision number.

``` r
if (symengine_have_component("mpfr"))
    evalf(Constant("pi"), bits = 400)
#> (RealMPFR,prec400)   3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066
```

### Object Equality

``` r
x + y == S("x + y")
#> [1] TRUE
x + y != S("x + y")
#> [1] FALSE
```

``` r
sin(x)/cos(x)
#> (Mul)    sin(x)/cos(x)
tan(x) == sin(x)/cos(x) # Different internal representation
#> [1] FALSE
```

## Acknowledgement

This project was a Google Summer of Code project under the organization
of The R Project for Statistical Computing in 2018. The student was Xin
Chen, mentored by Jialin Ma and Isuru Fernando.
