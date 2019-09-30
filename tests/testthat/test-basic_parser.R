
context("Constructors for Basic Object")

test_that("S(integer) works", {
    x <- S(42L)
    expect_identical(get_type(x), "Integer")
})

test_that("S(double) enabled check_whole_number", {
    x <- S(42)
    expect_identical(get_type(x), "Integer")
    
    x <- S(42.1)
    expect_identical(get_type(x), "RealDouble")
})

test_that("S(character) can be parsed as constant", {
    x <- S("x")
    expect_identical(get_type(x), "Symbol")
    
    x <- S("e")
    expect_identical(get_type(x), "Constant")
})


test_that("S() with NA, NaN, Inf", {
    x <- NA
    expect_error(S(x))
    x <- NA_integer_
    expect_error(S(x))
    x <- NA_real_
    expect_error(S(x))
    x <- NA_complex_
    expect_error(S(x))
    x <- NA_character_
    expect_error(S(x))
    
    x <- NaN
    expect_true(S(x) == S(0)/S(0)) # S(0)/S(0) is NaN
    
    x <- Inf
    expect_true(S(x) == S("inf"))
    x <- -Inf
    expect_true(S(x) != S("inf"))
    expect_true(S(x) == S("-inf"))
})


test_that("S(formula) works", {
    x <- S(~ x)
    expect_true(get_type(x) == "Symbol")
    
    x <- S(~ e)
    expect_true(get_type(x) == "Symbol")
    expect_true(get_type(x) != get_type(S("e")))
    
    x <- S(~ 3)
    expect_true(get_type(x) == "Integer")
    
    x <- S(~ 3.1)
    expect_true(get_type(x) == "RealDouble")
    
    x <- S(~ x + y)
    expect_true(x == S("y + x"))
    
    ## Backquote
    a <- "m"
    x <- S(~ .(a))
    expect_true(x == S("m"))
    
    a <- S("m")
    x <- S(~ .(a))
    expect_true(x == S("m"))
    
    # ## {{}} style backquote
    # a <- S("m")
    # x <- S(~ {{a}})
    # expect_true(x == S("m"))
    
    
    expect_error(S(~ .(1:2)))
    expect_error(S(~ .(character(0))))
    expect_error(S(~ NA_character_))
    expect_error(S(~ NA_real_))
})


test_that("S() with non-scalar data", {
    expect_error(S(character(0)))
    expect_error(S(c("x", "y")))
})

## Basic

test_that("Basic(double)", {
    expect_true(Basic(42) != S(42))
})


## Symbol

test_that("Symbol(character)", {
    x <- Symbol("m")
    expect_true(x == S("m"))
    
    expect_true(Symbol("e") != S("e"))
    
    expect_error(Symbol(NA_character_))
})

test_that("Symbol(Basic)", {
    expect_true(Symbol(S("x")) == S("x"))
    expect_error(Symbol(S("pi")))
})

test_that("Symbol(formula)", {
    x <- Symbol(~ e)
    expect_true(get_type(x) == "Symbol")
    
    expect_error(Symbol(~ x + y))
    expect_error(Symbol(~ 42L))
})

test_that("Symbol(number) throws error", {
    expect_error(Symbol(32))
    expect_error(Symbol(32L))
    expect_error(Symbol(Inf))
    expect_error(Symbol(NaN))
})

test_that("Symbol(non-scalar) throws error", {
    expect_error(Symbol(character(0)))
    expect_error(Symbol(c("x", "y")))
})


## Constants

test_that("Constant(character)", {
    x <- Constant("x")
    expect_true(get_type(x) == "Constant")
    
    expect_error(Constant(NA_character_))
})

test_that("Constant() should not accept formula", {
    expect_error(Constant(~ x))
})

## TODO: Test variable binding for common constants:
##       I, pi, E, EulerGamma, Catalan, GoldenRatio


## Real

test_that("Real(number)", {
    x <- Real(42L)
    expect_true(get_type(x) == "RealDouble")
    
    x <- Real(42)
    expect_true(get_type(x) == "RealDouble")
})

test_that("Real(character)", {
    ## FIXME:
    ## x <- Real("42")
    ## expect_true(get_type(x) == "RealDouble")
    
    x <- Real("42.0")
    expect_true(get_type(x) == "RealDouble")
    
    ## FIXME: (maybe throw a warning?)
    ## expect_error(Real("x"))
})

test_that("Real(number)", {
    x <- Real(42)
    expect_true(get_type(x) == "RealDouble")
    
    x <- Real(42L)
    expect_true(get_type(x) == "RealDouble")
    
    expect_true(Real(42) == Real(42L))
})

test_that("Real(number, bits) with MPFR enabled", {
    if (!symengine_have_component("mpfr"))
        skip("Not compiled with MPFR library")
    
    x <- Real(42L, 32)
    expect_true(get_type(x) == "RealMPFR")
    expect_identical(symengine:::s4basic_realmpfr_get_prec(x), 32L)
})

## Integer

test_that("NA_integer_ to Basic", {
    expect_error(Basic(NA_integer_))
    expect_error(S(NA_integer_))
})

test_that("Minimum and maximum integer", {
    basic_int_min <- -S(2L)^31L + 1L
    basic_int_max <- S(.Machine$integer.max)
    expect_true(get_type(basic_int_max) == "Integer")
    expect_true(get_type(basic_int_min) == "Integer")
    expect_true(basic_int_max + basic_int_min == Basic(0L))
})

test_that("as.integer overflow", {
    basic_int_min <- -S(2L)^31L + 1L
    basic_int_max <- S(.Machine$integer.max)
    
    expect_identical(as.integer(basic_int_max),  .Machine$integer.max)
    expect_identical(as.integer(basic_int_min), -.Machine$integer.max)
    
    basic_int_min_exceed <- basic_int_min - 1L
    basic_int_max_exceed <- basic_int_max + 1L
    
    ## TODO: Currently fails on Appveyor CI (windows)
    skip_on_appveyor()
    skip_on_cran()
    
    expect_true(basic_int_max_exceed > basic_int_max)
    expect_true(basic_int_min_exceed < basic_int_min)
    expect_identical(as.character(basic_int_max_exceed), "2147483648")
    expect_identical(as.character(basic_int_min_exceed), "-2147483648")
    
    expect_error(as.integer(basic_int_min_exceed))
    expect_error(as.integer(basic_int_max_exceed))
    expect_error(as.integer(basic_int_max_exceed + 1L))
})
