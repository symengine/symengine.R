context("Conversion between R expression and Basic")

test_that("S(SYMSXP) works", {
    s <- quote(x)
    expect_true(S(s) == S("x"))
    
    s <- quote(e)
    expect_true(S(s) != S("e"))
})

test_that("S(LANGSXP) works", {
    s <- quote(x^b + 3)
    expect_true(is(S(s), "Basic"))
})

test_that("S(EXPRSXP) works", {
    s <- expression(x^b + 3)
    ## TODO
})

test_that("S(formula) works", {
    s <- local({
        ~ x^b + 3
    })
    expect_true(is(S(s), "Basic"))
})

test_that("S(LANGSXP) does not check whole number", {
    s <- quote(x^b + 3)
    num <- get_args(S(s))[[1L]]
    expect_true(get_type(num) == "RealDouble")
})

test_that("S(formula) will convert whole number to Integer", {
    s <- ~ x^b + 3
    num <- get_args(S(s))[[1L]]
    expect_true(get_type(num) == "Integer")
})

test_that("backquote works in formula", {
    s <- local({
        m <- 42L
        ~ x^.(m)
    })
    expect_true(S(s) == S("x^42"))
    
    s <- quote(x^.(m))
    expect_error(S(s))
})

test_that("Nested formula works", {
    s1 <- local({
        aa <- S("a")
        ~ x^.(aa)
    })
    s2 <- local({
        bb <- S("b")
        ~ .(s1) + y^.(bb)
    })
    expect_true(S(s2) == S("x^a + y^b"))
})

test_that("as.language(Basic) works", {
    s <- S("x ^ a")
    expect_true(identical(as.language(s), quote(x^a)))
})

test_that("as.expression(Basic) works", {
    s <- S("x ^ a")
    expect_true(identical(as.expression(s), expression(x^a)))
})

expect_twoway_equivalent <- function(r, b) {
    r_tob <- S(r)
    expect_true(r_tob == b)
    r_back <- as.language(r_tob)
    expect_identical(r, r_back)
    
    b_tor <- as.language(b)
    expect_identical(b_tor, r)
    b_back <- S(b_tor)
    expect_true(b_back == b)
}

expect_lang2basic <- function(r, b) {
    r_tob <- S(r)
    expect_true(r_tob == b)
}
expect_basic2lang <- function(r, b) {
    b_tor <- as.language(b)
    expect_identical(b_tor, r)
}

test_that("+ - * / ^", {
    expect_lang2basic(quote(a + b), S("a + b"))
    expect_basic2lang(quote(b + a), S("a + b"))
    
    expect_lang2basic(quote(a - b),            S("a - b"))
    expect_basic2lang(bquote(.(-1L) * b + a),  S("a - b"))
    
    expect_twoway_equivalent(bquote(.(-1L) * x), S("-x"))
    expect_twoway_equivalent(quote(a * b), S("a * b"))
    
    expect_lang2basic(quote(a/b),             S("a/b"))
    expect_basic2lang(bquote(a * b ^ .(-1L)), S("a/b"))
    
    expect_twoway_equivalent(quote(a^b), S("a^b"))
})

test_that("Function with single argument", {
    # expect_twoway_equivalent(quote(sin(x)), S("sin(x)"))
})


