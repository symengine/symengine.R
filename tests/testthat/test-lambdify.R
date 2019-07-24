context("lambdify")

test_that("basic to expr conversion", {
    # Symbol
    expect_identical(as.language(Symbol(" w w")), quote(` w w`))
    # Add
    expect_setequal(
        all.vars(as.language(S("a + b + c"))),
        c("a", "b", "c")
    )
    expect_identical(as.language(S("1 + a")), quote(1L + a))
    # Mul
    expect_setequal(
        all.vars(as.language(S("a * b * c"))),
        c("a", "b", "c")
    )
    # Pow
    expect_identical(as.language(S("x ^ y")), quote(x ^ y))
    # Rational
    expect_identical(as.language(S("3/4")), 3/4)
    expect_identical(as.language(S("1/3")), 1/3)
    expect_identical(eval(as.language(S("1/pi"))), 1/pi)
    expect_identical(eval(as.language(S("1.5/pi"))), 1.5/pi)
    # Integer
    expect_identical(as.language(S("42")), 42L)
    # RealDouble
    expect_identical(as.language(S("4.2")), 4.2)
    # Infty
    expect_identical(as.language(S("inf")), Inf)
    expect_identical(eval(as.language(S("-inf"))), -Inf)
    # Constant
    expect_identical(as.language(S("pi")), pi)
})

test_that("as.expression", {
    x <- S("42")
    expect_true(is.expression(as.expression(x)))
})

test_that("as.symbol", {
    x <- S("42")
    expect_error(as.symbol(x))
    
    x <- S("x")
    expect_true(is.symbol(as.symbol(x)))
    expect_identical(as.symbol(x), quote(x))
})

test_that("lambdify is working", {
    f <- lambdify(S("x + y * z / 3 ^ 4"))
    args <- formals(f)
    expect_identical(length(args), 3L)
    expect_identical(names(args) , c("x", "y", "z"))
    
    f <- lambdify(S("a + b + 2"))
    expect_identical(as.call(body(f))[[1]], quote(`+`))
    expect_identical(as.call(body(f))[[2]][[1]], quote(`+`))
    expect_identical(environment(f), baseenv())
    expect_identical(formals(f), as.pairlist(alist(a = , b = )))
})

