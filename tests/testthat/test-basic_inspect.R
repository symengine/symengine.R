context("Misc functions to inspect a Basic object")

expect_vecbasic_equal <- function(a, b) {
    expect_true(length(a) == length(b))
    for (i in seq_along(a))
        expect_true(a[[i]] == b[[i]])
    invisible()
}

test_that("get_args", {
    expr <- S("((123 + x) * y) / z")
    
    expect_identical(get_type(expr), "Mul")
    args <- get_args(expr)
    expect_identical(length(args), 3L)
    
    expr2 <- S("((123 + x) / z) * y")
    args2 <- get_args(expr2)
    
    ## The orders are same
    expect_vecbasic_equal(args, args2)
    expect_vecbasic_equal(args, Vector("y", "1/z", "123 + x"))
})

test_that("free_symbols", {
    expr <- S("((123 + x) * y) / z")
    vars <- free_symbols(expr)
    expect_identical(length(vars), 3L)
    expect_vecbasic_equal(vars, Vector("x", "y", "z"))
})

test_that("function_symbols", {
    expr <- S("z + f(x + y, g(x), h(g(x)))")
    funs <- function_symbols(expr)
    
    expect_identical(length(funs), 3L)
    expect_vecbasic_equal(funs, Vector("g(x)", "h(g(x))", "f(x + y, g(x), h(g(x)))"))
})

test_that("function_getname", {
    x <- S("x")
    g <- S("g(x)")
    f <- S("f(g(x), 42)") # TODO: add function symbol constructor
    expect_identical(get_name(g), "g")
    expect_identical(get_name(f), "f")
    expect_error(get_name(x))
})

