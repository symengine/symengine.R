context("Basic")

test_that("Symbol", {
    x <- Symbol("x")
    expect_identical(type(x), "Symbol")
    expect_identical(as.character(x),  "x")
    
    x <- Symbol("")
    expect_identical(type(x), "Symbol")
    expect_identical(as.character(x),  "")
    
    x <- Symbol("pi")
    expect_identical(type(x), "Symbol")
    expect_identical(as.character(x),  "pi")
    
    expect_error(Symbol(NA_character_))
    expect_error(Symbol(Inf))
    expect_error(Symbol(NaN))
})

test_that("Constants", {
    expect_identical(type(Constant("x")), "Constant")
    expect_true(Constant("x") != S("x"))
    
    expect_identical(type(Constant("pi")), "Constant")
    expect_true(Constant("pi") == S("pi"))
    
    ## TODO: Variable binding for common constants:
    ##       I, pi, E, EulerGamma, Catalan, GoldenRatio
})

expect_vecbasic_equal <- function(a, b) {
    expect_true(length(a) == length(b))
    for (i in seq_along(a))
        expect_true(a[[i]] == b[[i]])
    invisible()
}

test_that("get_args", {
    expr <- S("((123 + x) * y) / z")
    
    expect_identical(type(expr), "Mul")
    args <- dissect(expr)$args
    expect_identical(length(args), 3L)
    
    expr2 <- S("((123 + x) / z) * y")
    args2 <- dissect(expr2)$args
    
    ## The orders are same
    expect_vecbasic_equal(args, args2)
    expect_vecbasic_equal(args, Vector("y", "1/z", "123 + x"))
})

test_that("free_symbols", {
    expr <- S("((123 + x) * y) / z")
    vars <- dissect(expr)$free_symbols
    expect_identical(length(vars), 3L)
    expect_vecbasic_equal(vars, Vector("x", "y", "z"))
})

test_that("function_symbols", {
    # TODO: this seems to have a parsing mistake, see
    #       https://github.com/symengine/symengine/issues/1447
    # expr <- S("z + f(x + y, g(x), h(g(x)))")
    expr <- S("f(x + y, g(x), h(g(x))) + z")
    funs <- dissect(expr)$function_symbols
    
    expect_identical(length(funs), 3L)
    expect_vecbasic_equal(funs, Vector("g(x)", "h(g(x))", "f(x + y, g(x), h(g(x)))"))
})

test_that("function_getname", {
    x <- S("x")
    g <- S("g(x)")
    f <- S("f(g(x), 42)") # TODO: add function symbol constructor
    expect_identical(symengine:::s4basic_function_getname(g), "g")
    expect_identical(symengine:::s4basic_function_getname(f), "f")
    expect_error(symengine:::s4basic_function_getname(x))
})

