context("Basic")

test_that("Symbol", {
    x <- Symbol("x")
    expect_identical(basic_type(x), "Symbol")
    expect_identical(basic_str(x),  "x")
    
    x <- Symbol("")
    expect_identical(basic_type(x), "Symbol")
    expect_identical(basic_str(x),  "")
    
    x <- Symbol(NA_character_)
    expect_identical(basic_type(x), "Symbol")
    expect_identical(basic_str(x),  "NA")
    
    x <- Symbol(Inf)
    expect_identical(basic_type(x), "Symbol")
    expect_identical(basic_str(x),  "Inf")
    
    x <- Symbol(NaN)
    expect_identical(basic_type(x), "Symbol")
    expect_identical(basic_str(x),  "NaN")
    
})

test_that("Constants", {
    #expect_true(basic_I() == S(1i))
    expect_true(basic_I()  == S("1I"))
    
    expect_true(basic_pi() == S("pi"))
    expect_true(basic_pi() == Constant("pi"))
    
    expect_true(basic_E() == S("E"))
    expect_true(basic_E() == Constant("E"))
    
    expect_true(basic_EulerGamma() == S("EulerGamma"))
    expect_true(basic_EulerGamma() == Constant("EulerGamma"))
    
    expect_true(basic_Catalan() == S("Catalan"))
    expect_true(basic_Catalan() == Constant("Catalan"))
    
    expect_true(basic_GoldenRatio() == S("GoldenRatio"))
    expect_true(basic_GoldenRatio() == Constant("GoldenRatio"))
    
    # TODO:
    #expect_true(basic_infinity() == S(Inf))
    #expect_true(basic_neginfinity() == S(-Inf))
    #expect_true(basic_complex_infinity() == ???)
    #expect_true(basic_nan() = ???)
})

expect_vecbasic_equal <- function(a, b) {
    expect_true(length(a) == length(b))
    for (i in seq_along(a))
        expect_true(a[[i]] == b[[i]])
    invisible()
}

test_that("get_args", {
    expr <- S("((123 + x) * y) / z")
    expect_identical(basic_type(expr), "Mul")
    args <- basic_get_args(expr)
    expect_identical(length(args), 3L)
    
    expr2 <- S("((123 + x) / z) * y")
    args2 <- basic_get_args(expr2)
    
    ## The orders are same
    expect_vecbasic_equal(args, args2)
    
    expect_vecbasic_equal(args, vecbasic("y", "1/z", "123 + x"))
})

test_that("free_symbols", {
    expr <- S("((123 + x) * y) / z")
    vars <- basic_free_symbols(expr)
    expect_identical(length(vars), 3L)
    expect_vecbasic_equal(vars, vecbasic("x", "y", "z"))
})

test_that("function_symbols", {
    # TODO: this seems to have a parsing mistake, see
    #       https://github.com/symengine/symengine/issues/1447
    # expr <- S("z + f(x + y, g(x), h(g(x)))")
    expr <- S("f(x + y, g(x), h(g(x))) + z")
    funs <- basic_function_symbols(expr)
    
    expect_identical(length(funs), 3L)
    expect_vecbasic_equal(funs, vecbasic("g(x)", "h(g(x))", "f(x + y, g(x), h(g(x)))"))
})

