context("Function Symbol")

test_that("Function works", {
    f <- Function("f")
    expect_true(get_type(f("x")) == "FunctionSymbol")
})

test_that("FunctionSymbol works", {
    f <- Function("f")
    f_x <- FunctionSymbol("f", c("x"))
    expect_true(f_x == f("x"))
    expect_true(f_x == S("f(x)"))
})
