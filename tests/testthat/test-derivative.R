context("Derivative")

test_that("D(expr, name) with more than one name", {
    expr <- S(~ (x + y^2 + z)^3 - 4*z + e)
    
    res1 <- D(expr, ~x)
    expect_true(is(res1, "Basic"))
    
    res2 <- D(expr, c(~x, ~y, ~e))
    expect_true(length(res2) == 3L)
    
    res3 <- D(expr, c("x", "y", "e"))
    expect_true(length(res2) == 3L)
    
    ## "e" is a Constant instead of a Symbol
    expect_error(D(expr, V("x", "y", "e")))
    
    res4 <- D(expr, V("x", "y", "z"))
    expect_true(length(res4) == 3L)
})
