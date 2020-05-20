context("solve")

test_that("solving polynomial", {
    p <- S(~ x^2 - 2*x + 1)
    ans <- solve(p)
    
    expect_true(is(ans, "VecBasic"))
    expect_true(length(ans) == 1)
    expect_true(ans[[1]] == S(1L))
})

## TODO...
