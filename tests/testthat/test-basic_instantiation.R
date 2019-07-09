context("Basic Instantiation")

test_that("New Basic object does not interfere with old ones", {
    x <- S("x")
    expect_true(as.character(x) == "x")
    
    y <- S("y")
    expect_true(as.character(y) == "y")
    
    expect_true(as.character(x) == "x")
    
    ## We can use .Internal(inspect) to check the memory address
    if (FALSE) {
        .Internal(inspect(x))
        .Internal(inspect(y))
    }
})
