context("vecbasic")

test_that("vecbasic length", {
    v1 <- vecbasic("x", "y", "z", 1, 2, 3, 42L)
    expect_true(length(v1) == 7)
})

test_that("vecbasic subset and get", {
    v1 <- vecbasic("x", "y", "z", 1, 2, 3, 42L)
    
    expect_true(v1[[1]] == S("x"))
    
    a <- v1[c(1,7)]
    expect_true(class(a) == "VecBasic")
    expect_true(a[[1]] == S("x"))
    expect_true(a[[2]] == S(42L))
    
    # TODO: we should normalize the index so that it works with boolean, negative
    # integer, etc.
})

