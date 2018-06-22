context("MapBasic")

test_that("mapbasic length", {
    m <- mapbasic(list(1,2,3), list('x','y','z'))
    expect_true(length(m) == 3)
})

test_that("mapbasic get", {
    m <- mapbasic(list(1,2,3L), list('x','y','z'))
    
    # get
    expect_true(m[[S(1)]] == S('x'))
    expect_true(m[[S(3L)]] == S('z'))

    # get (key doesn't exist)
    expect_error(m[[S(3)]], "key doesn't exist")
    
    # get (incorrect dimension)
    expect_error(m[[S(3),]], "incorrect number of dimensions")
})