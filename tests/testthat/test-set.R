context("SetBasic")

test_that("setbasic length", {
    s <- setbasic("x", "y", "z", 1, 2 ,3L)
    expect_true(length(s) == 6)
})

expect_setbasic_equal <- function(a, b) {
    expect_true(length(a) == length(b))
    for (i in 1:length(a)) {
        expect_true(a[[i]] == b[[i]])
    }
    invisible()
}

test_that("setbasic concatenate", {
    s1 <- setbasic("x", "y", "z", 1, 2 ,3L)
    s2 <- setbasic("x", "y", "z")
    s2 <- c(s1, 1, 2, 3L)
    expect_setbasic_equal(s1, s2)
})

test_that("setbasic get and subset", {
  s1 <- setbasic("x", "y", "z")
  
  # get
  expect_true(s1[[2]] == S("y"))

  # get wrong dimension
  expect_error(s1[[2,]], "incorrect number of dimensions")
  expect_error(s1[[,1]], "incorrect number of subscripts")

  # subset
  expect_setbasic_equal(s1[1:2], setbasic("x", "y"))

  # subset wrong dimension
  expect_error(s1[2,], "incorrect number of dimensions")
  expect_error(s1[,2], "incorrect number of dimensions")
})
