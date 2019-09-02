context("Summary functions")

a <- S(~a)
b <- S(~b)
c <- S(~c)

v0 <- V()
v1 <- V(a)
v2 <- V(a, b)
v3 <- V(a, b, c)

test_that("sum(a, b, c)", {
    ans <- sum(a, b, c)
    expect_true(ans == a + b + c)
})

test_that("sum(basic)", {
    ans <- sum(a)
    expect_true(ans == a)
})

test_that("sum(vecbasic)", {
    ans <- sum(v2)
    expect_true(ans == a + b)
    ans <- sum(v2)
    expect_true(ans == a + b)
})

test_that("sum([empty]) == S(0)", {
    sum2 <- selectMethod("sum", c(x = "Basic"))
    expect_true(sum2() == S(0))
})

test_that("sum(a, b) fails when len(a)>1 or len(b)>1", {
    expect_error(sum(a, v2))
})

test_that("prod(a, b, c)", {
    ans <- prod(a, b, c)
    expect_true(ans == a * b * c)
})

test_that("prod(basic)", {
    ans <- prod(a)
    expect_true(ans == a)
})

test_that("prod(vecbasic)", {
    ans <- prod(v2)
    expect_true(ans == a * b)
    ans <- prod(v2)
    expect_true(ans == a * b)
})

test_that("prod([empty]) == S(1)", {
    prod2 <- selectMethod("prod", c(x = "Basic"))
    expect_true(prod2() == S(1))
})

test_that("prod(a, b) fails when len(a)>1 or len(b)>1", {
    expect_error(prod(a, v2))
})

