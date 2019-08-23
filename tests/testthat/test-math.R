context("Math functions")

test_that("lgamma works", {
    s <- lgamma(S("x"))
    expect_true(get_type(s) == "LogGamma")
    
    s <- lgamma(S("1"))
    expect_true(s == S(0))
})

expect_vecbasic_equal <- function(a, b) {
    expect_true(length(a) == length(b))
    for (i in seq_along(a))
        expect_true(a[[i]] == b[[i]])
    invisible()
}

expect_denseMatrix_equal <- function(a, b) {
    expect_true(NROW(a) == NROW(b))
    expect_true(NCOL(a) == NCOL(b))
    for (i in 1:NROW(a)) {
        for (j in 1:NCOL(a)) {
            expect_true(a[[i,j]] == b[[i,j]])
        }
    }
    invisible()
}

test_that("ntheory", {
    # nextprime
    a <- S(11L)
    b <- nextprime(a)
    expect_true(b == S(13L))

    # gcd & lcm
    a <- S(294L)
    b <- S(546L)
    c <- GCD(a, b)
    d <- LCM(a, b)
    expect_true(c == S(42L))
    expect_true(d == S(3822L))

    # factorial
    a <- factorial(S(10))
    expect_true(a == S(3628800L))

    # binomial
    a <- choose(S(10L), 2)
    expect_true(a == S(45L))
})

test_that("basic Operation", {
    v1 <- Vector(1,2,3)
    v2 <- Vector(4,7,9)
    m1 <- Matrix(list(3,6,2,8,4,5), 2, 3)

    # add
    v <- v1 + v2
    m <- m1 + v2
    expect_vecbasic_equal(v, Vector(5,9,12))
    expect_denseMatrix_equal(m, Matrix(list(7,13,11,12,11,14),2,3))

    # sub
    v <- v1 - v2
    m <- m1 - v2
    expect_vecbasic_equal(v, Vector(-3,-5,-6))
    expect_denseMatrix_equal(m, Matrix(list(-1,-1,-7,4,-3,-4),2,3))

    # mul
    v <- v1 * v2
    m <- v2 * m1
    expect_vecbasic_equal(v, Vector(4,14,27))
    expect_denseMatrix_equal(m, Matrix(list(12,42,18,32,28,45),2,3))

    # div
    v <- v1 / v2
    m <- m1 / v2
    expect_vecbasic_equal(v, Vector(1/4, 2/7, 3/9))

    # pow
    v <- v1 ^ v2
    expect_vecbasic_equal(v, Vector(1,128,19683))

    vi1 <- Vector(1L,2L,3L)
    vi2 <- Vector(4L,7L,9L)
    # quotient
    v <- vi2 %/% vi1
    expect_vecbasic_equal(v, Vector(4L, 3L, 3L))

    # mod
    v <- vi2 %% vi1
    expect_vecbasic_equal(v, Vector(0L, 1L, 0L))
})


test_that("matrix-op", {
    v1 <- Vector(3,4)
    m1 <- Matrix(list(1,2,5,7), 2, 2)
    m2 <- Matrix(list(2,3,7,4), 2, 2)
    
    # multiply
    m <- m1 %*% m2
    expect_denseMatrix_equal(m, Matrix(list(17,25,27,42),2,2))
    m <- m2 %*% m1
    expect_denseMatrix_equal(m, Matrix(list(16,11,59,43),2,2))
    m <- m1 %*% v1
    expect_denseMatrix_equal(m, Matrix(list(23,34),2,1))
    m <- v1 %*% m1
    expect_denseMatrix_equal(m, Matrix(list(11,43),1,2))
    v <- v1 %*% v1
    expect_denseMatrix_equal(v, Matrix(25, 1, 1))
    
    # det
    a <- det(m1)
    expect_true(a == Real(-3))

    # inv
    m <- Matrix(list('x', 'x', 1, 2), 2, 2)
    expect_true(det(solve(m)) == Real(1.0) / S('x'))

    # lu (TODO)
    # r <- lu(m1)
    # m <- r$l %*% r$u
    # expect_denseMatrix_equal(m, m1)
})


## Some two-args functions

test_that("atan2", {
    s <- atan2(S(1), S(2))
    expect_true(is(s, "Basic"))
    expect_identical(as.double(evalf(s)), atan2(1L, 2L))
})

test_that("kronecker_delta", {
    s <- kronecker_delta(S("x"), S("x"))
    expect_true(s == S(1L))
})

test_that("lowergamma and uppergamma", {
    skip_if_not_installed("pracma")
    
    lower <- pracma::gammainc(3, 2)['lowinc']
    upper <- pracma::gammainc(3, 2)['uppinc']
    s_upper <- uppergamma(S(3), S(2))
    s_lower <- lowergamma(S(3), S(2))
    expect_true(is(s_upper, "Basic"))
    expect_true(is(s_lower, "Basic"))
    expect_true(abs(as.double(evalf(s_upper)) - upper) < 10^-15)
    expect_true(abs(as.double(evalf(s_lower)) - lower) < 10^-15)
})

test_that("beta", {
    s <- beta(S(3), S(2))
    expect_true(is(s, "Basic"))
    expect_identical(as.double(evalf(s)), beta(3, 2))
})

test_that("psigamma", {
    s <- psigamma(S(4), S(3))
    expect_true(is(s, "Basic"))
    expect_equal(as.double(evalf(s)), psigamma(4L,3L))
})

test_that("digamma, trigamma", {
    s <- digamma(S(4))
    expect_true(is(s, "Basic"))
    expect_equal(as.double(evalf(s)), digamma(4L))
    
    s <- trigamma(S(4))
    expect_true(is(s, "Basic"))
    expect_equal(as.double(evalf(s)), trigamma(4L))
})

