context("Operation")

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
    c <- gcd(a, b)
    d <- lcm(a, b)
    expect_true(c == S(42L))
    expect_true(d == S(3822L))

    # factorial
    a <- factorial(10)
    expect_true(a == S(3628800L))

    # binomial
    a <- binomial(S(10L), 2)
    expect_true(a == S(45L))
})

test_that("basic Operation", {
    v1 <- vecbasic(1,2,3)
    v2 <- vecbasic(4,7,9)
    m1 <- denseMatrix(list(3,6,2,8,4,5), 2, 3)

    # add
    v <- v1 + v2
    m <- m1 + v2
    expect_vecbasic_equal(v, vecbasic(5,9,12))
    expect_denseMatrix_equal(m, denseMatrix(list(7,13,11,12,11,14),2,3))

    # sub
    v <- v1 - v2
    m <- m1 - v2
    expect_vecbasic_equal(v, vecbasic(-3,-5,-6))
    expect_denseMatrix_equal(m, denseMatrix(list(-1,-1,-7,4,-3,-4),2,3))

    # mul
    v <- v1 * v2
    m <- v2 * m1
    expect_vecbasic_equal(v, vecbasic(4,14,27))
    expect_denseMatrix_equal(m, denseMatrix(list(12,42,18,32,28,45),2,3))

    # div
    v <- v1 / v2
    m <- m1 / v2
    expect_vecbasic_equal(v, vecbasic(1/4, 2/7, 3/9))

    # pow
    v <- v1 ^ v2
    expect_vecbasic_equal(v, vecbasic(1,128,19683))

    vi1 <- vecbasic(1L,2L,3L)
    vi2 <- vecbasic(4L,7L,9L)
    # quotient
    v <- vi2 %/% vi1
    expect_vecbasic_equal(v, vecbasic(4L, 3L, 3L))

    # mod
    v <- vi2 %% vi1
    expect_vecbasic_equal(v, vecbasic(0L, 1L, 0L))
})


test_that("matrix-op", {
    v1 <- vecbasic(3,4)
    m1 <- denseMatrix(list(1,2,5,7), 2, 2)
    m2 <- denseMatrix(list(2,3,7,4), 2, 2)
    
    # multiply
    m <- m1 %*% m2
    expect_denseMatrix_equal(m, denseMatrix(list(17,25,27,42),2,2))
    m <- m2 %*% m1
    expect_denseMatrix_equal(m, denseMatrix(list(16,11,59,43),2,2))
    m <- m1 %*% v1
    expect_denseMatrix_equal(m, denseMatrix(list(23,34),2,1))
    m <- v1 %*% m1
    expect_denseMatrix_equal(m, denseMatrix(list(11,43),1,2))
    v <- v1 %*% v1
    expect_denseMatrix_equal(v, denseMatrix(25, 1, 1))
    
    # det
    a <- det(m1)
    expect_true(a == S(-3))

    # inv
    m <- denseMatrix(list('x', 'x', 1, 2), 2, 2)
    expect_true(det(inv(m)) == S(1.0) / S('x'))

    # lu
    r <- lu(m1)
    m <- r$l %*% r$u
    expect_denseMatrix_equal(m, m1)
})