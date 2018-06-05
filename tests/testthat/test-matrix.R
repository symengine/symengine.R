context("matrix")

test_that("denseMatrix dimension", {
    m1 <- denseMatrix(c("x", "y", "z", 1, 2 ,3L), 3, 2)
    d  <- dim(m1)
    expect_true(d[1] == 3)
    expect_true(d[2] == 2)
})

test_that("denseMatrix subset and get", {
    m1 <- denseMatrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)
    expect_true(m1[[1,2]] == S(1))
    expect_true(m1[[3,2]] == S(3L))

    a <- m1[c(1,3), c(1,2)]
    expect_true(class(a) == "DenseMatrix")
    expect_true(NROW(a) == 2)
    expect_true(NCOL(a) == 2)
    expect_true(a[[1,1]] == S("x"))
    expect_true(a[[1,2]] == S(1))
    expect_true(a[[2,1]] == S("z"))
    expect_true(a[[2,2]] == S(3L))
})

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

expect_vecbasic_equal <- function(a, b) {
    expect_true(length(a) == length(b))
    for (i in seq_along(a))
        expect_true(a[[i]] == b[[i]])
    invisible()
}

test_that("Single bracket subscript subsetting", {
    m1 <- denseMatrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)

    # Missing first index
    expect_denseMatrix_equal(m1[, c(2)], m1[1:3, 2])

    # Missing second index
    expect_vecbasic_equal(m1[c(1,3,6)], vecbasic("x", "z", 3L))

    # Missing index
    expect_denseMatrix_equal(m1[], m1)

    # Out of bounds
    expect_error(m1[3,3], regexp = "out-of-bounds indices")
    expect_error(m1[4,2], regexp = "out-of-bounds indices")

    # Negative index
    expect_denseMatrix_equal(m1[-(1:2), 1:2], m1[3, 1:2])
    expect_denseMatrix_equal(m1[1:2, -1], m1[1:2, 2])

    # Logical index
    expect_denseMatrix_equal(m1[c(TRUE, FALSE), 1], m1[c(1,3), 1])

    # Logical index out-of-bounds
    expect_error(m1[rep(TRUE, 4), 1], regexp = "out-of-bounds TRUE values")

    # Mix negative and positive index
    expect_error(m1[c(-1, 1), 1], regexp = "Only 0's may be mixed with negative subscripts")

    # Zero index
    expect_denseMatrix_equal(m1[c(0,1), 1], m1[1,1])
})

test_that("Single bracket subscript replacing", {
    # Assign denseMatrix
    m  <- denseMatrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)
    m2 <- denseMatrix(list("h", "y", "h"), 3, 1)
    m[1:3, 1] <- m2
    expect_denseMatrix_equal(m, denseMatrix(list("h", "y", "h", 1, 2, 3L), 3, 2))

    # Assing denseMatrix (error if not multiple)
    m  <- denseMatrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)
    m2 <- denseMatrix(list("h", "y", "h"), 3, 1)
    expect_error(m[1:2, 1:2] <- m2, regexp = paste("number of items to replace is not",
        "a multiple of replacement length"))

    # Assign denseMatrix (error if to replace has zero length)
    m  <- denseMatrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)
    m2 <- denseMatrix(list("h", "y", "h"), 3, 1)
    expect_error(m[1:2, 1:2] <- m2[0,0], regexp = paste("Replacement has length zero"))

    # Assign vecbasic
    m <- denseMatrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)
    v <- vecbasic(7,8,9,10)
    m[1:2, 1:2] <- v
    expect_denseMatrix_equal(m, denseMatrix(list(7, 8, "z", 9, 10, 3L), 3, 2))

    # Assign basic
    m <- denseMatrix(list("x", "y", 1 ,3L), 2, 2)
    s <- S('z')
    m[1, 1:2] <- s
    expect_denseMatrix_equal(m, denseMatrix(list("z", "y", "z" ,3L), 2, 2))
  
    # Negative index
    m1 <- denseMatrix(list("x", "y", 1, 3L), 2, 2)
    m1[-2, 1] <- S("z")
    expect_denseMatrix_equal(m1, denseMatrix(list("z", "y", 1, 3L), 2, 2))

    # Check copy-on-modify
    m1 <- denseMatrix(list("x", "y", 1, 3L), 2, 2)
    m2 <- m1
    m2[1,1] <- S("q")
    expect_denseMatrix_equal(m1, denseMatrix(list("x", "y", 1, 3L), 2, 2))
    expect_denseMatrix_equal(m2, denseMatrix(list("q", "y", 1, 3L), 2, 2))
})