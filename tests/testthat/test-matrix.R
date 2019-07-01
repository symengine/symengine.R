context("matrix")

test_that("denseMatrix dimension", {
    m1 <- Matrix(c("x", "y", "z", 1, 2 ,3L), 3, 2)
    d  <- dim(m1)
    expect_true(d[1] == 3)
    expect_true(d[2] == 2)
})

test_that("denseMatrix subset and get", {
    m1 <- Matrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)
    expect_true(m1[[1,2]] == Real(1))
    expect_true(m1[[3,2]] == S(3L))

    a <- m1[c(1,3), c(1,2)]
    expect_true(class(a) == "DenseMatrix")
    expect_true(NROW(a) == 2)
    expect_true(NCOL(a) == 2)
    expect_true(a[[1,1]] == S("x"))
    expect_true(a[[1,2]] == Real(1))
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
    m1 <- Matrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)

    # Missing first index
    expect_denseMatrix_equal(m1[, 2, drop = FALSE], m1[1:3, 2, drop = FALSE])
    expect_vecbasic_equal(m1[, 2, drop = TRUE], m1[1:3, 2, drop = TRUE])

    # Missing second index
    expect_vecbasic_equal(m1[c(1,3,6)], Vector("x", "z", 3L))

    # Second index empty
    expect_denseMatrix_equal(m1[c(1,3),], Matrix(list("x","z",1, 3L),2,2))

    # Missing index
    expect_denseMatrix_equal(m1[], m1)

    # Out of bounds
    expect_error(m1[3,3], regexp = "out-of-bounds indices")
    expect_error(m1[4,2], regexp = "out-of-bounds indices")

    # Negative index
    expect_vecbasic_equal(m1[-(1:2), 1:2], m1[3, 1:2])
    expect_vecbasic_equal(m1[1:2, -1], m1[1:2, 2])

    # Logical index
    expect_vecbasic_equal(m1[c(TRUE, FALSE), 1], m1[c(1,3), 1])

    # Logical index out-of-bounds
    expect_error(m1[rep(TRUE, 4), 1], regexp = "out-of-bounds TRUE values")

    # Mix negative and positive index
    expect_error(m1[c(-1, 1), 1], regexp = "Only 0's may be mixed with negative subscripts")

    # Zero index
    expect_true(m1[c(0,1), 1] == m1[1,1])
})

test_that("Single bracket subscript replacing", {
    # Assign denseMatrix
    m  <- Matrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)
    m2 <- Matrix(list("h", "y", "h"), 3, 1)
    m[1:3, 1] <- m2
    expect_denseMatrix_equal(m, Matrix(list("h", "y", "h", 1, 2, 3L), 3, 2))

    # Assing denseMatrix (error if not multiple)
    m  <- Matrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)
    m2 <- Matrix(list("h", "y", "h"), 3, 1)
    expect_warning(m[1:2, 1:2] <- m2)

    # Assign denseMatrix (error if to replace has zero length)
    m  <- Matrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)
    m2 <- Matrix(list("h", "y", "h"), 3, 1)
    expect_error(m[1:2, 1:2] <- m2[0,0])

    # Assign vecbasic
    m <- Matrix(list("x", "y", "z", 1, 2 ,3L), 3, 2)
    v <- Vector(7,8,9,10)
    m[1:2, 1:2] <- v
    expect_denseMatrix_equal(m, Matrix(list(7, 8, "z", 9, 10, 3L), 3, 2))

    # Assign basic
    m <- Matrix(list("x", "y", 1 ,3L), 2, 2)
    s <- S('z')
    m[1, 1:2] <- s
    expect_denseMatrix_equal(m, Matrix(list("z", "y", "z" ,3L), 2, 2))
  
    # Negative index
    m1 <- Matrix(list("x", "y", 1, 3L), 2, 2)
    m1[-2, 1] <- S("z")
    expect_denseMatrix_equal(m1, Matrix(list("z", "y", 1, 3L), 2, 2))

    # Check copy-on-modify
    m1 <- Matrix(list("x", "y", 1, 3L), 2, 2)
    m2 <- m1
    m2[1,1] <- S("q")
    expect_denseMatrix_equal(m1, Matrix(list("x", "y", 1, 3L), 2, 2))
    expect_denseMatrix_equal(m2, Matrix(list("q", "y", 1, 3L), 2, 2))
})

test_that("rbind and cbind", {
    v1 <- Vector(1, 2, 3)
    v2 <- Vector(1, 2, 3, 4)
    m1 <- Matrix(1:6, 2, 3)
    m2 <- Matrix(1:6, 3, 2)

    # rbind
    m3 <- rbind(v1,m1)
    m4 <- Matrix(list(1,1L,2L,2,3L,4L,3,5L,6L),3,3)
    expect_denseMatrix_equal(m3, m4)

    # rbind
    expect_error(rbind(v2,m1))

    # rbind (error)
    expect_error(rbind(m1,m2))

    # cbind
    m3 <- cbind(v1,m2)
    m4 <- Matrix(list(1,2,3,1L,2L,3L,4L,5L,6L), 3, 3)
    expect_denseMatrix_equal(m3, m4)

    # cbind (error)
    expect_error(cbind(v2,m2))

    # cbind (error)
    expect_error(cbind(m1,m2))
})

test_that("Implicit missing and explicit missing in subsetting", {
    expect_no_error <- function(expr) expr
    m <- Matrix(1:20, nrow = 4)
    expect_no_error(m[5])
    expect_error(m[5, ])
    
    expect_equal(length(m[5]), 1L)
    expect_equal(length(m[c(4,5)]), 2L)
    
    ## TODO: check type of
    ## m[5], m[c(4,5)], m[5, drop=T/F] and m[c(4,5), drop=T/F]
})




