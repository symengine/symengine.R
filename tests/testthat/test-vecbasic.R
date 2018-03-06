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
})

expect_vecbasic_equal <- function(a, b) {
    expect_true(length(a) == length(b))
    for (i in seq_along(a))
        expect_true(a[[i]] == b[[i]])
    invisible()
}

test_that("Single bracket subscript subsetting", {
    v <- vecbasic("x", "y", "z", 1, 2, 3, 42L)
    
    expect_vecbasic_equal(v[c(1, 2)], c(v[1], v[[2]]))
    
    # Out of bounds
    expect_error(v[8], regexp = "out-of-bounds indices")
    
    # Negative index
    expect_vecbasic_equal(v[-(1:6)], v[7])
    
    # Logical index
    expect_vecbasic_equal(v[c(TRUE, FALSE)], v[c(1,3,5,7)])
    
    # Logical index out-of-bounds
    expect_error(v[rep(TRUE, 8)], regexp = "out-of-bounds TRUE values")
    expect_silent(v[rep(FALSE, 8)])
    
    # Mix negative and positive index
    expect_error(v[c(-1, 1)], regexp = "Only 0's may be mixed with negative subscripts")
    
    # Zero index
    expect_vecbasic_equal(v[0], vecbasic())
    expect_vecbasic_equal(v[c(0, 1)], v[1])
    
    # Missing index
    expect_vecbasic_equal(v[], v)
})


test_that("Single bracket subscript replacing", {
    
    # Assign vecbasic
    v  <- vecbasic(1,2,3)
    v2 <- vecbasic("a","b")
    v[c(2,3)] <- v2
    expect_vecbasic_equal(v, vecbasic(1,"a","b"))
    
    # Assign vecbasic (with recycling)
    v  <- vecbasic(1,2,3)
    v2 <- vecbasic("a")
    v[c(2,3)] <- v2
    expect_vecbasic_equal(v, vecbasic(1,"a","a"))
    
    # Assign vecbasic (warning with recycling)
    v  <- vecbasic(1,2,3)
    v2 <- vecbasic("a","b")
    expect_warning(v[c(1,2,3)] <- v2, "not a sub-multiple of the number of")
    expect_vecbasic_equal(v, vecbasic("a","b","a"))
    
    # Assign basic (with recycling)
    v  <- vecbasic(1,2,3)
    v[c(2,3)] <- S("a")
    expect_vecbasic_equal(v, vecbasic(1, "a", "a"))
    
    # Negative index
    v  <- vecbasic(1,2,3)
    v[-1] <- S("a")
    expect_vecbasic_equal(v, vecbasic(1, "a", "a"))
    
    # Check copy-on-modify
    v1 <- vecbasic(1,2,3)
    v2 <- v1
    v2[1] <- S("x")
    expect_vecbasic_equal(v1, vecbasic(1,2,3))
    expect_vecbasic_equal(v2, vecbasic("x",2,3))
    
    # TODO: check address of v
})




