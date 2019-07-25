
context("VecBasic constructors: Vector, V, c")

expect_vecbasic_identical <- function(a, b) {
    expect_true(is(a, "VecBasic"))
    expect_true(is(b, "VecBasic"))
    expect_true(length(a) == length(b))
    for (i in seq(length(a)))
        expect_true(a[[i]] == b[[i]])
}

test_that("Vector(x) for singular x", {
    # Basic
    x <- S("a^b")
    expect_true(length(Vector(x)) == 1L)
    
    # Integer
    x <- 42L
    expect_true(is(Vector(x), "VecBasic"))
    
    # list
    x <- list(42L)
    expect_true(is(Vector(x), "VecBasic"))
    expect_true(length(x) == 1L)
    
    # empty vector
    for (x in list(integer(), numeric(), character(), list())) {
        expect_true(is(Vector(x), "VecBasic"))
        expect_true(length(x) == 0L)
    }
    
    # String
    x <- "pi"
    expect_true(get_type(Vector(x)[[1]]) == "Constant")
    
    # Formula
    x <- ~ a^b
    expect_true(is(Vector(x), "VecBasic"))
    expect_true(length(Vector(x)) == 1L)
    
    # Expression
    #x <- quote(a^b)
    #expect_true(length(Vector(x)) == 1L)
})

test_that("Vector(c(a, b))", {
    # Integer
    x <- c(1L, 2L)
    expect_true(length(Vector(x)) == 2L)
    
    # List
    x <- list(1L, 2L)
    expect_vecbasic_identical(Vector(x), Vector(c(1L, 2L)))
    
    # Nested list is not supported
    x <- list(1L, c(2L, 3L))
    expect_error(Vector(x))
    
    # List of formula
    x <- list(~a^b, ~b^a)
    expect_true(length(Vector(x)) == 2L)
    
    # List of expressions
    #x <- list(quote(a^b), quote(b^a))
    #expect_true(length(Vector(x)) == 2L)
})

test_that("as(x, 'VecBasic')", {
    x <- c(1, 2)
    expect_vecbasic_identical(Vector(x), as(x, "VecBasic"))
})

test_that("Vector(x, ...)", {
})

test_that("V()", {
    
})

test_that("c() for VecBasic", {
    
})
