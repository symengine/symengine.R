context("Basic")

test_that("Symbol", {
    x <- Symbol("x")
    expect_identical(basic_type(x), "Symbol")
    x <- Symbol("")
    expect_identical(basic_type(x), "Symbol")
    x <- Symbol(NA_character_)
    expect_identical(basic_type(x), "Symbol")
    x <- Symbol(42L)
    expect_identical(basic_type(x), "Symbol")
    x <- Symbol(Inf)
    expect_identical(basic_type(x), "Symbol")
    x <- Symbol(NaN)
    expect_identical(basic_type(x), "Symbol")
    
})
