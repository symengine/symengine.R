context("Basic")

test_that("Symbol", {
    x <- Symbol("x")
    expect_identical(basic_type(x), "Symbol")
    expect_identical(basic_str(x),  "x")
    
    x <- Symbol("")
    expect_identical(basic_type(x), "Symbol")
    expect_identical(basic_str(x),  "")
    
    x <- Symbol(NA_character_)
    expect_identical(basic_type(x), "Symbol")
    expect_identical(basic_str(x),  "NA")
    
    x <- Symbol(Inf)
    expect_identical(basic_type(x), "Symbol")
    expect_identical(basic_str(x),  "Inf")
    
    x <- Symbol(NaN)
    expect_identical(basic_type(x), "Symbol")
    expect_identical(basic_str(x),  "NaN")
    
})
