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

test_that("Constants", {
    #expect_true(basic_I() == S(1i))
    expect_true(basic_I()  == S("1I"))
    
    expect_true(basic_pi() == S("pi"))
    expect_true(basic_pi() == Constant("pi"))
    
    expect_true(basic_E() == S("E"))
    expect_true(basic_E() == Constant("E"))
    
    expect_true(basic_EulerGamma() == S("EulerGamma"))
    expect_true(basic_EulerGamma() == Constant("EulerGamma"))
    
    expect_true(basic_Catalan() == S("Catalan"))
    expect_true(basic_Catalan() == Constant("Catalan"))
    
    expect_true(basic_GoldenRatio() == S("GoldenRatio"))
    expect_true(basic_GoldenRatio() == Constant("GoldenRatio"))
    
    # TODO:
    #expect_true(basic_infinity() == S(Inf))
    #expect_true(basic_neginfinity() == S(-Inf))
    #expect_true(basic_complex_infinity() == ???)
    #expect_true(basic_nan() = ???)
})


