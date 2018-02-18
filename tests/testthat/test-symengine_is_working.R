context("symengine_is_working")

test_that("ascii art, version, components", {
    version <- symengine_version()
    expect_true(is.character(version))
    
    ascii_art <- symengine_ascii_art()
    expect_true(is.character(ascii_art))
    
    expect_true(symengine_have_component("mpfr"))
    expect_true(symengine_have_component("mpc"))
})

test_that("GMP library is working", {
    
})

test_that("MPFR library is working", {
    
})

test_that("MPC library is working", {
    
})
