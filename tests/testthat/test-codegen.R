context("codegen")

test_that("ccode", {
    x <- S("x^y")
    expect_identical(codegen(x, "ccode"), "pow(x, y)")
})

test_that("mathml", {
    x <- S("x^y")
    expect_identical(codegen(x, "mathml"), "<apply><power/><ci>x</ci><ci>y</ci></apply>")
})

test_that("latex", {
    x <- S("x^y")
    expect_identical(codegen(x, "latex"), "x^y")
})

test_that("jscode", {
    x <- S("x^y")
    expect_identical(codegen(x, "jscode"), "Math.pow(x, y)")
})
