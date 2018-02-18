context("Basic")

test_that("Symbol", {
    # TODO
    Symbol("x")
    Symbol("x x")
    Symbol("")
    Symbol(NA_character_)
    Symbol(42L)
    Symbol(Inf)
    Symbol(NaN)
})
