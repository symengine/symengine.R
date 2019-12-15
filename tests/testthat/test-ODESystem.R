
context("ODESystem")

test_that("dxdt constructor", {
    x <- S("x")
    rhs <- x^2L + x + 1L
    eq <- dxdt(x) == rhs
    expect_true(class(eq) == "DxdtOde")
})

x <- S("x")
y <- S("y")
rhs1 <- 1L / (x^2L + y + 1L)
rhs2 <- 1L / (y^2L + 2L*x + 1L)

test_that("ODESystem and predict works", {
    skip_if(!requireNamespace("odeintr", quietly = TRUE))
    eq1 <- dxdt(x) == rhs1
    eq2 <- dxdt(y) == rhs2
    sys <- ODESystem(eq1, eq2)
    expect_true(class(sys) == "ODESystem")
    res <- predict(sys, init = c(x = 1, y = 1), duration = 10)
    expect_true(is.data.frame(res))
    expect_true(identical(names(res), c("Time", "x", "y")))
})
