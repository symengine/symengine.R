context("lambdify")

test_that("lambdify is working", {
    f <- lambdify(S("x + y * z / 3 ^ 4"), backend = "lambda")
    expect_true(is(f, "LambdaDoubleVisitor"))
    expect_identical(formals(f), as.pairlist(alist(x =, y=, z=)))
})

#test_that("lambdify is working", {
#    f <- lambdify(S("x + y * z / 3 ^ 4"))
#    args <- formals(f)
#    expect_identical(length(args), 3L)
#    expect_identical(names(args) , c("x", "y", "z"))
#    
#    f <- lambdify(S("a + b + 2"))
#    expect_identical(as.call(body(f))[[1]], quote(`+`))
#    expect_identical(as.call(body(f))[[2]][[1]], quote(`+`))
#    expect_identical(environment(f), baseenv())
#    expect_identical(formals(f), as.pairlist(alist(a = , b = )))
#})

