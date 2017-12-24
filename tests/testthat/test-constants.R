context("Test Constants")

test_that("Error in `api_builtin_const`", {
    expect_error(regexp = "Not a builtin constant", {
        api_builtin_const("inf")
    })
})

test_that("Builtin constants matching its str", {
    check_const_match_str <- function (from, to) {
        local({
            # Using `api_builtin_const`
            ptr <- api_builtin_const(from)
            res <- api_basic_str(ptr)
            
            expect_identical(res, to)
        })
        
        local({
            # Using `Constant`
            s   <- BuiltinConstant(from)
            res <- api_basic_str(s@.xData)
            
            expect_identical(res, to)
        })
        
        invisible(TRUE)
    }
    pairs <- c(
        "zero"         = "0"            ,
        "one"          = "1"            ,
        "minus_one"    = "-1"           ,
        "I"            = "I"            ,
        "pi"           = "pi"           ,
        "E"            = "E"            ,
        "EulerGamma"   = "EulerGamma"   ,
        "Catalan"      = "Catalan"      ,
        "GoldenRatio"  = "GoldenRatio"  ,
        "Inf"          = "oo"           ,
        "NegInf"       = "-oo"          ,
        "ComplexInf"   = "zoo"          ,
        "Nan"          = "nan"
    )
    
    for (i in seq_along(pairs))
        check_const_match_str(names(pairs)[[i]], pairs[[i]])
    
})

test_that("Custom constants", {
    # TODO
})
