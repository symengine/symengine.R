
## Utils  ======================================================================

#' @export
api_ptr_tag <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    .Call("R_ExternalPtrTag", ptr)
}

#' @export
api_new_ptr_emptybasic <- function () {
    warning("Should not be used in R side, only for testing", immediate. = TRUE)
    .Call("new_ptr_emptybasic")
}


## Logo and Version  ===========================================================

#' @useDynLib symengine c_ascii_art_str
#' @export
api_symengine_logo <- function () {
    s <- .Call("c_ascii_art_str")   
    s
}

#' @useDynLib symengine c_symengine_version
#' @export
api_symengine_version <- function () {
    .Call("c_symengine_version")
}

#' @useDynLib symengine c_symengine_have_component
#' @export
api_symengine_have_component <- function (
    which = c("mpfr", "flint", "arb", "mpc", "ecm",
              "primesieve", "piranha", "boost", "pthread", "llvm"))
{
    vapply(which, FUN.VALUE = logical(1L),
           function (x) .Call("c_symengine_have_component", x))
}


## New Symbols  ================================================================

#' @useDynLib symengine c_new_heap_symbol
#' @export
api_new_symbol <- function (string) {
    .Call("c_new_heap_symbol", string)
}

#' @useDynLib symengine c_parse_str
#' @export
api_parse_str <- function (string) {
    .Call("c_parse_str", string)
}

## Accessors for Basic  ========================================================

#' @useDynLib symengine c_basic_str
#' @export
api_basic_str <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    .Call("c_basic_str", ptr)
}

#' @useDynLib symengine c_basic_str_julia
#' @export
api_basic_str_julia <- function (ptr) {
    # I do not know all the difference between `basic_str` and `basic_str_julia`,
    # but `basic_str_julia` will show Pow with `^` instead of `**`.
    ptr <- as(ptr, "externalptr")
    .Call("c_basic_str_julia", ptr)
}


#' @useDynLib symengine c_basic_type
#' @export
api_basic_type <- function (ptr) {
    if (typeof(ptr) == "S4")
        ptr <- as(ptr, "externalptr")
    else
        stopifnot(typeof(ptr) == "externalptr")
    
    .Call("c_basic_type", ptr)
}


## Constants  ==================================================================

#' @useDynLib symengine c_builtin_const
#' @export
api_builtin_const <- function (which) {
    id <- switch(which,
        "zero"         =  1L  ,
        "one"          =  2L  ,
        "minus_one"    =  3L  ,
        "I"            =  4L  ,
        "pi"           =  5L  ,
        "E"            =  6L  ,
        "EulerGamma"   =  7L  ,
        "Catalan"      =  8L  ,
        "GoldenRatio"  =  9L  ,
        "Inf"          = 10L  ,
        "NegInf"       = 11L  ,
        "ComplexInf"   = 12L  ,
        "Nan"          = 13L  ,
        stop("Not a builtin constant")
    )
    .Call("c_builtin_const", id)
}

#' @useDynLib symengine c_make_const
#' @export
api_make_const <- function (string) {
    .Call("c_make_const", string)
}

## Integer  ====================================================================

#' @useDynLib symengine c_integer_from_int
#' @export
api_integer_from_int <- function (x) {
    if (is.na(x) || is.infinite(x) || is.nan(x))
        warning("TODO: to support NA, Inf and NaN")
    
    stopifnot(is.integer(x)) # or as.integer(x)?
    .Call("c_integer_from_int", x)
}

#' @useDynLib symengine c_integer_from_str
#' @export
api_integer_from_str <- function (x) {
    .Call("c_integer_from_str", x)
}

#' @useDynLib symengine c_integer_get_int
#' @export
api_integer_get_int <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    
    stopifnot(api_is_a_Integer(ptr))
    .Call("c_integer_get_int", ptr)
}


## Real  =======================================================================

#' @useDynLib symengine c_realdouble_from_d
#' @export
api_realdouble_from_d <- function (x) {
    # It seems that NA, Inf and NaN are directly supported, why??
    #
    # if (is.na(x) || is.infinite(x) || is.nan(x))
    #     warning("TODO: to support NA, Inf and NaN")
    
    stopifnot(is.double(x)) # or as.double for integer?
    .Call("c_realdouble_from_d", x)
}

#' @useDynLib symengine c_realdouble_get_d
#' @export
api_realdouble_get_d <- function (ptr) {
    if (typeof(ptr) == "S4")
        ptr <- as(ptr, "externalptr")
    else
        stopifnot(typeof(ptr) == "externalptr")
    
    stopifnot(api_is_a_RealDouble(ptr))
    .Call("c_realdouble_get_d", ptr)
}

## Basic: is_a_XXX  ============================================================


#' @useDynLib symengine c_is_a_Number
#' @useDynLib symengine c_is_a_Integer
#' @useDynLib symengine c_is_a_Rational
#' @useDynLib symengine c_is_a_Symbol
#' @useDynLib symengine c_is_a_Complex
#' @useDynLib symengine c_is_a_RealDouble
#' @useDynLib symengine c_is_a_ComplexDouble
#' @useDynLib symengine c_is_a_RealMPFR
#' @useDynLib symengine c_is_a_ComplexMPC
NULL
api_is_a_Number        <- function(ptr) .Call("c_is_a_Number"        , as(ptr, "externalptr"))
api_is_a_Integer       <- function(ptr) .Call("c_is_a_Integer"       , as(ptr, "externalptr"))
api_is_a_Rational      <- function(ptr) .Call("c_is_a_Rational"      , as(ptr, "externalptr"))
api_is_a_Symbol        <- function(ptr) .Call("c_is_a_Symbol"        , as(ptr, "externalptr"))
api_is_a_Complex       <- function(ptr) .Call("c_is_a_Complex"       , as(ptr, "externalptr"))
api_is_a_RealDouble    <- function(ptr) .Call("c_is_a_RealDouble"    , as(ptr, "externalptr"))
api_is_a_ComplexDouble <- function(ptr) .Call("c_is_a_ComplexDouble" , as(ptr, "externalptr"))
api_is_a_RealMPFR      <- function(ptr) .Call("c_is_a_RealMPFR"      , as(ptr, "externalptr"))
api_is_a_ComplexMPC    <- function(ptr) .Call("c_is_a_ComplexMPC"    , as(ptr, "externalptr"))


## Number is  ==================================================================

#' @useDynLib symengine c_number_is_zero
#' @export
api_number_is_zero <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_zero", ptr)
}

#' @useDynLib symengine c_number_is_negative
#' @export
api_number_is_negative <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_negative", ptr)
}

#' @useDynLib symengine c_number_is_positive
#' @export
api_number_is_positive <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_positive", ptr)
}

#' @useDynLib symengine c_number_is_complex
#' @export
api_number_is_complex <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_complex", ptr)
}

## Operations  =================================================================

#' @useDynLib symengine c_basic_add
#' @export
api_basic_add <- function (ptra, ptrb) {
    ptra <- as(ptra, "externalptr")
    ptrb <- as(ptrb, "externalptr")
    .Call("c_basic_add", ptra, ptrb)
}

#' @useDynLib symengine c_basic_sub
#' @export
api_basic_sub <- function (ptra, ptrb) {
    ptra <- as(ptra, "externalptr")
    ptrb <- as(ptrb, "externalptr")
    .Call("c_basic_sub", ptra, ptrb)
}

#' @useDynLib symengine c_basic_mul
#' @export
api_basic_mul <- function (ptra, ptrb) {
    ptra <- as(ptra, "externalptr")
    ptrb <- as(ptrb, "externalptr")
    .Call("c_basic_mul", ptra, ptrb)
}

#' @useDynLib symengine c_basic_div
#' @export
api_basic_div <- function (ptra, ptrb) {
    ptra <- as(ptra, "externalptr")
    ptrb <- as(ptrb, "externalptr")
    .Call("c_basic_div", ptra, ptrb)
}

#' @useDynLib symengine c_basic_pow
#' @export
api_basic_pow <- function (ptra, ptrb) {
    ptra <- as(ptra, "externalptr")
    ptrb <- as(ptrb, "externalptr")
    .Call("c_basic_pow", ptra, ptrb)
}

## Diff
#' @useDynLib symengine c_basic_diff
#' @export
api_basic_diff <- function (ptrexpr, ptrsym) {
    ptrexpr <- as(ptrexpr, "externalptr")
    ptrsym  <- as(ptrsym , "externalptr")
    stopifnot(api_basic_type(ptrsym) == "Symbol")
    .Call("c_basic_diff", ptrexpr, ptrsym)
}


#' @useDynLib symengine c_basic_eq
#' @export
api_basic_eq <- function (ptra, ptrb) {
    ptra <- as(ptra, "externalptr")
    ptrb <- as(ptrb, "externalptr")
    .Call("c_basic_eq", ptra, ptrb)
}

#' @useDynLib symengine c_basic_neq
#' @export
api_basic_neq <- function (ptra, ptrb) {
    ptra <- as(ptra, "externalptr")
    ptrb <- as(ptrb, "externalptr")
    .Call("c_basic_neq", ptra, ptrb)
}

#' @useDynLib symengine c_basic_hash
#' @export
api_basic_hash <- function (ptr) {
    .Call("c_basic_hash", as(ptr, "externalptr"))
}

if (FALSE) {
    api_basic_eq(S("x == x"), S("y == y"))
    api_basic_neq(S("x == x"), S("y == y"))
    api_basic_eq(S("x + 1 > x"), S("x + 2 > x + 1"))
}


## Functions with one argument

.flist <- c(
    alist(expand, neg, abs, erf, erfc),
    
    alist(sin, cos, tan, asin, acos, atan, csc, sec, cot, acsc, asec, acot, sinh,
          cosh, tanh, asinh, acosh, atanh, csch, sech, coth, acsch, asech, acoth),
    
    alist(lambertw, zeta, dirichlet_eta, gamma, sqrt, exp, log)
)

for (i in .flist) {
    fname    <- deparse(i)
    funcname <- paste0("api_basic_", deparse(i))
    callname <- paste0("c_basic_",   deparse(i))
    func <- eval(bquote(function (ptra) {
        .Call(.(callname), as(ptra, "externalptr"))
    }))
    assign(funcname, func)
    rm(fname, funcname, callname, func, i)
}

if (FALSE) {
    for (i in .flist) {
        cat("#' @useDynLib symengine c_basic_", deparse(i), "\n", sep = "")
    }
    for (i in .flist) {
        cat("#' @export\n")
        cat("api_basic_", i, " <- ", "api_basic_", i, "\n", sep = "")
    }
}

rm(.flist)


#' @export
api_basic_expand <- api_basic_expand
#' @export
api_basic_neg <- api_basic_neg
#' @export
api_basic_abs <- api_basic_abs
#' @export
api_basic_erf <- api_basic_erf
#' @export
api_basic_erfc <- api_basic_erfc
#' @export
api_basic_sin <- api_basic_sin
#' @export
api_basic_cos <- api_basic_cos
#' @export
api_basic_tan <- api_basic_tan
#' @export
api_basic_asin <- api_basic_asin
#' @export
api_basic_acos <- api_basic_acos
#' @export
api_basic_atan <- api_basic_atan
#' @export
api_basic_csc <- api_basic_csc
#' @export
api_basic_sec <- api_basic_sec
#' @export
api_basic_cot <- api_basic_cot
#' @export
api_basic_acsc <- api_basic_acsc
#' @export
api_basic_asec <- api_basic_asec
#' @export
api_basic_acot <- api_basic_acot
#' @export
api_basic_sinh <- api_basic_sinh
#' @export
api_basic_cosh <- api_basic_cosh
#' @export
api_basic_tanh <- api_basic_tanh
#' @export
api_basic_asinh <- api_basic_asinh
#' @export
api_basic_acosh <- api_basic_acosh
#' @export
api_basic_atanh <- api_basic_atanh
#' @export
api_basic_csch <- api_basic_csch
#' @export
api_basic_sech <- api_basic_sech
#' @export
api_basic_coth <- api_basic_coth
#' @export
api_basic_acsch <- api_basic_acsch
#' @export
api_basic_asech <- api_basic_asech
#' @export
api_basic_acoth <- api_basic_acoth
#' @export
api_basic_lambertw <- api_basic_lambertw
#' @export
api_basic_zeta <- api_basic_zeta
#' @export
api_basic_dirichlet_eta <- api_basic_dirichlet_eta
#' @export
api_basic_gamma <- api_basic_gamma
#' @export
api_basic_sqrt <- api_basic_sqrt
#' @export
api_basic_exp <- api_basic_exp
#' @export
api_basic_log <- api_basic_log

#' @useDynLib symengine c_basic_expand
#' @useDynLib symengine c_basic_neg
#' @useDynLib symengine c_basic_abs
#' @useDynLib symengine c_basic_erf
#' @useDynLib symengine c_basic_erfc
#' @useDynLib symengine c_basic_sin
#' @useDynLib symengine c_basic_cos
#' @useDynLib symengine c_basic_tan
#' @useDynLib symengine c_basic_asin
#' @useDynLib symengine c_basic_acos
#' @useDynLib symengine c_basic_atan
#' @useDynLib symengine c_basic_csc
#' @useDynLib symengine c_basic_sec
#' @useDynLib symengine c_basic_cot
#' @useDynLib symengine c_basic_acsc
#' @useDynLib symengine c_basic_asec
#' @useDynLib symengine c_basic_acot
#' @useDynLib symengine c_basic_sinh
#' @useDynLib symengine c_basic_cosh
#' @useDynLib symengine c_basic_tanh
#' @useDynLib symengine c_basic_asinh
#' @useDynLib symengine c_basic_acosh
#' @useDynLib symengine c_basic_atanh
#' @useDynLib symengine c_basic_csch
#' @useDynLib symengine c_basic_sech
#' @useDynLib symengine c_basic_coth
#' @useDynLib symengine c_basic_acsch
#' @useDynLib symengine c_basic_asech
#' @useDynLib symengine c_basic_acoth
#' @useDynLib symengine c_basic_lambertw
#' @useDynLib symengine c_basic_zeta
#' @useDynLib symengine c_basic_dirichlet_eta
#' @useDynLib symengine c_basic_gamma
#' @useDynLib symengine c_basic_sqrt
#' @useDynLib symengine c_basic_exp
#' @useDynLib symengine c_basic_log
NULL

## subs
#' @useDynLib symengine c_basic_subs2
#' @export
api_basic_subs2 <- function (ptrexpr, ptrold, ptrnew) {
    ptrexpr <- as(ptrexpr, "externalptr")
    ptrold  <- as(ptrold , "externalptr")
    ptrnew  <- as(ptrnew , "externalptr")
    .Call("c_basic_subs2", ptrexpr, ptrold, ptrnew)
}

## evalf
#' @useDynLib symengine c_basic_evalf
#' @export
api_basic_evalf <- function(ptrb, bits = 53L, real = TRUE) {
    ptrb <- as(ptrb, "externalptr")
    bits <- as.integer(bits)
    real <- as.logical(real)
    .Call("c_basic_evalf", ptrb, bits, real)
}

