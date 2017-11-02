
## Utils  ======================================================================

#' @export
api_ptr_tag <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    .Call("R_ExternalPtrTag", ptr)
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
    if (typeof(ptr) == "S4")
        ptr <- as(ptr, "externalptr")
    else
        stopifnot(typeof(ptr) == "externalptr")
    
    .Call("c_basic_str", ptr)
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
    if (typeof(ptr) == "S4")
        ptr <- as(ptr, "externalptr")
    else
        stopifnot(typeof(ptr) == "externalptr")
    
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

.function_template <- function (c_call, envir = parent.frame()) {
    f <- bquote(function (ptr) {
        if (typeof(ptr) == "S4")
            ptr <- as(ptr, "externalptr")
        else
            stopifnot(typeof(ptr) == "externalptr")
        .Call(.(c_call), ptr)
    })
    eval(f, envir = envir)
}

#' @export
api_is_a_Number            <- .function_template("c_is_a_Number")
#' @export
api_is_a_Integer           <- .function_template("c_is_a_Integer")
#' @export
api_is_a_Rational          <- .function_template("c_is_a_Rational")
#' @export
api_is_a_Symbol            <- .function_template("c_is_a_Symbol")
#' @export
api_is_a_Complex           <- .function_template("c_is_a_Complex")
#' @export
api_is_a_RealDouble        <- .function_template("c_is_a_RealDouble")
#' @export
api_is_a_ComplexDouble     <- .function_template("c_is_a_ComplexDouble")
#' @export
api_is_a_RealMPFR          <- .function_template("c_is_a_RealMPFR")
#' @export
api_is_a_ComplexMPC        <- .function_template("c_is_a_ComplexMPC")

rm(.function_template)


## Number is  ==================================================================

#' @useDynLib symengine c_number_is_zero
#' @export
api_number_is_zero <- function (ptr) {
    if (typeof(ptr) == "S4")
        ptr <- as(ptr, "externalptr")
    else
        stopifnot(typeof(ptr) == "externalptr")
    
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_zero", ptr)
}

#' @useDynLib symengine c_number_is_negative
#' @export
api_number_is_negative <- function (ptr) {
    if (typeof(ptr) == "S4")
        ptr <- as(ptr, "externalptr")
    else
        stopifnot(typeof(ptr) == "externalptr")
    
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_negative", ptr)
}

#' @useDynLib symengine c_number_is_positive
#' @export
api_number_is_positive <- function (ptr) {
    if (typeof(ptr) == "S4")
        ptr <- as(ptr, "externalptr")
    else
        stopifnot(typeof(ptr) == "externalptr")
    
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_positive", ptr)
}

#' @useDynLib symengine c_number_is_complex
#' @export
api_number_is_complex <- function (ptr) {
    if (typeof(ptr) == "S4")
        ptr <- as(ptr, "externalptr")
    else
        stopifnot(typeof(ptr) == "externalptr")
    
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


