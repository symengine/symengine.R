

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
    stopifnot(identical(typeof(ptr), "externalptr"))
    .Call("c_basic_str", ptr)
}

#' @useDynLib symengine c_basic_type
#' @export
api_basic_type <- function (ptr) {
    stopifnot(identical(typeof(ptr), "externalptr"))
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

# api_get_const <- function (x) {
#     # Available choices are specified in `get_const` function
#         "zero",
#         "one",
#         "minus_one",
#         "I",
#         "pi",
#         "E",
#         "EulerGamma",
#         "Catalan",
#         "GoldenRatio",
#         "Inf",
#         "NegInf",
#         "ComplexInf",
#         "Nan"
#     callback_u <- function (which) {
#         
#     }
#     .Call("c_get_const", x)
# }

