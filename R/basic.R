

setClass("Basic", contains = "externalptr")

## Show Methods ================================================================

setMethod("show", "Basic",
    function (object) {
        ptr <- as(object, "externalptr")
        
        type <- sprintf("(%s)", api_basic_type(ptr))
        str  <- api_basic_str(ptr)
        
        if (requireNamespace("crayon", quietly = TRUE)) {
            #str  <- crayon::yellow(str)
            type <- crayon::italic(type)
        }
        
        cat(type, "\t", str, "\n", sep = "")
        invisible()
    }
)



## Symbol  =====================================================================

#' @export
Symbol <- function (name) {
    # TODO: check NA and empty character?
    # TODO: should only accept character? Or give a warning when not?
    new("Basic", api_new_symbol(name))
}


if (FALSE) {
    Symbol("a")
    Symbol(NA_character_)
    Symbol("")
    Symbol(42L)
    Symbol(Inf)
}

# vars_init <- function (...) {
#     stop("TODO")
#     args <- dots(...)
#     args
# }
# 
# tibble::lst
# tibble:::lst_quos
# 
# dots <- function (...) {
#     eval(substitute(alist(...)))
# }


## Constant  ===================================================================

#' @export
Constant <- function (
    which = c(
        "zero",
        "one",
        "minus_one",
        "I",
        "pi",
        "E",
        "EulerGamma",
        "Catalan",
        "GoldenRatio",
        "Inf",
        "NegInf",
        "ComplexInf",
        "Nan"
    ),
    mk = NULL
)
{
    if (missing(which)) {
        if (is.null(mk))
            stop("Missing both arguments ", sQuote("which"), " and ", sQuote("mk"))
        return(new("Basic", api_make_const(mk)))
    }
    
    which <- match.arg(which)
    if (!is.null(mk))
        warning("Ignoring argument ", sQuote("mk"))
    
    ptr <- api_builtin_const(which = which)
    new("Basic", ptr)
}

## Integer  ====================================================================

#' @export
Integer <- function (x) {
    # TODO: should also support bigz (from gmp package), etc.
    if (is.integer(x))
        return(new("Basic", api_integer_from_int(x)))
    if (is.numeric(x))
        # Since not all double value can be coerced to integer,
        # we do not use `as.integer` here, but use `as.character(trunc(x))`
        return(new("Basic", api_integer_from_str(as.character(trunc(x)))))
    if (is.character(x))
        return(new("Basic", api_integer_from_str(x)))
    
    stop(sQuote(class(x)), " class is not supported")
}

if (FALSE) {
    (i <- Integer(as.character(- 2^31)))     # -base::.Machine$integer.max - 1L
    str(api_integer_get_int(i))
    
    (i <- Integer(as.character(- 2^31 + 1))) # -base::.Machine$integer.max
    str(api_integer_get_int(i))
    
    (i <- Integer(NA_integer_))
    str(api_integer_get_int(i))
    
    (i <- Integer(Inf))
    str(api_integer_get_int(i))
    
    (i <- Integer(NaN))
    str(api_integer_get_int(i))
}

## RealDouble  =================================================================

#' @export
RealDouble <- function (x) {
    # TODO: how to deal with NA (of character, logical), Inf, NaN?
    if (is.integer(x))
        return(new("Basic", api_realdouble_from_d(as.double(x))))
    if (is.double(x))
        return(new("Basic", api_realdouble_from_d(x)))
    
    stop(sQuote(class(x)), " class is not supported")
}

if (FALSE) {
    (d <- RealDouble(NA_integer_))
    str(api_realdouble_get_d(d))
    
    (d <- RealDouble(NaN))
    str(api_realdouble_get_d(d))
    
    (d <- RealDouble(Inf))
    str(api_realdouble_get_d(d))
    
    (d <- RealDouble(-Inf))
    str(api_realdouble_get_d(d))
}

## S  ==========================================================================

#' @export
setGeneric("S", def = function (x, ...) standardGeneric("S"))

#' @export
setMethod("S", c(x = "character"),
    function (x) {
        # TODO
        ptr <- api_parse_str(x)
        new("Basic", ptr)
    }
)

#' @export
setMethod("S", c(x = "integer"),
    function (x) {
        Integer(x)
    }
)


