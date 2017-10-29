

setClass("Basic", contains = "externalptr")


## Misc  =======================================================================



## Symbol  =====================================================================

#' @export
Symbol <- function (name) {
    # TODO: check NA and empty character?
    # TODO: should only accept character? Or give a warning when not?
    new("Basic", api_new_symbol(name))
}

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



## Parse From String  ==========================================================

#' @export
setGeneric("S", def = function (x) standardGeneric("S"))

#' @export
setMethod("S", c(x = "character"),
    function (x) {
        # TODO
        ptr <- api_parse_str(x)
        new("Basic", ptr)
    }
)


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

