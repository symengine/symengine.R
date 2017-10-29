

setClass("Basic", contains = "externalptr")


## Misc  =======================================================================


## Variable Init  ==============================================================

#' @export
vars <- function (name) {
    # TODO: check NA and empty character?
    ptr <- api_new_symbol(name)
    new("Basic", ptr)
}

if (FALSE) {
    vars("a")
    vars(NA_character_)
    vars("")
    vars(42L)
    vars(Inf)
}

#' @export
vars_init <- function (...) {
    stop("TODO")
    args <- dots(...)
    args
}

tibble::lst
tibble:::lst_quos

dots <- function (...) {
    eval(substitute(alist(...)))
}


## Get Constants  ==============================================================

#' @export
get_builtin_const <- function (
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
    )
)
{
    which <- match.arg(which)
    ptr <- api_builtin_const(which)
    new("Basic", ptr)
}

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

