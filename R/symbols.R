

setClass("Basic", contains = "externalptr")


## Misc  =======================================================================

#' @export
get_type <- function (x) {
    ptr <- x@.xData
    api_basic_type(ptr)
}

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
S <- function (string) {
    # TODO
    ptr <- api_parse_str(string)
    new("Basic", ptr)
}


## Show Methods ================================================================

setMethod("show", "Basic",
    function (object) {
        ptr <- as(object, "externalptr")
        
        type <- api_basic_type(ptr)
        str  <- api_basic_str(ptr)
        
        cat(paste0("SymEngine ", type, ":\t"))
        cat(str)
        cat("\n")
        invisible()
    }
)

