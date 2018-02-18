
#' @include RcppExports.R
NULL

setClass("Basic", contains = "externalptr")

## Show Methods ================================================================

setMethod("show", "Basic",
    function (object) {
        ptr <- as(object, "externalptr")
        
        type <- sprintf("(%s)", api_basic_type(ptr))
        str  <- api_basic_str_julia(ptr)
        
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
    new("Basic", .Symbol(name))
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
Constant <- function (name) {
    new("Basic", api_make_const(name)) 
}

## Integer  ====================================================================

#' @export
Integer <- function (x) {
    # TODO: should also support bigz (from gmp package), etc.
    if (is.integer(x))
        return(new("Basic", api_integer_from_int(x)))
    if (is.double(x))
        # Not all double value can be coerced to integer (i.e. int type), thus I use character.
        # This is a hack to generate the string representation of the integer part in case
        # the number is large. (e.g. `as.character(2^99)` or `format(2^99, digits=22)` won't work)
        # Any better way?
        #return(new("Basic", api_integer_from_str(as.character(trunc(x)))))
        return(new("Basic", api_integer_from_str(as.character(gmp::as.bigz(x)))))
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
        # TODO: additional arguments to specify the type
        new("Basic", api_parse_str(x))
    }
)

#' @export
setMethod("S", c(x = "integer"),
    function (x) {
        Integer(x)
    }
)

#' @export
setMethod("S", c(x = "numeric"),
    function (x) {
        RealDouble(x)
    }
)

#' @export
setMethod("S", c(x = "Basic"),
    function (x) x
)

#' @export
setMethod("S", c(x = "externalptr"),
    function (x) {
        stopifnot(identical(api_ptr_tag(x), "basic_struct*"))
        new("Basic", x)
    }
)

#' @export
setMethod("S", c(x = "formula"),
    function (x) {
        stopifnot(length(x) == 2)  # TODO
        name <- x[[2]]
        stopifnot(is.symbol(name)) # TODO
        Symbol(deparse(name))
    }
)

## Hash and Eq  ================================================================

#' @export
Hash <- function (x) {
    api_basic_hash(x)
}

#' @export
Eq <- function (a, b) {
    api_basic_eq(a, b)
}

#' @export
Neq <- function (a, b) {
    api_basic_neq(a, b)
}




