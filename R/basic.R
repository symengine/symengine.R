

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


## Trigonometry functions  =====================================================

#' @export
Trigonometry <- (function() {
    flist <- alist(sin, cos, tan, asin, acos, atan, csc, sec, cot, acsc, asec, acot,
                   sinh, cosh, tanh, asinh, acosh, atanh, csch, sech, coth, acsch, asech, acoth)
    ans <- vector("list", length(flist))
    for (i in seq_along(ans)) {
        ans[[i]] <- eval(envir = parent.frame(),
            bquote(function (x) {
                new("Basic", .(as.name(paste0("api_basic_", deparse(flist[[i]]))))(S(x)))
            }
        ))
        names(ans)[i] <- deparse(flist[[i]])
    }
    class(ans) <- "symengine.trigonometry" # Or?
    ans
    # ans <- as.environment(ans)
    # mkExportedEnv <- function(e) {
    #     stopifnot(is.environment(e))
    #     lockEnvironment(e)
    #     # for (binding in ls(e))
    #     #     lockBinding(binding, e)
    #     class(e) <- "exported.env"
    #     e
    # }
    # mkExportedEnv(ans)
})()


#' @export
print.symengine.trigonometry <- function(x, ...) {
    if (length(list(...)))
        warning("Extra arguments are ignored.")
    if (digest::digest(x) != digest::digest(Trigonometry))
        warning("Contents have been modified.")
    cat("Members:\n")
    a <- matrix(names(x), nrow = 3)
    for (i in seq(nrow(a))) {
        cat("  ")
        out <- format(a[i, ])
        if (requireNamespace("crayon", quietly = TRUE))
            out <- crayon::italic(out)
        cat(out)
        cat("\n")
    }
}

setMethod("sin",  c(x = "Basic"), Trigonometry$sin )
setMethod("cos",  c(x = "Basic"), Trigonometry$cos )
setMethod("tan",  c(x = "Basic"), Trigonometry$tan )
setMethod("acos", c(x = "Basic"), Trigonometry$acos)
setMethod("asin", c(x = "Basic"), Trigonometry$asin)
setMethod("atan", c(x = "Basic"), Trigonometry$atan)

setMethod("sinpi", c(x = "Basic"), function(x) sin(x * Constant("pi")))
setMethod("cospi", c(x = "Basic"), function(x) cos(x * Constant("pi")))
setMethod("tanpi", c(x = "Basic"), function(x) tan(x * Constant("pi")))
