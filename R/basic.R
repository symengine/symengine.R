
#' @include RcppExports.R
NULL

setClass("Basic", contains = "externalptr")

#' @export
setGeneric("S", def = function (x, ...) standardGeneric("S"))

## Get tag of an external ptr ==================================================

ptr_tag <- function (x) {
    .Call("R_ExternalPtrTag", as(x, "externalptr"))
}

## Show Methods ================================================================

setMethod("show", "Basic",
    function (object) {
        ptr <- as(object, "externalptr")
        
        type <- sprintf("(%s)", basic_type(ptr))
        str  <- basic_str(ptr)
        
        if (requireNamespace("crayon", quietly = TRUE)) {
            #str  <- crayon::yellow(str)
            type <- crayon::italic(type)
        }
        
        cat(type, "\t", str, "\n", sep = "")
        invisible()
    }
)

## Accessors  ==================================================================

#' @export
basic_type <- function (x) {
    .basic_type(as(x, "externalptr"))
}

#' @export
basic_str <- function (x) {
    .basic_str(as(x, "externalptr"))
}

setMethod("as.character", c(x = "Basic"), basic_str)

#' @export
basic_get_args <- function(x) {
    .basic_get_args(x)
}

#' @export
basic_free_symbols <- function(x) {
    .basic_free_symbols(x)
}

#' @export
basic_function_symbols <- function(x) {
    .basic_function_symbols(x)
}


## Hash and Eq  ================================================================

#' @export
basic_hash <- function (x) {
    .basic_hash(as(x, "externalptr"))
}

#' @export
Hash <- function (x) {
    basic_hash(x)
}

basic_eq <- function (a, b) {
    .basic_eq(a@.xData, b@.xData)
}

basic_neq <- function (a, b) {
    .basic_neq(a@.xData, b@.xData)
}

setMethod("==", c(e1 = "Basic", e2 = "Basic"),
    function(e1, e2) basic_eq(e1, e2)
)

setMethod("!=", c(e1 = "Basic", e2 = "Basic"),
    function(e1, e2) basic_neq(e1, e2)
)

if (FALSE) {
    S("x == x") == S("y == y")
    S("x == x") != S("y == y")
    S("x + 1 > x") == S("x + 2 > x + 1")
}


## Basic: is_a_XXX  ============================================================

basic_isNumber         <- function(x) .basic_isNumber(x@.xData)
basic_isInteger        <- function(x) .basic_isInteger(x@.xData)
basic_isRational       <- function(x) .basic_isRational(x@.xData)
basic_isSymbol         <- function(x) .basic_isSymbol(x@.xData)
basic_isComplex        <- function(x) .basic_isComplex(x@.xData)
basic_isRealDouble     <- function(x) .basic_isRealDouble(x@.xData)
basic_isComplexDouble  <- function(x) .basic_isComplexDouble(x@.xData)
basic_isRealMPFR       <- function(x) .basic_isRealMPFR(x@.xData)
basic_isComplexMPC     <- function(x) .basic_isComplexMPC(x@.xData)

basic_num_iszero       <- function(x) .basic_num_iszero(x@.xData)
basic_num_isnegative   <- function(x) .basic_num_isnegative(x@.xData)
basic_num_ispositive   <- function(x) .basic_num_ispositive(x@.xData)
basic_num_iscomplex    <- function(x) .basic_num_iscomplex(x@.xData)







## Symbol  =====================================================================

#' @export
basic_symbol <- function (name) {
    # TODO: check NA and empty character?
    # TODO: should only accept character? Or give a warning when not?
    new("Basic", .basic_symbol(name))
}

#' @export
Symbol <- basic_symbol


## Parser  =====================================================================

#' @export
basic_parse <- function (x) {
    new("Basic", .basic_parse(x))
}

setMethod("S", c(x = "character"),
    # TODO: additional arguments to specify the type
    function (x) basic_parse(x)
)


## Constant  ===================================================================

#' @export
basic_const <- function (name) {
    new("Basic", .basic_const(name))
}

#' @export
Constant <- function (x) {
    basic_const(x)
}

.basic_const_zero
.basic_const_one
.basic_const_minus_one

basic_I                 <- function () new("Basic", .basic_const_I())
basic_pi                <- function () new("Basic", .basic_const_pi())
basic_E                 <- function () new("Basic", .basic_const_E())
basic_EulerGamma        <- function () new("Basic", .basic_const_EulerGamma())
basic_Catalan           <- function () new("Basic", .basic_const_Catalan())
basic_GoldenRatio       <- function () new("Basic", .basic_const_GoldenRatio())
basic_infinity          <- function () new("Basic", .basic_const_infinity())
basic_neginfinity       <- function () new("Basic", .basic_const_neginfinity())
basic_complex_infinity  <- function () new("Basic", .basic_const_complex_infinity())
basic_nan               <- function () new("Basic", .basic_const_nan())


## Integer  ====================================================================

basic_integer_fromint <- function (x) {
    new("Basic", .basic_integer_fromint(x))
}
basic_integer_fromstr <- function (x) {
    new("Basic", .basic_integer_fromstr(x))
}

#' @export
Integer <- function (x) {
    if (is.na(x) || is.infinite(x) || is.nan(x))
        stop("NA, Inf, NaN can not be converted to Integer")
    
    # TODO: should also support bigz (from gmp package), etc.
    if (is.integer(x))
        return(new("Basic", basic_integer_fromint(x)))
    if (is.double(x))
        # Not all double value can be coerced to integer (i.e. int type), thus I use character.
        # This is a hack to generate the string representation of the integer part in case
        # the number is large. (e.g. `as.character(2^99)` or `format(2^99, digits=22)` won't work)
        # Any better way?
        #return(new("Basic",  basic_integer_fromstr(as.character(trunc(x)))))
        return(new("Basic", basic_integer_fromstr(as.character(gmp::as.bigz(x)))))
    if (is.character(x))
        return(new("Basic", basic_integer_fromstr(x)))
    
    stop(sQuote(class(x)), " class is not supported")
}

basic_integer_getint <- function (x) {
    .basic_integer_getint(as(x, "externalptr"))
}

setMethod("as.integer", c(x = "Basic"),
    function (x) {
        if (basic_isInteger(x))
            return(basic_integer_getint(x))
        stop("Not implemented")
    }
)

if (FALSE) {
    (i <- Integer(as.character(- 2^31)))     # -base::.Machine$integer.max - 1L
    str(as.integer(i))
    
    (i <- Integer(as.character(- 2^31 + 1))) # -base::.Machine$integer.max
    str(as.integer(i))
    
    (i <- Integer(NA_integer_))
    str(as.integer(i))
    
    (i <- Integer(Inf))
    str(as.integer(i))
    
    (i <- Integer(NaN))
    str(as.integer(i))
}

## RealDouble  =================================================================

basic_realdouble <- function (x) {
    new("Basic", .basic_realdouble(x))
}

basic_realdouble_getd <- function (ptr) {
    .basic_realdouble_getd(as(ptr, "externalptr"))
}


#' @export
RealDouble <- function (x) {
    # integer or double
    if (is.numeric(x))
        return(basic_realdouble(x))
    
    stop(sQuote(class(x)), " class is not supported")
}

setMethod("as.double", c(x = "Basic"),
    function (x) {
        if (basic_isRealDouble(x))
            return(basic_realdouble_getd(x))
        stop("Not implemented")
    }
)

if (FALSE) {
    (d <- RealDouble(NA_integer_))
    str(as.double(d))
    
    (d <- RealDouble(NaN))
    str(as.double(d))
    
    (d <- RealDouble(Inf))
    str(as.double(d))
    
    (d <- RealDouble(-Inf))
    str(as.double(d))
}








## S  ==========================================================================


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
        stopifnot(identical(ptr_tag(x), "basic_struct*"))
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


## ActiveBindings  =============================================================

# TODO...

# The error is:
#     ** preparing package for lazy loading
#     Error in .Call("c_builtin_const", id) : 
#         "c_builtin_const" not resolved from current namespace (symengine)
#     ERROR: lazy loading failed for package ‘symengine’
#
# Not sure why...

if (FALSE) {
    
Consts <- new.env()

makeActiveBinding("zero"       , function() Constant("zero"        ), Consts)
makeActiveBinding("one"        , function() Constant("one"         ), Consts)
makeActiveBinding("minus_one"  , function() Constant("minus_one"   ), Consts)
makeActiveBinding("I"          , function() Constant("I"           ), Consts)
makeActiveBinding("pi"         , function() Constant("pi"          ), Consts)
makeActiveBinding("E"          , function() Constant("E"           ), Consts)
makeActiveBinding("EulerGamma" , function() Constant("EulerGamma"  ), Consts)
makeActiveBinding("Catalan"    , function() Constant("Catalan"     ), Consts)
makeActiveBinding("GoldenRatio", function() Constant("GoldenRatio" ), Consts)
makeActiveBinding("Inf"        , function() Constant("Inf"         ), Consts)
makeActiveBinding("NegInf"     , function() Constant("NegInf"      ), Consts)
makeActiveBinding("ComplexInf" , function() Constant("ComplexInf"  ), Consts)
makeActiveBinding("Nan"        , function() Constant("Nan"         ), Consts)

}
