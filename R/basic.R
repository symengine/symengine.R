
#' @include classes.R
NULL

## Basic  ======================================================================

#' Converting R object to Basic
#' 
#' `S` converts a R object to a Basic object. `Symbol`, `Real` and `Constant`
#' construct a Basic object with type "Symbol", "RealDouble"/"RealMPFR"
#' and "Constant", respectively.
#' 
#' For double vector, `S` will check whether it is a whole number -- if true,
#' it will be converted to a Integer type. If this behavior is not desired,
#' you can use `as(x, "Basic")`.
#' 
#' @param x A R object.
#' 
#' @rdname S
#' @export
#' @examples
#' S("(x + y)^2")
#' S(~ (x + y)^2)
#' S(NaN)
#' S(42)
#' as(42, "Basic")
S <- function(x) {
    s4basic_parse(x, check_whole_number = TRUE)
}

if (FALSE) {
    S(~ {{ log(42) + 1 }})
    S(~ .(log(42) + 1))
}


## Some special constructors  ==================================================

## TODO: add test case:
##    Symbol(NA_character), Symbol(""), Symbol(character()), Symbol(c("x", "y")), Symbol(42)
#' @rdname S
#' @export
Symbol <- function(x) {
    # TODO: check NA and empty character?
    # TODO: should only accept character? Or give a warning when not?
    s4basic_symbol(x)
}

#' @rdname S
#' @export
#' @examples 
#' pi <- Constant("pi")
#' evalf(pi)
#' if (symengine_have_component("mpfr"))
#'     evalf(pi, 300)
Constant <- function (x) {
    s4basic_const(x)
}

#' @param prec If supplied, the argument will be parsed as a Basic object of type 
#' RealMPFR.
#' @rdname S
#' @export
#' @examples 
#' Real(42)
#' if (symengine_have_component("mpfr"))
#'     Real(42, prec = 140)
Real <- function(x, prec = NULL) {
    ## TODO: accept vectors
    s4basic_real(x, prec)
}

if (FALSE) {
    (d <- Real(NA_integer_))
    str(as.double(d))
    
    (d <- Real(NaN))
    str(as.double(d))
    
    (d <- Real(Inf))
    str(as.double(d))
    
    (d <- Real(-Inf))
    str(as.double(d))
}

## There is no constructor for Integer, using the parser should be enough

if (FALSE) {
    (i <- S(as.character(- 2^31)))     # -base::.Machine$integer.max - 1L
    str(as.integer(i))
    
    (i <- S(as.character(- 2^31 + 1))) # -base::.Machine$integer.max
    str(as.integer(i))
    
    (i <- S(NA_integer_))
    str(as.integer(i))
    
    (i <- S(Inf))
    str(as.integer(i))
    
    (i <- S(NaN))
    str(as.integer(i))
}

# #' @export
# Integer <- function (x) {
#     if (is.na(x) || is.infinite(x) || is.nan(x))
#         stop("NA, Inf, NaN can not be converted to Integer")
#     
#     # TODO: should also support bigz (from gmp package), etc.
#     if (is.integer(x))
#         return(basic_integer_fromint(x))
#     if (is.double(x))
#         # Not all double value can be coerced to integer (i.e. int type), thus I use character.
#         # This is a hack to generate the string representation of the integer part in case
#         # the number is large. (e.g. `as.character(2^99)` or `format(2^99, digits=22)` won't work)
#         # Any better way?
#         #return(new("Basic", ptr = basic_integer_fromstr(as.character(trunc(x)))))
#         return(basic_integer_fromstr(as.character(gmp::as.bigz(x))))
#     if (is.character(x))
#         return(basic_integer_fromstr(x))
#     
#     stop(sQuote(class(x)), " class is not supported")
# }


## Show Methods ================================================================

setMethod("show", "Basic",
    function(object) {
        notation <- s4basic_get_type(object)
        if (notation == "RealMPFR")
            notation <- paste0(notation, ",", "prec", s4basic_realmpfr_get_prec(object))
        notation <- sprintf("(%s)", notation)
        str  <- as.character(object)
        if (requireNamespace("crayon", quietly = TRUE)) {
            #str  <- crayon::yellow(str)
            notation <- crayon::italic(notation)
        }
        cat(notation, "\t", str, "\n", sep = "")
        invisible()
    }
)




# ## ActiveBindings  =============================================================
# 
# # TODO...
# 
# # The error is:
# #     ** preparing package for lazy loading
# #     Error in .Call("c_builtin_const", id) : 
# #         "c_builtin_const" not resolved from current namespace (symengine)
# #     ERROR: lazy loading failed for package ‘symengine’
# #
# # Not sure why...
# 
# if (FALSE) {
#     
# Consts <- new.env()
# 
# makeActiveBinding("zero"       , function() Constant("zero"        ), Consts)
# makeActiveBinding("one"        , function() Constant("one"         ), Consts)
# makeActiveBinding("minus_one"  , function() Constant("minus_one"   ), Consts)
# makeActiveBinding("I"          , function() Constant("I"           ), Consts)
# makeActiveBinding("pi"         , function() Constant("pi"          ), Consts)
# makeActiveBinding("E"          , function() Constant("E"           ), Consts)
# makeActiveBinding("EulerGamma" , function() Constant("EulerGamma"  ), Consts)
# makeActiveBinding("Catalan"    , function() Constant("Catalan"     ), Consts)
# makeActiveBinding("GoldenRatio", function() Constant("GoldenRatio" ), Consts)
# makeActiveBinding("Inf"        , function() Constant("Inf"         ), Consts)
# makeActiveBinding("NegInf"     , function() Constant("NegInf"      ), Consts)
# makeActiveBinding("ComplexInf" , function() Constant("ComplexInf"  ), Consts)
# makeActiveBinding("Nan"        , function() Constant("Nan"         ), Consts)
# 
# }


