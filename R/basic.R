
#' @include classes.R
NULL

## SymEnginePTR: Parent class holding Basic, VecBasic, etc. ====================

setAs(from = "SymEnginePTR", to = "externalptr",
    function(from) from@ptr
)

## Get tag of an external ptr
ptr_tag <- function (x) {
    .Call("R_ExternalPtrTag", as(x, "externalptr"))
}

## Basic  ======================================================================

#' @export
S <- function(x) {
    ## TODO: Handle formula
    s4basic_parse(x, check_whole_number = TRUE)
}

if (FALSE) {
    S(~ {{ log(42) + 1 }})
    S(~ .(log(42) + 1))
}


## TODO: add test case S(~ .(~x))
## Callback provided for C++
s4basic_parse_formula <- function(x) {
    stopifnot(length(x) == 2)  # i.e. only has the right hand side
    rhs <- x[[2]]
    env <- environment(x)
    parse_expr <- function(expr) {
        if (is.symbol(expr))
            return(Symbol(deparse(expr)))
        if (is.integer(expr))
            return(S(expr))
        if (is.double(expr))
            return(S(expr)) ## Will convert whole number to integer
        if (is.character(expr))
            return(S(expr)) ## Not sure we are supposed to parse expressions
        if (is.call(expr)) {
            ## This implements `{{ expr }}` style backquoting.
            if (expr[[1]] == as.name("{") && length(expr) == 2L
                                          && expr[[2]][[1]] == as.name("{")) {
                inner_exprs <- as.list(expr[[2]][-1])
                for (e in inner_exprs)
                    ## Evaluate in the environment where the formula is created
                    last <- eval(e, envir = env)
                ## Convert to basic
                return(s4basic_parse(last, check_whole_number = FALSE))
            }
            ## This implements `.(expr)` style backquoting.
            if (expr[[1]] == as.name(".")) {
                inner_expr <- expr[[2]]
                value <- eval(inner_expr, envir = env)
                return(s4basic_parse(value, check_whole_number = FALSE))
            }
            
            ## i.e. cases like f()(y)
            # I am not sure whether this should be allowed. (unless expr[[1]] is backquoted?)
            # Also note that bquote does not handle bquote(.(func)(x))
            if (!is.symbol(expr[[1]]))
                warning("TODO")
            ## TODO: add more supported functions
            else if (deparse(expr[[1]]) %in% c("+", "-", "*", "/", "^", "(")) {
                ## TODO: check -- arguments should not be named??
                args <- lapply(expr[-1], parse_expr)
                func <- get(deparse(expr[[1]]), mode = "function")
                return(do.call(func, args))
            }
        }
        if (is(expr, "Basic"))
            return(expr)
            
        stop(sprintf("Unable to parse %s", deparse(expr)))
    }
    parse_expr(rhs)
}

## Default method for converting R object to Basic
setAs(from = "ANY", to = "Basic",
      function(from) s4basic_parse(from, check_whole_number = FALSE)
)

setMethod("as.character", c(x = "Basic"),
    function(x) s4basic_str(x)
)

setGeneric("as.double")

setMethod("as.double", c(x = "Basic"),
    function(x) {
        as.double(s4basic_as_sexp(x))
    }
)

setMethod("as.integer", c(x = "Basic"),
    function(x) {
        if (s4basic_get_type(x) == "Integer")
            return(s4basic_as_sexp(x))
        stop("Not implemented")
    }
)

setMethod("as.character", c(x = "Basic"), function(x) s4basic_str(x))

setMethod("==", c(e1 = "Basic", e2 = "Basic"),
    function(e1, e2) s4basic_eq(e1, e2)
)

setMethod("!=", c(e1 = "Basic", e2 = "Basic"),
    function(e1, e2) s4basic_neq(e1, e2)
)

if (FALSE) {
    S("x == x") == S("y == y")
    S("x == x") != S("y == y")
    S("x + 1 > x") == S("x + 2 > x + 1")
}

#' @export
Hash <- function (x) {
    ## TODO: hash for VecBasic and DenseMatrix
    s4basic_hash(x)
}


## Some special constructors  ==================================================

## TODO: add test case:
##    Symbol(NA_character), Symbol(""), Symbol(character()), Symbol(c("x", "y")), Symbol(42)
#' @export
Symbol <- function(name) {
    # TODO: check NA and empty character?
    # TODO: should only accept character? Or give a warning when not?
    s4basic_symbol(name)
}

#' @export
Constant <- function (name) {
    s4basic_const(name)
}

#' @export
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

