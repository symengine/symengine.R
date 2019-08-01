
#' @include classes.R
NULL

## TODO: provide `==` and `!=` methods for VecBasic and DenseMatrix

#' Bindings for Operators and Math Functions
#' 
#' @param e1,e2,x,y,base Objects.
#' 
#' @rdname bindings
setMethod("==", c(e1 = "Basic", e2 = "Basic"),
    function(e1, e2) s4basic_eq(e1, e2)
)

#' @rdname bindings
setMethod("!=", c(e1 = "Basic", e2 = "Basic"),
    function(e1, e2) s4basic_neq(e1, e2)
)

if (FALSE) {
    S("x == x") == S("y == y")
    S("x == x") != S("y == y")
    S("x + 1 > x") == S("x + 2 > x + 1")
}

#' @rdname bindings
setMethod(Arith, c(e1 = "SymEngineDataType", e2 = "SymEngineDataType"),
    function(e1, e2)
        s4binding_op(e1, e2, .Generic)
)
## Note that array will be in fact converted to vector
#' @rdname bindings
setMethod(Arith, c(e1 = "SymEngineDataType", e2 = "vector"),
    function(e1, e2)
        s4binding_op(e1, e2, .Generic)
)
#' @rdname bindings
setMethod(Arith, c(e1 = "vector", e2 = "SymEngineDataType"),
    function(e1, e2)
        s4binding_op(e1, e2, .Generic)
)

#' @rdname bindings
setMethod("-", c(e1 = "SymEngineDataType", e2 = "missing"),
    function(e1, e2) s4binding_math(e1, "neg")
)

#' @rdname bindings
setMethod("+", c(e1 = "SymEngineDataType", e2 = "missing"),
    function(e1, e2) e1
)

## Matrix multiplication
#' @rdname bindings
setMethod("%*%", c(x = "DenseMatrix", y = "DenseMatrix"),
    function(x, y) s4DenseMat_mul_matrix(x, y)
)
#' @rdname bindings
setMethod("%*%", c(x = "VecBasic", y = "VecBasic"),
    function(x, y) s4DenseMat_mul_matrix(Matrix(x, nrow = 1L), Matrix(y, ncol = 1L))
)
#' @rdname bindings
setMethod("%*%", c(x = "DenseMatrix", y = "VecBasic"),
    function(x, y) {
        x_ncol <- ncol(x)
        if (x_ncol == length(y))
            y <- Matrix(y, nrow = x_ncol, ncol = 1L)
        else if (x_ncol == 1L)
            y <- Matrix(y, nrow = 1L)
        else
            stop("Can not make arguments comformable")
        s4DenseMat_mul_matrix(x, y)
    }
)
#' @rdname bindings
setMethod("%*%", c(x = "DenseMatrix", y = "vector"),
    function(x, y) x %*% Vector(y)
)
#' @rdname bindings
setMethod("%*%", c(x = "VecBasic", y = "DenseMatrix"),
    function(x, y) {
        y_nrow <- nrow(y)
        if (y_nrow == length(x))
            x <- Matrix(x, nrow = 1L, ncol = y_nrow)
        else if (y_nrow == 1L)
            x <- Matrix(x, ncol = 1L)
        else
            stop("Can not make arguments comformable")
        s4DenseMat_mul_matrix(x, y)
    }
)
#' @rdname bindings
setMethod("%*%", c(x = "vector", y = "DenseMatrix"),
    function(x, y) Vector(x) %*% y
)

#' @rdname bindings
setMethod("Math", c(x = "SymEngineDataType"),
    function(x) s4binding_math(x, .Generic)
)

#' @rdname bindings
setMethod("sinpi", c(x = "SymEngineDataType"),
    function(x) s4binding_math(s4binding_op(x, Constant("pi"), "*"), "sin")
)
#' @rdname bindings
setMethod("cospi", c(x = "SymEngineDataType"),
    function(x) s4binding_math(s4binding_op(x, Constant("pi"), "*"), "cos")
)
#' @rdname bindings
setMethod("tanpi", c(x = "SymEngineDataType"),
    function(x) s4binding_math(s4binding_op(x, Constant("pi"), "*"), "tan")
)
#' @rdname bindings
setMethod("log", c(x = "SymEngineDataType"),
    function(x, base) {
        if (missing(base))
            return(s4binding_math(x, "log"))
        s4binding_math(x, "log")/s4binding_math(base, "log")
    }
)


#' Expand a SymEngine Expression
#' 
#' @param x A SymEngine object.
#' @export
expand <- function(x) {
    s4binding_math(x, "expand")
}

#' Some Special Math Functions
#' 
#' Some special mathematical functions and functions related to number theory.
#' 
#' @param a,b,x,n,k SymEngine objects. Some functions require Integer type.
#' @rdname mathfuns
#' @export
LCM <- function(a, b) {
    ## TODO: Check type with (s4basic_get_type(a) != "Integer")?
    ##       But also need to consider VecBasic and DenseMatrix, maybe do it at C level.
    if (is.double(a)) a <- as.integer(a)
    if (is.double(b)) b <- as.integer(b)
    s4binding_op(a, b, "lcm")
}

#' @rdname mathfuns
#' @export
GCD <- function(a, b) {
    ## TODO: check type
    if (is.double(a)) a <- as.integer(a)
    if (is.double(b)) b <- as.integer(b)
    s4binding_op(a, b, "gcd")
}

#' @rdname mathfuns
#' @export
nextprime <- function(a) {
    ## TODO: check type
    if (is.double(a)) a <- as.integer(a)
    s4binding_math(a, "nextprime")
}

#' @rdname mathfuns
#' @exportMethod factorial
setGeneric("factorial")

#' @rdname mathfuns
setMethod("factorial", c(x = "SymEngineDataType"),
    function(x) s4binding_math(x, "factorial")
)

#' @rdname mathfuns
#' @exportMethod choose
setGeneric("choose")

#' @rdname mathfuns
setMethod("choose", c(n = "SymEngineDataType"),
    function(n, k) {
        if (is.double(k))
            k <- as.integer(k)
        s4binding_op(n, k, "binomial")
    }
)

#' @rdname mathfuns
#' @export
zeta <- function(a) {
    s4binding_math(a, "zeta")
}

#' @rdname mathfuns
#' @export
lambertw <- function(a) {
    s4binding_math(a, "lambertw")
}

#' @rdname mathfuns
#' @export
dirichlet_eta <- function(a) {
    s4binding_math(a, "dirichlet_eta")
}

#' @rdname mathfuns
#' @export
erf <- function(a) {
    s4binding_math(a, "erf")
}

#' @rdname mathfuns
#' @export
erfc <- function(a) {
    s4binding_math(a, "erfc")
}

#' @exportMethod D
setGeneric("D")

#' Derivatives of Basic Objects
#' 
#' @param expr A Basic object.
#' @param name A character vector or a Basic object of type Symbol.
#' @export
setMethod("D", c(expr = "SymEngineDataType"),
    function(expr, name) {
        if (missing(name)) {
            expr <- s4binding_parse(expr)
            if (!s4basic_check(expr))
                stop("'expr' should be convertible to Basic if 'name' is missing")
            free_symbols <- s4basic_free_symbols(expr)
            if (length(free_symbols) != 1L)
                stop("There is more than one variable in the expression, ",
                     "'name' argument must be supplied")
            name <- as(free_symbols, "Basic")
        }
        else if (length(name) == 1L || is.language(name)) {
            ## Avoid parser parsing name as a constant.
            name <- s4basic_symbol(name)
        }
        else if (!s4vecbasic_check(name)) {
            name <- lapply(name, s4basic_symbol)
        }
        ## TODO: there is a shortcut if expr is a DenseMatrix 
        expr <- s4binding_op(expr, name, "diff")
        expr
    }
)

## #' @export
## # usage: diff(expr, x), diff(expr, x, y), diff(expr, x, y, 3)
## diff <- function (expr, ...) {
##     v    <- to_vecbasic(expr)
##     args <- list(...)
##     i    <- 1
##     while (i <= length(args)) {
##         x <- args[[i]]
##         y <- 1
##         if (class(x) != "Basic")
##             stop("Invalid value type")
##         if (i + 1 <= length(args) && is.numeric(args[[i + 1]])) {
##             y <- as.integer(args[[i + 1]])
##             i <- i + 1
##         }
##         while (y > 0) {
##             v <- .vecbasic_diff(v, to_vecbasic(x))
##             y <- y - 1
##         }
##         i <- i + 1
##     }
##     return(get_final_output(expr, v))
## }

#' Substitute Expressions in SymEngine Objects
#' 
#' @param expr A SymEngine object.
#' @param ... Pairs of Basic objects, e.g. (from1, to1, from2, to2).
#' 
#' @export
subs <- function(expr, ...) {
    ## Usage:
    ## 1. If dot arguments are named, substitute the name (as Symbol) to argument
    ## 2. if dot arguments are not named, subs(expr, a,b,c,d) will do 'a -> b, 'c -> d
    ## TODO: wrap basic_subs and maybe implement a new function 'msubs
    options <- list(...)
    if (!is.null(names(options))) {
        if (any(names(options) == ""))
            stop("Extra arguments must be either all named or non named")
        pair_a <- lapply(names(options), Symbol)
        pair_b <- unname(options)
        for (i in seq_along(pair_a))
            expr <- subs(expr, pair_a[[i]], pair_b[[i]])
        return(expr)
    }
    if (...length() == 2L)
        return(s4basic_subs(expr, ..1, ..2))
    if (...length() %% 2L != 0L)
        stop(sprintf("Number of arguments [%s] must be a multiple of two", ...length()))
    pairs <- split(options, rep(seq(...length() / 2L), each = 2L))
    for (pair in pairs)
        expr <- subs(expr, pair[[1]], pair[[2]])
    expr
}

#' Evaluating a SymEngine Object
#' 
#' @param expr A SymEngine object.
#' @param bits The precision.
#' @param complex Whether or not to be evaluated as a complex number.
#' 
#' @export
evalf <- function(expr, bits = 53L, complex = FALSE) {
    s4binding_evalf(expr, bits, complex)
}

## Trigonometry functions  =====================================================

# #' @export
# Trigonometry <- (function() {
#     pkg_env <- parent.env(environment())
#     df <- function(...) data.frame(..., stringsAsFactors = FALSE)
#     table <- rbind(
#         df(name = "sin"   , base = NA),
#         df(name = "cos"   , base = NA),
#         df(name = "tan"   , base = NA),
#         df(name = "asin"  , base = NA),
#         df(name = "acos"  , base = NA),
#         df(name = "atan"  , base = NA),
#         df(name = "csc"   , base = NA),
#         df(name = "sec"   , base = NA),
#         df(name = "cot"   , base = NA),
#         df(name = "acsc"  , base = NA),
#         df(name = "asec"  , base = NA),
#         df(name = "acot"  , base = NA),
#         df(name = "sinh"  , base = NA),
#         df(name = "cosh"  , base = NA),
#         df(name = "tanh"  , base = NA),
#         df(name = "asinh" , base = NA),
#         df(name = "acosh" , base = NA),
#         df(name = "atanh" , base = NA),
#         df(name = "csch"  , base = NA),
#         df(name = "sech"  , base = NA),
#         df(name = "coth"  , base = NA),
#         df(name = "acsch" , base = NA),
#         df(name = "asech" , base = NA),
#         df(name = "acoth" , base = NA)
#     )
#     table$base <- table$name %in% Math@groupMembers
#     table$func <- lapply(table$name, function(name) {
#         ans <- eval(bquote(function(x) s4binding_math(x, .(name))), envir = pkg_env)
#         attr(ans, "srcref") <- NULL
#         ans
#     })
#     class(table) <- c("TrigonometryFunctionTable", "data.frame")
#     table
# })()
# 
# #' @export
# print.TrigonometryFunctionTable <- function(x, ...) {
#     if (length(list(...)))
#         warning("Extra arguments are ignored.")
#     if (digest::digest(x) != digest::digest(Trigonometry))
#         warning("Contents have been modified.")
#     cat("Members:\n")
#     nms <- x$name
#     nms <- ifelse(x$base, paste0(nms, "*"), nms)
#     a <- matrix(nms, nrow = 3)
#     for (i in seq(nrow(a))) {
#         cat("  ")
#         out <- format(a[i, ])
#         if (requireNamespace("crayon", quietly = TRUE))
#             out <- crayon::italic(out)
#         cat(out)
#         cat("\n")
#     }
# }

