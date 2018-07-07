
setClassUnion("BasicType", members = c("Basic", "VecBasic", "DenseMatrix"))

# Get top type object of (x, y) in DenseMatrix > VecBasic > Basic order
top_type <- function(x, y) {
    if (class(x) != "DenseMatrix" && class(y) == "DenseMatrix" ||
        class(x) != "DenseMatrix" && class(x) != "VecBasic" && class(y) == "VecBasic") {
        return(y)
    }
    return(x)
}

# process (x, y) to conformable matrices, return NULL otherwise
process_multiply <- function(x, y) {
    all_is_denseMatrix <- function(x, y) {
        d1 <- dim(x)
        d2 <- dim(y)
        if (d1[[2]] != d2[[1]])
            return(NULL)
        return(list(x=x, y=y))
    }

    one_is_denseMatrix <- function(x, y) {
        if (class(x) == "DenseMatrix") {
            d1 <- dim(x)
            v2 <- to_vecbasic(y)
            # add 1-row dim
            if (d1[[2]] == 1)
                return(list(x=x, y=denseMatrix(v2, 1, length(v2))))
            # add 1-column dim
            if (d1[[2]] == length(v2))
                return(list(x=x, y=denseMatrix(v2, length(v2), 1)))
            return(NULL)
        }
        if (class(y) == "DenseMatrix") {
            d2 <- dim(y)
            v1 <- to_vecbasic(x)
            if (d2[[1]] == length(v1))
                return(list(x=denseMatrix(v1, 1, length(v1)), y=y))
            if (d2[[1]] == 1)
                return(list(x=denseMatrix(v1, length(v1), 1), y=y))
            return(NULL)
        }
    }

    all_is_vecbasic <- function(x, y) {
        v1 <- vecbasic(x)
        v2 <- vecbasic(y)
        if (length(v1) != length(v2))
            return(NULL)
        return(list(x=denseMatrix(v1, 1, length(v1)), y=denseMatrix(v2, length(v2), 1)))
    }

    if (class(x) == "DenseMatrix" && class(y) == "DenseMatrix") {
        return(all_is_denseMatrix(x, y))
        
    } else if (class(x) == "DenseMatrix" || class(y) == "DenseMatrix") {
        return(one_is_denseMatrix(x, y))
    }
    return(all_is_vecbasic(x, y))
}

to_vecbasic <- function(x) {
    if (class(x) == "VecBasic")
        return(x)
    if (class(x) == "DenseMatrix")
        return(denseMatrix_to_vecbasic(x))
    # Basic or ANY
    return(vecbasic(x))
}

setMethods <- function (fs=list(), signatures=list(), definitions=list(),
                        where=topenv(parent.frame()), ...) {
    for (signature in signatures) {
        for (i in 1:length(fs)) {
            setMethod(fs[[i]], signature=signature,
                definition=definitions[[i]], where=where, ...)
        }
    }
}

check_matrix_dimension_same <- function(e1, e2) {
    if (class(e1) == "DenseMatrix" && class(e2) == "DenseMatrix") {
        d1 <- dim(e1)
        d2 <- dim(e2)
        if (any(d1 != d2))
            stop("non-conformable arrays")
    }
}

check_all_is_integer <- function(v) {
    for (i in seq_along(v)) {
        if (!basic_isInteger(v[[i]]))
            stop("element must be integer")
    }
}

get_final_output <- function(e, v) {
    if (class(e) == "DenseMatrix") {
        d <- dim(e)
        return(.denseMatrix(v, d[[1]], d[[2]]))
    }
    if (class(e) == "VecBasic") {
        return(v)
    }
    # Basic or ANY
    return(v[[1]])
}

# Basic operations: +, -, *, /, ^ =============================================
basicOpList = alist(add, sub, mul, div, pow)
basicOpFuncs = vector("list", length(basicOpList))
for (i in seq_along(basicOpFuncs)) {
    basicOpFuncs[[i]] <- eval(envir = environment(),
        bquote(function (e1, e2) {
            check_matrix_dimension_same(e1,e2)
            e  <- top_type(e1, e2)
            v1 <- to_vecbasic(e1)
            v2 <- to_vecbasic(e2)
            l1 <- length(v1)
            l2 <- length(v2)
            if (max(l1,l2) %% min(l1,l2))
                warning("longer object length is not a multiple of shorter object length")
            v  <- .(as.name(paste0(".vecbasic_", deparse(basicOpList[[i]]))))(v1, v2)
            return(get_final_output(e, v))
        }
    ))
    names(basicOpFuncs)[i] <- deparse(basicOpList[[i]])
}

# mod and quotient
setMethods("%%",
    list(c(e1 = "BasicType", e2 = "BasicType"),
         c(e1 = "BasicType", e2 = "ANY"),
         c(e1 = "ANY", e2 = "BasicType")),
    list("func"=
        function(e1, e2) {
            check_matrix_dimension_same(e1,e2)
            e <- top_type(e1, e2)
            v1 <- to_vecbasic(e1)
            check_all_is_integer(v1)
            v2 <- to_vecbasic(e2)
            check_all_is_integer(v2)
            l1 <- length(v1)
            l2 <- length(v2)
            if (max(l1,l2) %% min(l1,l2))
                warning("longer object length is not a multiple of shorter object length")
            v <- .vecbasic_mod_f(v1, v2)
            return(get_final_output(e, v))
        }
    )
)

setMethods("%/%",
    list(c(e1 = "BasicType", e2 = "BasicType"),
         c(e1 = "BasicType", e2 = "ANY"),
         c(e1 = "ANY", e2 = "BasicType")),
    list("func"=
        function(e1, e2) {
            check_matrix_dimension_same(e1,e2)
            e <- top_type(e1, e2)
            v1 <- to_vecbasic(e1)
            check_all_is_integer(v1)
            v2 <- to_vecbasic(e2)
            check_all_is_integer(v2)
            l1 <- length(v1)
            l2 <- length(v2)
            if (max(l1,l2) %% min(l1,l2))
                warning("longer object length is not a multiple of shorter object length")
            v <- .vecbasic_quotient(v1, v2)
            return(get_final_output(e, v))
        }
    )
)

setMethods(list("+", "-", "*", "/", "^"),
    list(c(e1 = "BasicType", e2 = "BasicType"),
         c(e1 = "BasicType", e2 = "ANY"),
         c(e1 = "ANY", e2 = "BasicType")),
    basicOpFuncs
)

setMethod("-", c(e1 = "BasicType", e2 = "missing"),
    function (e1, e2) {
        v1 <- to_vecbasic(e1)
        v  <- .vecbasic_neg(v1)
        return(get_final_output(e1, v))
    }
)

setMethod("+", c(e1 = "BasicType", e2 = "missing"),
    function (e1, e2) {
        e1
    }
)

setMethods("%*%",
    list(c(x = "BasicType", y = "BasicType"),
         c(x = "BasicType", y = "ANY"),
         c(x = "ANY", y = "BasicType")),
    list("func"=
        function(x, y) {
            r <- process_multiply(x, y)
            if (is.null(r))
                stop("non-conformable arguments")
            .dense_matrix_mul_matrix(r$x, r$y)
        }
    )
)

# One arg function ============================================================
#' @exportMethod abs erf erfc sin cos tan asin acos atan csc sec
#' @exportMethod cot acsc asec acot sinh cosh tanh asinh acosh
#' @exportMethod atanh csch sech coth acsch asech acoth lambertw
#' @exportMethod zeta dirichlet_eta gamma sqrt exp log
OneArgOpList = alist(abs, erf, erfc, sin, cos, tan, asin, acos, atan, csc, sec,
                     cot, acsc, asec, acot, sinh, cosh, tanh, asinh, acosh,
                     atanh, csch, sech, coth, acsch, asech, acoth, lambertw,
                     zeta, dirichlet_eta, gamma, sqrt, exp, log)

for (i in seq_along(OneArgOpList)) {
    name <- deparse(OneArgOpList[[i]])
    if (is.null(getGeneric(name))) {
        setGeneric(name, def = eval(parse(text = paste0("function(x) {",
           "standardGeneric('", name, "')","}"))))
    }
    setMethod(name, "BasicType",
        eval(envir = environment(),
            bquote(function(x) {
                v1 <- to_vecbasic(x)
                v  <- .(as.name(paste0(".vecbasic_", name)))(v1)
                return(get_final_output(x, v))
            })))
}

setMethod("sinpi", c(x = "BasicType"), function(x) sin(x * Constant("pi")))
setMethod("cospi", c(x = "BasicType"), function(x) cos(x * Constant("pi")))
setMethod("tanpi", c(x = "BasicType"), function(x) tan(x * Constant("pi")))

#' @export
expand <- function (expr) {
    v1 <- to_vecbasic(expr)
    v  <- .vecbasic_expand(v1)
    return(get_final_output(expr, v))
}

# Two arg function ============================================================
#' @export
# usage: diff(expr, x), diff(expr, x, y), diff(expr, x, y, 3)
diff <- function (expr, ...) {
    v    <- to_vecbasic(expr)
    args <- list(...)
    i    <- 1
    while (i <= length(args)) {
        x <- args[[i]]
        y <- 1
        if (class(x) != "Basic")
            stop("Invalid value type")
        if (i + 1 <= length(args) && is.numeric(args[[i + 1]])) {
            y <- as.integer(args[[i + 1]])
            i <- i + 1
        }
        while (y > 0) {
            v <- .vecbasic_diff(v, to_vecbasic(x))
            y <- y - 1
        }
        i <- i + 1
    }
    return(get_final_output(expr, v))
}

#' @export
# usage: subs(expr, x, y), subs(expr, x, z, y, u)
subs <- function(expr, a, b, ...) {
    helper_subs <- function(vec, old, new, ...) {
        if (missing(new))
            stop("invalid arguments length")

        r <- .vecbasic_subs(vec, old, new)
        if (length(list(...)) == 0)
            return(r)
        return(helper_subs(r, ...))
    }
    v <- to_vecbasic(expr)
    v <- helper_subs(v, a, b, ...)
    return(get_final_output(expr, v))
}