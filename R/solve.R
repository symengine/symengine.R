
####======= Solve =================================================

#' Solve Symbolic Equations
#' 
#' Solve system of equations or a polynomial equation.
#' 
#' \code{solve} is a generic function dispatched on the class of the first argument.
#' If \code{a} is a (square) DenseMatrix, it solves the equation \code{a \%*\% x = b}.
#' If \code{a} is a DenseMatrix and \code{b} is missing, \code{b} is taken to be an
#' identity matrix and \code{solve} will return the inverse of \code{a}.
#' If \code{a} is a VecBasic, it solves the system of linear equations represented by
#' \code{a} with regards to symbols represented in \code{b}.
#' If \code{a} is a Basic, it solves the polynomial equation represented by a with regards
#' to the symbol represented in \code{b}.
#' 
#' @param a,b Objects, see details.
#' @param ... Not used.
#' 
#' @rdname solve
#' @exportMethod solve
setGeneric("solve")

#' @rdname solve
setMethod("solve", c(a = "DenseMatrix"),
    function(a, b, ...) {
        if (!missing(...))
            warning("Extra arguments are ignored")
        if (missing(b))
            return(s4DenseMat_inv(a))
        s4DenseMat_LU_solve(a, Matrix(b))
    }
)

#' @rdname solve
setMethod("solve", c(a = "VecBasic"),
    function(a, b, ...) {
        if (!missing(...))
            warning("Extra arguments are ignored")
        s4binding_solve_lin(a, b)
    }
)

#' @rdname solve
setMethod("solve", c(a = "Basic"),
    function(a, b, ...) {
        if (!missing(...))
            warning("Extra arguments are ignored")
        if (missing(b)) {
            a <- s4basic_parse(a)
            free_vars <- s4basic_free_symbols(a)
            if (length(free_vars) != 1L)
                stop("Number of free symbols in 'a' is not one when 'b' is missing")
            b <- free_vars[[1L]]
        }
        s4binding_solve_poly(a, b)
    }
)

