
####======= Solve =================================================

#' Solve Symbolic Equations
#' 
#' Solve system of equations or a polynomial equation.
#' 
#' \code{solve} is a generic function dispatched on the class of the first argument.
#' If \code{a} is a (square) DenseMatrix, it solves the equation \code{a \%*\% x = b} for \code{x}.
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
#' @examples
#' ## Inverse of a symbolic matrix
#' mat <- Matrix(c("A", "B", "C", "D"), 2)
#' solve(mat)
#' 
#' ## Solve a %*% x == b
#' a <- Matrix(c("a11", "a21", "a12", "a22"), 2) # a is a 2x2 matrix
#' b <- Vector("b1", "b2")                       # b is a length 2 vector
#' solve(a, b)                                   # Solution of x (2x1 matrix)
#' 
#' ## Solve the system of linear equations represented by a with regards to
#' ## symbols in b
#' a <- Vector(~ -2*x + y - 4,  # A system of linear equations
#'             ~  3*x + y - 9)
#' b <- Vector(~x, ~y)          # Symbols to solve (x and y)
#' solve(a, b)                  # Solution of x and y
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

