#' symengine: R interface to SymEngine C++ library for symbolic computation
#' 
#' `symengine` is a R package for symbolic computation.
#' 
#' [SymEngine library](https://github.com/symengine/) is a standalone fast symbolic
#' manipulation library written in C++. It allows computation over mathematical expressions
#' in a way which is similar to the traditional manual computations of mathematicians and
#' scientists. The R interface of the library tries to provide a user-friendly way to do
#' symbolic computation in R and can be integrated into other packages to help solve related
#' tasks. The design of the package is somehow similar to the [SymPy](https://www.sympy.org)
#' package in Python. Unlike some other computer algebra systems, it does not invent its own
#' language or domain specific language but uses R language to manipulate the symbolic
#' expressions.
#' 
#' `symengine` uses the S4 dispatch system extensively to differentiate between calculation
#' over normal R objects and symengine objects. For example, the semantics of `sin` in
#' `expr <- Symbol("x"); sin(expr)` is different from the `sin` used over normal R numbers.
#' 
#' @section Basic class:
#' `Basic` is simply a S4 class holding a pointer representing a symbolic expression
#' in symengine. `Basic` objects have the same S4 class but can have different
#' C-level representations which can be accessed via [get_type()].
#' For example, `Basic(~ 1/2)` will have "Rational" type and `Basic(1/2)` will have
#' "RealDouble" type.
#' 
#' A `Basic` object will also have a list of associated sub-components
#' which can be accessed via [get_args()]. For example, `(expr <- S("x") * 3L * S("a"))`
#' will have type "Mul", and `as.list(get_args(expr))` will show the three factors of
#' the multiplication.
#' 
#' A `Basic` object can be constructed via [Basic()], [S()], [Symbol()], [Constant()] or
#' [Real()].
#' 
#' @section VecBasic and DenseMatrix class:
#' VecBasic and DenseMatrix are S4 classes representing a symbolic vector or matrix.
#' They can be constructed with [Vector()], [V()], [Matrix()], `c()`, `rbind()`
#' or `cbind()`. For example the following code will construct a 2x3 matrix.
#' 
#' ```
#' vec <- Vector("a", "b")
#' cbind(vec, vec^2L, c(S("c"), S("d")))
#' ```
#' 
#' The following functions are expected to work naturally with VecBasic and DenseMatrix
#' classes.
#' - `[`, `[[`, `[<-` and `[[<-` for subsetting and assignment.
#' - `dim()`, `dim<-`, `length()`, `t()`, `det()`, `rbind()`, `cbind()`, `c()`, `rep()`
#' - `%*%` for matrix multiplication
#' - `solve(a, b)`: solve \code{a \%*\% x = b} where `a` is a square DenseMatrix and
#'   `b` is a VecBasic/DenseMatrix.
#' - `solve(a)`: find the inverse of `a` where `a` is a square DenseMatrix.
#' - `solve(a, b)`: solve system of linear equations represented by `a` (VecBasic) with
#'   regards to symbols in `b` (VecBasic).
#' 
#' Further, the R functions that work on Basic objects (e.g. `sin`) are expected work
#' on VecBasic and DenseMatrix objects as well in a vectorized manner.
#' 
#' @section Function bindings:
#' 
#' The following is a (incomplete) list of functions that are expected to work with
#' symengine objects. Note that these functions can also be used inside a formula or
#' R language objects and passed to [S] or [Basic] or [Vector] to construct symengine
#' objects. For example `S(~ sin(x) + 1)` and `S(quote(sin(x) + 1))`.
#' 
#' - `+`, `-`, `*`, `/`, `^`
#' - `abs`, `sqrt`, `exp`, `expm1`, `log`, `log10`, `log2`, `log1p`
#' - `cos`, `cosh`, `sin`, `sinh`, `tan`, `tanh`, `acos`, `acosh`, `asin`, `asinh`, `atan`, `atanh`
#' - `cospi`, `sinpi`, `tanpi`, `gamma`, `lgamma`, `digamma`, `trigamma`
#' - `lambertw`, `zeta`, `dirichlet_eta`, `erf`, `erfc`
#' - `atan2`, `kronecker_delta`, `lowergamma`, `uppergamma`, `psigamma`, `beta`
#' 
#' @docType package
#' @name symengine
#' @md
NULL

#' @import methods
#' @importFrom Rcpp cppFunction
NULL
