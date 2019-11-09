
#' Get Information about Basic Object
#' 
#' These functions are used to access the underlying properties of a
#' \code{Basic} object.
#' 
#' \describe{
#'   \item{get_type}{Return the internal type}
#'   \item{get_args}{Return the internal arguments of a Basic object as a VecBasic}
#'   \item{get_hash}{Return the hash as a string}
#'   \item{get_str}{Return the string representation of the Basic object}
#'   \item{free_symbols}{Return free symbols in an expression}
#'   \item{function_symbols}{Return function symbols in an expression}
#'   \item{get_name}{Return name of a Basic object of type FunctionSymbol}
#'   \item{get_prec}{Return precision of a Basic object of type RealMPFR}
#' }
#' 
#' @param x A Basic object.
#' @return
#'   \itemize{
#'     \item \code{get_type()}, \code{get_hash()}, \code{get_str()}, \code{get_name()}
#'           return a string.
#'     \item \code{get_args()}, \code{free_symbols()}, \code{function_symbols()}
#'           return a \code{VecBasic} S4 object.
#'     \item \code{get_prec()} returns an integer.
#'   }
#' @rdname basic-getinfo
#' @export
get_type <- function(x) {
    s4basic_get_type(x)
}

#' @rdname basic-getinfo
#' @export
get_args <- function (x) {
    s4basic_get_args(x)
}

#' @rdname basic-getinfo
#' @export
get_hash <- function (x) {
    s4basic_hash(x)
}

#' @rdname basic-getinfo
#' @export
get_str <- function(x) {
    s4basic_str(x)
}

#' @rdname basic-getinfo
#' @export
free_symbols <- function(x) {
    s4basic_free_symbols(x)
}

#' @rdname basic-getinfo
#' @export
function_symbols <- function(x) {
    s4basic_function_symbols(x)
}

#' @rdname basic-getinfo
#' @export
get_name <- function(x) {
    s4basic_function_getname(x)
}

#' @rdname basic-getinfo
#' @export
get_prec <- function(x) {
    ## TODO: return 53L for RealDouble?
    s4basic_realmpfr_get_prec(x)
}
