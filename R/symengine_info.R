
#' @include RcppExports.R
NULL




#' Information about SymEngine Library
#' 
#' Functions to get symengine logo, version and external libraries built with.
#'
#' @rdname symengine_info
#' @export
symengine_version <- symengine_version

#' @rdname symengine_info
#' @export
symengine_ascii_art <- function() {
    strsplit(.symengine_ascii_art(), split = "\n")[[1]]
}

#' @rdname symengine_info
#' @export
symengine_have_component <- function (
    which = c("mpfr", "flint", "arb", "mpc", "ecm",
              "primesieve", "piranha", "boost", "pthread", "llvm"))
{
    vapply(which, FUN.VALUE = logical(1L), .symengine_have_component)
}

