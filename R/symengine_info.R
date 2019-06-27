
#' Information about SymEngine Library
#' 
#' Functions to get symengine logo, version and external libraries built with.
#'
#' @rdname symengine_info
#' @export
symengine_version <- function() {
    cwrapper_symengine_version()
}

#' @rdname symengine_info
#' @export
symengine_ascii_art <- function() {
    strsplit(cwrapper_symengine_ascii_art(), split = "\n")[[1]]
}

#' @rdname symengine_info
#' @export
symengine_have_component <- function (
    which = c("mpfr", "flint", "arb", "mpc", "ecm",
              "primesieve", "piranha", "boost", "pthread", "llvm"))
{
    vapply(which, FUN.VALUE = logical(1L), cwrapper_symengine_have_component)
}

#' @rdname symengine_info
symengine_compilation_notes <- function() {
    compilation_notes()
}

