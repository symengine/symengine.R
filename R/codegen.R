
#' Code Generation
#' 
#' Generate C/MathML/LaTeX/JavaScript code string from a \code{Basic}
#' or \code{VecBasic} object.
#' 
#' @param x A Basic or a VecBasic object.
#' @param type One of "ccode", "mathml", "latex" and "jscode".
#' 
#' @return A character vector.
#' @export
codegen <- function(x, type = c("ccode", "mathml", "latex", "jscode")) {
    type <- match.arg(type)
    if (s4basic_check(x))
        return(s4basic_codegen(x, type))
    if (s4vecbasic_check(x)) {
        return(vapply(x, function(a) s4basic_codegen(a, type), FUN.VALUE = character(1)))
    }
    stop("x should be a Basic or a VecBasic")
}
