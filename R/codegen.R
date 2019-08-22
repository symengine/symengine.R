
#' Generate C/mathml/latex/JavaScript String
#' 
#' @param x A Basic or a VecBasic object.
#' @param type One of "ccode", "mathml", "latex" and "jscode".
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
