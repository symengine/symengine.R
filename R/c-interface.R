
#' @useDynLib symengine c_ascii_art_str
#' @export
symengine_logo <- function () {
    s <- .Call("c_ascii_art_str")   
    strsplit(s, "\n")[[1]]
}
