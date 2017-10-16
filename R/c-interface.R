
#' @useDynLib symengine c_ascii_art_str
#' @export
symengine_logo <- function () {
    s <- .Call("c_ascii_art_str")   
    s
}

#' @useDynLib symengine c_new_heap_symbol
#' @export
symengine_new_symbol_heap <- function (string) {
    .Call("c_new_heap_symbol", string)
}

