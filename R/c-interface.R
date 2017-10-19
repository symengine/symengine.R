

#' @useDynLib symengine c_ascii_art_str
#' @export
api_symengine_logo <- function () {
    s <- .Call("c_ascii_art_str")   
    s
}

#' @useDynLib symengine c_new_heap_symbol
#' @export
api_new_symbol <- function (string) {
    .Call("c_new_heap_symbol", string)
}


## Accessors for Basic  ========================================================

#' @useDynLib symengine c_basic_str
#' @export
api_basic_str <- function (ptr) {
    stopifnot(identical(typeof(ptr), "externalptr"))
    .Call("c_basic_str", ptr)
}

#' @useDynLib symengine c_basic_type
#' @export
api_basic_type <- function (ptr) {
    stopifnot(identical(typeof(ptr), "externalptr"))
    .Call("c_basic_type", ptr)
}
