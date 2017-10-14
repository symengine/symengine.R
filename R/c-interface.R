

#' @useDynLib symengine hello
#' @export
c_hello <- function () {
    .Call("hello")
}
