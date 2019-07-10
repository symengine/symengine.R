
#' @include classes.R
NULL

#' Create a FunctionSymbol
#' 
#' \code{FunctionSymbol} creates a Basic object with type \code{FunctionSymbol}.
#' \code{Function} returns a generator.
#' 
#' @param name Name of the function symbol
#' @param args Dependent symbols
#' 
#' @rdname FunctionSymbol
#' @export
Function <- function(name) {
    new("FunctionSymbolGenerator", name = name)
}

#' @rdname FunctionSymbol
#' @export
FunctionSymbol <- function(name, args) {
    if (!is.character(name) || length(name) != 1L)
        stop("name argument must be a length-one character vector")
    args <- Vector(args)
    s4basic_function(name, args)
}

setMethod("show", c("FunctionSymbolGenerator"),
    function(object) {
        str <- sprintf("FunctionSymbolGenerator\t%s\n", object@name)
        cat(str)
    }
)


