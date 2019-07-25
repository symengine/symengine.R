
#' Convert Basic Object to R Function
#' 
#' @param x A Basic object.
#' @export
lambdify <- function(x) {
    if (length(s4basic_function_symbols(x)))
        stop("TODO")
    
    body <- as.language(x)
    
    syms <- as.list(s4basic_free_symbols(x))
    syms <- vapply(syms, FUN.VALUE = character(1), function(s) {
        stopifnot(s4basic_get_type(s) == "Symbol")
        as.character(s)
    })
    
    args <- vector("list", length(syms))
    for (i in seq_along(args))
        args[[i]] <- substitute()
    names(args) <- syms
    args <- as.pairlist(args)
    
    # Should set to baseenv or parent env?
    env <- baseenv()
    
    eval(call("function", args, body), env)
}

