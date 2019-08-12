
#' @include classes.R
NULL

setClass("LambdaDoubleVisitor", contains = c("function", "SymEnginePTR"),
         slots = c(visitor_args = "VecBasic", visitor_exprs = "BasicOrVecBasic"))

#' @export
LambdaDoubleVisitor <- function(exprs, args, perform_cse = TRUE) {
    if (missing(args)) {
        if (is(exprs, "Basic"))
            args <- free_symbols(exprs)
        else if (is(exprs, "VecBasic"))
            args <- unique(do.call(c, lapply(exprs, free_symbols)))
        else
            stop("'exprs' is not a Basic or VecBasic")
    }
    visitor <- s4lambdavit(args, exprs, perform_cse)
    visitor <- visitor_lambdify(visitor)
    visitor
}

visitor_lambdify <- function(x) {
    named_pairlist <- function(x) {
        ans <- replicate(length(x), substitute())
        names(ans) <- x
        as.pairlist(ans)
    }
    args_names <- as.character(x@visitor_args)
    args <- named_pairlist(args_names)
    body <- bquote(
        visitor_call(sys.function(),
                     .(as.call(c(quote(rbind), lapply(args_names, as.symbol)))))
    )
    envir <- parent.env(environment()) # package env
    
    x@.Data <- eval(call("function", args, body), envir)
    x
}

#' @export
visitor_call <- function(visitor, input) {
    s4lambdavit_call(visitor, input)
}

