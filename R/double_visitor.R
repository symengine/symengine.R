
#' @include classes.R
NULL

setClass("DoubleVisitor", contains = c("function", "SymEnginePTR"),
         slots = c(visitor_args = "VecBasic", visitor_exprs = "BasicOrVecBasic"))
setClass("LambdaDoubleVisitor", contains = "DoubleVisitor")
setClass("LLVMDoubleVisitor", contains = "DoubleVisitor")

#' @export
DoubleVisitor <- function(exprs, args, perform_cse = TRUE,
                          llvm_opt_level = if (symengine_have_component("llvm")) 2L else -1L) {
    if (missing(args)) {
        if (is(exprs, "Basic"))
            args <- free_symbols(exprs)
        else if (is(exprs, "VecBasic"))
            args <- unique(do.call(c, lapply(exprs, free_symbols)))
        else
            stop("'exprs' is not a Basic or VecBasic")
    }
    visitor <- s4visitor(args, exprs, perform_cse, llvm_opt_level)
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
        s4visitor_call(sys.function(),
                       .(as.call(c(quote(rbind), lapply(args_names, as.symbol)))),
                       do_transpose = TRUE)
    )
    envir <- parent.env(environment()) # package env
    
    x@.Data <- eval(call("function", args, body), envir)
    x
}

#' @export
visitor_call <- function(visitor, input, do_transpose = FALSE) {
    s4visitor_call(visitor, input, do_transpose)
}

setMethod("show", c(object = "DoubleVisitor"),
    function(object) {
        args  <- object@visitor_args
        exprs <- object@visitor_exprs
        cat(sprintf("%s:\n", class(object)))
        cat("(")
        cat(paste(as.character(args), collapse = ", "))
        cat(") => ")
        if (is(exprs, "Basic"))
            cat(as.character(exprs))
        else {
            cat("(")
            cat(paste(as.character(exprs), collapse = ", "))
            cat(")")
        }
        cat("\n")
    }
)

