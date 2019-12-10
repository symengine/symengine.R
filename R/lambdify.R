
#' Convert A Basic/VecBasic Object to R Function
#' 
#' These functions currently use \code{\link{DoubleVisitor}} to
#' convert a Basic/VecBasic object to a \code{DoubleVisitor} which
#' essentially is a S4 class extending R function.
#' 
#' @param x A Basic object or a VecBasic object.
#' @param args A VecBasic object specifying the arguments of the resulted function.
#' It will be passed to \code{\link{DoubleVisitor}} and can be missing.
#' @param backend One of "auto", "lambda" and "llvm". If "auto", 
#' \code{getOption("lambdify.backend")} will be used to determine the value. If that
#' option is not set, it will be determined based on \code{symengine_have_component("llvm")}.
#' @param perform_cse Passed to \code{\link{DoubleVisitor}}.
#' 
#' @return A \code{DoubleVisitor} S4 object.
#' 
#' @seealso \code{\link{DoubleVisitor}}
#' 
#' @rdname lambdify
#' @export
lambdify <- function(x, args, backend = c("auto", "lambda", "llvm"), perform_cse = TRUE) {
    backend <- match.arg(backend)
    if (backend == "auto") {
        opt <- getOption("lambdify.backend")
        if (is.null(opt))
            if (symengine_have_component("llvm"))
                backend <- "llvm"
            else
                backend <- "lambda"
        else
            backend <- opt
    }
    
    if (backend == "lambda")
        llvm_opt_level <- -1L
    else if (backend == "llvm")
        llvm_opt_level <- 2L
    
    if (!missing(args)) {
        args <- Vector(args)
    }
    
    DoubleVisitor(x, args, perform_cse = perform_cse, llvm_opt_level = llvm_opt_level)
}

#' @param ... Not used
#' @rdname lambdify
#' @export
as.function.BasicOrVecBasic <- function(x, args, backend = "auto", perform_cse = TRUE, ...) {
    if (!missing(...))
        warning("Extra arguments are ignored")
    lambdify(x, args, backend = backend, perform_cse = perform_cse)
}


## This is the old version of lambdify by converting Basic to R language object.
## However, it has flaws when there is no equivalent numeric functions in R.
## *Currently we do not export it*
lambdify_old <- function(x) {
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
    
    env <- parent.env(environment())
    
    eval(call("function", args, body), env)
}
