
#' @include classes.R
NULL

setClass("DoubleVisitor", contains = c("function", "SymEnginePTR"),
         slots = c(visitor_args = "VecBasic", visitor_exprs = "BasicOrVecBasic"))
setClass("LambdaDoubleVisitor", contains = "DoubleVisitor")
setClass("LLVMDoubleVisitor", contains = "DoubleVisitor")

#' Double Visitor
#' 
#' Numerically evaluate symbolic expressions.
#' 
#' \code{DoubleVisitor} constructs the visitor and visitor itself is callable.
#' \code{visitor_call} is the low level function to call the visitor with input.
#' 
#' @param exprs A Basic object or a VecBasic object to be evaluated.
#' @param args A VecBasic object indicating order of input arguments. Can be missing.
#' @param perform_cse Boolean.
#' @param llvm_opt_level Integer. If negative, it will return a \code{LambdaDoubleVisitor},
#' otherwise it will return a \code{LLVMDoubleVisitor} with the specified optimization level.
#' 
#' @seealso \code{\link{lambdify}}.
#' 
#' @rdname DoubleVisitor
#' @export
#' @examples
#' a <- S("a")
#' b <- S("b")
#' c <- S("c")
#' vec <- c(log(a), log(a)/log(b) + c)
#' func <- DoubleVisitor(vec, args = c(a, b, c))
#' args(func)
#' 
#' ## Use closure
#' func(a = 1:10, b = 10:1, c = 1.43)
#' 
#' ## Use visitor_call
#' input <- rbind(a = 1:10, b = 10:1, c = 1.43)
#' visitor_call(func, input, do_transpose = TRUE)
DoubleVisitor <- function(exprs, args, perform_cse = TRUE,
                          llvm_opt_level = if (symengine_have_component("llvm")) 3L else -1L) {
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

#' @param visitor A DoubleVisitor object.
#' @param input A numeric matrix. Each row is input value for one argument.
#' @param do_transpose Boolean. Matters when \code{exprs} is a VecBasic.
#' If true, output will have each column for one symbolic expression, otherwise
#' each row for one symbolic expression.
#' @rdname DoubleVisitor
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

