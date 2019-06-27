
#' @import methods
#' @importFrom Rcpp cppFunction
NULL


dots <- function (...)
    eval(substitute(alist(...)))

#setMethods <- function(f, signatures=list(), definition,
#                       where=topenv(parent.frame()), ...) {
#    for (signature in signatures)
#        setMethod(f, signature=signature, definition, where=where, ...)
#}

#' Initializing Variables
#' 
#' This is a convenient way to initialize variables and assign them in the given
#' environment.
#'
#' @param ... All the arguments will be quoted and parsed, if a argument is named,
#'     the name will be used as the name of variable to assign, otherwise the
#'     argument can only be a symbol.
#' @param .env Environment to assign.
#'
#' @return Invisibly returns a list of assigned variables.
#' @export
#'
#' @examples
#' use_vars(x, y, expr = "a + b", p = 3.14)
#' p * x + y
#' expand(expr^2L)
#' rm(x, y, expr, p)
use_vars <- function(..., .env = parent.frame()) {
    
    args <- dots(...)
    no_name <- is.null(names(args))
    
    l.vars <- vector("list", length(args))
    
    for (i in seq_along(args)) {
        if (no_name || names(args)[[i]] == "") {
            arg <- args[[i]]
            stopifnot(is.name(arg))
            names(l.vars)[[i]] <- deparse(arg)
            l.vars[[i]] <- Symbol(deparse(arg))
        }
        else {
            argname <- names(args)[[i]]
            arg     <- args[[i]]
            if (is.character(arg) || is.numeric(arg) || is.logical(arg) || is.complex(arg)) {
                names(l.vars)[[i]] <- argname
                l.vars[[i]] <- S(arg)
            }
            else if (is.name(arg)) {
                names(l.vars)[[i]] <- argname
                l.vars[[i]] <- Symbol(deparse(arg))
            }
            else
                stop(sprintf("Currently can not parse %s", class(arg)))
            # TODO: parse formula, function call, etc
        }
    }
    
    for (name in names(l.vars))
        assign(name, value = l.vars[[name]], envir = .env)
    
    message(sprintf("Initializing %s", paste(sQuote(names(l.vars)), collapse = ", ")))
    
    invisible(l.vars)
}


# tibble::lst
# tibble:::lst_quos
