
#' @import methods
#' @importFrom Rcpp cppFunction
NULL

#' @useDynLib symengine
NULL

dots <- function (...)
    eval(substitute(alist(...)))

#' @export
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
