
## Set SymEngine Options
##
## This function wraps `base::options` and may provide better hints to the
## available options as arguments.
##
## Is it necessary??
if (FALSE) {
    symengine_opts <- function(latex, latex.center) {
        # All arguments are missing, then return a list of all options.
        if (length(match.call()) == 1) {
            all_opt_names <- paste0("symengine.", names(formals(sys.function())))
            ans <- lapply(all_opt_names, function(x) getOption(x, default = NULL))
            return(ans)
        }
        matched_call <- match.call()
        supplied_arg_names <- names(as.list(matched_call)[2:length(matched_call)])
        opts <- mget(supplied_arg_names, envir = environment(), inherits = FALSE)
        names(opts) <- paste0("symengine.", names(opts))
        options(opts)
    }
}

## These S3 methods will be registered in `.onLoad`

knit_print.Basic <- function(x, ...) {
    if (getOption("symengine.latex", default = FALSE)) {
        latex_str <- codegen(x, type = "latex")
        if (getOption("symengine.latex.center", default = FALSE))
            latex_str <- sprintf("$$ %s $$", latex_str)
        else
            latex_str <- sprintf("$%s$", latex_str)
        return(knitr::asis_output(latex_str))
    }
    knitr::normal_print(x, ...)
}

