
## The internal routine converting Basic to R expression ===============

asLanguage <- function(x) {
    if (!inherits(x, "Basic"))
        return(x)
    btype <- s4basic_get_type(x)
    func <- get0(btype, envir = asLanguageTable, inherits = FALSE)
    if (is.null(func))
        stop(sprintf("Conversion method for %s has not implemented", btype))
    func(x)
}

asLanguageTable <- as.environment(list(
    Symbol     = function(s) as.name(as.character(s)),
    Integer    = function(s) as.integer(s),
    RealDouble = function(s) as.double(s),
    Constant   = function(s) as.double(evalf(s)),
    Rational   = function(s) as.double(evalf(s)),
    Add = function(s) {
        Reduce(x = as.list(s4basic_get_args(s)),
               function(a, b) bquote(.(asLanguage(a)) + .(asLanguage(b))))
    },
    Mul = function(s) {
        Reduce(x = as.list(s4basic_get_args(s)),
               function(a, b) bquote(.(asLanguage(a)) * .(asLanguage(b))))
    },
    Pow = function(s) {
        args <- s4basic_get_args(s)
        stopifnot(length(args) == 2)
        bquote(.(asLanguage(args[[1]])) ^ .(asLanguage(args[[2]])))
    },
    Infty = function(s) {
        if (s4basic_number_is_positive(s)) return(Inf)
        else if (s4basic_number_is_negative(s)) return(-Inf)
        stop("Unexpected")
    }
))


## Parse R expression and formula  =====================================

## This function will be called by 'S'

RExprSupported <- c(
    "+", "-", "*", "/", "^", "("
)
RPkgEnv <- environment()

## TODO: add test case S(~ .(~x))
s4basic_parse_language <- function(x) {
    parseLanguage(x, check_whole_number = FALSE, backquote_env = NA)
}

parseLanguage <- function(x, check_whole_number, backquote_env) {
    ## Formula
    if (is.call(x) && x[[1L]] == quote(`~`)) {
        if (length(x) != 2)
            stop("Can only accept formula with right hand side")
        formula_env <- environment(x)
        if (is.null(formula_env))
            formula_env <- NA ## Ensure it will fail the backquoting
        return(parseLanguage(x[[2L]],
            check_whole_number = TRUE, backquote_env = formula_env))
    }
    ## Backquote inside an expression
    if (is.call(x) && x[[1L]] == as.name(".")) {
        inner_x <- x[[2L]]
        value <- eval(inner_x, envir = backquote_env)
        ## Non-Basic value should be converted to Basic
        return(s4basic_parse(value, check_whole_number = FALSE))
    }
    # ## `{{ x }}` style backquoting.
    # if (is.call(x) && x[[1L]] == as.name("{")
    #     && length(x) == 2L && expr[[2]][[1]] == as.name("{")) {
    #     inner_x <- as.list(expr[[2]][-1])
    #     for (e in inner_x)
    #         last <- eval(e, envir = backquote_env)
    #     return(s4basic_parse(last, check_whole_number = FALSE))
    # }
    
    ## Supported functions
    if (is.call(x) && is.symbol(x[[1L]]) &&
        (as.character(x[[1L]]) %in% RExprSupported)) {
        args <- lapply(x[-1], parseLanguage,
                       check_whole_number = check_whole_number,
                       backquote_env = backquote_env)
        func <- get(as.character(x[[1L]]), mode = "function", envir = RPkgEnv)
        return(do.call(func, args))
    }
    
    if (is.symbol(x))
        return(Symbol(as.character(x)))
    if (is.integer(x))
        return(S(x))
    if (is.double(x))
        return(s4basic_parse(x, check_whole_number = check_whole_number))
    #if (is.character(expr))
    #    return(S(expr))
    
    if (inherits(x, "Basic"))
        return(x)
    
    ## i.e. cases like f()(y)
    ## I am not sure whether this should be allowed. (unless x[[1]] is backquoted?)
    ## Also note that bquote does not handle bquote(.(func)(x))
    if (is.call(x) && (!is.symbol(x[[1L]]))) {
        "pass"
    }
    
    stop(sprintf("Unable to parse %s", deparse(x)))
}

