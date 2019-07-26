
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

