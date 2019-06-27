
basic_to_expr <- function(s) {
    if (is.language(s))
        return(s)
    
    # Conversion by each type
    .Symbol <- function(s) {
        as.name(as.character(s))
    }
    .Add <- function(s) {
        Reduce(x = as.list(s4basic_get_args(s)), function(a, b)
            bquote(.(basic_to_expr(a)) + .(basic_to_expr(b))))
    }
    .Mul <- function(s) {
        Reduce(x = as.list(s4basic_get_args(s)), function(a, b)
            bquote(.(basic_to_expr(a)) * .(basic_to_expr(b))))
    }
    .Pow <- function(s) {
        args <- s4basic_get_args(s)
        stopifnot(length(args) == 2)
        bquote(.(basic_to_expr(args[[1]])) ^ .(basic_to_expr(args[[2]])))
    }
    .Integer <- function(s) {
        as.integer(s)
    }
    .RealDouble <- function(s) {
        as.double(s)
    }
    .Infty <- function(s) {
        if (s4basic_number_is_positive(s))
            return(Inf)
        else if (s4basic_number_is_negative(s))
            return(quote(-Inf))
        else
            stop("Should not happen")
    }
    .Constant <- function(s) {
        as.double(evalf(s))
    }
    .Rational <- function(s) {
        # Is this the proper way?
        as.double(evalf(s))
    }

    ans <- switch(
        s4basic_get_type(s),
        Symbol = .Symbol(s),
        Add = .Add(s),
        Mul = .Mul(s),
        Pow = .Pow(s),
        Integer = .Integer(s),
        RealDouble = .RealDouble(s),
        Infty = .Infty(s),
        Constant = .Constant(s),
        Rational = .Rational(s),
        stop(sprintf("Conversion method for %s has not implemented", s4basic_get_type(s)))
    )
    ans
}

#' @export
lambdify <- function(x) {
    if (length(s4basic_function_symbols(x)))
        stop("TODO")
    
    body <- basic_to_expr(x)
    
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

