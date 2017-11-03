
setMethods <- function (f, signatures=list(), definition,
                        where=topenv(parent.frame()), ...) {
    for (signature in signatures)
        setMethod(f, signature=signature, definition, where=where, ...)
}


#' @include basic.R

setMethods("+",
    list(c(e1 = "Basic", e2 = "Basic"),
         c(e1 = "Basic", e2 = "ANY"),
         c(e1 = "ANY"  , e2 = "Basic")),
    
    function (e1, e2) {
        new("Basic", api_basic_add(S(e1), S(e2)))
    }
)

setMethods("-",
    list(c(e1 = "Basic", e2 = "Basic"),
         c(e1 = "Basic", e2 = "ANY"),
         c(e1 = "ANY"  , e2 = "Basic")),
    
    function (e1, e2) {
        new("Basic", api_basic_sub(S(e1), S(e2)))
    }
)

setMethods("*",
    list(c(e1 = "Basic", e2 = "Basic"),
         c(e1 = "Basic", e2 = "ANY"),
         c(e1 = "ANY"  , e2 = "Basic")),
    
    function (e1, e2) {
        new("Basic", api_basic_mul(S(e1), S(e2)))
    }
)

setMethods("/",
    list(c(e1 = "Basic", e2 = "Basic"),
         c(e1 = "Basic", e2 = "ANY"),
         c(e1 = "ANY"  , e2 = "Basic")),
    
    function (e1, e2) {
        new("Basic", api_basic_div(S(e1), S(e2)))
    }
)

setMethods("^",
    list(c(e1 = "Basic", e2 = "Basic"),
         c(e1 = "Basic", e2 = "ANY"),
         c(e1 = "ANY"  , e2 = "Basic")),
    
    function (e1, e2) {
        new("Basic", api_basic_pow(S(e1), S(e2)))
    }
)

#' @export
diff <- function (expr, sym) {
    expr <- S(expr)
    if (is.character(sym))
        sym <- S(sym)
    if (api_basic_type(sym) != "Symbol")
        stop("sym should be a ", sQuote("Symbol"), ", got ", sQuote(api_basic_type(sym)))
    new("Basic", api_basic_diff(expr, sym))
}

