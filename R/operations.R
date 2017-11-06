
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

#' @export
expand <- function (expr) {
    expr <- S(expr)
    new("Basic", api_basic_expand(expr))
}



## Trigonometry functions  =====================================================

#' @export
Trigonometry <- (function() {
    flist <- alist(sin, cos, tan, asin, acos, atan, csc, sec, cot, acsc, asec, acot,
                   sinh, cosh, tanh, asinh, acosh, atanh, csch, sech, coth, acsch, asech, acoth)
    ans <- vector("list", length(flist))
    for (i in seq_along(ans)) {
        ans[[i]] <- eval(envir = parent.frame(),
            bquote(function (x) {
                new("Basic", .(as.name(paste0("api_basic_", deparse(flist[[i]]))))(S(x)))
            }
        ))
        names(ans)[i] <- deparse(flist[[i]])
    }
    class(ans) <- "symengine.trigonometry" # Or?
    ans
    # ans <- as.environment(ans)
    # mkExportedEnv <- function(e) {
    #     stopifnot(is.environment(e))
    #     lockEnvironment(e)
    #     # for (binding in ls(e))
    #     #     lockBinding(binding, e)
    #     class(e) <- "exported.env"
    #     e
    # }
    # mkExportedEnv(ans)
})()


#' @export
print.symengine.trigonometry <- function(x, ...) {
    if (length(list(...)))
        warning("Extra arguments are ignored.")
    if (digest::digest(x) != digest::digest(Trigonometry))
        warning("Contents have been modified.")
    cat("Members:\n")
    a <- matrix(names(x), nrow = 3)
    for (i in seq(nrow(a))) {
        cat("  ")
        out <- format(a[i, ])
        if (requireNamespace("crayon", quietly = TRUE))
            out <- crayon::italic(out)
        cat(out)
        cat("\n")
    }
}

setMethod("sin",  c(x = "Basic"), Trigonometry$sin )
setMethod("cos",  c(x = "Basic"), Trigonometry$cos )
setMethod("tan",  c(x = "Basic"), Trigonometry$tan )
setMethod("acos", c(x = "Basic"), Trigonometry$acos)
setMethod("asin", c(x = "Basic"), Trigonometry$asin)
setMethod("atan", c(x = "Basic"), Trigonometry$atan)

setMethod("sinpi", c(x = "Basic"), function(x) sin(x * Constant("pi")))
setMethod("cospi", c(x = "Basic"), function(x) cos(x * Constant("pi")))
setMethod("tanpi", c(x = "Basic"), function(x) tan(x * Constant("pi")))