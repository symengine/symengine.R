
#' @include basic.R
NULL


setMethods <- function (f, signatures=list(), definition,
                        where=topenv(parent.frame()), ...) {
    for (signature in signatures)
        setMethod(f, signature=signature, definition, where=where, ...)
}



basic_add <- function (a, b) {
    new("Basic", .basic_add(as(a, "externalptr"), as(b, "externalptr")))
}
basic_sub <- function (a, b) {
    new("Basic", .basic_sub(as(a, "externalptr"), as(b, "externalptr")))
}
basic_mul <- function (a, b) {
    new("Basic", .basic_mul(as(a, "externalptr"), as(b, "externalptr")))
}
basic_div <- function (a, b) {
    new("Basic", .basic_div(as(a, "externalptr"), as(b, "externalptr")))
}
basic_pow <- function (a, b) {
    new("Basic", .basic_pow(as(a, "externalptr"), as(b, "externalptr")))
}


basic_diff <- function (expr, sym) {
    new("Basic", .basic_diff(as(expr, "externalptr"), as(sym, "externalptr")))
}

basic_expand <- basic_expand
basic_neg <- basic_neg
basic_abs <- basic_abs
basic_erf <- basic_erf
basic_erfc <- basic_erfc
basic_sin <- basic_sin
basic_cos <- basic_cos
basic_tan <- basic_tan
basic_asin <- basic_asin
basic_acos <- basic_acos
basic_atan <- basic_atan
basic_csc <- basic_csc
basic_sec <- basic_sec
basic_cot <- basic_cot
basic_acsc <- basic_acsc
basic_asec <- basic_asec
basic_acot <- basic_acot
basic_sinh <- basic_sinh
basic_cosh <- basic_cosh
basic_tanh <- basic_tanh
basic_asinh <- basic_asinh
basic_acosh <- basic_acosh
basic_atanh <- basic_atanh
basic_csch <- basic_csch
basic_sech <- basic_sech
basic_coth <- basic_coth
basic_acsch <- basic_acsch
basic_asech <- basic_asech
basic_acoth <- basic_acoth
basic_lambertw <- basic_lambertw
basic_zeta <- basic_zeta
basic_dirichlet_eta <- basic_dirichlet_eta
basic_gamma <- basic_gamma
basic_sqrt <- basic_sqrt
basic_exp <- basic_exp
basic_log <- basic_log





setMethods("+",
    list(c(e1 = "Basic", e2 = "Basic"),
         c(e1 = "Basic", e2 = "ANY"),
         c(e1 = "ANY"  , e2 = "Basic")),
    
    function (e1, e2) {
        basic_add(S(e1), S(e2))
    }
)

setMethods("-",
    list(c(e1 = "Basic", e2 = "Basic"),
         c(e1 = "Basic", e2 = "ANY"),
         c(e1 = "ANY"  , e2 = "Basic")),
    
    function (e1, e2) {
        basic_sub(S(e1), S(e2))
    }
)

setMethods("*",
    list(c(e1 = "Basic", e2 = "Basic"),
         c(e1 = "Basic", e2 = "ANY"),
         c(e1 = "ANY"  , e2 = "Basic")),
    
    function (e1, e2) {
        basic_mul(S(e1), S(e2))
    }
)

setMethods("/",
    list(c(e1 = "Basic", e2 = "Basic"),
         c(e1 = "Basic", e2 = "ANY"),
         c(e1 = "ANY"  , e2 = "Basic")),
    
    function (e1, e2) {
        basic_div(S(e1), S(e2))
    }
)

setMethods("^",
    list(c(e1 = "Basic", e2 = "Basic"),
         c(e1 = "Basic", e2 = "ANY"),
         c(e1 = "ANY"  , e2 = "Basic")),
    
    function (e1, e2) {
        basic_pow(S(e1), S(e2))
    }
)

#' @export
diff <- function (expr, sym) {
    expr <- S(expr)
    if (is.character(sym))
        sym <- S(sym)
    #if (basic_type(sym) != "Symbol")
    #    stop("sym should be a ", sQuote("Symbol"), ", got ", sQuote(basic_type(sym)))
    basic_diff(expr, sym)
}

#' @export
expand <- function (expr) {
    basic_expand(S(expr))
}

setMethod("-", c(e1 = "Basic", e2 = "missing"),
    function (e1, e2) {
        basic_neg(S(e1))
    }
)

setMethod("+", c(e1 = "Basic", e2 = "missing"),
    function (e1, e2) {
        e1
    }
)

setMethod("abs", c(x = "Basic"),
    function (x) {
        basic_abs(S(x))
    }
)

setMethod("sqrt", c(x = "Basic"),
    function (x) {
        basic_sqrt(S(x))
    }
)

setMethod("exp", c(x = "Basic"),
    function (x) {
        basic_exp(S(x))
    }
)

# TODO: log


## Trigonometry functions  =====================================================

#' @export
Trigonometry <- (function() {
    flist <- alist(sin, cos, tan, asin, acos, atan, csc, sec, cot, acsc, asec, acot,
                   sinh, cosh, tanh, asinh, acosh, atanh, csch, sech, coth, acsch, asech, acoth)
    ans <- vector("list", length(flist))
    for (i in seq_along(ans)) {
        ans[[i]] <- eval(envir = parent.frame(),
            bquote(function (x) {
                .(as.name(paste0("basic_", deparse(flist[[i]]))))(S(x))
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


basic_subs2 <- function (expr, old, new) {
    expr@.xData <- .basic_subs2(expr@.xData, old@.xData, new@.xData)
    expr
}

basic_evalf <- function(x, bits = 53L, real = TRUE) {
    x@.xData <- .basic_evalf(x@.xData, bits, real)
    x
}

#' @export
subs <- function (expr, old, new) {
    expr <- S(expr)
    old  <- S(old)
    new  <- S(new)
    basic_subs2(expr, old, new)
}

#' @export
evalf <- function (expr, bits = 53L, to = c("real", "complex")) {
    expr    <- S(expr)
    to_real <- identical(match.arg(to), "real")
    basic_evalf(expr, bits = bits, real = to_real)
}



# ---------------------<<<<







## Equality  ===================================================================

setMethod("==", c(e1 = "Basic", e2 = "Basic"),
    function(e1, e2) api_basic_eq(e1, e2)
)

setMethod("!=", c(e1 = "Basic", e2 = "Basic"),
    function(e1, e2) api_basic_neq(e1, e2)
)

    