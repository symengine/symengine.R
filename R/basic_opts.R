
#' @include basic.R
NULL


setMethods <- function (f, signatures=list(), definition,
                        where=topenv(parent.frame()), ...) {
    for (signature in signatures)
        setMethod(f, signature=signature, definition, where=where, ...)
}



# basic_add <- function (a, b) {
#     new("Basic", .basic_add(as(a, "externalptr"), as(b, "externalptr")))
# }
# basic_sub <- function (a, b) {
#     new("Basic", .basic_sub(as(a, "externalptr"), as(b, "externalptr")))
# }
# basic_mul <- function (a, b) {
#     new("Basic", .basic_mul(as(a, "externalptr"), as(b, "externalptr")))
# }
# basic_div <- function (a, b) {
#     new("Basic", .basic_div(as(a, "externalptr"), as(b, "externalptr")))
# }
# basic_pow <- function (a, b) {
#     new("Basic", .basic_pow(as(a, "externalptr"), as(b, "externalptr")))
# }


# basic_diff <- function (expr, sym) {
#     new("Basic", .basic_diff(as(expr, "externalptr"), as(sym, "externalptr")))
# }


.subS4_extptr <- function(x, ptr) {
    x@.xData <- ptr
    x
}

basic_expand        <- function(x) .subS4_extptr(x, .basic_expand        (x@.xData))
basic_neg           <- function(x) .subS4_extptr(x, .basic_neg           (x@.xData))
basic_abs           <- function(x) .subS4_extptr(x, .basic_abs           (x@.xData))
basic_erf           <- function(x) .subS4_extptr(x, .basic_erf           (x@.xData))
basic_erfc          <- function(x) .subS4_extptr(x, .basic_erfc          (x@.xData))
basic_sin           <- function(x) .subS4_extptr(x, .basic_sin           (x@.xData))
basic_cos           <- function(x) .subS4_extptr(x, .basic_cos           (x@.xData))
basic_tan           <- function(x) .subS4_extptr(x, .basic_tan           (x@.xData))
basic_asin          <- function(x) .subS4_extptr(x, .basic_asin          (x@.xData))
basic_acos          <- function(x) .subS4_extptr(x, .basic_acos          (x@.xData))
basic_atan          <- function(x) .subS4_extptr(x, .basic_atan          (x@.xData))
basic_csc           <- function(x) .subS4_extptr(x, .basic_csc           (x@.xData))
basic_sec           <- function(x) .subS4_extptr(x, .basic_sec           (x@.xData))
basic_cot           <- function(x) .subS4_extptr(x, .basic_cot           (x@.xData))
basic_acsc          <- function(x) .subS4_extptr(x, .basic_acsc          (x@.xData))
basic_asec          <- function(x) .subS4_extptr(x, .basic_asec          (x@.xData))
basic_acot          <- function(x) .subS4_extptr(x, .basic_acot          (x@.xData))
basic_sinh          <- function(x) .subS4_extptr(x, .basic_sinh          (x@.xData))
basic_cosh          <- function(x) .subS4_extptr(x, .basic_cosh          (x@.xData))
basic_tanh          <- function(x) .subS4_extptr(x, .basic_tanh          (x@.xData))
basic_asinh         <- function(x) .subS4_extptr(x, .basic_asinh         (x@.xData))
basic_acosh         <- function(x) .subS4_extptr(x, .basic_acosh         (x@.xData))
basic_atanh         <- function(x) .subS4_extptr(x, .basic_atanh         (x@.xData))
basic_csch          <- function(x) .subS4_extptr(x, .basic_csch          (x@.xData))
basic_sech          <- function(x) .subS4_extptr(x, .basic_sech          (x@.xData))
basic_coth          <- function(x) .subS4_extptr(x, .basic_coth          (x@.xData))
basic_acsch         <- function(x) .subS4_extptr(x, .basic_acsch         (x@.xData))
basic_asech         <- function(x) .subS4_extptr(x, .basic_asech         (x@.xData))
basic_acoth         <- function(x) .subS4_extptr(x, .basic_acoth         (x@.xData))
basic_lambertw      <- function(x) .subS4_extptr(x, .basic_lambertw      (x@.xData))
basic_zeta          <- function(x) .subS4_extptr(x, .basic_zeta          (x@.xData))
basic_dirichlet_eta <- function(x) .subS4_extptr(x, .basic_dirichlet_eta (x@.xData))
basic_gamma         <- function(x) .subS4_extptr(x, .basic_gamma         (x@.xData))
basic_sqrt          <- function(x) .subS4_extptr(x, .basic_sqrt          (x@.xData))
basic_exp           <- function(x) .subS4_extptr(x, .basic_exp           (x@.xData))
basic_log           <- function(x) .subS4_extptr(x, .basic_log           (x@.xData))





# setMethods("+",
#     list(c(e1 = "Basic", e2 = "Basic"),
#          c(e1 = "Basic", e2 = "ANY"),
#          c(e1 = "ANY"  , e2 = "Basic")),
    
#     function (e1, e2) {
#         basic_add(S(e1), S(e2))
#     }
# )

# setMethods("-",
#     list(c(e1 = "Basic", e2 = "Basic"),
#          c(e1 = "Basic", e2 = "ANY"),
#          c(e1 = "ANY"  , e2 = "Basic")),
    
#     function (e1, e2) {
#         basic_sub(S(e1), S(e2))
#     }
# )

# setMethods("*",
#     list(c(e1 = "Basic", e2 = "Basic"),
#          c(e1 = "Basic", e2 = "ANY"),
#          c(e1 = "ANY"  , e2 = "Basic")),
    
#     function (e1, e2) {
#         basic_mul(S(e1), S(e2))
#     }
# )

# setMethods("/",
#     list(c(e1 = "Basic", e2 = "Basic"),
#          c(e1 = "Basic", e2 = "ANY"),
#          c(e1 = "ANY"  , e2 = "Basic")),
    
#     function (e1, e2) {
#         basic_div(S(e1), S(e2))
#     }
# )

# setMethods("^",
#     list(c(e1 = "Basic", e2 = "Basic"),
#          c(e1 = "Basic", e2 = "ANY"),
#          c(e1 = "ANY"  , e2 = "Basic")),
    
#     function (e1, e2) {
#         basic_pow(S(e1), S(e2))
#     }
# )

# #' @export
# diff <- function (expr, sym) {
#     expr <- S(expr)
#     if (is.character(sym))
#         sym <- S(sym)
#     #if (basic_type(sym) != "Symbol")
#     #    stop("sym should be a ", sQuote("Symbol"), ", got ", sQuote(basic_type(sym)))
#     basic_diff(expr, sym)
# }

# #' @export
# expand <- function (expr) {
#     basic_expand(S(expr))
# }

# setMethod("-", c(e1 = "Basic", e2 = "missing"),
#     function (e1, e2) {
#         basic_neg(S(e1))
#     }
# )

# setMethod("+", c(e1 = "Basic", e2 = "missing"),
#     function (e1, e2) {
#         e1
#     }
# )

# setMethod("abs", c(x = "Basic"),
#     function (x) {
#         basic_abs(S(x))
#     }
# )

# setMethod("sqrt", c(x = "Basic"),
#     function (x) {
#         basic_sqrt(S(x))
#     }
# )

# setMethod("exp", c(x = "Basic"),
#     function (x) {
#         basic_exp(S(x))
#     }
# )

# TODO: log


## Trigonometry functions  =====================================================

#' @export
Trigonometry <- (function() {
    flist <- alist(sin, cos, tan, asin, acos, atan, csc, sec, cot, acsc, asec, acot,
                   sinh, cosh, tanh, asinh, acosh, atanh, csch, sech, coth, acsch, asech, acoth)
    ans <- vector("list", length(flist))
    for (i in seq_along(ans)) {
        ans[[i]] <- eval(envir = parent.frame(),
            bquote(function (x) {z
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

# setMethod("sin",  c(x = "Basic"), Trigonometry$sin )
# setMethod("cos",  c(x = "Basic"), Trigonometry$cos )
# setMethod("tan",  c(x = "Basic"), Trigonometry$tan )
# setMethod("acos", c(x = "Basic"), Trigonometry$acos)
# setMethod("asin", c(x = "Basic"), Trigonometry$asin)
# setMethod("atan", c(x = "Basic"), Trigonometry$atan)

# setMethod("sinpi", c(x = "Basic"), function(x) sin(x * Constant("pi")))
# setMethod("cospi", c(x = "Basic"), function(x) cos(x * Constant("pi")))
# setMethod("tanpi", c(x = "Basic"), function(x) tan(x * Constant("pi")))


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

