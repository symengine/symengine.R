
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
    x@ptr <- ptr
    x
}

basic_expand        <- function(x) .subS4_extptr(x, .basic_expand        (x@ptr))
basic_neg           <- function(x) .subS4_extptr(x, .basic_neg           (x@ptr))
basic_abs           <- function(x) .subS4_extptr(x, .basic_abs           (x@ptr))
basic_erf           <- function(x) .subS4_extptr(x, .basic_erf           (x@ptr))
basic_erfc          <- function(x) .subS4_extptr(x, .basic_erfc          (x@ptr))
basic_sin           <- function(x) .subS4_extptr(x, .basic_sin           (x@ptr))
basic_cos           <- function(x) .subS4_extptr(x, .basic_cos           (x@ptr))
basic_tan           <- function(x) .subS4_extptr(x, .basic_tan           (x@ptr))
basic_asin          <- function(x) .subS4_extptr(x, .basic_asin          (x@ptr))
basic_acos          <- function(x) .subS4_extptr(x, .basic_acos          (x@ptr))
basic_atan          <- function(x) .subS4_extptr(x, .basic_atan          (x@ptr))
basic_csc           <- function(x) .subS4_extptr(x, .basic_csc           (x@ptr))
basic_sec           <- function(x) .subS4_extptr(x, .basic_sec           (x@ptr))
basic_cot           <- function(x) .subS4_extptr(x, .basic_cot           (x@ptr))
basic_acsc          <- function(x) .subS4_extptr(x, .basic_acsc          (x@ptr))
basic_asec          <- function(x) .subS4_extptr(x, .basic_asec          (x@ptr))
basic_acot          <- function(x) .subS4_extptr(x, .basic_acot          (x@ptr))
basic_sinh          <- function(x) .subS4_extptr(x, .basic_sinh          (x@ptr))
basic_cosh          <- function(x) .subS4_extptr(x, .basic_cosh          (x@ptr))
basic_tanh          <- function(x) .subS4_extptr(x, .basic_tanh          (x@ptr))
basic_asinh         <- function(x) .subS4_extptr(x, .basic_asinh         (x@ptr))
basic_acosh         <- function(x) .subS4_extptr(x, .basic_acosh         (x@ptr))
basic_atanh         <- function(x) .subS4_extptr(x, .basic_atanh         (x@ptr))
basic_csch          <- function(x) .subS4_extptr(x, .basic_csch          (x@ptr))
basic_sech          <- function(x) .subS4_extptr(x, .basic_sech          (x@ptr))
basic_coth          <- function(x) .subS4_extptr(x, .basic_coth          (x@ptr))
basic_acsch         <- function(x) .subS4_extptr(x, .basic_acsch         (x@ptr))
basic_asech         <- function(x) .subS4_extptr(x, .basic_asech         (x@ptr))
basic_acoth         <- function(x) .subS4_extptr(x, .basic_acoth         (x@ptr))
basic_lambertw      <- function(x) .subS4_extptr(x, .basic_lambertw      (x@ptr))
basic_zeta          <- function(x) .subS4_extptr(x, .basic_zeta          (x@ptr))
basic_dirichlet_eta <- function(x) .subS4_extptr(x, .basic_dirichlet_eta (x@ptr))
basic_gamma         <- function(x) .subS4_extptr(x, .basic_gamma         (x@ptr))
basic_sqrt          <- function(x) .subS4_extptr(x, .basic_sqrt          (x@ptr))
basic_exp           <- function(x) .subS4_extptr(x, .basic_exp           (x@ptr))
basic_log           <- function(x) .subS4_extptr(x, .basic_log           (x@ptr))





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
    expr@ptr <- .basic_subs2(expr@ptr, old@ptr, new@ptr)
    expr
}

basic_evalf <- function(x, bits = 53L, real = TRUE) {
    x@ptr <- .basic_evalf(x@ptr, bits, real)
    x
}

# #' @export
# subs <- function (expr, old, new) {
#     expr <- S(expr)
#     old  <- S(old)
#     new  <- S(new)
#     basic_subs2(expr, old, new)
# }

#' @export
evalf <- function (expr, bits = 53L, to = c("real", "complex")) {
    expr    <- S(expr)
    to_real <- identical(match.arg(to), "real")
    basic_evalf(expr, bits = bits, real = to_real)
}

binomial <- function(a, b) {
    if (basic_type(a) != "Integer")
        stop("a must be integer basic")
    .ntheory_binomial(a, as.integer(b))
}

factorial <- function(n) {
    .ntheory_factorial(as.integer(n))
}

nextprime <- function(a) {
    if (basic_type(a) != "Integer")
        stop("a must be integer basic")
    .ntheory_nextprime(a)
}

lcm <- function(a, b) {
    if (basic_type(a) != "Integer")
        stop("a must be integer basic")
    if (basic_type(a) != "Integer")
        stop("b must be integer basic")
    .ntheory_lcm(a, b)
}

gcd <- function(a, b) {
    if (basic_type(a) != "Integer")
        stop("a must be integer basic")
    if (basic_type(a) != "Integer")
        stop("b must be integer basic")
    .ntheory_gcd(a, b)
}