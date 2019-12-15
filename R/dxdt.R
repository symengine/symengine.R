
setClass("DxdtOdeConstructor", slots = c(x="Basic"))
setClass("DxdtOde", slots = c(x = "Basic", rhs = "Basic"))
setClass("ODESystem", slots = c(odesys = "list", cpp_source = "character",
                                compile_env = "environment"))

#' Solve System of Ordinary Differential Equations
#' 
#' This is a wrapper of the `odeintr` R package using
#' symengine objects to specify the ODE system and C code
#' generation functionality from symengine to generate the
#' C++ source. The `dxdt` function and defined `==` S4 method
#' allow one to intuitively specify the ODE system with symengine
#' objects. The `ODESystem` will generate C++ source on the fly
#' and compile with Rcpp. Then `predict` can be used to get
#' results.
#' 
#' @param x A SymEngine Basic object of type Symbol or a R object
#'     that will be converted to `Symbol(x)`.
#' @rdname ODESystem
#' @return `dxdt` returns a DxdtOdeConstructor S4 object.
#' @export
#' @md
#' @examples 
#' # A differential equation specified with dxdt and ==
#' x <- Symbol("x")
#' eq <- dxdt(x) == 1/exp(x)
#' print(eq)
dxdt <- function(x) {
    x <- Symbol(x)
    new("DxdtOdeConstructor", x = x)
}

#' @param e1 A DxdtOdeConstructor S4 object which can be returned by `dxdt`.
#' @param e2 A Basic object or an R object that will be converted to `S(e2)`.
#' @return S4 method of `==` for "DxdtOdeConstructor" returns a DxdtOde S4 object.
#' @rdname ODESystem
setMethod("==", signature=c(e1 = "DxdtOdeConstructor", e2 = "ANY"),
    function(e1, e2) {
        x <- e1@x
        rhs <- S(e2)
        new("DxdtOde", x = x, rhs = rhs)
    }
)

setMethod("show", signature = c(object = "DxdtOde"),
    function(object) {
        x <- object@x
        rhs <- object@rhs
        cat("Ordinary differential equation:\n")
        x_str <- as.character(x)
        rhs_str <- as.character(rhs)
        cat(sprintf("d(%s)/dt == %s\n", x_str, rhs_str))
    }
)

#' @param odesys,... DxdtOde S4 objects that can be returned with
#'     `dxdt(x) == rhs`. Or `odesys` can be a list of DxdtOde S4 objects
#'     when there is no dot arguments.
#' @param method,atol,rtol Passed to `odeintr::compile_sys`.
#' @param compile Logical, whether to compile the C++ source. Useful if
#'     you only want to obtain the code.
#' @return `ODESystem` returns a ODESystem S4 object.
#' @rdname ODESystem
#' @export
#' @examples
#' \dontrun{
#' ## Lorenz system
#' use_vars(x, y, z)
#' sigma <- 10
#' rho <- 28
#' beta <- 8/3
#' lorenz_sys <- ODESystem(
#'     dxdt(x) == sigma * (y - x),
#'     dxdt(y) == (rho - z) * x - y,
#'     dxdt(z) == - beta * z + x * y
#' )
#' res <- predict(
#'     lorenz_sys, init = c(x = 1, y = 1, z = 1), duration = 100, step_size = 0.001
#' )
#' plot(res[, c(2, 4)], type = 'l', col = "steelblue", main = "Lorenz Attractor")
#' }
ODESystem <- function(odesys, ..., method = "rk5_i",
                      atol = 1e-06, rtol = 1e-06, compile = TRUE) {
    if (!missing(...))
        odesys <- list(odesys, ...)
    if (is(odesys, "DxdtOde"))
        odesys <- list(odesys)
    
    ## Some checks
    check_odesys <- function(odesys) {
        stopifnot(is.list(odesys))
        
        for (el in odesys)
            stopifnot(is(el, "DxdtOde"))
        
        rhs_list <- lapply(odesys, function(x) x@rhs)
        x_list <- lapply(odesys, function(x) x@x)
        
        check_symbol <- function(names) {
            #if (any(grepl("^_", names)))
            #    stop(sprintf("variable name [%s] can not start with '_'",
            #                 names[grepl("^_", names)[1]]))
            if (any(names == ""))
                stop("variable name can not be empty string")
            if (any(grepl("(\\[|\\]| )", names)))
                stop(sprintf("variable name [%s] can not have space, '[' or ']'",
                             names[grepl("^_", names)[1]]))
        }
        check_symbol(unlist(lapply(rhs_list, function(rhs) as.character(free_symbols(rhs)))))
        check_symbol(unlist(lapply(x_list, as.character)))
        
        if (length(unique(Vector(x_list))) != length(x_list))
            stop("lhs variables should be unique")
    }
    check_odesys(odesys)
    
    rhs_list <- lapply(odesys, function(x) x@rhs)
    x_list <- lapply(odesys, function(x) x@x)
    
    x_mapping_to <- sprintf("x[%s]", seq(length(x_list)) - 1L)
    x_mapping_to <- lapply(x_mapping_to, Symbol)
    lhs_mapping_to <- sprintf("dxdt[%s]", seq(length(x_list)) - 1L)
    lhs_mapping_to <- lapply(lhs_mapping_to, Symbol)
    
    rhs_mapping_to <- rhs_list
    for (i in seq_along(x_list))
        for (j in seq_along(rhs_mapping_to))
            rhs_mapping_to[[j]] <- subs(rhs_mapping_to[[j]], x_list[[i]], x_mapping_to[[i]])
    
    ccode_str <- sprintf("%s = %s", codegen(Vector(lhs_mapping_to), type="ccode"),
                                    codegen(Vector(rhs_mapping_to), type="ccode"))
    
    compile_sys_env <- new.env()
    
    if (!requireNamespace("odeintr"))
        stop("'odeintr' package needs to be installed for this functionality")
    
    rcpp_code <- odeintr::compile_sys(
        name = "predict",
        sys = ccode_str,
        pars = NULL,
        const = FALSE,
        method = method,
        sys_dim = length(x_list),
        atol = atol,
        rtol = rtol,
        compile = FALSE,
        observer = NULL,
        env = compile_sys_env
    )
    # Sanity check
    stopifnot(length(ls(compile_sys_env, all.names = TRUE)) == 0L)
    if (compile) {
        res <- Rcpp::sourceCpp(
            code = rcpp_code, env = compile_sys_env, verbose = FALSE)
    }
    
    new("ODESystem", odesys = odesys,
        cpp_source = rcpp_code, compile_env = compile_sys_env)
}


#' @param object A ODESystem S4 object.
#' @param init A numeric vector specifying the initial conditions. It can
#'     be named with the variable names or it can be unnamed but in the
#'     same of order of equations.
#' @param duration,step_size,start Passed to the function generated by
#'     `odeintr::compile_sys`.
#' @return `predict` returns a dataframe.
#' @rdname ODESystem
#' @export
setMethod("predict", c(object = "ODESystem"),
    function(object, init, duration, step_size = 1, start = 0) {
        x_list <- lapply(object@odesys, function(x) x@x)
        x_names <- vapply(x_list, as.character, character(1))
        
        stopifnot(length(init) == length(x_list))
        
        # Reorder 'init' if it is named
        if (!is.null(names(init))) {
            matching <- match(x_names, names(init))
            if (anyDuplicated(matching) || any(is.na(matching)))
                stop("Invalid names specified 'init'")
            init <- init[matching]
        }
        
        ans <- object@compile_env$predict(init, duration, step_size, start)
        stopifnot(
            identical(names(ans), c("Time", paste0("X", seq_along(x_list))))
        )
        names(ans)[2:length(ans)] <- x_names
        ans
    }
)

setMethod("show", c(object = "ODESystem"),
    function(object) {
        x_list <- lapply(object@odesys, function(x) x@x)
        x_names <- vapply(x_list, as.character, character(1))
        rhs_list <- lapply(object@odesys, function(x) x@rhs)
        cat(sprintf('%s with variables %s:\n', class(object),
                    paste(x_names, collapse = ", ")))
        for (i in seq_along(x_list)) {
            cat(sprintf("  d(%s)/dt = %s\n", x_names[i], as.character(rhs_list[[i]])))
        }
    }
)
