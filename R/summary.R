

#' @param na.rm Ignored
#' @rdname bindings
setMethod("sum", c("SymEngineDataType"),
    function(x, ..., na.rm = FALSE) {
        if (na.rm) warning("na.rm argument is ignored")
        input <- Vector(x, ...)
        s4vecbasic_summary(input, "sum")
    }
)

#' @rdname bindings
setMethod("prod", c("SymEngineDataType"),
    function(x, ..., na.rm = FALSE) {
        if (na.rm) warning("na.rm argument is ignored")
        input <- Vector(x, ...)
        s4vecbasic_summary(input, "prod")
    }
)
