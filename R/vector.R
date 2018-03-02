#' @include RcppExports.R basic.R
NULL

## Vector    ===================================================================
setClass("VecBasic", contains = "externalptr")

vecbasic_new <- function() {
    new("VecBasic", .vecbasic_new())
}

vecbasic_push_back <- function(vec, val) {
    .vecbasic_push_back(vec@.xData, val@.xData)
    vec
}

vecbasic_get <- function(vec, n) {
    res <- S("tmp")
    .vecbasic_get(vec@.xData, n, res@.xData)
    res
}

vecbasic_set <- function(vec, n, val) {
    .vecbasic_set(vec@.xData, n, val@.xData)
    vec
}

vecbasic_size <- function(vec) {
    .vecbasic_size(vec@.xData)
}

vecbasic_erase <- function(vec, n) {
    .vecbasic_erase(vec@.xData, n)
    vec
}

basic_max <- function(vec) {
    res <- S("tmp")
    .basic_max(res@.xData, vec@.xData)
    res
}

basic_min <- function(vec) {
    res <- S("tmp")
    .basic_min(res@.xData, vec@.xData)
    res
}

setMethod("max", "VecBasic",
    function (x) {
        ptr <- as(x, "externalptr")
        basic_max(x)
    }
)

setMethod("min", "VecBasic",
    function (x) {
        ptr <- as(x, "externalptr")
        basic_min(x)
    }
)

setMethod("show", "VecBasic",
    function (object) {
        ptr <- as(object, "VecBasic")
        n <- vecbasic_size(ptr)
        til = min(10, n)
        for (id in 1:til) {
          obj <- vecbasic_get(ptr, id)
          show(obj)
        }
        if (til < n) {
          cat(n - til, "Symbols not printed.")
        }
        invisible()
    }
)

setMethod("length", "VecBasic",
    function(x) {
        vecbasic_size(x)
    }  
)

basic_vector <- function(...) {
    objs = unlist(...)
    n    = length(objs)
    vec  = vecbasic_new()
    if (n != 0) {
        for (i in 1:n) {
            vecbasic_push_back(vec, objs[[i]])
        }
    }
    vec
}