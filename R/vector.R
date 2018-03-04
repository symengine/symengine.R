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
        if (n == 0) {
            cat("Empty vector.")
        } else {
            til = min(10, n)
            for (id in 1:til) {
                obj <- vecbasic_get(ptr, id)
                show(obj)
            }
            if (til < n) {
                cat(n - til, "Symbols not printed.")
            }
        }
        invisible()
    }
)

setMethod("length", "VecBasic",
    function(x) {
        vecbasic_size(x)
    }
)

vecbasic_concentrate <- function(...) {
    lt = list(...)
    for (i in 1:length(lt)) {
        lt[[i]] = as(lt[[i]], "externalptr")
    }
    new("VecBasic", .vecbasic_concentrate(lt))
}

vecbasic_get <- function(vec, n) {
    new("Basic", .vecbasic_get(as(vec, "externalptr"), n))
}

# For now, the index should be valid.
# If indexs are all zero, it return empty vector.
vecbasic_subset <- function(vec, idx) {
    idx <- as.integer(idx)
    len_idx <- length(idx)
    len_vec <- length(vec)
    mi = min(idx)
    ma = max(idx)
    valid_idx <- as.integer(1:len_vec)
    if (mi == 0 && ma == 0) {
        stop("not implement")
    } else if (mi < 0 && ma > 0) {
        stop("only 0's may be mixed with negative subscripts")
    } else if (mi < 0) {
        valid_idx <- setdiff(valid_idx, -idx)
    } else if (ma > 0) {
        if (ma > len_vec) {
            stop("not implement")
        }
        valid_idx <- idx
    }
    new("VecBasic", .vecbasic_subset(as(vec, "externalptr"), valid_idx));
}

setMethod("[[", c(x = "VecBasic", i = "numeric", j = "ANY"),
    function(x, i, j, ...) {
        if (!missing(j)) {
            stop("incorrect number of dimensions")
        }
        vecbasic_get(x, i)
    }
)

setMethod("[", c(x = "VecBasic", i = "numeric", j = "ANY", drop = "ANY"),
    function (x, i, j, ..., drop = TRUE) {
        if (!missing(j)) {
            stop("incorrect number of dimensions")
        }
        vecbasic_subset(x, i)
    }
)

