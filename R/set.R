#' @include RcppExports.R basic.R vector.R
NULL

setClass("SetBasic", contains = "externalptr")

#' @export
setbasic <- function(...) {
    vec <- vecbasic(...)
    .setbasic(vec)
}

setbasic_get <- function(set, n) {
    # n can only be integer
    .setbasic_get(set, n)
}

setbasic_subset <- function(set, idx) {
    # idx can only be integer vector
    .setbasic_subset(set, idx)
}

setbasic_to_vecbasic <- function(set) {
    .setbasic_to_vecbasic(set)
}

setMethods("c", c(x = "SetBasic"),
    function (x, ...) {
        vec <- vecbasic(x, ...)
        .setbasic(vec)
    }
)

setMethod("length", "SetBasic",
    function(x) {
        .setbasic_length(x)
    }
)

setMethod("show", "SetBasic",
    function (object) {
        cat(sprintf("setbasic of length %s\n", length(object)))
        
        ids <- vector("character", length(object))
        str <- vector("character", length(object))
        
        for (i in seq_along(object)) {
            ids[[i]] <- paste0("[", i, "]")
            str[[i]] <- basic_str(object[[i]])
        }
        format(ids)
        format(str)
        lines <- paste(ids, str)
        cat(lines, sep = "\n")
        cat("\n")
    }
)

setMethod("[[", c(x = "SetBasic", i = "numeric", j = "ANY"),
    function(x, i, j, ...) {
        # TODO: normalize the index
        if (!missing(...))
            warning("Extra arguments are ignored")
        if (!missing(j))
            stop("incorrect number of dimensions")
        setbasic_get(x, as.integer(i))
    }
)

setMethod("[", c(x = "SetBasic"),
    function(x, i, j, ..., drop = TRUE) {
        if (!missing(...))
            warning("Extra arguments are ignored")
        if (!missing(drop))
            warning("Supplied argument 'drop' is ignored")
        if (!missing(j))
            stop("incorrect number of dimensions")
        
        i <- normalizeSingleBracketSubscript(i, x)
        setbasic_subset(x, i)
    }
)