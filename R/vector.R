#' @include RcppExports.R basic.R
NULL

## Vector    ===================================================================
setClass("VecBasic", contains = "externalptr")

#' @export
vecbasic <- function(...) {
    # Currently we do conversion in this way, however we should in future have
    # C-level function for conversion to basic to reduce overhead.
    lt <- lapply(unname(list(...)), function(x) {
        if (is(x, "Basic") || is(x, "VecBasic"))
            return(x@.xData)
        else
            return(S(x)@.xData)
    })
    new("VecBasic", .vecbasic(lt))
}

vecbasic_subset <- function(vec, idx) {
    # idx can only be integer vector
    new("VecBasic", .vecbasic_subset(vec@.xData, idx));
}

vecbasic_get <- function(vec, n) {
    # n can only be integer
    new("Basic", .vecbasic_get(vec@.xData, n))
}

setMethods("c", list(c(x = "VecBasic"), c(x = "Basic")),
    function (x, ...) {
        vecbasic(x, ...)
    }
)

setMethod("length", "VecBasic",
    function(x) {
        .vecbasic_length(x@.xData)
    }
)

setMethod("show", "VecBasic",
    function (object) {
        cat(sprintf("VecBasic of length %s\n", length(object)))
        
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

setMethod("[[", c(x = "VecBasic", i = "numeric", j = "ANY"),
    function(x, i, j, ...) {
        # TODO: normalize the index
        if (!missing(...))
            warning("Extra arguments are ignored")
        if (!missing(j))
            stop("incorrect number of dimensions")
        vecbasic_get(x, as.integer(i))
    }
)

setMethod("[", c(x = "VecBasic"),
    function(x, i, j, ..., drop = TRUE) {
        if (!missing(...))
            warning("Extra arguments are ignored")
        if (!missing(drop))
            warning("Supplied argument 'drop' is ignored")
        if (!missing(j))
            stop("incorrect number of dimensions")
        if (missing(i))
            return(x)
        i <- normalizeSingleBracketSubscript(i, x)
        vecbasic_subset(x, i)
    }
)

#' @export
setGeneric("as.list")

setMethod("as.list", c(x = "VecBasic"),
    function(x) {
        ans <- vector("list", length(x))
        for (i in seq_along(ans))
            ans[[i]] <- x[[i]]
        ans
    }
)

