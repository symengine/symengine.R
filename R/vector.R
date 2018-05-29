#' @include RcppExports.R basic.R
NULL

## Vector    ===================================================================
setClass("VecBasic", contains = "externalptr")

#' @export
vecbasic <- function(...) {
    # Currently we do conversion in this way, however we should in future have
    # C-level function for conversion to basic to reduce overhead.
    # x can not be vector like 1:10
    lt <- lapply(unname(list(...)), function(x) {
        if (is(x, "Basic") || is(x, "VecBasic"))
            return(x)
        else if (is(x, "DenseMatrix"))
            return(denseMatrix_to_vecbasic(x))
        else if (is(x, "SetBasic"))
            return(setbasic_to_vecbasic(x))
        else
            return(S(x))
    })
    .vecbasic(lt)
}

vecbasic_subset <- function(vec, idx) {
    # idx can only be integer vector
    .vecbasic_subset(vec, idx)
}

vecbasic_get <- function(vec, n) {
    # n can only be integer
    .vecbasic_get(vec, n)
}

vecbasic_assign <- function(vec1, idx, vec2) {
    # idx can only be integer vector
    .vecbasic_assign(vec1, idx, vec2)
}

setMethods("c", list(c(x = "VecBasic"), c(x = "Basic")),
    function (x, ...) {
        vecbasic(x, ...)
    }
)

setMethod("length", "VecBasic",
    function(x) {
        .vecbasic_length(x)
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
        
        i <- normalizeSingleBracketSubscript(i, x)
        vecbasic_subset(x, i)
    }
)

setMethod("[<-", c(x = "VecBasic"), 
    function (x, i, j, ..., value)  {
        if (!missing(j) || !missing(...))
            stop("Invalid subsetting")
        if (is(value, "Basic"))
            value <- vecbasic(value)
        i <- normalizeSingleBracketSubscript(i, x)
        
        li <- length(i)
        lv <- NROW(value)
        
        ## There are different results when missing `i` and length(i) == 0
        ## i.e. a[c()] <- 42, a[] <- 42
        if (li == 0L)
            return(x)
        if (lv == 0L)
            stop("Replacement has length zero")
        if (li != lv) {
            # Recycle of value when length(i) != length(value)
            if (li%%lv != 0L)
                warning("Number of values supplied is not a sub-multiple of the ",
                        "number of values to be replaced")
            # TODO: Currently we expand the value in R, however to be more
            # efficient, we should handle the recycle in C.
            value <- value[rep(seq_len(lv), length.out = li)]
        }
        vecbasic_assign(x, i, value)
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

