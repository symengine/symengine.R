
setClass("DenseMatrix", contains = "externalptr")

#' @export
denseMatrix <- function(data = NA, nrow = 1, ncol = 1) {
    vec <- do.call(vecbasic, as.list(data))
    n   <- length(vec)

    if (n == 0) {
        stop("'data' must be of a vecbasic type, was 'NULL'")
    }

    if (nrow < 0) {
        stop("invalid 'nrow' value (< 0)")
    } else if (ncol < 0) {
        stop("invalid 'ncol' value (< 0)")
    }

    if (missing(nrow) && missing(ncol)) {
        nrow = n
        ncol = 1
    } else if (missing(nrow)) {
        nrow = (n - 1) %/% ncol + 1
    } else if (missing(ncol)) {
        ncol = (n - 1) %/% nrow + 1
    }

    if (nrow %% n != 0 && ncol %% n != 0) {
        if (nrow < n && n %% nrow != 0 ||
            nrow > n && nrow %% n != 0) {
            warning(sprintf(paste("data length [%d] is not a sub-multiple or",
                    "multiple of the number of rows [%d]"), n, nrow))
        } else if (ncol < n && n %% ncol != 0 ||
                  ncol > n && ncol %% n != 0) {
            warning(sprintf(paste("data length [%d] is not a sub-multiple or",
                    "multiple of the number of columns [%d]"), n, ncol))
        }
    }

    .denseMatrix(vec, nrow, ncol)
}

denseMatrix_get <- function(mat, i, j) {
    # i, j can only be integer vector
    .denseMatrix_get(mat, i, j)
}

denseMatrix_subset <- function(mat, idxr, idxc) {
    # idx can only be integer vector
    .denseMatrix_subset(mat, idxr, idxc)
}

denseMatrix_assign <- function(mat, idxr, idxc, vec) {
    # idx can only be integer vector
    .denseMatrix_assign(mat, idxr, idxc, vec)
}

denseMatrix_to_vecbasic <- function(mat) {
    .denseMatrix_to_vecbasic(mat)
}


setMethod("show", "DenseMatrix",
    function (object) {
        mat_dim = dim(object)
        cat(sprintf("DenseMatrix of dim %d x %d\n", mat_dim[1], mat_dim[2]))
        str <- .denseMatrix_str(object)
        cat(str)
    }
)

setMethod("dim", "DenseMatrix",
    function (x) {
        nrows = .denseMatrix_rows(x)
        ncols = .denseMatrix_cols(x)
        c(nrows, ncols)
    }
)

setMethod("length", "DenseMatrix",
    function(x) {
        nrows = .denseMatrix_rows(x)
        ncols = .denseMatrix_cols(x)
        nrows * ncols
    }
)

setMethod("[[", c(x = "DenseMatrix", i = "numeric", j = "numeric"),
    function(x, i, j, ...) {
        #TODO: normalize the index
        if (!missing(...))
            warning("Extra arguments are ignored");
        if (length(i) == 0) {
            stop("attempt to select less than one element in get1index <real>");
        } else if (length(i) > 1) {
            stop("attempt to select more than one element in get1index");
        } else if (length(j) == 0) {
            stop("attempt to select less than one element in get1index <real>");
        } else if (length(j) > 1) {
            stop("attempt to select more than one element in get1index");
        }

        denseMatrix_get(x, as.integer(i), as.integer(j))
    }
)

setMethod("[", c(x = "DenseMatrix"),
    function(x, i, j, ..., drop = TRUE) {
        if (!missing(...))
            stop("incorrect number of dimensions")
        if (!missing(drop))
            warning("Supplied argument 'drop' is ignored")
        
        # use NROW, NCOL to work around for now.
        i <- normalizeSingleBracketSubscript(i, 1:NROW(x))
        j <- normalizeSingleBracketSubscript(j, 1:NCOL(x))
        denseMatrix_subset(x, i, j)
    }
)

setMethod("[<-", c(x = "DenseMatrix"),
    function(x, i, j, ..., value) {
        if (!missing(...))
            stop("Invalid subsetting")
        if (is(value, "Basic"))
            value <- vecbasic(value)
        if (is(value, "DenseMatrix"))
            value <- denseMatrix_to_vecbasic(value)
        # use NROW, NCOL to work around for now.
        i <- normalizeSingleBracketSubscript(i, 1:NROW(x))
        j <- normalizeSingleBracketSubscript(j, 1:NCOL(x))

        li <- length(i)
        lj <- length(j)
        lv <- length(value)
        
        if (li == 0L || lj == 0L)
            return(x)
        if (lv == 0L)
            stop("Replacement has length zero")
        if ((li * lj) %% lv != 0L)
            stop("number of items to replace is not a multiple of replacement length")
          
        denseMatrix_assign(x, i, j, value)
    }
)