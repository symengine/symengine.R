
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
    .denseMatrix_get(mat, i, j)
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

setMethod("[[", c(x = "DenseMatrix", i = "numeric", j = "numeric"),
    function(x, i, j, ...) {
        #TODO: normalize the index
        if (!missing(...))
            warning("Extra arguments are ignored")
        denseMatrix_get(x, as.integer(i), as.integer(j))
    }
)