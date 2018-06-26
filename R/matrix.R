#' @include vector.R
setClass("DenseMatrix", contains = "externalptr")

#' @export
denseMatrix <- function(data = NA, nrow = 1, ncol = 1, row_first = 0) {
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

    .denseMatrix(vec, nrow, ncol, as.integer(row_first))
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

denseMatrix_to_vecbasic <- function(mat, row_first=0) {
    .denseMatrix_to_vecbasic(mat, as.integer(row_first))
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
        args <- as.list(sys.call())[-1L]
        args$drop <- NULL
        len <- length(args)

        if (len > 3)
            stop("incorrect number of dimensions")
        if (!missing(drop))
            warning("Supplied argument 'drop' is ignored")
        
        if (len > 2) {
            i <- args[[2]]
            j <- args[[3]]
            i <- if(missing(i)) 1:NROW(x) else eval(i)
            j <- if(missing(j)) 1:NCOL(x) else eval(j)
        }

        # use NROW, NCOL to work around for now.
        if (missing(i) && missing(j)) {
            x
        }
        else if (missing(j)) {
            vec <- denseMatrix_to_vecbasic(x)
            vec[i]
        } else {
            i <- normalizeSingleBracketSubscript(i, 1:NROW(x))
            j <- normalizeSingleBracketSubscript(j, 1:NCOL(x))
            denseMatrix_subset(x, i, j)
        }
    }
)

setMethod("[<-", c(x = "DenseMatrix"),
    function(x, i, j, ..., value) {
        args <- as.list(sys.call())[-1L]
        len <- length(args)

        if (len > 4)
            stop("Invalid subsetting")
        if (is(value, "Basic"))
            value <- vecbasic(value)
        if (is(value, "DenseMatrix"))
            value <- denseMatrix_to_vecbasic(value)
        
        if (len > 2) {
            i <- args[[2]]
            j <- args[[3]]
            i <- if(missing(i)) 1:NROW(x) else eval(i)
            j <- if(missing(j)) 1:NCOL(x) else eval(j)
        }
            
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

# define vector-like S4 classUnion for rbind/cbind
setClassUnion("VecMat", members = c("VecBasic", "DenseMatrix"))

#' @exportMethod cbind
setGeneric("cbind", signature = "...")
setMethod("cbind", "VecMat",
    function(..., deparse.level = 1) {
        args <- list(...)
        if(all(vapply(args, is.atomic, NA)))
            return(base::cbind(..., deparse.level = deparse.level))
        
        # cbind for vecbasic, denseMatrix
        # currently, all element must be vecbasic/denseMatrix
        nrow        = 0
        ncol        = 0
        have_matrix = FALSE
        lt <- unname(list(...))

        for (i in seq_len(length(lt))) {
            x <- lt[[i]]
            if (is(x, "Basic")) {
                ncol    = ncol + 1
                nrow    = max(nrow, 1)
                lt[[i]] = vecbasic(x)
            } else if (is(x, "VecBasic")) {
                if (!have_matrix)
                    nrow = max(nrow, length(x))
                ncol = ncol + 1
            } else if (is(x, "DenseMatrix")) {
                if (have_matrix && nrow != NROW(x)) {
                    stop(sprintf("number of rows of matrices must match (see arg %d)", i))
                } else if (!have_matrix) {
                    have_matrix = TRUE
                    nrow = NROW(x)
                }
                ncol = ncol + NCOL(x)
            }
        }
        
        first_warning = TRUE
        for (i in seq_len(length(lt))) {
            x <- lt[[i]]
            if (is(x, "VecBasic")) {
                if (first_warning && (nrow %% length(x) != 0)) {
                    first_warning = FALSE
                    warning(sprintf("number of rows of result is not a multiple of vector length (arg %d)", i))
                }
                idx = rep(seq_len(length(x)), length.out=nrow)
                lt[[i]] = x[idx]
            } else if (is(x, "DenseMatrix")) {
                lt[[i]] = denseMatrix_to_vecbasic(x)
            }
        }
    
        vec <- do.call(vecbasic, lt)
        .denseMatrix(vec, nrow, ncol)
    }
)


#' @exportMethod rbind
setGeneric("rbind", signature = "...")
setMethod("rbind", "VecMat",
    function(..., deparse.level = 1) {
        args <- list(...)
        if(all(vapply(args, is.atomic, NA)))
            return(base::rbind(..., deparse.level = deparse.level))
        
        # rbind for vecbasic, denseMatrix
        # currently, all element must be vecbasic/denseMatrix
        nrow        = 0
        ncol        = 0
        have_matrix = FALSE
        lt <- unname(list(...))

        for (i in seq_len(length(lt))) {
            x <- lt[[i]]
            if (is(x, "Basic")) {
                nrow    = nrow + 1
                ncol    = max(ncol, 1)
                lt[[i]] = vecbasic(x)
            } else if (is(x, "VecBasic")) {
                if (!have_matrix)
                    ncol = max(ncol, length(x))
                nrow = nrow + 1
            } else if (is(x, "DenseMatrix")) {
                if (have_matrix && ncol != NCOL(x)) {
                    stop(sprintf("number of columns of matrices must match (see arg %d)", i))
                } else if (!have_matrix) {
                    have_matrix = TRUE
                    ncol = NCOL(x)
                }
                nrow = nrow + NROW(x)
            }
        }
        
        first_warning = TRUE
        for (i in seq_len(length(lt))) {
            x <- lt[[i]]
            if (is(x, "VecBasic")) {
                if (first_warning && (ncol %% length(x) != 0)) {
                    first_warning = FALSE
                    warning(sprintf("number of columns of result is not a multiple of vector length (arg %d)", i))
                }
                idx = rep(seq_len(length(x)), length.out=ncol)
                lt[[i]] = x[idx]
            } else if (is(x, "DenseMatrix")) {
                lt[[i]] = .denseMatrix_to_vecbasic(x, 1)
            }
        }
    
        vec <- do.call(vecbasic, lt)
        .denseMatrix(vec, nrow, ncol, 1)
    }
)

#' @exportMethod det
setGeneric("det", function(x, ...) { standardGeneric("det") })
setMethod("det", "DenseMatrix",
    function(x, ...) {
        if (is.atomic(x))
            return(base::det(x, ...))
        d = dim(x)
        if (d[[1]] != d[[2]])
            stop("'x' must be a square matrix")
        .denseMatrix_det(x)
    }
)

#' @exportMethod inv
setGeneric("inv", function(x) { standardGeneric("inv") })
setMethod("inv", "DenseMatrix",
    function(x) {
        d <- dim(x)
        if (d[[1]] != d[[2]])
            stop("'x' must be a square matrix")
        .denseMatrix_inv(x)
    }
)

#' @exportMethod t
setGeneric("t", function(x) { standardGeneric("t") })
setMethod("t", "DenseMatrix",
    function(x) {
        if (is.atomic(x))
            return(base::t(x))
        .denseMatrix_transpose(x)
    }
)

#' @exportMethod lu
setGeneric("lu", function(x) { standardGeneric("lu") })
setMethod("lu", "DenseMatrix",
    function(x) {
        # the address of l, u don't change
        l <- denseMatrix(0,0,0)
        u <- denseMatrix(0,0,0)
        .denseMatrix_LU(l,u,x)
        return(list(l=l, u=u))
    }
)