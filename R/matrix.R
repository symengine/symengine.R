
#' @include classes.R
NULL

## Maybe thanks to ALTREP, this does not actually allocate the memory
## even if the size is very large.
## microbenchmark::microbenchmark(shadow_matrix(c(10,1)),
##                                shadow_matrix(c(100000,1)))
index_matrix <- function(dim) {
    ans <- seq_len(prod(dim))
    dim(ans) <- dim
    ans
}

row_index_matrix <- function(dim) {
    nrow <- dim[1]
    ncol <- dim[2]
    ans <- rep(seq_len(nrow), ncol)
    dim(ans) <- dim
    ans
}

col_index_matrix <- function(dim) {
    nrow <- dim[1]
    ncol <- dim[2]
    ans <- rep(seq_len(ncol), each = nrow)
    dim(ans) <- dim
    ans
}

#' DenseMatrix Constructor
#' 
#' This function constructs a symbolic matrix (\code{DenseMatrix} S4 object)
#' with a similar interface with R's \code{matrix} function.
#' 
#' @param data A R object.
#' @param nrow,ncol Number of rows and columns.
#' @param byrow Boolean value. Whether the data should be filled by row or by column.
#' 
#' @return \code{DenseMatrix} S4 object.
#' @export
Matrix <- function(data, nrow = 1L, ncol = 1L, byrow = FALSE) {
    ## Return directly if it is alreay a DenseMatrix
    if (s4DenseMat_check(data)) {
        dim_data <- dim(data)
        ## Check nrow and ncol are correct
        if (((!missing(nrow)) && nrow != dim_data[1L]) ||
            ((!missing(ncol)) && ncol != dim_data[2L]))
            stop("Incorrect dimension for DenseMatrix input")
        if (!missing(byrow))
            warning("byrow argument is ignored")
        return(data)
    }
    if (is.matrix(data)) {
        dim_data <- dim(data)
        if (((!missing(nrow)) && nrow != dim_data[1L]) ||
            ((!missing(ncol)) && ncol != dim_data[2L]))
            stop("Incorrect dimension for DenseMatrix input")
        if (!missing(byrow))
            warning("byrow argument is ignored")
        data <- as.vector(data) ## i.e. remove dim attributes
        ## i.e. byrow = FALSE
        return(t(s4DenseMat_byrow(Vector(data), dim_data[2], dim_data[1])))
    }
    
    ## Fix nrow and ncol in case of missing
    if (missing(nrow) && missing(ncol))
        nrow <- length(data)
    else if (missing(nrow))
        nrow <- (length(data) - 1L) %/% ncol + 1L
    else if (missing(ncol))
        ncol <- (length(data) - 1L) %/% nrow + 1L
    
    nrow <- as.integer(nrow)
    ncol <- as.integer(ncol)
    
    ## Shortcut to Basic
    if (s4basic_check(data))
        ## 'byrow' argument now doesn't matter
        return(s4DenseMat_byrow(data, nrow = nrow, ncol = ncol))
    
    ## Support list or vector
    if (is.vector(data)) {
        data <- as(data, "VecBasic")
    }
    if (is.language(data)) {
        if (is.symbol(data)) {
            data <- paste0(as.character(data), "_", seq(nrow))
            data <- paste0(data, "_", rep(seq(nrow), each = nrow))
            data <- lapply(data, Symbol)
            data <- Vector(data)
        }
        else if (identical(data[[1]], as.name("~"))) {
            stop("TODO: if formula")
        }
    }
    
    ## Expand the data to the same size of (nrow * ncol)
    if (length(data) < (nrow*ncol)) {
        if (((nrow*ncol) %% length(data)) != 0L)
            stop(sprintf("Matrix size [%s] does not fit with the data length [%s]",
                         nrow*ncol, length(data)))
        data <- data[rep_len(seq(length(data)), length.out = nrow*ncol)]
    }
    stopifnot(length(data) == (nrow*ncol))
    
    ## We just fill it by row and transpose.
    ## An alternative approach is to fill the index into a matrix by row,
    ## then extract the transformed index with as.vector
    if (!byrow) {
        ans <- s4DenseMat_byrow(data, nrow = ncol, ncol = nrow)
        return(t(ans))
    }
    s4DenseMat_byrow(data, nrow = nrow, ncol = ncol)
}

#### Bindings for subset, etc. =======================

#' Methods Related to DenseMatrix
#' 
#' These are miscellaneous S3/S4 methods defined for \code{DenseMatrix} class.
#' 
#' @param x A DenseMatrix object.
#' @param i,j,value,...,drop Arguments for subsetting, assignment or replacing.
#' 
#' @return Same or similar with the generics of these methods.
#' 
#' @rdname densematrix-bindings
#' @export
as.matrix.DenseMatrix <- function(x, ...) {
    ## TODO: check whether S4 class inheritance works with S3 method
    if (!missing(...))
        warning("Extra arguments are ignored")
    array(as.vector(x), dim = dim(x))
}

#' @rdname densematrix-bindings
setMethod("dim", "DenseMatrix",
    function (x) s4DenseMat_dim(x)
)

setGeneric("dim<-")

## TODO: add test case
#' @rdname densematrix-bindings
setMethod(`dim<-`, c(x = "DenseMatrix"), function(x, value) {
    ndim <- length(value)
    if (ndim > 2L) stop("Higher dimensions (> 2L) are not supported")
    if (ndim == 0L) {
        if (length(x) == 1L)
            return(as(x, "Basic"))
        else
            return(as(x, "VecBasic"))
    }
    if (ndim == 1L) { ## as VecBasic
        if (length(x) != value[1L])
            stop("Dimension does not match with length of object")
        return(as(x, "VecBasic"))
    }
    
    dummy_idx <- index_matrix(dim(x))
    dim(dummy_idx) <- value
    dummy_idx <- as.integer(t(dummy_idx))
    
    ans <- s4binding_subset(x, dummy_idx, FALSE)
    s4DenseMat_byrow(ans, value[[1]], value[[2]])
})

#' @rdname densematrix-bindings
setMethod("dim<-", c(x = "VecBasic"), function(x, value) {
    ndim <- length(value)
    if (ndim > 2L) stop("Higher dimensions (> 2L) are not supported")
    if (ndim == 0L) return(x) ## i.e. NULL
    if (ndim == 1L) {
        if (length(x) != value)
            stop("Dimension does not match with length of object")
        return(x)
    }
    ## Okay, 2d case
    Matrix(x, nrow = value[1L], ncol = value[2L])
})

#' @rdname densematrix-bindings
setMethod("dim<-", c(x = "Basic"), function(x, value) {
    if (prod(value) != 1L)
        stop("Dimension does not match with length of object")
    ndim <- length(value)
    if (ndim > 2L)
        stop("Higher dimensions (> 2L) are not supported")
    if (ndim == 0L || ndim == 1L)
        return(x)
    Matrix(x, nrow = 1L, ncol = 1L)
})

#' @rdname densematrix-bindings
setMethod("dimnames<-", c(x = "DenseMatrix"), function(x, value) {
    ## TODO:
    ## cbind and rbind has default deparse.level = 1, hence they
    ## want to add dimnames to the Matrix.
    ## Currently just store them as S3 attributes.. But it should be
    ## stroed as slot when the dimname subsetting is supported later.
    attr(x, "dummy.dimnames") <- value
    x
})

#' @rdname densematrix-bindings
setMethod("dimnames", c(x = "DenseMatrix"), function(x) {
    ## TODO: store dimnames in slot
    attr(x, "dummy.dimnames", exact = TRUE)
})

#' @rdname densematrix-bindings
setMethod("length", "DenseMatrix",
    function(x) prod(s4DenseMat_dim(x))
)

setMethod("show", "DenseMatrix",
    function (object) {
        ## TODO: The printing method should be implemented more properly
        mat_dim = dim(object)
        cat(sprintf("DenseMatrix of dim %d x %d\n", mat_dim[1], mat_dim[2]))
        str <- s4DenseMat_str(object)
        #str <- gsub(x = str, pattern = "\n[", replacement = ",\n  V[", fixed = TRUE)
        #str <- sprintf("M[\n  V%s]\n", str)
        cat(str)
    }
)

#' @rdname densematrix-bindings
setMethod("[[", c(x = "DenseMatrix"),
    function(x, i, j, ...) {
        if (!missing(...))
            warning("Extra arguments are ignored");
        
        if (missing(j)) {
            i <- normalizeSingleBracketSubscript(i, seq_len(length(x)))
            if (length(i) != 1L)
                stop("Attempt to select more/less than one element")
            ## Select as if it is `as(x, "VecBasic)`
            return(s4vecbasic_get(x, i))
        }
        
        size <- length(i) * length(j)
        if (size != 1L)
            stop("Attempt to select more/less than one element")
        
        s4DenseMat_get(x, as.integer(i), as.integer(j), get_basic = TRUE)
    }
)

#' @rdname densematrix-bindings
setMethod("[[<-", c(x = "DenseMatrix"),
    function(x, i, j, ..., value) {
        if (!missing(...))
            warning("Extra arguments in dots are ignored")
        
        ## TODO: support the case when j is missing
        ans <- s4DenseMat_copy(x)
        s4DenseMat_mut_setbasic(ans, as.integer(i), as.integer(j), value)
        ans
    }
)

## TODO: maybe it is better to split this method into multiple methods by its index type
## e.g. see library(Matrix); showMethods("[")
#' @rdname densematrix-bindings
setMethod("[", c(x = "DenseMatrix"),
    function(x, i, j, ..., drop = TRUE) {
        ## NOTE that the function signature must be same as the generic function,
        ## i.e. keeping ... and drop. Otherwise the function will be wrapped in a
        ## ".local" and we can no longer use "nargs()" to determine implicit
        ## missing and "missing(drop)" will always be true.
        ## > findMethod("[", c(x = "DenseMatrix"))
        
        if (!missing(...))
            stop("Incorrect number of dimensions")
        
        n_real_args <- nargs() - !missing(drop)
        
        if (n_real_args <= 1L)
            stop("Unexpected")
        
        if (n_real_args == 2L) {
            ## i.e. x[]
            if (missing(i)) {
                if (!missing(drop))
                    warning("drop argument is ignored")
                return(x)
            }
            ## i.e. x[i]
            else {
                ## Subset by matrix
                if (is.matrix(i)) {
                    stopifnot(ncol(i) != 2L)
                    if (!missing(drop))
                        warning("drop argument is ignored for matrix index")
                    return(s4DenseMat_get(x, i[,1], i[,2], get_basic = FALSE))
                }
                ## Should we support this??
                if (!missing(drop))
                    warning("drop argument is ignored")
                i <- normalizeSingleBracketSubscript(i, seq_len(length(x)))
                ## Should be same to as(x,"VecBasic")[i]
                return(s4binding_subset(x, i, FALSE))
            }
        }
        
        ## Use i and j to subset
        ## i.e. x[i, j], x[i, ] or x[, j]
        x_shape <- dim(x)
        
        ## Also handles missing i or j
        i <- normalizeSingleBracketSubscript(i, seq_len(x_shape[1]))
        j <- normalizeSingleBracketSubscript(j, seq_len(x_shape[2]))
        
        ## Return basic
        if (drop && (length(i) * length(j)) == 1L)
            return(s4DenseMat_get(x, i, j, get_basic = TRUE))
        
        ## Extract the values as a VecBasic, then construct the new matrix
        row_idx <- rep(i, length(j))
        col_idx <- rep(j, each = length(i))
        values <- s4DenseMat_get(x, row_idx, col_idx, get_basic = FALSE)
        
        ## Return VecBasic
        if (drop && (length(i) == 1L || length(j) == 1L))
            return(values)
        
        ## TODO: Improve the performance of Matrix creatation by column,
        ##       or extract the values by row.
        ## TODO: add test case
        Matrix(values, nrow = length(i), ncol = length(j), byrow = FALSE)
    }
)

## TODO: add test case then right hand side is a matrix or DenseMatrix
#' @rdname densematrix-bindings
setMethod("[<-", c(x = "DenseMatrix"),
    function(x, i, j, ..., value) {
        if (!missing(...))
            stop("Incorrect number of dimensions")
        
        n_real_args <- nargs()
        
        if (n_real_args <= 2L)
            stop("Unexpected")
        
        # x[i] <- value or x[] <- value
        if (n_real_args == 3L) {
            ## i.e. x[] <- value
            if (missing(i)) {
                ## We need to handle this case so that
                ## `m[] <- lapply(m, function(x) func(x))` can work.
                stop("TODO: x[] <- value")
            }
            ## i.e. x[i] <- value, i can be a matrix or a index
            else {
                ## TODO: Also support using matrix as index
                stop("TODO")
            }
        }
        
        ## Using both i and j
        ## i.e. x[i, j] <- value
        x_shape <- dim(x)
        i <- normalizeSingleBracketSubscript(i, seq.int(x_shape[1]))
        j <- normalizeSingleBracketSubscript(j, seq.int(x_shape[2]))
        replacement_size <- length(i) * length(j)
        row_idx <- rep(i, length(j))
        col_idx <- rep(j, each = length(i))
        
        ## Shortcut to assign by Basic
        if (length(value) == 1L) {
            value <- as(value, "Basic")
            ans <- s4DenseMat_copy(x)
            for (each in seq_along(row_idx))
                s4DenseMat_mut_setbasic(ans, row_idx[each], col_idx[each], value)
            return(ans)
        }
        
        ## Assign with a VecBasic
        value <- as(value, "VecBasic")
        
        ## Perhaps we should force length(value) == 1L or length(value) == replacement_size ?
        if (replacement_size %% length(value) != 0L) {
            warning("Number of values supplied is not a sub-multiple of the ",
                    "number of values to be replaced")
        }
        ## Recycle the value to be the same size as replacement_size
        sync_value_idx <- rep(seq_len(length(value)), length.out = replacement_size)
        
        ans <- s4DenseMat_copy(x)
        for (each in seq(replacement_size)) {
            s4DenseMat_mut_setbasic(
                ans, row_idx[each], col_idx[each],
                s4vecbasic_get(value, sync_value_idx[each])
            )
        }
        return(ans)
    }
)

####======= Methods for cbind and rbind ===========================
#### Read the Details section of `?cbind` and `?cbind2`.

#' Joining DenseMatrix
#' 
#' S3 methods of \code{cbind} and \code{rbind} defined for
#' \code{DenseMatrix} and \code{VecBasic}.
#' 
#' @param ... DenseMatrix, VecBasic or R objects.
#' @param deparse.level Not used.
#' @method cbind SymEngineDataType
#' 
#' @return \code{DenseMatrix} S4 object.
#' 
#' @rdname cbind
#' @export
cbind.SymEngineDataType <- function(..., deparse.level) {
    ## TODO: support deparse.level argument? i.e. support dimnames for DenseMatrix
    if (!(missing(deparse.level) || deparse.level == 0L))
        warning("deparse.level argument is not supported")
    cbind_asDenseMatrix(...)
}
#' @method rbind SymEngineDataType
#' @rdname cbind
#' @export
rbind.SymEngineDataType <- function(..., deparse.level) {
    if (!(missing(deparse.level) || deparse.level == 0L))
        warning("deparse.level argument is not supported")
    rbind_asDenseMatrix(...)
}

cbind_asDenseMatrix <- function(...) {
    ## Input can be R vector, R matrix, Basic, VecBasic, DenseMatrix
    elements <- list(...)
    element_correct_nrows <- vapply(elements, FUN.VALUE = integer(1),
        function(x) if (length(d <- dim(x)) == 2L) d[1L] else length(x)
    )
    nrow <- max(element_correct_nrows)
    ans <- s4DenseMat_byrow(NULL, nrow = nrow, ncol = 0L)
    
    ## Convert each element to DenseMatrix and join them
    for (i in seq_along(elements)) {
        el <- elements[[i]]
        if (length(dim(el)) == 2L) #if (s4DenseMat_check(el) || is.matrix(el))
            s4DenseMat_mut_addcols(ans, Matrix(el, nrow = nrow))
        ## Then assume it is a one-dim vector or vecbasic
        else {
            ## Recycle the value if necessary
            if (element_correct_nrows[i] != nrow) {
                if ((nrow %% length(el)) != 0L)
                    warning("Number of rows of result is not a multiple of vector length")
                el <- rep_len(el, length.out = nrow)
            }
            el <- Matrix(el, nrow = nrow, ncol = 1L)
            s4DenseMat_mut_addcols(ans, el)
        }
    }
    ans
}
rbind_asDenseMatrix <- function(...) {
    elements <- list(...)
    element_correct_ncols <- vapply(elements, FUN.VALUE = integer(1),
        function(x) if (length(d <- dim(x)) == 2L) d[2L] else length(x)
    )
    ncol <- max(element_correct_ncols)
    ans <- s4DenseMat_byrow(NULL, nrow = 0L, ncol = ncol)
    for (i in seq_along(elements)) {
        el <- elements[[i]]
        if (length(dim(el)) == 2L)
            s4DenseMat_mut_addrows(ans, Matrix(el, ncol = ncol))
        else {
            if (element_correct_ncols[i] != ncol) {
                if ((ncol %% length(el)) != 0L)
                    warning("Number of cols of result is not a multiple of vector lengths")
                el <- rep_len(el, length.out = ncol)
            }
            el <- Matrix(el, nrow = 1L, ncol = ncol)
            s4DenseMat_mut_addrows(ans, el)
        }
    }
    ans
}

#' Transpose (as) a DenseMatrix
#' 
#' S4 methods of \code{t} defined for \code{Basic}, \code{VecBasic}
#' and \code{DenseMatrix}.
#' 
#' @param x A SymEngine object.
#' 
#' @return A \code{DenseMatrix} S4 object.
#' 
#' @rdname t
#' @exportMethod t
setGeneric("t")

#' @rdname t
setMethod("t", c(x = "Basic"),
    function(x) Matrix(x)
)
#' @rdname t
setMethod("t", c(x = "VecBasic"),
    function(x) s4DenseMat_byrow(x, nrow = 1L, ncol = length(x))
)
#' @rdname t
setMethod("t", "DenseMatrix",
    function(x) s4DenseMat_transpose(x)
)


####======= Determinant ===========================================

#' Calculate the Determinant of DenseMatrix
#' 
#' S4 method of \code{det} defined for \code{DenseMatrix}.
#' 
#' @param x A DenseMatrix object.
#' @param ... Unused.
#' 
#' @return A \code{Basic} object.
#' 
#' @rdname det
#' @exportMethod det
#' @examples
#' mat <- Matrix(LETTERS[1:9], 3)
#' det(mat)
setGeneric("det")

#' @rdname det
setMethod("det", c(x = "DenseMatrix"),
    function(x, ...) {
        if (!missing(...)) 
            warning("Extra arguments are ignored")
        s4DenseMat_det(x)
    }
)


####===============================================================

## TODO: LU decomposition, etc.




