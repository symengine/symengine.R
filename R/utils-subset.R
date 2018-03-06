# Light implementation of S4Vectors::normalizeSingleBracketSubscript

normalizeSingleBracketSubscript <- function (i, x) {
    .by.numeric <- function (i, x) {
        x_NROW <- NROW(x)
        i <- as.integer(i)
        if (anyNA(i))
            stop("Subscript contains NAs")
        i_max <- max(i)
        i_min <- min(i)
        if (i_max > x_NROW)
            stop("Subscript contains out-of-bounds indices")
        
        if (i_min < 0L) {
            if (i_max > 0L)
                stop("Only 0's may be mixed with negative subscripts")
            # Translate to positive indices
            i <- seq_len(x_NROW)[i]
        }
        else {
            # Remove 0 from subscript
            zero_idx <- which(i == 0L)
            if (length(zero_idx))
                i <- i[-zero_idx]
        }
        return(i)
    }
    .by.logical <- function (i, x) {
        x_NROW <- NROW(x)
        if (anyNA(i))
            stop("Logical subscript contains NAs")
        if (length(i) > x_NROW) {
            if (any(i[(x_NROW+1L):length(i)]))
                stop("Subscript is a logical vector with out-of-bounds TRUE values")
            i <- i[seq_len(x_NROW)]
        }
        if (length(i) < x_NROW)
            # Recycle logical indices if necessary
            i <- rep(i, length.out = x_NROW)
        i <- which(i)
        return(i)
    }
    
    if (is.numeric(i))
        return(.by.numeric(i, x))
    if (is.logical(i))
        return(.by.logical(i, x))
    
    stop(sprintf("Not implemented for %s", class(i)))
}


