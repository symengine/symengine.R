#' @include classes.R misc.R
NULL

## VecBasic  ===================================================================

#' VecBasic Constructors
#' 
#' @param x,... R objects.
#' 
#' @rdname Vector
#' @export
Vector <- function(x, ...) {
    ## Note that `Vector` will not check whole number, but `V` will
    
    ## Return empty vecbasic
    if (missing(x))
        return(s4vecbasic())
    
    ## Treat x as a vector
    if (missing(...))
        return(as(x, "VecBasic"))
    
    elements <- list(x, ...)
    ans <- s4vecbasic()
    s4vecbasic_mut_append(ans, elements)
    ans
}

#' @rdname Vector
#' @export
V <- function(...) {
    ## Parse each element with check_whole_number = TRUE
    elements <- list(...)
    for (i in seq_along(elements)) {
        elements[[i]] <- S(elements[[i]])
    }
    do.call(Vector, elements)
}

#' Methods Related to VecBasic
#' 
#' @param x Basic object or Vecbasic object.
#' 
#' @rdname vecbasic-bindings
setMethod("length", "VecBasic",
    function(x) s4vecbasic_size(x)
)

#' @rdname vecbasic-bindings
setMethod("rep", c(x = "VecBasic"),
    function(x, ...)
        s4binding_subset(x, rep(seq_len(length(x)), ...), get_basic = FALSE)
)
#' @rdname vecbasic-bindings
setMethod("rep", c(x = "Basic"),
    function(x, ...)
        s4binding_subset(x, rep(1L, ...), get_basic = FALSE)
)
setMethod("rep_len", c(x = "VecBasic"),
    function(x, length.out) rep(x, length.out = length.out)
)
setMethod("rep_len", c(x = "Basic"),
    function(x, length.out) rep(x, length.out = length.out)
)
setMethod("rep.int", c(x = "VecBasic"),
    function(x, times)      rep(x, times = times)
)
setMethod("rep.int", c(x = "Basic"),
    function(x, times)      rep(x, times = times)
)

## TODO: test case: c(S("x"), list(1,2,c(3,4)))
#' @rdname vecbasic-bindings
setMethod("c", c(x = "BasicOrVecBasic"),
    function (x, ...) {
        dots <- list(x, ...)
        ans <- s4vecbasic()
        for (i in seq_along(dots))
            s4vecbasic_mut_append(ans, dots[[i]])
        ans
    }
)

setMethod("show", "VecBasic",
    function (object) {
        cat(sprintf("VecBasic of length %s\n", length(object)))
        strs <- sapply(as.list(object), s4basic_str)
        out <- sprintf("V( %s )", paste0(strs, collapse = ", "))
        cat(out)
        cat("\n")
        #format()
    }
)

#' @param i,j,...,drop,value Arguments for subsetting or replacing.
#' @rdname vecbasic-bindings
setMethod("[[", c(x = "VecBasic", i = "numeric", j = "ANY"),
    function(x, i, j, ...) {
        # TODO: normalize the index
        if (!missing(...))
            warning("Extra arguments are ignored")
        if (!missing(j))
            stop("incorrect number of dimensions")
        s4vecbasic_get(x, as.integer(i))
    }
)

#' @rdname vecbasic-bindings
setMethod("[", c(x = "VecBasic"),
    function(x, i, j, ..., drop = TRUE) {
        if (!missing(...))
            warning("Extra arguments are ignored")
        if (!missing(drop))
            warning("Supplied argument 'drop' is ignored")
        if (!missing(j))
            stop("incorrect number of dimensions")
        
        i <- normalizeSingleBracketSubscript(i, x)
        s4binding_subset(x, i, FALSE)
    }
)

#' @rdname vecbasic-bindings
setMethod("[[<-", c(x = "VecBasic"),
    function(x, i, value) {
        i <- as.integer(i)
        if (i > length(x) || i <= 0)
            stop("Index out of bounds")
        if (is(value, "VecBasic")) {
            stopifnot(length(value) == 1)
            value <- value[[1]]
        }
        stopifnot(is(value, "Basic"))
        ## Copy the vecbasic
        ans <- s4vecbasic()
        s4vecbasic_mut_append(ans, x)
        s4vecbasic_mut_set(ans, i, value)
        ans
    }
)

#' @rdname vecbasic-bindings
setMethod("[<-", c(x = "VecBasic"), 
    function (x, i, j, ..., value)  {
        if (!missing(j) || !missing(...))
            stop("Invalid subsetting")
        i <- normalizeSingleBracketSubscript(i, x)
        
        if (length(value) == 1L) { ## Faster shortcut
            value <- as(value, "Basic")
            ## Copy the vecbasic
            ans <- s4vecbasic()
            s4vecbasic_mut_append(ans, x)
            for (idx in i)
                s4vecbasic_mut_set(ans, idx, value)
            return(ans)
        }
        
        ## Then do the replacement with VecBasic
        value <- as(value, "VecBasic")
        li <- length(i)
        lv <- NROW(value)
        ## There are different results when missing `i` and length(i) == 0
        ## i.e. a[c()] <- 42, a[] <- 42
        if (li == 0L)
            return(x)
        if (lv == 0L)
            stop("Replacement has length zero")
        
        ## Modify it to have the same length as "i"
        sync_value_idx <- seq_len(lv)
        if (li != lv) {
            ## Recycle the value
            if (li%%lv != 0L)
                warning("Number of values supplied is not a sub-multiple of the ",
                        "number of values to be replaced")
            sync_value_idx <- rep(sync_value_idx, length.out = li)
        }
        stopifnot(length(i) == length(sync_value_idx))
        
        ## Copy the VecBasic
        ans <- s4vecbasic()
        s4vecbasic_mut_append(ans, x)
        for (each in seq_along(i))
            ## FIXME: There is a overhead initializing the S4 Basic object for each loop
            s4vecbasic_mut_set(ans, i[[each]], s4vecbasic_get(value, sync_value_idx[[each]]))
        return(ans)
    }
)

