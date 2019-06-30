
## SymEnginePTR is the parent class holding Basic, VecBasic, etc.

setClass("SymEnginePTR", slots = c(ptr = "externalptr"))

setClass("Basic", contains = "SymEnginePTR")

setClass("VecBasic", contains = "SymEnginePTR")

setClass("DenseMatrix", contains = "SymEnginePTR")

## Class for dispatch purpose (TODO: maybe use class inheritance instead of union)
setClassUnion("SymEngineDataType", c("Basic", "VecBasic", "DenseMatrix"))

setClassUnion("BasicOrVecBasic", c("Basic", "VecBasic"))

## A context is an environment where symbols in the expression may be substituted from

setClass("SymEnginePTRWithContext", contains = "SymEnginePTR", slots = c(context = "environment"))

setClass("BasicWithContext", contains = c("Basic", "SymEnginePTRWithContext"))

setClass("VecBasicWithContext", contains = c("Basic", "SymEnginePTRWithContext"))

setClass("DenseMatrixWithContext", contains = c("Basic", "SymEnginePTRWithContext"))


#### Some conversion methods ==========================

setAs(from = "SymEnginePTR", to = "externalptr",
    function(from) from@ptr
)

#### setAs for Basic ==================================

setAs(from = "ANY", to = "Basic",
      function(from) s4basic_parse(from, check_whole_number = FALSE)
)

#' Some Conversion Methods
#' 
#' @param x The object to be converted.
#' @rdname conversion
setMethod("as.character", c(x = "Basic"),
    ## TODO: also define method for VecBasic
    function(x) s4basic_str(x)
)

#' @rdname conversion
setMethod("as.numeric", c(x = "Basic"),
    function(x) as.double(s4basic_as_sexp(x))
)

#' @rdname conversion
setMethod("as.integer", c(x = "Basic"),
    function(x) {
        if (s4basic_get_type(x) == "Integer")
            return(s4basic_as_sexp(x))
        stop("Not implemented")
    }
)

setMethod("as.vector", c(x = "Basic"),
    function(x, mode) {
        ## TODO
        stop(sprintf("mode [%s] not implemented", mode))
    }
)


#### setAs for VecBasic  ==============================

setAs("Basic", "VecBasic", function(from) {
    ans <- s4vecbasic()
    s4vecbasic_mut_append(ans, from)
    ans
})

setAs("VecBasic", "Basic", function(from) {
    stopifnot(length(from) == 1L)
    from[[1]]
})

setAs("vector", "VecBasic", function(from) {
    ans <- s4vecbasic()
    s4vecbasic_mut_append(ans, from)
    ans
})

## By defining as.vector, it automatically supports as.list, matrix, as.matrix, array, etc.
setMethod("as.vector", c(x = "VecBasic"),
    function(x, mode) {
        ## TODO: add as.vector method to Basic as well?
        if (mode == "any" || mode == "list") {
            ans <- vector("list", length(x))
            ## TODO: Improve the performance of this
            for (i in seq_along(ans))
                ans[[i]] <- s4vecbasic_get(x, i)
            return(ans)
        }
        
        ## TODO: it might be useful to convert to other modes (e.g. numeric),
        ##       if it is not possible, we can return NA and give a warning
        ##       (NA introduced by coercion)
        ## Other modes: logical, integer, numeric (double), complex, character, raw,
        ##              list, expression
        stop(sprintf("Can not convert VecBasic to %s", mode))
    }
)


#### setAs for DenseMatrix  ===========================

setAs("DenseMatrix", "VecBasic", function(from) {
    ## Extract by column
    ## TODO: this function is relative slow and used by other functions
    nrow <- nrow(from)
    ncol <- ncol(from)
    row_idx <- rep(seq.int(nrow), ncol)
    col_idx <- rep(seq.int(ncol), each = nrow)
    s4DenseMat_get(from, row_idx, col_idx, get_basic = FALSE)
})

setAs("VecBasic", "DenseMatrix", function(from) Matrix(from))

setAs("matrix", "DenseMatrix", function(from) Matrix(from))

setMethod("as.vector", c(x = "DenseMatrix"),
    function(x, mode) {
        ## TODO: maybe avoid converting to VecBasic with
        ##       s4binding_subset(x, idx, get_basic = TRUE)
        as.vector(as(x, "VecBasic"), mode)
    }
)
