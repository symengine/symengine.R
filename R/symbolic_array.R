
#### A simple wrapper around R's list. i.e. a list of Basic object.
#### This is still a work in progress, so do not export it.

symArray <- function(...) {
    ans <- lapply(list(...), S)
    class(ans) <- "symArray"
    ans
}

print.symArray <- function(x, ...) {
    ## TODO: to improve this...
    ## maybe we need to define method for format instead?
    for (i in seq_along(x))
        x[[i]] <- as.character(x[[i]])
    print.default(x, ...)
    x
}

## Preserve class attributes for these functions

`[.symArray` <- function(x,...) {
    ans <- NextMethod("[")
    class(ans) <- class(x)
    ans
}


## So that it can be used in data frame
as.data.frame.symArray <- as.data.frame.vector

