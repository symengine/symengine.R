
#' @include basic.R

setClass("MapBasic", contains = "SymEnginePTR")

#' @export
mapbasic <- function(key, mapped) {
    vkey    <- do.call(vecbasic, key)
    vmapped <- do.call(vecbasic, mapped)
    if (length(vkey) != length(vmapped))
        stop("the length of key and mapped must equal")
    .mapbasic(vkey, vmapped)
}

mapbasic_get <- function(map, key) {
    .mapbasic_get(map, key)
}

setMethod("length", "MapBasic",
    function(x) {
        .mapbasic_length(x)
    }
)

setMethod("show", "MapBasic",
    function (object) {
        cat(sprintf("mapbasic of length %s\n", length(object)))
    }
)

setMethod("[[", c(x = "MapBasic", i = "Basic", j = "ANY"),
    function(x, i, j, ...) {
        args <- as.list(sys.call())[-1L]
        len <- length(args)
        if (len > 2)
            stop("incorrect number of dimensions")
        mapbasic_get(x, i)
    }
)

# can't assign, because can't duplicate mapbasic