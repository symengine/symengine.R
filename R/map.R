setClass("MapBasic", contains = "externalptr")

mapbasic <- function(key, mapped) {
    vkey    <- vecbasic(key)
    vmapped <- vecbasic(mapped)
    if (length(vkey) != length(vmapped))
        stop("the length of key and mapped must equal")
    .mapbasic(key, mapped)
}

mapbasic_get <- function(map, key) {
    .mapbasic_get(map, key)
}

setMethod("length", "MapBasic",
    function(x) {
        .mapbasic_length(x)
    }
)

setMethod("[[", c(x = "MapBasic", i = "Basic", j = "ANY"),
    function(x, i, j, ...) {
        if (!missing(...))
            warning("Extra arguments are ignored")
        if (!missing(j))
            stop("incorrect number of dimensions")
        mapbasic_get(x, i)
    }
)

# can't assign, because can't duplicate mapbasic