
## Utils  ======================================================================














## Basic: is_a_XXX  ============================================================


api_is_a_Number        <- function(ptr) .Call("c_is_a_Number"        , as(ptr, "externalptr"))
api_is_a_Integer       <- function(ptr) .Call("c_is_a_Integer"       , as(ptr, "externalptr"))
api_is_a_Rational      <- function(ptr) .Call("c_is_a_Rational"      , as(ptr, "externalptr"))
api_is_a_Symbol        <- function(ptr) .Call("c_is_a_Symbol"        , as(ptr, "externalptr"))
api_is_a_Complex       <- function(ptr) .Call("c_is_a_Complex"       , as(ptr, "externalptr"))
api_is_a_RealDouble    <- function(ptr) .Call("c_is_a_RealDouble"    , as(ptr, "externalptr"))
api_is_a_ComplexDouble <- function(ptr) .Call("c_is_a_ComplexDouble" , as(ptr, "externalptr"))
api_is_a_RealMPFR      <- function(ptr) .Call("c_is_a_RealMPFR"      , as(ptr, "externalptr"))
api_is_a_ComplexMPC    <- function(ptr) .Call("c_is_a_ComplexMPC"    , as(ptr, "externalptr"))


## Number is  ==================================================================

api_number_is_zero <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_zero", ptr)
}

api_number_is_negative <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_negative", ptr)
}

api_number_is_positive <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_positive", ptr)
}

api_number_is_complex <- function (ptr) {
    ptr <- as(ptr, "externalptr")
    stopifnot(api_is_a_Number(ptr))
    .Call("c_number_is_complex", ptr)
}

# ==============================================

api_basic_eq <- function (ptra, ptrb) {
    ptra <- as(ptra, "externalptr")
    ptrb <- as(ptrb, "externalptr")
    .Call("c_basic_eq", ptra, ptrb)
}

api_basic_neq <- function (ptra, ptrb) {
    ptra <- as(ptra, "externalptr")
    ptrb <- as(ptrb, "externalptr")
    .Call("c_basic_neq", ptra, ptrb)
}

if (FALSE) {
    api_basic_eq(S("x == x"), S("y == y"))
    api_basic_neq(S("x == x"), S("y == y"))
    api_basic_eq(S("x + 1 > x"), S("x + 2 > x + 1"))
}


## Functions with one argument


## subs
api_basic_subs2 <- function (ptrexpr, ptrold, ptrnew) {
    ptrexpr <- as(ptrexpr, "externalptr")
    ptrold  <- as(ptrold , "externalptr")
    ptrnew  <- as(ptrnew , "externalptr")
    .Call("c_basic_subs2", ptrexpr, ptrold, ptrnew)
}

## evalf
api_basic_evalf <- function(ptrb, bits = 53L, real = TRUE) {
    ptrb <- as(ptrb, "externalptr")
    bits <- as.integer(bits)
    real <- as.logical(real)
    .Call("c_basic_evalf", ptrb, bits, real)
}

