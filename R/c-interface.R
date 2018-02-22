
## Utils  ======================================================================















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


