
# two arg function
vecbasic_add <- function(vec1, vec2)  .vecbasic_add(vec1, vec2)
vecbasic_sub <- function(vec1, vec2)  .vecbasic_sub(vec1, vec2)
vecbasic_mul <- function(vec1, vec2)  .vecbasic_mul(vec1, vec2)
vecbasic_div <- function(vec1, vec2)  .vecbasic_div(vec1, vec2)
vecbasic_pow <- function(vec1, vec2)  .vecbasic_pow(vec1, vec2)

vecbasic_diff <- function(vec1, vec2) .vecbasic_diff(vec1, vec2)

setMethods("+",
    list(c(e1 = "VecBasic", e2 = "VecBasic"),
         c(e1 = "VecBasic", e2 = "Basic"),
         c(e1 = "Basic"  , e2 = "VecBasic")),
    
    function (e1, e2) {
        v1 <- vecbasic(e1)
        v2 <- vecbasic(e2)
        l1 <- length(v1)
        l2 <- length(v2)
        if (max(l1,l2) %% min(l1,l2))
            warning("longer object length is not a multiple of shorter object length")
        vecbasic_add(v1, v2)
    }
)

setMethods("-",
    list(c(e1 = "VecBasic", e2 = "VecBasic"),
         c(e1 = "VecBasic", e2 = "Basic"),
         c(e1 = "Basic"  , e2 = "VecBasic")),
    
    function (e1, e2) {
        v1 <- vecbasic(e1)
        v2 <- vecbasic(e2)
        l1 <- length(v1)
        l2 <- length(v2)
        if (max(l1,l2) %% min(l1,l2))
            warning("longer object length is not a multiple of shorter object length")
        vecbasic_sub(v1, v2)
    }
)

setMethods("*",
    list(c(e1 = "VecBasic", e2 = "VecBasic"),
         c(e1 = "VecBasic", e2 = "Basic"),
         c(e1 = "Basic"  , e2 = "VecBasic")),
    
    function (e1, e2) {
        v1 <- vecbasic(e1)
        v2 <- vecbasic(e2)
        l1 <- length(v1)
        l2 <- length(v2)
        if (max(l1,l2) %% min(l1,l2))
            warning("longer object length is not a multiple of shorter object length")
        vecbasic_mul(v1, v2)
    }
)

setMethods("/",
    list(c(e1 = "VecBasic", e2 = "VecBasic"),
         c(e1 = "VecBasic", e2 = "Basic"),
         c(e1 = "Basic"  , e2 = "VecBasic")),
    
    function (e1, e2) {
        v1 <- vecbasic(e1)
        v2 <- vecbasic(e2)
        l1 <- length(v1)
        l2 <- length(v2)
        if (max(l1,l2) %% min(l1,l2))
            warning("longer object length is not a multiple of shorter object length")
        vecbasic_div(v1, v2)
    }
)

setMethods("^",
    list(c(e1 = "VecBasic", e2 = "VecBasic"),
         c(e1 = "VecBasic", e2 = "Basic"),
         c(e1 = "Basic"  , e2 = "VecBasic")),
    
    function (e1, e2) {
        v1 <- vecbasic(e1)
        v2 <- vecbasic(e2)
        l1 <- length(v1)
        l2 <- length(v2)
        if (max(l1,l2) %% min(l1,l2))
            warning("longer object length is not a multiple of shorter object length")
        vecbasic_pow(v1, v2)
    }
)

setMethod("-", c(e1 = "VecBasic", e2 = "missing"),
    function (e1, e2) {
        .vecbasic_neg(e1)
    }
)

setMethod("+", c(e1 = "VecBasic", e2 = "missing"),
    function (e1, e2) {
        e1
    }
)