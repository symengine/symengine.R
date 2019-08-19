
#include <R.h>
#include <Rcpp.h>

using namespace Rcpp;

typedef enum {
    S4UNKNOWN,
    S4BASIC,
    S4VECBASIC,
    S4DENSEMATRIX,
    S4LAMBDAVIT,
    S4LLVMVIT
} s4binding_t;


////////////////////////////////////////////////////////////////////


size_t s4vecbasic_size(SEXP robj);

SEXP s4vecbasic_get(RObject robj, int idx);

// 
// IntegerVector s4DenseMat_dim(S4 robj);

S4 s4DenseMat_get(S4 robj, IntegerVector rows, IntegerVector cols, bool get_basic);

//////////////////

