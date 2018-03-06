#define R_NO_REMAP
#include <R.h>
#include <Rcpp.h>
#include <symengine/cwrapper.h>
extern "C" {
  #include "utils.h"
}

// Vector // ===================================================================

// [[Rcpp::export(".vecbasic_length")]]
size_t sexp_vecbasic_length(SEXP ext) {
    CVecBasic* vec = elt_vecbasic(ext);
    size_t sz = vecbasic_size(vec);
    return sz;
}


static inline void _vecbasic_append_sexp(CVecBasic* self, SEXP ext);

// [[Rcpp::export(".vecbasic")]]
SEXP sexp_vecbasic_concentrate(SEXP dots) {
    SEXP       out = PROTECT(sexp_vecbasic_s4());
    CVecBasic* vec = elt_vecbasic(out);
    
    for (R_xlen_t i = 0; i < Rf_length(dots); i++)
        _vecbasic_append_sexp(vec, VECTOR_ELT(dots, i));
    
    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".vecbasic_subset")]]
SEXP sexp_vecbasic_subset(SEXP ext, SEXP idx) {
    if (TYPEOF(idx) != INTSXP)
        Rf_error("Internal? Index must be integer");
    
    CVecBasic* inv  = elt_vecbasic(ext);
    SEXP       out  = PROTECT(sexp_vecbasic_s4());
    CVecBasic* outv = elt_vecbasic(out);
    int*       ids  = INTEGER(idx);
    
    // Used as a temporarily value in the loop
    SEXP a = PROTECT(sexp_basic());
    basic_struct* val = elt_basic(a);
    for (int i = 0; i < Rf_length(idx); i++) {
        hold_exception(vecbasic_get(inv, ids[i] - 1, val));
        hold_exception(vecbasic_push_back(outv, val));
    }
    
    UNPROTECT(2);
    return out;
}

// [[Rcpp::export(".vecbasic_get")]]
SEXP sexp_vecbasic_get(SEXP ext, SEXP n) {
    SEXP          out  = PROTECT(sexp_basic_s4());
    basic_struct* outv = elt_basic(out);
    
    // Currently vecbasic_get is implemented with vecbasic_subset, then extract the value
    if (Rf_length(n) == 1) {
        SEXP res = PROTECT(sexp_vecbasic_subset(ext, n));
        hold_exception(vecbasic_get(elt_vecbasic(res), 0, outv));
        UNPROTECT(1);
    }
    
    if (Rf_length(n) > 1)
        Rf_error("Attempt to select more than one element in vectorIndex");
    
    if (Rf_length(n) == 0)
        Rf_error("Attempt to select less than one element in vectorIndex");
    
    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".vecbasic_assign")]]
SEXP sexp_vecbasic_assign(SEXP ext1, SEXP idx, SEXP ext2) {
    SEXP          out = PROTECT(sexp_vecbasic_s4());
    CVecBasic*   outv = elt_vecbasic(out);

    CVecBasic*   inv1 = elt_vecbasic(ext1);
    CVecBasic*   inv2 = elt_vecbasic(ext2);
    SEXP            a = PROTECT(sexp_basic_s4());
    basic_struct* val = elt_basic(a);
    int*          ids = INTEGER(idx);
    int           flag;
    for (size_t i = 0; i < Rf_length(idx); i++) {
        flag = (ids[i] == 0) ? 0 : ((ids[i] < 0) ? -1 : 1); 
        switch(flag) {
            case  1: hold_exception(vecbasic_get(inv1,  ids[i] - 1, val));  break;
            case -1: hold_exception(vecbasic_get(inv2, -ids[i] - 1, val));  break;
        }
        hold_exception(vecbasic_push_back(outv, val));
    }

    UNPROTECT(2);
    return out;
}


static inline
void _vecbasic_append_sexp(CVecBasic* self, SEXP ext) {
    if (is_basic(ext)) {
        hold_exception(vecbasic_push_back(self, elt_basic(ext)));
        return;
    }
    if (is_vecbasic(ext)) {
        
        CVecBasic* toappend = elt_vecbasic(ext);
        
        SEXP a = PROTECT(sexp_basic());
        basic_struct* val = elt_basic(a);
        for (size_t i = 0; i < vecbasic_size(toappend); i++) {
            hold_exception(vecbasic_get(toappend, i, val));
            hold_exception(vecbasic_push_back(self, val));
        }
        UNPROTECT(1);
        
        return;
    }
    // TODO: convert to basic
    Rf_error("TODO: not implemented");
}

