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
    CVecBasic*    vec = (CVecBasic*)    R_ExternalPtrAddr(ext);
    size_t sz = vecbasic_size(vec);
    return sz;
}


static inline void _vecbasic_append_sexp(CVecBasic* self, SEXP ext);

// [[Rcpp::export(".vecbasic")]]
SEXP sexp_vecbasic_concentrate(SEXP dots) {
    SEXP       out = PROTECT(sexp_vecbasic());
    CVecBasic* vec = (CVecBasic*) R_ExternalPtrAddr(out);
    
    for (R_xlen_t i = 0; i < Rf_length(dots); i++)
        _vecbasic_append_sexp(vec, VECTOR_ELT(dots, i));
    
    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".vecbasic_subset")]]
SEXP sexp_vecbasic_subset(SEXP ext, SEXP idx) {
    if (TYPEOF(idx) != INTSXP)
        Rf_error("Internal? Index must be integer");
    
    CVecBasic* inv  = (CVecBasic*) R_ExternalPtrAddr(ext);
    SEXP       out  = PROTECT(sexp_vecbasic());
    CVecBasic* outv = (CVecBasic*) R_ExternalPtrAddr(out);
    int*       ids  = INTEGER(idx);
    
    // Used as a temporarily value in the loop
    SEXP a = PROTECT(sexp_basic());
    basic_struct* val = (basic_struct*) R_ExternalPtrAddr(a);
    for (int i = 0; i < Rf_length(idx); i++) {
        vecbasic_get(inv, ids[i] - 1, val);
        vecbasic_push_back(outv, val);
    }
    
    UNPROTECT(2);
    return out;
}

// [[Rcpp::export(".vecbasic_get")]]
SEXP sexp_vecbasic_get(SEXP ext, SEXP n) {
    SEXP          out  = PROTECT(sexp_basic());
    basic_struct* outv = (basic_struct*) R_ExternalPtrAddr(out);
    
    // Currently vecbasic_get is implemented with vecbasic_subset, then extract the value
    if (Rf_length(n) == 1) {
        SEXP res = PROTECT(sexp_vecbasic_subset(ext, n));
        vecbasic_get((CVecBasic*) R_ExternalPtrAddr(res), 0, outv);
        UNPROTECT(1);
    }
    
    if (Rf_length(n) > 1)
        Rf_error("Attempt to select more than one element in vectorIndex");
    
    if (Rf_length(n) == 0)
        Rf_error("Attempt to select less than one element in vectorIndex");
    
    UNPROTECT(1);
    return out;
}


static inline
void _vecbasic_append_sexp(CVecBasic* self, SEXP ext) {
    if ((TYPEOF(ext) == EXTPTRSXP) &&
        R_compute_identical(R_ExternalPtrTag(ext), Rf_mkString("basic_struct*"), 15)) {
        
        vecbasic_push_back(self, (basic_struct*) R_ExternalPtrAddr(ext));
        return;
    }
    if ((TYPEOF(ext) == EXTPTRSXP) &&
        R_compute_identical(R_ExternalPtrTag(ext), Rf_mkString("CVecBasic*"), 15)) {
        
        CVecBasic* toappend = (CVecBasic*) R_ExternalPtrAddr(ext);
        
        SEXP a = PROTECT(sexp_basic());
        basic_struct* val = (basic_struct*) R_ExternalPtrAddr(a);
        for (size_t i = 0; i < vecbasic_size(toappend); i++) {
            vecbasic_get(toappend, i, val);
            vecbasic_push_back(self, val);
        }
        UNPROTECT(1);
        
        return;
    }
    // TODO: convert to basic
    Rf_error("TODO: not implemented");
}

