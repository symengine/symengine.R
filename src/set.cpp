#define R_NO_REMAP
#include <R.h>
#include <Rcpp.h>
#include <symengine/cwrapper.h>
extern "C" {
  #include "utils.h"
}

// [[Rcpp::export(".setbasic")]]
SEXP sexp_setbasic(SEXP ext) {
    SEXP          out = PROTECT(sexp_setbasic_s4());
    CSetBasic*    set = elt_setbasic(out);
    CVecBasic*    vec = elt_vecbasic(ext);
    size_t        len = vecbasic_size(vec);
    SEXP          a   = PROTECT(sexp_setbasic());
    basic_struct* val = elt_basic(a);

    for (size_t i = 0; i < len; i++) {
        hold_exception(vecbasic_get(vec, i, val));
        setbasic_insert(set, val);
    }

    UNPROTECT(2);
    return out;
}

// [[Rcpp::export(".setbasic_length")]]
size_t sexp_setbasic_length(SEXP ext) {
    CSetBasic* set = elt_setbasic(ext);
    size_t     sz  = setbasic_size(set);
    return sz;
}

// [[Rcpp::export(".setbasic_subset")]]
SEXP sexp_setbasic_subset(SEXP ext, SEXP idx) {
    if (TYPEOF(idx) != INTSXP)
        Rf_error("Internal? Index must be integer");
    
    CSetBasic* ins  = elt_setbasic(ext);
    SEXP       out  = PROTECT(sexp_setbasic_s4());
    CSetBasic* outs = elt_setbasic(out);
    int*       ids  = INTEGER(idx);
    
    // Used as a temporarily value in the loop
    SEXP          a   = PROTECT(sexp_basic());
    basic_struct* val = elt_basic(a);
    for (int i = 0; i < Rf_length(idx); i++) {
        setbasic_get(ins, ids[i] - 1, val);
        setbasic_insert(outs, val);
    }
    
    UNPROTECT(2);
    return out;
}

// [[Rcpp::export(".setbasic_get")]]
SEXP sexp_setbasic_get(SEXP ext, SEXP n) {
    if (Rf_length(n) > 1)
        Rf_error("Attempt to select more than one element in setIndex");
    if (Rf_length(n) == 0)
        Rf_error("Attempt to select less than one element in setIndex");
    if (Rf_asInteger(n) > sexp_setbasic_length(ext))
        Rf_error("Attempt to select out of range");

    // Currently setbasic_get is implemented with setbasic_subset, then extract the value
    SEXP          out  = PROTECT(sexp_basic_s4());
    basic_struct* outv = elt_basic(out);
    SEXP          res = PROTECT(sexp_setbasic_subset(ext, n));
    setbasic_get(elt_setbasic(res), 0, outv);

    UNPROTECT(2);
    return out;
}

// [[Rcpp::export(".setbasic_to_vecbasic")]]
SEXP sexp_setbasic_to_vecbasic(SEXP ext) {
    SEXP          out  = PROTECT(sexp_vecbasic_s4());
    CVecBasic*    vec  = elt_vecbasic(out);
    CSetBasic*    set  = elt_setbasic(ext);
    size_t        len  = setbasic_size(set);
    SEXP          a    = PROTECT(sexp_basic());
    basic_struct* val  = elt_basic(a);

    for (size_t i = 0; i < len; i++) {
        setbasic_get(set, i, val);
        hold_exception(vecbasic_push_back(vec, val));
    }

    UNPROTECT(2);
    return out;
}