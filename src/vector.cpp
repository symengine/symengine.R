#define R_NO_REMAP
#include <R.h>
#include <Rcpp.h>
#include <symengine/cwrapper.h>
extern "C" {
  #include "utils.h"
}

// Vector // ===================================================================

// [[Rcpp::export(".vecbasic_new")]]
SEXP sexp_vecbasic_new() {
    SEXP          out = PROTECT(sexp_vecbasic());
    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".vecbasic_push_back")]]
void sexp_vecbasic_push_back(SEXP ext, SEXP value) {
    CVecBasic*    vec = (CVecBasic*)    R_ExternalPtrAddr(ext);
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(value);
    hold_exception(vecbasic_push_back(vec, s));
    return;
}

// [[Rcpp::export(".vecbasic_get")]]
void sexp_vecbasic_get(SEXP ext, SEXP n, SEXP val) {
    CVecBasic*    vec = (CVecBasic*)    R_ExternalPtrAddr(ext);
    size_t        id  = Rf_asInteger(n) - 1;
    basic_struct* v   = (basic_struct*) R_ExternalPtrAddr(val);
    hold_exception(vecbasic_get(vec, id, v));
    return;
}

// [[Rcpp::export(".vecbasic_size")]]
size_t sexp_vecbasic_size(SEXP ext) {
    CVecBasic*    vec = (CVecBasic*)    R_ExternalPtrAddr(ext);
    size_t sz = vecbasic_size(vec);
    return sz;
}

// [[Rcpp::export(".vecbasic_set")]]
void sexp_vecbasic_set(SEXP ext, SEXP n, SEXP val) {
    CVecBasic*    vec = (CVecBasic*)    R_ExternalPtrAddr(ext);
    size_t        id  = Rf_asInteger(n) - 1;
    basic_struct* v   = (basic_struct*) R_ExternalPtrAddr(val);
    hold_exception(vecbasic_set(vec, id, v));
}

// [[Rcpp::export(".vecbasic_erase")]]
void sexp_vecbasic_erase(SEXP ext, SEXP n) {
    CVecBasic*    vec = (CVecBasic*)    R_ExternalPtrAddr(ext);
    size_t        id  = Rf_asInteger(n);
    hold_exception(vecbasic_erase(vec, id));
}

// [[Rcpp::export(".basic_max")]]
void sexp_basic_max(SEXP val, SEXP ext) {
    CVecBasic*    vec = (CVecBasic*)    R_ExternalPtrAddr(ext);
    basic_struct* v   = (basic_struct*) R_ExternalPtrAddr(val);
    hold_exception(basic_max(v, vec));
}

// [[Rcpp::export(".basic_min")]]
void sexp_basic_min(SEXP val, SEXP ext) {
    CVecBasic*    vec = (CVecBasic*)    R_ExternalPtrAddr(ext);
    basic_struct* v   = (basic_struct*) R_ExternalPtrAddr(val);
    hold_exception(basic_min(v, vec));
}


// Helper function for vecbasic_concentrate
// Append vecbasic vec2 to vecbasic vec1
void vecbasic_concentrate_vec(SEXP vec1, SEXP vec2) {
    size_t sz2 = sexp_vecbasic_size(vec2);
    SEXP   val = PROTECT(sexp_basic());
    for (size_t i = 0; i < sz2; i++) {
        sexp_vecbasic_get(vec2, Rf_ScalarInteger(i + 1), val);
        sexp_vecbasic_push_back(vec1, val);
    }
    UNPROTECT(1);
    return;
}

// [[Rcpp::export(".vecbasic_concentrate")]]
SEXP vecbasic_concentrate(SEXP dots) {
    R_len_t len = Rf_length(dots);
    SEXP    out = PROTECT(sexp_vecbasic());
    for (R_xlen_t i = 0; i < len; i++) {
        if (R_compute_identical(R_ExternalPtrTag(VECTOR_ELT(dots, i)), Rf_mkString("basic_struct*"), 15)) {
            sexp_vecbasic_push_back(out, VECTOR_ELT(dots, i));
        } else if (R_compute_identical(R_ExternalPtrTag(VECTOR_ELT(dots, i)), Rf_mkString("CVecBasic*"), 15)) {
            vecbasic_concentrate_vec(out, VECTOR_ELT(dots, i));
        } else {
            Rf_error("The %dth object's type is error'", i);
        }
    }
    UNPROTECT(1);
    return out;
}


// [[Rcpp::export(".vecbasic_get")]]
SEXP vecbasic_get(SEXP ext, SEXP n) {
    // It can't handle out_of_range for now.
    SEXP   out = PROTECT(sexp_basic());
    sexp_vecbasic_get(ext, n, out);
    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".vecbasic_subset")]]
SEXP vecbasic_subset(SEXP ext, SEXP dots) {
    // It can't handle out_of_range for now.
    SEXP   out = PROTECT(sexp_vecbasic());
    SEXP   val = PROTECT(sexp_basic());
    int*   lt = INTEGER(dots);
    int    len = Rf_length(dots);
    for (int i = 0; i < len; ++i) {
        if (lt[i] == 0) continue;
        sexp_vecbasic_get(ext, Rf_ScalarInteger(lt[i]), val);
        sexp_vecbasic_push_back(out, val);
    }
    UNPROTECT(2);
    return out;
}



