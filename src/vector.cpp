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