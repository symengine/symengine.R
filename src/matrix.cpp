#define R_NO_REMAP
#include <R.h>
#include <Rcpp.h>
#include <symengine/cwrapper.h>
extern "C" {
  #include "utils.h"
}

// [[Rcpp::export(".denseMatrix")]]
SEXP sexp_denseMatrix_init(SEXP ext, size_t nrow, size_t ncol) {
    SEXP          out = PROTECT(sexp_denseMatrix_s4(nrow, ncol));
    CDenseMatrix* mat = elt_denseMatrix(out);
    CVecBasic*    vec = elt_vecbasic(ext);
    SEXP          a   = PROTECT(sexp_basic());
    basic_struct* val = elt_basic(a);
    int           idx = 0;
    int           len = vecbasic_size(vec);
    
    for (size_t c = 0; c < ncol; c++) {
        for (size_t r = 0; r < nrow; r++) {
            hold_exception(vecbasic_get(vec, idx, val));
            idx = (idx + 1) < len ? idx + 1 : 0; 
            hold_exception(dense_matrix_set_basic(mat, r, c, val));
        }
    }
    UNPROTECT(2);
    return out;
}

// [[Rcpp::export(".denseMatrix_str")]]
SEXP sexp_denseMatrix_str(SEXP ext) {
    char* str = dense_matrix_str(elt_denseMatrix(ext));
    return Rf_mkString(str);
}

// [[Rcpp::export(".denseMatrix_rows")]]
size_t sexp_denseMatrix_rows(SEXP ext) {
    CDenseMatrix* mat  = elt_denseMatrix(ext);
    size_t        nrow = dense_matrix_rows(mat);
    return nrow;
}

// [[Rcpp::export(".denseMatrix_cols")]]
size_t sexp_denseMatrix_cols(SEXP ext) {
    CDenseMatrix* mat  = elt_denseMatrix(ext);
    size_t        ncol = dense_matrix_cols(mat);
    return ncol;
}

// [[Rcpp::export(".denseMatrix_get")]]
SEXP sexp_denseMatrix_get(SEXP ext, SEXP i, SEXP j) {
    SEXP          out  = PROTECT(sexp_basic_s4());
    basic_struct* outv = elt_basic(out);
    CDenseMatrix* mat  = elt_denseMatrix(ext);

    if (Rf_length(i) == 1 && Rf_length(j) == 1) {
        hold_exception(dense_matrix_get_basic(outv, mat,
            Rf_asInteger(i) - 1, Rf_asInteger(j) - 1));
    }
    UNPROTECT(1);

    return out;
}