#define R_NO_REMAP
#include <R.h>
#include <Rcpp.h>
#include <symengine/cwrapper.h>
extern "C" {
  #include "utils.h"
}

// [[Rcpp::export(".denseMatrix")]]
SEXP sexp_denseMatrix_init(SEXP ext, size_t nrow, size_t ncol, size_t row_first=0) {
    SEXP          out = PROTECT(sexp_denseMatrix_s4(nrow, ncol));
    CDenseMatrix* mat = elt_denseMatrix(out);
    CVecBasic*    vec = elt_vecbasic(ext);
    SEXP          a   = PROTECT(sexp_basic());
    basic_struct* val = elt_basic(a);
    int           idx = 0;
    int           len = vecbasic_size(vec);

    if (!row_first) {
        for (size_t c = 0; c < ncol; c++) {
            for (size_t r = 0; r < nrow; r++) {
                hold_exception(vecbasic_get(vec, idx, val));
                idx = (idx + 1) < len ? idx + 1 : 0; 
                hold_exception(dense_matrix_set_basic(mat, r, c, val));
            }
        }
    } else {
        for (size_t r = 0; r < nrow; r++) {
            for (size_t c = 0; c < ncol; c++) {
                hold_exception(vecbasic_get(vec, idx, val));
                idx = (idx + 1) < len ? idx + 1 : 0; 
                hold_exception(dense_matrix_set_basic(mat, r, c, val));
            }
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

    hold_exception(dense_matrix_get_basic(outv, mat,
        Rf_asInteger(i) - 1, Rf_asInteger(j) - 1));

    UNPROTECT(1);

    return out;
}

// [[Rcpp::export(".denseMatrix_subset")]]
SEXP sexp_denseMatrix_subset(SEXP ext, SEXP idxr, SEXP idxc) {
    if (TYPEOF(idxr) != INTSXP || TYPEOF(idxc) != INTSXP)
        Rf_error("Internal? Index must be integer");
    
    CDenseMatrix* inm  = elt_denseMatrix(ext);
    SEXP          out  = PROTECT(sexp_denseMatrix_s4(Rf_length(idxr), Rf_length(idxc)));
    CDenseMatrix* outm = elt_denseMatrix(out);
    int*          idsr = INTEGER(idxr);
    int*          idsc = INTEGER(idxc);
    
    // Used as a temporarily value in the loop
    SEXP a = PROTECT(sexp_basic());
    basic_struct* val = elt_basic(a);
    for (int i = 0; i < Rf_length(idxr); i++) {
        for (int j = 0; j < Rf_length(idxc); j++) {
            hold_exception(dense_matrix_get_basic(val, inm, idsr[i] - 1, idsc[j] - 1));
            hold_exception(dense_matrix_set_basic(outm, i, j, val));
        }
    }
    
    UNPROTECT(2);
    return out;
}

static inline
SEXP duplicate_denseMatrix(SEXP ext) {
    size_t        nrow = sexp_denseMatrix_rows(ext);
    size_t        ncol = sexp_denseMatrix_cols(ext);
    SEXP          out  = PROTECT(sexp_denseMatrix_s4(nrow, ncol));
    CDenseMatrix* outm = elt_denseMatrix(out);
    CDenseMatrix* inm  = elt_denseMatrix(ext);
    hold_exception(dense_matrix_set(outm, inm));
    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".denseMatrix_assign")]]
SEXP sexp_denseMatrix_assign(SEXP ext1, SEXP idxr, SEXP idxc, SEXP ext2) {
    // Duplicate the denseMatrix
    SEXP          out  = PROTECT(duplicate_denseMatrix(ext1));

    CDenseMatrix* outm = elt_denseMatrix(out);
    CVecBasic*    inv  = elt_vecbasic(ext2);
    size_t        lenv = vecbasic_size(inv);
    size_t        idx  = 0;
    SEXP          a    = PROTECT(sexp_basic_s4());
    basic_struct* val  = elt_basic(a);
    int*          idsr = INTEGER(idxr);
    int*          idsc = INTEGER(idxc);

    for (size_t j = 0; j < Rf_length(idxc); j++) {
        for (size_t i = 0; i < Rf_length(idxr); i++) {
            hold_exception(vecbasic_get(inv, idx, val));
            idx = (idx + 1) < lenv ? idx + 1 : 0;
            hold_exception(dense_matrix_set_basic(outm, idsr[i] - 1, idsc[j] - 1, val));
        }
    }

    UNPROTECT(2);
    return out;
}

// [[Rcpp::export(".denseMatrix_to_vecbasic")]]
SEXP sexp_denseMatrix_to_vecbasic(SEXP ext, size_t row_first=0) {
    SEXP          out  = PROTECT(sexp_vecbasic_s4());
    CVecBasic*    vec  = elt_vecbasic(out);
    CDenseMatrix* mat  = elt_denseMatrix(ext);
    size_t        nrow = sexp_denseMatrix_rows(ext);
    size_t        ncol = sexp_denseMatrix_cols(ext);
    SEXP          a    = PROTECT(sexp_basic());
    basic_struct* val  = elt_basic(a);

    if (!row_first) {
        for (size_t j = 0; j < ncol; j++) {
            for (size_t i = 0; i < nrow; i++) {
                hold_exception(dense_matrix_get_basic(val, mat, i, j));
                hold_exception(vecbasic_push_back(vec, val));
            }
        }
    } else {
        for (size_t i = 0; i < nrow; i++) {
            for (size_t j = 0; j < ncol; j++) {
                hold_exception(dense_matrix_get_basic(val, mat, i, j));
                hold_exception(vecbasic_push_back(vec, val));
            }
        }
    }
    
    UNPROTECT(2);
    return out;
}

// [[Rcpp::export(".dense_matrix_mul_matrix")]]
SEXP sexp_dense_matrix_mul_matrix(SEXP mata, SEXP matb) {
    CDenseMatrix* pa   = elt_denseMatrix(mata);
    CDenseMatrix* pb   = elt_denseMatrix(matb);
    size_t        nrow = dense_matrix_rows(pa);
    size_t        ncol = dense_matrix_rows(pb);
    SEXP          out  = PROTECT(sexp_denseMatrix_s4(nrow, ncol));
    CDenseMatrix* po   = elt_denseMatrix(out);
    
    hold_exception(dense_matrix_mul_matrix(po, pa, pb));
    
    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".denseMatrix_det")]]
SEXP sexp_denseMatrix_det(SEXP mat) {
    SEXP          out = PROTECT(sexp_basic_s4());
    basic_struct* po  = elt_basic(out);
    CDenseMatrix* pa  = elt_denseMatrix(mat);
    
    hold_exception(dense_matrix_det(po, pa));

    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".denseMatrix_inv")]]
SEXP sexp_denseMatrix_inv(SEXP mat) {
    CDenseMatrix* pa   = elt_denseMatrix(mat);
    size_t        nrow = dense_matrix_rows(pa);
    SEXP          out  = PROTECT(sexp_denseMatrix_s4(nrow, nrow));
    CDenseMatrix* po   = elt_denseMatrix(out);

    hold_exception(dense_matrix_inv(po, pa));

    UNPROTECT(1);
    return out; 
}

// [[Rcpp::export(".denseMatrix_transpose")]]
SEXP sexp_denseMatrix_transpose(SEXP mat) {
    CDenseMatrix* pa   = elt_denseMatrix(mat);
    size_t        nrow = dense_matrix_rows(pa);
    SEXP          out  = PROTECT(sexp_denseMatrix_s4(nrow, nrow));
    CDenseMatrix* po   = elt_denseMatrix(out);
    
    hold_exception(dense_matrix_transpose(po, pa));

    UNPROTECT(1);
    return out;
}
