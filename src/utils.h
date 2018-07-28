
// Some inline functions and functions in "utils.c"

#ifndef _Rsymengine_utils_
#define _Rsymengine_utils_



#define R_NO_REMAP
#include <Rinternals.h>
#include <symengine/cwrapper.h>

// Handle symengine exception ===================================================

const char* _exception_message_from_cwrapper(CWRAPPER_OUTPUT_TYPE id);

static inline void hold_exception(CWRAPPER_OUTPUT_TYPE output) {
    if (output)
        Rf_error(_exception_message_from_cwrapper(output));
    else
        return;
}

// Util function to wrap externalptr to S4 class  ==============================

static inline
SEXP s4(SEXP ext, const char* classname) {
    SEXP empty = PROTECT(R_do_new_object(R_getClassDef(classname)));
    SEXP out   = PROTECT(R_do_slot_assign(empty, Rf_mkString("ptr"), ext));
    UNPROTECT(2);
    return out;
}

// Helper function to initialize an EXTPTR SEXP with empty Basic ================

SEXP sexp_basic();
SEXP sexp_vecbasic();
SEXP sexp_denseMatrix(size_t nrow, size_t ncol);
SEXP sexp_setbasic();
SEXP sexp_mapbasic();

// Initialize new Basic/VecBasic/Matrix/SetBasic/MapBasic S4 object //=========

static inline
SEXP sexp_basic_s4() {
    return s4(sexp_basic(), "Basic");
}

static inline
SEXP sexp_vecbasic_s4() {
    return s4(sexp_vecbasic(), "VecBasic");
}

static inline
SEXP sexp_denseMatrix_s4(size_t nrow, size_t ncol) {
    return s4(sexp_denseMatrix(nrow, ncol), "DenseMatrix");
}

static inline
SEXP sexp_setbasic_s4() {
    return s4(sexp_setbasic(), "SetBasic");
}

static inline
SEXP sexp_mapbasic_s4() {
    return s4(sexp_mapbasic(), "MapBasic");
}

// Helper function to do conversion between setbasic and vecbasic ==============

SEXP sexp_vec2set(SEXP ext);
SEXP sexp_set2vec(SEXP ext);
SEXP sexp_set2vec_s4(SEXP ext);


// Helper function to check EXTPTR SEXP of Basic //==============================

// We may not need them anymore

static inline void sexp_check_basic(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer\n");
    if (!R_compute_identical(R_ExternalPtrTag(ext), Rf_mkString("basic_struct*"), 15))
        Rf_error("Tag of the pointer does not match to 'basic_struct*'\n");
    return;
}

static inline void sexp_check_vecbasic(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer\n");
    if (!R_compute_identical(R_ExternalPtrTag(ext), Rf_mkString("CVecBasic*"), 15))
        Rf_error("Tag of the pointer does not match to 'CVecBasic*'\n");
    return;
}

// Helper function to check the type of SEXP ==================================

static inline
int is_basic(SEXP x) {
    if (TYPEOF(x) == EXTPTRSXP)
        return R_compute_identical(R_ExternalPtrTag(x),
                                   Rf_mkString("basic_struct*"), 15);
    else if (TYPEOF(x) == S4SXP)
        return R_compute_identical(R_ExternalPtrTag(R_do_slot(x, Rf_mkString("ptr"))),
                                   Rf_mkString("basic_struct*"), 15);
    else
        Rf_error("Internal: not a S4 or an externalptr");
}

static inline
int is_vecbasic(SEXP x) {
    if (TYPEOF(x) == EXTPTRSXP)
        return R_compute_identical(R_ExternalPtrTag(x),
                                   Rf_mkString("CVecBasic*"), 15);
    else if (TYPEOF(x) == S4SXP)
        return R_compute_identical(R_ExternalPtrTag(R_do_slot(x, Rf_mkString("ptr"))),
                                   Rf_mkString("CVecBasic*"), 15);
    else
        Rf_error("Internal: not a S4 or an externalptr");
}

static inline
int is_denseMatrix(SEXP x) {
    if (TYPEOF(x) == EXTPTRSXP)
        return R_compute_identical(R_ExternalPtrTag(x),
                                   Rf_mkString("CDenseMatrix*"), 15);
    else if (TYPEOF(x) == S4SXP)
        return R_compute_identical(R_ExternalPtrTag(R_do_slot(x, Rf_mkString("ptr"))),
                                   Rf_mkString("CDenseMatrix*"), 15);
    else
        Rf_error("Internal: not a S4 or an externalptr");
}

static inline
int is_setbasic(SEXP x) {
    if (TYPEOF(x) == EXTPTRSXP)
        return R_compute_identical(R_ExternalPtrTag(x),
                                   Rf_mkString("CSetBasic*"), 15);
    else if (TYPEOF(x) == S4SXP)
        return R_compute_identical(R_ExternalPtrTag(R_do_slot(x, Rf_mkString("ptr"))),
                                   Rf_mkString("CSetBasic*"), 15);
    else
        Rf_error("Internal: not a S4 or an externalptr");
}

static inline
int is_mapbasic(SEXP x) {
    if (TYPEOF(x) == EXTPTRSXP)
        return R_compute_identical(R_ExternalPtrTag(x),
                                   Rf_mkString("CMapBasicBasic*"), 15);
    else if (TYPEOF(x) == S4SXP)
        return R_compute_identical(R_ExternalPtrTag(R_do_slot(x, Rf_mkString("ptr"))),
                                   Rf_mkString("CMapBasicBasic*"), 15);
    else
        Rf_error("Internal: not a S4 or an externalptr");
}

// Helper function to extract the pointer of SEXP =============================

static inline
basic_struct* elt_basic(SEXP x) {
    if (!is_basic(x))
        Rf_error("x is not basic");
    
    basic_struct* out;

    switch(TYPEOF(x)) {
        case S4SXP :
            out = (basic_struct*) R_ExternalPtrAddr(R_do_slot(x, Rf_mkString("ptr")));
            break;
        case EXTPTRSXP :
            out = (basic_struct*) R_ExternalPtrAddr(x);
            break;
        default :
            Rf_error("Internal");
    }
    
    if (NULL == out)
        Rf_error("Invalid pointer for 'Basic'");
    
    return out;
}

static inline
CVecBasic* elt_vecbasic(SEXP x) {
    if (!is_vecbasic(x))
        Rf_error("x is not vecbasic");

    CVecBasic* out;
    
    switch(TYPEOF(x)) {
        case S4SXP :
            out = (CVecBasic*) R_ExternalPtrAddr(R_do_slot(x, Rf_mkString("ptr")));
            break;
        case EXTPTRSXP :
            out = (CVecBasic*) R_ExternalPtrAddr(x);
            break;
        default :
            Rf_error("Internal");
    }
    
    if (NULL == out)
        Rf_error("Invalid pointer for 'VecBasic'");
    
    return out;
}

static inline
CDenseMatrix* elt_denseMatrix(SEXP x) {
    if (!is_denseMatrix(x))
        Rf_error("x is not denseMatrix");

    CDenseMatrix* out;
    
    switch(TYPEOF(x)) {
        case S4SXP :
            out = (CDenseMatrix*) R_ExternalPtrAddr(R_do_slot(x, Rf_mkString("ptr")));
            break;
        case EXTPTRSXP :
            out = (CDenseMatrix*) R_ExternalPtrAddr(x);
            break;
        default :
            Rf_error("Internal");
    }
    
    if (NULL == out)
        Rf_error("Invalid pointer for 'DenseMatrix'");
    
    return out;
}

static inline
CSetBasic* elt_setbasic(SEXP x) {
    if (!is_setbasic(x))
        Rf_error("x is not setbasic");

    CSetBasic* out;
    
    switch(TYPEOF(x)) {
        case S4SXP :
            out = (CSetBasic*) R_ExternalPtrAddr(R_do_slot(x, Rf_mkString("ptr")));
            break;
        case EXTPTRSXP :
            out = (CSetBasic*) R_ExternalPtrAddr(x);
            break;
        default :
            Rf_error("Internal");
    }
    
    if (NULL == out)
        Rf_error("Invalid pointer for 'DenseMatrix'");
    
    return out;
}

static inline
CMapBasicBasic* elt_mapbasic(SEXP x) {
    if (!is_mapbasic(x))
        Rf_error("x is not mapbasic");
        
    CMapBasicBasic* out;
    
    switch(TYPEOF(x)) {
        case S4SXP :
            out = (CMapBasicBasic*) R_ExternalPtrAddr(R_do_slot(x, Rf_mkString("ptr")));
            break;
            
        case EXTPTRSXP :
            out = (CMapBasicBasic*) R_ExternalPtrAddr(x);
            break;
            
        default :
            Rf_error("Internal");
    }
    
    if (NULL == out)
        Rf_error("Invalid pointer for 'DenseMatrix'");
    
    return out;
}

#endif // _Rsymengine_utils_

