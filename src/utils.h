
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

// Helper function to initialize an EXTPTR SEXP with empty Basic ================

SEXP sexp_basic();
SEXP sexp_vecbasic();
SEXP sexp_denseMatrix(size_t nrow, size_t ncol);
SEXP sexp_sparseMatrix();
SEXP sexp_setbasic();

// Initialize new Basic/VecBasic/Matrix/SetBasic S4 object //===================================

static inline
SEXP sexp_basic_s4() {
    SEXP empty = PROTECT(R_do_new_object(R_getClassDef("Basic")));
    SEXP out   = PROTECT(R_do_slot_assign(empty, Rf_mkString(".xData"), sexp_basic()));
    UNPROTECT(2);
    return out;
}

static inline
SEXP sexp_vecbasic_s4() {
    SEXP empty = PROTECT(R_do_new_object(R_getClassDef("VecBasic")));
    SEXP out   = PROTECT(R_do_slot_assign(empty, Rf_mkString(".xData"), sexp_vecbasic()));
    UNPROTECT(2);
    return out;
}

static inline
SEXP sexp_denseMatrix_s4(size_t nrow, size_t ncol) {
    SEXP empty = PROTECT(R_do_new_object(R_getClassDef("DenseMatrix")));
    SEXP out   = PROTECT(R_do_slot_assign(empty, Rf_mkString(".xData"), sexp_denseMatrix(nrow, ncol)));
    UNPROTECT(2);
    return out;
}

static inline
SEXP sexp_sparseMatrix_s4() {
    SEXP empty = PROTECT(R_do_new_object(R_getClassDef("SparseMatrix")));
    SEXP out   = PROTECT(R_do_slot_assign(empty, Rf_mkString(".xData"), sexp_sparseMatrix()));
    UNPROTECT(2);
    return out;
}

static inline
SEXP sexp_setbasic_s4() {
    SEXP empty = PROTECT(R_do_new_object(R_getClassDef("SetBasic")));
    SEXP out   = PROTECT(R_do_slot_assign(empty, Rf_mkString(".xData"), sexp_setbasic()));
    UNPROTECT(2);
    return out;
}


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

// Helper function to check whether an externalptr is basic or vecbasic ========

static inline
int is_basic(SEXP x) {
    if (TYPEOF(x) == EXTPTRSXP)
        return R_compute_identical(R_ExternalPtrTag(x),
                                   Rf_mkString("basic_struct*"), 15);
    else if (TYPEOF(x) == S4SXP)
        return R_compute_identical(R_ExternalPtrTag(R_do_slot(x, Rf_mkString(".xData"))),
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
        return R_compute_identical(R_ExternalPtrTag(R_do_slot(x, Rf_mkString(".xData"))),
                                   Rf_mkString("CVecBasic*"), 15);
    else
        Rf_error("Internal: not a S4 or an externalptr");
}

// Helper function to extract basic_struct*/CVecBasic* from S4 or externalptr ==

static inline
basic_struct* elt_basic(SEXP x) {
    // We do not check whether x is a basic here,
    // thus when using it, always add "if (is_basic(x))"
    basic_struct* out;
    
    switch(TYPEOF(x)) {
    
    case S4SXP :
        out = (basic_struct*) R_ExternalPtrAddr(R_do_slot(x, Rf_mkString(".xData")));
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
    // We do not check whether x is a vecbasic here,
    // thus when using it, always add "if (is_vecbasic(x))"
    CVecBasic* out;
    
    switch(TYPEOF(x)) {
    
    case S4SXP :
        out = (CVecBasic*) R_ExternalPtrAddr(R_do_slot(x, Rf_mkString(".xData")));
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
    // We do not check whether x is a vecbasic here,
    // thus when using it, always add "if (is_vecbasic(x))"
    CDenseMatrix* out;
    
    switch(TYPEOF(x)) {
    
    case S4SXP :
        out = (CDenseMatrix*) R_ExternalPtrAddr(R_do_slot(x, Rf_mkString(".xData")));
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
    // We do not check whether x is a vecbasic here,
    // thus when using it, always add "if (is_vecbasic(x))"
    CSetBasic* out;
    
    switch(TYPEOF(x)) {
    
    case S4SXP :
        out = (CSetBasic*) R_ExternalPtrAddr(R_do_slot(x, Rf_mkString(".xData")));
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


#endif // _Rsymengine_utils_

