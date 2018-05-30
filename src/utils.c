
#define R_NO_REMAP
#include <Rinternals.h>
#include <symengine/cwrapper.h>

const char* _exception_message_from_cwrapper(CWRAPPER_OUTPUT_TYPE id) {
    // Refer:
    // https://github.com/symengine/symengine/blob/master/symengine/symengine_exception.h
    switch(id) {
    case SYMENGINE_NO_EXCEPTION:
        return "<internal> No exception, it should not go here"     ;
    case SYMENGINE_RUNTIME_ERROR:
        return "SymEngine exception: runtime error"                 ;
    case SYMENGINE_DIV_BY_ZERO:
        return "SymEngine exception: div by zero"                   ;
    case SYMENGINE_NOT_IMPLEMENTED:
        return "SymEngine exception: not implemented"               ;
    case SYMENGINE_DOMAIN_ERROR:
        return "SymEngine exception: domain error"                  ;
    case SYMENGINE_PARSE_ERROR:
        return "SymEngine exception: parse error"                   ;
    default:
        return "<internal> Exception code not matched"              ;
    }
}



static void _basic_heap_finalizer(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        REprintf("Debug> _basic_heap_finalizer: Empty ptr\n");
        return;
    }
    //REprintf("Debug> _basic_heap_finalizer: Finalizing\n");
    basic_struct* symbol = (basic_struct*) R_ExternalPtrAddr(ext);
    basic_free_heap(symbol);
    R_ClearExternalPtr(ext);
}

SEXP sexp_basic () {
    basic_struct* ptr = basic_new_heap();
    SEXP out = PROTECT(R_MakeExternalPtr(ptr, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(out, _basic_heap_finalizer, TRUE);
    UNPROTECT(1);
    return out;
}

static void _vecbasic_finalizer(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        REprintf("Debug> _vecbasic_finalizer: Empty ptr\n");
        return;
    }
    CVecBasic* vec = (CVecBasic*) R_ExternalPtrAddr(ext);
    vecbasic_free(vec);
    R_ClearExternalPtr(ext);
}

SEXP sexp_vecbasic () {
    CVecBasic* ptr = vecbasic_new();
    SEXP out = PROTECT(R_MakeExternalPtr(ptr, Rf_mkString("CVecBasic*"), R_NilValue));
    R_RegisterCFinalizerEx(out, _vecbasic_finalizer, TRUE);
    UNPROTECT(1);
    return out;
}

static void _denseMatrix_finalizer(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        REprintf("Debug> _denseMatrix_finalizer: Empty ptr\n");
        return;
    }
    CDenseMatrix* mat = (CDenseMatrix*) R_ExternalPtrAddr(ext);
    dense_matrix_free(mat);
    R_ClearExternalPtr(ext);
}

SEXP sexp_denseMatrix (size_t nrow, size_t ncol) {
    CDenseMatrix* ptr = dense_matrix_new_rows_cols(nrow, ncol);
    SEXP out = PROTECT(R_MakeExternalPtr(ptr, Rf_mkString("CDenseMatrix*"), R_NilValue));
    R_RegisterCFinalizerEx(out, _denseMatrix_finalizer, TRUE);
    UNPROTECT(1);
    return out;
}

static void _sparseMatrix_finalizer(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        REprintf("Debug> _sparseMatrix_finalizer: Empty ptr\n");
        return;
    }
    CSparseMatrix* mat = (CSparseMatrix*) R_ExternalPtrAddr(ext);
    sparse_matrix_free(mat);
    R_ClearExternalPtr(ext);
}

SEXP sexp_sparseMatrix () {
    CSparseMatrix* ptr = sparse_matrix_new();
    SEXP out = PROTECT(R_MakeExternalPtr(ptr, Rf_mkString("CSparseMatrix*"), R_NilValue));
    R_RegisterCFinalizerEx(out, _sparseMatrix_finalizer, TRUE);
    UNPROTECT(1);
    return out;
}

static void _setbasic_finalizer(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        REprintf("Debug> _setbasic_finalizer: Empty ptr\n");
        return;
    }
    CSetBasic* vec = (CSetBasic*) R_ExternalPtrAddr(ext);
    setbasic_free(vec);
    R_ClearExternalPtr(ext);
}

SEXP sexp_setbasic () {
    CSetBasic* ptr = setbasic_new();
    SEXP out = PROTECT(R_MakeExternalPtr(ptr, Rf_mkString("CSetBasic*"), R_NilValue));
    R_RegisterCFinalizerEx(out, _setbasic_finalizer, TRUE);
    UNPROTECT(1);
    return out;
}

static void _mapbasic_finalizer(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        REprintf("Debug> _setbasic_finalizer: Empty ptr\n");
        return;
    }
    CMapBasicBasic* map = (CMapBasicBasic*) R_ExternalPtrAddr(ext);
    mapbasicbasic_free(map);
    R_ClearExternalPtr(ext);
}

SEXP sexp_mapbasic () {
    CMapBasicBasic* ptr = mapbasicbasic_new();
    SEXP out = PROTECT(R_MakeExternalPtr(ptr, Rf_mkString("CMapBasicBasic*"), R_NilValue));
    R_RegisterCFinalizerEx(out, _mapbasic_finalizer, TRUE);
    UNPROTECT(1);
    return out;
}