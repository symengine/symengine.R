
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

