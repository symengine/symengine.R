
#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include <gmp.h>
#include <symengine/cwrapper.h>

// Utils //=====================================================================

const char* exception_message(CWRAPPER_OUTPUT_TYPE id) {
    // Refer:
    // https://github.com/symengine/symengine/blob/master/symengine/symengine_exception.h
    switch(id) {
        case SYMENGINE_NO_EXCEPTION:
            return "<internal>, no exception, it should not go here.";
        case SYMENGINE_RUNTIME_ERROR:
            return("SymEngine exception: runtime error");
        case SYMENGINE_DIV_BY_ZERO:
            return("SymEngine exception: div by zero");
        case SYMENGINE_NOT_IMPLEMENTED:
            return("SymEngine exception: not implemented");
        case SYMENGINE_DOMAIN_ERROR:
            return("SymEngine exception: domain error");
        case SYMENGINE_PARSE_ERROR:
            return("SymEngine exception: parse error");
        default:
            return("<internal> Exception code not matched");
    }
}


// SymEngine Logo //============================================================

SEXP c_ascii_art_str() {
    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    char* s = ascii_art_str();
    out = Rf_mkString(s);
    basic_str_free(s);
    UNPROTECT(1);
    return out;
}

// Basic Symbol Initiator and Finalizer //======================================

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

SEXP c_new_heap_symbol(SEXP RString) {
    const char* str_symbol = CHAR(Rf_asChar(RString));
    
    //REprintf("Debug> c_new_heap_symbol: The extracted string is '%s'\n", str_symbol);
    
    basic_struct* symbol = basic_new_heap();
    CWRAPPER_OUTPUT_TYPE exception = symbol_set(symbol, str_symbol);
    
    // Handle exception
    if (exception)
        Rf_error(exception_message(exception));
    
    SEXP outptr = PROTECT(
        R_MakeExternalPtr(symbol, Rf_mkString("basic_struct*"), R_NilValue)
    );
    
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);
    
    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_str(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        Rf_error("Invalid pointer\n");
    }
    
    basic_struct* symbol = (basic_struct*) R_ExternalPtrAddr(ext);
    
    char* str = basic_str(symbol);
    SEXP out = PROTECT(Rf_mkString(str));
    basic_str_free(str);
    
    UNPROTECT(1);
    return(out);
}

SEXP c_basic_type(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        Rf_error("Invalid pointer");
    }
    
    basic_struct* symbol = (basic_struct*) R_ExternalPtrAddr(ext);
    
    TypeID typeid = basic_get_type(symbol);
    
    char* classname = basic_get_class_from_id(typeid);
    SEXP out = PROTECT(Rf_mkString(classname));
    
    UNPROTECT(1);
    return out;
}

