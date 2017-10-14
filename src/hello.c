
#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include <gmp.h>
#include <symengine/cwrapper.h>


// SymEngine Logo //============================================================

SEXP c_ascii_art_str() {
    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    const char* s = ascii_art_str();
    out = Rf_mkString(s);
    UNPROTECT(1);
    return out;
}

// Basic Symbol Initiator and Finalizer //======================================

static void _basic_heap_finalizer(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        Rprintf("Debug: empty ptr (_basic_heap_finalizer)\n");
        return;
    }
    Rprintf("Debug: finalizing (_basic_heap_finalizer)\n");
    basic_struct* symbol = (basic_struct*) R_ExternalPtrAddr(ext);
    basic_free_heap(symbol);
    R_ClearExternalPtr(ext);
}

SEXP c_new_heap_symbol(SEXP RString) {
    const char* str_symbol = CHAR(Rf_asChar(RString));
    
    basic_struct* symbol = basic_new_heap();
    symbol_set(symbol, str_symbol);
    
    SEXP outptr = PROTECT(
        R_MakeExternalPtr(symbol, Rf_mkString("basic_struct*"), R_NilValue)
    );
    
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);
    
    UNPROTECT(1);
    return outptr;
}


// It is not correct to use stack?? ---------------

static void _basic_stack_finalizer(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        Rprintf("Debug: empty ptr (_basic_stack_finalizer)\n");
        return;
    }
    Rprintf("Debug: finalizing (_basic_stack_finalizer)\n");
    basic* symbol = (basic*) R_ExternalPtrAddr(ext);
    basic_free_stack(* symbol);
    R_ClearExternalPtr(ext);
}

SEXP c_new_stack_symbol(SEXP RString) {
    const char* str_symbol = CHAR(Rf_asChar(RString));
    
    basic symbol;
    basic_new_stack(symbol);
    
    symbol_set(symbol, str_symbol);
    
    basic* symbolptr = &symbol;
    
    SEXP outptr = PROTECT(
        R_MakeExternalPtr(symbolptr, Rf_mkString("basic*"), R_NilValue)
    );
    
    R_RegisterCFinalizerEx(outptr, _basic_stack_finalizer, TRUE);
    
    UNPROTECT(1);
    return outptr;
}

