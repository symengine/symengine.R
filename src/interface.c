
#define R_NO_REMAP

#include <string.h>

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


// SymEngine Logo and Version //================================================

SEXP c_ascii_art_str() {
    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    char* s = ascii_art_str();
    out = Rf_mkString(s);
    basic_str_free(s);
    UNPROTECT(1);
    return out;
}

SEXP c_symengine_version() {
    SEXP out = PROTECT(Rf_mkString(symengine_version()));
    UNPROTECT(1);
    return out;
}

SEXP c_symengine_have_component(SEXP s) {
    const char* str = CHAR(Rf_asChar(s));
    SEXP out = PROTECT(Rf_ScalarLogical(symengine_have_component(str)));
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

SEXP c_parse_str(SEXP RString) {
    const char* str = CHAR(Rf_asChar(RString));
    
    basic_struct* s = basic_new_heap();
    CWRAPPER_OUTPUT_TYPE exception = basic_parse(s, str);
    
    if (exception)
        Rf_error(exception_message(exception));
    
    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);
    UNPROTECT(1);
    return outptr;
}


// Accessors  //================================================================

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

// Constants //=================================================================

SEXP c_builtin_const(SEXP id_which) {
    int id = Rf_asInteger(id_which);
    
    basic_struct* s = basic_new_heap();
    
    switch(id) {
        case  1: basic_const_zero             (s); break;
        case  2: basic_const_one              (s); break;
        case  3: basic_const_minus_one        (s); break;
        case  4: basic_const_I                (s); break;
        case  5: basic_const_pi               (s); break;
        case  6: basic_const_E                (s); break;
        case  7: basic_const_EulerGamma       (s); break;
        case  8: basic_const_Catalan          (s); break;
        case  9: basic_const_GoldenRatio      (s); break;
        case 10: basic_const_infinity         (s); break;
        case 11: basic_const_neginfinity      (s); break;
        case 12: basic_const_complex_infinity (s); break;
        case 13: basic_const_nan              (s); break;
        default: Rf_error("<internal> Unrecognized id for constant");
    }
    
    SEXP out = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(out, _basic_heap_finalizer, TRUE);
    
    UNPROTECT(1);
    return out;
}

SEXP c_make_const(SEXP string) {
    const char* str = CHAR(Rf_asChar(string));
    basic_struct* s = basic_new_heap();
    
    basic_const_set(s, str);
    
    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);
    
    UNPROTECT(1);
    return outptr;
}


// Integer  //====================================================================


SEXP c_integer_from_int(SEXP in) {
    int i = Rf_asInteger(in);

    basic_struct* s = basic_new_heap();
    // CWRAPPER_OUTPUT_TYPE integer_set_si(basic s, long i);
    CWRAPPER_OUTPUT_TYPE exception = integer_set_si(s, i);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}



// Basic: is_a_XXX  //============================================================


SEXP c_is_a_Number(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(is_a_Number(b));
}

SEXP c_is_a_Integer(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(is_a_Integer(b));
}

SEXP c_is_a_Rational(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(is_a_Rational(b));
}

SEXP c_is_a_Symbol(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(is_a_Symbol(b));
}

SEXP c_is_a_Complex(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(is_a_Complex(b));
}

SEXP c_is_a_RealDouble(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(is_a_RealDouble(b));
}

SEXP c_is_a_ComplexDouble(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(is_a_ComplexDouble(b));
}

SEXP c_is_a_RealMPFR(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(is_a_RealMPFR(b));
}

SEXP c_is_a_ComplexMPC(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(is_a_ComplexMPC(b));
}


// Number  //===================================================================

SEXP c_number_is_zero(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(number_is_zero(b));
}

SEXP c_number_is_negative(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(number_is_negative(b));
}

SEXP c_number_is_positive(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(number_is_positive(b));
}

SEXP c_number_is_complex(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(number_is_complex(b));
}
