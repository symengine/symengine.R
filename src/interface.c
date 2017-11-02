
#define R_NO_REMAP

#include <string.h>
#include <limits.h>

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
    return Rf_mkString(symengine_version());
}

SEXP c_symengine_have_component(SEXP s) {
    const char* str = CHAR(Rf_asChar(s));
    return Rf_ScalarLogical(symengine_have_component(str));
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


SEXP c_integer_from_int(SEXP x) {
    int i = Rf_asInteger(x);

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

SEXP c_integer_from_str(SEXP string) {
    const char* str = CHAR(Rf_asChar(string));

    basic_struct* s = basic_new_heap();
    // CWRAPPER_OUTPUT_TYPE integer_set_str(basic s, const char *c);
    CWRAPPER_OUTPUT_TYPE exception = integer_set_str(s, str);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_integer_get_int(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    // signed long integer_get_si(const basic s);
    signed long si = integer_get_si(b);
    // Note that INT_MIN is used as NA_integer_ in R
    if (si >= (INT_MIN + 1) && si <= INT_MAX)
        return Rf_ScalarInteger(si);
    else {
        // TODO
        Rf_error("Number %ld can not be coerced to integer range", si);
    }
}





// Real  //=======================================================================

SEXP c_realdouble_from_d(SEXP x) {
    double d = Rf_asReal(x);

    basic_struct* s = basic_new_heap();
    CWRAPPER_OUTPUT_TYPE exception = real_double_set_d(s, d);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_realdouble_get_d(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer");
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarReal(real_double_get_d(b));
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


// Operations  //==============================================================

/*******************************************************************************
 * //! Assigns s = a + b.
 * CWRAPPER_OUTPUT_TYPE basic_add(basic s, const basic a, const basic b);
 * //! Assigns s = a - b.
 * CWRAPPER_OUTPUT_TYPE basic_sub(basic s, const basic a, const basic b);
 * //! Assigns s = a * b.
 * CWRAPPER_OUTPUT_TYPE basic_mul(basic s, const basic a, const basic b);
 * //! Assigns s = a / b.
 * CWRAPPER_OUTPUT_TYPE basic_div(basic s, const basic a, const basic b);
 * //! Assigns s = a ** b.
 * CWRAPPER_OUTPUT_TYPE basic_pow(basic s, const basic a, const basic b);
 * //! Assign to s, derivative of expr with respect to sym. Returns 0 if sym is not
 * //! a symbol.
 * CWRAPPER_OUTPUT_TYPE basic_diff(basic s, const basic expr, const basic sym);
 *******************************************************************************/

SEXP c_basic_add(SEXP exta, SEXP extb) {
    if (NULL == R_ExternalPtrAddr(exta) || NULL == R_ExternalPtrAddr(extb))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_add(s, a, b);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_sub(SEXP exta, SEXP extb) {
    if (NULL == R_ExternalPtrAddr(exta) || NULL == R_ExternalPtrAddr(extb))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_sub(s, a, b);
    if (exception)
        Rf_error(exception_message(exception));
    
    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_mul(SEXP exta, SEXP extb) {
    if (NULL == R_ExternalPtrAddr(exta) || NULL == R_ExternalPtrAddr(extb))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_mul(s, a, b);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_div(SEXP exta, SEXP extb) {
    if (NULL == R_ExternalPtrAddr(exta) || NULL == R_ExternalPtrAddr(extb))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    basic_struct* s = basic_new_heap();
    
    CWRAPPER_OUTPUT_TYPE exception = basic_div(s, a, b);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_pow(SEXP exta, SEXP extb) {
    if (NULL == R_ExternalPtrAddr(exta) || NULL == R_ExternalPtrAddr(extb))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    basic_struct* s = basic_new_heap();
    
    CWRAPPER_OUTPUT_TYPE exception = basic_pow(s, a, b);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}







