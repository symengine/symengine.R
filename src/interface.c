
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

// Basic Finalizer //===========================================================

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


// Helper Function to Create a External Ptr to an empty Basic //================
// TODO: use macro instead of function?

SEXP ptr_emptybasic () {
    basic_struct* ptr = basic_new_heap();
    SEXP out = PROTECT(R_MakeExternalPtr(ptr, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(out, _basic_heap_finalizer, TRUE);
    UNPROTECT(1);
    return out;
}



// Basic Symbol Initiator //====================================================

SEXP c_new_heap_symbol(SEXP RString) {
    const char* str_symbol = CHAR(Rf_asChar(RString));
    
    //REprintf("Debug> c_new_heap_symbol: The extracted string is '%s'\n", str_symbol);

    SEXP          outptr = PROTECT(ptr_emptybasic());
    basic_struct* symbol = (basic_struct*) EXTPTR_PTR(outptr);

    CWRAPPER_OUTPUT_TYPE exception = symbol_set(symbol, str_symbol);
    if (exception)
        Rf_error(exception_message(exception));
    
    UNPROTECT(1);
    return outptr;
}

SEXP c_parse_str(SEXP RString) {
    const char* str = CHAR(Rf_asChar(RString));
    
    SEXP          outptr = PROTECT(ptr_emptybasic());
    basic_struct* s      = (basic_struct*) EXTPTR_PTR(outptr);

    CWRAPPER_OUTPUT_TYPE exception = basic_parse2(s, str, 1);
    
    if (exception)
        Rf_error(exception_message(exception));
    
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

SEXP c_basic_str_julia(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext)) {
        Rf_error("Invalid pointer\n");
    }
    
    basic_struct* symbol = (basic_struct*) R_ExternalPtrAddr(ext);
    
    char* str = basic_str_julia(symbol);
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
    
    SEXP          out = PROTECT(ptr_emptybasic());
    basic_struct* s   = (basic_struct*) EXTPTR_PTR(out);
    
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
    
    UNPROTECT(1);
    return out;
}

SEXP c_make_const(SEXP string) {
    const char* str = CHAR(Rf_asChar(string));

    SEXP          out = PROTECT(ptr_emptybasic());
    basic_struct* s   = (basic_struct*) EXTPTR_PTR(out);
    
    basic_const_set(s, str);
    
    UNPROTECT(1);
    return out;
}


// Integer  //====================================================================


SEXP c_integer_from_int(SEXP x) {
    int i = Rf_asInteger(x);

    SEXP          out = PROTECT(ptr_emptybasic());
    basic_struct* s   = (basic_struct*) EXTPTR_PTR(out);

    // CWRAPPER_OUTPUT_TYPE integer_set_si(basic s, long i);
    CWRAPPER_OUTPUT_TYPE exception = integer_set_si(s, i);
    if (exception)
        Rf_error(exception_message(exception));

    UNPROTECT(1);
    return out;
}

SEXP c_integer_from_str(SEXP string) {
    const char* str = CHAR(Rf_asChar(string));

    SEXP          out = PROTECT(ptr_emptybasic());
    basic_struct* s   = (basic_struct*) EXTPTR_PTR(out);

    // CWRAPPER_OUTPUT_TYPE integer_set_str(basic s, const char *c);
    CWRAPPER_OUTPUT_TYPE exception = integer_set_str(s, str);
    if (exception)
        Rf_error(exception_message(exception));

    UNPROTECT(1);
    return out;
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


/*******************************************************************************
 * //! Assign to s, derivative of expr with respect to sym.
 * //! Returns SYMENGINE_RUNTIME_ERROR if sym is not a symbol.
 * CWRAPPER_OUTPUT_TYPE basic_diff(basic s, const basic expr, const basic sym);
 *******************************************************************************/

SEXP c_basic_diff(SEXP extexpr, SEXP extsym) {
    if (NULL == R_ExternalPtrAddr(extexpr) || NULL == R_ExternalPtrAddr(extsym))
        Rf_error("Invalid pointer");
    basic_struct* expr = (basic_struct*) R_ExternalPtrAddr(extexpr);
    basic_struct* sym  = (basic_struct*) R_ExternalPtrAddr(extsym);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_diff(s, expr, sym);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

/*******************************************************************************
 * //! Returns 1 if both basic are equal, 0 if not
 * int basic_eq(const basic a, const basic b);
 * //! Returns 1 if both basic are not equal, 0 if they are
 * int basic_neq(const basic a, const basic b);
 *******************************************************************************/


SEXP c_basic_eq(SEXP exta, SEXP extb) {
    if (NULL == R_ExternalPtrAddr(exta) || NULL == R_ExternalPtrAddr(extb))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    return Rf_ScalarLogical(basic_eq(a, b));
}

SEXP c_basic_neq(SEXP exta, SEXP extb) {
    if (NULL == R_ExternalPtrAddr(exta) || NULL == R_ExternalPtrAddr(extb))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    return Rf_ScalarLogical(basic_neq(a, b));
}



/*******************************************************************************
 * //! Expands the expr a and assigns to s.
 * CWRAPPER_OUTPUT_TYPE basic_expand(basic s, const basic a);
 * //! Assigns s = -a.
 * CWRAPPER_OUTPUT_TYPE basic_neg(basic s, const basic a);
 * //! Assigns s = abs(a).
 * CWRAPPER_OUTPUT_TYPE basic_abs(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_expand(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_expand(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_neg(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_neg(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_abs(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_abs(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(
        R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue)
    );
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

/*******************************************************************************
 * //! Assigns s = erf(a).
 * CWRAPPER_OUTPUT_TYPE basic_erf(basic s, const basic a);
 * //! Assigns s = erfc(a).
 * CWRAPPER_OUTPUT_TYPE basic_erfc(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_erf(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_erf(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_erfc(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_erfc(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}


/*******************************************************************************
 * //! Assigns s = sin(a).
 * CWRAPPER_OUTPUT_TYPE basic_sin(basic s, const basic a);
 * //! Assigns s = cos(a).
 * CWRAPPER_OUTPUT_TYPE basic_cos(basic s, const basic a);
 * //! Assigns s = tan(a).
 * CWRAPPER_OUTPUT_TYPE basic_tan(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_sin(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_sin(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_cos(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_cos(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_tan(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_tan(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

/*******************************************************************************
 * //! Assigns s = asin(a).
 * CWRAPPER_OUTPUT_TYPE basic_asin(basic s, const basic a);
 * //! Assigns s = acos(a).
 * CWRAPPER_OUTPUT_TYPE basic_acos(basic s, const basic a);
 * //! Assigns s = atan(a).
 * CWRAPPER_OUTPUT_TYPE basic_atan(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_asin(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_asin(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_acos(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_acos(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_atan(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_atan(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}


/*******************************************************************************
 * //! Assigns s = csc(a).
 * CWRAPPER_OUTPUT_TYPE basic_csc(basic s, const basic a);
 * //! Assigns s = sec(a).
 * CWRAPPER_OUTPUT_TYPE basic_sec(basic s, const basic a);
 * //! Assigns s = cot(a).
 * CWRAPPER_OUTPUT_TYPE basic_cot(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_csc(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_csc(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_sec(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_sec(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_cot(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_cot(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}


/*******************************************************************************
 * //! Assigns s = acsc(a).
 * CWRAPPER_OUTPUT_TYPE basic_acsc(basic s, const basic a);
 * //! Assigns s = asec(a).
 * CWRAPPER_OUTPUT_TYPE basic_asec(basic s, const basic a);
 * //! Assigns s = acot(a).
 * CWRAPPER_OUTPUT_TYPE basic_acot(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_acsc(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_acsc(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_asec(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_asec(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_acot(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_acot(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}


/*******************************************************************************
 * //! Assigns s = sinh(a).
 * CWRAPPER_OUTPUT_TYPE basic_sinh(basic s, const basic a);
 * //! Assigns s = cosh(a).
 * CWRAPPER_OUTPUT_TYPE basic_cosh(basic s, const basic a);
 * //! Assigns s = tanh(a).
 * CWRAPPER_OUTPUT_TYPE basic_tanh(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_sinh(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_sinh(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_cosh(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_cosh(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_tanh(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_tanh(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

/*******************************************************************************
 * //! Assigns s = asinh(a).
 * CWRAPPER_OUTPUT_TYPE basic_asinh(basic s, const basic a);
 * //! Assigns s = acosh(a).
 * CWRAPPER_OUTPUT_TYPE basic_acosh(basic s, const basic a);
 * //! Assigns s = atanh(a).
 * CWRAPPER_OUTPUT_TYPE basic_atanh(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_asinh(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_asinh(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_acosh(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_acosh(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_atanh(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_atanh(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

/*******************************************************************************
 * //! Assigns s = csch(a).
 * CWRAPPER_OUTPUT_TYPE basic_csch(basic s, const basic a);
 * //! Assigns s = sech(a).
 * CWRAPPER_OUTPUT_TYPE basic_sech(basic s, const basic a);
 * //! Assigns s = coth(a).
 * CWRAPPER_OUTPUT_TYPE basic_coth(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_csch(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_csch(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_sech(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_sech(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_coth(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_coth(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

/*******************************************************************************
 * //! Assigns s = acsch(a).
 * CWRAPPER_OUTPUT_TYPE basic_acsch(basic s, const basic a);
 * //! Assigns s = asech(a).
 * CWRAPPER_OUTPUT_TYPE basic_asech(basic s, const basic a);
 * //! Assigns s = acoth(a).
 * CWRAPPER_OUTPUT_TYPE basic_acoth(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_acsch(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_acsch(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_asech(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_asech(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_acoth(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_acoth(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}


/*******************************************************************************
 * //! Assigns s = lambertw(a).
 * CWRAPPER_OUTPUT_TYPE basic_lambertw(basic s, const basic a);
 * //! Assigns s = zeta(a).
 * CWRAPPER_OUTPUT_TYPE basic_zeta(basic s, const basic a);
 * //! Assigns s = dirichlet_eta(a).
 * CWRAPPER_OUTPUT_TYPE basic_dirichlet_eta(basic s, const basic a);
 * //! Assigns s = gamma(a).
 * CWRAPPER_OUTPUT_TYPE basic_gamma(basic s, const basic a);
 * //! Assigns s = sqrt(a).
 * CWRAPPER_OUTPUT_TYPE basic_sqrt(basic s, const basic a);
 * //! Assigns s = exp(a).
 * CWRAPPER_OUTPUT_TYPE basic_exp(basic s, const basic a);
 * //! Assigns s = log(a).
 * CWRAPPER_OUTPUT_TYPE basic_log(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_lambertw(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_lambertw(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_zeta(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_zeta(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_dirichlet_eta(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_dirichlet_eta(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_gamma(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_gamma(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_sqrt(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_sqrt(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_exp(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_exp(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

SEXP c_basic_log(SEXP exta) {
    if (NULL == R_ExternalPtrAddr(exta))
        Rf_error("Invalid pointer");
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_log(s, a);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}


/*******************************************************************************
 * //! substitutes a basic 'a' with another basic 'b',
 * //! in the given basic 'e' and returns it through basic 's'
 * CWRAPPER_OUTPUT_TYPE basic_subs2(basic s, const basic e, const basic a, const basic b);
 *******************************************************************************/

SEXP c_basic_subs2(SEXP exte, SEXP exta, SEXP extb) {
    if (NULL == R_ExternalPtrAddr(exte) || NULL == R_ExternalPtrAddr(exta) ||
        NULL == R_ExternalPtrAddr(extb))
        Rf_error("Invalid pointer");
    basic_struct* e = (basic_struct*) R_ExternalPtrAddr(exte);
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_subs2(s, e, a, b);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}

/*******************************************************************************
 * //! Evaluate b and assign the value to s
 * CWRAPPER_OUTPUT_TYPE basic_evalf(basic s, const basic b, unsigned long bits, int real);
 *******************************************************************************/

SEXP c_basic_evalf(SEXP extb, SEXP bits, SEXP real) {
    if (NULL == R_ExternalPtrAddr(extb))
        Rf_error("Invalid pointer");
    unsigned long n_bits = Rf_asInteger(bits);
    int           i_real = Rf_asLogical(real);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    basic_struct* s = basic_new_heap();

    CWRAPPER_OUTPUT_TYPE exception = basic_evalf(s, b, n_bits, i_real);
    if (exception)
        Rf_error(exception_message(exception));

    SEXP outptr = PROTECT(R_MakeExternalPtr(s, Rf_mkString("basic_struct*"), R_NilValue));
    R_RegisterCFinalizerEx(outptr, _basic_heap_finalizer, TRUE);

    UNPROTECT(1);
    return outptr;
}


