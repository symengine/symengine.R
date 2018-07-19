
#define R_NO_REMAP
#include <R.h>
#include <Rcpp.h>
#include <symengine/cwrapper.h>

extern "C" {
  #include "utils.h"
}

// Accessors // ================================================================

// [[Rcpp::export(".basic_type")]]
SEXP sexp_basic_type(SEXP ext) {
    sexp_check_basic(ext);

    basic_struct* symbol = (basic_struct*) R_ExternalPtrAddr(ext);

    TypeID type_id = basic_get_type(symbol);
    char* classname = basic_get_class_from_id(type_id);

    return Rf_mkString(classname);
}

// [[Rcpp::export(".basic_str")]]
SEXP sexp_basic_str(SEXP ext) {
    sexp_check_basic(ext);
    basic_struct* symbol = (basic_struct*) R_ExternalPtrAddr(ext);

    char* str = basic_str_julia(symbol);
    SEXP out = PROTECT(Rf_mkString(str));
    basic_str_free(str);

    UNPROTECT(1);
    return(out);
}

// [[Rcpp::export(".basic_hash")]]
SEXP sexp_basic_hash(SEXP ext) {
    sexp_check_basic(ext);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    size_t hash = basic_hash(b);
    char str[256] = "";
    snprintf(str, sizeof(str), "%zu", hash);
    return Rf_mkString(str);
}

// [[Rcpp::export(".basic_eq")]]
SEXP sexp_basic_eq(SEXP exta, SEXP extb) {
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    return Rf_ScalarLogical(basic_eq(a, b));
}

// [[Rcpp::export(".basic_neq")]]
SEXP sexp_basic_neq(SEXP exta, SEXP extb) {
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    return Rf_ScalarLogical(basic_neq(a, b));
}


// get_args and free_symbols //=============
// They work for basic but depends on vecbasic

//! Returns a CVecBasic of vec_basic given by get_args
CWRAPPER_OUTPUT_TYPE basic_get_args(const basic self, CVecBasic *args);
//! Returns a CSetBasic of set_basic given by free_symbols
CWRAPPER_OUTPUT_TYPE basic_free_symbols(const basic self, CSetBasic *symbols);
//! Returns a CSetBasic of set_basic given by function_symbols
CWRAPPER_OUTPUT_TYPE basic_function_symbols(CSetBasic *symbols, const basic self);

// [[Rcpp::export(".basic_get_args")]]
SEXP sexp_basic_get_args(SEXP ext) {
    basic_struct* b = elt_basic(ext);
    SEXP out = PROTECT(sexp_vecbasic_s4());
    CVecBasic* outv = elt_vecbasic(out);
    
    hold_exception(basic_get_args(b, outv));
    
    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".basic_free_symbols")]]
SEXP sexp_basic_free_symbols(SEXP ext) {
    basic_struct* b = elt_basic(ext);
    SEXP out  = PROTECT(sexp_setbasic());
    CSetBasic* outv = elt_setbasic(out);
    
    hold_exception(basic_free_symbols(b, outv));
    
    UNPROTECT(1);
    return sexp_set2vec_s4(out);
}

// [[Rcpp::export(".basic_function_symbols")]]
SEXP sexp_basic_function_symbols(SEXP ext) {
    basic_struct* b = elt_basic(ext);
    SEXP out = PROTECT(sexp_setbasic());
    CSetBasic* outv = elt_setbasic(out);
    
    // Note that the arguments of basic_function_symbols are different from the two above
    hold_exception(basic_function_symbols(outv, b));
    
    UNPROTECT(1);
    return sexp_set2vec_s4(out);
}




// Symbol // ===================================================================

// [[Rcpp::export(".basic_symbol")]]
SEXP sexp_basic_symbol(SEXP RString) {
    const char* str_symbol = CHAR(Rf_asChar(RString));

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(symbol_set(s, str_symbol));

    UNPROTECT(1);
    return out;
}

// Parser // ===================================================================

// [[Rcpp::export(".basic_parse")]]
SEXP sexp_basic_parse(SEXP RString) {
    const char* str = CHAR(Rf_asChar(RString));

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(basic_parse2(s, str, 1));

    UNPROTECT(1);
    return out;
}

// Constants //=================================================================

// [[Rcpp::export(".basic_const")]]
SEXP sexp_basic_const(SEXP string) {
    const char* str = CHAR(Rf_asChar(string));

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    basic_const_set(s, str);

    UNPROTECT(1);
    return out;
}


static inline
SEXP call_get_const(void (* func)(basic)) {
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    func(s);

    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".basic_const_zero")]]
SEXP sexp_const_zero()             { return call_get_const(basic_const_zero); }
// [[Rcpp::export(".basic_const_one")]]
SEXP sexp_const_one()              { return call_get_const(basic_const_one); }
// [[Rcpp::export(".basic_const_minus_one")]]
SEXP sexp_const_minus_one()        { return call_get_const(basic_const_minus_one); }
// [[Rcpp::export(".basic_const_I")]]
SEXP sexp_const_I()                { return call_get_const(basic_const_I); }
// [[Rcpp::export(".basic_const_pi")]]
SEXP sexp_const_pi()               { return call_get_const(basic_const_pi); }
// [[Rcpp::export(".basic_const_E")]]
SEXP sexp_const_E()                { return call_get_const(basic_const_E); }
// [[Rcpp::export(".basic_const_EulerGamma")]]
SEXP sexp_const_EulerGamma()       { return call_get_const(basic_const_EulerGamma); }
// [[Rcpp::export(".basic_const_Catalan")]]
SEXP sexp_const_Catalan()          { return call_get_const(basic_const_Catalan); }
// [[Rcpp::export(".basic_const_GoldenRatio")]]
SEXP sexp_const_GoldenRatio()      { return call_get_const(basic_const_GoldenRatio); }
// [[Rcpp::export(".basic_const_infinity")]]
SEXP sexp_const_infinity()         { return call_get_const(basic_const_infinity); }
// [[Rcpp::export(".basic_const_neginfinity")]]
SEXP sexp_const_neginfinity()      { return call_get_const(basic_const_neginfinity); }
// [[Rcpp::export(".basic_const_complex_infinity")]]
SEXP sexp_const_complex_infinity() { return call_get_const(basic_const_complex_infinity); }
// [[Rcpp::export(".basic_const_nan")]]
SEXP sexp_const_nan()              { return call_get_const(basic_const_nan); }


// Integer //===================================================================


// [[Rcpp::export(".basic_integer_fromint")]]
SEXP sexp_basic_integer_fromint(SEXP x) {
    int i = Rf_asInteger(x);

    if (i == R_NaInt) // R_NaInt == INT_MIN
        Rf_error("NA value is not allowed");

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    // CWRAPPER_OUTPUT_TYPE integer_set_si(basic s, long i);
    hold_exception(integer_set_si(s, i));

    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".basic_integer_fromstr")]]
SEXP sexp_basic_integer_fromstr(SEXP RString) {
    const char* str = CHAR(Rf_asChar(RString));

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    // CWRAPPER_OUTPUT_TYPE integer_set_str(basic s, const char *c);
    hold_exception(integer_set_str(s, str));

    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".basic_integer_getint")]]
SEXP sexp_basic_integer_getint(SEXP ext) {
    
    sexp_check_basic(ext);

    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    signed long si = integer_get_si(b);
    // Note that INT_MIN is used as NA_integer_ in R
    if (si >= (INT_MIN + 1) && si <= INT_MAX)
        return Rf_ScalarInteger(si);
    else {
        // TODO
        Rf_error("Number %ld can not be coerced to integer range", si);
    }
}

// Real    //===================================================================

// [[Rcpp::export(".basic_realdouble")]]
SEXP sexp_basic_realdouble(SEXP x) {
    double d = Rf_asReal(x);

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(real_double_set_d(s, d));

    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".basic_realdouble_getd")]]
SEXP sexp_basic_realdouble_getd(SEXP ext) {
    sexp_check_basic(ext);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarReal(real_double_get_d(b));
}


// Basic: is_a_XXX  //============================================================

static inline
SEXP call_basic_is_xxx(SEXP ext, int (* func)(const basic)) {
    sexp_check_basic(ext);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(func(b));
}


// [[Rcpp::export(".basic_isNumber")]]
SEXP sexp_basic_isNumber(SEXP ext)        {return call_basic_is_xxx(ext, is_a_Number);}
// [[Rcpp::export(".basic_isInteger")]]
SEXP sexp_basic_isInteger(SEXP ext)       {return call_basic_is_xxx(ext, is_a_Integer);}
// [[Rcpp::export(".basic_isRational")]]
SEXP sexp_basic_isRational(SEXP ext)      {return call_basic_is_xxx(ext, is_a_Rational);}
// [[Rcpp::export(".basic_isSymbol")]]
SEXP sexp_basic_isSymbol(SEXP ext)        {return call_basic_is_xxx(ext, is_a_Symbol);}
// [[Rcpp::export(".basic_isComplex")]]
SEXP sexp_basic_isComplex(SEXP ext)       {return call_basic_is_xxx(ext, is_a_Complex);}
// [[Rcpp::export(".basic_isRealDouble")]]
SEXP sexp_basic_isRealDouble(SEXP ext)    {return call_basic_is_xxx(ext, is_a_RealDouble);}
// [[Rcpp::export(".basic_isComplexDouble")]]
SEXP sexp_basic_isComplexDouble(SEXP ext) {return call_basic_is_xxx(ext, is_a_ComplexDouble);}
// [[Rcpp::export(".basic_isRealMPFR")]]
SEXP sexp_basic_isRealMPFR(SEXP ext)      {return call_basic_is_xxx(ext, is_a_RealMPFR);}
// [[Rcpp::export(".basic_isComplexMPC")]]
SEXP sexp_basic_isComplexMPC(SEXP ext)    {return call_basic_is_xxx(ext, is_a_ComplexMPC);}

// [[Rcpp::export(".basic_num_iszero")]]
SEXP sexp_basic_num_iszero(SEXP ext)     {return call_basic_is_xxx(ext, number_is_zero);}
// [[Rcpp::export(".basic_num_isnegative")]]
SEXP sexp_basic_num_isnegative(SEXP ext) {return call_basic_is_xxx(ext, number_is_negative);}
// [[Rcpp::export(".basic_num_ispositive")]]
SEXP sexp_basic_num_ispositive(SEXP ext) {return call_basic_is_xxx(ext, number_is_positive);}
// [[Rcpp::export(".basic_num_iscomplex")]]
SEXP sexp_basic_num_iscomplex(SEXP ext)  {return call_basic_is_xxx(ext, number_is_complex);}



