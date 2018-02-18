
#define R_NO_REMAP
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


