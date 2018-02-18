
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

// [[Rcpp::export("basic_parse")]]
SEXP sexp_basic_parse(SEXP RString) {
    const char* str = CHAR(Rf_asChar(RString));

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(basic_parse2(s, str, 1));

    UNPROTECT(1);
    return out;
}

// Constants //=================================================================

// [[Rcpp::export("basic_const")]]
SEXP sexp_basic_const(SEXP string) {
    const char* str = CHAR(Rf_asChar(string));

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    basic_const_set(s, str);

    UNPROTECT(1);
    return out;
}



