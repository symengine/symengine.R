
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


// Symbol // ===================================================================

// [[Rcpp::export(".Symbol")]]
SEXP sexp_basic_symbol(SEXP RString) {
    const char* str_symbol = CHAR(Rf_asChar(RString));

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(symbol_set(s, str_symbol));

    UNPROTECT(1);
    return out;
}

