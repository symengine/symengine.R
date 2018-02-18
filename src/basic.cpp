
#define R_NO_REMAP
#include <Rcpp.h>
#include <symengine/cwrapper.h>

extern "C" {
  #include "utils.h"
}


// Symbol ======================================================================

// [[Rcpp::export(".Symbol")]]
SEXP sexp_basic_symbol(SEXP RString) {
    const char* str_symbol = CHAR(Rf_asChar(RString));

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(symbol_set(s, str_symbol));

    UNPROTECT(1);
    return out;
}

