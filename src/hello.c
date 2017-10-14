
#include <R.h>
#include <Rinternals.h>

#include <gmp.h>
#include <symengine/cwrapper.h>

SEXP c_ascii_art_str() {
    SEXP out = PROTECT(allocVector(STRSXP, 1));
    const char* s = ascii_art_str();
    out = mkString(s);
    UNPROTECT(1);
    return out;
}

