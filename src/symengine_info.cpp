
#define R_NO_REMAP
#include <Rcpp.h>
#include <symengine/cwrapper.h>


// [[Rcpp::export(".symengine_ascii_art")]]
SEXP sexp_symengine_ascii_art() {
    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    char* s = ascii_art_str();
    out = Rf_mkString(s);
    basic_str_free(s);
    UNPROTECT(1);
    return out;
}

// [[Rcpp::export("symengine_version")]]
SEXP sexp_symengine_version() {
    return Rf_mkString(symengine_version());
}

// [[Rcpp::export(".symengine_have_component")]]
SEXP sexp_symengine_have_component(SEXP s) {
    const char* str = CHAR(Rf_asChar(s));
    return Rf_ScalarLogical(symengine_have_component(str));
}

// [[Rcpp::export(".notes")]]
SEXP notes() {
    return Rcpp::List::create(
        Rcpp::Named("CompilationDate") = __DATE__
    );
}

