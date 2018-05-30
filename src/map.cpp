#define R_NO_REMAP
#include <R.h>
#include <Rcpp.h>
#include <symengine/cwrapper.h>
extern "C" {
  #include "utils.h"
}

// [[Rcpp::export(".mapbasic")]]
SEXP sexp_mapbasic(SEXP key, SEXP mapped) {
    SEXP            out  = PROTECT(sexp_mapbasic_s4());
    CMapBasicBasic* map  = elt_mapbasic(out);
    CVecBasic*      vkey = elt_vecbasic(key);
    CVecBasic*      vmap = elt_vecbasic(mapped);
    SEXP            a    = PROTECT(sexp_basic());
    SEXP            b    = PROTECT(sexp_basic());
    basic_struct*   vala = elt_basic(a);
    basic_struct*   valb = elt_basic(b);
    size_t          len  = vecbasic_size(vkey);
    
    for (size_t i = 0; i < len; i++) {
        hold_exception(vecbasic_get(vkey, i, vala));
        hold_exception(vecbasic_get(vmap, i, valb));
        mapbasicbasic_insert(map, vala, valb);
    }

    UNPROTECT(3);
    return out;
}

// [[Rcpp::export(".mapbasic_length")]]
size_t sexp_mapbasic_length(SEXP ext) {
    CMapBasicBasic* map = elt_mapbasic(ext);
    size_t          sz  = mapbasicbasic_size(map);
    return sz;
}

// [[Rcpp::export(".mapbasic_get")]]
SEXP sexp_mapbasic_get(SEXP ext1, SEXP ext2) {
    CMapBasicBasic* map    = elt_mapbasic(ext1);
    basic_struct*   key    = elt_basic(ext2);
    SEXP            out    = PROTECT(sexp_basic_s4());
    basic_struct*   mapped = elt_basic(out);
    int             flag   = mapbasicbasic_get(map, key, mapped);
    
    UNPROTECT(1);
    
    if (!flag)
      Rf_error("key doesn't exist");
    
    return out;
}