#define R_NO_REMAP
#include <R.h>
#include <Rcpp.h>
#include <symengine/cwrapper.h>

extern "C" {
    #include "utils.h"
}

static inline void _vecbasic_next_id(const int n, int& i) {
    i = i + 1;
    if (i == n) i = 0;
}

static inline
SEXP call_twoarg_opt(CWRAPPER_OUTPUT_TYPE (* func)(basic, const basic, const basic),
                          SEXP vec1, SEXP vec2) {
    SEXP          out  = PROTECT(sexp_vecbasic_s4());
    SEXP          tans = PROTECT(sexp_basic());
    SEXP          ta   = PROTECT(sexp_basic());
    SEXP          tb   = PROTECT(sexp_basic());
    CVecBasic*    pout = elt_vecbasic(out);
    CVecBasic*    pv1  = elt_vecbasic(vec1);
    CVecBasic*    pv2  = elt_vecbasic(vec2);
    basic_struct* pans = elt_basic(tans);
    basic_struct* pa   = elt_basic(ta);
    basic_struct* pb   = elt_basic(tb);
    int           lv1  = vecbasic_size(pv1);
    int           lv2  = vecbasic_size(pv2);
    int           lout = std::max(lv1, lv2);
    for (int i = 0, id1 = 0, id2 = 0; i < lout; i++) {
        hold_exception(vecbasic_get(pv1, id1, pa));
        hold_exception(vecbasic_get(pv2, id2, pb));
        hold_exception(func(pans, pa, pb));
        hold_exception(vecbasic_push_back(pout, pans));
        _vecbasic_next_id(lv1, id1);
        _vecbasic_next_id(lv2, id2);
    }

    UNPROTECT(4);
    return out;
}


// [[Rcpp::export(".vecbasic_add")]]
SEXP sexp_vecbasic_add(SEXP vec1, SEXP vec2)  { return call_twoarg_opt(basic_add,  vec1, vec2); }
// [[Rcpp::export(".vecbasic_sub")]]
SEXP sexp_vecbasic_sub(SEXP vec1, SEXP vec2)  { return call_twoarg_opt(basic_sub,  vec1, vec2); }
// [[Rcpp::export(".vecbasic_mul")]]
SEXP sexp_vecbasic_mul(SEXP vec1, SEXP vec2)  { return call_twoarg_opt(basic_mul,  vec1, vec2); }
// [[Rcpp::export(".vecbasic_div")]]
SEXP sexp_vecbasic_div(SEXP vec1, SEXP vec2)  { return call_twoarg_opt(basic_div,  vec1, vec2); }
// [[Rcpp::export(".vecbasic_pow")]]
SEXP sexp_vecbasic_pow(SEXP vec1, SEXP vec2)  { return call_twoarg_opt(basic_pow,  vec1, vec2); }
// [[Rcpp::export(".vecbasic_diff")]]
SEXP sexp_vecbasic_diff(SEXP vec1, SEXP vec2) { return call_twoarg_opt(basic_diff, vec1, vec2); }



