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
// [[Rcpp::export(".vecbasic_quotient")]]
SEXP sexp_vecbasic_quotient(SEXP vec1, SEXP vec2)  { return call_twoarg_opt(ntheory_quotient, vec1, vec2); }
// [[Rcpp::export(".vecbasic_mod_f")]]
SEXP sexp_vecbasic_mod(SEXP vec1, SEXP vec2)  { return call_twoarg_opt(ntheory_mod_f, vec1, vec2); }
// [[Rcpp::export(".vecbasic_pow")]]
SEXP sexp_vecbasic_pow(SEXP vec1, SEXP vec2)  { return call_twoarg_opt(basic_pow,  vec1, vec2); }
// [[Rcpp::export(".vecbasic_diff")]]
SEXP sexp_vecbasic_diff(SEXP vec1, SEXP vec2) { return call_twoarg_opt(basic_diff, vec1, vec2); }

static inline
SEXP wrap_basic_func_onearg(SEXP vec,
                            CWRAPPER_OUTPUT_TYPE (* func)(basic, const basic)) {
    CVecBasic*    inv  = elt_vecbasic(vec);
    size_t        len  = vecbasic_size(inv);
    SEXP          out  = PROTECT(sexp_vecbasic_s4());
    CVecBasic*    outv = elt_vecbasic(out);
    SEXP          ta   = PROTECT(sexp_basic());
    SEXP          tb   = PROTECT(sexp_basic());
    basic_struct* pa   = elt_basic(ta);
    basic_struct* pb   = elt_basic(tb);

    for (size_t i = 0; i < len; i++) {
        hold_exception(vecbasic_get(inv, i, pa));
        hold_exception(func(pb, pa));
        hold_exception(vecbasic_push_back(outv, pb));
    }

    UNPROTECT(3);
    return out;
}

// [[Rcpp::export(".vecbasic_expand")]]
SEXP sexp_vecbasic_expand        (SEXP vec) {return wrap_basic_func_onearg(vec, basic_expand);}
// [[Rcpp::export(".vecbasic_neg")]]
SEXP sexp_vecbasic_neg           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_neg);}
// [[Rcpp::export(".vecbasic_abs")]]
SEXP sexp_vecbasic_abs           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_abs);}
// [[Rcpp::export(".vecbasic_erf")]]
SEXP sexp_vecbasic_erf           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_erf);}
// [[Rcpp::export(".vecbasic_erfc")]]
SEXP sexp_vecbasic_erfc          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_erfc);}
// [[Rcpp::export(".vecbasic_sin")]]
SEXP sexp_vecbasic_sin           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_sin);}
// [[Rcpp::export(".vecbasic_cos")]]
SEXP sexp_vecbasic_cos           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_cos);}
// [[Rcpp::export(".vecbasic_tan")]]
SEXP sexp_vecbasic_tan           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_tan);}
// [[Rcpp::export(".vecbasic_asin")]]
SEXP sexp_vecbasic_asin          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_asin);}
// [[Rcpp::export(".vecbasic_acos")]]
SEXP sexp_vecbasic_acos          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_acos);}
// [[Rcpp::export(".vecbasic_atan")]]
SEXP sexp_vecbasic_atan          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_atan);}
// [[Rcpp::export(".vecbasic_csc")]]
SEXP sexp_vecbasic_csc           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_csc);}
// [[Rcpp::export(".vecbasic_sec")]]
SEXP sexp_vecbasic_sec           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_sec);}
// [[Rcpp::export(".vecbasic_cot")]]
SEXP sexp_vecbasic_cot           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_cot);}
// [[Rcpp::export(".vecbasic_acsc")]]
SEXP sexp_vecbasic_acsc          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_acsc);}
// [[Rcpp::export(".vecbasic_asec")]]
SEXP sexp_vecbasic_asec          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_asec);}
// [[Rcpp::export(".vecbasic_acot")]]
SEXP sexp_vecbasic_acot          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_acot);}
// [[Rcpp::export(".vecbasic_sinh")]]
SEXP sexp_vecbasic_sinh          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_sinh);}
// [[Rcpp::export(".vecbasic_cosh")]]
SEXP sexp_vecbasic_cosh          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_cosh);}
// [[Rcpp::export(".vecbasic_tanh")]]
SEXP sexp_vecbasic_tanh          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_tanh);}
// [[Rcpp::export(".vecbasic_asinh")]]
SEXP sexp_vecbasic_asinh         (SEXP vec) {return wrap_basic_func_onearg(vec, basic_asinh);}
// [[Rcpp::export(".vecbasic_acosh")]]
SEXP sexp_vecbasic_acosh         (SEXP vec) {return wrap_basic_func_onearg(vec, basic_acosh);}
// [[Rcpp::export(".vecbasic_atanh")]]
SEXP sexp_vecbasic_atanh         (SEXP vec) {return wrap_basic_func_onearg(vec, basic_atanh);}
// [[Rcpp::export(".vecbasic_csch")]]
SEXP sexp_vecbasic_csch          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_csch);}
// [[Rcpp::export(".vecbasic_sech")]]
SEXP sexp_vecbasic_sech          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_sech);}
// [[Rcpp::export(".vecbasic_coth")]]
SEXP sexp_vecbasic_coth          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_coth);}
// [[Rcpp::export(".vecbasic_acsch")]]
SEXP sexp_vecbasic_acsch         (SEXP vec) {return wrap_basic_func_onearg(vec, basic_acsch);}
// [[Rcpp::export(".vecbasic_asech")]]
SEXP sexp_vecbasic_asech         (SEXP vec) {return wrap_basic_func_onearg(vec, basic_asech);}
// [[Rcpp::export(".vecbasic_acoth")]]
SEXP sexp_vecbasic_acoth         (SEXP vec) {return wrap_basic_func_onearg(vec, basic_acoth);}
// [[Rcpp::export(".vecbasic_lambertw")]]
SEXP sexp_vecbasic_lambertw      (SEXP vec) {return wrap_basic_func_onearg(vec, basic_lambertw);}
// [[Rcpp::export(".vecbasic_zeta")]]
SEXP sexp_vecbasic_zeta          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_zeta);}
// [[Rcpp::export(".vecbasic_dirichlet_eta")]]
SEXP sexp_vecbasic_dirichlet_eta (SEXP vec) {return wrap_basic_func_onearg(vec, basic_dirichlet_eta);}
// [[Rcpp::export(".vecbasic_gamma")]]
SEXP sexp_vecbasic_gamma         (SEXP vec) {return wrap_basic_func_onearg(vec, basic_gamma);}
// [[Rcpp::export(".vecbasic_sqrt")]]
SEXP sexp_vecbasic_sqrt          (SEXP vec) {return wrap_basic_func_onearg(vec, basic_sqrt);}
// [[Rcpp::export(".vecbasic_exp")]]
SEXP sexp_vecbasic_exp           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_exp);}
// [[Rcpp::export(".vecbasic_log")]]
SEXP sexp_vecbasic_log           (SEXP vec) {return wrap_basic_func_onearg(vec, basic_log);}