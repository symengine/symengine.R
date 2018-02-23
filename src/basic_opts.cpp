
#define R_NO_REMAP
#include <R.h>
#include <Rcpp.h>
#include <symengine/cwrapper.h>

extern "C" {
#include "utils.h"
}

// Operations //=========================================================

static inline
SEXP call_twoarg_opt(CWRAPPER_OUTPUT_TYPE (* func)(basic, const basic, const basic),
                          SEXP exta, SEXP extb) {
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* a   = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b   = (basic_struct*) R_ExternalPtrAddr(extb);
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(func(s, a, b));

    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".basic_add")]]
SEXP sexp_basic_add(SEXP exta, SEXP extb) {return call_twoarg_opt(basic_add, exta, extb);}
// [[Rcpp::export(".basic_sub")]]
SEXP sexp_basic_sub(SEXP exta, SEXP extb) {return call_twoarg_opt(basic_sub, exta, extb);}
// [[Rcpp::export(".basic_mul")]]
SEXP sexp_basic_mul(SEXP exta, SEXP extb) {return call_twoarg_opt(basic_mul, exta, extb);}
// [[Rcpp::export(".basic_div")]]
SEXP sexp_basic_div(SEXP exta, SEXP extb) {return call_twoarg_opt(basic_div, exta, extb);}
// [[Rcpp::export(".basic_pow")]]
SEXP sexp_basic_pow(SEXP exta, SEXP extb) {return call_twoarg_opt(basic_pow, exta, extb);}

// [[Rcpp::export(".basic_diff")]]
SEXP sexp_basic_diff(SEXP exta, SEXP extb) {return call_twoarg_opt(basic_diff, exta, extb);}


// Here we also do conversion from S4 to externalptr, and initialization of the output
// S4 object. In other functions, they are done in the R level.
static inline
SEXP wrap_basic_func_onearg(SEXP exta,
                            CWRAPPER_OUTPUT_TYPE (* func)(basic, const basic)) {
    sexp_check_basic(exta);
    basic_struct* a   = (basic_struct*) R_ExternalPtrAddr(exta);
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(func(s, a));

    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".basic_expand")]]
SEXP sexp_basic_expand  (SEXP exta) {return wrap_basic_func_onearg(exta, basic_expand);}
// [[Rcpp::export(".basic_neg")]]
SEXP sexp_basic_neg     (SEXP exta) {return wrap_basic_func_onearg(exta, basic_neg);}
// [[Rcpp::export(".basic_abs")]]
SEXP sexp_basic_abs     (SEXP exta) {return wrap_basic_func_onearg(exta, basic_abs);}
// [[Rcpp::export(".basic_erf")]]
SEXP sexp_basic_erf     (SEXP exta) {return wrap_basic_func_onearg(exta, basic_erf);}
// [[Rcpp::export(".basic_erfc")]]
SEXP sexp_basic_erfc    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_erfc);}
// [[Rcpp::export(".basic_sin")]]
SEXP sexp_basic_sin     (SEXP exta) {return wrap_basic_func_onearg(exta, basic_sin);}
// [[Rcpp::export(".basic_cos")]]
SEXP sexp_basic_cos     (SEXP exta) {return wrap_basic_func_onearg(exta, basic_cos);}
// [[Rcpp::export(".basic_tan")]]
SEXP sexp_basic_tan     (SEXP exta) {return wrap_basic_func_onearg(exta, basic_tan);}
// [[Rcpp::export(".basic_asin")]]
SEXP sexp_basic_asin    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_asin);}
// [[Rcpp::export(".basic_acos")]]
SEXP sexp_basic_acos    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_acos);}
// [[Rcpp::export(".basic_atan")]]
SEXP sexp_basic_atan    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_atan);}
// [[Rcpp::export(".basic_csc")]]
SEXP sexp_basic_csc     (SEXP exta) {return wrap_basic_func_onearg(exta, basic_csc);}
// [[Rcpp::export(".basic_sec")]]
SEXP sexp_basic_sec     (SEXP exta) {return wrap_basic_func_onearg(exta, basic_sec);}
// [[Rcpp::export(".basic_cot")]]
SEXP sexp_basic_cot     (SEXP exta) {return wrap_basic_func_onearg(exta, basic_cot);}
// [[Rcpp::export(".basic_acsc")]]
SEXP sexp_basic_acsc    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_acsc);}
// [[Rcpp::export(".basic_asec")]]
SEXP sexp_basic_asec    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_asec);}
// [[Rcpp::export(".basic_acot")]]
SEXP sexp_basic_acot    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_acot);}
// [[Rcpp::export(".basic_sinh")]]
SEXP sexp_basic_sinh    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_sinh);}
// [[Rcpp::export(".basic_cosh")]]
SEXP sexp_basic_cosh    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_cosh);}
// [[Rcpp::export(".basic_tanh")]]
SEXP sexp_basic_tanh    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_tanh);}
// [[Rcpp::export(".basic_asinh")]]
SEXP sexp_basic_asinh   (SEXP exta) {return wrap_basic_func_onearg(exta, basic_asinh);}
// [[Rcpp::export(".basic_acosh")]]
SEXP sexp_basic_acosh   (SEXP exta) {return wrap_basic_func_onearg(exta, basic_acosh);}
// [[Rcpp::export(".basic_atanh")]]
SEXP sexp_basic_atanh   (SEXP exta) {return wrap_basic_func_onearg(exta, basic_atanh);}
// [[Rcpp::export(".basic_csch")]]
SEXP sexp_basic_csch    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_csch);}
// [[Rcpp::export(".basic_sech")]]
SEXP sexp_basic_sech    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_sech);}
// [[Rcpp::export(".basic_coth")]]
SEXP sexp_basic_coth    (SEXP exta) {return wrap_basic_func_onearg(exta, basic_coth);}
// [[Rcpp::export(".basic_acsch")]]
SEXP sexp_basic_acsch   (SEXP exta) {return wrap_basic_func_onearg(exta, basic_acsch);}
// [[Rcpp::export(".basic_asech")]]
SEXP sexp_basic_asech   (SEXP exta) {return wrap_basic_func_onearg(exta, basic_asech);}
// [[Rcpp::export(".basic_acoth")]]
SEXP sexp_basic_acoth   (SEXP exta) {return wrap_basic_func_onearg(exta, basic_acoth);}
// [[Rcpp::export(".basic_lambertw")]]
SEXP sexp_basic_lambertw      (SEXP exta) {return wrap_basic_func_onearg(exta, basic_lambertw);}
// [[Rcpp::export(".basic_zeta")]]
SEXP sexp_basic_zeta          (SEXP exta) {return wrap_basic_func_onearg(exta, basic_zeta);}
// [[Rcpp::export(".basic_dirichlet_eta")]]
SEXP sexp_basic_dirichlet_eta (SEXP exta) {return wrap_basic_func_onearg(exta, basic_dirichlet_eta);}
// [[Rcpp::export(".basic_gamma")]]
SEXP sexp_basic_gamma         (SEXP exta) {return wrap_basic_func_onearg(exta, basic_gamma);}
// [[Rcpp::export(".basic_sqrt")]]
SEXP sexp_basic_sqrt          (SEXP exta) {return wrap_basic_func_onearg(exta, basic_sqrt);}
// [[Rcpp::export(".basic_exp")]]
SEXP sexp_basic_exp           (SEXP exta) {return wrap_basic_func_onearg(exta, basic_exp);}
// [[Rcpp::export(".basic_log")]]
SEXP sexp_basic_log           (SEXP exta) {return wrap_basic_func_onearg(exta, basic_log);}



// [[Rcpp::export(".basic_subs2")]]
SEXP sexp_basic_subs2(SEXP exte, SEXP exta, SEXP extb) {
    sexp_check_basic(exte);
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* e = (basic_struct*) R_ExternalPtrAddr(exte);
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(basic_subs2(s, e, a, b));

    UNPROTECT(1);
    return out;
}

// [[Rcpp::export(".basic_evalf")]]
SEXP sexp_basic_evalf(SEXP extb, SEXP bits, SEXP real) {
    sexp_check_basic(extb);
    unsigned long n_bits = Rf_asInteger(bits);
    int           i_real = Rf_asLogical(real);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(basic_evalf(s, b, n_bits, i_real));

    UNPROTECT(1);
    return out;
}

