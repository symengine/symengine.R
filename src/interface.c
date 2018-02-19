
#define R_NO_REMAP
#include <Rinternals.h>
#include <symengine/cwrapper.h>

#include "utils.h"






// Integer  //====================================================================


SEXP c_integer_from_int(SEXP x) {
    int i = Rf_asInteger(x);

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    // CWRAPPER_OUTPUT_TYPE integer_set_si(basic s, long i);
    hold_exception(integer_set_si(s, i));

    UNPROTECT(1);
    return out;
}

SEXP c_integer_from_str(SEXP string) {
    const char* str = CHAR(Rf_asChar(string));

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    // CWRAPPER_OUTPUT_TYPE integer_set_str(basic s, const char *c);
    hold_exception(integer_set_str(s, str));

    UNPROTECT(1);
    return out;
}

SEXP c_integer_get_int(SEXP ext) {
    sexp_check_basic(ext);

    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    // signed long integer_get_si(const basic s);
    signed long si = integer_get_si(b);
    // Note that INT_MIN is used as NA_integer_ in R
    if (si >= (INT_MIN + 1) && si <= INT_MAX)
        return Rf_ScalarInteger(si);
    else {
        // TODO
        Rf_error("Number %ld can not be coerced to integer range", si);
    }
}





// Real  //=======================================================================

SEXP c_realdouble_from_d(SEXP x) {
    double d = Rf_asReal(x);

    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(real_double_set_d(s, d));

    UNPROTECT(1);
    return out;
}

SEXP c_realdouble_get_d(SEXP ext) {
    sexp_check_basic(ext);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarReal(real_double_get_d(b));
}



// The Util Function to Wrap is_a_XXX Functions //================================

static inline
SEXP call_basic_is_xxx(SEXP ext, int (* func)(const basic)) {
    sexp_check_basic(ext);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(ext);
    return Rf_ScalarLogical(func(b));
}

// Basic: is_a_XXX  //============================================================


SEXP c_is_a_Number(SEXP ext) {
    return call_basic_is_xxx(ext, is_a_Number);
}

SEXP c_is_a_Integer(SEXP ext) {
    return call_basic_is_xxx(ext, is_a_Integer);
}

SEXP c_is_a_Rational(SEXP ext) {
    return call_basic_is_xxx(ext, is_a_Rational);
}

SEXP c_is_a_Symbol(SEXP ext) {
    return call_basic_is_xxx(ext, is_a_Symbol);
}

SEXP c_is_a_Complex(SEXP ext) {
    return call_basic_is_xxx(ext, is_a_Complex);
}

SEXP c_is_a_RealDouble(SEXP ext) {
    return call_basic_is_xxx(ext, is_a_RealDouble);
}

SEXP c_is_a_ComplexDouble(SEXP ext) {
    return call_basic_is_xxx(ext, is_a_ComplexDouble);
}

SEXP c_is_a_RealMPFR(SEXP ext) {
    return call_basic_is_xxx(ext, is_a_RealMPFR);
}

SEXP c_is_a_ComplexMPC(SEXP ext) {
    return call_basic_is_xxx(ext, is_a_ComplexMPC);
}


// Number  //===================================================================

SEXP c_number_is_zero(SEXP ext) {
    return call_basic_is_xxx(ext, number_is_zero);
}

SEXP c_number_is_negative(SEXP ext) {
    return call_basic_is_xxx(ext, number_is_negative);
}

SEXP c_number_is_positive(SEXP ext) {
    return call_basic_is_xxx(ext, number_is_positive);
}

SEXP c_number_is_complex(SEXP ext) {
    return call_basic_is_xxx(ext, number_is_complex);
}


// Operations  //==============================================================

/*******************************************************************************
 * //! Assigns s = a + b.
 * CWRAPPER_OUTPUT_TYPE basic_add(basic s, const basic a, const basic b);
 * //! Assigns s = a - b.
 * CWRAPPER_OUTPUT_TYPE basic_sub(basic s, const basic a, const basic b);
 * //! Assigns s = a * b.
 * CWRAPPER_OUTPUT_TYPE basic_mul(basic s, const basic a, const basic b);
 * //! Assigns s = a / b.
 * CWRAPPER_OUTPUT_TYPE basic_div(basic s, const basic a, const basic b);
 * //! Assigns s = a ** b.
 * CWRAPPER_OUTPUT_TYPE basic_pow(basic s, const basic a, const basic b);
 *******************************************************************************/

SEXP c_basic_add(SEXP exta, SEXP extb) {
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* a   = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b   = (basic_struct*) R_ExternalPtrAddr(extb);
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(basic_add(s, a, b));

    UNPROTECT(1);
    return out;
}

SEXP c_basic_sub(SEXP exta, SEXP extb) {
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* a   = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b   = (basic_struct*) R_ExternalPtrAddr(extb);
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(basic_sub(s, a, b));

    UNPROTECT(1);
    return out;
}

SEXP c_basic_mul(SEXP exta, SEXP extb) {
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* a   = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b   = (basic_struct*) R_ExternalPtrAddr(extb);
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(basic_mul(s, a, b));

    UNPROTECT(1);
    return out;
}

SEXP c_basic_div(SEXP exta, SEXP extb) {
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* a   = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b   = (basic_struct*) R_ExternalPtrAddr(extb);
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(basic_div(s, a, b));

    UNPROTECT(1);
    return out;
}

SEXP c_basic_pow(SEXP exta, SEXP extb) {
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* a   = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b   = (basic_struct*) R_ExternalPtrAddr(extb);
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(basic_pow(s, a, b));

    UNPROTECT(1);
    return out;
}


/*******************************************************************************
 * //! Assign to s, derivative of expr with respect to sym.
 * //! Returns SYMENGINE_RUNTIME_ERROR if sym is not a symbol.
 * CWRAPPER_OUTPUT_TYPE basic_diff(basic s, const basic expr, const basic sym);
 *******************************************************************************/

SEXP c_basic_diff(SEXP extexpr, SEXP extsym) {
    sexp_check_basic(extexpr);
    sexp_check_basic(extsym);
    basic_struct* expr = (basic_struct*) R_ExternalPtrAddr(extexpr);
    basic_struct* sym  = (basic_struct*) R_ExternalPtrAddr(extsym);
    SEXP          out  = PROTECT(sexp_basic());
    basic_struct* s    = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(basic_diff(s, expr, sym));

    UNPROTECT(1);
    return out;
}

/*******************************************************************************
 * //! Returns 1 if both basic are equal, 0 if not
 * int basic_eq(const basic a, const basic b);
 * //! Returns 1 if both basic are not equal, 0 if they are
 * int basic_neq(const basic a, const basic b);
 *******************************************************************************/


SEXP c_basic_eq(SEXP exta, SEXP extb) {
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    return Rf_ScalarLogical(basic_eq(a, b));
}

SEXP c_basic_neq(SEXP exta, SEXP extb) {
    sexp_check_basic(exta);
    sexp_check_basic(extb);
    basic_struct* a = (basic_struct*) R_ExternalPtrAddr(exta);
    basic_struct* b = (basic_struct*) R_ExternalPtrAddr(extb);
    return Rf_ScalarLogical(basic_neq(a, b));
}




// The Util Function To Wrap One-Argument Functions //==========================

static inline
SEXP call_basic_func_onearg(SEXP exta,
                            CWRAPPER_OUTPUT_TYPE (* func)(basic, const basic)) {
    sexp_check_basic(exta);
    basic_struct* a   = (basic_struct*) R_ExternalPtrAddr(exta);
    SEXP          out = PROTECT(sexp_basic());
    basic_struct* s   = (basic_struct*) R_ExternalPtrAddr(out);

    hold_exception(func(s, a));

    UNPROTECT(1);
    return out;
}


/*******************************************************************************
 * //! Expands the expr a and assigns to s.
 * CWRAPPER_OUTPUT_TYPE basic_expand(basic s, const basic a);
 * //! Assigns s = -a.
 * CWRAPPER_OUTPUT_TYPE basic_neg(basic s, const basic a);
 * //! Assigns s = abs(a).
 * CWRAPPER_OUTPUT_TYPE basic_abs(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_expand(SEXP exta) {
    return call_basic_func_onearg(exta, basic_expand);
}

SEXP c_basic_neg(SEXP exta) {
    return call_basic_func_onearg(exta, basic_neg);
}

SEXP c_basic_abs(SEXP exta) {
    return call_basic_func_onearg(exta, basic_abs);
}

/*******************************************************************************
 * //! Assigns s = erf(a).
 * CWRAPPER_OUTPUT_TYPE basic_erf(basic s, const basic a);
 * //! Assigns s = erfc(a).
 * CWRAPPER_OUTPUT_TYPE basic_erfc(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_erf(SEXP exta) {
    return call_basic_func_onearg(exta, basic_erf);
}

SEXP c_basic_erfc(SEXP exta) {
    return call_basic_func_onearg(exta, basic_erfc);
}


/*******************************************************************************
 * //! Assigns s = sin(a).
 * CWRAPPER_OUTPUT_TYPE basic_sin(basic s, const basic a);
 * //! Assigns s = cos(a).
 * CWRAPPER_OUTPUT_TYPE basic_cos(basic s, const basic a);
 * //! Assigns s = tan(a).
 * CWRAPPER_OUTPUT_TYPE basic_tan(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_sin(SEXP exta) {
    return call_basic_func_onearg(exta, basic_sin);
}

SEXP c_basic_cos(SEXP exta) {
    return call_basic_func_onearg(exta, basic_cos);
}

SEXP c_basic_tan(SEXP exta) {
    return call_basic_func_onearg(exta, basic_tan);
}

/*******************************************************************************
 * //! Assigns s = asin(a).
 * CWRAPPER_OUTPUT_TYPE basic_asin(basic s, const basic a);
 * //! Assigns s = acos(a).
 * CWRAPPER_OUTPUT_TYPE basic_acos(basic s, const basic a);
 * //! Assigns s = atan(a).
 * CWRAPPER_OUTPUT_TYPE basic_atan(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_asin(SEXP exta) {
    return call_basic_func_onearg(exta, basic_asin);
}

SEXP c_basic_acos(SEXP exta) {
    return call_basic_func_onearg(exta, basic_acos);
}

SEXP c_basic_atan(SEXP exta) {
    return call_basic_func_onearg(exta, basic_atan);
}


/*******************************************************************************
 * //! Assigns s = csc(a).
 * CWRAPPER_OUTPUT_TYPE basic_csc(basic s, const basic a);
 * //! Assigns s = sec(a).
 * CWRAPPER_OUTPUT_TYPE basic_sec(basic s, const basic a);
 * //! Assigns s = cot(a).
 * CWRAPPER_OUTPUT_TYPE basic_cot(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_csc(SEXP exta) {
    return call_basic_func_onearg(exta, basic_csc);
}

SEXP c_basic_sec(SEXP exta) {
    return call_basic_func_onearg(exta, basic_sec);
}

SEXP c_basic_cot(SEXP exta) {
    return call_basic_func_onearg(exta, basic_cot);
}


/*******************************************************************************
 * //! Assigns s = acsc(a).
 * CWRAPPER_OUTPUT_TYPE basic_acsc(basic s, const basic a);
 * //! Assigns s = asec(a).
 * CWRAPPER_OUTPUT_TYPE basic_asec(basic s, const basic a);
 * //! Assigns s = acot(a).
 * CWRAPPER_OUTPUT_TYPE basic_acot(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_acsc(SEXP exta) {
    return call_basic_func_onearg(exta, basic_acsc);
}

SEXP c_basic_asec(SEXP exta) {
    return call_basic_func_onearg(exta, basic_asec);
}

SEXP c_basic_acot(SEXP exta) {
    return call_basic_func_onearg(exta, basic_acot);
}


/*******************************************************************************
 * //! Assigns s = sinh(a).
 * CWRAPPER_OUTPUT_TYPE basic_sinh(basic s, const basic a);
 * //! Assigns s = cosh(a).
 * CWRAPPER_OUTPUT_TYPE basic_cosh(basic s, const basic a);
 * //! Assigns s = tanh(a).
 * CWRAPPER_OUTPUT_TYPE basic_tanh(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_sinh(SEXP exta) {
    return call_basic_func_onearg(exta, basic_sinh);
}

SEXP c_basic_cosh(SEXP exta) {
    return call_basic_func_onearg(exta, basic_cosh);
}

SEXP c_basic_tanh(SEXP exta) {
    return call_basic_func_onearg(exta, basic_tanh);
}

/*******************************************************************************
 * //! Assigns s = asinh(a).
 * CWRAPPER_OUTPUT_TYPE basic_asinh(basic s, const basic a);
 * //! Assigns s = acosh(a).
 * CWRAPPER_OUTPUT_TYPE basic_acosh(basic s, const basic a);
 * //! Assigns s = atanh(a).
 * CWRAPPER_OUTPUT_TYPE basic_atanh(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_asinh(SEXP exta) {
    return call_basic_func_onearg(exta, basic_asinh);
}

SEXP c_basic_acosh(SEXP exta) {
    return call_basic_func_onearg(exta, basic_acosh);
}

SEXP c_basic_atanh(SEXP exta) {
    return call_basic_func_onearg(exta, basic_atanh);
}

/*******************************************************************************
 * //! Assigns s = csch(a).
 * CWRAPPER_OUTPUT_TYPE basic_csch(basic s, const basic a);
 * //! Assigns s = sech(a).
 * CWRAPPER_OUTPUT_TYPE basic_sech(basic s, const basic a);
 * //! Assigns s = coth(a).
 * CWRAPPER_OUTPUT_TYPE basic_coth(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_csch(SEXP exta) {
    return call_basic_func_onearg(exta, basic_csch);
}

SEXP c_basic_sech(SEXP exta) {
    return call_basic_func_onearg(exta, basic_sech);
}

SEXP c_basic_coth(SEXP exta) {
    return call_basic_func_onearg(exta, basic_coth);
}

/*******************************************************************************
 * //! Assigns s = acsch(a).
 * CWRAPPER_OUTPUT_TYPE basic_acsch(basic s, const basic a);
 * //! Assigns s = asech(a).
 * CWRAPPER_OUTPUT_TYPE basic_asech(basic s, const basic a);
 * //! Assigns s = acoth(a).
 * CWRAPPER_OUTPUT_TYPE basic_acoth(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_acsch(SEXP exta) {
    return call_basic_func_onearg(exta, basic_acsch);
}

SEXP c_basic_asech(SEXP exta) {
    return call_basic_func_onearg(exta, basic_asech);
}

SEXP c_basic_acoth(SEXP exta) {
    return call_basic_func_onearg(exta, basic_acoth);
}


/*******************************************************************************
 * //! Assigns s = lambertw(a).
 * CWRAPPER_OUTPUT_TYPE basic_lambertw(basic s, const basic a);
 * //! Assigns s = zeta(a).
 * CWRAPPER_OUTPUT_TYPE basic_zeta(basic s, const basic a);
 * //! Assigns s = dirichlet_eta(a).
 * CWRAPPER_OUTPUT_TYPE basic_dirichlet_eta(basic s, const basic a);
 * //! Assigns s = gamma(a).
 * CWRAPPER_OUTPUT_TYPE basic_gamma(basic s, const basic a);
 * //! Assigns s = sqrt(a).
 * CWRAPPER_OUTPUT_TYPE basic_sqrt(basic s, const basic a);
 * //! Assigns s = exp(a).
 * CWRAPPER_OUTPUT_TYPE basic_exp(basic s, const basic a);
 * //! Assigns s = log(a).
 * CWRAPPER_OUTPUT_TYPE basic_log(basic s, const basic a);
 *******************************************************************************/

SEXP c_basic_lambertw(SEXP exta) {
    return call_basic_func_onearg(exta, basic_lambertw);
}

SEXP c_basic_zeta(SEXP exta) {
    return call_basic_func_onearg(exta, basic_zeta);
}

SEXP c_basic_dirichlet_eta(SEXP exta) {
    return call_basic_func_onearg(exta, basic_dirichlet_eta);
}

SEXP c_basic_gamma(SEXP exta) {
    return call_basic_func_onearg(exta, basic_gamma);
}

SEXP c_basic_sqrt(SEXP exta) {
    return call_basic_func_onearg(exta, basic_sqrt);
}

SEXP c_basic_exp(SEXP exta) {
    return call_basic_func_onearg(exta, basic_exp);
}

SEXP c_basic_log(SEXP exta) {
    return call_basic_func_onearg(exta, basic_log);
}


/*******************************************************************************
 * //! substitutes a basic 'a' with another basic 'b',
 * //! in the given basic 'e' and returns it through basic 's'
 * CWRAPPER_OUTPUT_TYPE basic_subs2(basic s, const basic e, const basic a, const basic b);
 *******************************************************************************/

SEXP c_basic_subs2(SEXP exte, SEXP exta, SEXP extb) {
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

/*******************************************************************************
 * //! Evaluate b and assign the value to s
 * CWRAPPER_OUTPUT_TYPE basic_evalf(basic s, const basic b, unsigned long bits, int real);
 *******************************************************************************/

SEXP c_basic_evalf(SEXP extb, SEXP bits, SEXP real) {
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
