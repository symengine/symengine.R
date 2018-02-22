
#define R_NO_REMAP
#include <Rinternals.h>
#include <symengine/cwrapper.h>

#include "utils.h"













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
