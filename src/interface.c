
#define R_NO_REMAP
#include <Rinternals.h>
#include <symengine/cwrapper.h>

#include "utils.h"




















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








