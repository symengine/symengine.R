
// Some inline functions and functions in "utils.c"

#ifndef _Rsymengine_utils_
#define _Rsymengine_utils_



#define R_NO_REMAP
#include <Rinternals.h>
#include <symengine/cwrapper.h>

// Handle symengine exception ===================================================

const char* _exception_message_from_cwrapper(CWRAPPER_OUTPUT_TYPE id);

static inline void hold_exception(CWRAPPER_OUTPUT_TYPE output) {
    if (output)
        Rf_error(_exception_message_from_cwrapper(output));
    else
        return;
}

// Helper function to initialize an EXTPTR SEXP with empty Basic ================

SEXP sexp_basic();

// Helper function to check EXTPTR SEXP of Basic //==============================

static inline void sexp_check_basic(SEXP ext) {
    if (NULL == R_ExternalPtrAddr(ext))
        Rf_error("Invalid pointer\n");
    if (!R_compute_identical(R_ExternalPtrTag(ext), Rf_mkString("basic_struct*"), 15))
        Rf_error("Tag of the pointer does not match to 'basic_struct*'\n");
    return;
}


#endif // _Rsymengine_utils_

