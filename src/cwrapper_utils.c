
#include <R.h>
#include <symengine/cwrapper.h>

#include "cwrapper_utils.h"

// Exception Handling
const char* cwrapper_exception_message(CWRAPPER_OUTPUT_TYPE id) {
    // Refer:
    // https://github.com/symengine/symengine/blob/master/symengine/symengine_exception.h
    switch(id) {
    case SYMENGINE_NO_EXCEPTION:
        return "SymEngine exception: No exception, it should not go here";
    case SYMENGINE_RUNTIME_ERROR:
        return "SymEngine exception: Runtime error";
    case SYMENGINE_DIV_BY_ZERO:
        return "SymEngine exception: Div by zero";
    case SYMENGINE_NOT_IMPLEMENTED:
        return "SymEngine exception: Not implemented SymEngine feature";
    case SYMENGINE_DOMAIN_ERROR:
        return "SymEngine exception: Domain error";
    case SYMENGINE_PARSE_ERROR:
        return "SymEngine exception: Parse error";
    default:
        return "SymEngine exception: Unexpected SymEngine error code";
    }
}

CWRAPPER_OUTPUT_TYPE cwrapper_set2vec(CSetBasic* set, CVecBasic* vec) {
    basic val;
    basic_new_stack(val);
    size_t len = setbasic_size(set);
    
    void setbasic_get(CSetBasic *self, int n, basic result);
    
    for (size_t i = 0; i < len; i++) {
        setbasic_get(set, i, val); // Return void
        CWRAPPER_OUTPUT_TYPE status2 = vecbasic_push_back(vec, val);
        if (status2) {
            REprintf("Error at index %zu\n", i);
            basic_free_stack(val);
            return status2;
        }
    }
    basic_free_stack(val);
    return SYMENGINE_NO_EXCEPTION;
}

CWRAPPER_OUTPUT_TYPE cwrapper_vec_append_vec(CVecBasic* self, const CVecBasic* el, int idx) {
    basic val;
    basic_new_stack(val);
    if (idx >= 0) {
        CWRAPPER_OUTPUT_TYPE status1 = vecbasic_get(el, idx, val);
        CWRAPPER_OUTPUT_TYPE status2 = vecbasic_push_back(self, val);
        basic_free_stack(val);
        if (status1)
            return status1;
        if (status2)
            return status2;
        return SYMENGINE_NO_EXCEPTION;
    }
    
    // idx < 0, append all elements
    size_t len = vecbasic_size(el);
    for (size_t i = 0; i < len; i++) {
        CWRAPPER_OUTPUT_TYPE status1 = vecbasic_get(el, i, val);
        CWRAPPER_OUTPUT_TYPE status2 = vecbasic_push_back(self, val);
        if (status1) {
            REprintf("Error at index %zu\n", i);
            basic_free_stack(val);
            return status1;
        }
        if (status2) {
            REprintf("Error at index %zu\n", i);
            basic_free_stack(val);
            return status2;
        }
    }
    basic_free_stack(val);
    return SYMENGINE_NO_EXCEPTION;
}

