
#include <symengine/cwrapper.h>


// START extern "C"
#ifdef __cplusplus
extern "C" {
#endif


// Exception hanlding
const char* cwrapper_exception_message(CWRAPPER_OUTPUT_TYPE id);

static inline
void cwrapper_hold(CWRAPPER_OUTPUT_TYPE output) {
    if (output)
        Rf_error(cwrapper_exception_message(output));
    else
        return;
}

CWRAPPER_OUTPUT_TYPE cwrapper_set2vec(CSetBasic* set, CVecBasic* vec);
CWRAPPER_OUTPUT_TYPE cwrapper_vec_append_vec(CVecBasic* self, const CVecBasic* el, int idx);

#ifdef __cplusplus
}
#endif
// END extern "C"

