
#include <R.h>
#include <Rinternals.h>

SEXP hello() {
    Rprintf("hello\n");
    return ScalarInteger(42);
}
