#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP rewens_c(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"rewens_c", (DL_FUNC) &rewens_c, 2},
    {NULL, NULL, 0}
};

void R_init_ewens(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
