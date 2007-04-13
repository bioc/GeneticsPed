/* register.cc
 *-------------------------------------------------------------------------
 * What: Registering native routines to R
 * $Id: register.cc 1153 2007-03-02 17:12:03Z ggorjan $
 * Time-stamp: <2007-03-02 17:53:22 ggorjan>
 *-----------------------------------------------------------------------*/

#include "../include/meuwissen.h"
#include "../include/ainverse.h"
#include "../include/pedSort.h"

extern "C" {

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Argument types for each method

static R_NativePrimitiveArgType gpiType[] = {
  INTSXP,  // 1 integer        nobs
  INTSXP,  // 2 integer        n
  REALSXP, // 3 doble precison gp
  REALSXP, // 4 doble precison hwp
  REALSXP  // 5 doble precison ret
};

static R_NativePrimitiveArgType meuwissenType[] = {
  INTSXP,  // 1 int    *n
  STRSXP,  // 2 char   **ind
  STRSXP,  // 3 char   **father
  STRSXP,  // 4 char   **mother
  REALSXP, // 5 double *f
  INTSXP,  // 6 int    *idx
  STRSXP,  // 7 char   **na_value
  INTSXP   // 8 int    *n_na
};

static R_NativePrimitiveArgType inverseAdditiveType[] = {
  INTSXP,  // 1 int    *n
  STRSXP,  // 2 char   **ind
  STRSXP,  // 3 char   **father
  STRSXP,  // 4 char   **mother
  REALSXP, // 5 double **ainv
  INTSXP,  // 6 int    *idx
  STRSXP,  // 7 char   **na_value
  INTSXP   // 8 int    **n_na
};

static R_NativePrimitiveArgType pedSortType[] = {
  INTSXP,  // 1 int    *n
  STRSXP,  // 2 char   **ind
  STRSXP,  // 3 char   **father
  STRSXP,  // 4 char   **mother
  STRSXP,  // 5 char   **na_value
  INTSXP   // 6 int    **n_na
};

// Functions accessed via .C()

static const R_CMethodDef cMethods[] = {
  {"meuwissen",       (DL_FUNC) &meuwissen,       8, meuwissenType},
  {"inverseAdditive", (DL_FUNC) &inverseAdditive, 8, inverseAdditiveType},
  {"pedSort",         (DL_FUNC) &pedSort,         6, pedSortType},
  {NULL, NULL, 0}
};

// Functions accessed via .Fortran()

void F77_SUB(gpi)(int *nobs, int *n, float *gp, float *hwp, float *ret);

static const R_FortranMethodDef fortranMethod[] = {
  {"gpi", (DL_FUNC) &F77_SUB(gpi), 5, gpiType},
  {NULL, NULL, 0}
};

// Registration function

void R_init_GeneticsPed(DllInfo *info)
{

  R_registerRoutines(info,
                     cMethods,      // .C
                     NULL,          // .Call
                     fortranMethod, // .Fortran
                     NULL);         // .External
// FIXME: this is causing an error in compilation
//  R_useDynamicSymbols(info, FALSE);
}

}

/*-------------------------------------------------------------------------
  register.cc ends here */
