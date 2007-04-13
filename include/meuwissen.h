#ifndef MEUWISSEN_H
#define MEUWISSEN_H

using namespace std;

#include <sstream>
#include <R.h>
#include <Rdefines.h>
#include "../include/inbreed.h"
#include "../include/sortped.h"
#include <stdio.h>
#include <stdlib.h>
#include <vector>

extern "C" {
  void meuwissen(int *n, char **ind, char **father, char **mother, double *f,
                 int *idx, char **na_value, int *n_na);
}

#endif
