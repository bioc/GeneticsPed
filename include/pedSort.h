/*                                                                 */
/* Program to calculate directly the inverse of A                  */
/* Written by DAH 06/28/2000                                       */
/*                                                                 */
/* The program works by reading a pedigree and then                */
/* forming A^(-1) directly using established rules.                */
/*                                                                 */
/* Utility functions for forming A^(-1)                            */
/*                                                                 */

#ifndef PEDSORT_H
#define PEDSORT_H

using namespace std;

#include <R.h>
#include <Rdefines.h>
#include <string>
#include <map>
#include <algorithm>
#include "pedtemplate.h"
#include "sortped.h"

extern "C" {

void pedSort(int *n, char **ind, char **father, char **mother, char **na_value, int *n_na);

}

#endif
