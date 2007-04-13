/*                                                                 */
/* Program to calculate directly the inverse of A                  */
/* Written by DAH 06/28/2000                                       */
/*                                                                 */
/* The program works by reading a pedigree and then                */
/* forming A^(-1) directly using established rules.                */
/*                                                                 */
/* Utility functions for forming A^(-1)                            */
/*                                                                 */

#ifndef AINVERSE_H
#define AINVERSE_H

using namespace std;

#include <R.h>
#include <Rdefines.h>
#include <string>
#include <map>
#include <algorithm>
#include "eibd.h"
#include "pedtemplate.h"
#include "inbreed.h"
#include "sortped.h"

extern "C" {

void inverseAdditive(int *n, char **ind, char **father, char **mother,
                     double *ainv, int *idx, char **na_value, int *n_na);

void MakeEIBD( Pedigree & , EIBDMat & );
void AddCoeff( EIBDMat & , Pedigree & , VecMap & );
void SetColumns( EIBDMat & );
//unsigned int GetIndex( int , unsigned int , unsigned int );
unsigned int GetIndex( int , unsigned int , unsigned int  , unsigned int );
double GetEIBD( EIBDMat & , int , int );
//double GetEIBD( EIBDMat & , int , int , int );

}

#endif
