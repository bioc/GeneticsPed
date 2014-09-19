/*                                                                 */
/* Program to calculate directly the inverse of A                  */
/* Written by DAH 06/28/2000                                       */
/*                                                                 */
/* The program works by reading a pedigree and then                */
/* forming A^(-1) directly using established rules.                */
/*                                                                 */
/* Utility functions for calculating inbreeding coefficients       */
/*                                                                 */

#ifndef INBREED_H
#define INBREED_H

#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include "pedtemplate.h"
#include "math_utilities.h"

using namespace std;

class AmatRow;

typedef map< int , double , less< int > > VecMap;
typedef VecMap::value_type VecValue;
typedef map< int , int , greater< int > > IntMap;
typedef IntMap::value_type IntValue;
typedef map< int , AmatRow , less< int > > AmatMap;
typedef AmatMap::value_type AmatValue;
typedef enum{ RIGHT , LEFT } DirectionType;

void InbreedIt( Pedigree & , VecMap & );

class AmatRow
{
  public:
    AmatRow();
    AmatRow( int );
    AmatRow( int , double );
    ~AmatRow();
    void SetRowMember( int );
    void SetRowMember( int , double );
    double GetRowMember( int );
    int GetNextRowMember( int );
    int GetPreviousRowMember( int );
    bool Ends( int , DirectionType );
    void Show();
  private:
    VecMap rowmember;
};

class Amat
{
  public:
    Amat();
    ~Amat();
    void SetRowMember( int , int );
    void SetRowMember( int , int , double );
    double GetRowMember( int , int );
    int GetNextRowMember( int , int );
    int GetPreviousRowMember( int , int );
    void Show();
  private:
    AmatMap amat;
};

#endif
