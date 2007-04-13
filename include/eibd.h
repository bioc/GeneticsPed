/* Program to calculate directly the inverse of A                  */
/* Written by DAH 06/28/2000                                       */
/*                                                                 */
/* The program works by reading a pedigree and then                */
/* forming A^(-1) directly using established rules.                */
/*                                                                 */
/* Utility functions for declaration of A^(-1) classes             */
/*                                                                 */
/* File includes the following classes:                            */
/*                                                                 */
/*  EIBDMember  06/28/2000                                         */
/*  EIBDMat     06/28/2000                                         */
/*                                                                 */
/*                                                                 */

#ifndef EIBD_H
#define EIBD_H

using namespace std;

#include <R.h>
#include <Rdefines.h>
#include <map>
#include <algorithm>
#include <string>

class EIBDMember;

typedef map< unsigned int , EIBDMember , less< unsigned int > > EIBDmap;
typedef EIBDmap::value_type EIBDTValue;

class EIBDMember
{
  public:
    EIBDMember();
    EIBDMember( unsigned int , unsigned int , double );
    EIBDMember( const EIBDMember &copy );
    ~EIBDMember();
    unsigned int GetRow() { return row; }
    unsigned int GetCol() { return col; }
    void IncrValue( double v ) { value += v; }
    double GetValue() { return value; }
    void SetNextInColumn( unsigned int );
    unsigned int GetNextInColumn();
    void NextSet() { next_in_column_set = true; }
    bool IsNextSet() { return next_in_column_set; }
    void operator = ( const EIBDMember &copy );
    void Show();
  private:
    unsigned int row, col, *next_in_column;
    double value;
    bool next_in_column_set;
};

class EIBDMat
{
  public:
    EIBDMat();
    EIBDMat( string eff ) { effect = eff; }
    EIBDMat( const EIBDMat &copy );
    ~EIBDMat();
    void SetEffect( string eff ) { effect = eff; }
    void SetPedNumber( unsigned int num ) { pednumber = num; }
    unsigned int GetPedNumber() { return pednumber; }
    void SetDiag( unsigned int , unsigned int , unsigned int , double );
    unsigned int GetRow( unsigned int i ) { return eibd[i].GetRow(); }
    unsigned int GetCol( unsigned int i ) { return eibd[i].GetCol(); }
    void IncrValue( double , unsigned int );
    void IncrValue( double , unsigned int , unsigned int , unsigned int );
    string GetEffect() { return effect; }
    double GetValue( unsigned int );
    unsigned int GetLength() { return eibd.size(); }
    unsigned int GetNext( unsigned int );
    bool Exists( unsigned int );
    unsigned int GetNextCol( unsigned int );
    void operator = ( const EIBDMat &copy );
    void Show();
    void FillAInvVector( double * );
  private:
    string effect;
    unsigned int pednumber;
    EIBDmap eibd;
};

#endif
