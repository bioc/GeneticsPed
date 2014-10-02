/*                                                       */
/* Program to calculate directly the inverse of A        */
/* Written by DAH 06/28/2000                             */
/*                                                       */
/* The program works by reading a pedigree and then      */
/* forming A^(-1) directly using established rules.      */
/*                                                       */

#include "../include/ainverse.h"

using namespace std;

void MakeEIBD( Pedigree & Ped , EIBDMat & EIBD )
{
  VecMap diag;
  InbreedIt( Ped , diag );
  AddCoeff( EIBD , Ped , diag );
  SetColumns( EIBD );
}

void AddCoeff( EIBDMat & EIBD , Pedigree & Ped , VecMap & diag )
{
  unsigned int Pcol = 0, col = 0;
  int TNumAn = Ped.GetPedNumber(), mate = 0;
  EIBD.SetPedNumber( TNumAn );
  const double cov1 = -0.5, cov2 = 0.25;
  double Dii = 0.0, coeff1 = 0.0, coeff2 = 0.0, ais = 0.0, aid = 0.0;
  for ( int row = 0; row < TNumAn; row++ )
  {
    if ( Ped.GetParent( 0 , row ) )
      ais = 1 + diag[ Ped.GetParentIndex( 0 , row ) ];
    else
      ais = 0.0;
    if ( Ped.GetParent( 1 , row ) )
      aid = 1 + diag[ Ped.GetParentIndex( 1 , row ) ];
    else
      aid = 0.0;
    Dii = 1 / ( 1.0 - 0.25 * ( ais + aid ) );
    EIBD.IncrValue( Dii , GetIndex( 0 , row , row , TNumAn ) , row , row );
    for (int parent = 0; parent < 2; parent++ )
    {
      if ( Ped.GetParent( parent , row ) )
      {
        col = Ped.GetParentIndex( parent , row );
        mate = abs( parent - 1 );
        Pcol = Ped.GetParentIndex( mate , row );
        coeff1 = cov1 * Dii;
        coeff2 = cov2 * Dii;
        EIBD.IncrValue( coeff1 , GetIndex( 0 , col , row , TNumAn ) , col , row );
        EIBD.IncrValue( coeff2 , GetIndex( 0 , col , col , TNumAn ) , col , col );
        if ( col > Pcol )
        {
          EIBD.IncrValue( coeff2 , GetIndex( 0 , Pcol , col , TNumAn ) , Pcol , col );
        }
      }
    }
    if ( ( ( row + 1 ) % 1000 ) == 0)
    {
      Rprintf("At individual %d\n", row + 1);
    }
  }
}

void SetColumns( EIBDMat & EIBD )
{
  int index = 0, max = GetIndex( 0 , EIBD.GetPedNumber() , EIBD.GetPedNumber() , EIBD.GetPedNumber() );//, cindex = 0
  while ( index < max )
  {
//    cindex = EIBD.GetNextCol( index );
    index = EIBD.GetNext( index );
  }
}

unsigned int GetIndex( int type , unsigned int row , unsigned int col )
{
  if ( type == 0 )
    return ( (row + 1) * row ) / 2 + col;
  else if ( type == 1 )
    return ( row * (row - 1) ) / 2 + col - 1;
  else
    return 0;
}

unsigned int GetIndex( int type , unsigned int row , unsigned int col  , unsigned int row_length )
{
  if ( type == 0 )
    return ( row + 1 ) * row_length - ( row_length - ( col + 1 ) ) - row - ( (row - 1) * row ) / 2 - 1;
  else if ( type == 1 )
    return row * row_length - ( row_length - col ) - ( row - 1 ) - ( (row - 2) * ( row - 1 ) ) / 2;
  else
    return 0;
}

double GetEIBD( EIBDMat & EIBD , int i , int j )
{
  int row = 0, col = 0, row1 = 0, col1 = 0;
  unsigned int index = 0;
  if ( i >= j )
  {
    row = i;
    col = j;
  }
  if ( i < j )
  {
    row = j;
    col = i;
  }
  row1 = row + 1;
  col1 = col + 1;

  if ( row == col )
  {
    index = GetIndex( 1 , row1 , row1 );
  }
  else
  {
    index = GetIndex( 1 , row1 , col1 );
  }
  return EIBD.GetValue( index );
}

double GetEIBD( EIBDMat & EIBD , int i , int j , int k )
{
  int row = 0, col = 0, row1 = 0, col1 = 0;
  unsigned int index = 0;
  if ( i >= j )
  {
    row = i;
    col = j;
  }
  if ( i < j )
  {
    row = j;
    col = i;
  }
  row1 = row + 1;
  col1 = col + 1;

  if ( row == col )
  {
    index = GetIndex( 1 , row1 , row1 , k );
  }
  else
  {
    index = GetIndex( 1 , row1 , col1 , k );
  }
  return EIBD.GetValue( index );
}
