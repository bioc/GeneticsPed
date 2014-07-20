/* Program to calculate directly the inverse of A                  */
/* Written by DAH 06/28/2000                                       */
/*                                                                 */
/* The program works by reading a pedigree and then                */
/* forming A^(-1) directly using established rules.                */
/*                                                                 */
/* Utility functions for calculating inbreeding coefficients       */
/*                                                                 */

#include "../include/inbreed.h"

AmatRow::AmatRow()
{
}

AmatRow::AmatRow( int col )
{
  rowmember.insert( VecValue( col , 0.0 ) );
}

AmatRow::AmatRow( int col , double val )
{
  rowmember.insert( VecValue( col , val ) );
}

AmatRow::~AmatRow()
{
  rowmember.erase( rowmember.begin() , rowmember.end() );
}

void AmatRow::SetRowMember( int col )
{
  VecMap::iterator r = rowmember.find( col );
  if ( r == rowmember.end() )
    rowmember.insert( VecValue( col , 0.0 ) );
}

void AmatRow::SetRowMember( int col , double val )
{
  VecMap::iterator r = rowmember.find( col );
  if ( r != rowmember.end() )
    rowmember[ col ] = val;
  else
    rowmember.insert( VecValue( col , val ) );
}

double AmatRow::GetRowMember( int col )
{
  VecMap::iterator r = rowmember.find( col );
  if ( r != rowmember.end() )
    return r->second;
  else
    return 0.0;
}

int AmatRow::GetNextRowMember( int col )
{
  if ( Ends( col , RIGHT ) )
  {
    return -1;
  }
  else
  {
    VecMap::iterator r = rowmember.find( col );
    if ( r != rowmember.end() )
    {
      r++;
      return r->first;
    }
    else
      return -1;
  }
}

int AmatRow::GetPreviousRowMember( int col )
{
  if ( Ends( col , LEFT ) )
  {
    return -1;
  }
  else
  {
    VecMap::iterator r = rowmember.find( col );
    if ( r != rowmember.end() )
    {
      r--;
      return r->first;
    }
    else
      return -1;
  }
}

bool AmatRow::Ends( int col , DirectionType direction )
{
  VecMap::iterator r = rowmember.find( col );
  if ( r != rowmember.end() )
  {
    if ( direction == RIGHT )
    {
      r++;
      if ( r != rowmember.end() )
        return false;
      else
        return true;
    }
    else if ( direction == LEFT )
    {
      r--;
      if ( r != rowmember.begin() )
        return false;
      else
        return true;
    }
    else
      return true;
  }
  else
    return true;
}

void AmatRow::Show()
{
  VecMap::iterator r = rowmember.begin();
  Rprintf("With columns ");
  while ( r != rowmember.end() )
  {
    Rprintf("%d: %f ", r->first, r->second);
    r++;
  }
  Rprintf("\n");
}

Amat::Amat()
{
}

Amat::~Amat()
{
  amat.erase( amat.begin() , amat.end() );
}

void Amat::SetRowMember( int row , int col )
{
  AmatMap::iterator a = amat.find( row );
  if ( a != amat.end() )
    a->second.SetRowMember( col );
  else
    amat.insert( AmatValue( row , AmatRow( col ) ) );
}

void Amat::SetRowMember( int row , int col , double val )
{
  AmatMap::iterator a = amat.find( row );
  if ( a != amat.end() )
    a->second.SetRowMember( col , val );
  else
    amat.insert( AmatValue( row , AmatRow( col , val ) ) );
}

double Amat::GetRowMember( int row , int col )
{
  AmatMap::iterator a = amat.find( row );
  if ( a != amat.end() )
    return a->second.GetRowMember( col );
  else
    return 0.0;
}

int Amat::GetNextRowMember( int row , int col )
{
  AmatMap::iterator a = amat.find( row );
  if ( a != amat.end() )
    return a->second.GetNextRowMember( col );
  else
    return -1;
}

int Amat::GetPreviousRowMember( int row , int col )
{
  AmatMap::iterator a = amat.find( row );
  if ( a != amat.end() )
    return a->second.GetPreviousRowMember( col );
  else
    return -1;
}

void Amat::Show()
{
  AmatMap::iterator a = amat.begin();
  while ( a != amat.end() )
  {
    Rprintf("Row %d: ", a->first);
    a->second.Show();
    a++;
  }
}

void InbreedIt( Pedigree & Ped , VecMap & f )
{
  int TNumAn = Ped.GetPedNumber();
  VecMap d, l;
  IntMap ancestors;
  for ( int row = 0; row < TNumAn; row++ )
  {
    double fs = 0.0, fd = 0.0;
    VecMap::iterator ff = f.find( Ped.GetParentIndex( 0 , row ) );
    if ( ff != f.end() )
      fs = ff->second;
    else
      fs = -1.0;
    ff = f.find( Ped.GetParentIndex( 1 , row ) );
    if ( ff != f.end() )
      fd = ff->second;
    else
      fd = -1.0;
    double dii = .5 - .25 * ( fs + fd );
    d.insert( VecValue( row , dii ) );
    if ( Ped.IsBase( row ) != 0 )
    {
      f.insert( VecValue( row , 0.0 ) );
    }
    else if ( ( Ped.GetParentIndex( 0 , row ) == Ped.GetParentIndex( 0 , row - 1 ) )
        && ( Ped.GetParentIndex( 1 , row ) == Ped.GetParentIndex( 1 , row - 1 ) ) )
    {
      ff = f.find( row - 1 );
      f.insert( VecValue( row , ff->second ) );
    }
    else
    {
      ancestors[ Ped.GetParentIndex( 0 , row ) ] = Ped.GetParentIndex( 0 , row );
      ancestors[ Ped.GetParentIndex( 1 , row ) ] = Ped.GetParentIndex( 1 , row );
      double fi = -1.0;
      l[ row ] = 1.0;
      int ancestor = row;
      while ( !( ancestors.empty() ) )
      {
        int tancestor = ancestor;
        if ( Ped.GetParentIndex( 0 , tancestor ) > -1 )
          ancestors[ Ped.GetParentIndex( 0 , tancestor ) ] = Ped.GetParentIndex( 0 , tancestor );
        if ( Ped.GetParentIndex( 1 , tancestor ) > -1 )
          ancestors[ Ped.GetParentIndex( 1 , tancestor ) ] = Ped.GetParentIndex( 1 , tancestor );
        double r = 0.5 * l[ tancestor ];
        int oancestor = Ped.GetParentIndex( 0 , tancestor );
        int yancestor = Ped.GetParentIndex( 1 , tancestor );
        if ( oancestor > yancestor )
        {
          int tmpancestor = oancestor;
          oancestor = yancestor;
          yancestor = tmpancestor;
        }
        if ( yancestor > -1 )
        {
          l[ yancestor ] += r;
          if ( oancestor > -1 )
          {
            l[ oancestor ] += r;
          }
        }
        fi += l[ ancestor ] * l[ ancestor ] * d[ ancestor ];
        l[ ancestor ] = 0.0;
        tancestor = ancestor;
        ancestors.erase( ancestor );
        IntMap::iterator newancestor = ancestors.begin();
        ancestor = newancestor->first;
      }
      f[ row ] = fi;
    }
  }
  if ( !( l.empty() ) )
  {
    l.erase( l.begin() , l.end() );
  }
  if ( !( d.empty() ) )
  {
    d.erase( d.begin() , d.end() );
  }
}
