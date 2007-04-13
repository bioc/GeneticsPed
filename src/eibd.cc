/* Program to calculate directly the inverse of A                  */
/* Written by DAH 04/19/2000                                       */
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

#include "../include/eibd.h"

/* Constructor for the EIBDMember Class */

EIBDMember::EIBDMember()
{
  row = col = 0;
  value = 0.0;
  next_in_column = new unsigned int;
  next_in_column = NULL;
  next_in_column_set = false;
}

/* Constructor for the EIBDMember Class */

EIBDMember::EIBDMember( unsigned int r , unsigned int c , double v )
{
  row = r;
  col = c;
  value = v;
  next_in_column = new unsigned int;
  next_in_column = NULL;
  next_in_column_set = false;
}

/* Copy Constructor for the EIBDMember Class */

EIBDMember::EIBDMember( const EIBDMember &copy )
{
  *this = copy;
}

/* Destructor for the EIBDMember Class */

EIBDMember::~EIBDMember()
{
  delete next_in_column;
}

void EIBDMember::SetNextInColumn( unsigned int next )
{
  delete next_in_column;
  next_in_column = new unsigned int( next );
}

unsigned int EIBDMember::GetNextInColumn()
{
  if ( next_in_column )
    return *next_in_column;
  else
    return 0;
}

/* Overloaded operator = () for the EIBDMember Class */

void EIBDMember::operator = ( const EIBDMember &copy )
{
  if ( this == &copy ) return;
  row = copy.row;
  col = copy.col;
  value = copy.value;
  next_in_column_set = copy.next_in_column_set;
  if ( copy.next_in_column )
  {
    next_in_column = new unsigned int( *copy.next_in_column );
  }
  else
  {
    next_in_column = new unsigned int;
    next_in_column = NULL;
  }
}

void EIBDMember::Show()
{
  Rprintf("%d %d %f", row, col, value);
  if ( next_in_column )
  {
    Rprintf(" %d", *next_in_column);
  }
  else
  {
    Rprintf(" NULL");
  }
  Rprintf("\n");
}

/* Constructor for the EIBDMat Class */

EIBDMat::EIBDMat()
{
  effect = "";
}

/* Copy Constructor for the EIBDMat Class */

EIBDMat::EIBDMat( const EIBDMat &copy )
{
  *this = copy;
}

/* Destructor for the EIBDMat Class */

EIBDMat::~EIBDMat()
{
  eibd.erase( eibd.begin() , eibd.end() );
}

/* Implementation of SetDiag() */

void EIBDMat::SetDiag( unsigned int i , unsigned int r , unsigned int c , double v )
{
  eibd.insert( EIBDTValue( i , EIBDMember( r , c , v ) ) );
}

/* Implementation of IncrValue() */

void EIBDMat::IncrValue( double v , unsigned int i )
{
  EIBDmap::iterator e = eibd.find( i );
  if ( e != eibd.end() )
  {
    e->second.IncrValue( v );
  }
  else
  {
    eibd.insert( EIBDTValue( i , EIBDMember( 0 , 0 , v ) ) );
  }
}

/* Implementation of IncrValue() */

void EIBDMat::IncrValue( double v , unsigned int i , unsigned int r , unsigned int c )
{
  EIBDmap::iterator e = eibd.find( i );
  if ( e != eibd.end() )
  {
    e->second.IncrValue( v );
  }
  else
  {
    eibd.insert( EIBDTValue( i , EIBDMember( r , c , v ) ) );
  }
}

/* Implementation of GetValue() */

double EIBDMat::GetValue( unsigned int i )
{
  EIBDmap::iterator e = eibd.find( i );
  if ( e != eibd.end() )
  {
    return eibd[ i ].GetValue();
  }
  else
  {
    return 0.0;
  }
}

/* Implementation of GetNext() */

unsigned int EIBDMat::GetNext( unsigned int index )
{
  EIBDmap::iterator e = eibd.find( index );
  if ( e != eibd.end() )
  {
    if ( e->first != ( ( ( pednumber - 1 ) * pednumber ) / 2 + pednumber - 1 ) )
    {
      e++;
      return e->first;
    }
    else
    {
      return ( ( pednumber - 1 ) * pednumber ) / 2 + pednumber;
    }
  }
  else
  {
    return ( ( pednumber - 1 ) * pednumber ) / 2 + pednumber;
  }
}

/* Implementation of Exists() */

bool EIBDMat::Exists( unsigned int index )
{
  EIBDmap::iterator e = eibd.find( index );
  if ( e != eibd.end() )
  {
    return true;
  }
  else
  {
    return false;
  }
}

unsigned int EIBDMat::GetNextCol( unsigned int index )
{
  if ( Exists( index ) )
  {
    if ( eibd[ index ].IsNextSet() )
    {
      return eibd[ index ].GetNextInColumn();
    }
    else if ( index == 0 )
    {
      eibd[ index ].NextSet();
      return 0;
    }
    else
    {
      bool set = false;
      unsigned int row = eibd[ index ].GetRow();
      if ( row == 0 )
      {
        eibd[ index ].NextSet();
        return 0;
      }
      else
      {
       row--;
      }
      unsigned int col = eibd[ index ].GetCol();
      unsigned int row_length = pednumber;
      unsigned int new_index = 0;
      while ( !( set ) )
      {
        new_index = ( row + 1 ) * row_length - ( row_length - ( col + 1 ) ) - row - ( (row - 1) * row ) / 2 - 1;
        if ( Exists( new_index ) )
        {
          eibd[ index ].SetNextInColumn( new_index );
          eibd[ index ].NextSet();
          set = true;
        }
        else
        {
          if ( row == 0 )
          {
            eibd[ index ].NextSet();
            new_index = 0;
            set = true;
          }
          else
          {
            row--;
          }
        }
      }
      return new_index;
    }
  }
  else
  {
    return 0;
  }
}

/* Implementation of overloaded operator = () */

void EIBDMat::operator = ( const EIBDMat &copy )
{
  if ( this == &copy ) return;
  eibd = copy.eibd;
  effect = copy.effect;
  pednumber = copy.pednumber;
}

void EIBDMat::Show()
{
  EIBDmap::iterator e = eibd.begin();
  while ( e != eibd.end() )
  {
    Rprintf("%d ", e->first);
    e->second.Show();
    e++;
  }
}

void EIBDMat::FillAInvVector( double *f )
{
  EIBDmap::iterator e = eibd.begin();
  while ( e != eibd.end() )
  {
    int col = e->second.GetCol(), row = e->second.GetRow();
    if ( col == row )
    {
      f[col * pednumber + row] = e->second.GetValue();
    }
    else
    {
      f[col * pednumber + row] = e->second.GetValue();
      f[row * pednumber + col] = e->second.GetValue();
    }
    e++;
  }
}
