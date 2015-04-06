#define R_NO_REMAP

#include <R.h>
#include <Rdefines.h>
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include "../include/pedSort.h"
#include <vector>

extern "C" {

void pedSort( int *n , char **ind , char **father , char **mother , char **na_value , int *n_na )
{
  TPedVec pedtmp;
  Pedigree Ped;
  vector< string > missing_values;
  string miss = "";
  for ( int i = 0; i < *n_na; i++ )
  {
    ostringstream ms;
    ms << na_value[ i ];
    missing_values.insert( missing_values.end() , ms.str()  );
    if ( i == 0 )
    {
      miss = na_value[ i ];
    } 
  }
  for ( unsigned int i=0 ; i < static_cast< unsigned int >( *n ) ; i++ )
  {
    ostringstream kss, pss, mss, tmss, tpss;
    kss << ind[i];
    vector< string >::const_iterator miss_iter = missing_values.begin();
    tpss << father[i];
    if ( ( miss_iter = find( missing_values.begin() , missing_values.end() , tpss.str() ) ) != missing_values.end() )
    {
      pss << "";
    }
    else
    {
      pss << father[i];
    }
    tmss << mother[i];
    if ( ( miss_iter = find( missing_values.begin() , missing_values.end() , tmss.str() ) ) != missing_values.end() )
    {
      mss << "";
    }
    else
    {
      mss << mother[i];
    }
    string kid = kss.str();
    string pop = pss.str();
    string mom = mss.str();
    pedtmp.insert( pedtmp.end() , TPed( kid , pop , mom , static_cast< int >( i ) + 1 ) );
  }
  SortPed( Ped , pedtmp );
  missing_values.erase( missing_values.begin() , missing_values.end() );
  for ( unsigned int i=0 ; i < static_cast< unsigned int >( *n ) ; i++ )
  {
    ind[i] = const_cast< char* >( Ped.GetMember( i ).c_str() );
    if ( Ped.GetParent( 0 , i ) )
    {
      father[i] = const_cast< char* >( Ped.GetMember( Ped.GetParentIndex( i , SIRE ) ).c_str() );
    }
    else
    {
      father[i] = const_cast< char* >( miss.c_str() );
    }
    if ( Ped.GetParent( 0 , i ) )
    {
      mother[i] = const_cast< char* >( Ped.GetMember( Ped.GetParentIndex( i , DAM ) ).c_str() );
    }
    else
    {
      mother[i] = const_cast< char* >( miss.c_str() );
    }
  }
}

}
