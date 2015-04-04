#define R_NO_REMAP

#include <R.h>
#include <Rdefines.h>
#include "../include/inbreed.h"
#include "../include/sortped.h"
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include <vector>

using namespace std;

extern "C" {

void meuwissen( int *n , char **ind , char **father , char **mother , double *f , int *idx , char **na_value , int * n_na )
{
  TPedVec pedtmp;
  Pedigree ped;
  VecMap fval;
  int ii = 0;
  vector< string > missing_values;
  for ( int i = 0; i < *n_na; i++ )
  {
    ostringstream ms;
    ms << na_value[ i ];
    missing_values.insert( missing_values.end() , ms.str()  );
  }
  for ( unsigned int i = 0 ; i < static_cast< unsigned int >( *n ) ; i++ )
  {
    ostringstream kss, pss, mss, tmss, tpss;
    TPed tped;
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
    pedtmp.insert( pedtmp.end() , TPed( kss.str() , pss.str() , mss.str() , static_cast< int >( i ) + 1 ) );
  }
  SortPed( ped , pedtmp );
  for ( unsigned int i = 0 ; i < static_cast< unsigned int >( *n ) ; i++ )
  {
    idx[i] = ped.GetSortIndex( i );
  }
  InbreedIt( ped , fval );
  VecMap::iterator ff = fval.begin();
  while ( ff != fval.end() )
  {
    f[ii] = ff->second;
    ii++;
    ff++;
  }
  missing_values.erase( missing_values.begin() , missing_values.end() );
}

}
