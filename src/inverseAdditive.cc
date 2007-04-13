#define R_NO_REMAP

#include <R.h>
#include <Rdefines.h>
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include "../include/ainverse.h"
#include <vector>

extern "C" {

void inverseAdditive( int *n , char **ind , char **father , char **mother , double *ainv , int *idx , char **na_value , int *n_na )
{
  TPedVec pedtmp;
  Pedigree Ped;
  vector< string > missing_values;
  for ( int i = 0; i < *n_na; i++ )
  {
    ostringstream ms;
    ms << na_value[ i ];
    missing_values.insert( missing_values.end() , ms.str()  );
  }
  for ( unsigned int i=0 ; i < static_cast< unsigned int >( *n ) ; i++ )
  {
    ostringstream kss, pss, mss, tmss, tpss;
    kss << ind[i];
    vector< string >::const_iterator miss = missing_values.begin();
    tpss << father[i];
    if ( ( miss = find( missing_values.begin() , missing_values.end() , tpss.str() ) ) != missing_values.end() )
    {
      pss << "";
    }
    else
    {
      pss << father[i];
    }
    tmss << mother[i];
    if ( ( miss = find( missing_values.begin() , missing_values.end() , tmss.str() ) ) != missing_values.end() )
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
  for ( unsigned int i = 0 ; i < static_cast< unsigned int >( *n ) ; i++ )
  {
    idx[i] = Ped.GetSortIndex( i );
  }
  Ped.SetAncestor();
  EIBDMat EIBD;
  MakeEIBD( Ped , EIBD );
  EIBD.FillAInvVector( ainv );
  missing_values.erase( missing_values.begin() , missing_values.end() );
}

}
