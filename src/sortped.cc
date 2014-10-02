#include "../include/sortped.h"

void SortPed( Pedigree &Ped , TPedVec &Pedtmp )
{
  bool ped_problem = false;
  TPedVec::iterator mp = Pedtmp.begin();
  TPedVec all, finalprogeny;
  vector< string > parlist, sirelist, damlist;
  while ( !( Pedtmp.empty() ) )
  {
    if ( mp->IsBase() == 1)
    {
      all.insert( all.end() , *mp );
      Pedtmp.erase( mp );
    }
    else
    {
      finalprogeny.insert( finalprogeny.end() , *mp );
      if ( mp->Exists( SIRE ) )
      {
        vector< string >::const_iterator fs = find( damlist.begin() , damlist.end() , mp->ReturnSire() );
        if ( fs != damlist.end() )
        {
          Rprintf("Father: %s is also in the pedigree as a mother\n", mp->ReturnSire().c_str());
          ped_problem = true;
        }
        if ( mp->ReturnAnimal() == mp->ReturnSire() )
        {
          Rprintf("Individual: %s is also in the pedigree its father: %s\n", mp->ReturnAnimal().c_str(), mp->ReturnSire().c_str());
          ped_problem = true;
        }
        sirelist.insert( sirelist.end() , mp->ReturnSire() );
      }
      if ( mp->Exists( DAM ) )
      {
        vector< string >::const_iterator fd = find( sirelist.begin() , sirelist.end() , mp->ReturnDam() );
        if ( fd != sirelist.end() )
        {
          Rprintf("Mother: %s is also in the pedigree as a father\n", mp->ReturnDam().c_str());
          ped_problem = true;
        }
        if ( mp->ReturnAnimal() == mp->ReturnDam() )
        {
          Rprintf("Individual: %s is also in the pedigree its mother: %s\n", mp->ReturnAnimal().c_str(), mp->ReturnDam().c_str());
          ped_problem = true;
        }
        damlist.insert( damlist.end() , mp->ReturnDam() );
      }
      Pedtmp.erase( mp );
    }
  }
  if( ped_problem )
  {
    error("Problems in pedigree.  Stopping inbreeding calculations\n");
  }
  sort( sirelist.begin() , sirelist.end() );
  vector< string >::iterator s = unique( sirelist.begin() , sirelist.end() );
  sirelist.erase( s , sirelist.end() );
  sort( damlist.begin() , damlist.end() );
  vector< string >::iterator d = unique( damlist.begin() , damlist.end() );
  damlist.erase( d , damlist.end() );
  parlist.resize( sirelist.size() + damlist.size() );
  merge( sirelist.begin() , sirelist.end() , damlist.begin() , damlist.end() , parlist.begin() );
  sirelist.erase( sirelist.begin() , sirelist.end() );
  damlist.erase( damlist.begin() , damlist.end() );
  sort( parlist.begin() , parlist.end() );
  vector< string >::iterator p = unique( parlist.begin() , parlist.end() );
  parlist.erase( p , parlist.end() );
  TPedVec::iterator o = finalprogeny.begin();
  TPedVec::iterator a = all.begin();
  p = parlist.begin();
  while ( p != parlist.end() )
  {
    o = find( finalprogeny.begin() , finalprogeny.end() , *p );
    if ( o != finalprogeny.end() )
    {
      p++;
    }
    else if ( Ped.Ancestor() )
    {
      a = find( all.begin() , all.end() , *p );
      if ( a != all.end() )
      {
        parlist.erase( p );
      }
      else
      {
        all.insert( all.end() , TPed( *p ) );
        parlist.erase( p );
      }
    }
    else
    {
      p++;
    }
  }
  if ( !( parlist.empty() ) )
  {
    a = all.begin();
    while ( a != all.end() )
    {
      if ( parlist.empty() )
      {
        break;
      }
      p = find( parlist.begin() , parlist.end() , a->ReturnAnimal() );
      if ( p != parlist.end() )
      {
        parlist.erase( p );
      }
      a++;
    }
  }
  TPedVec parents;
  if ( !( parlist.empty() ) )
  {
    p = parlist.begin();
    while ( p != parlist.end() )
    {
      o = find( finalprogeny.begin() , finalprogeny.end() , *p );
      if ( o != finalprogeny.end() )
      {
        parents.insert( parents.end() , *o );
        finalprogeny.erase( o );
      }
      p++;
    }
    parlist.erase( parlist.begin() , parlist.end() );
  }
  a = all.begin();
  TPedVec::iterator q = parents.begin();
  while ( !( parents.empty() ) )
  {
    if ( q->IsBase() == 0 )
    {
      a = find( all.begin() , all.end() , q->ReturnSire() );
      if ( a != all.end() )
      {
        q->SetIndex( a - all.begin() , SIRE );
        a = find( all.begin() , all.end() , q->ReturnDam() );
        if ( a != all.end() )
        {
          q->SetIndex( a - all.begin() , DAM );
          all.insert( all.end() , q->ReturnTPed() );
          parents.erase( q );
          if ( q + 1 <= parents.end() )
          {
            q++;
          }
          else
          {
            q = parents.begin();
          }
        }
        else if ( q + 1 <= parents.end() )
        {
          q++;
        }
        else
        {
          q = parents.begin();
        }
      }
      else if ( q + 1 <= parents.end() )
      {
        q++;
      }
      else
      {
        q = parents.begin();
      }
    }
    else
    {
      if ( q->Exists( SIRE ) || q->Exists( DAM ) )
      {
        bool insertQ = false;
        if ( q->Exists( SIRE ) )
        {
          a = find( all.begin() , all.end() , q->ReturnSire() );
          if ( a != all.end() )
          {
            q->SetIndex( a - all.begin() , SIRE );
            insertQ = true;
          }
        }
        if ( q->Exists( DAM ) )
        {
          a = find( all.begin() , all.end() , q->ReturnDam() );
          if ( a != all.end() )
          {
            q->SetIndex( a - all.begin() , DAM );
            insertQ = true;
          }
        }
        if ( insertQ )
        {
          all.insert( all.end() , q->ReturnTPed() );
          q->ShowPed();
          parents.erase( q );
          if ( q + 1 <= parents.end() )
          {
            q++;
          }
          else
          {
            q = parents.begin();
          }
        }
        else if ( q + 1 <= parents.end() )
        {
          q++;
        }
        else
        {
          q = parents.begin();
        }
      }
      else if ( q + 1 <= parents.end() )
      {
        q++;
      }
      else
      {
        q = parents.begin();
      }
    }
  }
  o = finalprogeny.begin();
  while ( ! finalprogeny.empty() )
  {
    if ( o->Exists( SIRE ) )
    {
      a = find( all.begin() , all.end() , o->ReturnSire() );
      o->SetIndex( a - all.begin() , SIRE );
    }
    if ( o->Exists( DAM ) )
    {
      a = find( all.begin() , all.end() , o->ReturnDam() );
      o->SetIndex( a - all.begin() , DAM );
    }
    all.insert( all.end() , *o );
    finalprogeny.erase( o );
  }
  Ped.CreatePedigree( all );
}
