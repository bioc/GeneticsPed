#include "../include/sortped.h"

void SortPed( Pedigree &Ped , TPedVec &Pedtmp )
{
  bool ped_problem = false;
  TPedVec all, finalprogeny;
  vector< string > parlist, sirelist, damlist;
  for(TPedVec::const_iterator mp = Pedtmp.begin(); mp != Pedtmp.end(); ++mp)
  {
    if ( mp->IsBase() == 1)
    {
      all.push_back( *mp );
    }
    else
    {
      finalprogeny.push_back( *mp );
      // error checking for SIRE
      if ( mp->Exists( SIRE ) )
      {
	if(find(damlist.begin() , damlist.end() ,
		mp->ReturnSire() ) != damlist.end() )
        {
          Rprintf("Father: %s is also in the pedigree as a mother\n",
		  mp->ReturnSire().c_str());
          ped_problem = true;
        }
        if ( mp->ReturnAnimal() == mp->ReturnSire() )
        {
          Rprintf("Individual: %s is also in the pedigree its father: %s\n",
		  mp->ReturnAnimal().c_str(), mp->ReturnSire().c_str());
          ped_problem = true;
        }
        sirelist.push_back( mp->ReturnSire() );
      }
      // error checking for DAM
      if ( mp->Exists( DAM ) )
      {
        if(find( sirelist.begin() , sirelist.end() ,
		 mp->ReturnDam() ) != sirelist.end() )
        {
          Rprintf("Mother: %s is also in the pedigree as a father\n", mp->ReturnDam().c_str());
          ped_problem = true;
        }
        if ( mp->ReturnAnimal() == mp->ReturnDam() )
        {
          Rprintf("Individual: %s is also in the pedigree its mother: %s\n", mp->ReturnAnimal().c_str(), mp->ReturnDam().c_str());
          ped_problem = true;
        }
        damlist.push_back( mp->ReturnDam() );
      }
    }
  }
  if( ped_problem )
  {
    error("Problems in pedigree.  Stopping inbreeding calculations\n");
  }

  // remove duplicates from sire
  sort( sirelist.begin() , sirelist.end() );
  sirelist.erase( unique( sirelist.begin() , sirelist.end() ), sirelist.end() );

  // remove duplicates from dam
  sort( damlist.begin() , damlist.end() );
  damlist.erase( unique( damlist.begin() , damlist.end() ) , damlist.end() );

  // combine sirelist and damlist into parlist
  parlist.resize( sirelist.size() + damlist.size() );
  merge( sirelist.begin() , sirelist.end() , damlist.begin() , damlist.end() ,
	 parlist.begin() );

  // remove dups from parlist
  sort( parlist.begin() , parlist.end() );
  parlist.erase( unique( parlist.begin() , parlist.end() ) , parlist.end() );

  TPedVec::iterator o = finalprogeny.begin();
  TPedVec::iterator a = all.begin();

  vector< string >::iterator p = parlist.begin();
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
        p = parlist.erase( p );
      }
      else
      {
        all.push_back( TPed( *p ) );
        p = parlist.erase( p );
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
        p = parlist.erase( p );
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
        parents.push_back( *o );
        o = finalprogeny.erase( o );
      }
      p++;
    }
  }
  a = all.begin();
  TPedVec::iterator q = parents.begin();
  while ( !( parents.empty() ) )
  {
    // sire or dam member is set to non-empty string
    if ( q->IsBase() == 0 )
    {
      // set sire index if sire in all
      a = find( all.begin() , all.end() , q->ReturnSire() );
      if ( a != all.end() )
      {
        q->SetIndex( a - all.begin() , SIRE );
        // set dam index if dam in all
        a = find( all.begin() , all.end() , q->ReturnDam() );
        if ( a != all.end() )
        {
          q->SetIndex( a - all.begin() , DAM );
          all.push_back( q->ReturnTPed() );
          q = parents.erase( q );
          if ( q < parents.end() )
          {
              ++q;
          }
        }
        else if ( q < parents.end() )
        {
            ++q;
        }
      }
      else if ( q < parents.end() )
      {
          ++q;
      }
      if( q == parents.end() )
          q = parents.begin();
    }
    // no parents
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
          all.push_back( q->ReturnTPed() );
          q = parents.erase( q );
          if ( q == parents.end() )
          {
            q = parents.begin();
          }
        }
        else if ( q == parents.end() )
        {
          q = parents.begin();
        }
      }
      else if ( q == parents.end() )
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
    all.push_back( *o );
    o = finalprogeny.erase( o );
  }
  Ped.CreatePedigree( all );
}
