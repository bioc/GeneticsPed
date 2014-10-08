/* Program to calculate directly the inverse of A                  */
/* Written by DAH 06/23/2000                                       */
/*                                                                 */
/* The program works by reading a pedigree and then                */
/* forming A^(-1) directly using established rules.                */
/*                                                                 */
/* Utility functions for declaration of animal type classes        */
/*                                                                 */
/* File includes the following classes:                            */
/*                                                                 */
/*  TPed     06/23/2000                                            */
/*  Pedigree 06/23/2000                                            */
/*                                                                 */

#include "../include/pedtemplate.h"

TPed::TPed()
{
  animal = "";
  sire = "";
  dam = "";
  s_index = -1;
  d_index = -1;
  sort_index = 0;
  hasparents = false;
}

TPed::TPed( string a , string s , string d )
{
  bool parent = false;
  animal = a;
  s_index = -1;
  d_index = -1;
  sort_index = 0;
  if ( ( s != "0" ) && ( s != "" ) )
  {
    sire = s;
    parent = true;
  }
  else
  {
    sire = "";
  }
  if ( ( d != "0" ) && ( d != "" ) )
  {
    dam = d;
    parent = true;
  }
  else
  {
    dam = "";
  }
  if ( parent )
  {
    hasparents = true;
  }
  else
  {
    hasparents = false;
  }
}

TPed::TPed( string a , string s , string d , int i )
{
  bool parent = false;
  animal = a;
  s_index = -1;
  d_index = -1;
  sort_index = i;
  if ( ( s != "0" ) && ( s != "" ) )
  {
    sire = s;
    parent = true;
  }
  else
  {
    sire = "";
  }
  if ( ( d != "0" ) && ( d != "" ) )
  {
    dam = d;
    parent = true;
  }
  else
  {
    dam = "";
  }
  if ( parent )
  {
    hasparents = true;
  }
  else
  {
    hasparents = false;
  }
}

TPed::TPed( string a )
{
  animal = a;
  sire = "";
  dam = "";
  s_index = -1;
  d_index = -1;
  sort_index = 0;
  hasparents = false;
}

TPed::TPed( const TPed &copy )
{
  if ( this == &copy )
  {
    return;
  }
  copyPed( copy );
}
/*
TPed::TPed( TPed &&copy )
{
  if ( this == &copy )
  {
    copy.TPed();
    return;
  }
  SetPed( copy.animal , copy.sire , copy.dam , copy.sort_index , copy.GetIndex( SIRE ) , copy.GetIndex( DAM ) );
  copy.TPed();
}
*/
TPed::~TPed()
{
}

string TPed::ReturnAnimal() const
{
  return animal;
}

string TPed::ReturnSire() const
{
  return sire;
}

string TPed::ReturnDam() const
{
  return dam;
}

int TPed::ReturnIndex() const
{
  return sort_index;
}

void TPed::SetPed( string a , string s , string d )
{
  bool parent = false;
  animal = a;
  if ( ( s != "." ) && ( s != "" ) )
  {
    sire = s;
    parent = true;
  }
  else
  {
    sire = "";
  }
  if ( ( d != "." ) && ( d != "" ) )
  {
    dam = d;
    parent = true;
  }
  else
  {
    dam = "";
  }
  if ( parent )
  {
    hasparents = true;
  }
  else
  {
    hasparents = false;
  }
}

void TPed::SetPed( string a , string s , string d , int i )
{
  bool parent = false;
  animal = a;
  sort_index = i;
  if ( ( s != "." ) && ( s != "" ) )
  {
    sire = s;
    parent = true;
  }
  else
  {
    sire = "";
  }
  if ( ( d != "." ) && ( d != "" ) )
  {
    dam = d;
    parent = true;
  }
  else
  {
    dam = "";
  }
  if ( parent )
  {
    hasparents = true;
  }
  else
  {
    hasparents = false;
  }
}

void TPed::SetPed( string a , string s , string d , int i , int si , int di )
{
  bool parent = false;
  animal = a;
  sort_index = i;
  if ( ( s != "." ) && ( s != "" ) )
  {
    sire = s;
    SetIndex( si , SIRE );
    parent = true;
  }
  else
  {
    sire = "";
    SetIndex( -1 , SIRE );
  }
  if ( ( d != "." ) && ( d != "" ) )
  {
    dam = d;
    SetIndex( di , DAM );
    parent = true;
  }
  else
  {
    dam = "";
    SetIndex( -1 , DAM );
  }
  if ( parent )
  {
    hasparents = true;
  }
  else
  {
    hasparents = false;
  }
}

int TPed::IsBase() const
{
  if ( !(hasparents) )
  {
    return 1;
  }
  else if ( sire != "" && dam != "" )
  {
    return 0;
  }
  else
  {
    return -1;
  }
}

void TPed::SetIndex( int index , int par )
{
  if ( par == 1 )
  {
    if ( index >= 0 )
    {
      s_index = index;
    }
  }
  else
  {
    if ( index >= 0 )
    {
      d_index = index;
    }
  }
}

void TPed::SetIndex( int index , TParents par )
{
  if ( par == SIRE )
  {
    s_index = index;
  }
  else
  {
    d_index = index;
  }
}

void TPed::SetSortIndex( int index )
{
  sort_index = index;
}

int TPed::GetIndex( TParents par ) const
{
  int index = -1;
  if ( par == SIRE )
  {
    index = s_index;
  }
  else
  {
    index = d_index;
  }
  return index;
}

bool TPed::Exists( TParents par ) const
{
  if ( par == SIRE )
  {
    if ( sire != "" )
    {
      return true;
    }
    else
    {
      return false;
    }
  }
  else
  {
    if ( dam != "" )
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}

void TPed::operator= ( const TPed &copy )
{
  if ( this == &copy )
  {
    return;
  }
  copyPed( copy );
}
/*
void TPed::operator= ( TPed &&copy )
{
  if ( this == &copy )
  {
    copy.TPed();
    return;
  }
  SetPed( copy.animal , copy.sire , copy.dam , copy.sort_index , copy.GetIndex( SIRE ) , copy.GetIndex( DAM ) );
  copy.TPed();
}
*/
bool TPed::operator< ( const TPed& T ) const
{
  if ( animal < T.animal )
  {
    return true;
  }
  else
  {
    return false;
  }
}

bool TPed::operator== ( const string& arg )
{
  string tmp = arg;
  if ( animal == tmp )
  {
    return true;
  }
  else
  {
    return false;
  }
}

void TPed::ShowPed() const
{
  Rprintf("Individual: %s Father: %s Mother: %s Index: %d", animal.c_str(), sire.c_str(), dam.c_str() , sort_index);
  if ( Exists( SIRE ) )
  {
    Rprintf(" s_index: %d", s_index);
  }
  else
  {
    Rprintf(" s_index: NULL");
  }
  if ( Exists( DAM ) )
  {
    Rprintf(" d_index: %d\n", d_index);
  }
  else
  {
    Rprintf(" d_index: NULL\n");
  }
}

void TPed::copyPed( const TPed &copy )
{
  SetPed( copy.animal , copy.sire , copy.dam , copy.sort_index , copy.GetIndex( SIRE ) , copy.GetIndex( DAM ) );
}

void Pedigree::CreatePedigree( TPedVec& T )
{
  TPedVec::iterator tp = T.begin();
  while ( !( T.empty() ) )
  {
    pedigree.insert( pedigree.end() , tp->ReturnTPed() );
    T.erase(tp);
  }
}

int Pedigree::GetPedNumber()
{
  return pedigree.size();
}

void Pedigree::ShowPed()
{
  TPedVec::iterator p = pedigree.begin();
  while ( p != pedigree.end() )
  {
    Rprintf("%d\t", p - pedigree.begin());
    p->ShowPed();
    p++;
  }
}

bool Pedigree::GetParent( int p , int i )
{
  if ( p == 0 )
  {
    if ( pedigree[i].GetIndex( SIRE ) >= 0 )
    {
      return true;
    }
    else
    {
      return false;
    }
  }
  else
  {
    if ( pedigree[i].GetIndex( DAM ) >= 0 )
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}

int Pedigree::GetSortIndex( int i )
{
  return pedigree[i].GetSortIndex();
}

int Pedigree::GetParentIndex( int par , int i )
{
  if ( par == 0 )
  {
    return pedigree[i].GetIndex( SIRE );
  }
  else
  {
    return pedigree[i].GetIndex( DAM );
  }
}

int Pedigree::GetParentIndex( int i , TParents par )
{
  return pedigree[i].GetIndex( par );
}

int Pedigree::GetIndex( string a )
{
  TPedVec::iterator p = find( pedigree.begin() , pedigree.end() , a );
  if ( p != pedigree.end() )
  {
    return p - pedigree.begin();
  }
  else
  {
    return -1;
  }
}

void Pedigree::operator= ( const Pedigree &copy )
{
  if ( this == &copy )
  {
    return;
  }
  copyPed( copy );
}

void Pedigree::copyPed( const Pedigree &copy )
{
  pedigree = copy.pedigree;
  effect_label = copy.effect_label;
  ancestor = copy.ancestor;
}
