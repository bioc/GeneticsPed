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

#ifndef PEDTEMPLATE_H
#define PEDTEMPLATE_H

#define R_NO_REMAP

#include <R.h>
#include <Rdefines.h>
#include <string>
#include <algorithm>
#include <vector>

using namespace std;

class TPed;
class Pedigree;

typedef enum{ SIRE , DAM } TParents;
typedef vector< TPed > TPedVec;

class TPed
{
  public:
    TPed();
    TPed( string , string , string );
    TPed( string , string , string , int );
    TPed( string );
    TPed( const TPed &copy );
    ~TPed();
    string ReturnAnimal() const;
    string ReturnSire() const;
    string ReturnDam() const;
    int ReturnIndex() const;
    TPed ReturnTPed() const { return *this; }
    void SetPed( string , string , string );
    void SetPed( string , string , string , int );
    void SetPed( string , string , string , int , int , int );
    int IsBase() const;
    int Compare( TPed );
    void SetIndex( int , TParents );
    void SetIndex( int &sindex , int );
    void SetSortIndex( int );
    int GetIndex( TParents ) const;
    int GetSortIndex() const { return sort_index; }
    bool Exists( TParents ) const;
    bool operator< ( const TPed& T ) const;
    bool operator== ( const string& arg );
    void operator= ( const TPed &copy );
    void ShowPed() const;
  private:
    void copyPed( const TPed &copy );
    string animal, sire, dam;
    int *s_index, *d_index, sort_index;
    bool hasparents;
};

class Pedigree
{
  public:
    Pedigree() { ancestor = false; effect_label = ""; }
    ~Pedigree() { pedigree.erase( pedigree.begin() , pedigree.end() ); }
    void CreatePedigree( TPedVec& );
    void SetAncestor() { ancestor = true; }
    void SetLabel( string label ) { effect_label = label; }
    int IsBase( int i ) { return pedigree[i].IsBase(); }
    bool GetParent( int , int );
    bool Ancestor() { return ancestor; }
    int GetSortIndex( int );
    int GetParentIndex( int , int );
    int GetParentIndex( int , TParents );
    int GetPedNumber();
    void ShowPed();
    int GetIndex( string );
    string GetLabel() { return effect_label; }
    string GetMember( int i ) { return pedigree[ i ].ReturnAnimal(); }
    void Erase() { pedigree.erase( pedigree.begin() , pedigree.end() ); }
    void operator= ( const Pedigree &copy );
  private:
    void copyPed( const Pedigree &copy );
    TPedVec pedigree;
    string effect_label;
    bool ancestor;
};

#endif
