/*                                                                 */
/* Utility functions                                               */
/* Written by DAH 08/02/2000                                       */
/*                                                                 */

#ifndef MATH_UTILITIES_H
#define MATH_UTILITIES_H

#ifndef _PI_
#define _PI_ 3.141592653589793238462643383279502884197169399375
#endif

#ifndef _EPS_
#define _EPS_ 2.2204e-16
#endif

#ifndef _FPMIN_
#define _FPMIN_ 1.0e-30
#endif

using namespace std;

#include <R.h>
#include <Rdefines.h>
#include <vector>
#include <map>
#include <algorithm>
#include <math.h>
#include <cerrno>

class StdVals;

typedef vector< double > DVec;
typedef vector< int > IVec;
typedef vector< double > StdVec;
typedef map< double , StdVec , less< double > > StdMap;
typedef StdMap::value_type StdValue;
typedef map< int , double , less< int > > SimMap;
typedef SimMap::value_type SimValue;
typedef map< string , StdVals , less< string > > StdValMap;
typedef StdValMap::value_type StdValValue;

inline double dabs( double );
inline double erfinv( double );
inline int sign( int );
inline int sign( double );
inline double stdnormal_inv( double );
inline double stdnormal_cdf( double );
inline double lGamma( double );
inline double incomplete_beta( double , double , double );
inline double betacf( double , double , double );
inline double t_cdf( double , int );
inline double f_cdf( double , int , int );
inline double binomial_cdf( double , int , int );

class StdVals
{
  public:
    StdVals() { nu = 0; inu = 0.0; std = 0.0; }
    StdVals( unsigned int n1 , double n2 , double s ) { nu = n1; inu = n2; std = s; }
    ~StdVals() {}
    void SetNu( unsigned int n ) { nu = n; }
    void SetINu( double n ) { inu = n; }
    void SetStd( double s ) { std = s; }
    unsigned int GetNu() const { return nu; }
    double GetINu() const { return inu; }
    double GetStd() const { return std; }
  private:
    unsigned int nu;
    double inu, std;
};

inline double dabs( double var )
{
  double tmp = -1.0 * var;
  if ( var > tmp )
  {
    var *= 1.0;
  }
  else
  {
    var = tmp;
  }
  if ( var <= 0.0 )
  {
    Rprintf("Warning: var < 0.0 in dabs( %f )\n", var);
  }
  return var;
}

inline double erfinv( double x )
{
  double y = 0.0;
  if ( ( x > -1.0 ) && ( x < 1.0 ) )
  {
    double s = sqrt( _PI_ ) / 2.0, z_old = 1.0, z_new = sqrt( -log( 1.0 - dabs( x ) ) ) * double( sign( x ) ), tol = _EPS_;
    int maxit = 100, iterations = 1;
    while ( dabs( erf( z_old ) - x ) > tol * dabs( x ) )
    {
      z_old = z_new;
      z_new = z_old - ( erf( z_old ) - x ) * exp( pow( z_old , 2.0 ) ) * s;
      if ( ++iterations > maxit )
      {
        Rprintf("Warning: maximum iterations in erfinv exceeded\n");
	break;
      }
    }
    y = z_new;
  }
  return y;
}

inline int sign( int x )
{
  if ( x < 0 )
  {
    return -1;
  }
  else
  {
    return 1;
  }
}

inline int sign( double x )
{
  if ( x < 0.0 )
  {
    return -1;
  }
  else
  {
    return 1;
  }
}

inline double stdnormal_inv( double x )
{
  return sqrt( 2.0 ) * erfinv( ( 2.0 * x ) - 1.0 );
}

inline double stdnormal_cdf( double x )
{
  return ( 1.0 + erf( x / sqrt( 2.0 ) ) ) / 2.0;
}

inline double gammaln( double gam )
{
  double x = gam, y = gam, tmp = x + 5.5, ser = 1.000000000190015;
  DVec coef;
  coef.insert( coef.end() , 76.1800917247146 );
  coef.insert( coef.end() , -86.50532032941677 );
  coef.insert( coef.end() , 24.01409824083091 );
  coef.insert( coef.end() , -1.231739572450155 );
  coef.insert( coef.end() , 0.1208650973866179e-2 );
  coef.insert( coef.end() , -0.5395239384953e-5 );
  tmp -= ( x + 0.5 ) * log( tmp );
  for ( int i = 0; i < 6; i++ )
  {
    ser += coef[ i ] / ++y;
  }
  return -tmp + log( 2.5066282746310005 * ser / x );
}

inline double incomplete_beta( double x , double a , double b )
{
  double beta = 0.0;
  if ( ( x < 0.0 ) || ( x > 1.0 ) )
  {
    Rprintf("Error: x outside of parameter bounds!\n");
    exit( 0 );
  }
  if ( ( x == 0.0 ) || ( x == 1.0 ) )
  {
    beta = 0.0;
  }
  else
  {
    beta = exp( gammaln( a + b ) - gammaln( a ) - gammaln( b ) + a * log( x ) + b * log( 1 - x ) );
  }
  if ( x < ( a + 1.0 ) / ( a + b + 2.0 ) )
  {
    beta *= betacf( a , b , x ) / a;
  }
  else
  {
    beta = 1.0 - beta * betacf( b , a , 1.0 - x ) / b;
  }
  return beta;
}

inline double betacf( double a , double b , double x )
{
  double qab = a + b, qap = 1.0 + a, qam = a - 1.0, c = 1.0, d = 1.0 - qab * x / qap;
  bool maxit = false;
  if ( fabs( d ) < _FPMIN_ )
  {
    d = _FPMIN_;
  }
  d = 1.0 / d;
  double h = d;
  for ( int m = 0; m < 100; m++ )
  {
    int m2 = 2 * m;
    double aa = double( m ) * ( b - double( m ) ) * x / ( ( qam + double( m2 ) ) * ( a + double( m2 ) ) );
    d = 1.0 + aa * d;
    if ( fabs( d ) < _FPMIN_ )
    {
      d = _FPMIN_;
    }
    c = 1.0 + aa / c;
    if ( fabs( c ) < _FPMIN_ )
    {
      c = _FPMIN_;
    }
    d = 1.0 / d;
    h *= d * c;
    aa = -( a + double( m ) ) * ( qab + double( m ) ) * x / ( ( a + double( m2 ) ) * ( qap + double( m2 ) ) );
    d = 1.0 + aa * d;
    if ( fabs( d ) < _FPMIN_ )
    {
      d = _FPMIN_;
    }
    c = 1.0 + aa / c;
    if ( fabs( c ) < _FPMIN_ )
    {
      c = _FPMIN_;
    }
    d = 1.0 / d;
    double del = d * c;
    h *= del;
    if ( fabs( del - 1.0 ) < _EPS_ )
    {
      break;
    }
    if ( m == 99 )
    {
      maxit = true;
    }
  }
  if ( maxit )
  {
    Rprintf("a or b may be too large, or number of iterations too small\n");
    exit( 0 );
  }
  return h;
}

inline double t_cdf( double t , int df )
{
  return 1.0 - incomplete_beta( double( df ) / ( double( df ) + pow( t , 2.0 ) ) , double( df ) / 2.0 , 0.5 );
}

inline double f_cdf( double f , int df1 , int df2 )
{
  return 1.0 - incomplete_beta( double( df2 ) / ( double( df2 ) + double( df1 ) * f ) , double( df2 ) / 2.0 , double( df1 ) / 2.0 );
}

inline double binomial_cdf( double p , int n , int k )
{
  return incomplete_beta( p , k , n - k + 1 );
}

#endif
