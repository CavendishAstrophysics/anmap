/*
 *  Interface routine to evaluate functions specified in a free
 *  format notation.
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include "string.h"
#include "eqn.h"


void eval_eqn( eqn_string, nvals, x, y )
  char    *eqn_string;
  int     nvals;
  double  *x, *y;
{
  int      i;
  eqnode   *eqn;
  eqnode   *deqn;
  eqnode   *subeqn;

  eqn = sscan_eqn( eqn_string );
  eval_funs(eqn);

  for (i=0; i < nvals; ++i) {
    deqn = duplicate(eqn);
    subeqn = assign( "x", x[i] );
    substitute(deqn,subeqn);
    eval_funs(deqn);
    y[i] = eqnval( deqn );
  };
  free_eqn_tree( eqn ) ;
  free_eqn_tree( deqn ) ;
  free_eqn_tree( subeqn ) ;
}


void eval_eqn_( eqn, n, x, y )
  char     eqn[256];
  long int *n;
  double   *x, *y;
{
  int nvals;
  nvals = *n;
  eval_eqn( eqn, nvals, x, y );
}


void eval_array( eqn_string, iflag, i1, j1, x1, x2, y1, y2, ni, nj, array )
  char    *eqn_string;
  int     iflag, i1, j1,x1, x2, y1, y2, nj, ni;
  float  *array;
{
  int      i, j, ii, jj, n;
  double   x, y;
  eqnode   *eqn;
  eqnode   *deqn;
  eqnode   *subeqn;

  eqn = sscan_eqn( eqn_string );
  eval_funs(eqn);
  for (j=y1; j >= y2; --j) {
    jj = j1 - j;
    for (i=x1; i <= x2; ++i) {
      deqn = duplicate(eqn);
      ii = i - i1;
      x = i ; y = j;
      subeqn = assign( "x", x );
      substitute(deqn,subeqn);
      subeqn = assign( "y", y );
      substitute(deqn,subeqn);
      eval_funs(deqn);
      n = jj*ni + ii;
      if (iflag==1) {
        array[n] = array[n] + eqnval( deqn );
      } else {
        if (iflag==2) {
          array[n] = array[n] * eqnval( deqn );
        } else {
          array[n] = eqnval( deqn );
        }
      }
    }
  }
  free_eqn_tree( eqn ) ;
  free_eqn_tree( deqn ) ;
  free_eqn_tree( subeqn ) ;
}


void eval_array_( eqn, iflag, i1, j1, x1, x2, y1, y2, ni, nj, array )
  char     eqn[256];
  long int *iflag, *i1, *j1, *x1, *x2, *y1, *y2, *ni, *nj;
  float    *array;
{
  int  siflag, si1, sj1, sx1, sx2, sy1, sy2, snj, sni;
  siflag = *iflag;
  si1=*i1; sj1=*j1; sx1=*x1; sx2=*x2; sy1=*y1; sy2=*y2; sni=*ni; snj=*nj;
  eval_array( eqn, siflag, si1, sj1, sx1, sx2, sy1, sy2, sni, snj, array);
}
