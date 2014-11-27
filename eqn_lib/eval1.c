#include <stdlib.h>
#include <stdio.h>
#include "string.h"
#include "eqn.h"

void eval_eqn( eqn_string, nvals, x, y )
  char    *eqn_string;
  int     nvals;
  double  *x, *y;
{
  eqn_funs *fun_list;
  int      i;
  eqnode   *eqn;
  eqnode   *deqn;
  eqnode   *subeqn;

  fun_list = add_standard_functions(NULL);
  set_input_functions(fun_list);

  eqn = sscan_eqn( eqn_string );
  eval_funs(eqn);

  for (i=0; i < nvals; ++i) {
    deqn = duplicate(eqn);
    subeqn = assign( "x", x[i] );
    substitute(deqn,subeqn);
    eval_funs(deqn);
    y[i] = eqnval( deqn );
  };
};

main(argc, argv)
int argc; char **argv;
{
  double   x[10], y[10];
  int      i, nvals;
  nvals = 10;
  for (i=0; i < nvals; ++i) {
   x[i] = 0.1*(i+1);
  };
  eval_eqn( argv[1], nvals, x, y );
  for (i=0; i < nvals; ++i) {
    printf("\n x = %8.6f y %8.6f = ",x[i],y[i]);
  };
}

