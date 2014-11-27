#include "tk.h"
#include "string.h"

/*
 *   Routine to decode a list specified by variable name into array elements
 */
void
eval_list_ ( interp, xlist, ndata, x, s )
   Tcl_Interp *interp;
   char  xlist[40];
   int   *ndata;
   float *x;
   int   *s;
{
   int ndx;
   char **xa;
   double dx;
   int i;
   ndx = 0 ; *ndata = 0;
   Tcl_SplitList( interp, 
                  Tcl_GetVar( interp, xlist, TCL_GLOBAL_ONLY ), 
                  &ndx, &xa);
   *ndata = ndx;
   for (i=0; i < ndx ; i++) {
       Tcl_GetDouble( interp, xa[i], &dx );
       x[i] = dx;
   }
}

