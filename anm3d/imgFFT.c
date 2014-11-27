/*
 *  Routines to implement an image library toolset
 *
 */
#include "tk.h"
#include "string.h"
#include "ic.h"

/*
 * FFT routines
 */
/*
 *   Routine to provide the img_fft command
 *
 */
int imgFFT (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   char     value[128]
   int      i, l, type, dirn, status; 
   int      ih, ih_out, ih_work;
   img_defn def_in, def_out, def_work;

  /*
   * Check for compulsary imId argument
   */ 
   if (argc < 2) {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
		      "imId ?options? \"", (char *) NULL);
     return TCL_ERROR;
   }
   if (ic_parseImid(interp, argv[1], &ih) != TCL_OK) {
	return TCL_ERROR;
   }
   if ( def_in.ndims > 2 ) {
     Tcl_AppendResult(interp, "Error require ndims <= 2", (char *) NULL);
     return TCL_ERROR;
   }
   def_in = ic_hash[ih].defn;
   def_out = ic_hash[ih].defn;
   def_work = ic_hash[ih].defn;

  /*
   * loop though all arguments and sort out options
   */
   type = 1; dirn = 1
   for ( i = 2 ; i < argc ; i++) {
	l = strlen(argv[i]);
	if (strncmp(argv[i], "-forward", l) == 0) {
	    dirn = 1;
	} else if (strncmp(argv[i], "-reverse", l) == 0) {
	    dirn = -1;
	} else if (strncmp(argv[i], "-inverse", l) == 0) {
	    dirn = -1;
	}
   }
   if ( def_in.vdim == 1 ) {
	dirn = 1; type = 1;
        def_out.vdim = 2 ; def_out.v1 = 1; def_out.v2 = 2;
	def_out.ndata = 
   }
   if ( def_in.vdim > 2 ) {
     Tcl_AppendResult(interp, "Error require vdim <= 2", (char *) NULL);
     return TCL_ERROR;
   }
   status = 0;
   img_fft_( &def_in, def_in.data_p, def_out.data_p, def_work.data_p,
             &dirn, &type, &status );
   if (status != IC_OK) {
     Tcl_AppendResult(interp, "Error performing FFT", (char *) NULL);
     return TCL_ERROR;
   }
   sprintf( interp->results, "%d", ih_out );
   return TCL_OK;
}

