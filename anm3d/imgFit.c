/*
 *  Routines to implement image geometry-based operations
 *
 */

#include "string.h"
#include "tk.h"
#include "ic.h"


/*
 *   Routine to provide access to image functions
 *
 */
int imgFit (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      status;
   int      length;
   register char c;
   int      listc;
   char     **listv;
   char     value[128];
   int ih, ih2, ih_in, ih_out;
   img_defn def_in, def_out, def2;

  /*
   * Check for compulsary imId argument
   */
   status = 0 ;
   if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		"imId option ?subimage? ?vals? \"", (char *) NULL);
	return TCL_ERROR;
   }
   if (ic_parseImid(interp, argv[1], &ih) != TCL_OK) {
	return TCL_ERROR;
   }
   def_in = ic_hash[ih].defn;
   def_out = ic_hash[ih].defn;

  /*
   * Sort out option to command, returning an error on an incorrect option
   */
   status = 0 ;
   c = argv[2][0];
   length = strlen(argv[2]);
   if ((c == 'v') && (strncmp(argv[2], "velocity", length) ==0)) {
	int axis;
	double d;
	float v[3];
	def_out.z1 = 1 ; def_out.z2 = 3 ; def_out.zdim = 3;
	def_out.ndims = 3;
	if (argc < 4) {
	     Tcl_AppendResult(interp, "wrong # args: should be \"",
		      "imId velocity gate ?axis? \"", (char *) NULL);
	     return TCL_ERROR;
	}
	def_out.ndata = def_out.xdim*def_out.ydim*def_out.zdim;
        if (ic_imgCreate( &def_out, &ih) != IC_OK ) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
		return TCL_ERROR;
	}
	axis = 3;
	if (argc > 4) {
           if (Tcl_GetInt(interp, argv[4], &axis) != TCL_OK) {
		return TCL_ERROR;
	   }
	}
	v[0] = d;
	img_fitvel_( &def_in, def_in.data_p, &def_out, def_out.data_p, v, &axis, &status );
	if (status != IC_OK) {
	   interp->result = "unable to fit velocity profile";
	   return TCL_ERROR;
	}
	ic_resultImid( interp, ih);


   } else {
	Tcl_AppendResult(interp, "bad option \"", argv[2],
		"\": should be velocity,",
		"\"",
		(char *) NULL);
	return TCL_ERROR;
   }
   return TCL_OK;
}
