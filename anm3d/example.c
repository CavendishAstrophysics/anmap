/*
 *  Routines to implement image geometry-based operations
 *
 */

#include "string.h"
#include "tk.h"
#include "ic.h"

/*
 * Create a fixed-sized hash table, a hash record and a definition record
 *
 */
 extern ic_hashRec ic_hash[HASH_SIZE];
 extern int hashMode;
 extern img_defn imgDefn;
 extern ic_iclRec iclRec;

/*
 *   Routine to provide access to image functions
 *
 */
int imgGeom (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      length;
   register char c;
   int      listc;

  /*
   * Check for compulsary imId argument
   */
   if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		"imId option ?subimage? ?vals? \"", (char *) NULL);
	return TCL_ERROR;
   }

  /*
   * Sort out option to command, returning an error on an incorrect option
   */
   status = 0 ;
   c = argv[2][0];
   length = strlen(argv[2]);
   if ((c == 's') && (strncmp(argv[2], "subimage", length) ==0)) {
 	img_subim( );

   } else if ((c == 'p') && (strncmp(argv[2], "project", length) ==0)) {
	int l, axis;
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " imId project axis \"", (char *) NULL);
	    return TCL_ERROR;
	}
	l = strlen(argv[3]);
	if (strncmp(argv[3], "x", l) ==0) {
	   axis = 1;
	} else if (strncmp(argv[3], "y", l) ==0) {
	   axis = 2;
	} else if (strncmp(argv[3], "z", l) ==0) {
	   axis = 3;
	} else if (strncmp(argv[3], "t", l) ==0) {
	   axis = 4; 
	} else if (strncmp(argv[3], "v", l) ==0) {
	   axis = 5;
	} else {
	   interp->result = "unrecognized axis";
	   return TCL_ERROR;
	}
	img_project(axis);

   } else if ((c == 'b') && (strncmp(argv[2], "bshift", length) ==0)) {
	int ii, ll, k, dv[4] = {0, 0, 0, 0};
        for (ii=3; ii < argc; ii++) {
	  k = -1;
	  if (strcmp(argv[ii], "-dx") ==0) { k = 0;
	  } else if (strcmp(argv[ii], "-dy") ==0) { k = 1;
	  } else if (strcmp(argv[ii], "-dz") ==0) { k = 2;
	  } else if (strcmp(argv[ii], "-dt") ==0) { k = 3;
	  }
	  if ( k >= 0 ) {
	    if ( (ii+1) >= argc) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " -d(x|y|z|t) I \"", (char *) NULL);
		return TCL_ERROR;
	    }
	    ii++;
	    if (Tcl_GetInt( interp, argv[ii], &ll) != TCL_OK ) {
		return TCL_ERROR;
	    }
	    dv[k] = ll;
	  }
	}
	img_bshift( dv, k );



   } else {
	Tcl_AppendResult(interp, "bad option \"", argv[2],
		"\": should be subimage, bin, project, flip,",
		"bshift, transpose, transform, slice\"",
		(char *) NULL);
	return TCL_ERROR;
   }
   return TCL_OK;
}
