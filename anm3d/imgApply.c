/*
 *  Routines to implement image basic operations
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
int imgApply (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      status;
   int      i, ii, l, op;
   int      n, nn, nd;
   img_vect vec; 
   double   d; 
   float    v[3];
   int      ih,  ih2, ih3, ih_in, ih_out;
   img_defn def_in, def_out, def2, def3;

  /*
   * Check for compulsary imId argument
   */ 
   if (argc < 2) {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
		      "imId ?subimage? ?actions? \"", (char *) NULL);
     return TCL_ERROR;
   }
   if (ic_parseImid(interp, argv[1], &ih) != TCL_OK) {
	return TCL_ERROR;
   }
   def_in = ic_hash[ih].defn;
   def_out = ic_hash[ih].defn;

  /*
   * Parse for subimage to operate on
   */
   if (ic_parseSubIm( interp, 2, argc, argv, &def_out) != IC_OK ) {
	return TCL_ERROR;
   }

  /*
   * loop though all arguments and take action depending on supplied option
   */
   status = 0;
   for ( i = 2 ; i < argc ; i++) {
	l = strlen(argv[i]);
	if (status == 0) {
	     if ( (strncmp(argv[i], "=", l) == 0) || 
		  (strncmp(argv[i], "==", l) == 0) ||
		  (strncmp(argv[i], "-=", l) == 0) ||
		  (strncmp(argv[i], "-==", l) == 0) ||
		  (strncmp(argv[i], "+=", l) == 0) ||
		  (strncmp(argv[i], "+==", l) == 0) ||
		  (strncmp(argv[i], "/=", l) == 0) ||
		  (strncmp(argv[i], "/==", l) == 0) ||
		  (strncmp(argv[i], "*=", l) == 0) ||
		  (strncmp(argv[i], "*==", l) == 0) ) {
		ii = i; i++;
		if (i == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId1 op imId2 \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (ic_parseImid(interp, argv[i], &ih2) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		if (strncmp(argv[ii], "=", l) == 0) {
		  op = 1;
		} else if (strncmp(argv[ii], "==", l) == 0) {
		  op = -1;
		} else if (strncmp(argv[ii], "+=", l) == 0) {
		  op = 2;
		} else if (strncmp(argv[ii], "+==", l) == 0) {
		  op = -2;
		} else if (strncmp(argv[ii], "-=", l) == 0) {
		  op = 3;
		} else if (strncmp(argv[ii], "-==", l) == 0) {
		  op = -3;
		} else if (strncmp(argv[ii], "*=", l) == 0) {
		  op = 4;
		} else if (strncmp(argv[ii], "*==", l) == 0) {
		  op = -4;
		} else if (strncmp(argv[ii], "/=", l) == 0) {
		  op = 5;
		} else if (strncmp(argv[ii], "/==", l) == 0) {
		  op = -5;
		}
		def2 = ic_hash[ih2].defn;
		img_combine_( &def_in, &def_out, def_in.data_p, &op,
                              &def2, def2.data_p, &status );
	     } else if (strcmp(argv[i], "atan2") == 0) {
		i++;
		if ( (i+1) == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId1 atan2 imId2 imId3 \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (ic_parseImid(interp, argv[i], &ih2) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		i++;
        	if (ic_parseImid(interp, argv[i], &ih3) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		def2 = ic_hash[ih2].defn;
		def3 = ic_hash[ih3].defn;
		op = 1;
		img_combine2_( &def_in, &def_out, def_in.data_p, &op,
                               &def2, def2.data_p, &status, 
                               &def3, def3.data_p, &status );
	     } else if (strcmp(argv[i], "log") == 0) {
		op = 1;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "ln", l) == 0) {
		op = 1;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strcmp(argv[i], "log10") == 0) {
		op = 2;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "exp", l) == 0) {
		op = 3;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "10", l) == 0) {
		op = 5; v[0] = 10.0;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "raise", l) == 0) {
		i++;
		if (i == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId raise X \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d;
		op = 5;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "pow", l) == 0) {
		i++;
		if (i == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId pow X \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d;
		op = 6;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "sin", l) == 0) {
		op = 7;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "cos", l) == 0) {
		op = 8;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "tan", l) == 0) {
		op = 9;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "asin", l) == 0) {
		op = 10;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "acos", l) == 0) {
		op = 11;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "atan", l) == 0) {
		op = 12;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "scale", l) == 0) {
		i++;
		if (i == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId scale X \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d;
		op = 13;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "cscale", l) == 0) {
		i++;
		if (i == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId cscale X \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d;
		nn = img_IndexInit( def_in, def_out, &nd, &vec );
		for (n = 0 ; n < nd ; n++) {
		   if ( (nn >= 0) && (def_in.data_p[nn] != def_in.blank) ) {
			def_in.data_p[nn] = def_in.data_p[nn] * v[0];
			nn = img_IndexNext( def_in, def_out, &vec );
		   }
		}

	     } else if (strncmp(argv[i], "offset", l) == 0) {
		i++;
		if (i == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," apply imId scale X \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d;
		op = 14;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "binary", l) == 0) {
		i++;
		if (i == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId binary X \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d;
		op = 15;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "set", l) == 0) {
		i++;
		if (i == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId set X \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d;
		op = 16;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "pass", l) == 0) {
		i++;
		if ( (i+2) == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId pass min max X \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d; i ++;
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[1] = d; i ++;
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[2] = d;
		op = 17;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "range", l) == 0) {
		i++;
		if ( (i+3) == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId range min max op X \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d; i ++;
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[1] = d; i ++;
 		if (strcmp(argv[i], "=") == 0) { op = 30;
		} else if (strcmp(argv[i], "+") == 0) { op = 31;
		} else if (strcmp(argv[i], "-") == 0) { op = 32;
		} else if (strcmp(argv[i], "*") == 0) { op = 33;
		} else if (strcmp(argv[i], "/") == 0) { op = 34; }
		i ++;
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[2] = d;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "quantize", l) == 0) {
		i++;
		if ( (i+2) == argc) {
		  Tcl_AppendResult(interp, "wrong # args: should be \"",argv[0],
			" imId quantize min max increment \"", (char *) NULL);
		  return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d; i ++;
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[1] = d; i ++;
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[2] = d; i ++;
		op = 22;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "blank", l) == 0) {
		op = 18;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "unblank", l) == 0) {
		i++;
		if (i == argc) {
		   Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0]," imId unblank X \"", (char *) NULL);
		   return TCL_ERROR;
		}
        	if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    	    return TCL_ERROR;
		}
		v[0] = d;
		op = 19;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "inv", l) == 0) {
		op = 20;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     } else if (strncmp(argv[i], "sqrt", l) == 0) {
		op = 21;
		img_apply_( &def_in, &def_out, def_in.data_p, &op, v, &status );
	     }
	}
   }
   if (status == IC_OK) {
	   ic_resultImid( interp, ih );
   } else {
	   interp->result = "error modifying image";
	   return TCL_ERROR;
   }
   return TCL_OK;
}
