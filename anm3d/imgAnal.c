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
int imgAnal (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      status;
   int      length;
   register char c;
   char     value[128];
   int      ih;
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
   * parse for subimage
   */
   if (ic_parseSubIm( interp, 3, argc, argv, &def_out) != IC_OK ) {
	return TCL_ERROR;
   }

  /*
   * Sort out option to command, returning an error on an incorrect option
   */
   status = 0 ;
   c = argv[2][0];
   length = strlen(argv[2]);
   if ( ((c == 's') && (strncmp(argv[2], "statistics", length) == 0)) ||
	((c == 's') && (strncmp(argv[2], "sum", length) == 0)) ||
	((c == 'm') && (strncmp(argv[2], "maxmin", length) == 0)) ) {
	int    i, ist[11];
	float  rst[4], gates[4];
	double d;
	int    sttype, i1, i2;
	
	/* sort out the specific option --- the above options all support
	 * the same range of parameters hence */
	if ( strncmp(argv[2], "statistics", length) == 0 ) {
	    sttype = 0;
	} else if ( strncmp(argv[2], "maxmin", length) == 0 ) {
	    sttype = 1;
	} else if ( strncmp(argv[2], "sum", length) == 0 ) {
	    sttype = 2;
	}

	/* parse options passed to the command */
	gates[0] = -1.0E30 ; gates[1] = -1.0;
	gates[2] = -1.0E30 ; gates[3] = 1.0E30 ;
	sttype = 0;
	for ( i = 3 ; i < argc ; i++) {
	   if (strcmp(argv[i], "-gate") == 0) {
		if ( (i+1) < argc ) {
		   i++;
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
		      return TCL_ERROR;
		   }
		   gates[0] = d;
		} else {
		    Tcl_AppendResult(interp, "wrong # args: should be \"",
		       " -gate value \"", (char *) NULL);
		    return TCL_ERROR;
		}
	   } else if (strcmp(argv[i], "-agate") == 0) {
		if ( (i+1) < argc ) {
		   i++;
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
		      return TCL_ERROR;
		   }
		   gates[1] = d;
		} else {
		    Tcl_AppendResult(interp, "wrong # args: should be \"",
		       " -gate value \"", (char *) NULL);
		    return TCL_ERROR;
		}
	   } else if (strcmp(argv[i], "-min") == 0) {
		if ( (i+1) < argc ) {
		   i++;
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
		      return TCL_ERROR;
		   }
		   gates[2] = d;
		} else {
		    Tcl_AppendResult(interp, "wrong # args: should be \"",
		       " -min value \"", (char *) NULL);
		    return TCL_ERROR;
		}
	   } else if (strcmp(argv[i], "-max") == 0) {
		if ( (i+1) < argc ) {
		   i++;
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
		      return TCL_ERROR;
		   }
		   gates[3] = d;
		} else {
		    Tcl_AppendResult(interp, "wrong # args: should be \"",
		       " -max value \"", (char *) NULL);
		    return TCL_ERROR;
		}
	   } else if (strcmp(argv[i], "-stats") == 0) {
		sttype = 0;
	   } else if (strcmp(argv[i], "-maxmin") == 0) {
		sttype = 1;
	   } else if (strcmp(argv[i], "-sum") == 0) {
		sttype = 2;
	   }
	}

	/* execute the command */
	if ( sttype == 0 ) {
	   img_stats_(&def_in, &def_out, def_in.data_p, gates, ist, rst, &status);
	} else if ( sttype == 1) {
	   img_maxmin_(&def_in, &def_out, def_in.data_p, ist, rst, &status);
	} else if ( sttype == 2) {
	   img_sum_(&def_in, &def_out, def_in.data_p, gates, rst, &status);
	}

	/* return results --- again this depends on the command performed */
	if (status == 0) {
	   if ( sttype == 0 ) {
	      i1 = 4; i2 = 11;
	   } else if ( sttype == 1 ) {
	      i1 = 2; i2 = 10;
	   } else if ( sttype == 2 ) {
	      i1 = 1; i2 = 0;
	   }
	   for (i=0 ; i < i1 ; i++ ) {
		sprintf( value, "%f", rst[i] );
		Tcl_AppendElement( interp, value );
	   }
	   for (i=0 ; i < i2 ; i++ ) {
		sprintf( value, "%d", ist[i] );
		Tcl_AppendElement( interp, value );
	   }
	} else {
	   interp->result = "unable to obtain image statistics";
	   return TCL_ERROR;
	}


   } else if ((c == 'h') && (strncmp(argv[2], "histogram", length) ==0)) {
	img_vect vec; double d;
	int l, n, nn, mm, nd, ii, ist[11]; float low, high, rst[4], gates[2];
	nd = 100; gates[0] = -1.0E+30 ; gates[1] = -1.0;
	img_stats_(&def_in, &def_out, def_in.data_p, gates, ist, rst, &status);
	low = rst[0] ; high = rst[1];
        for (ii=3; ii < argc; ii++) {
	  l = strlen(argv[ii]);
	  if (strncmp(argv[ii], "-ndata", l) ==0) {
	     if ( (ii+1) < argc ) {
		ii++;
		if (Tcl_GetInt( interp, argv[ii], &nd ) != TCL_OK) {
	    	   return TCL_ERROR;
		}
	     } else {
	        Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " histogram imId ?-ndata n? \"", (char *) NULL);
	        return TCL_ERROR;
	     }
	  } else if (strncmp(argv[ii], "-low", l) ==0) {
	     if ( (ii+1) < argc ) {
		ii++;
		if (Tcl_GetDouble( interp, argv[ii], &d ) != TCL_OK) {
	    	   return TCL_ERROR;
		} ; low = d;
	     } else {
	        Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " histogram imId ?-low X? \"", (char *) NULL);
	        return TCL_ERROR;
	     }
	  } else if (strncmp(argv[ii], "-high", l) ==0) {
	     if ( (ii+1) < argc ) {
		ii++;
		if (Tcl_GetDouble( interp, argv[ii], &d ) != TCL_OK) {
	    	   return TCL_ERROR;
		} ; high = d;
	     } else {
	        Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " histogram imId ?-high X? \"", (char *) NULL);
	        return TCL_ERROR;
	     }
	  }
	}
	def2.ndims = 1; def2.ndata = 2*nd;
	def2.norm = 1.0; def2.blank = def_in.blank;
	def2.dtype = def_in.dtype;
	def2.xdim = nd; def2.ydim = 1; def2.zdim = 1;
	def2.tdim = 1; def2.vdim = 2;
        if (ic_imgCreate( &def2, &ih) != IC_OK ) {
	    return TCL_ERROR;
	}
	for (n = 0; n < nd; n++) {
	    def2.data_p[2*n] = low + n*(high-low)/( (float)(nd-1) );
	    def2.data_p[2*n+1] = 0.0;
	}
	nn = img_IndexInit( def_in, def_out, &mm, &vec );
	for (n = 0 ; n < mm ; n++) {
	    if ( nn >= 0 ) {
		if ( def_in.data_p[nn] != def_in.blank ) {
		    ii = nd*( (def_in.data_p[nn]-low)/(high-low) );
		    if ( (ii>=0) && (ii<nd) ) def2.data_p[2*ii+1] += 1.0;
		}
	    }
	    nn = img_IndexNext( def_in, def_out, &vec );
	}
	sprintf( interp->result, "%d", ih);
	return TCL_OK;


   } else {
	Tcl_AppendResult(interp, "bad option \"", argv[2],
		"\": should be histogram, maxmin, statistics or sum\"",
		(char *) NULL);
	return TCL_ERROR;
   }
   return TCL_OK;
}
