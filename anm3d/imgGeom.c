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
   if ((c == 's') && (strncmp(argv[2], "subimage", length) ==0)) {
        if (ic_parseSubIm( interp, 3, argc, argv, &def_out) != IC_OK ) {
	    return TCL_ERROR;
	}
        if (ic_imgCreate( &def_out, &ih) != IC_OK ) {
	    return TCL_ERROR;
	}
	img_subim_( &def_in, def_in.data_p, &def_out, def_out.data_p, &status );
	if (status != IC_OK) {
	   interp->result = "unable to define sub-image";
	   return TCL_ERROR;
	}
	ic_resultImid( interp, ih);

   } else if ((c == 'p') && (strncmp(argv[2], "project", length) ==0)) {
	int l, axis;
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " imId project axis \"", (char *) NULL);
	    return TCL_ERROR;
	}
	l = strlen(argv[3]);
	if (strncmp(argv[3], "x", l) ==0) {
	   axis = 1; def_out.ndata = def_out.ndata/def_out.xdim;
	   def_out.xdim = 1; def_out.x1 = 1; def_out.x2 = 1;
	} else if (strncmp(argv[3], "y", l) ==0) {
	   axis = 2; def_out.ndata = def_out.ndata/def_out.ydim;
	   def_out.ydim = 1; def_out.y1 = 1; def_out.y2 = 1;
	} else if (strncmp(argv[3], "z", l) ==0) {
	   axis = 3; def_out.ndata = def_out.ndata/def_out.zdim;
	   def_out.zdim = 1; def_out.z1 = 1; def_out.z2 = 1;
	} else if (strncmp(argv[3], "t", l) ==0) {
	   axis = 4; def_out.ndata = def_out.ndata/def_out.tdim;
	   def_out.tdim = 1; def_out.t1 = 1; def_out.t2 = 1;
	} else if (strncmp(argv[3], "v", l) ==0) {
	   axis = 5; def_out.ndata = def_out.ndata/def_out.vdim;
	   def_out.vdim = 1; def_out.v1 = 1; def_out.v2 = 1;
	} else {
	   interp->result = "unrecognized axis";
	   return TCL_ERROR;
	}
	if (ic_imgCreate( &def_out, &ih) != IC_OK ) {
	    return TCL_ERROR;
	}
	img_project_(&def_in, def_in.data_p, &axis, def_out.data_p, &status);
	if (status != IC_OK) {
	   interp->result = "unable to project sub-image";
	   ic_imgDestroy( ih );
	   return TCL_ERROR;
	}
	ic_resultImid( interp, ih);

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
	status = 0;
	if (ic_imgCreate( &def_out, &ih2) != IC_OK ) {
	    return TCL_ERROR;
	}
	img_bshift_(&def_in, def_in.data_p, def_out.data_p, dv, &status);
	if (status != IC_OK) {
	   interp->result = "unable to do barrel shift on image";
	   ic_imgDestroy( ih2 );
	   return TCL_ERROR;
	}
	ic_imgAssign( ih2, ih );
	ic_resultImid( interp, ih);

   } else if ((c == 'b') && (strncmp(argv[2], "bin", length) ==0)) {
	int        ii, ll, dv[4] = {1, 1, 1, 1};
	int        nn, mm, nd, md, n, m, i, j, k, l;
	img_vect   vec, vec1;
        for (ii=3; ii < argc; ii++) {
	  k = -1;
	  if (strcmp(argv[ii], "-x") ==0) { k = 0;
	  } else if (strcmp(argv[ii], "-y") ==0) { k = 1;
	  } else if (strcmp(argv[ii], "-z") ==0) { k = 2;
	  } else if (strcmp(argv[ii], "-t") ==0) { k = 3;
	  }
	  if ( k >= 0 ) {
	    if ( (ii+1) >= argc) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " -(x|y|z|t) I \"", (char *) NULL);
		return TCL_ERROR;
	    }
	    ii++;
	    if (Tcl_GetInt( interp, argv[ii], &ll) != TCL_OK ) {
		return TCL_ERROR;
	    }
	    if ( ll > 1 ) dv[k] = ll;
	  }
	}
	status = 0;
	def_out = def_in;
	if ( dv[0] != 1 ) {
	    def_out.xdim = def_in.xdim/dv[0];
	    if (def_out.xdim == 0) {
		def_out.xdim = def_in.xdim; dv[0] = 1;
	    } else {
		def_out.x1 = 1 ; def_out.x2 = def_out.xdim;
	    }
	}
	if ( dv[1] != 1 ) {
	    def_out.ydim = def_in.ydim/dv[1];
	    if (def_out.ydim == 0) {
		def_out.ydim = def_in.ydim; dv[1] = 1;
	    } else {
		def_out.y1 = 1 ; def_out.y2 = def_out.ydim;
	    }
	}
	if ( dv[2] != 1 ) {
	    def_out.zdim = def_in.zdim/dv[2];
	    if (def_out.zdim == 0) {
		def_out.zdim = def_in.zdim; dv[2] = 1;
	    } else {
		def_out.z1 = 1 ; def_out.z2 = def_out.zdim;
	    }
	}
	if ( dv[3] != 1 ) {
	    def_out.tdim = def_in.tdim/dv[3];
	    if (def_out.tdim == 0) {
		def_out.tdim = def_in.tdim; dv[3] = 1;
	    } else {
		def_out.t1 = 1 ; def_out.t2 = def_out.tdim;
	    }
	}
	def_out.ndata = def_out.xdim*def_out.ydim*def_out.zdim*
			def_out.tdim*def_out.vdim;
	if (ic_imgCreate( &def_out, &ih2) != IC_OK ) {
	    return TCL_ERROR;
	}
	nn = img_IndexInit( def_out, def_out, &nd, &vec );
	for (n = 0 ; n < nd ; n++) {
	    def_out.data_p[nn] = 0.0; m = 0; vec1 = vec;
	    for ( i = 1; i <= dv[0]; i++ ) {
	      vec1.x = def_in.x1 + (vec.x-1)*dv[0] + i - 1;
	      for ( j = 1; j <= dv[1]; j++ ) {
	        vec1.y = def_in.y1 + (vec.y-1)*dv[1] + j - 1;
	        for ( k = 1; k <= dv[2]; k++ ) {
	          vec1.z = def_in.z1 + (vec.z-1)*dv[2] + k - 1;
	          for ( l = 1; l <= dv[3]; l++ ) {
	            vec1.t = def_in.t1 + (vec.t-1)*dv[3] + l - 1;
		    mm = img_Vec2Index( def_in, vec1);
		    def_out.data_p[nn] = def_out.data_p[nn] + 
					 def_in.data_p[mm]; m++;
	    } } } }
	    def_out.data_p[nn] = def_out.data_p[nn] / ( (float)m );
	    nn = img_IndexNext( def_out, def_out, &vec );
	}
	ic_resultImid( interp, ih2);

   } else if ((c == 'f') && (strncmp(argv[2], "flip", length) ==0)) {
	int l, axis;
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " imId flip axis \"", (char *) NULL);
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
	} else {
	   interp->result = "unrecognized axis";
	   return TCL_ERROR;
	}
	img_flip_(&def_in, def_in.data_p, &axis, &status);
	if (status != IC_OK) {
	   interp->result = "unable to flip image";
	   return TCL_ERROR;
	}
	ic_resultImid( interp, ih);

   } else if ((c == 't') && (strncmp(argv[2], "transpose", length) ==0)) {
	int l, ii, order[4];
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " imId transpose ?x y z t? \"", (char *) NULL);
	    return TCL_ERROR;
	}
	order[0] = 1 ; order[1] = 2 ; order[2] = 3; order[3] = 4;
        for (ii=3; ii < argc; ii++) {
	  l = strlen(argv[ii]);
	  if (strncmp(argv[ii], "x", l) ==0) {
	      order[ii-3] = 1;
	  } else if (strncmp(argv[ii], "y", l) ==0) {
	      order[ii-3] = 2;
	  } else if (strncmp(argv[ii], "z", l) ==0) {
	      order[ii-3] = 3;
	  } else if (strncmp(argv[ii], "t", l) ==0) {
	      order[ii-3] = 4;
	  }
	}
	l = 0;
        for (ii=0; ii < 4; ii++) { l = l + order[ii]; }
	if ( l != 10 ) {
	   interp->result = "illegal axis list";
	   return TCL_ERROR;
	}
	if (ic_imgCreate( &def_out, &ih) != IC_OK ) {
	    return TCL_ERROR;
	}
	img_transpose_(&def_in, def_in.data_p, order, 
                       &def_out, def_out.data_p, &status);
	if (status != IC_OK) {
	   interp->result = "unable to transpose image";
	   ic_imgDestroy( ih );
	   return TCL_ERROR;
	}
	ic_hash[ih].defn = def_out;
	ic_resultImid( interp, ih);

   } else if ((c == 's') && (strncmp(argv[2], "slice", length) ==0)) {
	int l, nd, ii, order[4];
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " imId slice ?-ndata n? \"", (char *) NULL);
	    return TCL_ERROR;
	}
	def2 = def_in;
	nd = 100;
        if (ic_parseSubIm( interp, 3, argc, argv, &def2) != IC_OK ) {
	    return TCL_ERROR;
	}
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
		    " slice imId ?-ndata n? \"", (char *) NULL);
	        return TCL_ERROR;
	     }
	  }
	}
	def_out.ndims = 1; def_out.ndata = 2*nd;
	def_out.norm = 1.0; def_out.blank = def_in.blank;
	def_out.dtype = def_in.dtype;
	def_out.xdim = nd; def_out.x1 = 1; def_out.x2 = nd;
	def_out.ydim = 1; def_out.y1 = 1; def_out.y2 = 1;
	def_out.zdim = 1; def_out.z1 = 1; def_out.z2 = 1;
	def_out.tdim = 1; def_out.t1 = 1; def_out.t2 = 1;
	def_out.vdim = 2; def_out.v1 = 1; def_out.v2 = 2;
        if (ic_imgCreate( &def_out, &ih) != IC_OK ) {
	    return TCL_ERROR;
	}
	img_getslice_( &def_in, def_in.data_p, &def2, &nd, def_out.data_p, &status);
	if (status != IC_OK) {
	   interp->result = "unable to get slice";
	   ic_imgDestroy( ih );
	   return TCL_ERROR;
	}
	ic_hash[ih].defn = def_out;
	ic_resultImid( interp, ih);

   } else if ((c == 't') && (strncmp(argv[2], "transform", length) ==0)) {
	int l, ii, n, mm, mdim, vdim;
	float matrix[16], vec[4], cvec[4];
	double d;
	vdim = 4; mdim = 16;
	for (n = 0; n < 16; n++) matrix[n] = 0.0;
	for (n = 0; n < 16; n += 5) matrix[n] = 1.0;
	for (n = 0; n < 4; n++ ) vec[n] = 0.0;
	for (n = 0; n < 4; n++ ) cvec[n] = 0.0;
        for (ii=3; ii < argc; ii++) {
	  mm = -1;
	  l = strlen(argv[ii]);
	  if (strncmp(argv[ii], "-matrix", l) ==0) {
	     ii++ ;
	     if (ii < argc) {
		if (Tcl_SplitList(interp, argv[ii],
			          &listc, &listv) != TCL_OK) {
		    return TCL_ERROR;
		}
	     } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " -matrix {elements} \"", (char *) NULL);
		return TCL_ERROR;
	     }
	     if ( (listc == 16) || (listc == 9) || (listc == 4) ) {
		mdim = listc;
	     } else {
		Tcl_AppendResult(interp, "wrong dimension for matrix \"",
		    " should have 16, 9 or 4 elements \"", (char *) NULL);
		return TCL_ERROR;
	     }
	     for (n = 0; n < listc; n++) {
		if (Tcl_GetDouble(interp, listv[n], &d) != TCL_OK) {
		      return TCL_ERROR;
		}
		matrix[n] = d;
	     }
	     free( (char *) listv );
	  } else if (strncmp(argv[ii], "-vector", l) ==0) {
	     ii++ ;
	     if (ii < argc) {
		if (Tcl_SplitList(interp, argv[ii],
			          &listc, &listv) != TCL_OK) {
		    return TCL_ERROR;
		}
	     } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " -vector {elements} \"", (char *) NULL);
		return TCL_ERROR;
	     }
	     if (listc > vdim) listc = vdim;
	     for (n = 0; n < listc; n++) {
		if (Tcl_GetDouble(interp, listv[n], &d) != TCL_OK) {
		      return TCL_ERROR;
		}
		vec[n] = d;
	     }
	     free( (char *) listv );
	  } else if (strncmp(argv[ii], "-cvector", l) ==0) {
	     ii++ ;
	     if (ii < argc) {
		if (Tcl_SplitList(interp, argv[ii],
			          &listc, &listv) != TCL_OK) {
		    return TCL_ERROR;
		}
	     } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " -vector {elements} \"", (char *) NULL);
		return TCL_ERROR;
	     }
	     if (listc > vdim) listc = vdim;
	     for (n = 0; n < listc; n++) {
		if (Tcl_GetDouble(interp, listv[n], &d) != TCL_OK) {
		      return TCL_ERROR;
		}
		cvec[n] = d;
	     }
	     free( (char *) listv );
	  } else if (strncmp(argv[ii], "-m11", l) == 0) { mm = 0;
	  } else if (strncmp(argv[ii], "-m21", l) == 0) { mm = 1;
	  } else if (strncmp(argv[ii], "-m31", l) == 0) { mm = 2;
	  } else if (strncmp(argv[ii], "-m41", l) == 0) { mm = 3;
	  } else if (strncmp(argv[ii], "-m12", l) == 0) { mm = 4;
	  } else if (strncmp(argv[ii], "-m22", l) == 0) { mm = 5;
	  } else if (strncmp(argv[ii], "-m32", l) == 0) { mm = 6;
	  } else if (strncmp(argv[ii], "-m42", l) == 0) { mm = 7;
	  } else if (strncmp(argv[ii], "-m13", l) == 0) { mm = 8;
	  } else if (strncmp(argv[ii], "-m23", l) == 0) { mm = 9;
	  } else if (strncmp(argv[ii], "-m33", l) == 0) { mm = 10;
	  } else if (strncmp(argv[ii], "-m43", l) == 0) { mm = 11;
	  } else if (strncmp(argv[ii], "-m14", l) == 0) { mm = 12;
	  } else if (strncmp(argv[ii], "-m24", l) == 0) { mm = 13;
	  } else if (strncmp(argv[ii], "-m34", l) == 0) { mm = 14;
	  } else if (strncmp(argv[ii], "-m44", l) == 0) { mm = 15;
	  }
	  if ( mm > 0 )  {
	     ii++ ;
	     if (ii < argc) {
		if (Tcl_GetDouble(interp, argv[ii], &d) != TCL_OK) {
		      return TCL_ERROR;
		}
		matrix[mm] = d;
	     }
	  }	
	}
	if (ic_imgCreate( &def_out, &ih) != IC_OK ) {
	    return TCL_ERROR;
	}
	status = 0;
	img_transform_(&def_in, def_in.data_p,
		       &vdim, vec, cvec, &mdim, matrix, 
                       &def_out, def_out.data_p, &status);
	if (status != IC_OK) {
	   interp->result = "unable to transform image";
	   ic_imgDestroy( ih );
	   return TCL_ERROR;
	}
	ic_hash[ih].defn = def_out;
	ic_resultImid( interp, ih);

   } else {
	Tcl_AppendResult(interp, "bad option \"", argv[2],
		"\": should be subimage, bin, project, flip,",
		"bshift, transpose, transform, slice\"",
		(char *) NULL);
	return TCL_ERROR;
   }
   return TCL_OK;
}
