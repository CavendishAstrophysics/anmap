/*
 *  Routines to implement image basic operations
 *
 */

#include "string.h"
#include "eqn.h"
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
 * locaL structure to manage pixel options
 */
typedef struct img_pixOpt {
	int test;
	int testAll;
} img_pixOpt;

/*
 * Utility routines to parse input to the pixel command
 */
int img_parsePixel( interp, start, argc, argv, opt)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
    img_pixOpt *opt;			/* Pixel parsed options. */
{
  int i, j, l;
  double d;

  if (start >= argc)
	return TCL_OK;

  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strcmp(argv[i], "-testAll") == 0) {
	opt->testAll = 1;
    } else if (strcmp(argv[i], "-testAny") == 0) {
	opt->testAll = 0;
    }
  }
  return TCL_OK;
}


/*
 *   Routine to provide access to image functions
 *
 */
int imgPixel (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      status;
   int      length;
   register char c;
   char     **listv;
   int      listc;
   int      i, n, nn, mm, nv;
   double   d; 
   char     value[128];
   int      ih;
   img_defn def_in, def_out, def2;
   img_vect vec1, vec2, vec;
   img_vectF vecF;

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
   * sort out options for the Pixel command
   */
   c = argv[2][0];
   length = strlen(argv[1]);
   status = 0;

   if ((c == 'g') && (strncmp(argv[2], "get", length) ==0)) {
	nv = 99;
	nn = img_IndexInit( def_in, def_out, &mm, &vec1 );
	if ( mm < nv ) nv = mm;
	for (n = 0 ; n < nv ; n++) {
	    if ( nn >= 0 ) {
		sprintf( value, "%f", def_in.data_p[nn] );
		Tcl_AppendElement( interp, value );
	    }
	    nn = img_IndexNext( def_in, def_out, &vec1 );
	}
	return TCL_OK;

   } else if ((c == 's') && (strncmp(argv[2], "set", length) ==0)) {
	if (argc < 4) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " imId set val-list \"", (char *) NULL);
		return TCL_ERROR;
	}
	if (Tcl_SplitList(interp, argv[argc-1], &listc, &listv) != TCL_OK) {
		return TCL_ERROR;
	}
	nn = img_IndexInit( def_in, def_out, &mm, &vec1 );
	if ( mm < listc ) listc = mm;
	for (n = 0; n < listc; n++) {
		if (Tcl_GetDouble(interp, listv[n], &d) != TCL_OK) {
		      return TCL_ERROR;
		}
		if (nn >= 0) {
		      def_in.data_p[nn] = d;
		}
		nn = img_IndexNext( def_in, def_out, &vec1 );
	}
	return TCL_OK;

   } else if ((c == 'a') && (strncmp(argv[2], "annulus", length) ==0)) {
	float r1, r2, rr, val;
	vec.x = def_out.x1 ; vec.y = def_out.y1 ; vec.z = def_out.z1 ;
	vec.t = def_out.t1 ; vec.v = def_out.v1 ;
	if (ic_parseVect( interp, 3, argc, argv, &vec) != IC_OK ) {
	   return TCL_ERROR;
	}
	r1 = 0.0 ; r2 = 1000000.0; val = 0.0;
	for (n = 0; n < argc; n++) {
	   if (strcmp(argv[n],"-r1")==0) {
		n++;
		if ( n < argc ) {
		   if (Tcl_GetDouble(interp, listv[n], &d) != TCL_OK) {
		      return TCL_ERROR;
		   }
		}
		r1 = d;
	   } else if (strcmp(argv[n],"-r2")==0) {
		n++;
		if ( n < argc ) {
		   if (Tcl_GetDouble(interp, listv[n], &d) != TCL_OK) {
		      return TCL_ERROR;
		   }
		}
		r2 = d;
	   } else if (strcmp(argv[n],"-val")==0) {
		n++;
		if ( n < argc ) {
		   if (Tcl_GetDouble(interp, listv[n], &d) != TCL_OK) {
		      return TCL_ERROR;
		   }
		}
		val = d;
	   }
	}
	nn = img_IndexInit( def_in, def_out, &nv, &vec1 );
	r1 = r1*r1; r2 = r2*r2;
	for (n = 0; n < nv; n++) {
		if (nn >= 0) {
		   rr = sqrt( (double)( (vec1.x-vec.x)*(vec1.x-vec.x) +
				(vec1.y-vec.y)*(vec1.y-vec.y) +
				(vec1.z-vec.z)*(vec1.z-vec.z) +
				(vec1.t-vec.t)*(vec1.t-vec.t) ) );
		   if ( (rr >= r1) && (rr <= r2) ) {
		      def_in.data_p[nn] = val;
		   }
		}
		nn = img_IndexNext( def_in, def_out, &vec1 );
	}
	return TCL_OK;

   } else if ((c == 'c') && (strncmp(argv[2], "copy", length) ==0)) {
	vec.x = def_out.x1 ; vec.y = def_out.y1 ; vec.z = def_out.z1 ;
	vec.t = def_out.t1 ; vec.v = def_out.v1 ;
	if (ic_parseVect( interp, 3, argc, argv, &vec) != IC_OK ) {
	   return TCL_ERROR;
	}
	def2.x1 = vec.x; def2.x2 = vec.x + def_out.xdim - 1;
	def2.y1 = vec.y; def2.y2 = vec.y + def_out.ydim - 1;
	def2.z1 = vec.z; def2.z2 = vec.z + def_out.zdim - 1;
	def2.t1 = vec.t; def2.t2 = vec.t + def_out.tdim - 1;
	def2.v1 = vec.v; def2.v2 = vec.v + def_out.vdim - 1;
	nn = img_IndexInit( def_in, def_out, &nv, &vec1 );
	mm = img_IndexInit( def_in, def2, &nv, &vec2 );
	for (n = 0; n < nv; n++) {
		if ( (nn >= 0) && (mm >= 0) ) {
		      def_in.data_p[mm] = def_in.data_p[nn];
		}
		nn = img_IndexNext( def_in, def_out, &vec1 );
		mm = img_IndexNext( def_in, def2, &vec2 );
	}
	return TCL_OK;

   } else if ((c == 'w') && (strncmp(argv[2], "within", length) ==0)) {
	float x1, x2;
	if (argc < 5) {
	   Tcl_AppendResult(interp, "wrong # args: should be \"",
		"within Xmin Xmax ?subimage? \"", (char *) NULL);
	   return TCL_ERROR;
	}
	if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	      return TCL_ERROR;
	}
	x1 = d;
	if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	      return TCL_ERROR;
	}
	x2 = d;
	nn = img_IndexInit( def_in, def_out, &nv, &vec );
	vec1.x = def_in.x2; vec2.x = def_in.x1; 
	vec1.y = def_in.y2; vec2.y = def_in.y1; 
	vec1.z = def_in.z2; vec2.z = def_in.z1; 
	vec1.t = def_in.t2; vec2.t = def_in.t1; 
	for (n = 0; n < nv; n++) {
		if (nn >= 0) {
		    if (  (def_in.data_p[nn]>=x1) &&
			  (def_in.data_p[nn]<=x2)  ) {
			if (vec.x < vec1.x) vec1.x = vec.x;
			if (vec.x > vec2.x) vec2.x = vec.x;
			if (vec.y < vec1.y) vec1.y = vec.y;
			if (vec.y > vec2.y) vec2.y = vec.y;
			if (vec.z < vec1.z) vec1.z = vec.z;
			if (vec.z > vec2.z) vec2.z = vec.z;
			if (vec.t < vec1.t) vec1.t = vec.t;
			if (vec.t > vec2.t) vec2.t = vec.t;
		    }
		}
		nn = img_IndexNext( def_in, def_out, &vec );
	}
	sprintf( value, "%d", vec1.x );
	Tcl_AppendElement( interp, value );
	sprintf( value, "%d", vec1.y );
	Tcl_AppendElement( interp, value );
	sprintf( value, "%d", vec1.z );
	Tcl_AppendElement( interp, value );
	sprintf( value, "%d", vec1.t );
	Tcl_AppendElement( interp, value );
	sprintf( value, "%d", vec2.x );
	Tcl_AppendElement( interp, value );
	sprintf( value, "%d", vec2.y );
	Tcl_AppendElement( interp, value );
	sprintf( value, "%d", vec2.z );
	Tcl_AppendElement( interp, value );
	sprintf( value, "%d", vec2.t );
	Tcl_AppendElement( interp, value );
	return TCL_OK;

   } else if ((c == 'v') && (strncmp(argv[2], "value", length) ==0)) {
	float val;
	vecF.x = def_out.x1 ; vecF.y = def_out.y1 ; vecF.z = def_out.z1 ;
	vecF.t = def_out.t1 ; vecF.v = def_out.v1 ;
	if (ic_parseVectF( interp, 3, argc, argv, &vecF) != IC_OK ) {
	   return TCL_ERROR;
	}
	img_getvalue_( &def_in, def_in.data_p, vecF, &val);
	sprintf( value, "%f", val );
	Tcl_AppendElement( interp, value );
	return TCL_ERROR;

   } else if ( (strcmp(argv[2], "=") ==0)  || 
               (strcmp(argv[2], "+=") ==0) ||
               (strcmp(argv[2], "-=") ==0) ||
               (strcmp(argv[2], "*=") ==0) ||
               (strcmp(argv[2], "==") ==0) ||
               (strcmp(argv[2], "!=") ==0) ||
               (strcmp(argv[2], ">") ==0)  ||
               (strcmp(argv[2], ">=") ==0) ||
               (strcmp(argv[2], "<") ==0)  ||
               (strcmp(argv[2], "<=") ==0) ||
               (strcmp(argv[2], "/=") ==0) ) {
        int      op, test, doTest;
	double   d;
	eqnode   *eqn, *deqn, *subeqn;
	img_pixOpt opt;

	if (argc < 4) {
	   Tcl_AppendResult(interp, "wrong # args: should be \"",
		    " imId = eqn \"", (char *) NULL);
	   return TCL_ERROR;
	}

       /* parse options (if any) */
	opt.test = 0; opt.testAll = 1;
	if (img_parsePixel(interp, 4, argc, argv, &opt) != TCL_OK) {
	   Tcl_AppendResult(interp, "error parsing options", (char *) NULL);
	   return TCL_ERROR;
	}

       /* parse operator */
        if (strcmp(argv[2], "=") ==0) { op = 1;
        } else if (strcmp(argv[2], "+=") ==0) { op = 2;
        } else if (strcmp(argv[2], "-=") ==0) { op = 3;
        } else if (strcmp(argv[2], "*=") ==0) { op = 4;
        } else if (strcmp(argv[2], "/=") ==0) { op = 5;
        } else if (strcmp(argv[2], "==") ==0) { op = 6; opt.test = 1;
        } else if (strcmp(argv[2], "!=") ==0) { op = 7; opt.test = 1;
        } else if (strcmp(argv[2], ">") ==0)  { op = 8; opt.test = 1;
        } else if (strcmp(argv[2], ">=") ==0) { op = 9; opt.test = 1;
        } else if (strcmp(argv[2], "<") ==0)  { op = 10; opt.test = 1;
        } else if (strcmp(argv[2], "<=") ==0) { op = 11; opt.test = 1;
        }

       /* scan command line for equation to use */
	eqn = sscan_eqn( argv[3] );
	eval_funs(eqn);

       /* evaluate the function list */
	nn = img_IndexInit( def_in, def_out, &nv, &vec1 );
	test = 1; doTest = 1; n = 0;
	while ( (n < nv) && (doTest == 1) ) {
	   if (nn >= 0) {
		deqn = duplicate(eqn);
		d = vec1.x; subeqn = assign( "x", d );
		substitute(deqn,subeqn); free_eqn_tree( subeqn );
		d = vec1.y; subeqn = assign( "y", d );
		substitute(deqn,subeqn); free_eqn_tree( subeqn );
		d = vec1.z; subeqn = assign( "z", d );
		substitute(deqn,subeqn); free_eqn_tree( subeqn );
		d = vec1.t; subeqn = assign( "t", d );
		substitute(deqn,subeqn); free_eqn_tree( subeqn );
		d = vec1.v; subeqn = assign( "v", d );
		substitute(deqn,subeqn); free_eqn_tree( subeqn );
		eval_funs(deqn); 
        	if ( op == 1) { def_in.data_p[nn] = eqnval( deqn );
		} else if ( op == 2 ) { def_in.data_p[nn] += eqnval( deqn );
		} else if ( op == 3 ) { def_in.data_p[nn] -= eqnval( deqn );
		} else if ( op == 4 ) { def_in.data_p[nn] *= eqnval( deqn );
		} else if ( op == 5 ) { def_in.data_p[nn] /= eqnval( deqn );
		} else if ( op == 6 ) { 
		   if ( def_in.data_p[nn] == eqnval(deqn) ) {
			test = 1; } else { test = 0; }
		} else if ( op == 7 ) { 
		   if ( def_in.data_p[nn] != eqnval(deqn) ) {
			test = 1; } else { test = 0; }
		} else if ( op == 8 ) { 
		   if ( def_in.data_p[nn] > eqnval(deqn) ) {
			test = 1; } else { test = 0; }
		} else if ( op == 9 ) { 
		   if ( def_in.data_p[nn] >= eqnval(deqn) ) {
			test = 1; } else { test = 0; }
		} else if ( op == 10 ) { 
		   if ( def_in.data_p[nn] < eqnval(deqn) ) {
			test = 1; } else { test = 0; }
		} else if ( op == 11 ) { 
		   if ( def_in.data_p[nn] <= eqnval(deqn) ) {
			test = 1; } else { test = 0; }
		}
		free_eqn_tree( deqn );
		nn = img_IndexNext( def_in, def_out, &vec1 );
	   }
	   n++;
	   if (opt.test == 1) {
		if (opt.testAll == 1) {
		    if (test == 0) doTest = 0;
		} else {
		    if (test == 1) doTest = 0;
		}
	   }
	}
	if (opt.test == 1) {
		sprintf( value, "%d", test );
		Tcl_AppendElement( interp, value );
	}
	free_eqn_tree( subeqn ) ;
	return TCL_OK;

   } else {
	Tcl_AppendResult(interp, "bad option \"", argv[2],
		"\": should be set, get, copy, value within or an operator\"",
		(char *) NULL);
	return TCL_ERROR;
   }
}
