/*
 *  Routines to implement an image library toolset
 *
 */
#include "tk.h"
#include "string.h"
#include "ic.h"

/*
 * Utility routines used in tools
 */
typedef struct vectList {
	img_vect v;
	struct vectList *vp;
} vectList;

vectList *allocVectList( v, vp )
   img_vect v;
   vectList *vp;
{
   vectList *newVect = (vectList *) malloc( sizeof(vectList) );
   newVect->v = v;
   newVect->vp = vp;
   return(newVect);
}

/*
 *  Update an image to specify percolating regions:
 *
 *     The supplied image should have already been segmented so that the
 *     pixel values to search for percolation are in the range:
 *            0.5 < pixel < 1.5
 *     Usually this will be fulfilled by setting the image to (approximate)
 *     integer values and a percolating search is applied to pixel integer
 *     values.  The procedure updates to supplied image and sets the
 *     percolating region to pval --- again this will usually be an integer
 *     value greater than unity.
 */
int img_percolate( defn, def1, sv1, sv2, pval, nloop, ndata )
   img_defn defn, def1;
   float    sv1, sv2, pval;
   int      nloop, *ndata;
{
   img_vect  vec, vec1;
   vectList *vList = NULL;
   vectList *uList, *p1, *p2;
   int      n, nd, nn, nl, i, j, dvec[3];

  /* start the percolation from the region specified by def1 */
   nn = img_IndexInit( defn, def1, &nd, &vec );
   for (n = 0; n < nd; n++) {
     if (nn >= 0) {
	if ( (defn.data_p[nn] > sv1) && (defn.data_p[nn] < sv2) ) {
	   defn.data_p[nn] = pval;
	   vList = allocVectList(vec, vList);
	}
	nn = img_IndexNext( defn, def1, &vec );
     }
   }

  /* iterate while the list is not empty */
   nl = 1;
   while (vList != NULL) {
	uList = NULL;
	for ( p1 = vList; p1 != NULL; p1 = p2 ) {
	    vec = p1->v ; p2 = p1->vp; free(p1);
	    for (j=0; j<3; j++) {
		for (i=-1; i<=1; i += 2) {
		   dvec[0] = 0; dvec[1] = 0; dvec[2] = 0;
		   vec1 = vec; dvec[j] = i;
		   vec1.x = vec.x+dvec[0];
		   vec1.y = vec.y+dvec[1];
		   vec1.z = vec.z+dvec[2];
		   nn = img_Vec2Index( defn, vec1 );
		   if (nn >= 0) {
		      if ((defn.data_p[nn]>sv1) && (defn.data_p[nn]<sv2)) {
			 defn.data_p[nn] = pval; nd++;
			 if (nl != nloop) 
			     uList = allocVectList(vec1, uList);
		      }
		   }
		}
	    }
	}
	nl++;
	vList = uList;
   }
   *ndata = nd;
   return IC_OK;
}
/*
 *   Routine to provide the percolate command
 *
 */
int imgPercolate (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   char     value[128];
   int      i, l, nloop, ndata; 
   double   d;
   float    pval, sv1, sv2;
   int      ih;
   img_defn def_in, def_out;

  /*
   * Check for compulsary imId argument
   */ 
   if (argc < 2) {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
		      "imId ?subimage? ?-value X? \"", (char *) NULL);
     return TCL_ERROR;
   }
   if (Tcl_GetInt(interp, argv[1], &ih) != TCL_OK) {
	return TCL_ERROR;
   }
   if (ic_hashExists( ih ) != IC_OK) {
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
   pval = 2.0; sv1 = 0.5; sv2 = 1.5; nloop = -1;
   for ( i = 2 ; i < argc ; i++) {
	l = strlen(argv[i]);
	if (strncmp(argv[i], "-value", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetDouble(interp, argv[i], &d);
		pval =d;
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-value X \"", (char *) NULL);
		return TCL_ERROR;
	   }
	} else if (strncmp(argv[i], "-sv1", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetDouble(interp, argv[i], &d);
		sv1 =d;
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-sv1 X \"", (char *) NULL);
		return TCL_ERROR;
	   }
	} else if (strncmp(argv[i], "-sv2", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetDouble(interp, argv[i], &d);
		sv2 =d;
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-sv2 X \"", (char *) NULL);
		return TCL_ERROR;
	   }
	} else if (strncmp(argv[i], "-nloop", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetInt(interp, argv[i], &nloop);
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-nloop I \"", (char *) NULL);
		return TCL_ERROR;
	   }
	}
   }
   if (img_percolate(def_in, def_out, sv1, sv2, pval, nloop, &ndata) != IC_OK) {
     Tcl_AppendResult(interp, "Error forming percolated image", (char *) NULL);
     return TCL_ERROR;
   }
   sprintf( value, "%d", ndata);
   Tcl_AppendResult(interp, value, (char *) NULL);
   return TCL_OK;
}

/*
 *  Update an image by providing a number of levels of morphological
 *  thinning --- regions which have been thinned are identified by labbelling
 *  them with an index indicating at which stage they were thinned.
 *
 *     The supplied image should have already been segmented so that the
 *     pixel values to search for thinning are in the range:
 *            0.5 < pixel < 1.5
 *     Usually this will be fulfilled by setting the image to (approximate)
 *     unity values and thinning is applied to pixel unity values
 *     values.  A pixel is thinned if it currently has a number of neighbours
 *     which are not part of the structure (0.0 valued) or have been
 *     previously thinned.
 */
int img_morphThin( defn, def1, nsMin, nsMax, doIdent, doReport )
   img_defn defn, def1;
   int      nsMin, nsMax, doIdent, doReport;
{

   img_defn def2;
   int      doThin, nThin, levThin, nsThin;
   img_vect vec, vec1;
   int      n, nd, ndp, nn, ns, m, i, j, nt, dvec[3];
   float    x1, x2, x3;
   int      nreg, inReg, levReg;

  /* determine number of pixels to thin */
   nThin = 0;
   nn = img_IndexInit( defn, def1, &nd, &vec );
   for (n = 0; n < nd; n++) {
     if (nn >= 0) {
	if ( (defn.data_p[nn] > 0.5) && (defn.data_p[nn] < 1.5) ) {
	    nThin++;
	}
	nn = img_IndexNext( defn, def1, &vec );
     }
   }
   inReg = nThin;

  /* setup the main thinning loop */
   doThin =1; levThin = 2; nsThin = nsMax;
   while (doThin) {

   /* loop over all elements to thin */
	nt = 0;
	if ( doReport )
	  printf( "Thinning: level = %d   sides = %d   # to thin = %d \n",
	levThin, nsThin, nThin);
	nn = img_IndexInit( defn, def1, &nd, &vec );
	x1 = (float)levThin - 0.5 ; x2 = (float)levThin + 0.5;
	for (n = 0; n < nd; n++) {
	   if (nn >= 0) {
	      if ( (defn.data_p[nn] > 0.5) && (defn.data_p[nn] < 1.5) ) {
		ns = 0;
		for (j=0; j<3; j++) {
		   for (i=-1; i<=1; i += 2) {
		      dvec[0] = 0; dvec[1] = 0; dvec[2] = 0;
		      vec1 = vec; dvec[j] = i;
		      vec1.x = vec.x+dvec[0];
		      vec1.y = vec.y+dvec[1];
		      vec1.z = vec.z+dvec[2];
		      m = img_Vec2Index( defn, vec1 );
		      if ( m >= 0 ) {
			if ( (defn.data_p[m] != defn.blank) &&
			     ( (defn.data_p[m] < 0.5) ||
			       (defn.data_p[m] > 1.5) ) &&
			     ( (defn.data_p[m] < x1) ||
			       (defn.data_p[m] > x2) ) ) { ns++; }
		      }
		   }
		}
		if (ns >= nsThin) {
		   defn.data_p[nn] = (float)levThin;
		   nt += 1; nThin -= 1;
		}
	      }
	      nn = img_IndexNext( defn, def1, &vec );
	   }
	}
   /* check for continuations */
	if ( nThin == 0 ) { 
	   doThin = 0;
	} else {
	   levThin++;
	}
	if ( nt == 0 ) {
	   nsThin -= 1;
	   if (nsThin < nsMin)
		doThin = 0;
	} else {
	   nsThin = nsMax;
	}
	if ( ( nt == 0 ) && ( nsThin == 0 ) && (nThin !=0) ) {
	    printf( "Error --- nt = 0 and nsThin = 0\n" );
	    return IC_ERROR;
	}
   }
   if ( doIdent == 0 )
	return IC_OK;

  /* perform region identification on this thinned image */
   nreg = levThin; levReg = levThin;
   x1 = (float)levReg - 0.5; x2 = (float)levReg + 0.5;
   x3 = (float)levThin + 0.5;

  /* find seed regions */
   nn = img_IndexInit( defn, def1, &nd, &vec );
   for (n = 0; n < nd; n++) {
     if (nn >= 0) {
	if ( (defn.data_p[nn] > x1) && (defn.data_p[nn] < x2) ) {
	   nreg++;
	   def2 = def1;
	   def2.x1 = vec.x ; def2.x2 = vec.x ;
	   def2.y1 = vec.y ; def2.y2 = vec.y ;
	   def2.z1 = vec.z ; def2.z2 = vec.z ;
	   img_percolate( defn, def2, x1, x2, (float)nreg, -1, &ndp );
	   nt += ndp; inReg -= ndp;
	}
	nn = img_IndexNext( defn, def1, &vec );
     }
   }
   levReg -= 1;

  /* grow and re-seed regions */
   while ( inReg > 0 ) {
	nt = 0;
	x1 = (float)levReg - 0.5; x2 = (float)levReg + 0.5;
	if ( doReport )
	  printf( "Identifying: level %d   regions = %d   # to Id = %d \n",
		   levReg, nreg, inReg);
	/* grow onto existing regions */
	nn = img_IndexInit( defn, def1, &nd, &vec );
	for (n = 0; n < nd; n++) {
	  if (nn >= 0) {
	     if (defn.data_p[nn] > x3) {
		for (j=0; j<3; j++) {
		   for (i=-1; i<=1; i += 2) {
		      dvec[0] = 0; dvec[1] = 0; dvec[2] = 0;
		      vec1 = vec; dvec[j] = i;
		      vec1.x = vec.x+dvec[0];
		      vec1.y = vec.y+dvec[1];
		      vec1.z = vec.z+dvec[2];
		      m = img_Vec2Index( defn, vec1 );
		      if ( m >= 0 ) {
			if ( (defn.data_p[m]>x1) && (defn.data_p[m]<x2) ) {
			   nt++ ; defn.data_p[m] = -defn.data_p[nn];
			   inReg -= 1;
			}
		      }
		   }
		}
	     }
	     nn = img_IndexNext( defn, def1, &vec );
	   }
	}

	/* reset all grown regions */
	nn = img_IndexInit( defn, defn, &nd, &vec );
	for (n = 0; n < nd; n++) {
	  if (nn >= 0) {
	     if ( (defn.data_p[nn]<0.0) && (defn.data_p[nn]!=defn.blank) ) {
		defn.data_p[nn] = - defn.data_p[nn];
	     }
	  }
	  nn = img_IndexNext( defn, defn, &vec );
	}

	/* find new regions */
	if (nt == 0) {
	   nn = img_IndexInit( defn, def1, &nd, &vec );
	   for (n = 0; n < nd; n++) {
	     if (nn >= 0) {
		if ( (defn.data_p[nn] > x1) && (defn.data_p[nn] < x2) ) {
		   nreg++;
		   def2 = def1;
		   def2.x1 = vec.x ; def2.x2 = vec.x ;
		   def2.y1 = vec.y ; def2.y2 = vec.y ;
		   def2.z1 = vec.z ; def2.z2 = vec.z ;
		   img_percolate( defn, def2, x1, x2, (float)nreg, -1, &ndp );
		   nt += ndp; inReg -= ndp;
		}
		nn = img_IndexNext( defn, def1, &vec );
	     }
	   }
	}

	/* move onto next level if this one is exhausted */
	if (nt == 0) {
	   levReg -= 1;
	}
   }

  /* normalise range of ID's */
   nn = img_IndexInit( defn, def1, &nd, &vec );
   for (n = 0; n < nd; n++) {
     if (nn >= 0) {
	if (defn.data_p[nn] > x3) {
	   defn.data_p[nn] = defn.data_p[nn] - (float)levThin;
	}
	nn = img_IndexNext( defn, def1, &vec );
     }
   }
   return IC_OK;

}
/*
 *   Routine to provide the morphological thinning command
 *
 */
int imgMorphThin (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      i, l, nsMin, nsMax, doReport, doIdent; 
   double   d;
   int      ih;
   img_defn def_in, def_out;

  /*
   * Check for compulsary imId argument
   */ 
   if (argc < 2) {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
		      "imId ?subimage? ?-nsmin I? ?-nsmax I?\"", (char *) NULL);
     return TCL_ERROR;
   }
   if (Tcl_GetInt(interp, argv[1], &ih) != TCL_OK) {
	return TCL_ERROR;
   }
   if (ic_hashExists( ih ) != IC_OK) {
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
   nsMin = 1; doReport = 0; doIdent = 0;
   nsMax = 5; if (def_in.ndims == 2) {nsMax = 3;}
   for ( i = 2 ; i < argc ; i++) {
	l = strlen(argv[i]);
	if (strncmp(argv[i], "-nsmin", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetInt(interp, argv[i], &nsMin);
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-nsMin I \"", (char *) NULL);
		return TCL_ERROR;
	   }
	} else if (strncmp(argv[i], "-nsmax", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetInt(interp, argv[i], &nsMax);
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-nsMax I \"", (char *) NULL);
		return TCL_ERROR;
	   }
	} else if (strncmp(argv[i], "-report", l) == 0) {
	   doReport = 1;
	} else if (strncmp(argv[i], "-identify", l) == 0) {
	   doIdent = 1;
	} else if (strncmp(argv[i], "-noreport", l) == 0) {
	   doReport = 0;
	} else if (strncmp(argv[i], "-noidentify", l) == 0) {
	   doIdent = 0;
	}
   }
   if (img_morphThin( def_in, def_out, nsMin, nsMax, doIdent, doReport ) != IC_OK) {
     Tcl_AppendResult(interp, "Error forming thinned image", (char *) NULL);
     return TCL_ERROR;
   }
   return TCL_OK;
}

/*
 *  Update an image by identifying discrete regions within the image 
 *  and applying a label to all such regions.
 *
 *     The supplied image is segmented into regions according to:
 *            x1 <= pixel <= x2
 *     Regions identified in the image will be labelled by setting them equal
 *     to values which are determined as follows (pseudocode)
 *
 *     foreach region { region_index = index + (region - 1)*incr }
 *
 */
int img_regIdent( defn, def1, x1, x2, indx, incr )
   img_defn defn, def1;
   float    x1, x2;
   float    indx, incr;
{
   img_defn def2;
   img_vect vec;
   int      n, nd, nn, ndp;

   nn = img_IndexInit( defn, def1, &nd, &vec );
   for (n = 0; n < nd; n++) {
     if (nn >= 0) {
	if ( (defn.data_p[nn] >= x1) && (defn.data_p[nn] <= x2) ) {
	   def2 = def1;
	   def2.x1 = vec.x ; def2.x2 = vec.x ;
	   def2.y1 = vec.y ; def2.y2 = vec.y ;
	   def2.z1 = vec.z ; def2.z2 = vec.z ;
	   img_percolate( defn, def2, x1, x2, indx, -1, &ndp );
	   indx += incr;
	}
	nn = img_IndexNext( defn, def1, &vec );
     }
   }
   return IC_OK;

}
/*
 *   Routine to provide the indentify command
 *
 */
int imgRegIdent (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      i, l;
   float    x1, x2, indx, incr;
   double   d;
   int      ih;
   img_defn def_in, def_out;

  /*
   * Check for compulsary imId argument
   */ 
   if (argc < 4) {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
		      "imId min max ?subimage? ?-index X ?-incr X?\"", (char *) NULL);
     return TCL_ERROR;
   }
   if (Tcl_GetInt(interp, argv[1], &ih) != TCL_OK) {
	return TCL_ERROR;
   }
   if (ic_hashExists( ih ) != IC_OK) {
	return TCL_ERROR;
   }
   def_in = ic_hash[ih].defn;
   def_out = ic_hash[ih].defn;
   if (Tcl_GetDouble(interp, argv[2], &d) != TCL_OK) {
	return TCL_ERROR;
   }
   x1 = d;
   if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	return TCL_ERROR;
   }
   x2 = d;

  /*
   * Parse for subimage to operate on
   */
   if (ic_parseSubIm( interp, 4, argc, argv, &def_out) != IC_OK ) {
	return TCL_ERROR;
   }

  /*
   * loop though all arguments and take action depending on supplied option
   */
   indx = -1.0 ; incr = -1.0;
   for ( i = 4 ; i < argc ; i++) {
	l = strlen(argv[i]);
	if (strncmp(argv[i], "-index", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetDouble(interp, argv[i], &d);
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-nsMin I \"", (char *) NULL);
		return TCL_ERROR;
	   }
	   indx = d;
	} else if (strncmp(argv[i], "-incr", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetDouble(interp, argv[i], &d);
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-nsMax I \"", (char *) NULL);
		return TCL_ERROR;
	   }
	   incr = d;
	}
   }
   if (img_regIdent( def_in, def_out, x1, x2, indx, incr ) != IC_OK) {
     Tcl_AppendResult(interp, "Error identifying image", (char *) NULL);
     return TCL_ERROR;
   }
   return TCL_OK;
}
