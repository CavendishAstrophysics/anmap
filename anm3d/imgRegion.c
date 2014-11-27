/*
 *  Routines to implement an image library toolset
 *
 */
#include "tk.h"
#include "string.h"
#include "math.h"
#include "ic.h"

/*
 * Data used in the analysis of nearest neighbours
 *
 */
int dvecMax = 26;
int dvec[26][3]={{-1,0,0},{1,0,0},{0,-1,0},{0,1,0},{0,0,-1},{0,0,1},
		 {-1,-1,0},{-1,1,0},{1,1,0},{1,-1,0},
		 {-1,0,-1},{-1,0,1},{1,0,1},{1,0,-1},
		 {0,-1,-1},{0,-1,1},{0,1,1},{0,1,-1},
		 {-1,-1,-1},{-1,1,-1},{1,1,-1},{1,-1,-1},
		 {-1,-1,1},{-1,1,1},{1,1,1},{1,-1,1}};

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
   img_vectList *vList = NULL;
   img_vectList *uList, *p1, *p2;
   int      n, nd, nn, nl, i;

  /* start the percolation from the region specified by def1 */
   nn = img_IndexInit( defn, def1, &nd, &vec );
   for (n = 0; n < nd; n++) {
     if (nn >= 0) {
	if ( (defn.data_p[nn] > sv1) && (defn.data_p[nn] < sv2) ) {
	   defn.data_p[nn] = pval;
	   vList = img_allocVectList(vec, vList);
	}
	nn = img_IndexNext( defn, def1, &vec );
     }
   }

  /* iterate while the list is not empty */
   nl = 1;
   while (vList != NULL) {
	uList = NULL;
	for ( p1 = vList; p1 != NULL; p1 = p2 ) {
	    vec = p1->v ; p2 = p1->vp; free(p1); vec1 = vec;
	    for (i=0; i < dvecMax; i++) {
		vec1.x = vec.x+dvec[i][0];
		vec1.y = vec.y+dvec[i][1];
		vec1.z = vec.z+dvec[i][2];
		nn = img_Vec2Index( defn, vec1 );
		if (nn >= 0) {
		   if ((defn.data_p[nn]>sv1) && (defn.data_p[nn]<sv2)) {
			 defn.data_p[nn] = pval; nd++;
			 if (nl != nloop) 
			     uList = img_allocVectList(vec1, uList);
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
 *  Test for a percolating path in the specified image.  Data ranges
 *  arre used to indicate sets of voxels between which a percolating
 *  path is searched
 */
int img_percVal( defn, def1, iv1, iv2, fv1, fv2, sv1, sv2, pval, perc )
   img_defn defn, def1;
   float    iv1, iv2, fv1, fv2, sv1, sv2, pval;
   int      *perc;
{
   img_vect  vec, vec1;
   img_vectList *vList = NULL;
   img_vectList *uList, *p1, *p2;
   int      n, nd, nn, nl, i;

  /* start the percolation from the region specified by def1 using the
     initial search range on the data */
   nn = img_IndexInit( defn, def1, &nd, &vec );
   for (n = 0; n < nd; n++) {
     if (nn >= 0) {
	if ( (defn.data_p[nn] > iv1) && (defn.data_p[nn] < iv2) ) {
	   defn.data_p[nn] = pval;
	   vList = img_allocVectList(vec, vList);
	}
	nn = img_IndexNext( defn, def1, &vec );
     }
   }

  /* iterate while the list is not empty */
   nl = 1; *perc = 0;
   while ((vList != NULL) && (*perc == 0)) {
	uList = NULL;
	for ( p1 = vList; p1 != NULL; p1 = p2 ) {
	    vec = p1->v ; p2 = p1->vp; free(p1); vec1 = vec;
	    for (i=0; i < dvecMax; i++) {
		vec1.x = vec.x+dvec[i][0];
		vec1.y = vec.y+dvec[i][1];
		vec1.z = vec.z+dvec[i][2];
		nn = img_Vec2Index( defn, vec1 );
		if (nn >= 0) {
		   if ((defn.data_p[nn]>sv1) && (defn.data_p[nn]<sv2)) {
			defn.data_p[nn] = pval; nd++;
			uList = img_allocVectList(vec1, uList);
		   } else if ((defn.data_p[nn]>fv1) && (defn.data_p[nn]<fv2)) {
			*perc = 1;
		   }
		}
	    }
	}
	nl++;
	vList = uList;
   }
   return IC_OK;
}

int img_percList( defn, sv1, sv2, pval, np, nd )
   img_defn   defn;
   float      sv1, sv2, pval;
   ida_nList  **np;
   int        *nd;
{
   img_vect     vec, vec1;
   img_vectList *vList = NULL;
   img_vectList *uList, *p1, *p2;
   int          nn, i;

  /* start the percolation from the first element of **np */
   nn = (*np)->n ; img_Index2Vec( defn, nn, &vec );
   defn.data_p[nn] = pval; *nd = 1;
   vList = img_allocVectList( vec, vList);

  /* iterate while the list is not empty */
   while (vList != NULL) {
	uList = NULL;
	for ( p1 = vList; p1 != NULL; p1 = p2 ) {
	    vec = p1->v ; p2 = p1->vp; free(p1); vec1 = vec;
	    for (i=0; i < dvecMax; i++) {
		vec1.x = vec.x+dvec[i][0];
		vec1.y = vec.y+dvec[i][1];
		vec1.z = vec.z+dvec[i][2];
		nn = img_Vec2Index( defn, vec1 );
		if (nn >= 0) {
		      if ((defn.data_p[nn]>sv1) && (defn.data_p[nn]<sv2)) {
			 defn.data_p[nn] = pval; *nd += 1;
			 *np = ida_allocNList( nn, *np );
			 uList = img_allocVectList(vec1, uList);
		      }
		}
	    }
	}
	vList = uList;
   }
   return IC_OK;
}

/*
 *   Routine to provide the percolate command
 *
 */
int imgPercolate (interp, argc, argv)
   Tcl_Interp	*interp;
   int		argc;
   char		**argv;
{
   char     value[128];
   int      i, l, nloop, ndata, perc, doTest; 
   double   d;
   float    pval, iv1, iv2, fv1, fv2, sv1, sv2;
   int      ih;
   img_defn def_in, def_out;

  /*
   * Check for compulsary imId argument
   */ 
   if (argc < 3) {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
		      "percolate imId ?subimage? ?-value X? \"", (char *) NULL);
     return TCL_ERROR;
   }
   if (Tcl_GetInt(interp, argv[2], &ih) != TCL_OK) {
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
   if (ic_parseSubIm( interp, 3, argc, argv, &def_out) != IC_OK ) {
	return TCL_ERROR;
   }

  /*
   * loop though all arguments and take action depending on supplied option
   */
   pval = 2.0; sv1 = 0.5; sv2 = 1.5; nloop = -1; dvecMax = 6;
   iv1 = 2.5; iv2 = 3.5; fv1 = 3.5; fv2 = 4.5; doTest = 0;
   for ( i = 3 ; i < argc ; i++) {
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
	} else if (strncmp(argv[i], "-iv1", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetDouble(interp, argv[i], &d);
		iv1 =d;
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-iv1 X \"", (char *) NULL);
		return TCL_ERROR;
	   }
	} else if (strncmp(argv[i], "-fv1", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetDouble(interp, argv[i], &d);
		fv1 =d;
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-fv1 X \"", (char *) NULL);
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
	} else if (strncmp(argv[i], "-iv2", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetDouble(interp, argv[i], &d);
		iv2 =d;
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-iv2 X \"", (char *) NULL);
		return TCL_ERROR;
	   }
	} else if (strncmp(argv[i], "-fv2", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetDouble(interp, argv[i], &d);
		fv2 =d;
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-fv2 X \"", (char *) NULL);
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
	} else if (strncmp(argv[i], "-near1", l) == 0) {
	   dvecMax = 6;
	} else if (strncmp(argv[i], "-near2", l) == 0) {
	   dvecMax = 18;
	} else if (strncmp(argv[i], "-near3", l) == 0) {
	   dvecMax = 26;
	} else if (strncmp(argv[i], "-test", l) == 0) {
	   doTest = 1;
	}
   }
   if (doTest == 0) {
     if (img_percolate(def_in,def_out,sv1,sv2,pval,nloop,&ndata) != IC_OK) {
       Tcl_AppendResult(interp, "Error forming percolated image",(char *) NULL);
       return TCL_ERROR;
     }
     sprintf( value, "%d", ndata);
   } else {
     if (img_percVal(def_in,def_out,iv1,iv2,fv1,fv2,
			sv1,sv2,pval,&perc) != IC_OK) {
       Tcl_AppendResult(interp, "Error in percolation", (char *) NULL);
       return TCL_ERROR;
     }
     sprintf( value, "%d", perc);
   }
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
int thin_testPerc( pe )
   int pe[3][3][3];
{
   int i, j, k, ii, jj, kk, i1, i2, j1, j2, k1, k2, ns, nt, nf;

   nt = 0;
   for (i=0; i<=2; i++) {
     for (j=0; j<=2; j++) {
       for (k=0; k<=2; k++) {
	 if ( pe[i][j][k]==1 ) {
	    nt++ ; if (nt==1) pe[i][j][k]=2;
         }
       }
     }
   }
   nt--;
   nf=1;
   while (nf>0 && nt>0) {
     nf = 0;
     for (i=0; i<=2; i++) {
       for (j=0; j<=2; j++) {
         for (k=0; k<=2; k++) {
	    if ( pe[i][j][k]==2 ) {
		i1 = (i-1)>=0?(i-1):0; i2 = (i+1)<=2?(i+1):2;
		j1 = (j-1)>=0?(j-1):0; j2 = (j+1)<=2?(j+1):2;
		k1 = (k-1)>=0?(k-1):0; k2 = (k+1)<=2?(k+1):2;
		for (ii=i1; ii<=i2; ii++) {
		  for (jj=j1; jj<=j2; jj++) {
		    for (kk=k1; kk<=k2; kk++) {
		      if ( pe[ii][jj][kk]==1 ) {
			   nf++; nt--; pe[ii][jj][kk]=2;
		      }
		    }
		  }
		}
	    }
         }
       }
     }
   }
   if (nt>0) {
	ns=0;
   } else {
	ns=1;
   }
   return ns;
}

int thin_testErode( pe )
   int pe[3][3][3];
{
   int p1[3], p2[3];
   int i, j, k, ii, jj, kk, i1, i2, j1, j2, k1, k2, ns, nt, nf;

   /* remove corners */
   for (i=0; i<=2; i+=2) {
     for (j=0; j<=2; j+=2) {
       for (k=0; k<=2; k+=2) {
	 pe[i][j][k]=0;
       }
     }
   }
   /* slim down the volume when a face-centre is present */
   for (i=0; i<=2; i+=2) {
	if ( pe[i][1][1] == 1 ) {
	   pe[i][0][1] = 0; pe[i][2][1] = 0;
	   pe[i][1][0] = 0; pe[i][1][2] = 0;
	}
   }
   for (i=0; i<=2; i+=2) {
	if ( pe[1][i][1] == 1 ) {
	   pe[0][i][1] = 0; pe[2][i][1] = 0;
	   pe[1][i][0] = 0; pe[1][i][2] = 0;
	}
   }
   for (i=0; i<=2; i+=2) {
	if ( pe[1][1][i] == 1 ) {
	   pe[0][1][i] = 0; pe[2][1][i] = 0;
	   pe[1][0][i] = 0; pe[1][2][i] = 0;
	}
   }

   /* perform erosion */
   nt = 0;
   for (i=0; i<=2; i++) {
     for (j=0; j<=2; j++) {
       for (k=0; k<=2; k++) {
	 if ( pe[i][j][k]==1 ) {
	    nt++;
         }
       }
     }
   }
   nf=1;
   while (nf>0 && nt>3) {
     nf = 0;
     for (i=0; i<=2; i++) {
       for (j=0; j<=2; j++) {
         for (k=0; k<=2; k++) {
	    if ( pe[i][j][k]==1 ) {
		ns = 0;
		i1 = (i-1)>=0?(i-1):0; i2 = (i+1)<=2?(i+1):2;
		j1 = (j-1)>=0?(j-1):0; j2 = (j+1)<=2?(j+1):2;
		k1 = (k-1)>=0?(k-1):0; k2 = (k+1)<=2?(k+1):2;
		for (ii=i1; ii<=i2; ii++) {
		  for (jj=j1; jj<=j2; jj++) {
		    for (kk=k1; kk<=k2; kk++) {
		      if ( pe[ii][jj][kk]==1 ) {
			   ns++;
			   if (ns==1) {p1[1]=ii;p1[2]=jj;p1[3]=kk;}
			   if (ns==2) {p2[1]=ii;p2[2]=jj;p2[3]=kk;}
		      }
		    }
		  }
		}
		if ( ns <= 1 ) {
		  pe[i][j][k] = 0 ; nf++ ; nt-- ;
		} else if ( ns == 2 ) {
		  if (	(p1[1]-p2[1]+p1[2]-p2[2]+p1[3]-p2[3])==0 ) {
		    pe[i][j][k] = 0 ; nf++ ; nt-- ;
		  }
		}
	    }
         }
       }
     }
   }
   if (nt>3) {
	ns=0;
   } else {
	ns=1;
   }
   return ns;
}

int thin_test2D( ta2D, nsThin, thinType )
    int ta2D[3][3];
    int nsThin;
    int thinType;
{
    int    ii, i, j, ns, nt;
    int    y[9];

    ns = 0; nt = 0;
    for (ii=0; ii < 4; ii++) {
	i = 1 + dvec[ii][0];
	j = 1 + dvec[ii][1];
	if ( ta2D[i][j] == 0 ) { ns++; }
    }
    if ( dvecMax > 6 ) {
      for (ii=6; ii < 10; ii++) {
	i = 1 + dvec[ii][0];
	j = 1 + dvec[ii][1];
	if ( ta2D[i][j] == 0 ) { ns++; }
      }
    }
    if ( (thinType == 1) && (ns >= nsThin) ) {
	for (i=0; i<=2; i++) {
	   for (j=0; j<=2; j++) {
		if (ta2D[i][j]==2) ta2D[i][j]=0;
	   }
	}
	for (i=0; i<=2; i++) {
	   for (j=0; j<=2; j++) {
		nt += ta2D[i][j];
	   }
	}
	if ( (nt==7) || (nt==8) || (nt<=2) ) {
	   ns = 0;
	} else {
	   nt = 0;
	   y[0]=ta2D[1][2]; y[1]=ta2D[2][2]; y[2]=ta2D[2][1];
	   y[3]=ta2D[2][0]; y[4]=ta2D[1][0]; y[5]=ta2D[0][0];
	   y[6]=ta2D[0][1]; y[7]=ta2D[0][2]; y[8]=ta2D[1][2];
	   for (ii=0; ii<=7; ii++) {
		if ( (y[ii]==0) && (y[ii+1]==1) ) {nt++;}
	   }
	   if ( nt != 1 ) { ns = 0; }
	}
    }
    return ns;
}

int thin_test3D( ta3D, nsThin, thinType )
    int ta3D[3][3][3];
    int nsThin;
    int thinType;
{
    int    ii, i, j, k, ns, nt, nf;
    int    pe[3][3][3];
    int    pf[3][3][3];

    ns = 0; nt = 0;
    for (ii=0; ii < dvecMax; ii++) {
	i = 1 + dvec[ii][0];
	j = 1 + dvec[ii][1];
	k = 1 + dvec[ii][2];
	if ( ta3D[i][j][k] == 0 ) { ns++; }
    }
    if ( (thinType == 1) && (ns >= nsThin) ) {
	/* check that the region still percolates */
	nt = 0;
	for (i=0; i<=2; i++) {
	   for (j=0; j<=2; j++) {
	      for (k=0; k<=2; k++) {
		pe[i][j][k] = ta3D[i][j][k];
		pf[i][j][k] = ta3D[i][j][k];
		if ( (i==1) && (j==1) && (k==1) ) {
		    pe[i][j][k]=0;
		    pf[i][j][k]=0;
		} else if (pe[i][j][k]==2) {
		    pe[i][j][k]=0;
		    pf[i][j][k]=1;
		}
		if ( pe[i][j][k]==1 ) {
		   nt++ ;
		}
	      }
	   }
	}
	if (nt==1) ns=0; /* this would erode the skeleton */
	if (ns>=nsThin) {
	   nf = thin_testPerc( pe );
	   if (nf==0) ns=0;
	}

	/* check that no punctures are created */
	if ( ns >= nsThin ) {
	   nf = thin_testErode( pf );
	   if (nf==0) ns=0;
	}
    }
    return ns;
}


int img_morphThin( defn, nsMin, nsMax, doReport, thinType )
   img_defn defn;
   int      nsMin, nsMax, doReport, thinType;
{

   int      doThin, nThin, levThin, nsThin;
   img_vect vec, vec1;
   int      nd, ndp, nn, ns, m, i, j, k, nt;
   float    x1, x2;
   int      ta2D[3][3], ta3D[3][3][3];

  /* determine number of pixels to thin */
   nThin = 0;
   for (nn = 0; nn < defn.ndata; nn++) {
	if ( (defn.data_p[nn] > 0.5) && (defn.data_p[nn] < 1.5) ) {
	    nThin++;
	}
   }

  /* setup the main thinning loop */
   doThin =1; levThin = 2; nsThin = nsMax;
   while (doThin) {

   /* loop over all elements to thin */
	nt = 0;
	if ( doReport )
	  printf( "Thinning: level = %d   sides = %d   # to thin = %d \n",
	levThin, nsThin, nThin);
	x1 = (float)levThin - 0.5 ; x2 = (float)levThin + 0.5;
	for (nn = 0; nn < defn.ndata; nn++) {
	      if ( (defn.data_p[nn] > 0.5) && (defn.data_p[nn] < 1.5) ) {
		ns = 0; img_Index2Vec( defn, nn, &vec ); vec1 = vec;
		if ( defn.ndims == 2 ) {
		   for (i=0; i<=2; i++) {
		     for (j=0; j<=2; j++) {
		        vec1.x = vec.x+i-1;
		        vec1.y = vec.y+j-1;
		        m = img_Vec2Index( defn, vec1 );
		        if ( (m>=0) &&
			     (defn.data_p[m]>0.5) && 
			     (defn.data_p[m]<1.5) ) {
			    ta2D[i][j] = 1;
		        } else if ( (defn.data_p[m]<x1) ||
				    (defn.data_p[m]==defn.blank) ) {
			    ta2D[i][j] = 0;
			} else {
			    ta2D[i][j] = 2;
			}
		     }
		   }
		   ns = thin_test2D( ta2D, nsThin, thinType );
		} else if ( defn.ndims = 3 ) {
		   for (i=0; i<=2; i++) {
		     for (j=0; j<=2; j++) {
		        for (k=0; k<=2; k++) {
		          vec1.x = vec.x+i-1;
		          vec1.y = vec.y+j-1;
		          vec1.z = vec.z+k-1;
		          m = img_Vec2Index( defn, vec1 );
		          if ( (m>=0) &&
			       (defn.data_p[m]>0.5) && 
			       (defn.data_p[m]<1.5) ) {
			    ta3D[i][j][k] = 1;
			  } else if ( (defn.data_p[m]<x1) ||
				      (defn.data_p[m]==defn.blank) ) {
			    ta3D[i][j][k] = 0;
		          } else {
			    ta3D[i][j][k] = 2;
			  }
			}
		     }
		   }
		   ns = thin_test3D( ta3D, nsThin, thinType );
		}
		if (ns >= nsThin) {
		   defn.data_p[nn] = (float)levThin;
		   nt += 1; nThin -= 1;
		}
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
	   if (nsThin < nsMin) {
		doThin = 0;
		for (nn = 0; nn < defn.ndata; nn++) {
		   if ( (defn.data_p[nn] > 0.5) && (defn.data_p[nn] < 1.5) ) {
			defn.data_p[nn] = (float)(levThin);		   }
		}
	   }
	} else {
	   nsThin = nsMax;
	}
   }
   return IC_OK;
}

/*
 *  Perform region identification on a quantized image.  The input
 *  image should have been quantized in integer steps.  This will
 *  usually have been performed using the img_morphThin procedure,
 *  but may alternatively have been prodiced via some other quantization
 *  procedddure or via a modification to the image produced by
 *  img_morphThin.
 */
int img_morphIdent( defn, doReport )
   img_defn defn;
   int      doReport;
{
   ida_nList *np = NULL, *p0, *p1, *p2;
   img_defn  def2;
   img_vect  vec, vec1;
   int       levThin, nd, ne, nn, ns, m, i, nt, search;
   float     x1, x2, x3;
   double    d;
   int       nreg, inReg, levReg;

  /* count pixels to identify */
   inReg = 0; levThin = 0;
   for (nn = 0; nn < defn.ndata; nn++) {
	if ( defn.data_p[nn] > 0.5 ) {
		inReg++;
		if ( defn.data_p[nn] > (float)levThin ) {
			levThin = defn.data_p[nn] + 0.5;
			ns = nn;
		}
	}
   }

  /* perform region identification on this quantized image */
   nreg = levThin + 1; levReg = levThin;
   x3 = (float)levThin + 0.5;


  /* setup seed region */
   np = ida_allocNList( ns, np );
   defn.data_p[ns] = (float)nreg;
   inReg--;

  /* grow and re-seed regions */
   while ( inReg > 0 ) {
	nt = 0;
	x1 = (float)levReg - 0.5; x2 = (float)levReg + 0.5;
	if ( doReport )
	  printf( "Identifying: level %d   regions = %d   # to Id = %d \n",
		   levReg, nreg, inReg);
	/* grow onto existing regions */
	p0 = np;
	for (p1 = np; p1 != NULL; p1 = p2) {
		nn = p1->n; ne = dvecMax;
		img_Index2Vec( defn, nn, &vec ); vec1 = vec;
		for (i=0; i < dvecMax; i++) {
		      vec1.x = vec.x+dvec[i][0];
		      vec1.y = vec.y+dvec[i][1];
		      vec1.z = vec.z+dvec[i][2];
		      m = img_Vec2Index( defn, vec1 );
		      if ( m >= 0 ) {
			if ( (defn.data_p[m]>x1) && (defn.data_p[m]<x2) ) {
			   nt++; inReg--;
			   defn.data_p[m] = defn.data_p[nn];
			   np = ida_allocNList( m, np );
			} else if ( (defn.data_p[m]>(defn.data_p[nn]-0.5)) &&
				    (defn.data_p[m]<(defn.data_p[nn]+0.5)) ) {
			  ne -= 1;
			}
		      } else {
			ne -= 1;
		      }
		}
		if ( ne == 0 ) {
			p2 = p1->p; free( p1 ); p0->p = p2;
		} else {
			p0 = p1 ; p2 = p1->p;
		}
	}

	/* find new regions */
	if (nt == 0) {
	   nn = 0;
	   while ( nn < defn.ndata ) {
		if ( (defn.data_p[nn] > x1) && (defn.data_p[nn] < x2) ) {
		   nreg++;
		   defn.data_p[nn] = (float)nreg;
		   np = ida_allocNList( nn, np );
		   img_percList( defn, x1, x2, defn.data_p[nn], &np, &ns );
		   nt += ns; inReg -= ns;
		}
		nn++ ;
	   }
	}

	/* move onto next level if this one is exhausted */
	if (nt == 0) {
	   levReg -= 1;
	}
   }

  /* normalise range of ID's */
   for (nn = 0; nn < defn.ndata; nn++) {
	if (defn.data_p[nn] > x3) {
	   defn.data_p[nn] = defn.data_p[nn] - (float)levThin;
	}
   }
   for (p1 = np; p1 != NULL; p1 = p2) {p2 = p1->p; free(p1);}
   return IC_OK;
}

/*
 *   Routine to provide the morphological thinning and identification commands
 *
 */
int imgMorphThin (interp, argc, argv)
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      i, l, nsMin, nsMax, doReport, thinType; 
   double   d;
   int      ih;
   img_defn def_in;

  /*
   * Check for compulsary imId argument
   */ 
   if (argc < 3) {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
		      "imId ?-nsmin I? ?-nsmax I?\"", (char *) NULL);
     return TCL_ERROR;
   }
   if (Tcl_GetInt(interp, argv[2], &ih) != TCL_OK) {
	return TCL_ERROR;
   }
   if (ic_hashExists( ih ) != IC_OK) {
	return TCL_ERROR;
   }
   def_in = ic_hash[ih].defn;

  /*
   * loop though all arguments and take action depending on supplied option
   */
   nsMin = 1; doReport = 0; dvecMax = 6; thinType = 0;
   nsMax = 5; if (def_in.ndims == 2) {nsMax = 3;}
   for ( i = 3 ; i < argc ; i++) {
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
	} else if (strncmp(argv[i], "-noreport", l) == 0) {
	   doReport = 0;
	} else if (strncmp(argv[i], "-near1", l) == 0) {
	   dvecMax = 6;
	} else if (strncmp(argv[i], "-near2", l) == 0) {
	   dvecMax = 18;
	} else if (strncmp(argv[i], "-near3", l) == 0) {
	   dvecMax = 26;
	} else if (strncmp(argv[i], "-full", l) == 0) {
	   thinType = 0;
	} else if (strncmp(argv[i], "-skeleton", l) == 0) {
	   thinType = 1;
	}
   }
   if (img_morphThin( def_in, nsMin, nsMax, doReport, thinType ) != IC_OK) {
     Tcl_AppendResult(interp, "Error forming thinned image", (char *) NULL);
     return TCL_ERROR;
   }
   return TCL_OK;
}

int imgMorphIdent (interp, argc, argv)
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      i, l, nsMin, nsMax, doReport; 
   double   d;
   int      ih;
   img_defn def_in;

  /*
   * Check for compulsary imId argument
   */ 
   if (argc < 3) {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
		      "imId \"", (char *) NULL);
     return TCL_ERROR;
   }
   if (ic_parseImid(interp, argv[2], &ih) != TCL_OK) {
	return TCL_ERROR;
   }
   def_in = ic_hash[ih].defn;

  /*
   * loop though all arguments and take action depending on supplied option
   */
   doReport = 0; dvecMax = 26;
   for ( i = 3 ; i < argc ; i++) {
	l = strlen(argv[i]);
	if (strncmp(argv[i], "-report", l) == 0) {
	   doReport = 1;
	} else if (strncmp(argv[i], "-noreport", l) == 0) {
	   doReport = 0;
	} else if (strncmp(argv[i], "-near1", l) == 0) {
	   dvecMax = 6;
	} else if (strncmp(argv[i], "-near2", l) == 0) {
	   dvecMax = 18;
	} else if (strncmp(argv[i], "-near3", l) == 0) {
	   dvecMax = 26;
	}
   }
   if (img_morphIdent( def_in, doReport ) != IC_OK) {
     Tcl_AppendResult(interp, "Error identifying image", (char *) NULL);
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
 *   Routine to provide the identify command
 *
 */
int imgRegIdent (interp, argc, argv)
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
   if (argc < 5) {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
		      "imId min max ?subimage? ?-index X ?-incr X?\"", (char *) NULL);
     return TCL_ERROR;
   }
   if (Tcl_GetInt(interp, argv[2], &ih) != TCL_OK) {
	return TCL_ERROR;
   }
   if (ic_hashExists( ih ) != IC_OK) {
	return TCL_ERROR;
   }
   def_in = ic_hash[ih].defn;
   def_out = ic_hash[ih].defn;
   if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	return TCL_ERROR;
   }
   x1 = d;
   if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	return TCL_ERROR;
   }
   x2 = d;

  /*
   * Parse for subimage to operate on
   */
   if (ic_parseSubIm( interp, 5, argc, argv, &def_out) != IC_OK ) {
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
		    "-index I \"", (char *) NULL);
		return TCL_ERROR;
	   }
	   indx = d;
	} else if (strncmp(argv[i], "-incr", l) == 0) {
	   i++;
	   if (i<argc) {
		Tcl_GetDouble(interp, argv[i], &d);
	   } else {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    "-incr I \"", (char *) NULL);
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

/*
 *   Routine to provide the full range of region-analysis commands
 *
 */
int imgRegion (dummy, interp, argc, argv)
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
   * Sort out options to command, returning an error on an incorrect option
   */
   if (argc < 1) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
	"img_region option \"", (char *) NULL);
	return TCL_ERROR;
   }
   status = 0 ;
   c = argv[1][0];
   length = strlen(argv[1]);
   if ((c == 'p') && (strncmp(argv[1], "percolate", length) == 0)) {
	if ( imgPercolate(interp, argc, argv) != TCL_OK) {
	      return TCL_ERROR;
	}
	return TCL_OK;

   } else if ((c == 't') && (strncmp(argv[1], "thin", length) == 0)) {
	if ( imgMorphThin(interp, argc, argv) != TCL_OK) {
	      return TCL_ERROR;
	}
	return TCL_OK;

   } else if ((c == 'i') && (strncmp(argv[1], "identify", length) == 0)) {
	if ( imgMorphIdent(interp, argc, argv) != TCL_OK) {
	      return TCL_ERROR;
	}
	return TCL_OK;

   } else if ((c == 'r') && (strncmp(argv[1], "region-identify", length) == 0)) {
	if ( imgRegIdent(interp, argc, argv) != TCL_OK) {
	      return TCL_ERROR;
	}
	return TCL_OK;

   } else if ((c == 'c') && 
	      (strncmp(argv[1], "clear-classification", length) ==0)) {
	int n;
	ida_nList *p1, *p2;
	ida_nbList *ap1, *ap2;
	if (argc < 3) {
	   Tcl_AppendResult(interp, "wrong # args: should be \"",
		"claer-classification imId \"", (char *) NULL);
	   return TCL_ERROR;
	}
	if (ic_parseImid(interp, argv[2], &ih) != TCL_OK) {
	   return TCL_ERROR;
	}
	for (n = 0; n < ic_hash[ih].class.nreg; n++) {
	   for (p1 = ic_hash[ih].class.reg[n].p; p1 != NULL; p1 = p2) {
		p2 = p1->p; free(p1);
	   }
	   for (ap1 = ic_hash[ih].class.reg[n].ap; ap1 != NULL; ap1 = ap2) {
		ap2 = ap1->p; free(ap1);
	   }
	}
	if (ic_hash[ih].class.nreg > 0) free( ic_hash[ih].class.reg );
	ic_hash[ih].class.nreg = 0;

   } else if ((c == 'c') && (strncmp(argv[1], "classify", length) ==0)) {
	img_vect vec, vec1;
	ida_nbList   *p1, *p2;
	double   d;
	int      i, l, m, n, nn, nd, found, type;
	float    xmin, xmax, xincr, x1, x2;
	int      nreg, ireg, save_nlist;
	img_reg  *reg;


	if (argc < 6) {
	   Tcl_AppendResult(interp, "wrong # args: should be \"",
		"clasify imId Xmin Xmax Xincr ?subimage? \"", (char *) NULL);
	   return TCL_ERROR;
	}
	if (ic_parseImid(interp, argv[2], &ih) != TCL_OK) {
	   return TCL_ERROR;
	}
	def_in = ic_hash[ih].defn;

	/* parse for compulsary items */

	if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	      return TCL_ERROR;
	}
	ic_hash[ih].class.min = d; xmin = d;
	if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	      return TCL_ERROR;
	}
	ic_hash[ih].class.max = d; xmax = d;

	if (Tcl_GetDouble(interp, argv[5], &d) != TCL_OK) {
	      return TCL_ERROR;
	}
	ic_hash[ih].class.incr = d; xincr = d;
	dvecMax = 6; save_nlist = 1;
	for ( i = 6 ; i < argc ; i++) {
	   l = strlen(argv[i]);
	   if (strncmp(argv[i], "-near1", l) == 0) {
		dvecMax = 6;
	   } else if (strncmp(argv[i], "-near2", l) == 0) {
		dvecMax = 18;
	   } else if (strncmp(argv[i], "-near3", l) == 0) {
		dvecMax = 26;
	   } else if (strncmp(argv[i], "-nolist", l) == 0) {
		save_nlist = 0;
	   }
	}

	/* check that the volume is not already classified */
	nreg = ic_hash[ih].class.nreg;
	if ( nreg > 0 ) {
		sprintf( value, "%d", nreg);
		Tcl_AppendElement( interp, value );
		return TCL_OK;
	}

	/* determine and allocate number of regions */
	ic_hash[ih].class.nreg = 2.5 + ( xmax - xmin )/ xincr;
	nreg = ic_hash[ih].class.nreg;
	ic_hash[ih].class.reg = ( img_reg * )
		 malloc ( ic_hash[ih].class.nreg * sizeof( img_reg ) );
	reg = ic_hash[ih].class.reg;
	for (n = 0; n < nreg; n++) {
		reg[n].p = NULL;
		reg[n].ap = NULL;
		reg[n].x1 = xmin + xincr * (float) (n-1);
		reg[n].x2 = reg[n].x1 + xincr;
		reg[n].anum = 0;
		reg[n].num = 0;
		reg[n].xs = 0.0;
		reg[n].ys = 0.0;
		reg[n].zs = 0.0;
		reg[n].xs2 = 0.0;
		reg[n].ys2 = 0.0;
		reg[n].zs2 = 0.0;
	}

	/* do shape analysis */
	for (nn = 0; nn < def_in.ndata; nn++) {
		if ( def_in.data_p[nn] != def_in.blank ) {
			if ( def_in.data_p[nn] < xmin ) {
			     ireg = 0;
			} else if ( def_in.data_p[nn] > xmax ) {
			     ireg = nreg - 1;
			} else {
			     img_Index2Vec( def_in, nn, &vec ); vec1 = vec;
			     ireg = 1 + (def_in.data_p[nn]-xmin)/xincr;
			     x1 = reg[ireg].x1; x2 = reg[ireg].x2;
			     reg[ireg].num += 1;
			     if (save_nlist) {
				reg[ireg].p = ida_allocNList( nn, reg[ireg].p );
			     }
			     reg[ireg].xs += (float)vec.x;
			     reg[ireg].ys += (float)vec.y;
			     reg[ireg].zs += (float)vec.z;
			     reg[ireg].xs2 += (float)vec.x * (float)vec.x;
			     reg[ireg].ys2 += (float)vec.y * (float)vec.y;
			     reg[ireg].zs2 += (float)vec.z * (float)vec.z;
			     for (i=0; i < dvecMax; i++) {
				vec1.x = vec.x+dvec[i][0];
				vec1.y = vec.y+dvec[i][1];
				vec1.z = vec.z+dvec[i][2];
				m = img_Vec2Index( def_in, vec1 );
				if ( m >= 0 ) {
				    if ( (def_in.data_p[m] != def_in.blank) &&
					 ((def_in.data_p[m] < x1) ||
					  (def_in.data_p[m] > x2)) ) {
					reg[ireg].anum += 1;
					type = def_in.data_p[m] + 0.5;
					found = 0; p1 = reg[ireg].ap;
					while (p1 != NULL ) {
					  if ( p1->type == type ) {
					    found = 1; p1->num += 1; p1 = NULL;
					  } else {
					    p1 = p1->p;
					  }
					}
					if (found == 0) {
					  reg[ireg].ap = 
					   ida_allocNbList( type, reg[ireg].ap );
					   reg[ireg].ap->num += 1;
					}
				    } 
				}
			     }
			}

		}
	}
	sprintf( value, "%d", nreg);
	Tcl_AppendElement( interp, value );
	return TCL_OK;

   } else if ((c == 'e') && (strncmp(argv[1], "enqshape", length) ==0)) {
	ida_nbList   *nb1 = NULL;
	ida_nbList   *p1, *p2;
	float	     x1, x2, xs, ys, zs, xs2, ys2, zs2, rr;
	int          ns, num, ir, type, Vol, Area;
	img_reg      *reg;

	if (argc < 4) {
	   Tcl_AppendResult(interp, "wrong # args: should be \"",
		"shape imId regionId \"", (char *) NULL);
	   return TCL_ERROR;
	}
	if (ic_parseImid(interp, argv[2], &ih) != TCL_OK) {
	   return TCL_ERROR;
	}
	def_in = ic_hash[ih].defn;
	def_out = ic_hash[ih].defn;

	/* parse for compulsary items */
	if (Tcl_GetInt(interp, argv[3], &ir) != TCL_OK) {
	      return TCL_ERROR;
	}
	if ( (ir < 0) || (ir >= ic_hash[ih].class.nreg) ) {
	      Tcl_AppendResult(interp, "illegal region identifier",
		(char *) NULL);
	      return TCL_ERROR;
	}


	/* do shape analysis on classified volume data */
	reg = ic_hash[ih].class.reg;
	x1 = reg[ir].x1;
	x2 = reg[ir].x2;
	Vol = reg[ir].num;
	Area = reg[ir].anum;
	xs = reg[ir].xs;
	xs2 = reg[ir].xs2;
	ys = reg[ir].ys;
	ys2 = reg[ir].ys2;
	zs = reg[ir].zs;
	zs2 = reg[ir].zs2;
	ns = Vol;
	xs = xs / (float)ns ; ys = ys / (float)ns ; zs = zs / (float)ns ;
	xs2 = xs2 / (float)ns ; ys2 = ys2 / (float)ns ; zs2 = zs2 / (float)ns ;
	rr = xs2 + ys2 + zs2 - (xs*xs + ys*ys + zs*zs); rr = sqrt( (double)rr );
	sprintf( value, "%d", Vol);
	Tcl_AppendElement( interp, value );
	sprintf( value, "%d", Area);
	Tcl_AppendElement( interp, value );
	sprintf( value, "%f %f %f %f", xs, ys, zs, rr);
	Tcl_AppendElement( interp, value );
	for (p1 = reg[ir].ap; p1 != NULL; p1 = p1->p) {
		type = p1->type; num = p1->num;
		sprintf( value, "%d %d", type, num);
		Tcl_AppendElement( interp, value );
	}
	return TCL_OK;

   } else if ((c == 's') && (strncmp(argv[1], "shape", length) ==0)) {
	ida_nbList   *nb1 = NULL;
	ida_nbList   *p1, *p2;
	ida_nList    *np;
	img_vect     vec, vec1;
	double       d;
	int          m, n, nn, ns, i, l, ir, type, num, found;
	float	     x1, x2, xs, ys, zs, xs2, ys2, zs2, rr;
	int          Vol, Area;

	if (argc < 4) {
	   Tcl_AppendResult(interp, "wrong # args: should be \"",
		"shape imId regionId \"", (char *) NULL);
	   return TCL_ERROR;
	}
	if (ic_parseImid(interp, argv[2], &ih) != TCL_OK) {
	   return TCL_ERROR;
	}
	def_in = ic_hash[ih].defn;
	def_out = ic_hash[ih].defn;

	/* parse for compulsary items */
	if (Tcl_GetInt(interp, argv[3], &ir) != TCL_OK) {
	      return TCL_ERROR;
	}
	if ( (ir < 0) || (ir >= ic_hash[ih].class.nreg) ) {
	      Tcl_AppendResult(interp, "illegal region identifier",
		(char *) NULL);
	      return TCL_ERROR;
	}

	/* parse for optional items */
	dvecMax = 6;
	for ( i = 4 ; i < argc ; i++) {
	   l = strlen(argv[i]);
	   if (strncmp(argv[i], "-near1", l) == 0) {
		dvecMax = 6;
	   } else if (strncmp(argv[i], "-near2", l) == 0) {
		dvecMax = 18;
	   } else if (strncmp(argv[i], "-near3", l) == 0) {
		dvecMax = 26;
	   }
	}

	/* do shape analysis on classified volume data */
	x1 = ic_hash[ih].class.reg[ir].x1;
	x2 = ic_hash[ih].class.reg[ir].x2;
	Vol = 0; Area = 0; nb1 = ida_allocNbList( 0, nb1 );
	xs = 0.0; xs2 = 0.0; ys = 0.0; ys2 = 0.0; zs = 0.0; zs2 = 0.0;
	ns = 0;
	for (np = ic_hash[ih].class.reg[ir].p ; np != NULL; np = np->p) {
		nn = np->n; Vol++; img_Index2Vec( def_in, nn, &vec );
		vec1 = vec; ns++;
		xs += (float)vec.x; ys += (float)vec.y; zs += (float)vec.z;
		xs2 += (float)vec.x * (float)vec.x;
		ys2 += (float)vec.y * (float)vec.y;
		zs2 += (float)vec.z * (float)vec.z;
		for (i=0; i < dvecMax; i++) {
			vec1.x = vec.x+dvec[i][0];
			vec1.y = vec.y+dvec[i][1];
			vec1.z = vec.z+dvec[i][2];
			m = img_Vec2Index( def_in, vec1 );
			if ( m >= 0 ) {
			    if ( (def_in.data_p[m] != def_in.blank) &&
				 ((def_in.data_p[m] < x1) ||
				  (def_in.data_p[m] > x2)) ) {
				Area ++;
				type = def_in.data_p[m] + 0.5;
				found = 0; p1 = nb1;
				while (p1 != NULL ) {
					if ( p1->type == type ) {
					   found = 1; p1->num += 1; p1 = NULL;
					} else {
					   p1 = p1->p;
					}
				}
				if (found == 0) {
					nb1 = ida_allocNbList( type, nb1 );
					nb1->num += 1;
				} 
			    }
			}
		}
	}
	xs = xs / (float)ns ; ys = ys / (float)ns ; zs = zs / (float)ns ;
	xs2 = xs2 / (float)ns ; ys2 = ys2 / (float)ns ; zs2 = zs2 / (float)ns ;
	rr = xs2 + ys2 + zs2 - (xs*xs + ys*ys + zs*zs); rr = sqrt( (double)rr );
	sprintf( value, "%d", Vol);
	Tcl_AppendElement( interp, value );
	sprintf( value, "%d", Area);
	Tcl_AppendElement( interp, value );
	sprintf( value, "%f %f %f %f", xs, ys, zs, rr);
	Tcl_AppendElement( interp, value );
	for (p1 = nb1; p1 != NULL; p1 = p2) {
		type = p1->type; num = p1->num; p2 = p1->p; free(p1);
		sprintf( value, "%d %d", type, num);
		Tcl_AppendElement( interp, value );
	}
	return TCL_OK;

   } else if ((c == 'g') && (strncmp(argv[1], "getshape", length) ==0)) {
	ida_nbList   *nb1 = NULL;
	ida_nbList   *p1, *p2;
	ida_nList    *np;
	img_vect vec, vec1;
	double   d;
	int      l, m, n, nn, nd, i, ir, surface, found, toStdout, f;
	int      type, nt, mt;
	float    x1, x2;
	if (argc < 4) {
	   Tcl_AppendResult(interp, "wrong # args: should be \"",
		"getshape imId regionId \"", (char *) NULL);
	   return TCL_ERROR;
	}

	if (ic_parseImid(interp, argv[2], &ih) != TCL_OK) {
	   return TCL_ERROR;
	}
	def_in = ic_hash[ih].defn;
	def_out = ic_hash[ih].defn;

	/* parse for compulsary items */
	if (Tcl_GetInt(interp, argv[3], &ir) != TCL_OK) {
	      return TCL_ERROR;
	}

	/* parse for optional items */
	dvecMax = 6; surface = 0; toStdout = 0;
	for ( i = 4 ; i < argc ; i++) {
	   l = strlen(argv[i]);
	   if (strncmp(argv[i], "-surface", l) == 0) {
		surface = 1;
	   } else if (strncmp(argv[i], "-near1", l) == 0) {
		dvecMax = 6;
	   } else if (strncmp(argv[i], "-near2", l) == 0) {
		dvecMax = 18;
	   } else if (strncmp(argv[i], "-near3", l) == 0) {
		dvecMax = 26;
	   } else if (strncmp(argv[i], "-stdout", l) == 0) {
		toStdout = 1;
	   }
	}
	if ( (ir < 0) || (ir >= ic_hash[ih].class.nreg) ) {
	      Tcl_AppendResult(interp, "illegal region identifier",
		(char *) NULL);
	      return TCL_ERROR;
	}

	/* get shape from classified volume data */
	x1 = ic_hash[ih].class.reg[ir].x1;
	x2 = ic_hash[ih].class.reg[ir].x2;
	nb1 = ida_allocNbList( 0, nb1 );  nt = 0;
	for (np = ic_hash[ih].class.reg[ir].p ; np != NULL; np = np->p) {
		nn = np->n; img_Index2Vec( def_in, nn, &vec );
		if ( surface == 1 ) {
			found = 0; vec1 = vec;
			for (i=0; i < dvecMax; i++) {
			      vec1.x = vec.x+dvec[i][0];
			      vec1.y = vec.y+dvec[i][1];
			      vec1.z = vec.z+dvec[i][2];
			      m = img_Vec2Index( def_in, vec1 );
			      if ( m >= 0 ) {
				if ( (def_in.data_p[m] != def_in.blank) &&
				     ((def_in.data_p[m] < x1) ||
				      (def_in.data_p[m] > x2)) ) {
				   found = 1;
				   type = def_in.data_p[m] + 0.5;
				   f = 0; p1 = nb1;
				   while (p1 != NULL ) {
					if ( p1->type == type ) {
					   f = 1; mt = p1->num; p1 = NULL;
					} else {
					   p1 = p1->p;
					}
				   }
				   if (f == 0) {
					nb1 = ida_allocNbList( type, nb1 );
					nt++; nb1->num = nt; mt = nt;
				   } 
				}
			      }
			}
		} else {
			found = 1; nt = 1; mt = 0;
		}
		if (found == 1) {
		   if (toStdout == 1) {
			printf( "%d %d %d %d %d\n", vec.x, vec.y, vec.z, mt, ir);
		   } else {
			sprintf( value, "%d %d %d %d", vec.x, vec.y, vec.z, mt);
			Tcl_AppendElement( interp, value );
		   }
		}
	}
	for (p1 = nb1; p1 != NULL; p1 = p2) {p2 = p1->p; free(p1);}
	return TCL_OK;

   } else {
	Tcl_AppendResult(interp, "bad option \"", argv[2],
		"\": should be ...\"",
		(char *) NULL);
	return TCL_ERROR;
   }
}
