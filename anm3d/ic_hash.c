/*
 *  Routines to implement image IO and hash table management
 *
 */

#include "tk.h"
#include "string.h"
#include "ic.h"

/*
 * Initialise the hash table
 */
int ic_hashInit( ihash )
   int ihash;
{
   int i, i1, i2;
   i1 = ihash ; i2 = ihash-1;
   if ( (ihash < 0) || (ihash > HASH_SIZE) ) {
	i1 = 0 ; i2 = HASH_SIZE;
   }
   for (i = i1 ; i < i2 ; i++) {
	ic_hash[i].mode = 0;
   }
   return IC_OK;
}

/*
 * Next free entry in the hash table
 */
int ic_hashNext(free)
   int *free;
{
   int i;
   *free = -1;
   for (i = 0 ; i < HASH_SIZE ; i++) {
	if (ic_hash[i].mode == 0) {
		*free = i;
		return IC_OK;
	}
   }
   return IC_ERROR;
}

/*
 * Check that an entry in the hash table is valid
 */
int ic_hashExists( ih )
   int ih;
{
   int i;
   if ( (ih >= 0) && (ih < HASH_SIZE) && (ic_hash[ih].mode == 1) ) {
	return IC_OK;
   } else {
	return IC_ERROR;
   }
}

/*
 * Create an image
 */
int ic_imgCreate( defn, ih )
   img_defn *defn;
   int *ih;
{
   if (ic_hashNext(ih) == IC_OK) {
	defn->data_p = (float *)malloc( defn->ndata * sizeof(float) );
	defn->header_p = (int *)malloc( HEADER_SIZE * sizeof(int) );
	ic_hash[*ih].defn = *defn;
	ic_hash[*ih].mode = 1;
   } else {
	return IC_NOHASH;
   }
   if ( (defn->x2 - defn->x1 + 1) != defn->xdim ) {
	defn->x1 = 1; defn->x2 = defn->xdim; }
   if ( (defn->y2 - defn->y1 + 1) != defn->ydim ) {
	defn->y1 = 1; defn->y2 = defn->ydim; }
   if ( (defn->z2 - defn->z1 + 1) != defn->zdim ) {
	defn->z1 = 1; defn->z2 = defn->zdim; }
   if ( (defn->t2 - defn->t1 + 1) != defn->tdim ) {
	defn->t1 = 1; defn->t2 = defn->tdim; }
   if ( (defn->v2 - defn->v1 + 1) != defn->vdim ) {
	defn->v1 = 1; defn->v2 = defn->vdim; }
   ic_hash[*ih].defn = *defn;
   ic_hash[*ih].class.nreg = 0;
   return IC_OK;
}

/*
 * Destroy an image
 */
int ic_imgDestroy( ih )
   int ih;
{
   int n;
   ida_nList *p1, *p2;
   if ( ic_hashExists( ih ) == IC_OK ) {
	free( ic_hash[ih].defn.data_p );
	free( ic_hash[ih].defn.header_p );
	ic_hash[ih].mode = 0;
	for (n = 0; n < ic_hash[ih].class.nreg; n++) {
	   for (p1 = ic_hash[ih].class.reg[n].p; p1 != NULL; p1 = p2) {
		p2 = p1->p; free(p1);
	   }
	}
	if (ic_hash[ih].class.nreg > 0) free( ic_hash[ih].class.reg );
	ic_hash[ih].class.nreg = 0;
   }
   return IC_OK;
}

/*
 * Assign data to an existing image and destroy workspace
 */
int ic_imgAssign( ih_from, ih_to )
   int ih_from, ih_to;
{
   float *pf;
   int   *pi;
   if ( (ic_hashExists( ih_to ) == IC_OK ) &&
        (ic_hashExists( ih_from ) == IC_OK ) ) {
	free( ic_hash[ih_to].defn.data_p );
	free( ic_hash[ih_to].defn.header_p );
        ic_hash[ih_to].defn.data_p = ic_hash[ih_from].defn.data_p;
        ic_hash[ih_to].defn.header_p = ic_hash[ih_from].defn.header_p;
	ic_hash[ih_from].mode = 0;
	return IC_OK;
   }
}



/*
 * Read image data
 */
int ic_imgRead( rec, file, type, defn, ih )
   ic_iclRec *rec;
   char *file;
   int type;
   img_defn *defn;
   int *ih;
{
   if (ic_hashExists( *ih ) == IC_OK) {
	ic_imgDestroy( *ih );
   } else {
	if (ic_hashNext( ih ) == IC_ERROR) {
		return IC_ERROR;
	}
   }
   if (ic_imgFileDefn( file, &type, defn ) == IC_OK) {
	defn->data_p = (float *)malloc( defn->ndata * sizeof(float) );
	defn->header_p = (int *)malloc( HEADER_SIZE * sizeof(int) );
	ic_hash[*ih].defn = *defn;
	ic_hash[*ih].rec = *rec;
	strcpy( ic_hash[*ih].rec.file, file );
	ic_hash[*ih].rec.type = type;
	ic_hash[*ih].mode = 1;
	ic_imgFileRead( file, &type, defn );
	return IC_OK;
   } else {
	return IC_NOHASH;
   }
}

/*
 * Write image data
 */
int ic_imgWrite( ih, file, type )
   int ih;
   char *file;
   int type;
{
   if (ic_hashExists( ih ) == IC_OK) {
	ic_imgFileWrite( file, &type, &ic_hash[ih].defn );
	return IC_OK;
   } else {
	return IC_NOHASH;
   }
}

/*
 * Read image definition from named file
 */
int ic_imgFileDefn( file, type, defn )
   char *file;
   int *type;
   img_defn *defn;
{
   int status = 0;
   char ff[128];
   strcpy( ff, file );
   ic_imgfreaddefn_( ff, type, defn, &status );
   return status;
}

/*
 * Read image data from named file
 */
int ic_imgFileRead( file, type, defn )
   char *file;
   int *type;
   img_defn *defn;
{
   int status = 0;
   char ff[128];
   strcpy( ff, file );
   ic_imgfreaddata_( ff, type, defn, defn->data_p, defn->header_p, &status );
   return status;
}

/*
 * Write image data to named file
 */
int ic_imgFileWrite( file, type, defn )
   char *file;
   int *type;
   img_defn *defn;
{
   int status = 0;
   char ff[128];
   strcpy( ff, file );
   ic_imgfwritedata_( ff, type, defn, defn->data_p, defn->header_p, &status );
   return status;
}

/*
 * Parse an image definition
 */
int ic_parseImid( interp, string, ih )
    Tcl_Interp *interp;			/* Current interpreter. */
    char *string;			/* String containing imId?. */
    int *ih;				/* Returned imId. */
{
   if (Tcl_GetInt(interp, string, ih) != TCL_OK) {
	return TCL_ERROR;
   }
   if (ic_hashExists( *ih ) != IC_OK) {
        Tcl_AppendResult(interp, "invalid imId", (char *) NULL);
	return TCL_ERROR;
   }
   return TCL_OK;
}

/*
 * Set results string to an imId
 */
int ic_resultImid( interp, ih )
    Tcl_Interp *interp;			/* Current interpreter. */
    int ih;				/* imId. */
{
   sprintf( interp->result, "%d", ih);
   return TCL_OK;
}

/*
 * Parse a sub-image definition
 */
int ic_parseSubIm( interp, start, argc, argv, defn)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
    img_defn *defn;			/* Definition record. */
{
  int i, j, l;
  double d;

  if (start >= argc)
	return TCL_OK;

  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strcmp(argv[i], "-x1") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  defn->x1 = j;
	}
    } else if (strcmp(argv[i], "-x2") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  defn->x2 = j;
	}
    } else if (strcmp(argv[i], "-y1") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  defn->y1 = j;
	}
    } else if (strcmp(argv[i], "-y2") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  defn->y2 = j;
	}
    } else if (strcmp(argv[i], "-z1") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  defn->z1 = j;
	}
    } else if (strcmp(argv[i], "-z2") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  defn->z2 = j;
	}
    } else if (strcmp(argv[i], "-t1") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  defn->t1 = j;
	}
    } else if (strcmp(argv[i], "-t2") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  defn->t2 = j;
	}
    } else if (strcmp(argv[i], "-v1") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  defn->v1 = j;
	}
    } else if (strcmp(argv[i], "-v2") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  defn->v2 = j;
	}
    }
  }
  if ( (defn->x2 < defn->x1) ) {
	i = defn->x2 ; defn->x2 = defn->x1 ; defn->x1 = i;
  }
  if ( (defn->y2 < defn->y1) ) {
	i = defn->y2 ; defn->y2 = defn->y1 ; defn->y1 = i;
  }
  if ( (defn->z2 < defn->z1) ) {
	i = defn->z2 ; defn->z2 = defn->z1 ; defn->z1 = i;
  }
  if ( (defn->t2 < defn->t1) ) {
	i = defn->t2 ; defn->t2 = defn->t1 ; defn->t1 = i;
  }
  if ( (defn->v2 < defn->v1) ) {
	i = defn->v2 ; defn->v2 = defn->v1 ; defn->v1 = i;
  }
  defn->xdim = (defn->x2 - defn->x1) + 1;
  defn->ydim = (defn->y2 - defn->y1) + 1;
  defn->zdim = (defn->z2 - defn->z1) + 1;
  defn->tdim = (defn->t2 - defn->t1) + 1;
  defn->vdim = (defn->v2 - defn->v1) + 1;
  defn->ndata = defn->xdim * defn->ydim * defn->zdim * defn->tdim;
  defn->ndims = 1;
  if (defn->tdim > 1) {
	defn->ndims = 4;
  } else if (defn->zdim > 1) {
	defn->ndims = 3;
  } else if (defn->ydim > 1) {
	defn->ndims = 2;
  }
  return IC_OK;
}

/*
 * Parse a vector position in an image
 */
int ic_parseVect( interp, start, argc, argv, vec)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
    img_vect *vec;			/* Image vector position. */
{
  int i, j, l;
  double d;

  if (start >= argc)
	return TCL_OK;

  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strcmp(argv[i], "-x") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  vec->x = j;
	}
    } else if (strcmp(argv[i], "-y") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  vec->y = j;
	}
    } else if (strcmp(argv[i], "-z") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  vec->z = j;
	}
    } else if (strcmp(argv[i], "-t") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  vec->t = j;
	}
    } else if (strcmp(argv[i], "-v") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  vec->v = j;
	}
    }
  }
  return IC_OK;
}

/*
 * Parse a vector position in an as a floating point position
 */
int ic_parseVectF( interp, start, argc, argv, vec)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
    img_vectF *vec;			/* Image vector position. */
{
  int i, j, l;
  double d;

  if (start >= argc)
	return TCL_OK;

  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strcmp(argv[i], "-x") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  vec->x = d;
	}
    } else if (strcmp(argv[i], "-y") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  vec->y = d;
	}
    } else if (strcmp(argv[i], "-z") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  vec->z = d;
	}
    } else if (strcmp(argv[i], "-t") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  vec->t = d;
	}
    } else if (strcmp(argv[i], "-v") == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  vec->v = j;
	}
    }
  }
  return IC_OK;
}
