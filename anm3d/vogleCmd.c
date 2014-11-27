/*
 *   Tcl binding to the PGPLOT graphics library
 *
 */

#include "tk.h"
#include "eqn.h"
#include "string.h"
#include "ic.h"
#include "vogle.h"

/* 
 * Define default style parameters
 *
 */
   static int   dimension = 3;
   static int   colour = 7;
   static int   background = 0;
   static int   foreground = 1;
   static int   polyFill = 1;
   static int   polyHatch = 0;
   static float polyAngle = 45.0;
   static float polyPitch = 1.0;
   static int   lineType = 0;
   static int   lineClipping = 1;
   static int   circlePrecision = 32;
   static int   curvePrecision = 32;
   static int   patchPrecision_tseg = 32;
   static int   patchPrecision_useg = 32;
   static int   backfaceView = 1;
   static int   backfaceSense = 1;
   static float lineDash = 1.0 ;
   static float charHeight = 1.0 ;
   static float charWidth = 1.0 ;
   static char  lineStyle[20] = "1";
   static char  charFont[20] = "times.r" ;

/*
 * Global data structures used in all routines
 *
 */
   static int dim = 3;
   static int type = 0;

/* 
 * data arrays and number of data points
 */
   static float *x, *y, *ex1, *ex2, *ey1, *ey2 ;
   static int   errx, erry, err2x, err2y;
   static int   ndata;


/*
 *  Sort out style options for all drawing actions
 *
 */
static int
vogleStyle(interp, start, argc, argv)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
  int i, l;
  double d;
  float x;
  if (start >= argc)
	return TCL_OK;

  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strncmp(argv[i], "-dash", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  lineDash = d ; setdash( lineDash );
	}
    } else if (strncmp(argv[i], "-style", l) == 0) {
	i++;
	if ( i < argc) {
	   strncpy( lineStyle, argv[i], 20 ) ;
	   linestyle( lineStyle ) ;
        }
    } else if (strncmp(argv[i], "-font", l) == 0) {
	i++;
	if ( i < argc) {
	   strncpy( charFont, argv[i], 20 ) ;
	   font( charFont ) ;
	}
    } else if (strncmp(argv[i], "-size", l) == 0) {
	i++;
	if ( (i+1) < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  charHeight = d;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  charWidth = d;
	  textsize( charHeight, charWidth );
	}
    } else if (strncmp(argv[i], "-fill", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &polyFill) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  polyfill( polyFill );
	}
    } else if (strncmp(argv[i], "-hatching", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &polyHatch) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  polyhatch( polyHatch );
	}
    } else if (strncmp(argv[i], "-angle", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  polyAngle = d;
	  hatchang( polyAngle );
	}
    } else if (strncmp(argv[i], "-pitch", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  polyPitch = d;
	  hatchpitch( polyPitch );
	}
    } else if (strncmp(argv[i], "-binned", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &lineType) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}
    } else if (strncmp(argv[i], "-clipping", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &lineClipping) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  clipping( lineClipping );
	}
    } else if (strncmp(argv[i], "-backface", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &backfaceView) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  backface( backfaceView ) ;
	}
    } else if (strncmp(argv[i], "-sense", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &backfaceSense) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  backfacedir( backfaceSense ) ;
	}

    } else if (
		(strncmp(argv[i], "-colour", l) == 0) ||
		(strncmp(argv[i], "-color", l) == 0)) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &colour) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  color( colour );
	}
    } else if ( (strncmp(argv[i], "-background", l) == 0) ) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &background) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  color( background );
	}
    } else if ( (strncmp(argv[i], "-foreground", l) == 0) ) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &foreground) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  color( foreground );
	}
    } else if ( (strncmp(argv[i], "-circleprecision", l) == 0) ) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &circlePrecision) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  circleprecision( circlePrecision );
	}
    } else if ( (strncmp(argv[i], "-curveprecision", l) == 0) ) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &curvePrecision) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  curveprecision( curvePrecision );
	}
    } else if (strncmp(argv[i], "-rgb", l) == 0) {
	int col, r, g, b;
	col = 1 ;
	if ( (i+4) < argc ) {
	  i++;
	  if (Tcl_GetInt(interp, argv[i], &col) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetInt( interp, argv[i], &r) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetInt(interp, argv[i], &g) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetInt(interp, argv[i], &b) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  mapcolor( col, r, g, b );
	}
    } else if (strncmp(argv[i], "-save", l) == 0) {
	pushattributes();

    } else if (strncmp(argv[i], "-restore", l) == 0) {
	popattributes();
    }
  }
  return TCL_OK;
}

/*
 *  Provide an enquiry routine
 *
 */
static int
vogleEnquire(interp, argc, argv)
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Enquire Options. */
{
  char string[30];
  int n, l, units;
  float s, wx1, wx2, wy1, wy2;
  double d;

  l = strlen( argv[2] );
  if (strncmp(argv[2], "device", l) == 0) {
	vgetdev( string );
	sprintf( interp->result, "%s", string);
	return TCL_OK;
  } else if (strncmp(argv[2], "depth", l) == 0) {
	n = getdepth();
	sprintf( interp->result, "%d", n);
	return TCL_OK;
  } else if (strncmp(argv[2], "numchars", l) == 0) {
	n = numchars();
	sprintf( interp->result, "%d", n);
	return TCL_OK;
  } else if (strncmp(argv[2], "charsize", l) == 0) {
	char c;
	if (argc < 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" enquire charsize char\"", (char *) NULL);
	    return TCL_ERROR;
	}
	c = argv[3][0];
	getcharsize( c, &wx1, &wx2 );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx2);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if (strncmp(argv[2], "fontsize", l) == 0) {
	getfontsize( &wx1, &wx2 );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx2);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if (strncmp(argv[2], "spos2", l) == 0) {
	sgetgp2( &wx1, &wx2 );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx2);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if (strncmp(argv[2], "pos2", l) == 0) {
	getgp2( &wx1, &wx2 );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx2);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if (strncmp(argv[2], "pos3", l) == 0) {
	getgp( &wx1, &wx2, &wy1 );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx2);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy1);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if ( (strncmp(argv[2], "viewport", l) == 0) || 
	      (strncmp(argv[2], "vport", l) == 0) ) {
	getviewport( &wx1, &wx2, &wy1, &wy2 );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx2);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy2);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if (strncmp(argv[2], "aspect", l) == 0) {
	wx1 = getaspect();
	sprintf( interp->result, "%f", wx1);
	return TCL_OK;
  } else if (strncmp(argv[2], "factors", l) == 0) {
	getfactors( &wx1, &wx2 );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx2);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if (strncmp(argv[2], "display", l) == 0) {
	getdisplaysize( &wx1, &wx2 );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx2);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if (strncmp(argv[2], "locator", l) == 0) {
	int k;
	k = locator( &wx1, &wy1 );
	sprintf( string, "%d", k);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy1);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if (strncmp(argv[2], "slocator", l) == 0) {
	int k;
	k = slocator( &wx1, &wy1 );
	sprintf( string, "%d", k);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy1);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if (strncmp(argv[2], "key", l) == 0) {
	int k;
	k = getkey();
	sprintf( interp->result, "%d", k);
	return TCL_OK;

  } else {
	Tcl_AppendResult(interp, "bad option \"",argv[2],"\" should be ","\"",
	    "device, depth, numchars, charsize, fontsize, spos2, pos2, pos3, ",
	    "aspect, factors, display, locator, slocator, key",
		"\"",(char *) NULL);
	return TCL_ERROR;
  }
}

/*
 *  Provide a curve routine
 *
 */
static int
vogleCurve(interp, argc, argv)
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Enquire Options. */
{
  int i, j, k, l;
  double d;

  l = strlen( argv[2] );
  if (strncmp(argv[2], "basis", l) == 0) {
    float basis[4][4];
    k = 2 ;
    for (i = 0 ; i < 4 ; i++ ) {
	for (j = 0 ; j < 4 ; j++ ) {
	    basis[i][j] = 0.0 ; k++ ;
	    if (k < argc) {
		if (Tcl_GetDouble(interp, argv[k], &d) != TCL_OK) {
	           return TCL_ERROR;
		}
		basis[i][j] = d;
	    }
	}
    }
    curvebasis( basis ) ;
    return TCL_OK;

  } else if ( (strncmp(argv[2], "precision", l) == 0) ) {
    k = 3;
    if ( k < argc) {
	if (Tcl_GetInt(interp, argv[k], &curvePrecision) != TCL_OK) {
	    return TCL_ERROR;
	}
	curveprecision( curvePrecision );
    }
    return TCL_OK;

  } else if (strncmp(argv[2], "rational", l) == 0) {
    float geom[4][4];
    k = 2 ;
    for (i = 0 ; i < 4 ; i++ ) {
	for (j = 0 ; j < 4 ; j++ ) {
	    geom[i][j] = 0.0 ; k++ ;
	    if (k < argc) {
		if (Tcl_GetDouble(interp, argv[k], &d) != TCL_OK) {
	           return TCL_ERROR;
		}
		geom[i][j] = d;
	    }
	}
    }
    rcurve( geom ) ;
    return TCL_OK;

  } else if (strncmp(argv[2], "curve", l) == 0) {
    float geom[4][3];
    k = 2 ;
    for (i = 0 ; i < 4 ; i++ ) {
	for (j = 0 ; j < 3 ; j++ ) {
	    geom[i][j] = 0.0 ; k++ ;
	    if (k < argc) {
		if (Tcl_GetDouble(interp, argv[k], &d) != TCL_OK) {
	           return TCL_ERROR;
		}
		geom[i][j] = d;
	    }
	}
    }
    curve( geom ) ;
    return TCL_OK;
  }
}


/*
 *  Provide a patch routine
 *
 */
static int
voglePatch(interp, argc, argv)
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Enquire Options. */
{
  int i, j, k, l;
  double d;

  l = strlen( argv[2] );
  if (strncmp(argv[2], "basis", l) == 0) {
    float tbasis[4][4], ubasis[4][4];
    k = 2 ;
    for (i = 0 ; i < 4 ; i++ ) {
	for (j = 0 ; j < 4 ; j++ ) {
	    tbasis[i][j] = 0.0 ; k++ ;
	    if (k < argc) {
		if (Tcl_GetDouble(interp, argv[k], &d) != TCL_OK) {
	           return TCL_ERROR;
		}
		tbasis[i][j] = d;
	    }
	}
    }
    for (i = 0 ; i < 4 ; i++ ) {
	for (j = 0 ; j < 4 ; j++ ) {
	    ubasis[i][j] = 0.0 ; k++ ;
	    if (k < argc) {
		if (Tcl_GetDouble(interp, argv[k], &d) != TCL_OK) {
	           return TCL_ERROR;
		}
		ubasis[i][j] = d;
	    }
	}
    }
    patchbasis( tbasis, ubasis ) ;
    return TCL_OK;

  } else if ( (strncmp(argv[2], "precision", l) == 0) ) {
    k = 3;
    if ( argc != 5) {
	if (Tcl_GetInt(interp, argv[k], &patchPrecision_tseg) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (Tcl_GetInt(interp, argv[k], &patchPrecision_useg) != TCL_OK) {
	    return TCL_ERROR;
	}
	patchprecision( patchPrecision_tseg, patchPrecision_useg );
    }
    return TCL_OK;

  } else if ( (strncmp(argv[2], "curves", l) == 0) ) {
    int nt, nu;
    k = 3;
    if ( argc == 5) {
	if (Tcl_GetInt(interp, argv[k], &nt) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (Tcl_GetInt(interp, argv[k], &nu) != TCL_OK) {
	    return TCL_ERROR;
	}
	patchcurves( nt, nu );
    }
    return TCL_OK;

  } else  {
    float gx[4][4], gy[4][4], gz[4][4], gw[4][4];
    k = 2 ;
    for (i = 0 ; i < 4 ; i++ ) {
	for (j = 0 ; j < 4 ; j++ ) {
	    gx[i][j] = 0.0 ; k++ ;
	    if (k < argc) {
		if (Tcl_GetDouble(interp, argv[k], &d) != TCL_OK) {
	           return TCL_ERROR;
		}
		gx[i][j] = d;
	    }
	}
    }
    for (i = 0 ; i < 4 ; i++ ) {
	for (j = 0 ; j < 4 ; j++ ) {
	    gy[i][j] = 0.0 ; k++ ;
	    if (k < argc) {
		if (Tcl_GetDouble(interp, argv[k], &d) != TCL_OK) {
	           return TCL_ERROR;
		}
		gy[i][j] = d;
	    }
	}
    }
    for (i = 0 ; i < 4 ; i++ ) {
	for (j = 0 ; j < 4 ; j++ ) {
	    gz[i][j] = 0.0 ; k++ ;
	    if (k < argc) {
		if (Tcl_GetDouble(interp, argv[k], &d) != TCL_OK) {
	           return TCL_ERROR;
		}
		gz[i][j] = d;
	    }
	}
    }
    if (strncmp(argv[2], "rational", l) == 0) {
      for (i = 0 ; i < 4 ; i++ ) {
	for (j = 0 ; j < 4 ; j++ ) {
	    gw[i][j] = 0.0 ; k++ ;
	    if (k < argc) {
		if (Tcl_GetDouble(interp, argv[k], &d) != TCL_OK) {
	           return TCL_ERROR;
		}
		gw[i][j] = d;
	    }
	}
      }
      rpatch( gx, gy, gz, gw ) ;
    } else if (strncmp(argv[2], "rational", l) == 0) {
      patch( gx, gy, gz, gw ) ;
    }
    return TCL_OK;
  }
}


/*
 *  Parse positional data
 *
 */
static int
parsePosition(interp, start, argc, argv, x, y, z)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
    float *x, *y, *z;			/* Position coordinates */
{
  int i, l;
  double d;

  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strncmp(argv[i], "-dimension", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &dim) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}
    } else if (strncmp(argv[i], "-3d", l) == 0) {
	dim = 3;
    } else if (strncmp(argv[i], "-2d", l) == 0) {
	dim = 2;
    } else {
	if ( (i+dim) <= argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  *x = d; i++;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  *y = d; i++;
	  if (dim == 3) {
	    if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	      return TCL_ERROR;
	    }
	  *z = d;
	  }
	  return TCL_OK;
	} else {
	  Tcl_AppendResult(interp, "Insufficient data",(char *) NULL);
		return TCL_ERROR;
	}  
    }
  }
  return TCL_OK;
}

/*
 *  Parse style options
 *
 */
static int
parseStyle(interp, start, argc, argv)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
  int i, l, j;
  float x, y;
  double d;
  if (start >= argc)
	return TCL_OK;

  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strncmp(argv[i], "-dash", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  x = d ; setdash( x );
	}
    } else if (strncmp(argv[i], "-style", l) == 0) {
	i++;
	if ( i < argc) {
	   linestyle( argv[i] ) ;
        }
    } else if (strncmp(argv[i], "-fill", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  polyfill( j );
	}
    } else if (strncmp(argv[i], "-hatching", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  polyhatch( j );
	}
    } else if (strncmp(argv[i], "-angle", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  x = d*3.14159265/180.0 ;
	  hatchang( x );
	}
    } else if (strncmp(argv[i], "-pitch", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  x = d;
	  hatchpitch( x );
	}
    } else if (strncmp(argv[i], "-binned", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &type) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}
    } else if (
		(strncmp(argv[i], "-colour", l) == 0) ||
		(strncmp(argv[i], "-color", l) == 0)) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  color( j );
	}
    } else if ( (strncmp(argv[i], "-background", l) == 0) ) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  color( j );
	}
    } else if ( (strncmp(argv[i], "-foreground", l) == 0) ) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  color( j );
	}
    } else if ( (strncmp(argv[i], "-circleprecision", l) == 0) ) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &circlePrecision) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  circleprecision( circlePrecision );
	}
    }
  }
  return TCL_OK;
}

/*
 *  Parse text options
 *
 */
static int
parseText(interp, start, argc, argv)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
  int i, l, j;
  float x, y;
  double d;
  if (start >= argc)
	return TCL_OK;

  pushattributes();
  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strncmp(argv[i], "-size", l) == 0) {
	i++;
	if ( (i+1) < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  x = d;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  y = d;
	  textsize( x, y );
	}
    } else if (
		(strncmp(argv[i], "-colour", l) == 0) ||
		(strncmp(argv[i], "-color", l) == 0)) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  color( j );
	}
    } else if (strncmp(argv[i], "-font", l) == 0) {
	i++;
	if ( i < argc) {
	   font( argv[i] ) ;
	}
    } else if (strncmp(argv[i], "-fixedwidth", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  fixedwidth( j );
	}
    } else if (strncmp(argv[i], "-centred", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  centertext( j );
	}
    } else if (strncmp(argv[i], "-justify", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &j) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  textjustify( j );
	}
    } else if (strncmp(argv[i], "-leftjustify", l) == 0) {
	leftjustify();
    } else if (strncmp(argv[i], "-rightjustify", l) == 0) {
	rightjustify();
    } else if (strncmp(argv[i], "-topjustify", l) == 0) {
	topjustify();
    } else if (strncmp(argv[i], "-bottomjustify", l) == 0) {
	bottomjustify();
    } else if (strncmp(argv[i], "-xcentred", l) == 0) {
	xcentertext();
    } else if (strncmp(argv[i], "-ycentred", l) == 0) {
	ycentertext();
    } else if (strncmp(argv[i], "-angle", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  x = d ; textang( x );
	}
    }
  }
  return TCL_OK;
}


/*
 * Bring style up to the current setting
 *
 */
void 
vogleSetStyle()
{
   float x;
	setdash( lineDash );
	linestyle( lineStyle ) ;
	font( charFont ) ;
	hatchang( polyAngle );
	hatchpitch( polyPitch );
	textsize( charHeight, charWidth );
	color( colour );
}

/* 
 * Vogle command
 *
 */
int
vogleCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    char string[128];
    int length;
    register char *p, c;
    int match;
    int first;
    int left = 0, right = 0;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" option arg ?arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);

    if ( ((c == 'b') && (strncmp(argv[1], "buffer", length) == 0)) ||
         ((c == 'f') && (strncmp(argv[1], "flush", length) == 0))  ) {
        int buffer;
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    argv[1]," true/false\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetBoolean(interp, argv[2], &buffer) != TCL_OK) {
	    return TCL_ERROR;
	}
	vsetflush( buffer );
	if (buffer)
	  vflush();
	return TCL_OK;


    } else if ((c == 'u') && (strncmp(argv[1], "update", length) == 0)) {
	if (argc != 2) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " paper\"", (char *) NULL);
	    return TCL_ERROR;
	}
	vflush();
	return TCL_OK;

    } else if ( ((c == 'p') && (strncmp(argv[1], "paper", length) == 0)) || 
		((c == 's') && (strncmp(argv[1], "size", length) == 0)) ) {
	int x, y;
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " paper/size width height\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetInt(interp, argv[2], &x) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetInt(interp, argv[3], &y) != TCL_OK) {
	    return TCL_ERROR;
	}
	prefsize( x, y );
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "position", length) == 0)) {
	int x, y;
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " position x y\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetInt(interp, argv[2], &x) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetInt(interp, argv[3], &y) != TCL_OK) {
	    return TCL_ERROR;
	}
	prefposition( x, y );
	return TCL_OK;

    } else if ( ((c == 'c') && (strncmp(argv[1], "clear", length) == 0)) ||
		((c == 'p') && (strncmp(argv[1], "page", length) == 0))) {
	color( BLACK );
	clear();
	vogleSetStyle;
	return TCL_OK;

    } else if ((c == 'd') && (strncmp(argv[1], "dimension", length) == 0)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " dimension dim\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetInt(interp, argv[2], &dimension) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (dimension <= 2 ) {
	   dimension = 2;
	} else {
	   dimension = 3;
	}
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "stream", length) == 0)) {
	int s, id, nx = 1, ny = 1;
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " stream option ?args?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (strncmp(argv[2], "close", length) == 0) {
	    vexit();
	} else if (strncmp(argv[2], "open", length) == 0) {
	    if (argc < 4) {
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " stream open device\"", (char *) NULL);
	    	return TCL_ERROR;
	    }
	    vinit( argv[3] );
	    color( BLACK ) ; clear() ; vogleSetStyle();
	} else if (strncmp(argv[2], "pixmap", length) == 0) {
	} else {
	    Tcl_AppendResult(interp, "Unkown option ",argv[2]," to ",
		argv[0]," stream", (char *) NULL);
	    return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 'w') && (strncmp(argv[1], "window", length) == 0)) {
	double dx1, dx2, dy1, dy2, dz1, dz2;
	float x1, x2, y1, y2, z1, z2;
	if (argc < 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " window x1, x2, y1, y2 ?z1? ?z2?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dx2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dy1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[5], &dy2) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (argc == 8) {
          if (Tcl_GetDouble(interp, argv[6], &dz1) != TCL_OK) {
	    return TCL_ERROR;
	  }
          if (Tcl_GetDouble(interp, argv[7], &dz2) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  dimension = 3;
	} else {
	  dz1 = -1.5 ; dz2 = 1.5 ;
	  dimension = 2;
	}
	x1 = dx1 ; x2 = dx2;
	y1 = dy1 ; y2 = dy2;
	z1 = dz1 ; z2 = dz2;
	window( x1, x2, y1, y2, z1, z2 );
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "perspective", length) == 0)) {
	double dx1, dx2, dy1, dy2;
	float x1, x2, y1, y2;
	if (argc != 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " perspective fov aspect near far\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dx2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dy1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[5], &dy2) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx1 ; x2 = dx2;
	y1 = dy1 ; y2 = dy2;
	perspective( x1, x2, y1, y2 );
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "polarview", length) == 0)) {
	double dx1, dx2, dy1, dy2;
	float x1, x2, y1, y2;
	if (argc != 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " polarview distance azimuth incidence twist\"",
		     (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dx2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dy1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[5], &dy2) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx1 ; x2 = dx2;
	y1 = dy1 ; y2 = dy2;
	polarview( x1, x2, y1, y2 );
	return TCL_OK;

    } else if ((c == 'u') && (strncmp(argv[1], "up", length) == 0)) {
	double dx, dy, dz;
	float x, y, z;
	if (argc != 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " up x y z\"",
		     (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dy) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dz) != TCL_OK) {
	    return TCL_ERROR;
	}
	x = dx ; y = dy ; z = dz ;
	up( x, y, z );
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "lookat", length) == 0)) {
	double dx1, dx2, dy1, dy2, dz1, dz2, d;
	float x1, x2, y1, y2, z1, z2, t;
	if (argc != 9) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " lookat vx vy vz px py pz twist\"",
		     (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dy1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dz1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[5], &dx2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[6], &dy2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[7], &dz2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[8], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx1 ; x2 = dx2;
	y1 = dy1 ; y2 = dy2;
	z1 = dz1 ; y2 = dz2;
	t = d;
	lookat( x1, y1, z1, x2, y2, z2, t );
	return TCL_OK;

    } else if ((c == 'o') && (strncmp(argv[1], "orthogonal", length) == 0)) {
	double dx1, dx2, dy1, dy2, dz1, dz2;
	float x1, x2, y1, y2, z1, z2;
	if (argc < 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " orthogonal x1, x2, y1, y2 ?z1? ?z2?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dx2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dy1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[5], &dy2) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx1 ; x2 = dx2;
	y1 = dy1 ; y2 = dy2;
	if (argc == 8) {
          if (Tcl_GetDouble(interp, argv[6], &dz1) != TCL_OK) {
	    return TCL_ERROR;
	  }
          if (Tcl_GetDouble(interp, argv[7], &dz2) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  dimension = 3;
	  z1 = dz1 ; z2 = dz2;
	  ortho( x1, x2, y1, y2, z1, z2);
	} else {
	  dimension = 2;
	  ortho2( x1, x2, y1, y2);
	}
	return TCL_OK;

    } else if ((c == 'v') && (
	(strncmp(argv[1], "viewport", length) == 0) ||
	(strncmp(argv[1], "vport", length) == 0))) {
	double dx1, dx2, dy1, dy2;
	float x1, x2, y1, y2;
	if (argc != 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " viewport x1, x2, y1, y2\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dx2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dy1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[5], &dy2) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx1 ; x2 = dx2;
	y1 = dy1 ; y2 = dy2;
	viewport( &x1, &x2, &y1, &y2 );
	return TCL_OK;

    } else if ( ((c == 's') && (strncmp(argv[1], "style", length) == 0)) ||
		((c == 's') && (strncmp(argv[1], "set", length) == 0)) ){
        if (vogleStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 'e') && (strncmp(argv[1], "enquire", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " enquire option ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (vogleEnquire(interp, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 'm') && (strncmp(argv[1], "move", length) == 0)) {
	float  x, y, z;
	dim = dimension ;
	if (parsePosition( interp, 2, argc, argv, &x, &y, &z ) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (dim == 2) {
	   move2( x, y);
	} else {
	   move(x, y, z);
	}
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "smove", length) == 0)) {
	float  x, y, z;
	dim = dimension ;
	if (parsePosition( interp, 2, argc, argv, &x, &y, &z ) != TCL_OK) {
	    return TCL_ERROR;
	}
	smove2( x, y );
	return TCL_OK;

    } else if ((c == 'r') && (strncmp(argv[1], "rmove", length) == 0)) {
	float  x, y, z;
	dim = dimension ;
	if (parsePosition( interp, 2, argc, argv, &x, &y, &z ) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (dim == 2) {
	   rmove2( x, y);
	} else {
	   rmove(x, y, z);
	}
	return TCL_OK;

    } else if ((c == 'r') && (strncmp(argv[1], "rsmove", length) == 0)) {
	float  x, y, z;
	dim = dimension ;
	if (parsePosition( interp, 2, argc, argv, &x, &y, &z ) != TCL_OK) {
	    return TCL_ERROR;
	}
	rsmove2( x, y );
	return TCL_OK;

    } else if ((c == 'd') && (strncmp(argv[1], "draw", length) == 0)) {
	float  x, y, z;
	dim = dimension ;
	if (parsePosition( interp, 2, argc, argv, &x, &y, &z ) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (dim == 2) {
	   draw2( x, y);
	} else {
	   draw(x, y, z);
	}
	vogleSetStyle( );
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "sdraw", length) == 0)) {
	float  x, y, z;
	dim = dimension ;
	if (parsePosition( interp, 2, argc, argv, &x, &y, &z ) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	sdraw2( x, y);
	vogleSetStyle( );
	return TCL_OK;

    } else if ((c == 'r') && (strncmp(argv[1], "rsdraw", length) == 0)) {
	float  x, y, z;
	dim = dimension ;
	if (parsePosition( interp, 2, argc, argv, &x, &y, &z ) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	rsdraw2( x, y);
	vogleSetStyle( );
	return TCL_OK;

    } else if ((c == 'r') && (strncmp(argv[1], "rdraw", length) == 0)) {
	float  x, y, z;
	dim = dimension ;
	if (parsePosition( interp, 2, argc, argv, &x, &y, &z ) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (dim == 2) {
	   rdraw2( x, y);
	} else {
	   rdraw(x, y, z);
	}
	vogleSetStyle( );
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "line", length) == 0)) {
	float  x1, y1, z1;
	float  x2, y2, z2;
	dim = dimension ;

	if (parsePosition( interp, 2, argc, argv, &x1, &y1, &z1 ) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (parsePosition( interp, 2, argc, argv, &x2, &y2, &z2 ) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (dim == 2) {
	   move2(x1, y1) ; draw2(x2, y2);
	} else {
	   move(x1, y1, z1) ; draw(x2, y2, z2);
	}
	vogleSetStyle( );
	return TCL_OK;


    } else if ((c == 'r') && (strncmp(argv[1], "rectangle", length) == 0)) {
	float  x1, y1;
	float  x2, y2;
	double d;
	if (argc < 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " rectangle x1 y1 x2 y2 ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = d;
        if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	y1 = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	x2 = d;
        if (Tcl_GetDouble(interp, argv[5], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	y2 = d;
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	rect( x1, y1, x2, y2);
	vogleSetStyle( );
	return TCL_OK;

    } else if ((c == 'c') && (strncmp(argv[1], "cube", length) == 0)) {
	float  x, y, z, side;
	double d;
	if (argc < 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " cube x y z side ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	x = d;
        if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	y = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	z = d;
        if (Tcl_GetDouble(interp, argv[5], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	side = d/2.0;
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	pushmatrix();
	   translate( x, y, z );
	   makepoly();
		move( -side, -side, side );
		draw( side, -side, side ); draw( side, side, side );
		draw( -side, side, side ); draw( -side, -side, side );
	   closepoly();
	   makepoly();
		move( side, -side, -side );
		draw( -side, -side, -side ); draw( -side, side, -side );
		draw( side, side, -side ); draw( side, -side, -side );
	   closepoly();
	   makepoly();
		move( side, -side, side );
		draw( side, -side, -side ); draw( side, side, -side );
		draw( side, side, side ); draw( side, -side, side );
	   closepoly();
	   makepoly();
		move( -side, -side, side );
		draw( -side, side, side ); draw( -side, side, -side );
		draw( -side, -side, -side ); draw( -side, -side, side );
	   closepoly();
	   makepoly();
		move( side, side, side );
		draw( side, side, -side ); draw( -side, side, -side );
		draw( -side, side, side ); draw( side, side, side );
	   closepoly();
	   makepoly();
		move( -side, -side, side );
		draw( -side, -side, -side ); draw( side, -side, -side );
		draw( side, -side, side ); draw( -side, -side, side );
	   closepoly();
	popmatrix();
	vogleSetStyle( );
	return TCL_OK;

    } else if ((c == 'c') && (strncmp(argv[1], "cloud", length) == 0)) {
	int ih, i;
	float vmin, vmax, x, y, z;
	double d;
	img_defn defn;
	img_vect  vec;

	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " cloud imId val1 val2 ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (ic_parseImid(interp, argv[2], &ih) != TCL_OK) {
	   return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	vmin = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	vmax = d;
        if (parseStyle(interp, 5, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	defn = ic_hash[ih].defn;
	x=(defn.x2-defn.x1)/2;
	y=(defn.y2-defn.y1)/2;
	z=(defn.z2-defn.z1)/2;
        for (i=0; i<defn.ndata; i++) {
	   if ( (defn.data_p[i]>=vmin) && (defn.data_p[i]<=vmax) ) {
		img_Index2Vec( defn, i, &vec );
		point( (float)vec.x-x, (float)vec.y-y, (float)vec.z-z );
	   }
	}
	vogleSetStyle( );
	return TCL_OK;

    } else if ((c == 'c') && (strncmp(argv[1], "curve", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " curve option ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (vogleCurve(interp, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "patch", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " curve option ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (voglePatch(interp, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 'c') && (strncmp(argv[1], "circle", length) == 0)) {
	float  x, y, r;
	double d;
	dim = dimension ;
	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " circle x y radius ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	x = d;
        if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	y = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	r = d;
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	circle( x, y, r);
	vogleSetStyle( );
	return TCL_OK;

    } else if ((c == 'a') && (strncmp(argv[1], "arc", length) == 0)) {
	float  x, y, r, sa, ea;
	double d;
	dim = dimension ;
	if (argc < 7) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " arc x y radius atsrangle endangle ?options?\"", 
		    (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	x = d;
        if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	y = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	r = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	sa = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	ea = d;
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	arc( x, y, r, sa, ea);
	vogleSetStyle( );
	return TCL_OK;
    } else if ((c == 's') && (strncmp(argv[1], "sector", length) == 0)) {
	float  x, y, r, sa, ea;
	double d;
	dim = dimension ;
	if (argc < 7) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " sector x y radius atsrangle endangle ?options?\"", 
		    (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	x = d;
        if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	y = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	r = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	sa = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	ea = d;
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	sector( x, y, r, sa, ea);
	vogleSetStyle( );
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "polygon", length) == 0)) {
	double d;
	int n, i, j;
	float xyz[100][3];
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " polygon n xi yi zi ...\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetInt(interp, argv[2], &n) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ( (2+3*n) >= argc ) {
	    Tcl_AppendResult(interp, "Insufficient data", 
		(char *) NULL);
	    return TCL_ERROR;
	}
	j = 2 ;
	for ( i = 0 ; i < n ; i++ ){
	   j++;
           if (Tcl_GetDouble(interp, argv[j], &d) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   xyz[i][0] = d; j++ ;
           if (Tcl_GetDouble(interp, argv[j], &d) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   xyz[i][1] = d; j++ ;
           if (Tcl_GetDouble(interp, argv[j], &d) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   xyz[i][2] = d;
	}
        if (parseStyle(interp, j+1, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	poly( n, xyz );
	vogleSetStyle( );
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "poly2d", length) == 0)) {
	double d;
	int n, i, j;
	float xy[100][2];
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " polygon n xi yi ...\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetInt(interp, argv[2], &n) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ( (2+2*n) >= argc ) {
	    Tcl_AppendResult(interp, "Insufficient data", 
		(char *) NULL);
	    return TCL_ERROR;
	}
	j = 2 ;
	for ( i = 0 ; i < n ; i++ ) {
	   j++;
           if (Tcl_GetDouble(interp, argv[j], &d) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   xy[i][0] = d; j++ ;
           if (Tcl_GetDouble(interp, argv[j], &d) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   xy[i][1] = d;
	}
        if (parseStyle(interp, j+1, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	for ( i = 0 ; i < n ; i++ ) {
	  printf( " %d :  %f    %f  \n", i, xy[i][0], xy[i][1] );
	}
	poly2( n, xy );
	vogleSetStyle( );
	return TCL_OK;


    } else if ((c == 'c') && (strncmp(argv[1], "character", length) == 0)) {
	char dc;
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " character char ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (parseText(interp, 3, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	dc = argv[2][0];
	drawchar( argv[2] );
	popattributes;
	return TCL_OK;

    } else if ((c == 't') && (strncmp(argv[1], "text", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " text string ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (parseText(interp, 3, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	drawstr( argv[2] );
	popattributes;
	return TCL_OK;

    } else if ((c == 'b') && (strncmp(argv[1], "boxtext", length) == 0)) {
	float x, y, w, h;
	double d;
	if (argc < 7) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " text string ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	x = d;
	if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	y = d;
	if (Tcl_GetDouble(interp, argv[5], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	w = d;
	if (Tcl_GetDouble(interp, argv[6], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	h = d;
	if (parseText(interp, 7, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	boxtext( x, y, w, h, argv[2] );
	return TCL_OK;

    } else if ((c == 'm') && (strncmp(argv[1], "makepoly", length) == 0)) {
	makepoly();
	return TCL_OK;

    } else if ((c == 'c') && (strncmp(argv[1], "closepoly", length) == 0)) {
	closepoly();
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "pushmatrix", length) == 0)) {
	pushmatrix();
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "popmatrix", length) == 0)) {
	popmatrix();
	return TCL_OK;

    } else if ((c == 't') && (strncmp(argv[1], "translate", length) == 0)) {
	double d;
	float x, y, z;
	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " translate x y z\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	x = d;
        if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	y = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	z = d;
	translate( x, y, z);
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "scale", length) == 0)) {
	double d;
	float x, y, z;
	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " scale x y z\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	x = d;
        if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	y = d;
        if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	z = d;
	scale( x, y, z);
	return TCL_OK;

    } else if ((c == 'r') && (strncmp(argv[1], "rotate", length) == 0)) {
	double d;
	float angle;
	char c;
	if (argc < 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " rotate axis angle\"", (char *) NULL);
	    return TCL_ERROR;
	}
	c = argv[2][0] ;
	if ( (c == 'x') || (c == 'y') || (c == 'z') ) {
           if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   angle = d ;
	   rotate( angle, c );
	   return TCL_OK;
	} else {
	    Tcl_AppendResult(interp, "illegal axis: ", argv[2],
		    " should be \" x | y | z\"", (char *) NULL);
	    return TCL_ERROR;
	}

    } else if ((c == 'o') && (strncmp(argv[1], "object", length) == 0)) {
	int l, id, res;
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " option ?args?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	l = strlen( argv[2] );
	if (strncmp(argv[2], "open", l) == 0) {
	    if (argc < 4) {
	        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " open object\"", (char *) NULL);
	        return TCL_ERROR;
	   }
           if (Tcl_GetInt(interp, argv[3], &id) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   makeobj( id );
	   return TCL_OK;
	} else if (strncmp(argv[2], "close", l) == 0) {
	   closeobj();
	   return TCL_OK;
	} else if (strncmp(argv[2], "exists", l) == 0) {
	    if (argc < 4) {
	        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " exists object\"", (char *) NULL);
	        return TCL_ERROR;
	   }
           if (Tcl_GetInt(interp, argv[3], &id) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   res = isobj( id );
	   sprintf( interp->result, "%d", res);
	   return TCL_OK;
	} else if ( (strncmp(argv[2], "draw", l) == 0) ||
		    (strncmp(argv[2], "plot", l) == 0) ) {
	    if (argc < 4) {
	        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " draw object\"", (char *) NULL);
	        return TCL_ERROR;
	   }
           if (Tcl_GetInt(interp, argv[3], &id) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   callobj( id );
	   return TCL_OK;
	} else if ( (strncmp(argv[2], "delete", l) == 0) ||
		    (strncmp(argv[2], "remove", l) == 0) ) {
	    if (argc < 4) {
	        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " delete object\"", (char *) NULL);
	        return TCL_ERROR;
	   }
           if (Tcl_GetInt(interp, argv[3], &id) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   delobj( id );
	   return TCL_OK;
	} else if (strncmp(argv[2], "next", l) == 0) {
	   id = genobj();
	   sprintf( interp->result, "%d", id);
	   return TCL_OK;
	} else if (strncmp(argv[2], "current", l) == 0) {
	   id = getopenobj();
	   sprintf( interp->result, "%d", id);
	   return TCL_OK;
	} else if (strncmp(argv[2], "save", l) == 0) {
	    if (argc < 5) {
	        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " save object file\"", (char *) NULL);
	        return TCL_ERROR;
	   }
           if (Tcl_GetInt(interp, argv[3], &id) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   saveobj( id, argv[4] );
	   return TCL_OK;
	} else if (strncmp(argv[2], "load", l) == 0) {
	    if (argc < 5) {
	        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " save object file\"", (char *) NULL);
	        return TCL_ERROR;
	   }
           if (Tcl_GetInt(interp, argv[3], &id) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   loadobj( id, argv[4] );
	   return TCL_OK;
	} else {
	  Tcl_AppendResult(interp, "bad option \"",argv[2],"\" should be ","\"",
		"open, close, next, current, save, load,",
		" draw, plot, exists, delete, remove",
		"\"",(char *) NULL);
	    return TCL_ERROR;
	}
    } else if ((c == 'b') && (strncmp(argv[1], "backbuffer", length) == 0)) {
	backbuffer();
	return TCL_OK;

    } else if ((c == 'f') && (strncmp(argv[1], "frontbuffer", length) == 0)) {
	frontbuffer();
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "swapbuffers", length) == 0)) {
	swapbuffers();
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "pushviewport", length) == 0)) {
	pushviewport();
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "popviewport", length) == 0)) {
	popviewport();
	return TCL_OK;

    } else {
	Tcl_AppendResult(interp, "bad option \"",argv[1],"\" should be ","\"",
	    "flush, buffer, bbuf, ebuf, backbuffer, frontbuffer, swapbuffers, ",
	    "stream, size, position, clear, style, enquire, object, update, ",
	    "pushmatrix, popmatrix, translate, scale, rotate, ",
	    "window, orthogonal, viewport, polarview, up, lookat, perspective ",
	    "pushviewport, popviewport ",
		"\"",(char *) NULL);
	return TCL_ERROR;
    }
}
