/*
 *   Tcl binding to the PGPLOT graphics library
 *
 */

#include "tcl.h"
#include "eqn.h"
#include "string.h"
#include "ic.h"
#include "cpgplot.h"

extern ic_hashRec ic_hash[HASH_SIZE];

/* 
 * Define default style parameters
 *
 */
   static int zero = 0, unity = 1, two = 2, three = 3, true = 1, false = 1;
   static int lineStyle = 1;		/* Default line style */
   static int lineType = 0;		/* Default line type */
   static int symbol = 1 ;		/* Default symbol type */
   static int font = 1;			/* Default font */
   static int fill = 1;			/* Default filling style */
   static int width = 1;		/* Default line width */
   static int colour = 1;		/* Default colour index */
   static int arrowFill = 1;		/* Default arrow fill style */
   static float arrowAngle = 45.0;	/* Default arrow-head angle */
   static float arrowVent = 0.3;	/* Default arrow-head vent */
   static float size = 1.0;		/* Default symbol size */
   static float errorTop = 1.0;		/* Default error-bar hat size */
   static float pSize = 0.0;		/* Default page size */
   static float pAspect = 0.0 ;		/* Default page aspect ratio */

/*
 * Global data structures used in all routines
 *
 */
   static int line = 1;			/* Parsed line style */
   static int lli = 1;			/* Line line style */
   static int sli = 1;			/* Symbol line style */
   static int type = 0;			/* Parsed line type */
   static int symb = 0;			/* Parsed symbol type */
   static int fo = 1;			/* Parsed font */
   static int fi = 1;			/* Parsed fill style */
   static int wi = 1;			/* Parsed line width */
   static int lwi = 1;			/* Line line width */
   static int swi = 1;			/* Symbol line width */
   static int co = 1;			/* Parsed colour */
   static int lco = 1;			/* Line colour */
   static int sco = 1;			/* Symbol colour */
   static int aFi = 1;			/* Parsed arrow fill style */
   static float si = 1.0;		/* Parsed symbol size */
   static float aAn = 45.0;		/* Parsed arrow-head angle */
   static float aVe = 0.3;		/* Parsed arrow-head vent */
   static float eTop = 1.0;		/* Parsed error-bar hat size */

 /* data arrays and number of data points */
   static float *x, *y, *ex1, *ex2, *ey1, *ey2 ;
   static int   errx, erry, err2x, err2y;
   static int   ndata;

 /* data structure used to implement colour table management */
   static int ct_type = 3;
   static float ct_vals[18] = {1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
                               0.0, 1.0, 0.2, 1.0, 0.4, 1.0,
                               0.0, 0.0, 0.0, 1.0, 1.0, 1.0};

/*
 *  Sort out style options for all drawing actions
 *
 */
static int
pgplotStyle(interp, start, argc, argv)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
  int i, l;
  double d;
  if (start >= argc)
	return TCL_OK;

  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strncmp(argv[i], "-line", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &lineStyle) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgsls_( &lineStyle );
	}
    } else if (strncmp(argv[i], "-font", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &font) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgscf_( &font );
	}
    } else if (strncmp(argv[i], "-fill", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &fill) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgsfs_( &fill ) ;
	}
    } else if (strncmp(argv[i], "-symbol", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &symbol) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}
    } else if (strncmp(argv[i], "-binned", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &lineType) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}
    } else if (strncmp(argv[i], "-size", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  size = d;
	  pgsch_( &size);
	}
    } else if (
		(strncmp(argv[i], "-colour", l) == 0) ||
		(strncmp(argv[i], "-color", l) == 0)) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &colour) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgsci_( &colour );
	}
    } else if (strncmp(argv[i], "-rgb", l) == 0) {
	int col = 1 ; float r, g, b;
	if ( (i+4) < argc ) {
	  i++;
	  if (Tcl_GetInt(interp, argv[i], &col) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  r = d ; i++;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  g = d ; i++;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  b = d ;
	  pgscr_( &col, &r, &g, &b );
	}
    } else if (strncmp(argv[i], "-hls", l) == 0) {
	int col = 1 ; float h, l, s;
	if ( (i+4) < argc ) {
	  i++;
	  if (Tcl_GetInt(interp, argv[i], &col) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  h = d ; i++;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  l = d ; i++;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  s = d ;
	  pgshls_( &col, &h, &l, &s );
	}
    } else if (strncmp(argv[i], "-width", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &width) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgslw_( &width );
	}
    } else if (strncmp(argv[i], "-arrow", l) == 0) {
	if ( (i+3) < argc ) {
	  i++;
	  if (Tcl_GetInt(interp, argv[i], &arrowFill) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++ ; arrowAngle = d;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  arrowVent = d;
	  pgsah_( &arrowFill, &arrowAngle, &arrowVent );
	}
    } else if (strncmp(argv[i], "-top", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  errorTop = d;
	}
    } else if (strncmp(argv[i], "-save", l) == 0) {
	pgsave_();

    } else if (strncmp(argv[i], "-restore", l) == 0) {
	pgunsa_();
    }
  }
  return TCL_OK;
}

/*
 *  Provide an enquiry routine
 *
 */
static int
pgplotEnquire(interp, argc, argv)
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Enquire Options. */
{
  char string[30];
  int n, l, units;
  float s, wx1, wx2, wy1, wy2;
  double d;

  l = strlen( argv[2] );
  if (strncmp(argv[2], "line", l) == 0) {
	pgqls_( &n );
	sprintf( interp->result, "%d", n);
	return TCL_OK;
  } else if (strncmp(argv[2], "fill", l) == 0) {
	pgqfs_( &n );
	sprintf( interp->result, "%d", n);
	return TCL_OK;
  } else if (strncmp(argv[2], "width", l) == 0) {
	pgqlw_( &n );
	sprintf( interp->result, "%d", n);
	return TCL_OK;
  } else if (strncmp(argv[2], "font", l) == 0) {
	pgqcf_( &n );
	sprintf( interp->result, "%d", n);
	return TCL_OK;
  } else if (strncmp(argv[2], "colour", l) == 0) {
	pgqci_( &n );
	sprintf( interp->result, "%d", n);
	return TCL_OK;
  } else if (strncmp(argv[2], "color", l) == 0) {
	pgqci_( &n );
	sprintf( interp->result, "%d", n);
	return TCL_OK;
  } else if (strncmp(argv[2], "size", l) == 0) {
	pgqch_( &s );
	sprintf( interp->result, "%f", s);
	return TCL_OK;
  } else if (strncmp(argv[2], "top", l) == 0) {
	sprintf( interp->result, "%f", errorTop);
	return TCL_OK;
  } else if (strncmp(argv[2], "binned", l) == 0) {
	sprintf( interp->result, "%d", lineType);
	return TCL_OK;
  } else if (strncmp(argv[2], "window", l) == 0) {
	pgqwin_( &wx1, &wx2, &wy1, &wy2 );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx2);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy2);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if ( (strncmp(argv[2], "viewport", l) == 0) || 
	      (strncmp(argv[2], "vport", l) == 0) ) {
	if (argc < 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" enquire viewport units\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (Tcl_GetInt(interp, argv[3], &units) != TCL_OK) {
	    return TCL_ERROR;
	}
	pgqvp_( &units, &wx1, &wx2, &wy1, &wy2 );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wx2);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy2);
	Tcl_AppendElement( interp, string);
	return TCL_OK;
  } else if (strncmp(argv[2], "range", l) == 0) {
	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" enquire range x1 x2\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	wx1 = d ;
	if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	    return TCL_ERROR;
	}
	wx2 = d ;
	pgrnge_( &wx1, &wx2, &wy1, &wy2 );
	sprintf( string, "%f", wy1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy2);
	Tcl_AppendElement( interp, string);

  } else if (strncmp(argv[2], "cursor", l) == 0) {
	char  c[1];
	if (argc == 5) {
	   if (Tcl_GetDouble(interp, argv[3], &d) != TCL_OK) {
	      return TCL_ERROR;
	   }
	   wx1 = d ;
	   if (Tcl_GetDouble(interp, argv[4], &d) != TCL_OK) {
	      return TCL_ERROR;
	   }
	   wy1 = d ;
	} else {
	   pgqwin_( &wx1, &wx2, &wy1, &wy2 );
	   wx1 = 0.5*(wx2 + wx1) ;
	   wy1 = 0.5*(wy2 + wy1) ;
	}
	pgreadcursor_( &wx1, &wy1, c );
	sprintf( string, "%f", wx1);
	Tcl_AppendElement( interp, string);
	sprintf( string, "%f", wy1);
	Tcl_AppendElement( interp, string);
	Tcl_AppendElement( interp, c);
	return TCL_OK;
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
  int i, l;
  double d;

  line = lineStyle ; type = lineType ; symb = symbol ;
  wi = width ; fo = font ; fi = fill ; co = colour ; si = size ;
  lli = line ; sli = line ; lwi = wi ; swi = wi ;
  aAn = arrowAngle ; aVe = arrowVent ; eTop = errorTop ;
  if (start >= argc)
	return TCL_OK;

  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strncmp(argv[i], "-line", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &line) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  if (line > 0) {
		pgsls_( &line ); lli = line ; sli = line ;
	  }
	}
    } else if (strncmp(argv[i], "-lline", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &lli) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  if (lli > 0) {
		pgsls_( &lli );
	  }
	}
    } else if (strncmp(argv[i], "-sline", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &sli) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  if (sli > 0) {
		pgsls_( &sli );
	  }
	}
    } else if (strncmp(argv[i], "-font", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &fo) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgscf_( &fo );
	}
    } else if (strncmp(argv[i], "-fill", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &fi) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgsfs_( &fi ) ;
	}
    } else if (strncmp(argv[i], "-symbol", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &symb) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}
    } else if (strncmp(argv[i], "-binned", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetBoolean(interp, argv[i], &type) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}
    } else if (strncmp(argv[i], "-size", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  si = d;
	  pgsch_( &si);
	}
    } else if (
		(strncmp(argv[i], "-colour", l) == 0) ||
		(strncmp(argv[i], "-color", l) == 0)) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &co) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgsci_( &co ); lco = co ; sco = co;
	}
    } else if (
		(strncmp(argv[i], "-lcolour", l) == 0) ||
		(strncmp(argv[i], "-lcolor", l) == 0)) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &lco) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgsci_( &lco );
	}
    } else if (
		(strncmp(argv[i], "-scolour", l) == 0) ||
		(strncmp(argv[i], "-scolor", l) == 0)) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &sco) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgsci_( &sco );
	}
    } else if (strncmp(argv[i], "-width", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &wi) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgslw_( &wi ); lwi = wi ; swi = wi;
	}
    } else if (strncmp(argv[i], "-lwidth", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &lwi) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgslw_( &lwi );
	}
    } else if (strncmp(argv[i], "-swidth", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetInt(interp, argv[i], &swi) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  pgslw_( &swi );
	}
    } else if (strncmp(argv[i], "-arrow", l) == 0) {
	if ( (i+3) < argc ) {
	  i++;
	  if (Tcl_GetInt(interp, argv[i], &aFi) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++ ; aAn = d;
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  aVe = d;
	  pgsah_( &aFi, &aAn, &aVe );
	}
    } else if (strncmp(argv[i], "-top", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  eTop = d;
	}
    }
  }
  return TCL_OK;
}

/*
 *  Parse graph options
 *
 */
static int
parseGraph(interp, start, argc, argv, gopt, gscale)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
    int gopt[6];			/* Options. */
    float gscale[6];			/* Scaling parameters. */
{
  int i, l;
  double d;

  gscale[0] = 1.0 ; gscale[1] = 0.0;
  gscale[2] = 1.0 ; gscale[3] = 0.0;
  gopt[0] = 0; gopt[1] = 0; gopt[2] = 0; gopt[3] = 0;
  if (start >= argc)
	return TCL_OK;

  for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strncmp(argv[i], "-xscale", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  gscale[0] = d;
	}
    } else if (strncmp(argv[i], "-xoffset", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  gscale[1] = d;
	}
    } else if (strncmp(argv[i], "-yscale", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  gscale[2] = d;
	}
    } else if (strncmp(argv[i], "-yoffset", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  gscale[3] = d;
	}
    } else if (strncmp(argv[i], "-xlog", l) == 0) {
	gopt[0] = 1;
    } else if (strncmp(argv[i], "-ylog", l) == 0) {
	gopt[1] = 1;
    } else if (strncmp(argv[i], "-xauto", l) == 0) {
	gopt[2] = 1;
    } else if (strncmp(argv[i], "-yauto", l) == 0) {
	gopt[3] = 1;
    }
  }
  return TCL_OK;
}

/*
 *  Parse image options
 *
 */
static int
parseImage(interp, start, argc, argv, tr)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
    float *tr;				/* Array giving transformation matrix */
{
  int i, ii, l;
  double d;

   if (start >= argc)
	return TCL_OK;

/*
 * setup default options
 */
   cpgsitf( 0 );

   for ( i = start ; i < argc ; i++) {
    l = strlen(argv[i]);
    if (strncmp(argv[i], "-linear", l) == 0) {
	cpgsitf( 0 );
    } else if (strncmp(argv[i], "-log", l) == 0) {
	cpgsitf( 1 );
    } else if (strncmp(argv[i], "-sqrt", l) == 0) {
	cpgsitf( 2 );
    } else if (strncmp(argv[i], "-colour", l) == 0) {
	ct_type = 1;
    } else if (strncmp(argv[i], "-standard", l) == 0) {
	ct_type = 2;
    } else if (strncmp(argv[i], "-grey", l) == 0) {
	ct_type = 3;
    } else if (strncmp(argv[i], "-egrey", l) == 0) {
	ct_type = 4;
    } else if (strncmp(argv[i], "-invgrey", l) == 0) {
	ct_type = 5;
    } else if (strncmp(argv[i], "-einvgrey", l) == 0) {
	ct_type = 6;
    } else if (strncmp(argv[i], "-ecolour", l) == 0) {
	ct_type = 7;
    } else if (strncmp(argv[i], "-power", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[0] = d;
	}
    } else if (strncmp(argv[i], "-lower", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[1] = d;
	}
    } else if (strncmp(argv[i], "-upper", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[2] = d;
	}
    } else if (strncmp(argv[i], "-rpower", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[3] = d;
	}
    } else if (strncmp(argv[i], "-rlower", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[6] = d;
	}
    } else if (strncmp(argv[i], "-rupper", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[7] = d;
	}
    } else if (strncmp(argv[i], "-gpower", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[4] = d;
	}
    } else if (strncmp(argv[i], "-glower", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[8] = d;
	}
    } else if (strncmp(argv[i], "-gupper", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[9] = d;
	}
    } else if (strncmp(argv[i], "-bpower", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[5] = d;
	}
    } else if (strncmp(argv[i], "-blower", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[10] = d;
	}
    } else if (strncmp(argv[i], "-bupper", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[11] = d;
	}
    } else if (strncmp(argv[i], "-rstart", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[12] = d;
	}
    } else if (strncmp(argv[i], "-gstart", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[13] = d;
	}
    } else if (strncmp(argv[i], "-bstart", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[14] = d;
	}
    } else if (strncmp(argv[i], "-rindex", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[15] = d;
	}
    } else if (strncmp(argv[i], "-gindex", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[16] = d;
	}
    } else if (strncmp(argv[i], "-bindex", l) == 0) {
	i++;
	if ( i < argc) {
	  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  ct_vals[17] = d;
	}
    } else if (strncmp(argv[i], "-tr", l) == 0) {
	i++;
	for (ii=0; ii < 5; ii++) {
	  if (i < argc) {
	     printf( "%s %d %d\n", argv[i], i, ii);
	     if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	        return TCL_ERROR;
	     }
	     tr[ii] = d; i++;
	  }
	}
    }
  }
  pgplotctmod_( &ct_type, ct_vals );
  return TCL_OK;
}

/*
 * Parse command line for data definitions and options
 *
 */
static int
parseData(interp, list )
    Tcl_Interp *interp;			/* Current interpreter. */
    char *list;				/* List to parse. */
{
  char **listArgv;
  int i, l, j, listArgc;
  double d;

 /*
  * split list into array of strings to parse
  */
  if (Tcl_SplitList(interp, list, &listArgc, &listArgv) != TCL_OK) {
	return TCL_ERROR;
  }

 /*
  * Check for special options for errors on X and Y
  *
  */
  errx = 0 ; erry = 0 ; err2x = 0 ; err2y = 0 ; j = 0 ;
  for ( i = 0 ; i < listArgc ; i++) {
    l = strlen(listArgv[i]);
    if (strncmp(listArgv[i], "-errx", l) == 0) {
	errx = 1;
    } else if (strncmp(listArgv[i], "-erry", l) == 0) {
	erry = 1;
    } else if (strncmp(listArgv[i], "-err2x", l) == 0) {
	err2x = 1; errx = 1;
    } else if (strncmp(listArgv[i], "-err2y", l) == 0) {
	err2y = 1; erry = 1;
    } else if (strncmp(listArgv[i], "-values", l) == 0) {
	j = i + 1;
    } else {
	j = i ; break ;
    }
  }

 /*
  * parse this data structure
  */
  if (Tcl_GetInt(interp, listArgv[j], &ndata) != TCL_OK) {
	return TCL_ERROR;
  }
  if ( ndata >= listArgc ) {
	Tcl_AppendResult(interp, "Insufficient data",(char *) NULL);
		return TCL_ERROR;
  }
  x = (float *)calloc( ndata, sizeof(float) );
  y = (float *)calloc( ndata, sizeof(float) );
  ex1 = (float *)calloc( ndata, sizeof(float) );
  ex2 = (float *)calloc( ndata, sizeof(float) );
  ey1 = (float *)calloc( ndata, sizeof(float) );
  ey2 = (float *)calloc( ndata, sizeof(float) );
  for ( i = 0 ; i < ndata ; i++ ){
	j++;
	if (Tcl_GetDouble(interp, listArgv[j], &d) != TCL_OK) {
	    goto error;
	}
	x[i] = d; j++ ;
	if (Tcl_GetDouble(interp, listArgv[j], &d) != TCL_OK) {
	    goto error;
	}
	y[i] = d;
	if (errx) {
	    j++ ;
	    if (Tcl_GetDouble(interp, listArgv[j], &d) != TCL_OK) {
	        goto error;
	    }
	    ex1[i] = d;
	    if (err2x) {
		j++ ;
	        if (Tcl_GetDouble(interp, listArgv[j], &d) != TCL_OK) {
	            goto error;
	        }
		ex2[i] = d;
	    } else {
		ex2[i] = ex1[i] ;
	    }
	}
	if (erry) {
	    j++ ;
	    if (Tcl_GetDouble(interp, listArgv[j], &d) != TCL_OK) {
	        goto error;
	    }
	    ey1[i] = d;
	    if (err2y) {
		j++ ;
	        if (Tcl_GetDouble(interp, listArgv[j], &d) != TCL_OK) {
	            goto error;
	        }
		ey2[i] =  d;
	    } else {
		ey2[i] = ey1[i] ;
	    }
	}
  }
  free((char *) listArgv);
  return TCL_OK;

error:
  free(x) ; free(y); free(ex1); free(ey1); free(ex2) ; free(ey2);
  return TCL_ERROR;

}

/*
 * Parse command line for 1D imId definitions
 *
 */
static int
parseimId(interp, list )
    Tcl_Interp *interp;			/* Current interpreter. */
    char *list;				/* List to parse. */
{
  char **listArgv;
  char file[128];
  int i, l, listArgc;
  int ix, iy, ih;
  img_defn defn;

 /*
  * split list into array of strings to parse
  */
  if (Tcl_SplitList(interp, list, &listArgc, &listArgv) != TCL_OK) {
	return TCL_ERROR;
  }

 /*
  * The first item should be the imId
  *
  */
  if ( listArgc == 0 ) {
    Tcl_AppendResult(interp, "imId not specified", (char *) NULL);
    return TCL_ERROR;
  }
  if (Tcl_GetInt(interp, listArgv[0], &ih) != TCL_OK) {
	return TCL_ERROR;
  }
  if (ic_hashExists( ih ) != IC_OK) {
	return TCL_ERROR;
  }
  defn = ic_hash[ih].defn;

 /*
  * Parse the remaining options, but allow for a specific -file
  * option specifier
  *
  */
  ix = 1 ; iy = 2;
  errx = 0 ; erry = 0 ; err2x = 0 ; err2y = 0;
  for ( i = 1 ; i < listArgc ; i++) {
    l = strlen(listArgv[i]);
    if (strncmp(listArgv[i], "-x", l) == 0) {
	if ( (i+1) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &ix) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
    } else if (strncmp(listArgv[i], "-y", l) == 0) {
	if ( (i+1) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &iy) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
    } else if (strncmp(listArgv[i], "-errx", l) == 0) {
	if ( (i+1) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &errx) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
     } else if (strncmp(listArgv[i], "-erry", l) == 0) {
	if ( (i+1) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &erry) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
    } else if (strncmp(listArgv[i], "-err2x", l) == 0) {
	if ( (i+2) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &errx) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &err2x) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
    } else if (strncmp(listArgv[i], "-err2y", l) == 0) {
	if ( (i+2) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &erry) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &err2y) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}
    }
  }

 /*
  * read this data structure
  */
  ndata = defn.xdim;
  if (ndata > 0) {
     x = (float *)calloc( ndata, sizeof(float) );
     y = (float *)calloc( ndata, sizeof(float) );
     ex1 = (float *)calloc( ndata, sizeof(float) );
     ex2 = (float *)calloc( ndata, sizeof(float) );
     ey1 = (float *)calloc( ndata, sizeof(float) );
     ey2 = (float *)calloc( ndata, sizeof(float) );
  }
  img_getvec1_( &defn, defn.data_p, &ix, x );
  img_getvec1_( &defn, defn.data_p, &iy, y );
  img_getvec1_( &defn, defn.data_p, &errx, ex1 );
  img_getvec1_( &defn, defn.data_p, &err2x, ex2 );
  img_getvec1_( &defn, defn.data_p, &erry, ey1 );
  img_getvec1_( &defn, defn.data_p, &err2y, ey2 );
  free((char *) listArgv);
  return TCL_OK;

error:
  free(x) ; free(y); free(ex1); free(ey1); free(ex2) ; free(ey2);
  return TCL_ERROR;

}

/*
 * Parse command line for function definition and options
 *
 */
static int
parseFunction(interp, list )
    Tcl_Interp *interp;			/* Current interpreter. */
    char *list;				/* List to parse. */
{
  int i;
  float    wx1, wx2, wy1, wy2;
  eqnode   *eqn, *deqn, *subeqn;
  double   dx;

  ndata = 101;
  errx = 0 ; erry = 0 ; err2x = 0 ; err2y = 0;
  x = (float *)calloc( ndata, sizeof(float) );
  y = (float *)calloc( ndata, sizeof(float) );
  ex1 = (float *)calloc( ndata, sizeof(float) );
  ey1 = (float *)calloc( ndata, sizeof(float) );
  ex2 = (float *)calloc( ndata, sizeof(float) );
  ey2 = (float *)calloc( ndata, sizeof(float) );
  pgqwin_( &wx1, &wx2, &wy1, &wy2 );
  x[0] = wx1;
  eqn = sscan_eqn( list );
  eval_funs(eqn);
  for ( i = 0 ; i < ndata ; i++ ) {
	x[i] = wx1 + (float) i * (wx2 - wx1 ) / ( (float)(ndata -1) );
	deqn = duplicate(eqn);
	dx = x[i];
	subeqn = assign( "x", dx );
	substitute(deqn,subeqn);
	eval_funs(deqn);
	y[i] = (float)eqnval( deqn );
  }
  free_eqn_tree( eqn ) ;
  free_eqn_tree( deqn ) ;
  free_eqn_tree( subeqn ) ;
  return TCL_OK;
}

/*
 * Parse command line for spectral definitions and options
 *
 */
static int
parseSpectrum(interp, list )
    Tcl_Interp *interp;			/* Current interpreter. */
    char *list;				/* List to parse. */
{
  char **listArgv;
  char file[128];
  int i, l, j, listArgc;
  int ix, iy, id;
  double d;

 /*
  * split list into array of strings to parse
  */
  if (Tcl_SplitList(interp, list, &listArgc, &listArgv) != TCL_OK) {
	return TCL_ERROR;
  }

 /*
  * The first item should be the file name of the spectrum
  *
  */
  ix = 1 ; iy = 2;
  errx = 0 ; erry = 0 ; err2x = 0 ; err2y = 0 ; j = 0 ;
  if ( listArgc > 0 ) {
    strcpy( file, listArgv[0] );
  } else {
    Tcl_AppendResult(interp, "Filename not specified", (char *) NULL);
    return TCL_ERROR;
  }

 /*
  * Parse the remaining options, but allow for a specific -file
  * option specifier
  *
  */
  for ( i = 0 ; i < listArgc ; i++) {
    l = strlen(listArgv[i]);
    if (strncmp(listArgv[i], "-x", l) == 0) {
	if ( (i+1) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &ix) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
    } else if (strncmp(listArgv[i], "-y", l) == 0) {
	if ( (i+1) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &iy) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
    } else if (strncmp(listArgv[i], "-errx", l) == 0) {
	if ( (i+1) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &errx) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
     } else if (strncmp(listArgv[i], "-erry", l) == 0) {
	if ( (i+1) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &erry) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
    } else if (strncmp(listArgv[i], "-err2x", l) == 0) {
	if ( (i+2) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &errx) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &err2x) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
    } else if (strncmp(listArgv[i], "-err2y", l) == 0) {
	if ( (i+2) < listArgc ) {
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &erry) != TCL_OK) {
	    return TCL_ERROR;
	  }
	  i++;
	  if (Tcl_GetInt(interp, listArgv[i], &err2y) != TCL_OK) {
	    return TCL_ERROR;
	  }
	}       
    } else if (strncmp(listArgv[i], "-file", l) == 0) {
	if ( (i+1) < listArgc ) {
	  i++;
	  strcpy( file, listArgv[i] );
	}       
    }
  }

 /*
  * read this data structure
  */
  pgopenspectrum_( file, &id, &ndata );
  if (ndata > 0) {
     x = (float *)calloc( ndata, sizeof(float) );
     y = (float *)calloc( ndata, sizeof(float) );
     ex1 = (float *)calloc( ndata, sizeof(float) );
     ex2 = (float *)calloc( ndata, sizeof(float) );
     ey1 = (float *)calloc( ndata, sizeof(float) );
     ey2 = (float *)calloc( ndata, sizeof(float) );
  }
  pgreadspectrum_( &id,  &ix, &iy, &errx, &err2x, &erry, &err2y,
                   &ndata, x,   y,   ex1,    ex2,   ey1,    ey2 );
  free((char *) listArgv);
  return TCL_OK;

error:
  free(x) ; free(y); free(ex1); free(ey1); free(ex2) ; free(ey2);
  return TCL_ERROR;

}



/*
 * Bring style up to the current setting
 *
 */
void 
pgplotSetStyle()
{
	pgsls_( &lineStyle );
	pgscf_( &font );
	pgsfs_( &fill );
	pgsch_( &size );
	pgsci_( &colour );
	pgslw_( &width );
	pgsah_( &arrowFill, &arrowAngle, &arrowVent );
}

void 
pgplotSetParseStyle()
{
	pgsls_( &line );
	pgscf_( &fo );
	pgsfs_( &fi );
	pgsch_( &si );
	pgsci_( &co );
	pgslw_( &wi );
	pgsah_( &aFi, &aAn, &aVe );
}

void 
pgplotSetLineStyle()
{
	pgsls_( &lli );
	pgscf_( &fo );
	pgsfs_( &fi );
	pgsch_( &si );
	pgsci_( &lco );
	pgslw_( &lwi );
	pgsah_( &aFi, &aAn, &aVe );
}

void 
pgplotSetSymbStyle()
{
	pgsls_( &sli );
	pgscf_( &fo );
	pgsfs_( &fi );
	pgsch_( &si );
	pgsci_( &sco );
	pgslw_( &swi );
	pgsah_( &aFi, &aAn, &aVe );
}

/*
 *  Parse curve or line options
 *
 */
static int
parseCurve(interp, start, argc, argv)
    Tcl_Interp *interp;			/* Current interpreter. */
    int start;				/* First unparsed argument. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
  int i, l;
  if (start >= argc)
	return TCL_OK;
  for ( i = start ; i < argc ; i++) {
	l = strlen(argv[i]);
	if (strncmp(argv[i], "-data", l) == 0) {
		i++;
		if (i < argc) {
		    if (parseData(interp, argv[i]) != TCL_OK) {
		        return TCL_ERROR;
		    }
		}
	} else if (strncmp(argv[i], "-spectrum", l) == 0) {
		i++;
		if (i < argc) {
		    if (parseSpectrum(interp, argv[i]) != TCL_OK) {
		        return TCL_ERROR;
		    }
		}
	} else if (strncmp(argv[i], "-function", l) == 0) {
		i++;
		if (i < argc) {
		    if (parseFunction(interp, argv[i]) != TCL_OK) {
		        return TCL_ERROR;
		    }
		}
	} else if (strncmp(argv[i], "-id", l) == 0) {
		i++;
		if (i < argc) {
		    if (parseimId(interp, argv[i]) != TCL_OK) {
		        return TCL_ERROR;
		    }
		}
	}
  }
  if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	return TCL_ERROR;
  }
  return TCL_OK;
}


/* 
 * Pgplot command
 *
 */
int
pgplotCmd(dummy, interp, argc, argv)
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

    if ((c == 'b') && (strncmp(argv[1], "buffer", length) == 0)) {
        int buffer;
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " buffer true/false\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetBoolean(interp, argv[2], &buffer) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (buffer) {
	    pgbbuf_();
	} else {
	    pgebuf_();
	}
	return TCL_OK;

    } else if ((c == 'b') && (strncmp(argv[1], "bbuf", length) == 0)) {
	if (argc != 2) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " bbuf\"", (char *) NULL);
	    return TCL_ERROR;
	}
        pgbbuf_();
	return TCL_OK;

    } else if ((c == 'e') && (strncmp(argv[1], "ebuf", length) == 0)) {
	if (argc != 2) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " ebuf\"", (char *) NULL);
	    return TCL_ERROR;
	}
        pgebuf_();
	return TCL_OK;

    } else if ((c == 'u') && (strncmp(argv[1], "update", length) == 0)) {
	if (argc != 2) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " update\"", (char *) NULL);
	    return TCL_ERROR;
	}
        pgupdt_();
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "page", length) == 0)) {
	if (argc != 2) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " page\"", (char *) NULL);
	    return TCL_ERROR;
	}
	pgpage_();
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "paper", length) == 0)) {
        double d1, d2;
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " paper width aspect\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &d1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &d2) != TCL_OK) {
	    return TCL_ERROR;
	}
	pSize = d1/2.5; pAspect = d2;
	cpgpap( pSize, pAspect );
	return TCL_OK;

    } else if ((c == 'b') && (strncmp(argv[1], "begin", length) == 0)) {
        int s, id = 1, nx = 1, ny = 1;
	strcpy( string, argv[2] );
	pginitstream_(string, &pSize, &pAspect, &nx, &ny, &id, &s);
	if ( s != 0 ) {
		Tcl_AppendResult(interp, "Illegal device specification",
		argv[4], (char *) NULL);
		return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "stream", length) == 0)) {
	int s, id, nx = 1, ny = 1;
	if (argc < 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " stream option id ?args?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	/*
	 * Get the id for a IO stream 1 <= id <= 3
	 */
        if (Tcl_GetInt(interp, argv[3], &id) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (id < 1)
	   id = 1;
	if (id > 3)
	   id = 3;
	if (strncmp(argv[2], "close", length) == 0) {
	    pgclose_( &id );
	} else if (strncmp(argv[2], "select", length) == 0) {
	    pgselect_( &id );
	} else if (strncmp(argv[2], "open", length) == 0) {
	    strcpy( string, argv[4] );
	    pginitstream_(string, &pSize, &pAspect, &nx, &ny, &id, &s);
	    if ( s != 0 ) {
		Tcl_AppendResult(interp, "Illegal device specification",
		argv[4], (char *) NULL);
		return TCL_ERROR;
	    }
	} else {
		Tcl_AppendResult(interp, "Unkown option ",argv[2]," to ",
		argv[0]," stream", (char *) NULL);
		return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 'w') && (strncmp(argv[1], "window", length) == 0)) {
	double dx1, dx2, dy1, dy2;
	float x1, x2, y1, y2;
	if (argc != 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " window x1, x2, y1, y2\"", (char *) NULL);
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
	pgwindow_( &x1, &x2, &y1, &y2 );
	return TCL_OK;

    } else if ((c == 'c') && ( strncmp(argv[1], "cursor", length) == 0)) {
	char rch[1];
	int l, i, m, posn;
	double d;
	float x1, x2, y1, y2;
	cpgqpos( &x1, &y1 ); x2 = x1 ; y2 = y1;
	m = 0; posn = 0;
	if (argc > 2) {
	   for (i = 2; i < argc; i++) {
		l = strlen( argv[i] );
		if (strncmp(argv[i], "-xref", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -xref X\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			return TCL_ERROR;
		   }
		   x2 = d;
		} else if (strncmp(argv[i], "-yref", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -yref X\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			return TCL_ERROR;
		   }
		   y2 = d;
		} else if (strncmp(argv[i], "-x", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -x X\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			return TCL_ERROR;
		   }
		   x1 = d;
		} else if (strncmp(argv[i], "-y", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -x X\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			return TCL_ERROR;
		   }
		   y1 = d;
		} else if (strncmp(argv[i], "-mode", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -mode mode\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   if (Tcl_GetInt(interp, argv[i], &m) != TCL_OK) {
			return TCL_ERROR;
		   }
		   if (m < 0) m = 0;
		   if (m > 7) m = 7;
		} else if (strncmp(argv[2], "-position", l) == 0) {
		   posn = 1;
		}
	   }
	}
	if (cpgband( m, posn, x2, y2, &x1, &y1, rch)) {
	   sprintf( string, "%f", x1);
	   Tcl_AppendElement( interp, string);
	   sprintf( string, "%f", y1);
	   Tcl_AppendElement( interp, string);
	   Tcl_AppendElement( interp, rch);
	   return TCL_OK;
	} else {
	   return TCL_ERROR;
	}

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
	pgvport_( &x1, &x2, &y1, &y2 );
	return TCL_OK;

    } else if ( ((c == 's') && (strncmp(argv[1], "style", length) == 0)) ||
		((c == 's') && (strncmp(argv[1], "set", length) == 0)) ) {
        if (pgplotStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 'e') && (strncmp(argv[1], "enquire", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " enquire option ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (pgplotEnquire(interp, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 'c') && (strncmp(argv[1], "clear", length) == 0)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " clear option\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (strncmp(argv[2], "screen", length) == 0) {
	    pgclearscreen_();
	} else {
	    pgclearvport_();
	}
	return TCL_OK;

    } else if ((c == 'm') && (strncmp(argv[1], "move", length) == 0)) {
	double dx, dy;
	float  x1, y1;
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " move x y\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dy) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx ; y1 = dy ; pgmove_( &x1, &y1);
	return TCL_OK;

    } else if ((c == 'd') && (strncmp(argv[1], "draw", length) == 0)) {
	double dx, dy;
	float  x1, y1;
	if (argc < 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " draw x y ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dy) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 4, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx ; y1 = dy ; pgdraw_( &x1, &y1);
	pgplotSetStyle( );
	return TCL_OK;

    } else if ( ((c == 'p') && (strncmp(argv[1], "point", length) == 0)) ||
		((c == 's') && (strncmp(argv[1], "symbol", length) == 0)) ) {
	double dx, dy;
	float  x1, y1;
	symb = symbol;
	if (argc < 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " point x y ?symbol?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dy) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 4, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx ; y1 = dy ; pgpoint_( &unity, &x1, &y1, &symb);
	pgplotSetStyle( );
	return TCL_OK;

    } else if ((c == 'e') && (strncmp(argv[1], "error", length) == 0)) {
	double d;
	float  x1, y1, y2, top;
	int l, i, etype = 1;
	eTop = errorTop;
	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " error x y1 y2 ?options?\"", (char *) NULL);
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
	y2 = d;
	if (argc > 5) {
	  for ( i = 5 ; i < argc ; i++) {
            l = strlen(argv[i]);
	    if (strncmp(argv[i], "-x", l) == 0) {
		etype = 0 ;
	    } else if (strncmp(argv[i], "-y", l) == 0) {
		etype = 1 ;
	    }
	  }
	}
        if (parseStyle(interp, 5, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (etype) {
	   pgerry_( &unity, &x1, &y1, &y2, &eTop );
	} else {
	   pgerrx_( &unity, &y1, &y2, &x, &eTop );
	}
	pgplotSetStyle( );
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "line", length) == 0)) {
	double dx1, dy1, dx2, dy2;
	float  x1, y1;
	if (argc < 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " line x1 y1 x2 y2\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dy1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dx2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[5], &dy2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 6, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx1 ; y1 = dy1 ; pgmove_( &x1, &y1);
	x1 = dx2 ; y1 = dy2 ; pgdraw_( &x1, &y1);
	pgplotSetStyle( );
	return TCL_OK;

    } else if ((c == 'a') && (strncmp(argv[1], "arrow", length) == 0)) {
	double dx1, dy1, dx2, dy2;
	float  x1, y1, x2, y2;
	if (argc < 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " arrow x1 y1 x2 y2\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dy1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dx2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[5], &dy2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 6, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx1 ; y1 = dy1 ;
	x2 = dx2 ; y2 = dy2 ; pgarro_( &x1, &y1, &x2, &y2);
	pgplotSetStyle( );
	return TCL_OK;

    } else if ((c == 'r') && (strncmp(argv[1], "rectangle", length) == 0)) {
	double dx1, dy1, dx2, dy2;
	float  x1, y1, x2, y2;
	if (argc < 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " rectangle x1 y1 x2 y2\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dy1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dx2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[5], &dy2) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 6, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx1 ; y1 = dy1 ;
	x2 = dx2 ; y2 = dy2 ; pgrect_( &x1, &x2, &y1, &y2);
	pgplotSetStyle( );
	return TCL_OK;

    } else if ((c == 'c') && (strncmp(argv[1], "circle", length) == 0)) {
	double dx1, dy1, dr;
	float  x1, y1, r;
	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " rectangle x1 y1 x2 y2\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dy1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &dr) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (parseStyle(interp, 5, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx1 ; y1 = dy1 ; r = dr ; pgcirc_( &x1, &y1, &r);
	pgplotSetStyle( );
	return TCL_OK;

    } else if ((c == 'p') && (strncmp(argv[1], "polygon", length) == 0)) {
	double d;
	int n, i, j;
	float *xp, *yp;
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
	xp = (float *)calloc( n, sizeof(float) );
	yp = (float *)calloc( n, sizeof(float) );
	j = 2 ;
	for ( i = 0 ; i < n ; i++ ){
	   j++;
           if (Tcl_GetDouble(interp, argv[j], &d) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   xp[i] = d; j++ ;
           if (Tcl_GetDouble(interp, argv[j], &d) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   yp[i] = d;
	}
        if (parseStyle(interp, j+1, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	pgpoly_( &n, xp, yp );
	free( xp ) ; free( yp );
	pgplotSetStyle( );
	return TCL_OK;

    } else if ((c == 't') && (strncmp(argv[1], "text", length) == 0)) {
	double dx, dy;
	float x1, y1, r = 0.0, just = 0.0;
	int i, l;
	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " text x1 x2 string ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[2], &dx) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &dy) != TCL_OK) {
	    return TCL_ERROR;
	}
	x1 = dx ; y1 = dy;
	if (argc > 5) {
	  for ( i = 5 ; i < argc ; i++) {
             l = strlen(argv[i]);
	     if (strncmp(argv[i], "-rotation", l) == 0) {
		i++;
		if ( i < argc) {
        	    if (Tcl_GetDouble(interp, argv[i], &dx) != TCL_OK) {
	    		return TCL_ERROR;
		    }
		    r = dx ;
		}
	     }  else if (strncmp(argv[i], "-justify", l) == 0) {
		i++;
		if ( i < argc) {
        	    if (Tcl_GetDouble(interp, argv[i], &dx) != TCL_OK) {
	    		return TCL_ERROR;
		    }
		    just = dx ;
		}
	     }
	  }
	}
        if (parseStyle(interp, 5, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	strcpy( string, argv[4] );
	pgtextstring_( string, &x1, &y1, &r, &just );
	pgplotSetStyle( );
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "label", length) == 0)) {
	double d;
	char *side = "t ";
	float x1=1.0, y1=0.5, just = 0.5;
	int i, l;
	errx = 0 ; erry = 0 ; err2x = 0 ; err2y = 0 ;
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " label string ?options?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (argc > 3) {
	  for ( i = 3 ; i < argc ; i++) {
             l = strlen(argv[i]);
	     if (strncmp(argv[i], "-displacement", l) == 0) {
		i++;
		if ( i < argc) {
        	    if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    		return TCL_ERROR;
		    }
		    x1 = d ;
		}
	     } else if (strncmp(argv[i], "-justify", l) == 0) {
		i++;
		if ( i < argc) {
        	    if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    		return TCL_ERROR;
		    }
		    just = d ;
		}
	     } else if (strncmp(argv[i], "-coordinate", l) == 0) {
		i++;
		if ( i < argc) {
        	    if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
	    		return TCL_ERROR;
		    }
		    y1 = d ;
		}
	     } else if (strncmp(argv[i], "-side", l) == 0) {
		i++;
		if ( i < argc) {
		   strncpy( side, argv[i], 2 );
		} 
	     }
	  }
	}
        if (parseStyle(interp, 3, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	strcpy( string, argv[2] );
	pgtextlabel_( side, string, &x1, &y1, &just );
	pgplotSetStyle( );
	return TCL_OK;

    } else if ((c == 'b') && (strncmp(argv[1], "box", length) == 0)) {
	double d;
	char xopt[10], yopt[10];
	int i, l, nx = 0, ny = 0, btype = 0;
	float xtick = 0.0 , ytick = 0.0 ;
	strcpy( xopt, "BCNTS"); strcpy( yopt, "BCNTS");
        if (parseStyle(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	for ( i = 2 ; i < argc ; i++) {
            l = strlen(argv[i]);
	    if (strncmp(argv[i], "-xdivisions", l) == 0) {
		i++;
		if ( i < argc) {
		  if (Tcl_GetInt(interp, argv[i], &nx) != TCL_OK) {
		    return TCL_ERROR;
		  }
		}
	    } else if (strncmp(argv[i], "-ydivisions", l) == 0) {
		i++;
		if ( i < argc) {
		  if (Tcl_GetInt(interp, argv[i], &ny) != TCL_OK) {
		    return TCL_ERROR;
		  }
		}
	    } else if (strncmp(argv[i], "-xticks", l) == 0) {
		i++;
		if ( i < argc) {
		  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
		    return TCL_ERROR;
		  }
		}
		xtick = d ;
	    } else if (strncmp(argv[i], "-yticks", l) == 0) {
		i++;
		if ( i < argc) {
		  if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
		    return TCL_ERROR;
		  }
		}
		ytick = d ;
	    } else if (strncmp(argv[i], "-xopts", l) == 0) {
		i++;
		if ( i < argc) {
		   strcpy( xopt, argv[i]);
		}
	    } else if (strncmp(argv[i], "-yopts", l) == 0) {
		i++;
		if ( i < argc) {
		   strcpy( yopt, argv[i]);
		}
	    } else if (strncmp(argv[i], "-radec", l) == 0) {
		btype = 1;
	    }
	}
	pgdrawbox_(&btype, xopt, &xtick, &nx, yopt, &ytick, &ny);
	pgplotSetStyle( );
	return TCL_OK;

    } else if ((c == 'c') && (strncmp(argv[1], "curve", length) == 0)) {
	double d;
	int i, l;
	ndata = 0; symb = 0 ; line = lineStyle; 
	type = lineType; eTop = errorTop;
	if (parseCurve(interp, 2, argc, argv) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (line > 0) {
	    pgplotSetLineStyle( );
	    if ( type == 0 ) {
		pgline_( &ndata, x, y );
	    } else if (type == 1)  {
	        pgdrawbinned_( &ndata, x, y );
	    }
	}
	if (symb > 0) {
		pgplotSetSymbStyle( );
		pgpnts_( &ndata, x, y, &symb, &unity);
	}
	pgplotSetLineStyle( );
	pgploterror_( &ndata, &errx, &erry, &eTop,
		      x, y, ex1, ex2, ey1, ey2);
	pgplotSetStyle( );
	free( x ) ; free( y );
	free( ex1 ) ; free( ey1 );
	free( ex2 ) ; free( ey2 );
	return TCL_OK;
    } else if ((c == 'k') && (strncmp(argv[1], "key", length) == 0)) {
        double dx, dy;
        float xp, yp, r;
	char string[128];
	double d;
	int i, l, fr;
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " key option ?args?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (	(strncmp(argv[2], "start", length) == 0) ||
		(strncmp(argv[2], "initialise", length) == 0)  ) {
	   if (parseStyle(interp, 3, argc, argv) != TCL_OK) {
		return TCL_ERROR;
	   }
	   fr = 0; r = 1.0;
	   xp = 0.0 ; yp = 0.0;
	   for ( i = 3 ; i < argc ; i++) {
		l = strlen(argv[i]);
		if (strncmp(argv[i], "-frame", l) == 0) {
		    i++;
		    if ( i < argc) {
	  		if (Tcl_GetBoolean(interp, argv[i], &fr) != TCL_OK) {
			   return TCL_ERROR;
	  		}
		    }
		} else if (strncmp(argv[i], "-drop", l) == 0) {
		    i++;
		    if ( i < argc) {
	  		if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			   return TCL_ERROR;
	  		}
			r = d;
		    }
		} else if (strncmp(argv[i], "-x", l) == 0) {
		    i++;
		    if ( i < argc) {
	  		if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			   return TCL_ERROR;
	  		}
			xp = d;
		    }
		} else if (strncmp(argv[i], "-y", l) == 0) {
		    i++;
		    if ( i < argc) {
	  		if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			   return TCL_ERROR;
	  		}
			yp = d;
		    }
		} 
	   }
	   pgskey_( &xp, &yp, &r, &fr );

	} else if (strncmp(argv[2], "add", length) == 0) {
	   strcpy( string, "" );
	   for ( i = 3 ; i < argc ; i++) {
	     l = strlen(argv[i]);
	     if (strncmp(argv[i], "-label", l) == 0) {
		    i++;
		    if ( i < argc) {
			strcpy( string, argv[i] );
		    }
	     }
	   }
	   if (parseStyle(interp, 3, argc, argv) != TCL_OK) {
		return TCL_ERROR;
	   }
	   pgakey_( &lli, &lwi, &lco, &symb, &si, &swi, &sco, string);

	} else if (strncmp(argv[2], "end", length) == 0) {
	   if (parseStyle(interp, 3, argc, argv) != TCL_OK) {
		return TCL_ERROR;
	   }
	   pgekey_( );
	   pgplotSetStyle( );

	} else {
		Tcl_AppendResult(interp, "Unkown option ",argv[2]," to ",
		argv[0]," key, should be start, initialise, add or end",
		(char *) NULL);
		return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 'g') && (strncmp(argv[1], "graph", length) == 0)) {
	static int ncurve; int n; char string[30];
	static int gopt[6] ; static float gscale[6]; static float rxy[4];
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " graph option ?args?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (	(strncmp(argv[2], "start", length) == 0) ||
		(strncmp(argv[2], "initialise", length) == 0)  ) {
	   if (parseStyle(interp, 3, argc, argv) != TCL_OK) {
		return TCL_ERROR;
	   }
	   ncurve = 0;
	   pggraphinit_( gscale, gopt, rxy );

	} else if (strncmp(argv[2], "range", length) == 0) {
	   if (parseCurve(interp, 2, argc, argv) != TCL_OK) {
		return TCL_ERROR;
	   }
	   if (parseGraph(interp, 2, argc, argv, gopt, gscale) != TCL_OK) {
		return TCL_ERROR;
	   }
	   pggraphrange_( gscale, gopt, &ndata, x, y, rxy );
	   for ( n = 0 ; n < 4 ; n++ ) {
		sprintf( string, "%f", rxy[n]);
		Tcl_AppendElement( interp, string);
	   }
	   return TCL_OK;

	} else if (	(strncmp(argv[2], "line", length) == 0) ||
			(strncmp(argv[2], "add", length) == 0)   ) {
	   if (parseCurve(interp, 2, argc, argv) != TCL_OK) {
		return TCL_ERROR;
	   }
	   if (parseGraph(interp, 2, argc, argv, gopt, gscale) != TCL_OK) {
		return TCL_ERROR;
	   }
	   pggraphscale_( gscale, gopt, &ndata, 
		          x, y, ex1, ex2, ey1, ey2  );
	   if (line > 0) {
	     pgplotSetLineStyle( );
	     if ( type == 0 ) {
		pgline_( &ndata, x, y );
	     } else if (type == 1)  {
	        pgdrawbinned_( &ndata, x, y );
	     }
	   }
	   if (symb > 0) {
		pgplotSetSymbStyle( );
		pgpnts_( &ndata, x, y, &symb, &unity);
	   }
	   pgplotSetLineStyle( );
	   pggrapherror_( gscale, gopt, &ndata, &errx, &erry, 
		          &eTop, x, y, ex1, ex2, ey1, ey2  );
	   pgplotSetStyle( );
	   free( x ) ; free( y );
	   free( ex1 ); free( ex2 ); 
	   free( ey1 ); free( ey2 ); 
	   return TCL_OK;

	} else if (strncmp(argv[2], "end", length) == 0) {
	   ncurve = 0;
	   pgplotSetStyle( );

	} else {
		Tcl_AppendResult(interp, "Unkown option ",argv[2]," to ",
		argv[0]," graph, should be start, initialise, add, line or end",
		(char *) NULL);
		return TCL_ERROR;
	}
	return TCL_OK;

    } else if ((c == 'i') && (strncmp(argv[1], "image", length) == 0)) {
	int i1, i2, j1, j2;
	int ih; img_defn defn, def1;
	float a1, a2, tr[6];
	double da1, da2;
	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " image imId a1 a2\"", (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetInt(interp, argv[2], &ih) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[3], &da1) != TCL_OK) {
	    return TCL_ERROR;
	}
        if (Tcl_GetDouble(interp, argv[4], &da2) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (ic_hashExists( ih ) == IC_OK) {
	    defn = ic_hash[ih].defn ; a1 = da1; a2 = da2;
	    def1 = defn;
	    tr[0] = 0.0 ; tr[1] = 1.0 ; tr[2] = 0.0;
	    tr[3] = 0.0 ; tr[4] = 0.0 ; tr[5] = 1.0;
	    if (ic_parseSubIm( interp, 5, argc, argv, &def1) != IC_OK ) {
		return TCL_ERROR;
	    }
	    if (parseImage( interp, 5, argc, argv, tr ) != TCL_OK) {
	        return TCL_ERROR;
	    }
	    i1 = def1.x1 - defn.x1 + 1; i2 = def1.x2 - defn.x1 + 1; 
	    j1 = def1.y1 - defn.y1 + 1; j2 = def1.y2 - defn.y1 + 1; 
	    cpgimag( defn.data_p, defn.xdim, defn.ydim,
		     i1, i2, j1, j2, a1, a2, tr);
	}
	return TCL_OK;


     } else if ((c == 'c') && (strncmp(argv[1], "ct", length) == 0)) {
	int i1, i2; float tr[5];
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " index option ?args?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (parseImage( interp, 2, argc, argv, tr ) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ( strncmp(argv[2], "range-set", length) == 0  ) {
	   if (argc != 5) {
	      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			    " index set c1 c2\"", (char *) NULL);
	      return TCL_ERROR;
	   }
           if (Tcl_GetInt(interp, argv[3], &i1) != TCL_OK) {
	       return TCL_ERROR;
	   }
           if (Tcl_GetInt(interp, argv[4], &i2) != TCL_OK) {
	       return TCL_ERROR;
	   }
	   cpgscir( i1, i2 );

	} else if ( strncmp(argv[2], "range-enquire", length) == 0  ) {
	   cpgqcir( &i1, &i2);
	   sprintf( string, "%d", i1);
	   Tcl_AppendElement( interp, string);
	   sprintf( string, "%d", i2);
	   Tcl_AppendElement( interp, string);
	}
	return TCL_OK;

    } else if ((c == 'w') && ( strncmp(argv[1], "wedge", length) == 0)) {
	char label[128], side[10];
	int l, i;
	double d;
	float disp, width, fg, bg;
	strcpy( label, "");
	strcpy( side, "li");
	fg = 1.0 ; bg = 0.0; disp = 1.5 ; width = 1.0;
	if (argc > 2) {
	   for (i = 2; i < argc; i++) {
		l = strlen( argv[i] );
		if (strncmp(argv[i], "-side", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -side SIDE\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   strcpy( side, argv[i] );
		} else if (strncmp(argv[i], "-label", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -label TEXT\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   strcpy( label, argv[i] );
		} else if (strncmp(argv[i], "-disp", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -disp X\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			return TCL_ERROR;
		   }
		   disp = d;
		} else if (strncmp(argv[i], "-width", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -width X\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			return TCL_ERROR;
		   }
		   width = d;
		} else if (strncmp(argv[i], "-fg", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -fg X\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			return TCL_ERROR;
		   }
		   fg = d;
		} else if (strncmp(argv[i], "-bg", l) == 0) {
		   i++;
		   if ( i == argc ) {
	    		Tcl_AppendResult(interp, "wrong # args: should be \"",
			" -fg X\"", (char *) NULL);
			return TCL_ERROR;
		   }
		   if (Tcl_GetDouble(interp, argv[i], &d) != TCL_OK) {
			return TCL_ERROR;
		   }
		   bg = d;
		}
	   }
	}
	cpgwedg( side, disp, width, fg, bg, label );
	return TCL_OK;

    } else {
	Tcl_AppendResult(interp, "bad option \"",argv[1],"\" should be ",
		" buffer, bbuf, ebuf, update, page, paper, stream, begin,",
                " set, style, enquire, clear, viewport, vport,",
                " window, move, draw, line, error, circle, rectangle,",
                " arrow, point, symbol, curve, polygon, text, label,",
		" key, graph, image, ct, wedge\"",
		(char *) NULL);
	return TCL_ERROR;
    }
}
