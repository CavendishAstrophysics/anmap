/* 
 * imcatAppInit.c --
 */

#include "tk.h"
#include "string.h"

/*
 * Define the basic structure used to access the image catalogue records
 *
 */

struct	imcat_record {
	int ce;
	char file[96];
	char source[24];
	char program[16];
	int u1;
	int u2;
	int v1;
	int v2;
	int xdim;
	int ydim;
	int dtype;
	float blank;
	int data;
	int access;
	int open;
	int unit;
	int type;
	int page;
} iclRec;
char file[96], source[24], program[16];
double rec_blank;
int ce_max = 256;
int ce;

/*
 *---------------------------------------------------------------------
 *   IMage catalogue / stack commands
 *
 */
static int              iclPriv _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));

/*
 *----------------------------------------------------------------------
 *
 * imcat_Init --
 *
 *      Initialisation for use of image catalogue / stack commands in a
 *      tcl/tk based Anmap package.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	None
 *
 *----------------------------------------------------------------------
 */

int
imcat_Init(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{



    Tcl_CreateCommand(interp, "iclPriv", iclPriv, (ClientData) NULL,
	    (void (*)()) NULL);

  /* define the basic structure as a global Tcl array */
    Tcl_LinkVar( interp, "iclRec(ce)", (char *) &iclRec.ce, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(u1)", (char *) &iclRec.u1, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(u2)", (char *) &iclRec.u2, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(v1)", (char *) &iclRec.v1, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(v2)", (char *) &iclRec.v2, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(xdim)", (char *) &iclRec.xdim, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(ydim)", (char *) &iclRec.ydim, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(dtype)", (char *) &iclRec.dtype, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(data)", (char *) &iclRec.data, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(access)", (char *) &iclRec.access, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(open)", (char *) &iclRec.open, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(unit)", (char *) &iclRec.unit, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(type)", (char *) &iclRec.type, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(page)", (char *) &iclRec.page, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(blank)", (char *) &rec_blank, TCL_LINK_INT) ;
    return TCL_OK;
}

/*
 *   Routine to provide basic access to the image catalogue.
 *
 *   Options:                           Returned:  Updated:
 *      open                            NULL       
 *      close                           NULL
 *      read  record_no                 NULL       iclRec
 *      write record_no                 NULL       iclRec
 *
 */
static int
iclPriv (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      status;
   int      length, i;
   char     value[20];
   char     **list;

   if (argc < 2) {
     interp->result = "wrong # args";
     return TCL_ERROR;
   }

   status = 0 ;
   length = strlen(argv[1]);
  /*
   * Sort out option to command, returning an error on an incorrect option
   */
   if (strncmp(argv[1], "open", length) ==0) {
     mapcat_open_( &status ) ;
     if ( status != 0 ) {
        interp->result = "unable to open catalogue";
        return TCL_ERROR ;
     }

   } else if (strncmp(argv[1], "close", length) ==0) {
     mapcat_close_( &status ) ;
     if ( status != 0 ) {
        interp->result = "unable to close catalogue";
        return TCL_ERROR ;
     }

   } else if (strncmp(argv[1], "setdef", length) ==0) {
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     if (Tcl_GetInt( interp, argv[2], &i ) == TCL_ERROR) {
       return TCL_ERROR;
     }
     ce =  i ; map_setdef_( &ce, &status );
     if ( status != 0 ) {
        interp->result = "unable to set default catalogue entry";
        return TCL_ERROR ;
     }

   } else if (strncmp(argv[1], "enqdef", length) ==0) {
     if (argc != 2) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     map_enqdef_( &ce, &status );
     if ( status != 0 ) {
        interp->result = "unable to set default catalogue entry";
        return TCL_ERROR ;
     }
     sprintf( interp->result, "%d", ce );

   } else if (strncmp(argv[1], "read", length) ==0) {
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     if (Tcl_GetInt( interp, argv[2], &i ) == TCL_ERROR) {
       return TCL_ERROR;
     }
     ce =  i ; mapcat_read1_( &ce, &iclRec, &status );
     if ( status != 0 ) {
        interp->result = "unable to read catalogue entry";
        return TCL_ERROR ;
     }
     iclRec.ce = ce ;
     strncpy( file, iclRec.file, 96 );
     strncpy( source, iclRec.source, 24 );
     strncpy( program, iclRec.program, 16 );

     Tcl_SetVar( interp, "iclRec(file)", file, TCL_GLOBAL_ONLY ) ;
     Tcl_SetVar( interp, "iclRec(file)", "\0", TCL_GLOBAL_ONLY|TCL_APPEND_VALUE ) ;
     Tcl_SetVar( interp, "iclRec(source)", source, TCL_GLOBAL_ONLY ) ;
     Tcl_SetVar( interp, "iclRec(source)", "\0", TCL_GLOBAL_ONLY|TCL_APPEND_VALUE ) ;
     Tcl_SetVar( interp, "iclRec(program)", program, TCL_GLOBAL_ONLY ) ;
     Tcl_SetVar( interp, "iclRec(program)", "\0", TCL_GLOBAL_ONLY|TCL_APPEND_VALUE ) ;

   } else if (strncmp(argv[1], "enqmap", length) ==0) {
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     strncpy( iclRec.file, argv[2], 96 );
     status = 0; mapcat_mapenq_( &iclRec, &status );
     if ( status != 0 ) {
        interp->result = "unable to read map information";
        return TCL_ERROR ;
     }
     strncpy( file, iclRec.file, 96 );
     strncpy( source, iclRec.source, 24 );
     strncpy( program, iclRec.program, 16 );

     Tcl_SetVar( interp, "iclRec(file)", file, TCL_GLOBAL_ONLY ) ;
     Tcl_SetVar( interp, "iclRec(file)", "\0", TCL_GLOBAL_ONLY|TCL_APPEND_VALUE ) ;
     Tcl_SetVar( interp, "iclRec(source)", source, TCL_GLOBAL_ONLY ) ;
     Tcl_SetVar( interp, "iclRec(source)", "\0", TCL_GLOBAL_ONLY|TCL_APPEND_VALUE ) ;
     Tcl_SetVar( interp, "iclRec(program)", program, TCL_GLOBAL_ONLY ) ;
     Tcl_SetVar( interp, "iclRec(program)", "\0", TCL_GLOBAL_ONLY|TCL_APPEND_VALUE ) ;

   } else if (strncmp(argv[1], "write", length) ==0) {
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     if (Tcl_GetInt( interp, argv[2], &i ) != TCL_OK) {
       return TCL_ERROR;
     }
     ce = i ; iclRec.ce = ce ;
     strncpy( iclRec.file, Tcl_GetVar(interp, "iclRec(file)", TCL_GLOBAL_ONLY), 96 );
     strncpy( iclRec.source, Tcl_GetVar(interp, "iclRec(source)", TCL_GLOBAL_ONLY), 24 );
     strncpy( iclRec.program, Tcl_GetVar(interp, "iclRec(program)", TCL_GLOBAL_ONLY), 16 );
     status = 0 ; mapcat_write1_( &ce, &iclRec, &status );

   } else if (strncmp(argv[1], "stack-display", length) ==0) {
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     if (Tcl_GetInt( interp, argv[2], &i ) != TCL_OK) {
       return TCL_ERROR;
     }
     stack_display1_( &i, &status );

   } else if (strncmp(argv[1], "stack-initialise", length) ==0) {
     int i1 = -1, i2 = -1;
     float x1 = 0.0, x2 = 0.0;
     if (argc != 2) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     stack_init_(&i1, &x1, &i2, &x2, &status);

   } else if (strncmp(argv[1], "set-directory", length) ==0) {
     char dir[128];
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     strcpy( dir, argv[2] );
     mapcat_setdefdir1_( dir, &status );

   } else if (strncmp(argv[1], "enq-directory", length) ==0) {
     char dir[128];
     if (argc != 2) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     mapcat_enqdefdir1_( dir, &status );
     Tcl_SetResult( interp, dir, TCL_STATIC );

   } else if (strncmp(argv[1], "set-area", length) ==0) {
     float a ; double da;
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     if (Tcl_GetDouble( interp, argv[2], &da ) != TCL_OK) {
       return TCL_ERROR;
     }
     a = da ; mapcat_setarea1_( &a, &status );

   } else if (strncmp(argv[1], "report", length) ==0) {
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     if (Tcl_GetInt( interp, argv[2], &i ) != TCL_OK) {
       return TCL_ERROR;
     }
     mapcat_setrm1_( &i );

   } else if (strncmp(argv[1], "result-init", length) ==0) {
     mapcat_initlres_();

   } else if (strncmp(argv[1], "result-set", length) ==0) {
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     if (Tcl_GetInt( interp, argv[2], &i ) != TCL_OK) {
       return TCL_ERROR;
     }
     mapcat_setlres_( &i );

   } else if (strncmp(argv[1], "result-enq", length) ==0) {
     int i;
     char string[100];
     int lres[10];
     mapcat_enqlres_( &lres );
     for ( i = 0 ; i < 10 ; i++ ) {
	sprintf( string, "%d", lres[i]);
	Tcl_AppendElement( interp, string);
     }

   } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": should be read, write ",
		(char *) NULL);
	return TCL_ERROR;
   }
   return TCL_OK;
}













