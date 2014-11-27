#include "tk.h"
#include "string.h"
#include "ic.h"

/*
 * define global structures used to access records in the catalogue
 *
 */
	ic_iclRec iclRec;
	double rec_blank;
	static char file[100], source[28], program[20];
	static int ce_max = 256;
	static int ce;

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
int ic_iclPriv (dummy, interp, argc, argv)
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
     ic_catopen_( &status ) ;
     if ( status != 0 ) {
        interp->result = "unable to open catalogue";
        return TCL_ERROR ;
     }

   } else if (strncmp(argv[1], "close", length) ==0) {
     ic_catclose_( &status ) ;
     if ( status != 0 ) {
        interp->result = "unable to close catalogue";
        return TCL_ERROR ;
     }

   } else if (strncmp(argv[1], "setdef", length) ==0) {
     int i;
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     if (Tcl_GetInt( interp, argv[2], &ce ) == TCL_ERROR) {
       return TCL_ERROR;
     }
     i = 0 ; ic_catdef_( &i, &ce, &status );
     if ( status != 0 ) {
        interp->result = "unable to set default catalogue entry";
        return TCL_ERROR ;
     }

   } else if (strncmp(argv[1], "enqdef", length) ==0) {
     int i;
     if (argc != 2) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     i = 1 ; ce =  0 ; ic_catdef_( &i, &ce, &status );
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
     ce =  i ; ic_catread_( &ce, &iclRec, &status );
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
     status = 0 ; ic_catwrite_( &ce, &iclRec, &status );

   } else if (strncmp(argv[1], "enqmap", length) ==0) {
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     strncpy( iclRec.file, argv[2], 96 );
     status = 0; ic_mapenq_( &iclRec, &status );
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


   } else if (strncmp(argv[1], "set-directory", length) ==0) {
     int i;
     char dir[128];
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     strcpy( dir, argv[2] );
     i = 0 ; ic_defdir_( &i, dir, &status );

   } else if (strncmp(argv[1], "enq-directory", length) ==0) {
     char dir[128];
     if (argc != 2) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     i = 1 ; ic_defdir_( &i, dir, &status );
     Tcl_SetResult( interp, dir, TCL_STATIC );

   } else if (strncmp(argv[1], "set-file", length) ==0) {
     int i;
     char dir[128];
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     strcpy( dir, argv[2] );
     i = 0 ; ic_catfile_( &i, dir, &status );

   } else if (strncmp(argv[1], "enq-file", length) ==0) {
     char dir[128];
     if (argc != 2) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     i = 1 ; ic_catfile_( &i, dir, &status );
     Tcl_SetResult( interp, dir, TCL_STATIC );


   } else if (strncmp(argv[1], "result-init", length) ==0) {
     int i, j, lres[10];
     i = 0 ; j = 0; ic_catlres_( &i, &j, &lres );

   } else if (strncmp(argv[1], "result-set", length) ==0) {
     int i, j, lres[10];
     i = 0 ; ic_catlres_( &i, &lres );
     if (argc != 3) {
       interp->result = "wrong # args";
       return TCL_ERROR;
     }
     if (Tcl_GetInt( interp, argv[2], &j ) != TCL_OK) {
       return TCL_ERROR;
     }
     i = 1 ; ic_catlres_( &i, &j, &lres );

   } else if (strncmp(argv[1], "result-enq", length) ==0) {
     int i, j, lres[10];
     char string[100];
     i = 2 ; j = 0 ; ic_catlres_( &i, &j, &lres );
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



