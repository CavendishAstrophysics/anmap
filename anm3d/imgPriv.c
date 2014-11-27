#include "tk.h"
#include "string.h"
#include "ic.h"

/*
 * Create a fixed-sized hash table, a hash record and a definition record
 *
 */
 extern ic_hashRec ic_hash[HASH_SIZE];
 int hashMode;
 img_defn imgDefn;
 extern ic_iclRec iclRec;

/*
 *   Routine to provide access to Image IO.
 *
 */
int imgPriv (dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int      status;
   int      length, i, length1;
   char     value[20];
   char     *string;
   char     **list;

   if (argc < 2) {
     interp->result = "wrong # args";
     return TCL_ERROR;
   }

  /*
   * Update the iclRec structure to reflect the tcl state
   */
   strncpy( iclRec.file, Tcl_GetVar(interp, "iclRec(file)", TCL_GLOBAL_ONLY), 96 );
   strncpy( iclRec.source, Tcl_GetVar(interp, "iclRec(source)", TCL_GLOBAL_ONLY), 24 );
   strncpy( iclRec.program, Tcl_GetVar(interp, "iclRec(program)", TCL_GLOBAL_ONLY), 16 );

  /*
   * Sort out option to command, returning an error on an incorrect option
   */
   status = 0 ;
   length = strlen(argv[1]);
   if (strncmp(argv[1], "hash", length) ==0) {
     if (argc < 3) {
       interp->result = "wrong # args should be \"hash option ?args?\"";
       return TCL_ERROR;
     }
     length1 = strlen(argv[2]);
     if (strncmp(argv[2], "init", length1) ==0) {
	if (ic_hashInit( -1 ) == IC_ERROR) {
	   return TCL_ERROR;
	}
     } else if (strncmp(argv[2], "clear", length) ==0) {
	int i;
	if (argc != 4) {
	   interp->result = "wrong # args should be \"hash clear imId\"";
	   return TCL_ERROR;
	}
	if (ic_parseImid( interp, argv[3], &i ) == TCL_ERROR) {
	   return TCL_ERROR;
	}

     } else if (strncmp(argv[2], "next", length) ==0) {
	int i;
	if ( ic_hashNext( &i ) == IC_ERROR ) {
	   interp->result = "no free hash-table entries";
	   return TCL_ERROR;
	}
	sprintf( interp->result, "%d", i);

     } else if (strncmp(argv[2], "exists", length) ==0) {
	int i;
	if (argc != 4) {
	   interp->result = "wrong # args should be \"hash exists imId\"";
	   return TCL_ERROR;
	}
	if (ic_parseImid( interp, argv[3], &i ) == TCL_ERROR) {
		i = 0;
	} else {
		i = 1;
	}
	sprintf( interp->result, "%d", i);

     } else if (strncmp(argv[2], "max", length) ==0) {
	int i;
	i = HASH_SIZE; 	sprintf( interp->result, "%d", i);

     } else {
	Tcl_AppendResult(interp, "bad option \"", argv[2],
		"\": should be init clear next exists max\"",
		(char *) NULL);
	return TCL_ERROR;
     }
 

   } else if (strncmp(argv[1], "definition", length) ==0) {
     int ih, idata;
     if (argc < 3) {
	   interp->result = "wrong # args should be \"definition ?option? imId\"";
	   return TCL_ERROR;
     }
     length1 = strlen(argv[2]);
     if (strncmp(argv[2], "get", length1) ==0) {
	if (argc != 4) {
	   interp->result = "wrong # args should be \"definition get imId\"";
	   return TCL_ERROR;
	}
	if (ic_parseImid( interp, argv[3], &ih ) == TCL_ERROR) {
	   return TCL_ERROR;
	}
	imgDefn = ic_hash[ih].defn;
	hashMode = ic_hash[ih].mode;
	sprintf( value, "%f", imgDefn.blank );
	Tcl_SetVar( interp, "imgDefn(blank)", value, TCL_GLOBAL_ONLY);
	sprintf( value, "%f", imgDefn.norm );
	Tcl_SetVar( interp, "imgDefn(norm)", value, TCL_GLOBAL_ONLY);

     } else if (strncmp(argv[2], "set", length) ==0) {
	double d;
	if (argc != 4) {
	   interp->result = "wrong # args should be \"definition set imId\"";
	   return TCL_ERROR;
	}
	if (ic_parseImid( interp, argv[3], &ih ) == TCL_ERROR) {
	   return TCL_ERROR;
	}
	/* Perform consistence checks on setting the definition data structure */
        string = Tcl_GetVar( interp, "imgDefn(norm)", TCL_GLOBAL_ONLY);
	Tcl_GetDouble( interp, string, &d ) ; imgDefn.norm = d;
        string = Tcl_GetVar( interp, "imgDefn(blank)", TCL_GLOBAL_ONLY);
	Tcl_GetDouble( interp, string, &d ) ; imgDefn.blank = d;
	if ( imgDefn.ndata <= ic_hash[ih].defn.ndata ) {
	   idata = imgDefn.xdim * imgDefn.ydim * 
		   imgDefn.zdim * imgDefn.tdim * imgDefn.vdim;
	   if ( idata == imgDefn.ndata ) {
		ic_hash[ih].defn = imgDefn;
	   }
	}

     } else if (strncmp(argv[2], "read", length) ==0) {
	int type;
	if (argc != 5) {
	   interp->result = "wrong # args should be \"definition read file type\"";
	   return TCL_ERROR;
	}
	if (Tcl_GetInt( interp, argv[4], &type ) == TCL_ERROR) {
	   return TCL_ERROR;
	}
	if (ic_imgFileDefn( argv[3], &type, &imgDefn ) != IC_OK) {
	   return TCL_ERROR;
	}
	hashMode = ic_hash[ih].mode;
	sprintf( value, "%f", imgDefn.blank );
	Tcl_SetVar( interp, "imgDefn(blank)", value, TCL_GLOBAL_ONLY);
	sprintf( value, "%f", imgDefn.norm );
	Tcl_SetVar( interp, "imgDefn(norm)", value, TCL_GLOBAL_ONLY);
     }

   } else if (strncmp(argv[1], "iclrec", length) ==0) {
     int ih;
     length1 = strlen(argv[2]);
     if (strncmp(argv[2], "get", length1) ==0) {
	if (argc != 4) {
	   interp->result = "wrong # args should be \"iclrec get imId\"";
	   return TCL_ERROR;
	}
	if (ic_parseImid( interp, argv[2], &ih ) == TCL_ERROR) {
	   return TCL_ERROR;
	}
	iclRec = ic_hash[ih].rec;
	Tcl_SetVar( interp, "iclRec(file)", iclRec.file, TCL_GLOBAL_ONLY ) ;
	Tcl_SetVar( interp, "iclRec(source)", iclRec.source, TCL_GLOBAL_ONLY ) ;
	Tcl_SetVar( interp, "iclRec(program)", iclRec.program, TCL_GLOBAL_ONLY ) ;

     } else if (strncmp(argv[2], "set", length) ==0) {
	if (argc != 4) {
	   interp->result = "wrong # args should be \"iclrec set imId\"";
	   return TCL_ERROR;
	}
	if (ic_parseImid( interp, argv[2], &ih ) == TCL_ERROR) {
	   return TCL_ERROR;
	}
	ic_hash[ih].rec = iclRec;

     }

   } else if (strncmp(argv[1], "create", length) ==0) {
     int ih = -1;
     if (ic_imgCreate( &imgDefn, &ih ) == IC_OK) {
	ic_resultImid( interp, ih);
     } else {
	interp->result = "error creating image";
	return TCL_ERROR;
     }
     hashMode = ic_hash[ih].mode;

   } else if (strncmp(argv[1], "destroy", length) ==0) {
     int ih = -1;
     if (argc != 3) {
	interp->result = "wrong # args should be \"destroy imId\"";
	return TCL_ERROR;
     }
     if (ic_parseImid( interp, argv[2], &ih ) == TCL_ERROR) {
	return TCL_ERROR;
     }
     if (ic_imgDestroy( ih ) == IC_OK) {
	ic_resultImid( interp, ih);
	return TCL_OK;
     } else {
	interp->result = "error destroying image";
	return TCL_ERROR;
     }

   } else if (strncmp(argv[1], "read", length) ==0) {
     int type;
     int ih = -1;
     char file[128];
     if (argc >= 3) {
	strcpy( file, argv[2] );
     } else {
	strcpy( file, ic_hash[ih].rec.file );
	type = ic_hash[ih].rec.type;
     }
     if (argc >= 4) {
	if (Tcl_GetInt( interp, argv[3], &type) == TCL_ERROR) {
	   return TCL_ERROR;
	}
     }
     if (ic_imgRead( &iclRec, file, type, &imgDefn, &ih ) == IC_OK) {
	ic_resultImid( interp, ih);
     } else {
	interp->result = "error reading image";
	return TCL_ERROR;
     }
     hashMode = ic_hash[ih].mode;
     sprintf( value, "%f", imgDefn.blank );
     Tcl_SetVar( interp, "imgDefn(blank)", value, TCL_GLOBAL_ONLY);
     sprintf( value, "%f", imgDefn.norm );
     Tcl_SetVar( interp, "imgDefn(norm)", value, TCL_GLOBAL_ONLY);

   } else if (strncmp(argv[1], "write", length) ==0) {
     int type;
     int ih = -1;
     char file[128];
     if (argc < 3) {
	interp->result = "wrong # args should be \"write imId ?file? ?type?\"";
	return TCL_ERROR;
     }
     if (ic_parseImid( interp, argv[2], &ih ) == TCL_ERROR) {
	return TCL_ERROR;
     }
     if (ic_hashExists( ih ) == IC_OK) {
	strcpy( file, ic_hash[ih].rec.file );
	type = ic_hash[ih].rec.type;
     }
     if (argc >= 4) {
	strcpy( file, argv[3] );
     }
     if (argc >= 5) {
	if (Tcl_GetInt( interp, argv[4], &type) == TCL_ERROR) {
	   return TCL_ERROR;
	}
     }
     if (ic_imgWrite( ih, file, type ) == IC_OK) {
	ic_resultImid( interp, ih);
	return TCL_OK;
     } else {
	interp->result = "error writing image";
	return TCL_ERROR;
     }

   } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": should be hash, defintion, create, destroy, ",
		"read, write\"",
		(char *) NULL);
	return TCL_ERROR;
   }
   return TCL_OK;
}

