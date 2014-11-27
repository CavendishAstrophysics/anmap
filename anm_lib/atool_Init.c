#include "tk.h"
#include "string.h"


/*
 * Initialise Anmap related commands for use in a Tcl interpreter
 *
 * P. Alexander, MRAO Cambridge
 * Version 1  23/2/94
 */

/*
 * Define external data structures
 */
extern float *map_array;

/*
 *---------------------------------------------------------------------
 *   Anmap routines --- used for command dispatching to the main
 *                      Anmap application
 */
static int              anm_init _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
static int              iocmd_system _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));

/*
 *   Anmap Tool initialisation
 */
int
atool_Init(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
    Tcl_CreateCommand(interp, "anmap_init", anm_init, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "anmap_system", iocmd_system, (ClientData) NULL,
	    (void (*)()) NULL);
    return TCL_OK;
}
/*
 *---------------------------------------------------------------------
 *   Implementation routines for above commands
 *
 */
static int
anm_init(dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   long int status;
   long int nm, msize, nb, bsize, scope;
   int snm, smsize, snb, sbsize;
   int tsize;
   scope = 1;
   if (argc != 5 ) {
     interp->result = "wrong # args";
     return TCL_ERROR;
   }
   if (Tcl_GetInt( interp, argv[1], &snm) != TCL_OK) {
     return TCL_ERROR;
   }
   if (Tcl_GetInt( interp, argv[2], &smsize) != TCL_OK) {
     return TCL_ERROR;
   }
   if (Tcl_GetInt( interp, argv[3], &snb) != TCL_OK) {
     return TCL_ERROR;
   }
   if (Tcl_GetInt( interp, argv[4], &sbsize) != TCL_OK) {
     return TCL_ERROR;
   }
   nm = snm; msize = smsize ; nb = snb; bsize = sbsize ;
   tsize = nm*msize + nb*bsize;
   status = 0;
   anm_start_( &scope, &nm, &msize, &nb, &bsize, &status );
   map_array = (float *)realloc(map_array,tsize*sizeof(float));
   return TCL_OK;
}

static int
iocmd_system(clientData, interp, argc, argv)
    ClientData  clientData;
    Tcl_Interp *interp;
    int         argc;
    char      **argv;
{
    int exitCode;

    if (argc != 2) {
        interp->result = "wrong # args";
        return TCL_ERROR;
    }
    exitCode = system (argv [1]);
    if (exitCode == -1) {
        interp->result = Tcl_PosixError (interp);
        return TCL_ERROR;
    }
    interp->result = "";
    return TCL_OK;
}
