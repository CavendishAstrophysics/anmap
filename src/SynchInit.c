/* 
 * AnmapInit.c --
 *
 * Routine to Initialise Anmap commands
 */

#include "tcl.h"
#include "string.h"

/*
 *    Include default definitions
 */
extern int main();
int *tclDummyMainPtr = (int *) main;
extern float *map_array;

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */


extern int atool_Init _ANSI_ARGS_(( Tcl_Interp *interp )) ;
extern int imcat_Init _ANSI_ARGS_(( Tcl_Interp *interp )) ;
static int synch_command _ANSI_ARGS_((ClientData clientData,
	         Tcl_Interp *interp, int argc, char **argv));


int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
  static char anmap_initCmd[] = 
    "set Xanmap 0 ; source /mrao/anmap/etc/synch.tcl";

    /* 
     * Setup Tcl Interpreter
     */
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    /* 
     * Setup Anmap commands
     */
    if (atool_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }
    if (imcat_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }
    Tcl_CreateCommand(interp, "synch_exec", synch_command, (ClientData) NULL,
	    (void (*)()) NULL);

    /*
     * Perform anmap specific initialisation
     */
    if (Tcl_Eval(interp, anmap_initCmd) == TCL_ERROR) {
        return TCL_ERROR;
    }  
    tcl_RcFileName = "~/.tclrc";

    return TCL_OK;

}


static int
synch_command(dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   long int csl, status;
   int i;
   char cs[1024];
   if (argc < 2) {
     interp->result = "wrong # args";
     return TCL_ERROR;
   }
   strcpy( cs, argv[1] );
   for (i=2; i < argc; i++ ) {
     strcat( cs, " ");
     strcat( cs, argv[i] );
   }
   csl = strlen( cs );
   status = 0;
   synch_dispatch_( interp, map_array, cs, &csl, &status );
   if (status) {
     return TCL_ERROR;
   }
   if (csl) {
     Tcl_SetResult( interp, cs, TCL_VOLATILE );
   }
   return TCL_OK;
}

