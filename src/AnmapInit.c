/* 
 * AnmapInit.c --
 *
 * Routine to Initialise Anmap commands
 */

#include "tcl.h"
#include "eqn.h"
#include "string.h"
/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

#ifdef NEED_MATHERR
extern int matherr();
int *tclDummyMathPtr = (int *) matherr;
#endif

float *map_array;
/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tcl_Main never returns here, so this procedure never
 *	returns either.
 *
 * Side effects:
 *	Whatever the application does.
 *
 *----------------------------------------------------------------------
 */

int
main(argc, argv)
    int argc;			/* Number of command-line arguments. */
    char **argv;		/* Values of command-line arguments. */
{
    Tcl_Main(argc, argv, Tcl_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
}

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

extern int anmap_Init _ANSI_ARGS_(( Tcl_Interp *interp )) ;
extern int iocmd_Init _ANSI_ARGS_(( Tcl_Interp *interp )) ;

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
  static eqn_funs *fun_list;
  static char anmap_initCmd[] = 
    "set Xanmap 0 ; source /mrao/anmap_v7.5/etc/anmap.tcl";

    /* 
     * Setup Tcl Interpreter
     */
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    /* 
     * Setup Anmap commands
     */
    if (anmap_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }

    /* 
     * Setup IO commands
     */
    if (iocmd_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }

    /* 
     * Setup Catalogue commands
     */
    if (imcat_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }

    /* 
     * Setup Graphic commands
     */
    if (graphic_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }
    /* 
     * Setup function handling
     */

    fun_list = add_standard_functions(NULL);
    set_input_functions(fun_list);

    /*
     * Perform anmap specific initialisation
     */
    if (Tcl_Eval(interp, anmap_initCmd) == TCL_ERROR) {
        return TCL_ERROR;
    }  
    tcl_RcFileName = "~/.tclrc";

    return TCL_OK;

}





