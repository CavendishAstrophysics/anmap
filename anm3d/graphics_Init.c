/* 
 * anm_Init.c --
 */

#include "tk.h"
#include "eqn.h"
#include "string.h"


/*
 * Initialise graphics commands for use in a Tcl interpreter
 *
 * P. Alexander, MRAO Cambridge
 * Version 1  3/3/94
 */


/*
 *---------------------------------------------------------------------
 *   Graphics routines --- for user graphics applications
 *
 */
extern int              pgplotCmd _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
extern int              vogleCmd _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
/*
 *   Graphic initialisation
 */
int
graphics_Init(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{

    /* 
     * Setup graphics commands
     */
    Tcl_CreateCommand(interp, "pg", pgplotCmd, (ClientData) NULL,
	    (void (*)()) NULL);

    Tcl_CreateCommand(interp, "vg", vogleCmd, (ClientData) NULL,
	    (void (*)()) NULL);
    return TCL_OK;
}


