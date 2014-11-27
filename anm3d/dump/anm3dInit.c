/* 
 * AnmapInit.c --
 *
 * Routine to Initialise Anmap commands
 */

#include "tcl.h"
#include "eqn.h"
#include "string.h"
#include "ic.h"

/*
 *    Include default definitions
 */
extern int main();
int *tclDummyMainPtr = (int *) main;
eqn_funs *stdfun_list;

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


extern int imcat_Init _ANSI_ARGS_(( Tcl_Interp *interp )) ;
extern int ic_Init _ANSI_ARGS_(( Tcl_Interp *interp )) ;


int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
  static char anmap_initCmd[] = 
    "set Xanm3d 0 ; source /mrao/anmap/anm3d/tcllib/init";

    /* 
     * Setup Tcl Interpreter
     */
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    /* 
     * Setup Anmap commands
     */
    if (ic_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }
    if (graphics_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }

    /* 
     * Setup function handling
     */
    stdfun_list = add_standard_functions(NULL);
    stdfun_list = add_external_function(stdfun_list,
			"image2(i,x,y)",img_accImg2,"i","x","y");
    stdfun_list = add_external_function(stdfun_list,
			"image2v(i,v,x,y)",img_accImg2v,"i","v","x","y");
    stdfun_list = add_external_function(stdfun_list,
			"Image2(i,x,y)",img_intImg2,"i","x","y");
    stdfun_list = add_external_function(stdfun_list,
			"Image2v(i,v,x,y)",img_intImg2v,"i","v","x","y");
    stdfun_list = add_external_function(stdfun_list,
			"image3(i,x,y,z)",img_accImg3,"i","x","y","z");
    stdfun_list = add_external_function(stdfun_list,
			"image3v(i,v,x,y,z)",img_accImg3v,"i","v","x","y","z");
    stdfun_list = add_external_function(stdfun_list,
			"Image3(i,x,y,z)",img_intImg2,"i","x","y","z");
    stdfun_list = add_external_function(stdfun_list,
			"Image3v(i,v,x,y,z)",img_intImg3v,"i","v","x","y","z");
    set_input_functions(stdfun_list);

    /*
     * Perform anmap specific initialisation
     */
    if (Tcl_Eval(interp, anmap_initCmd) == TCL_ERROR) {
        return TCL_ERROR;
    } 
    tcl_RcFileName = "~/.tclrc";

    return TCL_OK;

}


