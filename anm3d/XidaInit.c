/* 
 * tkAppInit.c --
 *
 *	Provides a default version of the Tcl_AppInit procedure for
 *	use in wish and similar Tk-based applications.
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef lint
static char sccsid[] = "@(#) tkAppInit.c 1.12 94/12/17 16:30:56";
#endif /* not lint */

#include "tk.h"
#include "eqn.h"
#include "string.h"
#include "ic.h"

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

#ifdef NEED_MATHERR
extern int matherr();
int *tclDummyMathPtr = (int *) matherr;
#endif

/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tk_Main never returns here, so this procedure never
 *	returns either.
 *
 * Side effects:
 *	Whatever the application does.
 *
 *----------------------------------------------------------------------
 */
eqn_funs *stdfun_list;

int
main(argc, argv)
    int argc;			/* Number of command-line arguments. */
    char **argv;		/* Values of command-line arguments. */
{
    Tk_Main(argc, argv, Tcl_AppInit);
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
extern int	Tk_GnuplotCmd _ANSI_ARGS_((ClientData clientData,
 		    Tcl_Interp *interp, int argc, char **argv));

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
  static char ida_initCmd[] = 
    "set Xida 1 ; if ![info exists env(IDA_SOURCE)] {set env(IDA_SOURCE) /mrao/anmap/anm3d} ; source $env(IDA_SOURCE)/tcllib/init ";

    Tk_Window main;

    main = Tk_MainWindow(interp);

    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    /* 
     * Setup ida commands
     */
    if (ic_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }
    if (graphics_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }
    Tcl_CreateCommand(interp, "gnuplot", Tk_GnuplotCmd,
	(ClientData) main, (void (*)()) NULL);

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
     * Perform ida specific initialisation
     */
    if (Tcl_Eval(interp, ida_initCmd) == TCL_ERROR) {
        return TCL_ERROR;
    }

    tcl_RcFileName = "~/.idarc";
    return TCL_OK;
}
