/* 
 * Initialise command-line version of Anmap -- No X support
 */



#include "tk.h"
#include "eqn.h"
#include "string.h"

#define MY_SYSVR4_FIX 1 /* on my System 5 Rel 4 (Esix) I had to make */
			/* this fix. It should not affect normal */
			/* working, so I define this by default.*/

/*
 * The following variable is a special hack that allows applications
 * to be linked using the procedure "main" from the Tk library.  The
 * variable generates a reference to "main", which causes main to
 * be brought in from the library (and all of Tk and Tcl with it).
 */

extern int main();
int *tclDummyMainPtr = (int *) main;

extern float *map_array;


/*
 *---------------------------------------------------------------------
 *   Anmap specific commands
 *
 */
static int              anm_init _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
static int              anm_command _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
static int              anm_system _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
static int              iocmd _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
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

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
  static eqn_funs *fun_list;
  static char anmap_initCmd[] = "set Xanmap 0 ; source /mrao/anmap_v7.5/etc/anmap.tcl";
  

  /*
   * Call the init procedures for included packages.  Each call should
   * look like this:
   *
   * if (Mod_Init(interp) == TCL_ERROR) {
   *     return TCL_ERROR;
   * }
   *
   * where "Mod" is the name of the module.
   */
  
  if (Tcl_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }


  /*
   * Create anmap specific commands and do anmap initialisation
   */
    Tcl_CreateCommand(interp, "anmap_init", anm_init, (ClientData) main,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "anmap_exec", anm_command, (ClientData) main,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "anmap_command", anm_command, (ClientData) main,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "anmap_system", anm_system, (ClientData) main,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "iocmd", iocmd, (ClientData) main,
	    (void (*)()) NULL);
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
anm_command(dummy, interp, argc, argv)
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
   anmap_command_( interp, map_array, cs, &csl, &status );
   if (status) {
     return TCL_ERROR;
   }
   if (csl) {
     Tcl_SetResult( interp, cs, TCL_VOLATILE );
   }
   return TCL_OK;
}

static int
anm_system(clientData, interp, argc, argv)
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

static int
iocmd(dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   long int csl, resl, status;
   int i;
   char cs[256];
   char res[256];
   strcpy( cs, " ");
   for (i=1; i < argc; i++ ) {
     strcat( cs, " ");
     strcat( cs, argv[i] );
   }
   csl = strlen( cs );
   status = 0;
   iocmd_getoper_( cs, &csl, res, &resl, &status );
   if (status) {
     return TCL_ERROR;
   }
   if (resl) {
     Tcl_SetResult( interp, res, TCL_VOLATILE );
   }
   return TCL_OK;
}
/* eof */













