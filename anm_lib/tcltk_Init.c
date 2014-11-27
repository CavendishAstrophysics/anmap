/* 
 * anm_Init.c --
 */

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
static int              anm_command _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));

/*
 *---------------------------------------------------------------------
 *   IO routines    --- used for user interaction
 *
 */
static int              iocmd _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
static int              iocmd_system _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));

/*
 *---------------------------------------------------------------------
 *   Graphics routines --- for user graphics applications
 *
 */
static int              pgplotcmd _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));

/*
 *---------------------------------------------------------------------
 *   Dispatching routines --- used for dispatching commands to various
 *                            sub-systems
 *
 */

/*
 *---------------------------------------------------------------------
 *   Initialisation routines to be used in a call to an Application
 *   initialisation
 *
 */
/*
 *   Main Anmap initialisation
 */
int
anmap_Init(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
    Tcl_CreateCommand(interp, "anmap_init", anm_init, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "anmap_exec", anm_command, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "anmap_command", anm_command, (ClientData) NULL,
	    (void (*)()) NULL);
    return TCL_OK;
}
/*
 *   IO system initialisation
 */
int
iocmd_Init(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
    Tcl_CreateCommand(interp, "anmap_system", iocmd_system, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "iocmd_system", iocmd_system, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "iocmd", iocmd, (ClientData) NULL,
	    (void (*)()) NULL);
    return TCL_OK;
}
/*
 *   Graphic initialisation
 */
graphic_Init(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
    Tcl_CreateCommand(interp, "pgplot_", pgplotcmd, (ClientData) NULL,
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
   anm_dispatch_( interp, map_array, cs, &csl, &status );
   if (status) {
     return TCL_ERROR;
   }
   if (csl) {
     Tcl_SetResult( interp, cs, TCL_VOLATILE );
   }
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

static int
iocmd(dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int csl, resl, status;
   int i;
   char cs[2048];
   char res[2048];
   strcpy( cs, " ");
   for (i=1; i < argc; i++ ) {
     strcat( cs, " ");
     strcat( cs, argv[i] );
   }
   csl = strlen( cs );
   status = 0;
   iocmd_getoper_( interp, cs, &csl, res, &resl, &status );
   if (status) {
     return TCL_ERROR;
   }
   if (resl) {
     Tcl_SetResult( interp, res, TCL_VOLATILE );
   }
   return TCL_OK;
}

static int
pgplotcmd(dummy, interp, argc, argv)
   ClientData dummy;
   Tcl_Interp *interp;
   int argc;
   char **argv;
{
   int csl, resl, status;
   int i;
   char cs[2048];
   char res[2048];
   strcpy( cs, " ");
   for (i=1; i < argc; i++ ) {
     strcat( cs, " ");
     strcat( cs, argv[i] );
   }
   csl = strlen( cs );
   status = 0;
   pgplot_cmd_( interp, cs, &csl, res, &resl, &status );
   if (status) {
     return TCL_ERROR;
   }
   if (resl) {
     Tcl_SetResult( interp, res, TCL_VOLATILE );
   }
   return TCL_OK;
}






