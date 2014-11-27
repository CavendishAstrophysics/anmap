/* 
 * imcatAppInit.c --
 */

#include "tk.h"
#include "string.h"
#include "ic.h"

/*
 * define global structures used to access records in the catalogue
 *
 */
	extern int hashMode;
	extern ic_iclRec iclRec;
	extern double rec_blank;
	extern img_defn imgDefn;
/*
 *---------------------------------------------------------------------
 *   Image catalogue and imaging initialisation
 *
 */
extern int              ic_iclPriv _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
extern int              imgPriv _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
extern int              imgApply _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
extern int              imgGeom _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
extern int              imgFit _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
extern int              imgAnal _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
extern int              imgPixel _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));
extern int              imgRegion _ANSI_ARGS_((ClientData clientData,
	                    Tcl_Interp *interp, int argc, char **argv));

/*
 *----------------------------------------------------------------------
 *
 * ic_Init --
 *
 *      Initialisation for use of image catalogue / stack commands in a
 *      tcl/tk based Anmap package.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	None
 *
 *----------------------------------------------------------------------
 */

int
ic_Init(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{



    Tcl_CreateCommand(interp, "iclPriv", ic_iclPriv, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "imgPriv", imgPriv, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "img_apply", imgApply, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "img_fit", imgFit, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "img_geom", imgGeom, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "img_pixel", imgPixel, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "img_region", imgRegion, (ClientData) NULL,
	    (void (*)()) NULL);
    Tcl_CreateCommand(interp, "img_anal", imgAnal, (ClientData) NULL,
	    (void (*)()) NULL);

  /* define the basic structure as a global Tcl array */
    Tcl_LinkVar( interp, "iclRec(ce)", (char *) &iclRec.ce, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(u1)", (char *) &iclRec.u1, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(u2)", (char *) &iclRec.u2, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(v1)", (char *) &iclRec.v1, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(v2)", (char *) &iclRec.v2, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(xdim)", (char *) &iclRec.xdim, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(ydim)", (char *) &iclRec.ydim, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(dtype)", (char *) &iclRec.dtype, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(data)", (char *) &iclRec.data, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(access)", (char *) &iclRec.access, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(open)", (char *) &iclRec.open, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(unit)", (char *) &iclRec.unit, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(type)", (char *) &iclRec.type, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(page)", (char *) &iclRec.page, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "iclRec(blank)", (char *) &rec_blank, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(mode)", (char *) &hashMode, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(ndims)", (char *) &imgDefn.ndims, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(xdim)", (char *) &imgDefn.xdim, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(ydim)", (char *) &imgDefn.ydim, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(zdim)", (char *) &imgDefn.zdim, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(tdim)", (char *) &imgDefn.tdim, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(vdim)", (char *) &imgDefn.vdim, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(ndata)", (char *) &imgDefn.ndata, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(x1)", (char *) &imgDefn.x1, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(x2)", (char *) &imgDefn.x2, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(y1)", (char *) &imgDefn.y1, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(y2)", (char *) &imgDefn.y2, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(z1)", (char *) &imgDefn.z1, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(z2)", (char *) &imgDefn.z2, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(t1)", (char *) &imgDefn.t1, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(t2)", (char *) &imgDefn.t2, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(v1)", (char *) &imgDefn.v1, TCL_LINK_INT) ;
    Tcl_LinkVar( interp, "imgDefn(v2)", (char *) &imgDefn.v2, TCL_LINK_INT) ;
    return TCL_OK;
}

