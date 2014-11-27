#include "tcl.h"


/* 
 * subroutine iocmd_tclset ( interp, name, val )
 * ----------------------------------------------
 *
 * F77 interface routine to set a tcl variable
 *
 * Given:
 *  pointer to interpreter:   integer         interp(*)
 *  name of variable          character*64    name
 *  value for variable        character*256   val
 *
 */
void iocmd_tclset_(interp, name, val )
  Tcl_Interp *interp;
  char name[64];
  char val[256];
{
  Tcl_SetVar( interp, name, val, TCL_GLOBAL_ONLY);
}

/* 
 * subroutine iocmd_tcllset ( interp, name, val )
 * ----------------------------------------------
 *
 * F77 interface routine to set a tcl variable
 *
 * Given:
 *  pointer to interpreter:   integer         interp(*)
 *  name of variable          character*64    name
 *  value for variable        character*256   val
 *
 */
void iocmd_tcllset_(interp, name, val )
  Tcl_Interp *interp;
  char name[64];
  char val[2048];
{
  Tcl_SetVar( interp, name, val, TCL_GLOBAL_ONLY);
}
/* 
 * subroutine iocmd_tclapplist ( interp, name, val )
 * -------------------------------------------------
 *
 * F77 interface routine to set a tcl list variable
 *
 * Given:
 *  pointer to interpreter:   integer         interp(*)
 *  name of variable          character*64    name
 *  value for variable        character*256   val
 *
 */
void iocmd_tclapplist_(interp, name, val )
  Tcl_Interp *interp;
  char name[64];
  char val[256];
{
  Tcl_SetVar( interp, name, val,
    TCL_GLOBAL_ONLY|TCL_LIST_ELEMENT|TCL_APPEND_VALUE);
}


/* 
 * subroutine iocmd_tclenqv ( interp, name, val )
 * ----------------------------------------------
 *
 * F77 interface routine to enquire a tcl variable
 *
 * Given:
 *  pointer to interpreter:   integer         interp(*)
 *  name of variable          character*64    name
 *  value for variable        character*256   val
 *
 */
void iocmd_tclenqv_(interp, name, val )
  Tcl_Interp *interp;
  char name[64];
  char val[256];
{
  val = Tcl_GetVar( interp, name, TCL_GLOBAL_ONLY);
}

