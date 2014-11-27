/*
 *  Routines to implement ida library functions
 *
 */
#include "tk.h"
#include "string.h"
#include "ic.h"

/*
 * Utility routines used in manipulating lists analysis
 */

img_vectList *img_allocVectList( v, vp )
   img_vect v;
   img_vectList *vp;
{
   img_vectList *newVect = (img_vectList *) malloc( sizeof(img_vectList) );
   newVect->v = v;
   newVect->vp = vp;
   return(newVect);
}

ida_nbList *ida_allocNbList( type, p )
   int     type;
   ida_nbList *p;
{
   ida_nbList *newNb = (ida_nbList *) malloc( sizeof(ida_nbList) );
   newNb->type = type;
   newNb->num = 0;
   newNb->p = p;
   return(newNb);
}

ida_nList *ida_allocNList( n, p )
   int        n;
   ida_nList *p;
{
   ida_nList *newN = (ida_nList *) malloc( sizeof(ida_nList) );
   newN->n = n;
   newN->p = p;
   return(newN);
}
