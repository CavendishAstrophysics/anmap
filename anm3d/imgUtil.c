/*
 *  Routines to implement image library functions
 *
 */
#include "tk.h"
#include "string.h"
#include "ic.h"

/*
 *  Check for a valid image vector
 *
 */
int img_VecCheck( defn, vec )
   img_defn defn;
   img_vect vec;
{
   int indx;
   if ( (vec.x>=defn.x1) && (vec.x<=defn.x2) &&
        (vec.y>=defn.y1) && (vec.y<=defn.y2) &&
        (vec.z>=defn.z1) && (vec.z<=defn.z2) &&
        (vec.t>=defn.t1) && (vec.t<=defn.t2) &&
        (vec.v>=defn.v1) && (vec.v<=defn.v2) ) {
       indx = 1;
   } else {
       indx = 0;
   }
   return indx;
}

/*
 *  Return an index given an image vector
 *
 */
int img_Vec2Index( defn, vec )
   img_defn defn;
   img_vect vec;
{
   int indx;
   if (img_VecCheck( defn, vec ) == 1) {
     indx = (vec.t-defn.t1)*(defn.vdim*defn.xdim*defn.ydim*defn.zdim) +
            (vec.z-defn.z1)*(defn.vdim*defn.xdim*defn.ydim) +
            (vec.y-defn.y1)*(defn.vdim*defn.xdim) +
            (vec.x-defn.x1)*(defn.vdim) +
             vec.v - 1;
     if ( (indx < 0) || (indx >= defn.ndata) ) {
	  indx = -1;
     }
   } else {
     indx = -1;
   }
   return indx;
}

/*
 *  Return an image vector given an index
 *
 */
void img_Index2Vec( defn, indx, vec )
   img_defn defn;
   int      indx;
   img_vect *vec;
{
   int n;

   if (  (indx >= 0) && (indx <= defn.ndata) ) {
     n = indx;
     vec->t = n/(defn.vdim*defn.xdim*defn.ydim*defn.zdim);
     n -= vec->t*(defn.vdim*defn.xdim*defn.ydim*defn.zdim);
     vec->z = n/(defn.vdim*defn.xdim*defn.ydim);
     n -= vec->z*(defn.vdim*defn.xdim*defn.ydim);
     vec->y = n/(defn.vdim*defn.xdim);
     n -= vec->y*(defn.vdim*defn.xdim);
     vec->x = n/(defn.vdim);
     vec->v = n - vec->x*(defn.vdim);
     vec->x += defn.x1; vec->y += defn.y1; vec->z += defn.z1;
     vec->t += defn.t1; vec->v += defn.v1;
   }
}


/*
 *  Initialise counters for moving through a sub-image
 *
 */
int img_IndexInit( defn0, defn, ndata, vec )
   img_defn defn0, defn;
   int *ndata;
   img_vect *vec;
{

   vec->x = defn.x1; vec->xr = defn.x2 - defn.x1;
   vec->y = defn.y1; vec->yr = defn.y2 - defn.y1;
   vec->z = defn.z1; vec->zr = defn.z2 - defn.z1;
   vec->t = defn.t1; vec->tr = defn.t2 - defn.t1;
   vec->v = defn.v1; vec->vr = defn.v2 - defn.v1;

   *ndata = 	(defn.x2 - defn.x1 + 1) * (defn.y2 - defn.y1 + 1) *
		(defn.z2 - defn.z1 + 1) * (defn.t2 - defn.t1 + 1) *
		(defn.v2 - defn.v1 + 1);

   return img_Vec2Index( defn0, *vec );
}

/*
 *  Return next pixel in a sub-image
 *
 */
int img_IndexNext( defn0, defn, vec )
   img_defn defn0, defn;
   img_vect *vec;
{
   if (vec->vr) {
	vec->v++ ;
	if (vec->v > defn.v2) {
	     vec->v = defn.v1 ; vec->x++ ;
	     if (vec->x > defn.x2) {
		vec->x = defn.x1 ; vec->y++ ;
		if (vec->y > defn.y2) {
		   vec->y = defn.y1 ; vec->z++ ;
		   if (vec->z > defn.z2) {
		       vec->z = defn.z1 ; vec->t++ ;
		   }
		}
	     }
	}
   } else {
	vec->x++ ;
	if (vec->x > defn.x2) {
	    vec->x = defn.x1 ; vec->y++ ;
	    if (vec->y > defn.y2) {
		   vec->y = defn.y1 ; vec->z++ ;
		   if (vec->z > defn.z2) {
		       vec->z = defn.z1 ; vec->t++ ;
		   }
	    }
	}
   }
   return img_Vec2Index( defn0, *vec );
}

/*
 *  Functions to return image data
 *
 */
double img_accImg2( img, dx, dy )
   double img, dx, dy;
{
   double d;
   img_defn  defn;
   img_vect  vect;
   int       ih;

   ih = img + 0.5; 
   if ( (ih >= 0) && (ih < HASH_SIZE) && (ic_hash[ih].mode == 1) ) {
	defn = ic_hash[ih].defn;
	vect.x = dx + 0.5 ; vect.y = dy + 0.5 ; vect.z = defn.z1 ;
	vect.t = defn.t1 ; vect.v = defn.v1 ;
	d = defn.data_p[ img_Vec2Index( defn, vect ) ];
   } else {
	d = 0.0;
   }
   return d;
}

double img_accImg3( img, dx, dy, dz )
   double img, dx, dy, dz;
{
   double d;
   img_defn  defn;
   img_vect  vect;
   int       ih;

   ih = img + 0.5; 
   if ( (ih >= 0) && (ih < HASH_SIZE) && (ic_hash[ih].mode == 1) ) {
	defn = ic_hash[ih].defn;
	vect.x = dx + 0.5 ; vect.y = dy + 0.5 ; vect.z = dz + 0.5 ;
	vect.t = defn.t1 ; vect.v = defn.v1 ;
	d = defn.data_p[ img_Vec2Index( defn, vect ) ];
   } else {
	d = 0.0;
   }
   return d;
}

double img_accImg2v( img, dv, dx, dy )
   double img, dv, dx, dy;
{
   double d;
   img_defn  defn;
   img_vect  vect;
   int       ih;

   ih = img + 0.5; 
   if ( (ih >= 0) && (ih < HASH_SIZE) && (ic_hash[ih].mode == 1) ) {
	defn = ic_hash[ih].defn;
	vect.x = dx + 0.5 ; vect.y = dy + 0.5 ; vect.z = defn.z1 ;
	vect.t = defn.t1 ; vect.v = dv + 0.5 ;
	d = defn.data_p[ img_Vec2Index( defn, vect ) ];
   } else {
	d = 0.0;
   }
   return d;
}

double img_accImg3v( img, dv, dx, dy, dz )
   double img, dv, dx, dy, dz;
{
   double d;
   img_defn  defn;
   img_vect  vect;
   int       ih;

   ih = img + 0.5; 
   if ( (ih >= 0) && (ih < HASH_SIZE) && (ic_hash[ih].mode == 1) ) {
	defn = ic_hash[ih].defn;
	vect.x = dx + 0.5 ; vect.y = dy + 0.5 ; vect.z = dz + 0.5 ;
	vect.t = defn.t1 ; vect.v = dv + 0.5 ;
	d = defn.data_p[ img_Vec2Index( defn, vect ) ];
   } else {
	d = 0.0;
   }
   return d;
}

double img_intImg2( img, dx, dy )
   double img, dx, dy;
{
   double d;
   float  c;
   img_defn  defn;
   img_vectF vect;
   int       ih;

   ih = img + 0.5; 
   if ( (ih >= 0) && (ih < HASH_SIZE) && (ic_hash[ih].mode == 1) ) {
	defn = ic_hash[ih].defn;
	vect.x = dx; vect.y = dy; vect.z = defn.z1 ;
	vect.t = defn.t1 ; vect.v = defn.v1 ;
	img_getvalue_( &defn, defn.data_p, vect, c ); d = c;
   } else {
	d = 0.0;
   }
   return d;
}

double img_intImg3( img, dx, dy, dz )
   double img, dx, dy, dz;
{
   double d;
   float  c;
   img_defn  defn;
   img_vectF vect;
   int       ih;

   ih = img + 0.5; 
   if ( (ih >= 0) && (ih < HASH_SIZE) && (ic_hash[ih].mode == 1) ) {
	defn = ic_hash[ih].defn;
	vect.x = dx; vect.y = dy; vect.z = dz;
	vect.t = defn.t1 ; vect.v = defn.v1 ;
	img_getvalue_( &defn, defn.data_p, vect, c ); d = c;
   } else {
	d = 0.0;
   }
   return d;
}

double img_intImg2v( img, dv, dx, dy )
   double img, dv, dx, dy;
{
   double d;
   float  c;
   img_defn  defn;
   img_vectF vect;
   int       ih;

   ih = img + 0.5; 
   if ( (ih >= 0) && (ih < HASH_SIZE) && (ic_hash[ih].mode == 1) ) {
	defn = ic_hash[ih].defn;
	vect.x = dx; vect.y = dy; vect.z = defn.z1 ;
	vect.t = defn.t1 ; vect.v = dv + 0.5 ;
	img_getvalue_( &defn, defn.data_p, vect, c ); d = c;
   } else {
	d = 0.0;
   }
   return d;
}

double img_intImg3v( img, dv, dx, dy, dz )
   double img, dv, dx, dy, dz;
{
   double d;
   float  c;
   img_defn  defn;
   img_vectF vect;
   int       ih;

   ih = img + 0.5; 
   if ( (ih >= 0) && (ih < HASH_SIZE) && (ic_hash[ih].mode == 1) ) {
	defn = ic_hash[ih].defn;
	vect.x = dx; vect.y = dy; vect.z = dz;
	vect.t = defn.t1 ; vect.v = dv + 0.5 ;
	img_getvalue_( &defn, defn.data_p, vect, c ); d = c;
   } else {
	d = 0.0;
   }
   return d;
}

