/* ttt.f -- translated by f2c (version 19950602).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int ttt_(a, n1, n2)
real *a;
integer *n1, *n2;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    static integer i, j;
    static real s;

    /* Parameter adjustments */
    a_dim1 = *n1;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n1;
    for (i = 1; i <= i__1; ++i) {
	i__2 = *n2;
	for (j = 1; j <= i__2; ++j) {
	    s += a[i + j * a_dim1];
	}
    }
} /* ttt_ */

