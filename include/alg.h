#include <gl.h>
#include <trace.h>
#include <math.h>
#include <stdio.h>
#include <device.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <eqn.h>

#ifndef MAXORDER
#  define MAXORDER 25
#endif
#define MAXORDERT2 2*MAXORDER
#define MAXORDERP2 MAXORDER+2

extern eqnode *duplicate(), *set_parameter();
extern int *make_rpe();
extern double eval_rpe();
