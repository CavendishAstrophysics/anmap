/*
 * Copyright I guess there should be some copywrite for this package,
 * 
 * 			Copyright (c) 1992
 * 
 * 	Liverpool University Department of Pure Mathematics,
 * 	Liverpool, L69 3BX, England.
 * 
 * 	Author Dr R. J. Morris.
 * 
 * 	e-mail rmorris@uk.ac.liv.uxb
 *
 * This software is copyrighted as noted above.  It may be freely copied,
 * modified, and redistributed, provided that the copyright notice is
 * preserved on all copies.
 *
 * There is no warranty or other guarantee of fitness for this software,
 * it is provided solely "as is".  Bug reports or fixes may be sent
 * to the authors, who may or may not act on them as they desire.
 *
 * You may not include this software in a program or other software product
 * without supplying the source, or without informing the end-user that the
 * source is available for no extra charge.
 *
 * If you modify this software, you should include a notice giving the
 * name of the person performing the modification, the date of modification,
 * and the reason for such modification.
 *
 * All this software is public domain, as long as it is not used by any military
 * establishment. Please note if you are a military establishment then a mutating
 * virus has now escaped into you computer and is presently turning all your
 * programs into socially useful, peaceful ones.
 * 
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#define I_AM_EQNFUNCTIONS
#include "eqn.h"
#include <varargs.h>
/*
#define SORT_ADD
#define PRINT_EXPANSION
#define PRINT_DIFF_FUN
*/
#define SILLYFUNS

#define grballoc(node) (node *) malloc(sizeof(node))
#define MAX(a,b)       a > b ? a : b ;
#define TRUE 1
#define FALSE 0

#define MAX_FUN_ARGS 10

/**************************************************************************/
/*                                                                        */
/* Now we hav a reverse polish calculator                                 */
/*   make_rpe( eqn , n , names[n] ) creates a reverse polish string       */
/*   eval_rpe( rpe, vars[] )        evaluates it using a stack            */
/*   clear_rpe_const()              clears the constant array             */
/*   check_rpe( rpe )               checks that rpe will be sucessfully   */
/*                                    evaluated.                          */
/*   print_rpe( rpe, n ,names[n] )  prints out the rp-string              */
/*                                                                        */
/*                                                                        */
/*   A stack is used for the evaluation of an rp-string and a global      */
/*   array of constants is used.                                          */
/*                                                                        */
/*   For example if eqn is                                                */
/*   (x+y)/(w-z)=0.7, n = 4 names[] = "x","y","z","w" the string produced */
/*   is " '`1 `2 + `4 `3 - / ``0 = "   where `1, `2, `3, `4 refere to     */
/*   the 1st, 2nd, 3rd and 4th names ``0 referes to constant[0]           */
/*   which is in this case 0.7.                                           */ 
/*   When evaluating we have the following sequence of ops                */
/*                                                                        */
/*   token    action              state of stack                          */
/*                                                                        */
/*     `1     push(vars[0])         x                                     */
/*     `2     push(vars[1])         y , x                                 */
/*      +     r = pop()             x                                     */
/*            l = pop()                                                   */
/*            push(l+r)             x+y                                   */
/*     `4     push(vars[3])         w, x+y                                */
/*     `3     push(vars[2])         z, w, x+y                             */
/*      -     r = pop()             w, x+y                                */
/*            l = pop()             x+y                                   */
/*            push(l-r)             w-z, x+y                              */
/*      /     r = pop()             x+y                                   */
/*            l = pop()                                                   */
/*            push(l/r)             (x+y)/(w-z)                           */
/*      ``0   push(const[0])        0.7, (x+y)/(w-z)                      */
/*      =     r = pop()             (x+y)/(w-z)                           */
/*            l = pop()                                                   */
/*            return(l-r)                                                 */
/*                                                                        */
/*   so we get a return value of (x+y)/(w-z) - 0.7                        */
/*                                                                        */
/*   the rpe string consists of integers coded as follows                 */
/*                                                                        */
/*  0 = rpe_name_base..rpe_op_base-1     variable( i )                     */
/*      rpe_op_base..rpe_const_base-1    op( i - rpe_op_base )            */
/*      rpe_const_base..                 constant( i - rpe_const_base )   */
/*                                                                        */
/*   the constants are stored in a global array 'rpe_const' and the       */
/*   global pointer 'rpe_const_ptr' contains the current top of list      */
/*                                                                        */
/*   DIAGNOSTIC                                                           */
/*      make_rpe() returns NULL if a valid rp-string can not be found     */
/*   in particular this will happen if eqn contains an unknown name.      */
/*                                                                        */
/**************************************************************************/

/**** Definitions used for the reverse polish calculator ****/

#define rpe_name_base 0
#define rpe_op_base 256
#define END_RPE rpe_op_base 
#define rpe_fun_base 512
#define rpe_fun_max  256
#define rpe_const_base (rpe_fun_base + rpe_fun_max)
#define rpe_const_max  10000

eqn_funs *rpe_funs[rpe_fun_max];
int rpe_fun_ptr = 0;
double rpe_const[rpe_const_max];
int rpe_const_ptr = 0 ;		/* the current position in array */
int rpe_ptr;

/**** A stack 'dbl_stack' is used for evaluating an rp-string *****/

#ifndef stack_size 
#  define stack_size 500
#endif
double dbl_stack[stack_size];

/*
 * Function:	make_rpe
 * Action:	constructs an rpe string,
 *		returns NULL if failed
*/

rpeint *make_rpe( eqn , n , names )
eqnode *eqn; int n; char **names;
{
  rpeint *temp;
  int rpe_size,i;

  if(eqn == NULL )
  {
	fprintf(stderr,"make_rpe: NULL eqnation\n");
	return(NULL);
  }

  if(names == NULL)
  {
	fprintf(stderr,"make_rpe: NULL names list\n");
	return(NULL);
  }
  for(i=0;i<n;++i)
  {
	if(names[i] == NULL)
	{
		fprintf(stderr,"make_rpe: NULL name number %d\n",i);
		return(NULL);
	}
  }
  rpe_size = count_eqn_tree( eqn ); /* the length of string is number of
                                         nodes in the equation */
  rpe_ptr = 0;
  temp = (rpeint *) calloc( rpe_size + 1, sizeof(rpeint) );
  temp[rpe_size] = END_RPE; /* a flag to signal end of string */
  if( !make_rpe2( temp, eqn, n, names ) || !check_rpe( temp, n, names ) )
  {
    free(temp);
    return(NULL);
  }
  return( temp );
}

/**** a recursive sub-routine called by make_rpe  ****/

int make_rpe2( rpe, eqn, n, names )
rpeint rpe[]; eqnode *eqn; int n; char **names;
{
  int leftres,rightres,i;
  double num;

  switch( eqn->op )
  {
  case NAME:
    for(i=0;i<n;++i)
      if( !strcmp( eqn->u.str, names[i] ) )
      {
        rpe[rpe_ptr++] = (rpeint) i + rpe_name_base;
        return( TRUE );
      }
    fprintf(stderr,"make_rpe2: name not found %s\n",eqn->u.str);
    return( FALSE );

  case NUMBER:
    for(i=0;i<rpe_const_ptr;++i)
    {
	if( rpe_const[i] == eqn->u.num )
	{
		rpe[rpe_ptr++] = (rpeint) rpe_const_base + i;
		return(TRUE);
	}
    }
    rpe[rpe_ptr++] = (rpeint) rpe_const_ptr + rpe_const_base ;
    rpe_const[rpe_const_ptr] = eqn->u.num;
    if ( ++rpe_const_ptr >= rpe_const_max )
    {
      fprintf(stderr,"make_rpe2: too many constants\n");
      return(FALSE);
    }
    return( TRUE );

  case '+': case '-': case '*': case '/': case '=':
    leftres = make_rpe2( rpe, eqn->u.n.l, n, names );
    rightres = make_rpe2( rpe, eqn->u.n.r, n, names );
    rpe[rpe_ptr++] = (rpeint) eqn->op + rpe_op_base;
    return( leftres && rightres );

  case '^':
    leftres = make_rpe2( rpe, eqn->u.n.l, n, names );
    if(eqn->u.n.r->op == NUMBER )
    {
	num = eqn->u.n.r->u.num;
	if( num - floor(num) < 1.0e-9 && num > 0.0 && num < 32768.0 )
	{
		rpe[rpe_ptr++] = (rpeint) INT_POW + rpe_op_base;
		rpe[rpe_ptr++] = (rpeint) num;
		return(leftres);
	}
    }
		
    rightres = make_rpe2( rpe, eqn->u.n.r, n, names );
    rpe[rpe_ptr++] = (rpeint) eqn->op + rpe_op_base;
    return( leftres && rightres );

  case ',':
    rightres = make_rpe2( rpe, eqn->u.n.r, n, names );
    leftres = make_rpe2( rpe, eqn->u.n.l, n, names );
    return( leftres && rightres );

  case FUNCTION:
	switch(eqn->u.f.f->type)
	{
	case CONSTANT_FUN:
	    for(i=0;i<rpe_const_ptr;++i)
	    {
		if( rpe_const[i] == eqn->u.f.f->val )
		{
			rpe[rpe_ptr++] = (rpeint) rpe_const_base + i;
			return(TRUE);
		}
	    }
	    rpe[rpe_ptr++] = (rpeint) rpe_const_ptr + rpe_const_base ;
	    rpe_const[rpe_const_ptr] = eqn->u.f.f->val;
	    if ( ++rpe_const_ptr >= rpe_const_max )
	    {
	      fprintf(stderr,"make_rpe2: too many constants\n");
	      return(FALSE);
	    }
	    return( TRUE );

	case OPERATOR:
		fprintf(stderr,"Can't convert equation involving operator %s into a reverse polish string\n",eqn->u.f.f->name);
		return(FALSE);

	case INTERN_FUN: case EXTERN_FUN:
	case INTERN_MAP: case EXTERN_MAP:
		if( count_eqn_args(eqn->u.f.a) != eqn->u.f.f->nvars )
		{
			fprintf(stderr,"Actual argument count different to profile for function %s\n",eqn->u.f.f->name);
			return(FALSE);
		}
	
		if( eqn->u.f.f->nvars > MAX_FUN_ARGS )
		{
			fprintf(stderr,"Sorry function %s has too many arguments to convert to reverse polish string\n",eqn->u.f.f->name);
			return(FALSE);
		}
	
		leftres = make_rpe2(rpe,eqn->u.f.a, n, names );

		/* Check the list of rpe_funs */

		for(i=0;i<rpe_fun_ptr;++i)
		{
			if(rpe_funs[i] == eqn->u.f.f)
			{
				rpe[rpe_ptr++] = (rpeint) rpe_fun_base + i;
				return(TRUE);
			}
		}
		rpe[rpe_ptr++] = (rpeint) rpe_fun_ptr + rpe_fun_base;
		rpe_funs[rpe_fun_ptr] = eqn->u.f.f;
		if( ++rpe_fun_ptr >= rpe_fun_max )
		{
			fprintf(stderr,"make_rpe2: too many functions\n");
			return(FALSE);
		}
		return(leftres);
	default:
		fprintf(stderr,"Bad function type %d in make_rpe\n",eqn->u.f.f->type);
		return(FALSE);
	}

  default:
	fprintf(stderr,"make_rpe2: bad op ");
        fprint_op(stderr,eqn->op);
        fprintf(stderr,"\n");
	break;
  }
  return(FALSE);
}

/*****
*     prints out the rpe string 'rpe'
*     'names' is an array of 'n' names.
*****/

print_rpe( rpe, names )
rpeint rpe[]; char **names;
{
  int ptr = 0;
  rpeint c;

  if( rpe == NULL )
  {
    printf("NULL reverse polish string\n");
    return;
  }

  do
  {
    c = rpe[ptr++];
    if( c < rpe_op_base )          printf("\tvar\t%s\n",names[c]); 
    else if ( c == rpe_op_base + INT_POW )
				   printf("\top\t^ %d\n",rpe[ptr++]);
    else if( c < rpe_fun_base )    printf("\top\t%c\n",c-rpe_op_base); 
    else if( c < rpe_const_base )  printf("\tfun\t%s\n",
					rpe_funs[c-rpe_fun_base]->name);
    else			   printf("\tconst\t%lf\n",
					rpe_const[c-rpe_const_base]);
  }
  while( c != END_RPE && c != rpe_op_base + '=' );
}

fprint_rpe(fp, rpe, names )
FILE *fp;
rpeint rpe[]; char **names;
{
  int ptr = 0;
  rpeint c;

  if( rpe == NULL )
  {
    fprintf(fp,"NULL reverse polish string\n");
    return;
  }

  do
  {
    c = rpe[ptr++];
    if( c < rpe_op_base )          fprintf(fp,"\tvar\t%s\n",names[c]); 
    else if ( c == rpe_op_base + INT_POW )
				   fprintf(fp,"\top\t^ %d\n",rpe[ptr++]);
    else if ( c < rpe_fun_base )   fprintf(fp,"\top\t%c\n",c-rpe_op_base); 
    else if ( c < rpe_const_base ) fprintf(fp,"\tfun\t%s\n",
					rpe_funs[c-rpe_fun_base]->name);
    else			   fprintf(fp,"\tconst\t%lf\n",
					rpe_const[c-rpe_const_base]);
  }
  while( c != END_RPE && c != rpe_op_base + '=' );
}

/*****
*     The no-holds bared rpe-calculator.
*     This has been tweeked for super fast operation.
*     Note 
*       all variables are registers.
*       the state of the stack is one behind what it should be
*         the top of stack is contained in 'r' 
*         this prevents wastful operations such as push(r); .. ; pop(r); 
*       the stack is an array but we use pointers for speed.
*       push and pop are defined by macros.
*****/

#define push_dbl(x) *(dbl_stack_ptr++) = x 
#define pop_dbl()   *(--dbl_stack_ptr) 

double *dbl_stack_base = dbl_stack; /* pointers quicker than arrays */

double eval_rpe( rpe, vars )
register rpeint *rpe; 		/*  All variables are registers  */
register double *vars;
{
  register rpeint c;
  register double l,r;
  register double *dbl_stack_ptr = dbl_stack_base;
  double   *old_stack_base;
  eqn_funs *fun;
  double fun_vars[MAX_FUN_ARGS];
  double *res;

  if( rpe == NULL )
  {
    fprintf(stderr,"eval_rpe: NULL reverse polish string\n");
    return(sqrt(-1.0));
  }

  while( TRUE )
  {
    c = *(rpe++);

    if( c < rpe_op_base ) 
    {
        push_dbl(r);			/* one extra push at begining */
        r = *(vars+c); 			/* r holds the top of stack */
    }

    else if( c >= rpe_const_base ) 
    {
        push_dbl(r);
        r = rpe_const[ c - rpe_const_base ];
    }

    else if( c >= rpe_fun_base )
    {
	fun =  rpe_funs[c - rpe_fun_base];
	switch(fun->type)
	{
	case EXTERN_FUN:
	    switch( fun->nvars )
	    {
	    case 1: r = (*fun->fun)(r); break;
	    case 2: r = (*fun->fun)(r,pop_dbl()); break;
	    case 3: 
		r = (*fun->fun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2));
		dbl_stack_ptr -= 2;
		break;

	    case 4: 
		r = (*fun->fun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3));
		dbl_stack_ptr -= 3;
		break;

	    case 5: 
		r = (*fun->fun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4));
		dbl_stack_ptr -= 4;
		break;

	    case 6: 
		r = (*fun->fun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4),
			 *(dbl_stack_ptr-5));
		dbl_stack_ptr -= 5;
		break;

	    case 7: 
		r = (*fun->fun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4),
			 *(dbl_stack_ptr-5), *(dbl_stack_ptr-6));
		dbl_stack_ptr -= 6;
		break;

	    case 8: 
		r = (*fun->fun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4),
			 *(dbl_stack_ptr-5), *(dbl_stack_ptr-6),
			 *(dbl_stack_ptr-7));
		dbl_stack_ptr -= 7;
		break;

	    case 9: 
		r = (*fun->fun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4),
			 *(dbl_stack_ptr-5), *(dbl_stack_ptr-6),
			 *(dbl_stack_ptr-7), *(dbl_stack_ptr-8));
		dbl_stack_ptr -= 8;
		break;

	    case 10: 
		r = (*fun->fun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4),
			 *(dbl_stack_ptr-5), *(dbl_stack_ptr-6),
			 *(dbl_stack_ptr-7), *(dbl_stack_ptr-8),
			 *(dbl_stack_ptr-9));
		dbl_stack_ptr -= 9;
		break;

	    default:
		break;
	    }
	    break;

	case INTERN_FUN:
	    switch( fun->nvars )
	    {
	    case 1:
		fun_vars[0] = r;
		break;
	    case 2:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		break;
	    case 3:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		break;
	    case 4:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		break;
	    case 5:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		break;

	    case 6:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		fun_vars[5] = pop_dbl();
		break;

	    case 7:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		fun_vars[5] = pop_dbl();
		fun_vars[6] = pop_dbl();
		break;

	    case 8:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		fun_vars[5] = pop_dbl();
		fun_vars[6] = pop_dbl();
		fun_vars[7] = pop_dbl();
		break;

	    case 9:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		fun_vars[5] = pop_dbl();
		fun_vars[6] = pop_dbl();
		fun_vars[7] = pop_dbl();
		fun_vars[8] = pop_dbl();
		break;

	    case 10:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		fun_vars[5] = pop_dbl();
		fun_vars[6] = pop_dbl();
		fun_vars[7] = pop_dbl();
		fun_vars[8] = pop_dbl();
		fun_vars[9] = pop_dbl();
		break;

	    default:
		break;
	    }
  	    old_stack_base = dbl_stack_base;
  	    dbl_stack_base = dbl_stack_ptr;
	    r = eval_rpe(fun->rpe,fun_vars);
  	    dbl_stack_base = old_stack_base;
	    break;

	case EXTERN_MAP:
	    switch( fun->nvars )
	    {
	    case 1: res = (*fun->vfun)(r); break;
	    case 2: res = (*fun->vfun)(r,pop_dbl()); break;
	    case 3: 
		res = (*fun->vfun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2));
		dbl_stack_ptr -= 2;
		break;

	    case 4: 
		res = (*fun->vfun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3));
		dbl_stack_ptr -= 3;
		break;

	    case 5: 
		res = (*fun->vfun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4));
		dbl_stack_ptr -= 4;
		break;

	    case 6: 
		res = (*fun->vfun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4),
			 *(dbl_stack_ptr-5));
		dbl_stack_ptr -= 5;
		break;

	    case 7: 
		res = (*fun->vfun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4),
			 *(dbl_stack_ptr-5), *(dbl_stack_ptr-6));
		dbl_stack_ptr -= 6;
		break;

	    case 8: 
		res = (*fun->vfun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4),
			 *(dbl_stack_ptr-5), *(dbl_stack_ptr-6),
			 *(dbl_stack_ptr-7));
		dbl_stack_ptr -= 7;
		break;

	    case 9: 
		res = (*fun->vfun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4),
			 *(dbl_stack_ptr-5), *(dbl_stack_ptr-6),
			 *(dbl_stack_ptr-7), *(dbl_stack_ptr-8));
		dbl_stack_ptr -= 8;
		break;

	    case 10: 
		res = (*fun->vfun)(r, *(dbl_stack_ptr-1), *(dbl_stack_ptr-2),
			 *(dbl_stack_ptr-3), *(dbl_stack_ptr-4),
			 *(dbl_stack_ptr-5), *(dbl_stack_ptr-6),
			 *(dbl_stack_ptr-7), *(dbl_stack_ptr-8),
			 *(dbl_stack_ptr-9));
		dbl_stack_ptr -= 9;
		break;

	    default:
		break;
	    }
	    switch(fun->dim)
	    {
		case 1: r = res[0]; break;
		case 2: r = res[0]; push_dbl(res[1]); break;
		case 3: r = res[0]; push_dbl(res[2]); push_dbl(res[1]); break;
	    }
	    break;

	case INTERN_MAP:
	    switch(fun->nvars)
	    {
	    case 1:
		fun_vars[0] = r;
		break;
	    case 2:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		break;
	    case 3:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		break;
	    case 4:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		break;
	    case 5:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		break;

	    case 6:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		fun_vars[5] = pop_dbl();
		break;

	    case 7:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		fun_vars[5] = pop_dbl();
		fun_vars[6] = pop_dbl();
		break;

	    case 8:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		fun_vars[5] = pop_dbl();
		fun_vars[6] = pop_dbl();
		fun_vars[7] = pop_dbl();
		break;

	    case 9:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		fun_vars[5] = pop_dbl();
		fun_vars[6] = pop_dbl();
		fun_vars[7] = pop_dbl();
		fun_vars[8] = pop_dbl();
		break;

	    case 10:
		fun_vars[0] = r;
		fun_vars[1] = pop_dbl();
		fun_vars[2] = pop_dbl();
		fun_vars[3] = pop_dbl();
		fun_vars[4] = pop_dbl();
		fun_vars[5] = pop_dbl();
		fun_vars[6] = pop_dbl();
		fun_vars[7] = pop_dbl();
		fun_vars[8] = pop_dbl();
		fun_vars[9] = pop_dbl();
		break;

	    default:
		break;
	    }
  	    old_stack_base = dbl_stack_base;
  	    dbl_stack_base = dbl_stack_ptr; /* set global stack ptr */
	    switch(fun->dim)
	    {
		case 1:
	    		r = eval_rpe(fun->vrpe[0],fun_vars);
			break;
		case 2:
	    		r = eval_rpe(fun->vrpe[1],fun_vars); push_dbl(r);
  	    		dbl_stack_base = dbl_stack_ptr;
	    		r = eval_rpe(fun->vrpe[0],fun_vars);
			break;
		case 3:
	    		r = eval_rpe(fun->vrpe[2],fun_vars); push_dbl(r);
  	    		dbl_stack_base = dbl_stack_ptr;
	    		r = eval_rpe(fun->vrpe[1],fun_vars); push_dbl(r);
  	    		dbl_stack_base = dbl_stack_ptr;
	    		r = eval_rpe(fun->vrpe[0],fun_vars);
			break;
	    }
  	    dbl_stack_base = old_stack_base;
	    break;

	} /* End switch fun->type */
    }   

    else		/* Its got to be an op */
      switch( c )
      {
      case '+' + rpe_op_base:
        l = pop_dbl();
        r +=  l;
        break;
      case '-' + rpe_op_base:
        l = pop_dbl();
        r =  l-r;
        break;
      case '*' + rpe_op_base:
        l = pop_dbl();
        r = l*r;
        break;
      case '/' + rpe_op_base:
        l = pop_dbl();
        r = l/r;
        break;
      case '^' + rpe_op_base:
        l = pop_dbl();
        r = pow(l,r);
        break;
      case INT_POW + rpe_op_base:
    	c = *(rpe++);
	l = 1.0;
	for(;c>0;--c) l = l*r;
	r = l;
	break;
      case '=' + rpe_op_base:
        l = pop_dbl();
	pop_dbl();	/* One extra pop at end */
        return( l-r );
      case END_RPE:
	pop_dbl();	/* One extra pop at end */
	return(r);
      }
   }
} 

#undef push_dbl
#undef pop_dbl

/*****
*     a routine to check that the rp-string will evaluate ok
*     'n' is the number of variables which are expected.
*****/

#define StackERR {\
      if( stackptr >= stack_size ) { \
        fprintf(stderr,"check_rpe: stack full, require %d\n",stackptr); \
        fprintf(stderr,"           please recompile with 'cc -Dstack_size=999 ...\n"); \
        flag =  FALSE; } }

int check_rpe( rpe, n, names )
rpeint rpe[], n;
char **names;
{
  int ptr = 0,i;
  rpeint c;
  eqn_funs *fun;
  static int stackptr=0;
  auto   int initStackPtr = stackptr;
  int flag = TRUE;

  if( rpe == NULL )
  {
    fprintf(stderr,"check_rpe: NULL reverse polish string\n");
    return(FALSE);
  }

  do
  {
    c = rpe[ptr++];

    if( c < rpe_op_base )          /* c a variable */
    {
      StackERR;
      ++stackptr;
      if( c >= n )
      {
        fprintf(stderr,"check_rpe: bad variable referance var[%d]\n",c);
        flag =  FALSE;
      }
    }

    else if( c < rpe_fun_base )  /* c an operator */
    {
      switch( c )
      {
      case '+' + rpe_op_base:
      case '-' + rpe_op_base:
      case '*' + rpe_op_base:
      case '/' + rpe_op_base:
      case '^' + rpe_op_base:
      case '=' + rpe_op_base:
	--stackptr;
      break;
      case INT_POW + rpe_op_base:
        c = rpe[ptr++];
      break;
      }
    }

    else if( c < rpe_const_base ) /* a function */
    {
      fun = rpe_funs[c-rpe_fun_base];
      switch( fun->type )
      {
      case EXTERN_FUN:
      	stackptr -= fun->nvars - 1;
      	StackERR;
	break;
      
      case INTERN_FUN:
      	stackptr -= fun->nvars - 1;
      	StackERR;
	flag = flag && check_rpe(fun->rpe,fun->nvars,fun->vars);
	break;

      case EXTERN_MAP:
      	stackptr -= fun->nvars - fun->dim;
      	StackERR;
	break;

      case INTERN_MAP:
      	stackptr -= fun->nvars - fun->dim;
      	StackERR;
	for(i=0;i<fun->dim;++i)
	    flag = flag && check_rpe(fun->vrpe[i],fun->nvars,fun->vars);
	break;
      
      default:
	fprintf(stderr,"Bad type for function when checking rpe %d\n",
		fun->type);
	break;
      }
    }

    else			/*  c a constant */
    {
      StackERR;
      ++stackptr;
      if( c < rpe_const_base || c >= rpe_const_base + rpe_const_max )
      {
        fprintf(stderr,"check_rpe: bad constant ref, const[%d]\n",c-rpe_const_base);
        flag =  FALSE;
      }
    }    /* end if */

  } while( c != END_RPE );

  if( --stackptr != initStackPtr )
  {
	fprintf(stderr,"check_rpe: stack corupted on exit: initial %d final %d\n",
		initStackPtr,stackptr);
	fprint_rpe(stderr,rpe,names);
/*
  	flag = FALSE;
*/
	stackptr = initStackPtr;
  }
  if( !flag ) stackptr = initStackPtr;
  return( flag );
}

/****  resets constant stack to zero ****/

clear_rpe_const() {rpe_const_ptr = 0;}
