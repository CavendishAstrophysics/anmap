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

typedef rpeint vrpeint;

/**************************************************************************/
/*                                                                        */
/* Now we hav a reverse polish calculator                                 */
/*   make_vrpe( eqn , n , names[n] ) creates a reverse polish string       */
/*   eval_vrpe( vrpe, vars[] )        evaluates it using a stack            */
/*   clear_vrpe_const()              clears the constant array             */
/*   check_vrpe( vrpe )               checks that vrpe will be sucessfully   */
/*                                    evaluated.                          */
/*   print_vrpe( vrpe, n ,names[n] )  prints out the rp-string              */
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
/*   the vrpe string consists of integers coded as follows                 */
/*                                                                        */
/*  0 = vrpe_name_base..vrpe_op_base-1     variable( i )                     */
/*      vrpe_op_base..vrpe_const_base-1    op( i - vrpe_op_base )            */
/*      vrpe_const_base..                 constant( i - vrpe_const_base )   */
/*                                                                        */
/*   the constants are stored in a global array 'vrpe_const' and the       */
/*   global pointer 'vrpe_const_ptr' contains the current top of list      */
/*                                                                        */
/*   DIAGNOSTIC                                                           */
/*      make_vrpe() returns NULL if a valid rp-string can not be found     */
/*   in particular this will happen if eqn contains an unknown name.      */
/*                                                                        */
/**************************************************************************/

/**** Definitions used for the reverse polish calculator ****/

#define vrpe_name_base 0
#define vrpe_op_base 256
#define END_RPE vrpe_op_base 
#define vrpe_fun_base 512
#define vrpe_fun_max  256
#define vrpe_const_base (vrpe_fun_base + vrpe_fun_max)
#define vrpe_const_max  10000

eqn_funs *vrpe_funs[vrpe_fun_max];
int vrpe_fun_ptr = 0;
double vrpe_const[vrpe_const_max];
int vrpe_const_ptr = 0 ;		/* the current position in array */
int vrpe_ptr;

/**** A stack 'dbl_stack' is used for evaluating an rp-string *****/

#ifndef stack_size 
#  define stack_size 500
#endif
double dbl_stack[stack_size];

/*
 * Function:	make_vrpe
 * Action:	constructs an vrpe string,
 *		returns NULL if failed
*/

vrpeint *make_vrpe( eqn , n , names )
eqnode *eqn; int n; char **names;
{
  vrpeint *temp;
  int vrpe_size,i;

  if(eqn == NULL )
  {
	fprintf(stderr,"make_vrpe: NULL eqnation\n");
	return(NULL);
  }

  if(names == NULL)
  {
	fprintf(stderr,"make_vrpe: NULL names list\n");
	return(NULL);
  }
  for(i=0;i<n;++i)
  {
	if(names[i] == NULL)
	{
		fprintf(stderr,"make_vrpe: NULL name number %d\n",i);
		return(NULL);
	}
  }
  vrpe_size = count_eqn_tree( eqn ); /* the length of string is number of
                                         nodes in the equation */
  vrpe_ptr = 0;
  temp = (vrpeint *) calloc( vrpe_size + 1, sizeof(vrpeint) );
  temp[vrpe_size] = END_RPE; /* a flag to signal end of string */
  if( !make_vrpe2( temp, eqn, n, names )
   || !check_vrpe( temp, n, names ,count_eqn_args(eqn)) )
  {
    free(temp);
    return(NULL);
  }
  return( temp );
}

/**** a recursive sub-routine called by make_vrpe  ****/

int make_vrpe2( vrpe, eqn, n, names )
vrpeint vrpe[]; eqnode *eqn; int n; char **names;
{
  int leftres,rightres,leftcount,rightcount,i;
  double num;

  switch( eqn->op )
  {
  case NAME:
    for(i=0;i<n;++i)
      if( !strcmp( eqn->u.str, names[i] ) )
      {
        vrpe[vrpe_ptr++] = (vrpeint) i + vrpe_name_base;
        return( TRUE );
      }
    fprintf(stderr,"make_vrpe2: name not found %s\n",eqn->u.str);
    return( FALSE );

  case NUMBER:
    for(i=0;i<vrpe_const_ptr;++i)
    {
	if( vrpe_const[i] == eqn->u.num )
	{
		vrpe[vrpe_ptr++] = (vrpeint) vrpe_const_base + i;
		return(TRUE);
	}
    }
    vrpe[vrpe_ptr++] = (vrpeint) vrpe_const_ptr + vrpe_const_base ;
    vrpe_const[vrpe_const_ptr] = eqn->u.num;
    if ( ++vrpe_const_ptr >= vrpe_const_max )
    {
      fprintf(stderr,"make_vrpe2: too many constants\n");
      return(FALSE);
    }
    return( TRUE );

  case '+':
    rightres = make_vrpe2( vrpe, eqn->u.n.r, n, names );
    leftres = make_vrpe2( vrpe, eqn->u.n.l, n, names );
    leftcount = count_eqn_args(eqn->u.n.l);
    rightcount = count_eqn_args(eqn->u.n.r);
    if( leftcount != rightcount )
    {
	fprintf(stderr,"make_vrpe: different counts for + %d %d\n",
		leftcount,rightcount);
	return(FALSE);
    }
    switch(leftcount)
    {
	case 1: vrpe[vrpe_ptr++] = SUM1 + vrpe_op_base; break;
	case 2: vrpe[vrpe_ptr++] = SUM2 + vrpe_op_base; break;
	case 3: vrpe[vrpe_ptr++] = SUM3 + vrpe_op_base; break;
	default:
		fprintf(stderr,"make_vrpe: bad count %d\n",leftcount);
		return(FALSE);
    }
    return( leftres && rightres );

  case '-': case '=':
    leftres = make_vrpe2( vrpe, eqn->u.n.l, n, names );
    rightres = make_vrpe2( vrpe, eqn->u.n.r, n, names );
    leftcount = count_eqn_args(eqn->u.n.l);
    rightcount = count_eqn_args(eqn->u.n.r);
    if( leftcount != rightcount )
    {
	fprintf(stderr,"make_vrpe: different counts for + %d %d\n",
		leftcount,rightcount);
	return(FALSE);
    }
    switch(leftcount)
    {
	case 1: vrpe[vrpe_ptr++] = SUB1 + vrpe_op_base; break;
	case 2: vrpe[vrpe_ptr++] = SUB2 + vrpe_op_base; break;
	case 3: vrpe[vrpe_ptr++] = SUB3 + vrpe_op_base; break;
	default:
		fprintf(stderr,"make_vrpe: bad count %d\n",leftcount);
		return(FALSE);
    }
    return( leftres && rightres );

  case '*':
    leftcount = count_eqn_args(eqn->u.n.l);
    rightcount = count_eqn_args(eqn->u.n.r);
    if( leftcount != 1 && rightcount != 1)
    {
	fprintf(stderr,"make_vrpe: bad counts for '*' %d %d\n",
		leftcount,rightcount);
	return(FALSE);
    }
    if(leftcount == 1)
    {
	    rightres = make_vrpe2( vrpe, eqn->u.n.r, n, names );
	    leftres = make_vrpe2( vrpe, eqn->u.n.l, n, names );
	    switch(rightcount)
	    {
		case 1: vrpe[vrpe_ptr++] = MULT1 + vrpe_op_base; break;
		case 2: vrpe[vrpe_ptr++] = SCALE2 + vrpe_op_base; break;
		case 3: vrpe[vrpe_ptr++] = SCALE3 + vrpe_op_base; break;
		default:
			fprintf(stderr,"make_vrpe: bad count %d\n",rightcount);
			return(FALSE);
	    }
	    return( leftres && rightres );
    }
    else
    {
		/* reverse order */

	    leftres = make_vrpe2( vrpe, eqn->u.n.l, n, names );
	    rightres = make_vrpe2( vrpe, eqn->u.n.r, n, names );
	    switch(leftcount)
	    {
		case 1: vrpe[vrpe_ptr++] = MULT1 + vrpe_op_base; break;
		case 2: vrpe[vrpe_ptr++] = SCALE2 + vrpe_op_base; break;
		case 3: vrpe[vrpe_ptr++] = SCALE3 + vrpe_op_base; break;
		default:
			fprintf(stderr,"make_vrpe: bad count %d\n",leftcount);
			return(FALSE);
	    }
	    return( leftres && rightres );
    }

  case '/':
    rightres = make_vrpe2( vrpe, eqn->u.n.r, n, names );
    leftres = make_vrpe2( vrpe, eqn->u.n.l, n, names );
    leftcount = count_eqn_args(eqn->u.n.l);
    rightcount = count_eqn_args(eqn->u.n.r);
    if( leftcount != rightcount || leftcount != 1 )
    {
	fprintf(stderr,"make_vrpe: bad counts for / %d %d\n",
		leftcount,rightcount);
	return(FALSE);
    }
    vrpe[vrpe_ptr++] = DIV1 + vrpe_op_base;
    return( leftres && rightres );

  case '^':
    rightcount = count_eqn_args(eqn->u.n.r);
    leftcount = count_eqn_args(eqn->u.n.l);
    if( leftcount != rightcount )
    {
	fprintf(stderr,"make_vrpe: bad counts for / %d %d\n",
		leftcount,rightcount);
	return(FALSE);
    }
    if( leftcount == 3 )	/* Cross product */
    {
	rightres = make_vrpe2( vrpe, eqn->u.n.r, n, names );
	leftres = make_vrpe2( vrpe, eqn->u.n.l, n, names );
	vrpe[vrpe_ptr++] = CROSS3 + vrpe_op_base;
	return(leftres && rightres);
    }
    else			/* Power */
    {
	leftres = make_vrpe2( vrpe, eqn->u.n.l, n, names );
	if(eqn->u.n.r->op == NUMBER )
	{
		num = eqn->u.n.r->u.num;
		if( num - floor(num) < 1.0e-9 && num > 0.0 && num < 32768.0 )
		{
			vrpe[vrpe_ptr++] = (vrpeint) INT_POW + vrpe_op_base;
			vrpe[vrpe_ptr++] = (vrpeint) num;
			return(leftres);
		}
	}
	rightres = make_vrpe2( vrpe, eqn->u.n.r, n, names );
	vrpe[vrpe_ptr++] = POW1 + vrpe_op_base;
	return( leftres && rightres );
    }

  case '.':
    rightres = make_vrpe2( vrpe, eqn->u.n.r, n, names );
    leftres = make_vrpe2( vrpe, eqn->u.n.l, n, names );
    leftcount = count_eqn_args(eqn->u.n.l);
    rightcount = count_eqn_args(eqn->u.n.r);
    if( leftcount != rightcount )
    {
	fprintf(stderr,"make_vrpe: different counts for '.' %d %d\n",
		leftcount,rightcount);
	return(FALSE);
    }
    switch(leftcount)
    {
	case 1: vrpe[vrpe_ptr++] = MULT1 + vrpe_op_base; break;
	case 2: vrpe[vrpe_ptr++] = DOT2 + vrpe_op_base; break;
	case 3: vrpe[vrpe_ptr++] = DOT3 + vrpe_op_base; break;
	default:
		fprintf(stderr,"make_vrpe: bad count %d\n",leftcount);
		return(FALSE);
    }
    return( leftres && rightres );

  case ',':
    rightres = make_vrpe2( vrpe, eqn->u.n.r, n, names );
    leftres = make_vrpe2( vrpe, eqn->u.n.l, n, names );
    return( leftres && rightres );

  case FUNCTION:
	switch(eqn->u.f.f->type)
	{
	case CONSTANT_FUN:
	    for(i=0;i<vrpe_const_ptr;++i)
	    {
		if( vrpe_const[i] == eqn->u.f.f->val )
		{
			vrpe[vrpe_ptr++] = (vrpeint) vrpe_const_base + i;
			return(TRUE);
		}
	    }
	    vrpe[vrpe_ptr++] = (vrpeint) vrpe_const_ptr + vrpe_const_base ;
	    vrpe_const[vrpe_const_ptr] = eqn->u.f.f->val;
	    if ( ++vrpe_const_ptr >= vrpe_const_max )
	    {
	      fprintf(stderr,"make_vrpe2: too many constants\n");
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
	
		leftres = make_vrpe2(vrpe,eqn->u.f.a, n, names );

		/* Check the list of vrpe_funs */

		for(i=0;i<vrpe_fun_ptr;++i)
		{
			if(vrpe_funs[i] == eqn->u.f.f)
			{
				vrpe[vrpe_ptr++] = (vrpeint) vrpe_fun_base + i;
				return(TRUE);
			}
		}
		vrpe[vrpe_ptr++] = (vrpeint) vrpe_fun_ptr + vrpe_fun_base;
		vrpe_funs[vrpe_fun_ptr] = eqn->u.f.f;
		if( ++vrpe_fun_ptr >= vrpe_fun_max )
		{
			fprintf(stderr,"make_vrpe2: too many functions\n");
			return(FALSE);
		}
		return(leftres);
	default:
		fprintf(stderr,"Bad function type %d in make_vrpe\n",eqn->u.f.f->type);
		return(FALSE);
	}

  default:
	fprintf(stderr,"make_vrpe2: bad op ");
        fprint_op(stderr,eqn->op);
        fprintf(stderr,"\n");
	break;
  }
  return(FALSE);
}

/*****
*     prints out the vrpe string 'vrpe'
*     'names' is an array of 'n' names.
*****/

print_vrpe( vrpe, names )
vrpeint vrpe[]; char **names;
{
  int ptr = 0;
  vrpeint c;

  if( vrpe == NULL )
  {
    printf("NULL reverse polish string\n");
    return;
  }

  do
  {
    c = vrpe[ptr++];
    if( c < vrpe_op_base )          printf("\tvar\t%s\n",names[c]); 
    else if( c < vrpe_fun_base ) {  printf("\top\t");
				    print_op(c-vrpe_op_base); 
				    printf("\n"); }
    else if( c < vrpe_const_base )  printf("\tfun\t%s\n",
					vrpe_funs[c-vrpe_fun_base]->name);
    else			   printf("\tconst\t%lf\n",
					vrpe_const[c-vrpe_const_base]);
  }
  while( c != END_RPE && c != vrpe_op_base + '=' );
}

fprint_vrpe(fp, vrpe, names )
FILE *fp;
vrpeint vrpe[]; char **names;
{
  int ptr = 0;
  vrpeint c;

  if( vrpe == NULL )
  {
    fprintf(fp,"NULL reverse polish string\n");
    return;
  }

  do
  {
    c = vrpe[ptr++];
    if( c < vrpe_op_base )          fprintf(fp,"\tvar\t%s\n",names[c]); 
    else if ( c < vrpe_fun_base ) { fprintf(fp,"\top\t");
				    fprint_op(fp,c-vrpe_op_base); 
				    fprintf(fp,"\n"); }
    else if ( c < vrpe_const_base ) fprintf(fp,"\tfun\t%s\n",
					vrpe_funs[c-vrpe_fun_base]->name);
    else			   fprintf(fp,"\tconst\t%lf\n",
					vrpe_const[c-vrpe_const_base]);
  }
  while( c != END_RPE && c != vrpe_op_base + '=' );
}

/*****
*     The no-holds bared vrpe-calculator.
*     This has been tweeked for super fast operation.
*     Note 
*       all variables are registers.
*       the state of the stack is one behind what it should be
*         the top of stack is contained in 'r' 
*         this prevents wastful operations such as push(r); .. ; pop(r); 
*       the stack is an array but we use pointers for speed.
*       push and pop are defined by macros.
*****/

#define push_dbl(x) *(--sp) = x 
#define pop_dbl()   *(sp++) 

double *dbl_vstack_base = dbl_stack + stack_size; /* pointers quicker than arrays */

double *eval_vrpe( vrpe, vars )
register vrpeint *vrpe; 		/*  All variables are registers  */
register double *vars;
{
  register vrpeint c;
  register double l,r,t;
  register double *sp = dbl_vstack_base;
  double   *old_stack_base;
  eqn_funs *fun;
  double fun_vars[MAX_FUN_ARGS];
  double *res;

  if( vrpe == NULL )
  {
    fprintf(stderr,"eval_vrpe: NULL reverse polish string\n");
    *dbl_vstack_base = sqrt(-1.0);
    return(dbl_vstack_base);
  }

  while( TRUE )
  {
    c = *(vrpe++);

    if( c < vrpe_op_base ) 
    {
        push_dbl(r);			/* one extra push at begining */
        r = *(vars+c); 			/* r holds the top of stack */
    }

    else if( c >= vrpe_const_base ) 
    {
        push_dbl(r);
        r = vrpe_const[ c - vrpe_const_base ];
    }

    else if( c >= vrpe_fun_base )
    {
	fun =  vrpe_funs[c - vrpe_fun_base];
	switch(fun->type)
	{
	case EXTERN_FUN:
	    switch( fun->nvars )
	    {
	    case 1: r = (*fun->fun)(r); break;
	    case 2: r = (*fun->fun)(r,*sp); sp += 1; break;
	    case 3: r = (*fun->fun)(r, *sp, *(sp+1)); sp += 2; break;

	    case 4: 
		r = (*fun->fun)(r, *(sp),
			 *(sp+1), *(sp+2));
		sp += 3;
		break;

	    case 5: 
		r = (*fun->fun)(r, *(sp),
			 *(sp+1), *(sp+2),
			 *(sp+3));
		sp += 4;
		break;

	    case 6: 
		r = (*fun->fun)(r, *(sp),
			 *(sp+1), *(sp+2),
			 *(sp+3), *(sp+4));
		sp += 5;
		break;

	    case 7: 
		r = (*fun->fun)(r, *(sp),
			 *(sp+1), *(sp+2),
			 *(sp+3), *(sp+4),
			 *(sp+5));
		sp += 6;
		break;

	    case 8: 
		r = (*fun->fun)(r, *(sp),
			 *(sp+1), *(sp+2),
			 *(sp+3), *(sp+4),
			 *(sp+5), *(sp+6));
		sp += 7;
		break;

	    case 9: 
		r = (*fun->fun)(r, *(sp),
			 *(sp+1), *(sp+2),
			 *(sp+3), *(sp+4),
			 *(sp+5), *(sp+6),
			 *(sp+7));
		sp += 8;
		break;

	    case 10: 
		r = (*fun->fun)(r, *(sp),
			 *(sp+1), *(sp+2),
			 *(sp+3), *(sp+4),
			 *(sp+5), *(sp+6),
			 *(sp+7), *(sp+8));
		sp += 9;
		break;

	    default:
		break;
	    }
	    break;

	case INTERN_FUN:
	    push_dbl(r);
  	    old_stack_base = dbl_vstack_base;
  	    dbl_vstack_base = sp;
	    r = eval_rpe(fun->rpe,sp);
  	    dbl_vstack_base = old_stack_base;
	    sp += fun->nvars;
	    break;

#ifdef NOT_DEF
	case EXTERN_MAP:
	    switch( fun->nvars )
	    {
	    case 1: res = (*fun->vfun)(r); break;
	    case 2: res = (*fun->vfun)(r,pop_dbl()); break;
	    case 3: 
		res = (*fun->vfun)(r, *(sp-1), *(sp-2));
		sp -= 2;
		break;

	    case 4: 
		res = (*fun->vfun)(r, *(sp-1), *(sp-2),
			 *(sp-3));
		sp -= 3;
		break;

	    case 5: 
		res = (*fun->vfun)(r, *(sp-1), *(sp-2),
			 *(sp-3), *(sp-4));
		sp -= 4;
		break;

	    case 6: 
		res = (*fun->vfun)(r, *(sp-1), *(sp-2),
			 *(sp-3), *(sp-4),
			 *(sp-5));
		sp -= 5;
		break;

	    case 7: 
		res = (*fun->vfun)(r, *(sp-1), *(sp-2),
			 *(sp-3), *(sp-4),
			 *(sp-5), *(sp-6));
		sp -= 6;
		break;

	    case 8: 
		res = (*fun->vfun)(r, *(sp-1), *(sp-2),
			 *(sp-3), *(sp-4),
			 *(sp-5), *(sp-6),
			 *(sp-7));
		sp -= 7;
		break;

	    case 9: 
		res = (*fun->vfun)(r, *(sp-1), *(sp-2),
			 *(sp-3), *(sp-4),
			 *(sp-5), *(sp-6),
			 *(sp-7), *(sp-8));
		sp -= 8;
		break;

	    case 10: 
		res = (*fun->vfun)(r, *(sp-1), *(sp-2),
			 *(sp-3), *(sp-4),
			 *(sp-5), *(sp-6),
			 *(sp-7), *(sp-8),
			 *(sp-9));
		sp -= 9;
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
  	    old_stack_base = dbl_vstack_base;
  	    dbl_vstack_base = sp; /* set global stack ptr */
	    switch(fun->dim)
	    {
		case 1:
	    		r = eval_vrpe(fun->vrpe[0],fun_vars);
			break;
		case 2:
	    		r = eval_vrpe(fun->vrpe[1],fun_vars); push_dbl(r);
  	    		dbl_vstack_base = sp;
	    		r = eval_vrpe(fun->vrpe[0],fun_vars);
			break;
		case 3:
	    		r = eval_vrpe(fun->vrpe[2],fun_vars); push_dbl(r);
  	    		dbl_vstack_base = sp;
	    		r = eval_vrpe(fun->vrpe[1],fun_vars); push_dbl(r);
  	    		dbl_vstack_base = sp;
	    		r = eval_vrpe(fun->vrpe[0],fun_vars);
			break;
	    }
  	    dbl_vstack_base = old_stack_base;
	    break;

#endif
	} /* End switch fun->type */
    }   

    else		/* Its got to be an op */

	/* Note for speed all the cases are close in value
		this results in faster switch statement on iris */
      switch( c )
      {
      case SUM1 + vrpe_op_base:
        r += pop_dbl();
        break;

      case SUB1 + vrpe_op_base:
        l = pop_dbl();
	r = l - r;
        break;

      case MULT1 + vrpe_op_base:
        r *= pop_dbl();
        break;

      case DIV1 + vrpe_op_base:
        r /= pop_dbl();
        break;

      case POW1 + vrpe_op_base:
        l = pop_dbl();
        r = pow(r,l);
        break;

      case INT_POW + vrpe_op_base:
    	c = *(vrpe++);
	l = 1.0;
	for(;c>0;--c) l = l*r;
	r = l;
	break;

      case SUM2 + vrpe_op_base:
	r += *(sp+1);
	*(sp+2) += *(sp);
	sp += 2;
	break;

      case SUM3 + vrpe_op_base:
	r += *(sp+2);
	*(sp+3) += *sp;
	*(sp+4) += *(sp+1);
	sp += 3;
	break;

      case SUB2 + vrpe_op_base:
	r = *(sp+1) - r;
	*(sp+2) -= *sp;
	sp += 2;
	break;

      case SUB3 + vrpe_op_base:
	r = *(sp+2) - r;
	*(sp+3) -= *sp;
	*(sp+4) -= *(sp+1);
	sp += 3;
	break;

      case DOT2 + vrpe_op_base:
	r = *(sp+1) * r + *sp * *(sp+2);
	sp += 3;
	break;

      case DOT3 + vrpe_op_base:
	r = *(sp+2)  *r
	  + *sp * *(sp+3)
	  + *(sp+1) * *(sp+4);
	sp += 5;
	break;

      case SCALE2 + vrpe_op_base:
	*(sp+1) *= r;
	r *= *(sp);
	sp++;
	break;

      case SCALE3 + vrpe_op_base:
	*(sp+1) *= r;
	*(sp+2) *= r;
	r *= *(sp);
	sp++;
	break;

      case CROSS3 + vrpe_op_base:
	l = r * *(sp+3) - *(sp) * *(sp+2);
	t = *(sp+1) * *(sp+2) - r * *(sp+4);
	r = *sp * *(sp+4) - *(sp+1) * *(sp+3);
	sp += 3;
	*sp = t;
	*(sp+1) = l;
	break;

      case EQUALS1 + vrpe_op_base:
        l = pop_dbl();
	r = l-r;
	push_dbl(r);
        return( sp );

      case END_RPE:
	push_dbl(r);
        return( sp );
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
        fprintf(stderr,"check_vrpe: stack full, require %d\n",stackptr); \
        fprintf(stderr,"           please recompile with 'cc -Dstack_size=999 ...\n"); \
        flag =  FALSE; } }

int check_vrpe( vrpe, n, names, nargs)
vrpeint vrpe[]; int n,nargs;
char **names;
{
  int ptr = 0,i;
  vrpeint c;
  eqn_funs *fun;
  static int stackptr=0;
  auto   int initStackPtr = stackptr;
  int flag = TRUE;

  if( vrpe == NULL )
  {
    fprintf(stderr,"check_vrpe: NULL reverse polish string\n");
    return(FALSE);
  }

  do
  {
    c = vrpe[ptr++];

    if( c < vrpe_op_base )          /* c a variable */
    {
      StackERR;
      ++stackptr;
      if( c >= n )
      {
        fprintf(stderr,"check_vrpe: bad variable referance var[%d]\n",c);
        flag =  FALSE;
      }
    }

    else if( c < vrpe_fun_base )  /* c an operator */
    {
      switch( c )
      {
      case SUM1 + vrpe_op_base:
      case SUB1 + vrpe_op_base:
      case MULT1 + vrpe_op_base:
      case DIV1 + vrpe_op_base:
      case POW1 + vrpe_op_base:
      case EQUALS1 + vrpe_op_base:
	--stackptr;
        break;
      case SUM2 + vrpe_op_base:
      case SUB2 + vrpe_op_base:
	stackptr -= 2;
	break;
      case SUM3 + vrpe_op_base:
      case SUB3 + vrpe_op_base:
	stackptr -= 3;
	break;
      case DOT2 + vrpe_op_base:
	stackptr -= 2;
	break;
      case DOT3 + vrpe_op_base:
	stackptr -= 3;
	break;
      case SCALE2 + vrpe_op_base:
      case SCALE3 + vrpe_op_base:
	--stackptr;
	break;
      case CROSS3 + vrpe_op_base:
	stackptr -= 3;
	break;
      case INT_POW + vrpe_op_base:
        c = vrpe[ptr++];
        break;
      case END_RPE:
	break;

      default:
	fprintf(stderr,"Bad op "); fprint_op(stderr,c-vrpe_op_base); fprintf(stderr," in check_vrpe\n");
	break;
      }
    }

    else if( c < vrpe_const_base ) /* a function */
    {
      fun = vrpe_funs[c-vrpe_fun_base];
      switch( fun->type )
      {
      case EXTERN_FUN:
      	stackptr -= fun->nvars - 1;
      	StackERR;
	break;
      
      case INTERN_FUN:
      	stackptr -= fun->nvars - 1;
      	StackERR;
	flag = flag && check_vrpe(fun->rpe,fun->nvars,fun->vars);
	break;

      case EXTERN_MAP:
      	stackptr -= fun->nvars - fun->dim;
      	StackERR;
	break;

      case INTERN_MAP:
      	stackptr -= fun->nvars - fun->dim;
      	StackERR;
	for(i=0;i<fun->dim;++i)
	    flag = flag && check_vrpe(fun->vrpe[i],fun->nvars,fun->vars);
	break;
      
      default:
	fprintf(stderr,"Bad type for function when checking vrpe %d\n",
		fun->type);
	break;
      }
    }

    else			/*  c a constant */
    {
      StackERR;
      ++stackptr;
      if( c < vrpe_const_base || c >= vrpe_const_base + vrpe_const_max )
      {
        fprintf(stderr,"check_vrpe: bad constant ref, const[%d]\n",c-vrpe_const_base);
        flag =  FALSE;
      }
    }    /* end if */

  } while( c != END_RPE );

  if( stackptr != initStackPtr + nargs )
  {
	fprintf(stderr,"check_vrpe: stack corupted on exit: initial %d final %d\n",
		initStackPtr,stackptr);
	fprint_vrpe(stderr,vrpe,names);
/*
  	flag = FALSE;
*/
	stackptr = initStackPtr;
  }
  if( !flag ) stackptr = initStackPtr;
  return( flag );
}

/****  resets constant stack to zero ****/

clear_vrpe_const() {vrpe_const_ptr = 0;}
