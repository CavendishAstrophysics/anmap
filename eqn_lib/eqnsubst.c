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

/**************************************************************************/
/*                                                                        */
/* Now some routines for handeling substitution.                          */
/* 'assignment' can be used with 'substitute' to assign parameters        */
/*     in equations.                                                      */
/*     eg.  temp = assignment("a",5.0);                                   */
/*          substitute( main, temp );                                     */
/*     changes each occurance of "a" in 'main' to be 5.0.                 */
/*                                                                        */
/**************************************************************************/

/*****
*     'sub' is an equation of the form 
*        name = eqn
*     each apperance of 'name' in the equation 'base' is replaced by 'eqn'
*****/

substitute( base, sub )
eqnode *base,*sub;
{
  eqnode *temp;

  if( sub == NULL )
  {
	fprintf(stderr,"substitute: sub eqn is NULL\n");
	return(FALSE);
  }
  if( base == NULL )
  {
	fprintf(stderr,"substitute: base eqn is NULL\n");
	return(FALSE);
  }
  if( sub->op != '=' )
  {
	fprintf(stderr,"substitute: sub not an assignment\n");
	fprint_eqn(stderr,sub);
	fprintf(stderr,"\n");
    return(FALSE);
  }
  if( sub->u.n.l == NULL || sub->u.n.r == NULL)
  {
	fprintf(stderr,"Bad substitution equation\n");
        fprint_eqn(stderr,sub);
	fprintf(stderr,"\n");
	return(FALSE);
  }

  if( sub->u.n.l->op == NAME)
  {
	if( base->op == NAME && !strcmp( sub->u.n.l->u.str , base->u.str ) )
	{
	/*****   names match now copy ************/

		free(base->u.str);
		temp = duplicate(sub->u.n.r);
		copy_node(base,temp);
		return(TRUE);
	}
  }

  /*** Sub is a macro definition ***/

  else if( sub->u.n.l->op == '*' && sub->u.n.l->u.n.l->op == NAME )
  {
	if( base->op == '*' && base->u.n.l->op == NAME
		&& !strcmp(base->u.n.l->u.str,sub->u.n.l->u.n.l->u.str ) )
	{
		eqnode subnode, subvarnode;
		eqnode *args,*baseargs,*temp;
		char	varname[3];
		int	i,num;

		substitute(base->u.n.r,sub);
		temp = duplicate(sub->u.n.r);

		/* First changes names in sub to @1, @2, ... */

		args = sub->u.n.l->u.n.r;
		subnode.op = '=';
		subnode.u.n.r = &subvarnode;
		subvarnode.op = NAME;
		subvarnode.u.str = varname;
		varname[0] = '@';
		varname[2] = '\0';
		num = count_eqn_args(args);

		if( num != count_eqn_args(base->u.n.r) )
		{
			fprintf(stderr,"Different number of arguments in profile and actual occurance\n");
			fprintf(stderr,"Profile: ");
			fprint_eqn(stderr,sub->u.n.l);
			fprintf(stderr,"\nActual: ");
			fprint_eqn(stderr,base);
			return(FALSE);
		}

		for(i=1;i<=num;++i)
		{
			subnode.u.n.l = get_eqn_arg(args,i);
			varname[1] = '0' + i;
			substitute(temp,&subnode);
		}

		/* Now substitute @1, @2, ... by args in base */

		subnode.u.n.l = &subvarnode;

		for(i=1;i<=num;++i)
		{
			subnode.u.n.r = get_eqn_arg(base->u.n.r,i);
			varname[1] = '0' + i;
			substitute(temp,&subnode);
		}
		free_eqn_tree(base->u.n.l);
		free_eqn_tree(base->u.n.r);
		copy_node(base,temp);
		free(temp);
		return(TRUE);
	}
  }
  else
  {
    fprintf(stderr,"substitute: bad substitution equation:\n");
    fprint_eqn(stderr,sub);
    fprintf(stderr,"\n");
    return(FALSE);
  }

  /* The base didn't match  recursivly check rest */

  switch( base->op )
  {
  case NAME:
  case NUMBER:
	break;

  case '+': case '-': case '*': case '/': case '^': case '=': case ',':
  case '.':
	substitute( base->u.n.l, sub );
	substitute( base->u.n.r, sub );
	break;

  case BRACKET:
	substitute( base->u.n.r, sub );
	break;

  case FUNCTION:
	if( base->u.f.f->type != CONSTANT_FUN )
		substitute(base->u.f.a,sub);
	break;

  default:
	fprintf(stderr,"substitute: bad op ");
        fprint_op(stderr,base->op);
        fprintf(stderr,"\n");
	break;
  }
  return(TRUE);
}

/*****
*     'assign' creates an equation of the form
*        'name' = value
*****/

eqnode *assign(name,val)
char *name; double val;
{
  eqnode *temp;
  
  temp = grballoc( eqnode );
  temp->op = '=';
  temp->u.n.l = grballoc( eqnode );
  temp->u.n.l->op = NAME;
  temp->u.n.l->u.str = (char *) calloc( strlen(name)+1, sizeof(char) );
  strcpy(temp->u.n.l->u.str,name);
  temp->u.n.r = grballoc( eqnode );
  temp->u.n.r->op = NUMBER;
  temp->u.n.r->u.num = val;
  return(temp);
}
