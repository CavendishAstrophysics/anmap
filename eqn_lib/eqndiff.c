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
#include <ctype.h>
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
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

/************************************************************************/
/*									*/
/*	Now differentiate the equation.					*/
/*	Using chain & product rule.					*/
/*	No simplification occurs.					*/
/*									*/
/************************************************************************/

diff_wrt(base,var_name)
eqnode *base;
char *var_name;
{
	eqnode *left1, *left2, *right1, *right2, *leftnode, *rightnode;
	int	i,failed,leftcount,rightcount;

	if( base == NULL )
	{
		fprintf(stderr,"Tried to differentiate a NULL equation\n");
		return;
	}
	failed = ! isalpha(var_name[0]);
	i = 1;
	while( var_name[i] != '\0' && !failed )
	{
		if( !isalnum(var_name[i]) && var_name[i] != '_' )
			failed = TRUE;
		++i;
	}
	if(failed)
	{
		fprintf(stderr,"bad name for differentiation %s\n",var_name);
		return;
	}
	
	switch( base->op )
	{
	case FUNCTION:
		if(base->u.f.f->type != CONSTANT_FUN )
			return(diff_fun_wrt(base,var_name));

	case NUMBER:
		base->op = NUMBER;
		base->u.num = 0.0;
		break;

	case BRACKET:
		return(diff_wrt(base->u.n.r,var_name));

	case NAME:
		if( !strcmp(var_name,base->u.str) )
		{
			free( base->u.str );
			base->op = NUMBER;
			base->u.num = 1.0;
		}
		else
		{
			free( base->u.str );
			base->op = NUMBER;
			base->u.num = 0.0;
		}
		break;

	case '+': case '-': case '=': case ',':
		diff_wrt(base->u.n.l,var_name);
		diff_wrt(base->u.n.r,var_name);
		break;

	case '^':	/* a^n -> (n * a') * (a^(n-1)) */
	/* should really be a^b -> d(a^b)_da da_dx + d(a^b)_db db_dx */

	    leftcount = count_eqn_args(base->u.n.l);
	    rightcount = count_eqn_args(base->u.n.r);
	    if(leftcount != rightcount )
	    {
		fprintf(stderr,"Different counts while differentiating '^' %d %d\n",leftcount,rightcount);
		diff_wrt(base->u.n.l,var_name);
		diff_wrt(base->u.n.r,var_name);
		break;
	    }
	    else if( leftcount == 1 )
	    {
		left1 = base->u.n.l;
		left2 = duplicate(left1);
		right1 = base->u.n.r;
		diff_wrt(left1,var_name);
		base->op = '*';
		base->u.n.l = grballoc(eqnode);
		base->u.n.l->op = '*';
		base->u.n.l->u.n.l = duplicate(right1);
		base->u.n.l->u.n.r = left1;
		base->u.n.r = grballoc(eqnode);
		base->u.n.r->op = '^';
		base->u.n.r->u.n.l = left2;
		base->u.n.r->u.n.r = grballoc(eqnode);
		base->u.n.r->u.n.r->op = '-';
		base->u.n.r->u.n.r->u.n.l = right1;
		base->u.n.r->u.n.r->u.n.r = grballoc(eqnode);
		base->u.n.r->u.n.r->u.n.r->op = NUMBER;
		base->u.n.r->u.n.r->u.n.r->u.num = 1.0;
		break;
	    }

	    /* if leftcount != 1 fall through to do cross product */

	case '*': case '.':	/* a * b -> ( da * b ) + ( a * db ) */

		left1 = base->u.n.l;
		left2 = duplicate( left1 );
		right1 = base->u.n.r;
		right2 = duplicate( right1 );
		diff_wrt(left1,var_name);
		diff_wrt(right2,var_name);
		leftnode = grballoc( eqnode );
		rightnode = grballoc( eqnode );
		leftnode->op = base->op;
		rightnode->op = base->op;
		leftnode->u.n.l = left1;
		leftnode->u.n.r = right1;
		rightnode->u.n.l = left2;
		rightnode->u.n.r = right2;
		base->op = '+';
		base->u.n.l = leftnode;
		base->u.n.r = rightnode;
		break;

	case '/':	/* a / b -> (a' * b - a * b')/(b * b)   */

		left1 = base->u.n.l;
		left2 = duplicate(left1);
		right1 = base->u.n.r;
		right2 = duplicate( right1 );
		diff_wrt(left1,var_name);
		diff_wrt(right2,var_name);
		base->op = '/';
		base->u.n.l = grballoc(eqnode);
		base->u.n.l->op = '-';
		base->u.n.l->u.n.l = grballoc(eqnode);
		base->u.n.l->u.n.l->op = '*';
		base->u.n.l->u.n.l->u.n.l = left1;
		base->u.n.l->u.n.l->u.n.r = right1;
		base->u.n.l->u.n.r = grballoc(eqnode);
		base->u.n.l->u.n.r->op = '*';
		base->u.n.l->u.n.r->u.n.l = left2;
		base->u.n.l->u.n.r->u.n.r = right2;
		base->u.n.r = grballoc(eqnode);
		base->u.n.r->op = '*';
		base->u.n.r->u.n.l = duplicate(right1);
		base->u.n.r->u.n.r = duplicate(right1);
		break;


	default:
		fprintf(stderr,"diff_wrt couldn't handel code %d\n",base->op);
	} /* end switch */

	return(TRUE);
}

/************************************************************************/
/*									*/
/*	Now to differentiate a function					*/
/*	df(a,b,c)_dx = df_da da_dx + df_db db_dx + df_dc dc_dx		*/
/*									*/
/************************************************************************/

diff_fun_wrt(base,name)
eqnode *base;
char *name;
{
	eqnode *args,*diff,*temp,*diffarg,*new = NULL;
	eqnode sub,subvarnode;
	eqn_funs *fun;
	int	i,j;
	char	newvar[3];
	int	failed;

	if( base == NULL )
	{
		fprintf(stderr,"Tried to differentiate a NULL equation\n");
		return;
	}
	if( base->op != FUNCTION )
	{
		fprintf(stderr,"Tried to differentiate a function which is not a function\n");
		return;
	}
	if( base->u.f.f->type == OPERATOR )
	{
		fprintf(stderr,"Don't know how to differentiate operator %s\n",
			base->u.f.f->name);
		return;
	}

	failed = ! isalpha(name[0]);
	i = 1;
	while( name[i] != '\0' && !failed )
	{
		if( !isalnum(name[i]) && name[i] != '_' )
			failed = TRUE;
		++i;
	}
	if(failed)
	{
		fprintf(stderr,"bad name for differentiation %s\n",name);
		return;
	}

	/* Make a copy of arguments */

#ifdef PRINT_DIFF_FUN
	fprintf(stderr,"diff_fun_wrt:\n");
	fprint_eqn(stderr,base);
	fprintf(stderr,"\n");
#endif

	args = duplicate(base->u.f.a);
	diff_wrt(base->u.f.a,name);	/* a,b,c --> da,db,dc */

	/* Initilise two nodes for substitution */

	sub.op = '=';
	subvarnode.op = NAME;
	subvarnode.u.str = newvar;
	newvar[0] = '@'; newvar[2] = '\0';

	/* Get pointer to function */

	fun = base->u.f.f;
	base->u.n.r = base->u.f.a;

	/* Now loop through all arguments replacing da by da * df_da */

	for(i=0; i< fun->nvars; ++i)
	{
		diff = duplicate(fun->diff[i]);

		/* replace the varible names in diff by @1 @2 @3 etc */
	
		sub.u.n.r = &subvarnode;
		sub.u.n.l = grballoc(eqnode);
		sub.u.n.l->op = NAME;

		for(j=0;j<fun->nvars;++j)
		{
			sub.u.n.l->u.str = fun->vars[j];
			newvar[1] = '1'+j;
			substitute(diff,&sub);
		}

		/* Now substitute the arguments into diff */

		free(sub.u.n.l);
		sub.u.n.l = &subvarnode;
		diffarg = args;

		for(j=0;j<fun->nvars;++j)
		{
			newvar[1] = '1'+j;

			sub.u.n.r = get_eqn_arg(args,j+1);
			substitute(diff,&sub);
		}

		/* Great now have da_dx, next use chain rule
			multiply by df_da and add to new */

		if( new == NULL )
		{
			new = grballoc(eqnode);
			new->op = '*';
			new->u.n.l = diff;
			new->u.n.r = duplicate(get_eqn_arg(base->u.n.r,i+1));
		}
		else
		{
			temp = grballoc(eqnode);
			temp->op = '+';
			temp->u.n.l = new;
			temp->u.n.r = grballoc(eqnode);
			temp->u.n.r->op = '*';
			temp->u.n.r->u.n.l = diff;
			temp->u.n.r->u.n.r = duplicate(get_eqn_arg(base->u.n.r,i+1));
			new = temp;
		}
	}
	free_eqn_tree(base->u.n.r);
	free_eqn_tree(args);
	copy_node(base,new);
	return(TRUE);
}

/*
 * Function:	diff_wrt_eqn
 * Action:	performs differentiation of left wrt right
 *		where left and right are seperated by a comma
 */

eqnode *diff_wrt_eqn(eqn)
eqnode *eqn;
{
	eqn_node *temp;

	if( eqn == NULL )
	{
		fprintf(stderr,"Tried to differentiate a null equation\n");
		return(NULL);
	}

	if( eqnop(eqn) != ',' || eqnop(eqnr(eqn)) != NAME )
	{
		fprintf(stderr,"Must have 'eqn,name' for differentiation\n");
		fprint_eqn(stderr,eqn);
		fprintf(stderr,"\n");
		return(eqn);
	}

	temp = eqnl(eqn);
	diff_wrt(temp,eqnname(eqnr(eqn)));
	free_eqn_node(eqnr(eqn));
	free_eqn_node(eqn);
	return(temp);
}
