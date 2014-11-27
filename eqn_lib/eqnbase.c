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
#define I_AM_EQNBASE
#include "eqn.h"
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

/*************************************************************************/
/*                                                                       */
/*  Global Varibles                                                      */
/*                                                                       */
/*************************************************************************/

int eqnin_mode;     /* mode of input file/stdin/string */
FILE *eqnin_file;   /* input file for equation         */
char *eqnin_str;    /* input string for equation       */
eqnode *equation_base; /* a pointer the base of the equation tree being read */
                       /* this is assigned in eeparse                        */

extern int ptrerrstring;

/*************************************************************************/
/*                                                                       */
/* first some subroutines to call the parser                             */
/*                                                                       */
/*************************************************************************/

/**** input from stdin ****/

eqnode *scan_eqn()
{
  eqnin_mode = EQNFROM_STDIN;
  ptrerrstring = 0;
  eeparse();
  return(equation_base);
}

/**** input from a file ****/

eqnode *fscan_eqn(fp)
FILE *fp;
{
  eqnin_mode = EQNFROM_FILE;
  eqnin_file = fp;
  ptrerrstring = 0;
  eeparse();
  return(equation_base);
}

/**** input from a string ****/

eqnode *sscan_eqn(string)
char *string;
{
  char stringblock[160];

  eqnin_mode = EQNFROM_STRING;
  if( strlen(string) >= 160 ) 
  {
    fprintf(stderr,"sscan_eqn: string to long <%s>\n",string);
    exit(-1);
  }
  eqnin_str = stringblock;
  strcpy( eqnin_str, string);
  ptrerrstring = 0;
  eeparse();
  return(equation_base);
}

/*************************************************************************/
/*                                                                       */
/* Now some routines for basic tree handeling                            */
/*                                                                       */
/*************************************************************************/

/**** displays the equation in a tree fashion ****/

display_eqn( base, depth )
eqnode *base; int depth;
{
  int i;
 
  if( base == NULL )
  {
	fprintf(stderr,"display_eqn: base == NULL\n");
	return;
  }

  switch( base->op )
  {
  case NAME:
    for(i=1;i<=depth;++i) printf("    ");
    printf("%s\n",base->u.str);
    break;

  case NUMBER:
    for(i=1;i<=depth;++i) printf("    ");
    printf("%lf\n",base->u.num);
    break;

  case FUNCTION:
    for(i=1;i<=depth;++i) printf("    ");
    printf("%s\n",base->u.f.f->name);
    if(base->u.f.f->type != CONSTANT_FUN)
    	display_eqn(base->u.f.a,depth+1);
    break;
 
  case '+': case '-': case '*': case '/': case '^': case '=': case ',':
  case '.':
    display_eqn(base->u.n.l,depth+1);
    for(i=1;i<=depth;++i) printf("    ");
    printf("%c\n",base->op);
    display_eqn(base->u.n.r,depth+1);
    break;

  default:
    fprintf(stderr,"display_eqn: bad op ");
        fprint_op(stderr,base->op);
        fprintf(stderr,"\n");
  }
}

/**** prints the equation with lots of brackets! ****/

fprint_eqn(file, base )
FILE *file;
eqnode *base;
{
  if( base == NULL )
  {
	fprintf(stderr,"NULL\007");
	return;
  }

  switch( base->op )
  {
  case FUNCTION:
    if(base->u.f.f->type == CONSTANT_FUN)
    {
	fprintf(file,"%s",base->u.f.f->name);
	break;
    }
    fprintf(file,"%s(",base->u.f.f->name);
    fprint_eqn(file,base->u.f.a);
    fprintf(file,")");
    break;
  case NAME:
    fprintf(file,"%s",base->u.str);
    break;
  case NUMBER:
    if( eqnval(base) == rint(eqn_val(base)) ) 
	fprintf(file,"%.0lf",eqn_val(base));
    else if( eqnval(base)*10.0 == rint(eqn_val(base)*10.0) ) 
	fprintf(file,"%.1lf",eqn_val(base));
    else if( eqnval(base)*100.0 == rint(eqn_val(base)*100.0) ) 
	fprintf(file,"%.2lf",eqn_val(base));
    else if( eqnval(base)*1000.0 == rint(eqn_val(base)*1000.0) ) 
	fprintf(file,"%.3lf",eqn_val(base));
    else if( eqnval(base)*10000.0 == rint(eqn_val(base)*10000.0) ) 
	fprintf(file,"%.4lf",eqn_val(base));
    else
	fprintf(file,"%lf",eqn_val(base));
    break;
  case BRACKET:
    fprintf(file,"(");
    fprint_eqn(file,base->u.n.r);
    fprintf(file,")");
    break;

  case INTERVAL:
    fprintf(file,"[");
    fprint_eqn(file,base->u.n.l);
    fprintf(file,",");
    fprint_eqn(file,base->u.n.r);
    fprintf(file,"]");
    break;
  case '+':
    switch( base->u.n.l->op )
    {
    case '^': case '*': case '/': case '+': case NAME: case NUMBER:
    case FUNCTION:
      fprint_eqn(file,base->u.n.l); break;
    default:	/* ',' and '-' and '=' */
      fprintf(file,"("); fprint_eqn(file,base->u.n.l); fprintf(file,")"); break;
    }

    fprintf(file,"+");

    switch( base->u.n.r->op )
    {
    case '^': case '*': case '/': case '+': case NAME: case NUMBER:
    case FUNCTION:
      fprint_eqn(file,base->u.n.r); break;
    default:
      fprintf(file,"("); fprint_eqn(file,base->u.n.r); fprintf(file,")"); break;
    }
    break;
  case '-':
    switch( base->u.n.l->op )
    {
    case '^': case '*': case '/': case '+': case NAME: case NUMBER:
    case FUNCTION:
      fprint_eqn(file,base->u.n.l); break;
    default:
      fprintf(file,"("); fprint_eqn(file,base->u.n.l); fprintf(file,")"); break;
    }

    fprintf(file,"-");

    switch( base->u.n.r->op )
    {
    case '^': case '*': case '/': case NAME: case NUMBER:
    case FUNCTION:
      fprint_eqn(file,base->u.n.r); break;
    default:
      fprintf(file,"("); fprint_eqn(file,base->u.n.r); fprintf(file,")"); break;
    }
    break;
  case '*': case '.':
    switch( base->u.n.l->op )
    {
    case '^': case '*': case NAME: case NUMBER: case FUNCTION:
      fprint_eqn(file,base->u.n.l); break;
    default: /* '+' '-' '/' ',' '=' */
      fprintf(file,"("); fprint_eqn(file,base->u.n.l); fprintf(file,")"); break;
    }

    fprintf(file,"%c",base->op);

    switch( base->u.n.r->op )
    {
    case '^': case '*': case NAME: case NUMBER: case FUNCTION:
      fprint_eqn(file,base->u.n.r); break;
    default:
      fprintf(file,"("); fprint_eqn(file,base->u.n.r); fprintf(file,")"); break;
    }
    break;
  case '/':
    switch( base->u.n.l->op )
    {
    case '^': case '*': case NAME: case NUMBER: case FUNCTION:
      fprint_eqn(file,base->u.n.l); break;
    default:
      fprintf(file,"("); fprint_eqn(file,base->u.n.l); fprintf(file,")"); break;
    }

    fprintf(file,"/");

    switch( base->u.n.r->op )
    {
    case '^': case NAME: case NUMBER: case FUNCTION:
      fprint_eqn(file,base->u.n.r); break;
    default:
      fprintf(file,"("); fprint_eqn(file,base->u.n.r); fprintf(file,")"); break;
    }
    break;
  case '^':
    switch( base->u.n.l->op )
    {
    case NAME: case NUMBER: case FUNCTION:
      fprint_eqn(file,base->u.n.l); break;
    default:
      fprintf(file,"("); fprint_eqn(file,base->u.n.l); fprintf(file,")"); break;
    }

    fprintf(file,"^");

    switch( base->u.n.r->op )
    {
    case NAME: case NUMBER: case FUNCTION:
      fprint_eqn(file,base->u.n.r); break;
    default:
      fprintf(file,"("); fprint_eqn(file,base->u.n.r); fprintf(file,")"); break;
    }
    break;
  case '=': case ',':
    fprint_eqn(file,base->u.n.l);
    fprintf(file,"%c",base->op);
    fprint_eqn(file,base->u.n.r);
    break;
  default:
    fprintf(file,"bad op: ");
    fprint_op(file,base->op);
    fprintf(file," ");
  }
}

print_eqn(eqn)
eqn_node *eqn;
{
	fprint_eqn(stdout,eqn);
}

fprint_op(fp,op)
FILE *fp;
int op;
{
	switch(op)
	{
		case '+': case '-': case '*': case '/': case '=': 
		case ',': case '^': case '.':
		fprintf(fp,"'%c'",op);
		case NUMBER:
			fprintf(fp,"NUMBER"); break;
		case INTERVAL:
			fprintf(fp,"INTERVAL"); break;
		case FUNCTION:
			fprintf(fp,"FUNCTION"); break;
		case NAME:
			fprintf(fp,"NAME"); break;
		case SUM1:
			fprintf(fp,"+"); break;
		case SUM2:
			fprintf(fp,"SUM2"); break;
		case SUM3:
			fprintf(fp,"SUM3"); break;
		case SUB1:
			fprintf(fp,"-"); break;
		case SUB2:
			fprintf(fp,"SUB2"); break;
		case SUB3:
			fprintf(fp,"SUB3"); break;
		case DOT2:
			fprintf(fp,"DOT2"); break;
		case DOT3:
			fprintf(fp,"DOT3"); break;
		case CROSS3:
			fprintf(fp,"CROSS3"); break;
		case SCALE2:
			fprintf(fp,"SCALE2"); break;
		case SCALE3:
			fprintf(fp,"SCALE3"); break;
		case INT_POW:
			fprintf(fp,"INT_POW"); break;
		case MULT1:
			fprintf(fp,"*"); break;
		case DIV1:
			fprintf(fp,"/"); break;
		case EQUALS1:
			fprintf(fp,"="); break;
		case POW1:
			fprintf(fp,"^"); break;
		default:
			fprintf(fp,"unknown %d",op); break;
	}
}

print_op(op)
int op;
{
	fprint_op(stdout,op);
}
/**** returns a pointer to a copy of the equation given by base ****/

eqnode *duplicate( base )
eqnode *base;
{
  eqnode *temp;

  if( base == NULL )
  {
	fprintf(stderr,"Tried to duplicate a NULL equation\n");
	return(NULL);
  }
  temp = grballoc( eqnode );
  temp->op = base->op;
  switch( base->op )
  {
  case BRACKET:
    temp->u.n.r=duplicate(base->u.n.r);
    break;

  case FUNCTION:
    temp->u.f.f = base->u.f.f;
    if(base->u.f.f->type == CONSTANT_FUN)
	temp->u.f.a = NULL;
    else
    	temp->u.f.a = duplicate(base->u.f.a);
    break;
  case NAME:
    temp->u.str = (char *) calloc( strlen(base->u.str)+1 , sizeof( char ) );
    strcpy(temp->u.str,base->u.str);
    break;
  case NUMBER:
    temp->u.num = base->u.num;
    break;
  case '+': case '-': case '*': case '/': case '^': case '=': case ',': case INTERVAL: case '.':
    temp->u.n.l=duplicate(base->u.n.l);
    temp->u.n.r=duplicate(base->u.n.r);
    break;
  default:
	fprintf(stderr,"duplicate: bad op ");
	fprint_op(stderr,base->op);
	fprintf(stderr,"\n");
	break;
  }
  return( temp );
}

/*
 * Function:	copy node
 * Action:	copies the information from right to base
 */

copy_node(base,right)
eqnode *base,*right;
{
	if(right == NULL)
	{
		fprintf(stderr,"Tried to copy a Null equation\n");
		return;
	}
	if(base == NULL)
	{
		fprintf(stderr,"Tried to copy onto a Null node\n");
		return;
	}

	base->op = right->op;
	switch( right->op )
	{
	case NUMBER:
		base->u.num = right->u.num;
		break;
	case NAME:
		base->u.str = right->u.str;
		break;
	case '+': case '-': case '*': case '/': case '^': case '=': case ',':
	case '.':
		base->u.n.l = right->u.n.l;
		base->u.n.r = right->u.n.r;
		break;
	case FUNCTION:
		base->u.f.f = right->u.f.f;
		base->u.f.a = right->u.f.a;
		break;
	case BRACKET:
		base->u.n.r = right->u.n.r;
		break;

	default:
		fprintf(stderr,"copy_node: bad op ");
        	fprint_op(stderr,base->op);
	        fprintf(stderr,"\n");
		break;
	}
}

/*
* Function:	count_eqn_tree
* Action:	counts the number of nodes in a tree 
*		note a,b only counts as two nodes
*/

int count_eqn_tree( base )
eqnode *base;
{
  if( base == NULL )
  {
	fprintf(stderr,"tried to count the nodes in a null equation\n");
	return(0);
  }
  switch( base->op )
  {
  case NAME:
  case NUMBER:
    return( 1 );
  case '+': case '-': case '*': case '/': case '^': case '=': case '.':
    return( 1 + count_eqn_tree(base->u.n.l) + count_eqn_tree(base->u.n.r) );
  case ',':
    return( count_eqn_tree(base->u.n.l) + count_eqn_tree(base->u.n.r) );
  case FUNCTION:
    if(base->u.f.f->type == CONSTANT_FUN ) return( 1 );
    return( 1 + count_eqn_tree(base->u.f.a) );
  case BRACKET:
    return(count_eqn_tree(base->u.n.r));
  default:
	fprintf(stderr,"count_eqn_tree: bad op ");
        fprint_op(stderr,base->op);
        fprintf(stderr,"\n");
	break;
  }
  return( 0 );
}

int count_eqn_args(base)
eqnode *base;
{
  int leftcount,rightcount;

  if(base == NULL)
  {
	fprintf(stderr,"Null equation while trying to count number of arguments\n");
	return(0);
  }

  switch( base->op )
  {
  case NAME:
  case NUMBER:
    return( 1 );

  case '+': case '-': case '=':
    leftcount = count_eqn_args(base->u.n.l);
    rightcount = count_eqn_args(base->u.n.r);
    if(leftcount == 0 || rightcount == 0 ) return(0);
    if(leftcount != rightcount )
    {
	fprintf(stderr,"Different counts for '%c' %d %d\n",
		base->op,leftcount,rightcount);
	return(0);
    }
    return(leftcount);

  case '*':
    leftcount = count_eqn_args(base->u.n.l);
    rightcount = count_eqn_args(base->u.n.r);
    if(leftcount == 0 || rightcount == 0 ) return(0);
    if(leftcount != 1 )
    {
	fprintf(stderr,"Bad counts for '%c' %d %d\n",
		base->op,leftcount,rightcount);
	return(0);
    }
    return(rightcount);

  case '.':
    leftcount = count_eqn_args(base->u.n.l);
    rightcount = count_eqn_args(base->u.n.r);
    if(leftcount == 0 || rightcount == 0 ) return(0);
    if(leftcount != rightcount )
    {
	fprintf(stderr,"Bad counts for '%c' %d %d\n",
		base->op,leftcount,rightcount);
	return(0);
    }
    return(1);

  case '/':
    leftcount = count_eqn_args(base->u.n.l);
    rightcount = count_eqn_args(base->u.n.r);
    if(leftcount == 0 || rightcount == 0 ) return(0);
    if(leftcount != 1 || rightcount != 1)
    {
	fprintf(stderr,"Bad counts for '%c' %d %d\n",
		base->op,leftcount,rightcount);
	return(0);
    }
    return(rightcount);

  case '^':
    leftcount = count_eqn_args(base->u.n.l);
    rightcount = count_eqn_args(base->u.n.r);
    if(leftcount == 0 || rightcount == 0 ) return(0);
    if(leftcount != rightcount || ( leftcount != 1 && leftcount != 3) )
    {
	fprintf(stderr,"Bad counts for '%c' %d %d\n",
		base->op,leftcount,rightcount);
	return(0);
    }
    return(rightcount);

  case FUNCTION:
    if( base->u.f.f->type == EXTERN_MAP || base->u.f.f->type == INTERN_MAP )
	return(base->u.f.f->dim);
    else
	return(1);

  case ',':
    return( count_eqn_args(base->u.n.l) + count_eqn_args(base->u.n.r) );

  default:
	fprintf(stderr,"count_eqn_args: bad op ");
        fprint_op(stderr,base->op);
        fprintf(stderr,"\n");
	break;
  }
  return( 0 );
}

/*
 * Function:	get_eqn_arg
 * Action:	gets the i th argument from a comma seperated list
 *		can cope with (a,b,c),(d,e,f)
 */

eqnode *get_eqn_arg(base,i)
eqnode *base;
int i;
{
	int leftcount;

	if(base == NULL)
	{
		fprintf(stderr,"get_eqn_arg: base = NULL\n");
		return(NULL);
	}

	if( base->op != ',' )
	{
		if(i == 1) return(base);
		else	   return(NULL);
	}

	leftcount = count_eqn_args(base->u.n.l);
	if(i <= leftcount ) return(get_eqn_arg(base->u.n.l,i));
	else return(get_eqn_arg(base->u.n.r,i-leftcount));
}

/**** frees the space used by the equation base ****/

free_eqn_tree( base )
eqnode *base;
{
  if( base == NULL )
  {
	fprintf(stderr,"Tried to free a null equation\n");
	return;
  }
  switch( base->op )
  {
  case NAME:
    free( base->u.str );
    break;
  case NUMBER:
    break;
  case '+': case '-': case '*': case '/': case '^': case '=': case ',': case INTERVAL:
    free_eqn_tree(base->u.n.l);
    free_eqn_tree(base->u.n.r);
    break;

  case FUNCTION:
    if(base->u.f.f->type != CONSTANT_FUN)
    	free_eqn_tree(base->u.f.a);
    break;

  case BRACKET:
    free_eqn_tree(base->u.n.r);
    break;

  }
  free( base );
}

free_eqn_node( node )
eqnode *node;
{
	if( node->op == NAME) free( node->u.str );
	free( node );
}

/*
 * Function:	eqn_list_product
 * Action:	multiply together two lists of same structure
 * Returns:	pointer to product, NULL on error
 */

eqnode *eqn_list_product(left,right)
eqnode *left,*right;
{
	eqnode *node;

	if(left == NULL )
	{
		fprintf(stderr,"eqn_list_product: NULL left\n");
		return(NULL);
	}
	if(right == NULL )
	{
		fprintf(stderr,"eqn_list_product: NULL right\n");
		return(NULL);
	}

	if( left->op == ',' && right->op == ',' )
	{
		node = grballoc(eqnode);
		node->op == '+';
		node->u.n.l = eqn_list_product(left->u.n.l,right->u.n.l);
		node->u.n.r = eqn_list_product(left->u.n.r,right->u.n.r);
		if(node->u.n.l == NULL || node->u.n.r == NULL)
		{
			free(node);
			return(NULL);
		}
	}
	else if( left->op != ',' && right->op != ',' )
	{
		node = grballoc(eqnode);
		node->op = '*';
		node->u.n.l = left;
		node->u.n.r = right;
	}
	else
	{
		fprintf(stderr,"eqn_list_product: left and right trees didn't match\n");
		fprint_eqn(stderr,left); fprintf(stderr,"\n");
		fprint_eqn(stderr,right); fprintf(stderr,"\n");
		return(NULL);
	}
	return(node);
}

/*
 * Functions:	join_eqn,join_dup_eqn
 * Action:	Joins two equations together linked by op
 *		for join_dup_eqns copys of the equations are used.
 */

eqnode *join_eqns(op,left,right)
int op;
eqnode *left, *right;
{
	eqnode *temp;

	if(left == NULL )
	{
		fprintf(stderr,"Null left equation while joining equations\n");
		return(NULL);
	}
	if(right == NULL )
	{
		fprintf(stderr,"Null right equation while joining equations\n");
		return(NULL);
	}

	switch(op)
	{
  		case '+': case '-': case '*': case '/': case '^': case '=':
		case ',': case '.':
			break;
		default:
			fprintf(stderr,"Bad op ");
			fprint_op(stderr,op);
			fprintf(stderr," while joining equations\n");
			return(NULL);
	}
	temp = grballoc(eqnode);
	temp->op = op;
	temp->u.n.l = left;
	temp->u.n.r = right;
	return(temp);
}

eqnode *join_dup_eqns(op,left,right)
int op;
eqnode *left, *right;
{
	eqnode *temp;

	if(left == NULL )
	{
		fprintf(stderr,"Null left equation while joining equations\n");
		return(NULL);
	}
	if(right == NULL )
	{
		fprintf(stderr,"Null right equation while joining equations\n");
		return(NULL);
	}

	switch(op)
	{
  		case '+': case '-': case '*': case '/': case '^': case '=':
		case ',': case '.':
			break;
		default:
			fprintf(stderr,"Bad op ");
			fprint_op(stderr,op);
			fprintf(stderr," while joining equations\n");
			return(NULL);
	}
	temp = grballoc(eqnode);
	temp->op = op;
	temp->u.n.l = duplicate(left);
	temp->u.n.r = duplicate(right);
	return(temp);
}
