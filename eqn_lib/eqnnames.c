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


#ifdef ADD_NAMES
eqn_names *eqn_basename = NULL;   /* the base of the list of names */
#endif

/*****************************************************************************/
/*                                                                           */
/*  Now some routines for handling names.                                    */
/*                                                                           */
/*****************************************************************************/

/*
 * Function:	add_eqn_names
 * Action:	find all the names in the equation and add them to
 *		the list (if it exists).
 */

eqn_names *add_eqn_names(oldvars,eqn)
eqn_names *oldvars;eqnode *eqn;
{
  eqn_names *temp, *prev, *newnode;
  int      res;

  if(eqn == NULL ) return oldvars;

  switch( eqn->op )
  {
  case '+': case '-': case '*': case '/': case '^': case INTERVAL: case '=': case ',': case '.':
	oldvars = add_eqn_names(oldvars,eqn->u.n.l);
	oldvars = add_eqn_names(oldvars,eqn->u.n.r);
	return oldvars;

  case BRACKET:
	oldvars = add_eqn_names(oldvars,eqn->u.n.r);
	return oldvars;

  case NUMBER:
	return oldvars;

  case FUNCTION:
	if(eqn->u.f.f->type != CONSTANT_FUN )
		oldvars = add_eqn_names(oldvars,eqn->u.f.a);
	return oldvars;

  case NAME:
	prev = NULL;
	temp = oldvars;
	while(temp != NULL)
	{
		res = strcmp(temp->str,eqn->u.str);
		if( !res ) return oldvars;	/* Name matches do nowt */
		if( res > 0 )		/* Lexagraphically less		*/
		{
			/* Insert before temp in the list */

			newnode = grballoc( eqn_names );
			newnode->next = temp->next;
			temp->next = newnode;
			newnode->type = PARAMETER;
			newnode->str = temp->str;
			temp->str = (char *) calloc( strlen(eqn->u.str) +1,
				sizeof(char));
			strcpy( temp->str, eqn->u.str );
			return oldvars;
		}		
		prev = temp;
		temp = temp->next;
	}
	
	/* Lexagraphically greater add at end of list */

	temp = grballoc(eqn_names);
	temp->next = NULL;
	temp->type = PARAMETER;
	temp->str =(char *) calloc(strlen(eqn->u.str)+1,sizeof(char));
	strcpy(temp->str,eqn->u.str);
	if( prev == NULL ) return temp;
	else
	{
		prev->next = temp;
		return oldvars;
	}
  default:
	fprintf(stderr,"add_eqn_names: bad op");
        fprint_op(stderr,eqn->op);
        fprintf(stderr,"\n");
  }
  return oldvars;
}

/****  Print out the list of names ****/

print_eqn_names(ptr)
eqn_names *ptr;
{
  while( ptr != NULL )
  {
    printf("name '%s' type ",ptr->str);
    switch(ptr->type)
    {
    case VARIABLE: printf("variable\n"); break;
    case PARAMETER: printf("parameter\n"); break;
    default:
      printf("bad type %d\n",ptr->type);
    }
    ptr = ptr->next;
  }
}

fprint_eqn_names(fp,ptr)
FILE *fp; eqn_names *ptr;
{
  while( ptr != NULL )
  {
    fprintf(fp,"name '%s' type ",ptr->str);
    switch(ptr->type)
    {
    case VARIABLE: fprintf(fp,"variable\n"); break;
    case PARAMETER: fprintf(fp,"parameter\n"); break;
    default:
      fprintf(fp,"bad type %d\n",ptr->type);
    }
    ptr = ptr->next;
  }
}

/**** returns the number of parameters ****/

int num_parameters(ptr)
eqn_names *ptr;
{
  int n = 0;

  while( ptr != NULL )
  {
    if( ptr->type == PARAMETER ) ++n;
    ptr = ptr->next;
  }
  return( n );
}

/*****
*     On exit 'string' contains the name of the n'th parameter.
*     If there is no n'th parameter FALSE is returned
*****/

char *get_parameter(ptr,n)
eqn_names *ptr;int n;
{
  while( ptr != NULL && n >= 1 )
  {
    if( ptr->type == PARAMETER )
    {
      if( --n == 0 ) 
      {
        return ptr->str;
      }
    }
    ptr = ptr->next;
  }
  return( NULL );
}

/**** Clears the list of names ****/

void free_eqn_names(ptr)
eqn_names *ptr;
{
  eqn_names *next;

  while( ptr != NULL )
  {
  	next = ptr->next;
	free(ptr->str);
	free(ptr);
	ptr = next;
  }
}

/*
 * Function:	make_varible
 * Action:	delete the variable string from the list of varibles
 */

int make_variable(ptr,string)
eqn_names *ptr;char *string;
{
  if(string == NULL)
  {
	fprintf(stderr,"NULL variable name\n");
	return(FALSE);
  }
  while( ptr != NULL )
  {
    if( !strcmp( ptr->str , string ) ) 
    {
      ptr->type = VARIABLE;
      return( TRUE );
    }
    ptr = ptr->next;
  }
  fprintf(stderr,"Warning: variable %s not found in equations.\n",string);
  return( FALSE );
}
