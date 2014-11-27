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

/***************************************************************************/
/*                                                                         */
/*  Now a set of routines to convert to poly form where the equation is    */
/*    \sum poly[i][j] name1^i name2^j                                      */
/*                                                                         */
/***************************************************************************/

/**** finds the order of a polynomial in one variables ****/
/****  ord1 is a pointer to locations which will hold the order ****/

order_poly1(poly,ord1)
double poly[MAXORDER];
int *ord1;
{
  int i;

  *ord1 = 0;
  for( i=0 ; i<MAXORDER; ++i)
      if( poly[i] != 0.0 )
      {
        *ord1 = MAX( *ord1, i );
      }
}

/**** finds the order of a polynomial in two variables ****/
/****  ord1, ord2 are pointers to locations which will hold the order ****/

order_poly2(poly,ord1,ord2)
double poly[MAXORDER][MAXORDER];
int *ord1,*ord2;
{
  int i,j;

  *ord1 = *ord2 = 0;
  for( i=0 ; i<MAXORDER; ++i)
    for( j=0; j<MAXORDER; ++j)
      if( poly[i][j] != 0.0 )
      {
        *ord1 = MAX( *ord1, i );
        *ord2 = MAX( *ord2, j );
      }
}

order_poly3(poly,ord1,ord2,ord3)
double poly[MAXORDER][MAXORDER][MAXORDER];
int *ord1,*ord2,*ord3;
{
  int i,j,k;

  *ord1 = *ord2 = *ord3 = 0;
  for( i=0; i<MAXORDER; ++i)
  for( j=0; j<MAXORDER; ++j)
  for( k=0; k<MAXORDER; ++k)
    if( poly[i][j][k] != 0.0 )
    {
      *ord1 = MAX( *ord1, i );
      *ord2 = MAX( *ord2, j );
      *ord3 = MAX( *ord3, k );
    }
}

/**** print a ploynomial in one variable ****/

print_poly1(poly)
double poly[MAXORDER];
{
  int ord1;
  int i;

  order_poly1(poly, &ord1);
  for(i=0;i<=ord1;++i) printf("%f\t",poly[i]);
  printf("\n");
}

/**** print a ploynomial in two variables ****/

print_poly2(poly)
double poly[MAXORDER][MAXORDER];
{
  int ord1,ord2;
  int i,j;

  order_poly2(poly, &ord1, &ord2);
  for(j=0;j<=ord2;++j)
  {
    for(i=0;i<=ord1;++i) printf("%f\t",poly[i][j]);
    printf("\n");
  }
  printf("\n");
}

print_poly3(poly)
double poly[MAXORDER][MAXORDER][MAXORDER];
{
  int ord1,ord2,ord3;
  int i,j,k;

  order_poly3(poly, &ord1, &ord2, &ord3);

  for(k=0;k<=ord3;++k)
  {
    for(j=0;j<=ord2;++j)
    {
      for(i=0;i<=ord1;++i) printf("%f\t",poly[i][j][k]);
      printf("\n");
    }
    printf("\n");
  }
}

/**** print a ploynomial in one variable ****/

fprint_poly1(fp,poly)
FILE *fp;
double poly[MAXORDER];
{
  int ord1;
  int i;

  order_poly1(poly, &ord1);
  for(i=0;i<=ord1;++i) fprintf(fp,"%f\t",poly[i]);
  fprintf(fp,"\n");
}

fprint_poly2(fp,poly)
FILE *fp;
double poly[MAXORDER][MAXORDER];
{
  int ord1,ord2;
  int i,j;

  order_poly2(poly, &ord1, &ord2);
  for(j=0;j<=ord2;++j)
  {
    for(i=0;i<=ord1;++i) fprintf(fp,"%f\t",poly[i][j]);
    fprintf(fp,"\n");
  }
  fprintf(fp,"\n");
}

fprint_poly3(fp,poly)
FILE *fp;
double poly[MAXORDER][MAXORDER][MAXORDER];
{
  int ord1,ord2,ord3;
  int i,j,k;

  order_poly3(poly, &ord1, &ord2, &ord3);

  for(k=0;k<=ord3;++k)
  {
    for(j=0;j<=ord2;++j)
    {
      for(i=0;i<=ord1;++i) fprintf(fp,"%f\t",poly[i][j][k]);
      fprintf(fp,"\n");
    }
    fprintf(fp,"\n");
  }
}

/**** sets a polynomial in one variable to zero ****/

init_poly1(poly)
double poly[MAXORDER];
{
  bzero(poly,MAXORDER*sizeof(double));
/*
  int i;
  for( i=0 ; i<MAXORDER; ++i)
     poly[i] =0.0;
*/
}

init_poly2(poly)
double poly[MAXORDER][MAXORDER];
{
  bzero(poly,MAXORDER*MAXORDER*sizeof(double));
/*
  int i,j;
  for( i=0 ; i<MAXORDER; ++i)
    for( j=0; j<MAXORDER; ++j)
      poly[i][j] =0.0;
*/
}

init_poly3(poly)
double poly[MAXORDER][MAXORDER][MAXORDER];
{
  bzero(poly,MAXORDER*MAXORDER*MAXORDER*sizeof(double));
/*
  int i,j,k;
  for( i=0 ; i<MAXORDER; ++i)
    for( j=0; j<MAXORDER; ++j)
      for( k=0; k<MAXORDER; ++k) poly[i][j][k] =0.0;
*/
}

/*****
*     adds the equation 'eqn' to the two variable polynomial 'poly'
*     'name1' and 'name2' are two strings which hold the names of 
*     the variables.
*     'add_to_poly2' returns TRUE if the polynomial was sucessfully created.
*     FALSE will be returned if:-
*        'eqn' contains a name which is neither 'name1' or 'name2'.
*        a polynomial can not be created eg 'x/y'
*        the degree of the polynomial is too large.
*     It is assumed that 'eqn' has been previously been expanded by 'expand'
*     which if sucessful will leave the equation in the form
*        t1 + t2 + t3 + .....
*     where the terms t1,t2,... are of the form
*        a * ( name1 ^ n ) * ( name2 ^ m ) * ......
*     where 'a' is a real number 'n', 'm' are integers.
*****/

int add_to_poly2(eqn,poly,name1,name2)
eqnode *eqn;
double poly[MAXORDER][MAXORDER];
char *name1,*name2;
{
  double coeff;
  int ord1,ord2,leftres,rightres;

  switch( eqn->op )
  {
  case NUMBER:
    poly[0][0] += eqn->u.num;
    return( TRUE );
  case NAME:
    if( !strcmp( eqn->u.str , name1 ) )
      poly[1][0] += 1.0;
    else if( !strcmp( eqn->u.str , name2 ) )
      poly[0][1] += 1.0;
    else
    {
      fprintf(stderr,"add_to_poly2: bad name %s\n",eqn->u.str);
      return( FALSE );
    }
    return( TRUE );
  case '+':
    leftres = add_to_poly2( eqn->u.n.l, poly, name1 ,name2 );
    rightres = add_to_poly2( eqn->u.n.r, poly, name1 ,name2 );
    return( leftres && rightres );
  case '-': case'=':
    leftres = add_to_poly2( eqn->u.n.l, poly, name1 ,name2 );
    rightres = sub_from_poly2( eqn->u.n.r, poly, name1 ,name2 );
    return( leftres && rightres );
  case '*': case '/': case '^':
    coeff = 1.0; ord1 = ord2 = 0;
    leftres = eval_term2( eqn, name1, name2, &coeff, &ord1, &ord2 );
    if( ord1 >= MAXORDER || ord2>= MAXORDER ||
        ord1 <  0        || ord2 < 0        || !leftres )
    {
      fprintf(stderr,"add_to_poly2: orders to big %d %d\n",ord1,ord2);
      return( FALSE );
    }
    poly[ord1][ord2] += coeff;
    return( TRUE );
  }
  return( FALSE );
}

/*****
*     'sub_from_poly' behaves very much like 'add_to_poly'
*     except the equation is subtracted.
*****/

int sub_from_poly2(eqn,poly,name1,name2)
eqnode *eqn;
double poly[MAXORDER][MAXORDER];
char *name1,*name2;
{
  double coeff;
  int ord1,ord2,leftres,rightres;

  switch( eqn->op )
  {
  case NUMBER:
    poly[0][0] -= eqn->u.num;
    return( TRUE );
  case NAME:
    if( !strcmp( eqn->u.str , name1 ) )
      poly[1][0] -= 1.0;
    else if( !strcmp( eqn->u.str , name2 ) )
      poly[0][1] -= 1.0;
    else
    {
      fprintf(stderr,"add_to_poly2: bad name %s\n",eqn->u.str);
      return( FALSE );
    }
    return( TRUE );
  case '+':
    leftres = sub_from_poly2( eqn->u.n.l, poly, name1 ,name2 );
    rightres = sub_from_poly2( eqn->u.n.r, poly, name1 ,name2 );
    return( leftres && rightres );
  case '-': case'=':
    leftres = sub_from_poly2( eqn->u.n.l, poly, name1 ,name2 );
    rightres = add_to_poly2( eqn->u.n.r, poly, name1 ,name2 );
    return( leftres && rightres );
  case '*': case '/': case '^':
    coeff = 1.0; ord1 = ord2 = 0;
    leftres = eval_term2( eqn, name1, name2, &coeff, &ord1, &ord2 );
    if( ord1 >= MAXORDER || ord2>= MAXORDER ||
        ord1 <  0        || ord2 < 0        || !leftres )
    {
      fprintf(stderr,"sub_from_poly2: orders to big %d %d\n",ord1,ord2);
      return( FALSE );
    }
    poly[ord1][ord2] -= coeff;
    return( TRUE );
  }
  return( FALSE );
}

int add_to_poly1(eqn,poly,name1)
eqnode *eqn;
double poly[MAXORDER];
char *name1;
{
  double coeff;
  int ord1,ord2,leftres,rightres;

  switch( eqn->op )
  {
  case NUMBER:
    poly[0] += eqn->u.num;
    return( TRUE );
  case NAME:
    if( !strcmp( eqn->u.str , name1 ) )
      poly[1] += 1.0;
    else
    {
      fprintf(stderr,"add_to_poly2: bad name %s\n",eqn->u.str);
      return( FALSE );
    }
    return( TRUE );
  case '+':
    leftres = add_to_poly1( eqn->u.n.l, poly, name1 );
    rightres = add_to_poly1( eqn->u.n.r, poly, name1 );
    return( leftres && rightres );
  case '-': case'=':
    leftres = add_to_poly1( eqn->u.n.l, poly, name1 );
    rightres = sub_from_poly1( eqn->u.n.r, poly, name1 );
    return( leftres && rightres );
  case '*': case '/': case '^':
    coeff = 1.0; ord1 = ord2 = 0;
    leftres = eval_term1( eqn, name1, &coeff, &ord1 );
    if( ord1 >= MAXORDER || ord1 <  0 || !leftres )
    {
      fprintf(stderr,"add_to_poly1: orders to big %d %d\n",ord1);
      return( FALSE );
    }
    poly[ord1] += coeff;
    return( TRUE );
  }
  return( FALSE );
}

/*****
*     'sub_from_poly' behaves very much like 'add_to_poly'
*     except the equation is subtracted.
*****/

int sub_from_poly1(eqn,poly,name1)
eqnode *eqn;
double poly[MAXORDER];
char *name1;
{
  double coeff;
  int ord1,leftres,rightres;

  switch( eqn->op )
  {
  case NUMBER:
    poly[0] -= eqn->u.num;
    return( TRUE );
  case NAME:
    if( !strcmp( eqn->u.str , name1 ) )
      poly[1] -= 1.0;
    else
    {
      fprintf(stderr,"sub_from_poly2: bad name %s\n",eqn->u.str);
      return( FALSE );
    }
    return( TRUE );
  case '+':
    leftres = sub_from_poly1( eqn->u.n.l, poly, name1 );
    rightres = sub_from_poly1( eqn->u.n.r, poly, name1 );
    return( leftres && rightres );
  case '-': case'=':
    leftres = sub_from_poly1( eqn->u.n.l, poly, name1 );
    rightres = add_to_poly1( eqn->u.n.r, poly, name1 );
    return( leftres && rightres );
  case '*': case '/': case '^':
    coeff = 1.0; ord1 = 0;
    leftres = eval_term1( eqn, name1, &coeff, &ord1 );
    if( ord1 >= MAXORDER || ord1 <  0 || !leftres )
    {
      fprintf(stderr,"sub_from_poly1: orders to big %d %d\n",ord1);
      return( FALSE );
    }
    poly[ord1] -= coeff;
    return( TRUE );
  }
  return( FALSE );
}

int add_to_poly3(eqn,poly,name1,name2,name3)
eqnode *eqn;
double poly[MAXORDER][MAXORDER][MAXORDER];
char *name1,*name2, *name3;
{
  double coeff;
  int ord1,ord2,ord3,leftres,rightres;

#ifdef PRINT_ADD3
  fprintf(stderr,"add_to_poly3: eqn ");
  fprint_eqn(stderr,eqn); fprintf(stderr,"\n");
  fprint_poly3(stderr,poly);
#endif

  switch( eqn->op )
  {
  case NUMBER:
    poly[0][0][0] += eqn->u.num;
    return( TRUE );
  case NAME:
    if( !strcmp( eqn->u.str , name1 ) )
      poly[1][0][0] += 1.0;
    else if( !strcmp( eqn->u.str , name2 ) )
      poly[0][1][0] += 1.0;
    else if( !strcmp( eqn->u.str , name3 ) )
      poly[0][0][1] += 1.0;
    else
    {
      fprintf(stderr,"add_to_poly3: bad name %s\n",eqn->u.str);
      return( FALSE );
    }
    return( TRUE );
  case '+':
    leftres = add_to_poly3( eqn->u.n.l, poly, name1 ,name2, name3 );
    rightres = add_to_poly3( eqn->u.n.r, poly, name1 ,name2, name3 );
    return( leftres && rightres );
  case '-': case'=':
    leftres = add_to_poly3( eqn->u.n.l, poly, name1 ,name2, name3 );
    rightres = sub_from_poly3( eqn->u.n.r, poly, name1 ,name2, name3 );
    return( leftres && rightres );
  case '*': case '/': case '^':
    coeff = 1.0; ord1 = ord2 = ord3 = 0;
    leftres = eval_term3(eqn,name1,name2,name3,&coeff,&ord1,&ord2,&ord3);
    if( ord1 >= MAXORDER || ord2>= MAXORDER || ord3 >= MAXORDER ||
        ord1 <  0        || ord2 < 0        || ord3 <  0 || !leftres )
    {
      fprintf(stderr,"add_to_poly3: orders to big %d %d %d\n",ord1,ord2,ord3);
      return( FALSE );
    }
    poly[ord1][ord2][ord3] += coeff;
    return( TRUE );
  }
  return( FALSE );
}

/*****
*     'sub_from_poly' behaves very much like 'add_to_poly'
*     except the equation is subtracted.
*****/

int sub_from_poly3(eqn,poly,name1,name2,name3)
eqnode *eqn;
double poly[MAXORDER][MAXORDER][MAXORDER];
char *name1,*name2, *name3;
{
  double coeff;
  int ord1,ord2,ord3,leftres,rightres;

#ifdef PRINT_ADD3
  fprintf(stderr,"sub_from_poly3: eqn ");
  fprint_eqn(stderr,eqn); fprintf(stderr,"\n");
  fprint_poly3(stderr,poly);
#endif

  switch( eqn->op )
  {
  case NUMBER:
    poly[0][0][0] -= eqn->u.num;
    return( TRUE );
  case NAME:
    if( !strcmp( eqn->u.str , name1 ) )
      poly[1][0][0] -= 1.0;
    else if( !strcmp( eqn->u.str , name2 ) )
      poly[0][1][0] -= 1.0;
    else if( !strcmp( eqn->u.str , name3 ) )
      poly[0][0][1] -= 1.0;
    else
    {
      fprintf(stderr,"sub_from_poly3: bad name %s\n",eqn->u.str);
      return( FALSE );
    }
    return( TRUE );
  case '+':
    leftres = sub_from_poly3( eqn->u.n.l, poly, name1 ,name2, name3 );
    rightres = sub_from_poly3( eqn->u.n.r, poly, name1 ,name2, name3 );
    return( leftres && rightres );
  case '-': case'=':
    leftres = sub_from_poly3( eqn->u.n.l, poly, name1 ,name2, name3 );
    rightres = add_to_poly3( eqn->u.n.r, poly, name1 ,name2, name3 );
    return( leftres && rightres );
  case '*': case '/': case '^':
    coeff = 1.0; ord1 = ord2 = ord3 = 0;
    leftres = eval_term3(eqn,name1,name2,name3,&coeff,&ord1,&ord2,&ord3);
    if( ord1 >= MAXORDER || ord2>= MAXORDER || ord3 >= MAXORDER ||
        ord1 <  0        || ord2 < 0        || ord3 <  0 || !leftres )
    {
      fprintf(stderr,"sub_from_poly3: orders to big %d %d %d\n",ord1,ord2,ord3);
      return( FALSE );
    }
    poly[ord1][ord2][ord3] -= coeff;
    return( TRUE );
  }
  return( FALSE );
}

/*****
*     'eval_term2' is used by the two previous routines to find
*     the order of the term 'eqn'.
*     'eqn' is assumed just to consist of *, /, and ^ operations
*     The routine will return FALSE if + or - is encountered.
*     As for previous routines 'name1' and 'name2' are the two variable names,
*     the locations '*ord1', '*ord2', '*coeff' contain the order of 'name1'
*     'name2' and the real coefficient of the term.
*****/

int eval_term2( eqn, name1, name2, coeff, ord1, ord2 )
eqnode *eqn; char *name1,*name2;
double *coeff; int *ord1,*ord2;
{
  int leftres, rightres, divord1, divord2;
  double divcoeff;

  switch( eqn->op )
  {
  case NUMBER:
    *coeff *= eqn->u.num;
    return( TRUE );
  case NAME:
    if( !strcmp( eqn->u.str , name1 ) )      ++(*ord1);
    else if( !strcmp( eqn->u.str , name2 ) ) ++(*ord2);
    else
    {
      fprintf(stderr,"eval_term2: bad name %s\n",eqn->u.str);
      return( FALSE );
    }
    return( TRUE );

  case '+': case '-': case'=':
    fprintf(stderr,"eval_term2: cant cope with op ");
        fprint_op(stderr,eqn->op);
        fprintf(stderr,"\n");
    return( FALSE );

  case '*':
    leftres = eval_term2( eqn->u.n.l, name1, name2, coeff, ord1, ord2 );
    rightres = eval_term2( eqn->u.n.r, name1, name2, coeff, ord1, ord2 );
    return( leftres && rightres );

  case '/':
    leftres = eval_term2( eqn->u.n.l, name1, name2, coeff, ord1, ord2 );
    divcoeff = 1.0; divord1 = divord2 = 0;
    rightres = eval_term2(eqn->u.n.r,name1,name2,&divcoeff,&divord1,&divord2 );
    if( leftres && rightres )
    {
      *coeff /= divcoeff;
      *ord1  -= divord1;
      *ord2  -= divord2;
      return( TRUE );
    }
    return( FALSE );

  case '^':
    divcoeff = 1.0; divord1 = divord2 = 0;
    leftres = eval_term2(eqn->u.n.l,name1,name2,&divcoeff,&divord1,&divord2 );
    if( eqn->u.n.r->op != NUMBER )
    {
      fprintf(stderr,"eval_term: bad power op ");
        fprint_op(stderr,eqn->u.n.r->op);
        fprintf(stderr,"\n");
      return( FALSE );
    }
    *coeff *= pow( divcoeff , eqn->u.n.r->u.num );
    *ord1  += divord1 * eqn->u.n.r->u.num;
    *ord2  += divord2 * eqn->u.n.r->u.num;
    return( TRUE );
  }
  return( FALSE );
}

int eval_term1( eqn, name1, coeff, ord1 )
eqnode *eqn; char *name1;
double *coeff; int *ord1;
{
  int leftres, rightres, divord1;
  double divcoeff;

  switch( eqn->op )
  {
  case NUMBER:
    *coeff *= eqn->u.num;
    return( TRUE );
  case NAME:
    if( !strcmp( eqn->u.str , name1 ) )      ++(*ord1);
    else
    {
      fprintf(stderr,"eval_term1: bad name %s\n",eqn->u.str);
      return( FALSE );
    }
    return( TRUE );

  case '+': case '-': case'=':
    fprintf(stderr,"eval_term1: cant cope with op ");
        fprint_op(stderr,eqn->op);
        fprintf(stderr,"\n");
    return( FALSE );

  case '*':
    leftres = eval_term1( eqn->u.n.l, name1, coeff, ord1 );
    rightres = eval_term1( eqn->u.n.r, name1, coeff, ord1 );
    return( leftres && rightres );

  case '/':
    leftres = eval_term1( eqn->u.n.l, name1, coeff, ord1 );
    divcoeff = 1.0; divord1 = 0;
    rightres = eval_term1(eqn->u.n.r,name1,&divcoeff,&divord1 );
    if( leftres && rightres )
    {
      *coeff /= divcoeff;
      *ord1  -= divord1;
      return( TRUE );
    }
    return( FALSE );

  case '^':
    divcoeff = 1.0; divord1 = 0;
    leftres = eval_term1(eqn->u.n.l,name1,&divcoeff,&divord1 );
    if( eqn->u.n.r->op != NUMBER )
    {
      fprintf(stderr,"eval_term1: bad power op ");
        fprint_op(stderr,eqn->u.n.r->op);
        fprintf(stderr,"\n");
      return( FALSE );
    }
    *coeff *= pow( divcoeff , eqn->u.n.r->u.num );
    *ord1  += divord1 * eqn->u.n.r->u.num;
    return( TRUE );
  }
  return( FALSE );
}

int eval_term3( eqn, name1, name2, name3, coeff, ord1, ord2, ord3 )
eqnode *eqn; char *name1,*name2,*name3;
double *coeff; int *ord1,*ord2,*ord3;
{
  int leftres, rightres, divord1, divord2, divord3;
  double divcoeff;

  switch( eqn->op )
  {
  case NUMBER:
    *coeff *= eqn->u.num;
    return( TRUE );
  case NAME:
    if( !strcmp( eqn->u.str , name1 ) )      ++(*ord1);
    else if( !strcmp( eqn->u.str , name2 ) ) ++(*ord2);
    else if( !strcmp( eqn->u.str , name3 ) ) ++(*ord3);
    else
    {
      fprintf(stderr,"eval_term3: bad name %s\n",eqn->u.str);
      fprintf(stderr,"names are %s %s %s\n",name1,name2,name3);
      return( FALSE );
    }
    return( TRUE );

  case '+': case '-': case'=':
    fprintf(stderr,"eval_term2: cant cope with op ");
        fprint_op(stderr,eqn->op);
        fprintf(stderr,"\n");
    return( FALSE );

  case '*':
    leftres = eval_term3( eqn->u.n.l, name1,name2,name3, coeff,ord1,ord2,ord3);
    rightres = eval_term3( eqn->u.n.r, name1,name2,name3, coeff,ord1,ord2,ord3);
    return( leftres && rightres );

  case '/':
    leftres = eval_term3( eqn->u.n.l, name1,name2,name3, coeff,ord1,ord2,ord3);
    divcoeff = 1.0; divord1 = divord2 = divord3 = 0;
    rightres = eval_term3( eqn->u.n.r, name1,name2,name3,
	&divcoeff,&divord1,&divord2, &divord3 );
    if( leftres && rightres )
    {
      *coeff /= divcoeff;
      *ord1  -= divord1;
      *ord2  -= divord2;
      *ord3  -= divord3;
      return( TRUE );
    }
    return( FALSE );

  case '^':
    divcoeff = 1.0; divord1 = divord2 = divord3 = 0;
    leftres = eval_term3( eqn->u.n.l, name1,name2,name3,
	&divcoeff,&divord1,&divord2,&divord3);
    if( eqn->u.n.r->op != NUMBER )
    {
      fprintf(stderr,"eval_term: bad power op ");
        fprint_op(stderr,eqn->u.n.r->op);
        fprintf(stderr,"\n");
      return( FALSE );
    }
    *coeff *= pow( divcoeff , eqn->u.n.r->u.num );
    *ord1  += divord1 * eqn->u.n.r->u.num;
    *ord2  += divord2 * eqn->u.n.r->u.num;
    *ord3  += divord3 * eqn->u.n.r->u.num;
    return( TRUE );
  }
  return( FALSE );
}
