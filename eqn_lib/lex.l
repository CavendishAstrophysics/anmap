%{
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
/*****************************************************************************/
/*                                                                           */
/*  multiplecharacter.c  a lexical analysier for equations                   */
/*    this version treats varibles as being a multiple character             */
/*    alpha-numeric strings which must be separated by a non alpha-numeric   */
/*    character.                                                             */
/*    for portability this has been written in 'C' so it can run on the IBM  */ 
/*                                                                           */
/*****************************************************************************/
#define  I_AM_LEX
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include "eqn.h"
#include "y.tab.h"
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !(FALSE)
#endif
#define grballoc(node) (node *)malloc(sizeof(node))
#undef input
#undef unput
#define input()	gginput()
#define	unput(c) ggunput(c)

extern int eqnin_mode;
extern FILE *eqnin_file;
extern char *eqnin_str;
extern eqn_funs *base_funs;

char errorstring[1024];
int  ptrerrstring = 0;
%}
%%
"("	return( YYLBRACE );
")"	return( YYRBRACE );
"["	return( YYLSQUARE );
"]"	return( YYRSQUARE );
".."	return( YYDOTDOT );
"+"	return( YYPLUS   );
"*"	return( YYTIMES  );
"-"	return( YYMINUS  );
"/"	return( YYDEVIDE );
"^"	return( YYPOWER );
"**"	return( YYPOWER );
"="	return( YYEQUAL );
":="	return( YYEQUAL );
","	return( YYCOMMA );
":"	return( YYCOLON );
[0-9]*\.?[0-9]* {
		char c1;

		if(yyleng == 1 && yytext[0] == '.' ) return(YYDOT);
		if(yytext[yyleng-1] == '.')
		{
			c1 = input();	/* test for 1..1 */
			if( c1 == '.' )
			{
				unput(c1);
				yytext[yyleng-1] = '\0';
				--yyleng;
#ifdef NOT_DEF
				yyless(yyleng-1);
#endif
			}
			unput(c1);
		}
	
                yylval.y_dbl = atof(yytext); 
      		return(YYNUMBER);
        }
"."	return( YYDOT );
[A-Za-z][A-Za-z0-9_]*[']*	{
		char c1;
		eqn_funs *fun;

		/* First test the name for a function */

		fun = base_funs;
		while( fun != NULL )
		{
			if(!strcmp(fun->name,yytext))
			{
			    if(fun->type == EXTERN_FUN 
			     || fun->type == INTERN_FUN 
			     || fun->type == EXTERN_MAP 
			     || fun->type == INTERN_MAP 
			     || fun->type == OPERATOR )
			    {
				yylval.y_fun = fun;
				return(YYFUNNAME);
			    }
			    if(fun->type == CONSTANT_FUN )
			    {
				yylval.y_fun = fun;
				return(YYCONSTANT);
			    }
			
			}
			fun = fun->next;
		}
		/* Now its just a name */

                yylval.y_str = (char *) calloc(yyleng + 1,sizeof(char));
                strcpy(yylval.y_str,yytext);
#ifdef NOT_DEF
		c1 = input();
		if( c1 == '(' ) return(YYFUNNAME);
		unput(c1);
#endif
                return(YYNAME);
        }
"/*"	{
		char  c1, c2;

		do 
		{
			while ((c1 = gginput()) != '*' && c1 != EOF) ;
			c2 = gginput();
		
		} while (c2 != '/' && c2 != EOF);
		
		if (c1 == EOF || c2 == EOF)
		{
      			fprintf(stderr,"unterminated comment \n");
      			exit(-3);
		}
	}
[ \n\t]	;
[;]	return(0);
#.*$	;	/* remove coments */
\d	return(0);
\0	return(0);
%%
gginput()
{
  int c;
  switch( eqnin_mode )
  {
  case EQNFROM_STDIN:
    errorstring[ptrerrstring++] = c = getchar();
    if(ptrerrstring >= 1024 ) ptrerrstring = 0;
    if( c == EOF ) c = 0;
    return(c);
  case EQNFROM_FILE:
    errorstring[ptrerrstring++] = c = getc(eqnin_file);
    if(ptrerrstring >= 1024 ) ptrerrstring = 0;
    if( c == EOF ) c = 0;
    return(c);
  case EQNFROM_STRING:
    errorstring[ptrerrstring++] = c = *(eqnin_str++);
    if(ptrerrstring >= 1024 ) ptrerrstring = 0;
    if( c == EOF ) c = 0;
    return(c);
  }
  fprintf(stderr,"input: bad call of equation parser\n");
  exit(-1);
}

ggunput(c)
int c;
{
  --ptrerrstring;
  if( ptrerrstring == 0 ) ptrerrstring = 0;
  switch( eqnin_mode )
  {
  case EQNFROM_STDIN:
    return( ungetc(c,stdin) );
  case EQNFROM_FILE:
    return( ungetc(c,eqnin_file ) );
  case EQNFROM_STRING:
    return( *(--eqnin_str) = c );
  }
  fprintf(stderr,"unput: bad call to yyparse\n");
  exit(-1);
}

int yywrap()
{
return 1;
}
