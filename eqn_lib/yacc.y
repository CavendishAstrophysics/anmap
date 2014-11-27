%{
#include <stdio.h>
#include <string.h>
#include "eqn.h"

#define grballoc(node) (node *) malloc(sizeof(node))
#define yyparse() eeparse()  /* and the parser  */

extern eqnode *equation_base;
extern char errorstring[];
extern int  ptrerrstring;
eqnode *zap;

%}

%union{
	eqnode *y_equ;
	double y_dbl;
	char   *y_str;
	eqn_funs *y_fun;
      };

%token YYLBRACE YYRBRACE YYLSQUARE YYRSQUARE YYCOMMA YYCOLON YYDOTDOT YYDOT
%token <y_dbl> YYNUMBER
%token <y_str> YYNAME
%token <y_fun> YYFUNNAME YYCONSTANT
%type <y_equ> expr simp simppow

%right YYEQUAL
%right YYCOMMA
%left YYPLUS YYMINUS
%nonassoc UMINUS
%left YYTIMES YYDEVIDE YYDOT
%left ASSOCIATE  
%left YYPOWER

%%

equation : expr YYEQUAL expr
	{
	  equation_base = grballoc( eqnode );
	  equation_base->op = '=';
	  equation_base->u.n.l = $1;
	  equation_base->u.n.r = $3;
	}
	| expr
	{
	  equation_base = $1;
	}
	|
	{
	  equation_base = NULL;
	}
	;

expr:	expr YYDOTDOT expr
	{
	  $$ = grballoc( eqnode );
	  $$->op = INTERVAL;
	  $$->u.n.l = $1;
	  $$->u.n.r = $3;
	}
	|  expr YYCOMMA expr
	{
	  $$ = grballoc( eqnode );
	  $$->op = ',';
	  $$->u.n.l = $1;
	  $$->u.n.r = $3;
	}
	|  expr YYPLUS expr
	{
	  $$ = grballoc( eqnode );
	  $$->op = '+';
	  $$->u.n.l = $1;
	  $$->u.n.r = $3;
	}
	| expr YYMINUS expr
	{
	  $$ = grballoc( eqnode );
	  $$->op = '-';
	  $$->u.n.l = $1;
	  $$->u.n.r = $3;
	}
	| expr YYTIMES expr
	{
	  $$ = grballoc( eqnode );
	  $$->op = '*';
	  $$->u.n.l = $1;
	  $$->u.n.r = $3;
	}
	| expr YYDOT expr
	{
	  $$ = grballoc( eqnode );
	  $$->op = '.';
	  $$->u.n.l = $1;
	  $$->u.n.r = $3;
	}
	| expr expr %prec ASSOCIATE
	{
	  $$ = grballoc( eqnode );
	  $$->op = '*';
	  $$->u.n.l = $1;
	  $$->u.n.r = $2;
	}
	| YYMINUS expr %prec UMINUS
        {
          $$ = grballoc( eqnode );
          $$->op = '-';
          $$->u.n.l = grballoc( eqnode );
          $$->u.n.l-> op = NUMBER;
          $$->u.n.l->u.num = (double) 0;
          $$->u.n.r = $2;
        }
        | YYPLUS expr %prec UMINUS
        {
          $$ = $2;
        }
	| simppow
	| simppow YYDEVIDE simppow 
	{
	  $$ = grballoc( eqnode );
	  $$->op = '/';
	  $$->u.n.l = $1;
	  $$->u.n.r = $3;
	}
	| simppow YYDEVIDE YYMINUS simppow 
	{
	  $$ = grballoc( eqnode );
	  $$->op = '/';
	  $$->u.n.l = $1;
	  $$->u.n.r = grballoc( eqnode );
	  $$->u.n.r->op = '-';
	  $$->u.n.r->u.n.l = grballoc( eqnode );
	  $$->u.n.r->u.n.l->op = NUMBER;
	  $$->u.n.r->u.n.l->u.num = 0.0;
	  $$->u.n.r->u.n.r = $4;
	}
	;

simppow:  simp
	| simp YYPOWER simp
	{
	  $$ = grballoc( eqnode );
	  $$->op = '^';
	  $$->u.n.l = $1;
	  $$->u.n.r = $3;
	}
	| simp YYPOWER YYMINUS simp
	{
	  $$ = grballoc( eqnode );
	  $$->op = '^';
	  $$->u.n.l = $1;
	  $$->u.n.r = grballoc( eqnode );
	  $$->u.n.r->op = '-';
	  $$->u.n.r->u.n.l = grballoc( eqnode );
	  $$->u.n.r->u.n.l->op = NUMBER;
	  $$->u.n.r->u.n.l->u.num = 0.0;
	  $$->u.n.r->u.n.r = $4;
	}
	;

simp:   YYNAME
	{
	  zap = grballoc( eqnode );
	  $$ = zap;
	  $$->op = NAME;
	  $$->u.str = (char *) calloc( strlen($1)+1 , sizeof( char ) );
	  strcpy($$->u.str,$1);
	}
	| YYNUMBER
	{
	  $$ = grballoc( eqnode );
	  $$->op = NUMBER;
	  $$->u.num = $1;
	}
	| YYFUNNAME simp
	{
	  $$ = grballoc( eqnode );
	  $$->op = FUNCTION;
	  $$->u.f.f = $1;
	  $$->u.f.a = $2;
	}
	| YYCONSTANT
	{
	  $$ = grballoc( eqnode );
	  $$->op = FUNCTION;
	  $$->u.f.f = $1;
	  $$->u.f.a = NULL;
	}
	| YYLBRACE expr YYRBRACE
	{
	  $$ = $2;
	}
	| YYLSQUARE expr YYCOMMA expr YYRSQUARE
	{
	  $$ = grballoc( eqnode );
	  $$->op = INTERVAL;
	  $$->u.n.l = $2;
	  $$->u.n.r = $4;
	}
%%
yyerror(string)
char *string;
{
  int i;

  fprintf(stderr,"yyerror: %s\n",string);
  fprintf(stderr,"equation to date is :-\n");

  for( i = 0; i< ptrerrstring; ++i ) putc( errorstring[i], stderr );

  fprintf(stderr,"\n");
}
