
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
{
	eqnode *y_equ;
	double y_dbl;
	char   *y_str;
	eqn_funs *y_fun;
      } YYSTYPE;
extern YYSTYPE yylval;
# define YYLBRACE 257
# define YYRBRACE 258
# define YYLSQUARE 259
# define YYRSQUARE 260
# define YYCOMMA 261
# define YYCOLON 262
# define YYDOTDOT 263
# define YYDOT 264
# define YYNUMBER 265
# define YYNAME 266
# define YYFUNNAME 267
# define YYCONSTANT 268
# define YYEQUAL 269
# define YYPLUS 270
# define YYMINUS 271
# define UMINUS 272
# define YYTIMES 273
# define YYDEVIDE 274
# define ASSOCIATE 275
# define YYPOWER 276
