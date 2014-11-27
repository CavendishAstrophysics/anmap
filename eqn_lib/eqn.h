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

#ifndef EQN_HEADER
#define EQN_HEADER

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

/* Codes used for the operator in equations and rpe's */
/* Can also have '+' '-' '*' '/' '^' '=' ',' 	      */

#define NAME -23
#define NUMBER -24
#define INTERVAL -25
#define BRACKET -26
#define FUNCTION 3

/* Special codes used in rpe calculator */

#define SUM1	5
#define SUM2	6
#define SUM3	7
#define SUB1	8
#define SUB2	9
#define SUB3	10 
#define DOT2	11
#define	DOT3	12
#define	CROSS3	13
#define SCALE2	14
#define SCALE3	15
#define MULT1	16
#define DIV1	17
#define POW1	18
#define INT_POW 19
#define EQUALS1 20

/* Maximum order for polynomials */

#define MAXORDER 25

/* Where to read an equation from */

#define EQNFROM_STDIN 1
#define EQNFROM_FILE 2
#define EQNFROM_STRING 3

/* The main type for equations */

struct Fnode;	/* Forward def */

typedef struct Eqnode
{
	short int op;	/* The operator */
	union
	{
		struct
		{
			struct Eqnode *l, *r; /* left + right sub trees */
		} n;
		struct
		{
			struct Fnode *f;	/* Function	*/
			struct Eqnode *a;	/* Its arguments */
		} f;
	    
		double num;			/* Value for NUMBERS */
		char   *str;			/* String for NAMES */
	} u;
} eqnode;

typedef eqnode eqn_node;
typedef eqnode * eqn_ptr;

#define VARIABLE 1
#define PARAMETER 2

typedef struct Nnode   /* a node about the list of names */
{
  int type;			/* The type of name	*/
  char *str;			/* The name		*/
  struct Nnode *next;		/* next name in list	*/
} eqn_names;

/* The list of functions */

#define EXTERN_FUN 4
#define INTERN_FUN 5
#define CONSTANT_FUN 6
#define OPERATOR 7
#define EXTERN_MAP 8
#define INTERN_MAP 9

typedef struct Fnode
{
	short   type;		/* The type of function 		*/
	char    *name;		/* The name				*/
	short   nvars;		/* The number of arguments		*/
	short	dim;		/* Dimension of target space		*/
	char	**vars;		/* The list of varible names		*/
	double  (*fun)();	/* Pointer to single valued function	*/
	double  *(*vfun)();	/* Pointer to multivalued mapping	*/
	eqn_ptr (*op)();	/* Pointer to operator			*/
	int	*rpe;		/* Reverse polish string		*/
	int	*vrpe;		/* Vetor rpe mappings			*/
	double  val;		/* Value of constant			*/
	eqnode  *eqn;		/* The equation for the function	*/
	eqnode	**diff;		/* Equations for each derivative	*/
	struct  Fnode *next;	/* The next function in the list.	*/
} eqn_funs;

typedef int rpeint;	/* The type used in rpe strings */

/* Definitions from eqnbase.c */

extern eqnode	*scan_eqn(), *fscan_eqn(), *sscan_eqn(), *duplicate();
extern		display_eqn(), print_eqn(), fprint_eqn();
extern		copy_node(), free_eqn_tree(), free_eqn_node();
extern int	count_eqn_tree(), count_eqn_args();
extern eqnode	*get_eqn_arg(), *join_dup_eqns(), *join_eqns();

/* Definitions from eqnexpand.c */

extern		clean_eqn(),eval_funs(),eval_ops();
extern int	expand(), multiply_out(), devide_out(), raise_out();

/* Definition from eqndiff.c */

extern 		diff_wrt(), diff_fun_wrt();
extern eqnode	*diff_wrt_eqn();

/* Definitions from eqnsubst.c */

extern		substitute();
extern eqnode	*assign();

/* Definitions from eqnpoly.c */

extern		fprint_poly2(), print_poly2(), init_poly2(), order_poly2();
extern		fprint_poly3(),	print_poly3(), init_poly3(), order_poly3();
extern int	add_to_poly2(), sub_from_poly2();
extern int	add_to_poly3(), sub_from_poly3();
extern int	eval_term2(), eval_term3();

/* Definitions from eqnrpe.c */

extern int	make_rpe2(), check_rpe();
extern rpeint	*make_rpe();
extern		fprint_rpe(), print_rpe(), clear_rpe_const();
extern double	eval_rpe();

/* Definitions from eqnvrpe.c */

extern int	make_vrpe2(), check_vrpe();
extern rpeint	*make_vrpe();
extern		fprint_vrpe(), print_vrpe(), clear_vrpe_const();
extern double	*eval_vrpe();

/* Definitions from eqnnames.c */

extern eqn_names	*add_eqn_names();
extern		print_eqn_names();
extern		fprint_eqn_names();
extern int	num_parameters();
extern void	free_eqn_names();
extern int	make_variable();
extern char	*get_parameter();

/* Definitions from eqnfunct.c */

extern eqn_funs	*add_external_function();
extern eqn_funs	*add_internal_function();
extern eqn_funs	*add_standard_functions();
extern eqn_funs	*add_constant();
extern eqn_funs	*add_operator();
extern		fprint_funs();
extern int	use_functions();
extern 		set_input_functions();
extern eqn_funs *get_input_functions();

/* Macros */

#define	eqn_op(eqn)	(eqn == NULL ? NULL : (eqn)->op)
#define eqn_l(eqn)	(eqn == NULL ? NULL :(eqn)->u.n.l)
#define	eqn_r(eqn)	(eqn == NULL ? NULL :(eqn)->u.n.r)
#define eqn_val(eqn)	(eqn == NULL ? NULL : (eqn)->op == NUMBER ? (eqn)->u.num : NULL)
#define eqn_name(eqn)	(eqn == NULL ? NULL : (eqn)->op == NAME ? (eqn)->u.str : NULL)
#define eqn_fun(eqn)	(eqn == NULL ? NULL : (eqn)->op == FUNCTION ? (eqn)->u.f.f : NULL)
#define eqn_arg(eqn)	(eqn == NULL ? NULL : (eqn)->op == FUNCTION ? (eqn)->u.f.a : NULL)

#define	eqnop(eqn)	((eqn)->op)
#define eqnl(eqn)	((eqn)->u.n.l)
#define	eqnr(eqn)	((eqn)->u.n.r)
#define eqnval(eqn)	((eqn)->u.num)
#define eqnname(eqn)	((eqn)->u.str)
#define eqnfun(eqn)	((eqn)->u.f.f)
#define eqnarg(eqn)	((eqn)->u.f.a )

#define remove_eqn_name(namelist,name)	make_varible(namelist,name)
#define num_eqn_names(namelist)	num_parameters(namelist)
#define get_eqn_name(namelist,i)	get_parameter(namelist,i)

#endif /* EQN_HEADER */
