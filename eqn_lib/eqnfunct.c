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
#define USE_VRPE
*/
#define SILLYFUNS

#define grballoc(node) (node *) malloc(sizeof(node))
#define MAX(a,b)       a > b ? a : b ;
#define TRUE 1
#define FALSE 0

extern	int rpe_const_ptr;
int	funs_const_max;
eqn_funs *base_funs = NULL;		/* The functions used by scan-eqn */

set_input_functions(funlist)
eqn_funs *funlist;
{
	base_funs = funlist;
}

eqn_funs *get_input_functions()
{
	return(base_funs);
}

/************************************************************************/
/*									*/
/*	Now we have some routines to handle functions			*/
/*	There are two different types of function			*/
/*	external functions like sin(x) which can are evaluated		*/
/*	by a call to a 'C' routine,					*/
/*	and internal functions which are defined by an equation		*/
/*	and converted into an rpe.					*/
/*	We also need to know about the derivatives of a function	*/
/*									*/
/************************************************************************/
 
/*
 * Function:	add_external_function
 * Action;	adds an external function to a list
 */

eqn_funs *add_external_function(funlist,profile,funptr,va_alist)
eqn_funs *funlist;
char	*profile;
double	(*funptr)();
va_dcl
{
	va_list ap;
	eqn_funs *new,*fun;
	int	i;
	eqnode *eqn,*args;
	char	*string;

	eqn = sscan_eqn(profile);
	if( eqn == NULL )
	{
		fprintf(stderr,"NULL profile for external function\n");
		return(funlist);
	}
	if( eqn->op != '*' || eqn->u.n.l->op != NAME)
	{
		fprintf(stderr,"Bad profile for external function\n");
		fprintf(stderr,"%s\n",profile);
		return(funlist);
	}
	new = grballoc(eqn_funs);
	new->next = funlist;
	new->type = EXTERN_FUN;
	new->fun =  funptr;
	new->rpe = NULL;

	new->name = (char *) calloc(strlen(eqn->u.n.l->u.str)+1,
			sizeof(char));
	strcpy(new->name,eqn->u.n.l->u.str);

	/* Now work out the number of arguments */

	new->nvars = count_eqn_args(eqn->u.n.r);
	new->vars = (char **) calloc(new->nvars,sizeof(char *));

	/* And find the names */

	for(i=0;i<new->nvars;++i)
	{
		args = get_eqn_arg(eqn->u.n.r,i+1);

		if( args->op == NAME )
		{
			new->vars[i] = (char *) calloc(strlen(args->u.str)+1,
				sizeof(char));
			strcpy(new->vars[i],args->u.str);
		}
		else
		{
			fprintf(stderr,
				"Bad syntax for external function profile\n");
			fprintf(stderr,"%s\n",profile);
			return(funlist);
		}
	}

	/* Now the derivatives */

	va_start(ap);
	new->diff = (eqnode **) calloc(new->nvars,sizeof(eqnode *));
	
	for(i=0;i<new->nvars;++i)
	{
		string = va_arg(ap,char *);
		new->diff[i] = sscan_eqn(string);
	}
	va_end(ap);

	/* Now use this function for derivatives of all other functions */

	new->next = NULL;

	fun = funlist;
	while( fun != NULL )
	{
		if( fun->type == EXTERN_FUN || fun->type == INTERN_FUN 
		 || fun->type == EXTERN_MAP || fun->type == INTERN_MAP )
		    for(i=0;i<fun->nvars;++i)
			use_functions(fun->diff[i],new);
		fun = fun->next;
	}
	new->next = funlist;

	/* Finally use this and all other functions on the derivatives */
	
	for(i=0;i<new->nvars;++i)
		use_functions(new->diff[i],new);

	return(new);
}
 
/*
 * Function:	add_internal_function
 * Action;	adds an internal function to a list
 */

eqn_funs *add_internal_function(funlist,eqn)
eqn_funs *funlist;
eqnode  *eqn;
{
	eqn_funs *new,*fun;
	int	i;
	eqnode	*args;

	if( eqn == NULL )
	{
		fprintf(stderr,"Tried to add a NULL internal function\n");
		return(funlist);
	}
	if( eqn->op != '=' || eqn->u.n.l->op != '*'
		|| eqn->u.n.l->u.n.l->op != NAME)
	{
		fprintf(stderr,"Bad syntax for internal function equation\n");
		fprint_eqn(stderr,eqn);
		return(funlist);
	}
	new = grballoc(eqn_funs);
	new->next = NULL;
	new->type = INTERN_FUN;
	use_functions(eqn->u.n.r,funlist);
	new->eqn  = duplicate(eqn->u.n.r);
	new->name = (char *) calloc(strlen(eqn->u.n.l->u.n.l->u.str)+1,
			sizeof(char));
	strcpy(new->name,eqn->u.n.l->u.n.l->u.str);

	/* Now work out the number of arguments */

	new->nvars = count_eqn_args(eqn->u.n.l->u.n.r);
	new->vars = (char **) calloc(new->nvars,sizeof(char *));

	/* And find the names */

	for(i=0;i<new->nvars;++i)
	{
		args = get_eqn_arg(eqn->u.n.l->u.n.r,i+1);

		if( args->op == NAME )
		{
			new->vars[i] = (char *) calloc(strlen(args->u.str)+1,
				sizeof(char));
			strcpy(new->vars[i],args->u.str);
		}
#ifdef NOT_DEF
		else if( args->op == ',' && args->u.n.l->op == NAME )
		{
			new->vars[i] = (char *) calloc(
				strlen(args->u.n.l->u.str)+1, sizeof(char));
			strcpy(new->vars[i],args->u.n.l->u.str);
			args = args->u.n.r;
		}
#endif
		else
		{
			fprintf(stderr,
				"Bad syntax for internal function equation\n");
			fprint_eqn(stderr,eqn);
			return(funlist);
		}
	}

	/* Compute the rpe */

#ifdef USE_VRPE
	new->rpe = make_vrpe(eqn->u.n.r,new->nvars,new->vars);
#else
	new->rpe = make_rpe(eqn->u.n.r,new->nvars,new->vars);
#endif
	new->fun =  NULL;

	/* Now the derivatives */

	new->diff = (eqnode **) calloc(new->nvars,sizeof(eqnode *));

	for(i=0;i<new->nvars;++i)
	{
		new->diff[i] = duplicate(new->eqn);
		diff_wrt(new->diff[i],new->vars[i]);
		clean_eqn(new->diff[i]);
	}
	
	/* Now use this function for derivatives of all other functions */

	new->next = NULL;	/* insures this is only fun in list */

	fun = funlist;
	while( fun != NULL )
	{
		if( fun->type == EXTERN_FUN || fun->type == INTERN_FUN 
		 || fun->type == EXTERN_MAP || fun->type == INTERN_MAP )
		    for(i=0;i<fun->nvars;++i)
			use_functions(fun->diff[i],new);
		fun = fun->next;
	}
	
	new->next = funlist;	/* Now we have the complete list */

	/* Finally use this and all other functions on the derivatives */
	
	for(i=0;i<new->nvars;++i)
		use_functions(new->diff[i],new);

	return(new);
}

/*
 * Function:	add_constant
 * Action;	adds a constant to list of functions
 */

eqn_funs *add_constant(funlist,name,value)
eqn_funs *funlist;
char *name; double value;
{
	eqn_funs *new,*fun;
	int	i;
	eqnode	*args;

	new = grballoc(eqn_funs);
	new->type = CONSTANT_FUN;
	new->name = (char *) calloc(strlen(name)+1,sizeof(char));
	strcpy(new->name,name);
	new->val = value;
	new->next = funlist;
	return(new);
}

/*
 * Function:	add_operator
 * Action;	adds an operator to a list
 */

eqn_funs *add_operator(funlist,name,funptr)
eqn_funs *funlist;
char	 *name;
eqn_ptr	(*funptr)();
{
	eqn_funs *new,*fun;
	int	i;
	eqnode	*args;

	new = grballoc(eqn_funs);
	new->type = OPERATOR;
	new->name = (char *) calloc(strlen(name)+1,sizeof(char));
	strcpy(new->name,name);
	new->op =  funptr;
	new->next = funlist;
	return(new);
}
 
/*
 * Function:	add_external_mapping
 * Action;	adds an external mapping to a list
 */

eqn_funs *add_external_mapping(funlist,profile,dim,funptr,va_alist)
eqn_funs *funlist;
char	*profile;
int	dim;
double	*(*funptr)();
va_dcl
{
	va_list ap;
	eqn_funs *new,*fun;
	int	i;
	eqnode *eqn,*args;
	char	*string;

	eqn = sscan_eqn(profile);
	if( eqn == NULL )
	{
		fprintf(stderr,"NULL profile for external mapping\n");
		return(funlist);
	}
	if( eqn->op != '*' || eqn->u.n.l->op != NAME)
	{
		fprintf(stderr,"Bad profile for external mapping\n");
		fprintf(stderr,"%s\n",profile);
		return(funlist);
	}
	new = grballoc(eqn_funs);
	new->next = funlist;
	new->type = EXTERN_MAP;
	new->vfun =  funptr;
	new->rpe = NULL;
	new->dim = dim;

	new->name = (char *) calloc(strlen(eqn->u.n.l->u.str)+1,
			sizeof(char));
	strcpy(new->name,eqn->u.n.l->u.str);

	/* Now work out the number of arguments */

	new->nvars = count_eqn_args(eqn->u.n.r);
	new->vars = (char **) calloc(new->nvars,sizeof(char *));

	/* And find the names */

	for(i=0;i<new->nvars;++i)
	{
		args = get_eqn_arg(eqn->u.n.r,i+1);

		if( args->op == NAME )
		{
			new->vars[i] = (char *) calloc(strlen(args->u.str)+1,
				sizeof(char));
			strcpy(new->vars[i],args->u.str);
		}
		else
		{
			fprintf(stderr,
				"Bad syntax for external mapping profile\n");
			fprintf(stderr,"%s\n",profile);
			return(funlist);
		}
	}

	/* Now the derivatives */

	va_start(ap);
	new->diff = (eqnode **) calloc(new->nvars,sizeof(eqnode *));
	
	for(i=0;i<new->nvars;++i)
	{
		string = va_arg(ap,char *);
		new->diff[i] = sscan_eqn(string);
	}
	va_end(ap);

	/* Now use this mapping for derivatives of all other mappings */

	new->next = NULL;

	fun = funlist;
	while( fun != NULL )
	{
		if( fun->type == EXTERN_FUN || fun->type == INTERN_FUN 
		 || fun->type == EXTERN_MAP || fun->type == INTERN_MAP )
		    for(i=0;i<fun->nvars;++i)
			use_functions(fun->diff[i],new);
		fun = fun->next;
	}
	new->next = funlist;

	/* Finally use this and all other mappings on the derivatives */
	
	for(i=0;i<new->nvars;++i)
		use_functions(new->diff[i],new);

	return(new);
}
 
/*
 * Function:	add_internal_mapping
 * Action;	adds an internal mapping to a list
 */

eqn_funs *add_internal_mapping(funlist,eqn)
eqn_funs *funlist;
eqnode  *eqn;
{
	eqn_funs *new,*fun;
	int	i;
	eqnode	*args;

	if( eqn == NULL )
	{
		fprintf(stderr,"Tried to add a NULL internal mapping\n");
		return(funlist);
	}
	if( eqn->op != '=' || eqn->u.n.l->op != '*'
		|| eqn->u.n.l->u.n.l->op != NAME)
	{
		fprintf(stderr,"Bad syntax for internal mapping equation\n");
		fprint_eqn(stderr,eqn);
		return(funlist);
	}
	new = grballoc(eqn_funs);
	new->next = NULL;
	new->type = INTERN_MAP;
	use_functions(eqn->u.n.r,funlist);
	new->eqn  = duplicate(eqn->u.n.r);
	new->name = (char *) calloc(strlen(eqn->u.n.l->u.n.l->u.str)+1,
			sizeof(char));
	strcpy(new->name,eqn->u.n.l->u.n.l->u.str);

	/* Now work out the number of arguments */

	new->nvars = count_eqn_args(eqn->u.n.l->u.n.r);
	new->vars = (char **) calloc(new->nvars,sizeof(char *));

	/* And find the names */

	for(i=0;i<new->nvars;++i)
	{
		args = get_eqn_arg(eqn->u.n.l->u.n.r,i+1);

		if( args->op == NAME )
		{
			new->vars[i] = (char *) calloc(strlen(args->u.str)+1,
				sizeof(char));
			strcpy(new->vars[i],args->u.str);
		}
		else
		{
			fprintf(stderr,
				"Bad syntax for internal mapping equation\n");
			fprint_eqn(stderr,eqn);
			return(funlist);
		}
	}

	/* Now work out dimension of target */

	new->dim = count_eqn_args(eqn->u.n.r);

	/* Compute the rpes */

	new->vrpe = make_vrpe(eqn->u.n.r, new->nvars,new->vars);
	new->fun =  NULL;

	/* Use previous function definitions on equation */

	use_functions(new->eqn,funlist);

	/* Now the derivatives */

	new->diff = (eqnode **) calloc(new->nvars,sizeof(eqnode *));

	for(i=0;i<new->nvars;++i)
	{
		new->diff[i] = duplicate(new->eqn);
		diff_wrt(new->diff[i],new->vars[i]);
		eval_funs(new->diff[i]);
	}
	
	/* Now use this mapping for derivatives of all other functions */

	new->next = NULL;	/* insures this is only fun in list */

	fun = funlist;
	while( fun != NULL )
	{
		if( fun->type == EXTERN_FUN || fun->type == INTERN_FUN 
		 || fun->type == EXTERN_MAP || fun->type == INTERN_MAP )
		    for(i=0;i<fun->nvars;++i)
			use_functions(fun->diff[i],new);
		fun = fun->next;
	}
	
	new->next = funlist;	/* Now we have the complete list */

	/* Finally use this and all other functions on the derivatives */
	
	for(i=0;i<new->nvars;++i)
		use_functions(new->diff[i],new);

	return(new);
}

/*
 * Function:	fprint_funs
 * Action:	prints out the list of functions
 */

fprint_funs(fp,funs)
FILE *fp;
eqn_funs *funs;
{
	int i;

	while(funs != NULL )
	{
	    switch(funs->type)
	    {
  	    case CONSTANT_FUN: 
		fprintf(fp,"CONSTANT %s = %15.13lf\n",funs->name,funs->val );
		break;

	    case OPERATOR:
		fprintf(fp,"OPERATOR %s\n",funs->name);
		break;

	    case EXTERN_FUN:
		fprintf(fp,"FUNCTION %s(",funs->name);
		for(i=0;i<funs->nvars;++i)
		{
			if(i>0) fprintf(fp," , ");
			fprintf(fp,"%s",funs->vars[i]);
		}
		fprintf(fp,")\t");
		fprintf(fp,"defined internally\n");
		for(i=0;i<funs->nvars;++i)
		{
			fprintf(fp,"\td%s_d%s = ",funs->name,funs->vars[i]);
			fprint_eqn(fp,funs->diff[i]);
			fprintf(fp,";\n");
		}
		break;

	    case INTERN_FUN:
		fprintf(fp,"FUNCTION %s(",funs->name);
		for(i=0;i<funs->nvars;++i)
		{
			if(i>0) fprintf(fp," , ");
			fprintf(fp,"%s",funs->vars[i]);
		}
		fprintf(fp,") = ");
		fprint_eqn(fp,funs->eqn);
		fprintf(fp,";\n");
		for(i=0;i<funs->nvars;++i)
		{
			fprintf(fp,"\td%s_d%s = ",funs->name,funs->vars[i]);
			fprint_eqn(fp,funs->diff[i]);
			fprintf(fp,";\n");
		}
		break;

	    case EXTERN_MAP:
		fprintf(fp,"MAPPING %s(",funs->name);
		for(i=0;i<funs->nvars;++i)
		{
			if(i>0) fprintf(fp," , ");
			fprintf(fp,"%s",funs->vars[i]);
		}
		fprintf(fp,")\t");
		fprintf(fp,"dimension %d defined internally\n",funs->dim);
		for(i=0;i<funs->nvars;++i)
		{
			fprintf(fp,"\td%s_d%s = ",funs->name,funs->vars[i]);
			fprint_eqn(fp,funs->diff[i]);
			fprintf(fp,";\n");
		}
		break;

	    case INTERN_MAP:
		fprintf(fp,"MAPPING %s(",funs->name);
		for(i=0;i<funs->nvars;++i)
		{
			if(i>0) fprintf(fp," , ");
			fprintf(fp,"%s",funs->vars[i]);
		}
		fprintf(fp,") = ");
		fprint_eqn(fp,funs->eqn);
		fprintf(fp,";\n");
		for(i=0;i<funs->nvars;++i)
		{
			fprintf(fp,"\td%s_d%s = ",funs->name,funs->vars[i]);
			fprint_eqn(fp,funs->diff[i]);
			fprintf(fp,";\n");
		}
		break;

	    default:
		fprintf(fp,"Unknow function type %d\n",funs->type);
		break;
	    }
	    funs = funs->next;
	}
}

/*
 * Function:	use_functions
 * Action:	substitutes each occurance of a function name with
 *		a pointer to the function.
 */

int use_functions(eqn,funlist)
eqnode *eqn;
eqn_funs *funlist;
{
	eqn_funs *fun;
	int leftres, rightres;
	eqnode	*temp;

	if( eqn == NULL )
	{
		fprintf(stderr,"NULL equation in use_functions\n");
		return(FALSE);
	}
#ifdef PRI_USE_FUNS
	fprintf(stderr,"use_functions: eqn\n");
	fprint_eqn(stderr,eqn);
	fprintf("\n");
#endif

	if(eqn->op == '*' && eqn->u.n.l->op == NAME)
	{
		fun = funlist;
		while(fun != NULL )
		{
			if((fun->type == EXTERN_FUN || fun->type == INTERN_FUN )
				&& !strcmp(eqn->u.n.l->u.str,fun->name) )
			{
				free_eqn_tree(eqn->u.n.l);
				eqn->op = FUNCTION;
				temp = eqn->u.n.r;
				eqn->u.f.a = temp;
				eqn->u.f.f = fun;
				return(use_functions(eqn->u.f.a,funlist));
			}
			fun = fun->next;
		}
		leftres = use_functions(eqn->u.n.l,funlist);
		rightres = use_functions(eqn->u.n.r,funlist);
		return(leftres && rightres);
	}
	else if( eqn->op == NAME )
	{
		fun = funlist;
		while(fun != NULL)
		{
			if(fun->type == CONSTANT_FUN &&
				!strcmp(eqn->u.str,fun->name) )
			{
				free(eqn->u.str);
#ifdef NOT_DEF
				eqn->op = NUMBER;
				eqn->u.num = fun->val;
#endif
				eqn->op = FUNCTION;
				eqn->u.f.f = fun;
				eqn->u.f.a = NULL;
				return(TRUE);
			}
			fun = fun->next;
		}
		return(TRUE);
	}
	else if( eqn->op == NUMBER ) return(TRUE);
	else if( eqn->op == FUNCTION )
	{
		if(eqn->u.f.f->type == CONSTANT_FUN)
			return(TRUE);
		return(use_functions(eqn->u.f.a,funlist));
	}
	else if( eqn->op == BRACKET )
		return(use_functions(eqn->u.n.r,funlist));

	leftres = use_functions(eqn->u.n.l,funlist);
	rightres = use_functions(eqn->u.n.r,funlist);
	return(leftres && rightres );
}

#ifdef SILLYFUNS
double t3(x,y,z)
double x,y,z;
{
	fprintf(stderr,"t3(%f,%f,%f)\n",x,y,z);
	return((10 * x + y)*10+z);
}

double t4(w,x,y,z)
double w,x,y,z;
{
	fprintf(stderr,"t4(%f,%f,%f,%f)\n",w,x,y,z);
	return((10 * (10 * w + x) + y)*10+z);
}

double t5(v,w,x,y,z)
double v,w,x,y,z;
{
	fprintf(stderr,"t5(%f,%f,%f,%f,%f)\n",v,w,x,y,z);
	return((10 * (10 * (10 * v + w) + x) + y)*10+z);
}
#endif

double sgn(x)
double x;
{
	if( x != x ) return( x ); /*	NaN */
	return( x > 0.0 ? 1.0 : ( x < 0.0 ? -1.0 : 0.0 ) );
}

double If_fun(a,b,c)
double a,b,c;
{
	if( a != a ) return(a);	/* NaN */
	if( a >= 0 ) return(b);
	else	     return(c);
}

double *unit_fun(a,b,c)
double a,b,c;
{
	double res[3],len;

	len = sqrt(a*a+b*b+c*c);
	if(len == 0.0 || len != len ) len = 1.0;
	res[0] = a/len;
	res[1] = b/len;
	res[2] = c/len;
	return(res);
}

/*
 * Function:	add_standard_functions
 * Action:	add all the standard functions to the list
 */

eqn_funs *add_standard_functions(funlist)
eqn_funs *funlist;
{
	eqnode *eqn;
	static char   *names[2] = {"x","y"};

#ifdef SILLYFUNS
	funlist = add_external_function(funlist,"t3(x,y,z)",t3,
		"100","10","1");
	funlist = add_external_function(funlist,"t4(w,x,y,z)",t4,
		"0","0","0","0");
	funlist = add_external_function(funlist,"t5(v,w,x,y,z)",t5,
		"0","0","0","0","0");
	funlist = add_internal_function(funlist,
		sscan_eqn("tt(x,y,z) = 2 t3(z,y,x)"));
#endif

	funlist = add_external_function(funlist,"cos(x)",cos,"-sin(x)");
	funlist = add_external_function(funlist,"sin(x)",sin,"cos(x)");
	funlist = add_external_function(funlist,"tan(x)",tan,
		"1/((cos(x))^2)");
	
	funlist = add_external_function(funlist,"asin(x)",asin,
			"1/(sqrt(1-x^2))");
	funlist = add_external_function(funlist,"acos(x)",acos,
			"-1/(sqrt(1-x^2))");
	funlist = add_external_function(funlist,"atan(x)",atan,
			"-1/(1+x^2)");

	funlist = add_external_function(funlist,"sinh(x)",sinh,"cosh(x)");
	funlist = add_external_function(funlist,"cosh(x)",cosh,"sinh(x)");
	funlist = add_external_function(funlist,"tanh(x)",sinh,
		"1 - (tanh(x))^2");

	funlist = add_external_function(funlist,"asinh(x)",asinh,
		"1/(sqrt(1+x^2))");
	funlist = add_external_function(funlist,"acosh(x)",acosh,
		"1/(sqrt(x^2-1))");
	funlist = add_external_function(funlist,"atanh(x)",atanh,
		"1/(1-x^2)");

	funlist = add_external_function(funlist,"sqrt(x)",sqrt,
		"1/(2 sqrt(x))");
	funlist = add_external_function(funlist,"exp(x)",exp,"exp(x)");
	funlist = add_external_function(funlist,"pow(x,n)",pow,
		"n*pow(x,n-1)","(ln(n)) (pow(x,n))");
	funlist = add_external_function(funlist,"ln(x)",log,"1/x");

	eqn = sscan_eqn("sec(x)=1/(cos(x))");
	funlist = add_internal_function(funlist,eqn);
	free_eqn_tree(eqn);

	eqn = sscan_eqn("cosec(x)=1/(sin(x))");
	funlist = add_internal_function(funlist,eqn);
	free_eqn_tree(eqn);

	eqn = sscan_eqn("cot(x)=1/(tan(x))");
	funlist = add_internal_function(funlist,eqn);
	free_eqn_tree(eqn);

	funlist = add_external_function(funlist,"abs(x)",fabs,"sgn(x)");
	funlist = add_external_function(funlist,"sgn(x)",sgn,"0");

	eqn = sscan_eqn("max(x,y)=(x+y+abs(x-y))/2");
	funlist = add_internal_function(funlist,eqn);
	free_eqn_tree(eqn);

	eqn = sscan_eqn("min(x,y)=(x+y-abs(x-y))/2");
	funlist = add_internal_function(funlist,eqn);
	free_eqn_tree(eqn);

	funlist = add_constant(funlist,"Pi",M_PI);

	funlist = add_operator(funlist,"diff",diff_wrt_eqn);

	funlist = add_external_function(funlist,"if(a,b,c)",If_fun,
			"0","if(a,1,0)","if(a,0,1)");

	eqn = sscan_eqn("dot((a,b,c),(d,e,f))=a d+b e+c f");
	funlist = add_internal_function(funlist,eqn);
	free_eqn_tree(eqn);

	eqn = sscan_eqn("cross((a,b,c),(d,e,f))=(b f-c e,c d-a f,a e-b d)");
	funlist = add_internal_mapping(funlist,eqn);
	free_eqn_tree(eqn);

	eqn = sscan_eqn("ele1(a,b,c)=a");
	funlist = add_internal_function(funlist,eqn);
	free_eqn_tree(eqn);

	eqn = sscan_eqn("ele2(a,b,c)=b");
	funlist = add_internal_function(funlist,eqn);
	free_eqn_tree(eqn);

	eqn = sscan_eqn("ele3(a,b,c)=c");
	funlist = add_internal_function(funlist,eqn);
	free_eqn_tree(eqn);

#ifdef NOT_DEF
	eqn = sscan_eqn("vsum((a,b,c),(d,e,f))=(a+d,b+e,c+f)");
	funlist = add_internal_mapping(funlist,eqn);
	free_eqn_tree(eqn);

	eqn = sscan_eqn("vscale(l,(a,b,c))=(l a,l b,l c)");
	funlist = add_internal_mapping(funlist,eqn);
	free_eqn_tree(eqn);

	eqn = sscan_eqn("vlincomb(l,(a,b,c),m,(d,e,f))=(l a+m d,l b+m e,l c+m f)");
	funlist = add_internal_mapping(funlist,eqn);
	free_eqn_tree(eqn);

	eqn = sscan_eqn("unit(a,b,c)=vscale(1/(sqrt(a^2+b^2+c^2)),(a,b,c))");
	funlist = add_internal_mapping(funlist,eqn);
	free_eqn_tree(eqn);
#endif

/*
"vlincomb((1/(sqrt(a^2+b^2+c^2)),(1,0,0), -a/((sqrt(a^2+b^2+c^2))^3),(a,b,c)",
"vlincomb((1/(sqrt(a^2+b^2+c^2)),(0,1,0), -b/((sqrt(a^2+b^2+c^2))^3),(a,b,c)",
"vlincomb((1/(sqrt(a^2+b^2+c^2)),(0,0,1), -c/((sqrt(a^2+b^2+c^2))^3),(a,b,c)");

"vscale((1/(sqrt(a^2+b^2+c^2))),vlincomb(1,(1,0,0),-a/(a^2+b^2+c^2),(a,b,c)))"
*/
	return(funlist);
}

/*
 * Function:	clear_rpe_user_const
 * Action:	clears the top of the constant pool those defined
 *		in the standard functions are not touched.
 */

clear_rpe_user_const()
{
	rpe_const_ptr = funs_const_max;
}
