/*
 * File:	eqntool.c
 * Action:	interactive equation handling
 * Date:	9/4/93
 * Author:	Richard Morris
 */

#include <stdlib.h>
#include <stdio.h>
#include "string.h"
#include "eqn.h"

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#define MAX_EQNS 50

/*
*/
#define USE_VRPE

eqnode *eqns[MAX_EQNS];		/* An array of equations */
eqnode *subeqns[MAX_EQNS];	/* the substitution equations */
int	num_eqns;		/* The number of equations */
eqn_funs *fun_list;		/* A list of function definitions */

int	expand_flag = FALSE;	/* Perform expansion? */
int	diff_flag = FALSE;	/* Perform differentiation */
int	clean_flag = TRUE;	/* Clean up all equations */
int	funct_flag = TRUE;	/* Use the function definitions */
int	sub_flag = FALSE;	/* Try substitutions */

/*
 * Function:	process_eqn
 * Action:	for each equation read in we want to do certain actions
 *		which are caried out here.
 */

process_eqn(eqn)
eqnode *eqn;
{
	int i,n;
	eqn_names *namelist = NULL;

	if(clean_flag)  eval_funs(eqn); /* remove instances like cos(Pi) */
	if(expand_flag) expand(eqn);	/* expand the equation */
	if(clean_flag)  eval_funs(eqn);

			/* Now see if its a sub */
	if( sub_flag && eqnop(eqn) == '=' && eqnop(eqnl(eqn)) == NAME 
		&& eqnop(eqnr(eqn)) != INTERVAL )
	{
		/* an asignment see if name in other eqns */

	        for(i=0;i<num_eqns;++i)
               		namelist = add_eqn_names(namelist,eqns[i]);

		n = num_parameters(namelist);
		for(i=0;i<n;++i)
		{
			if(!strcmp(get_parameter(namelist,i+1),
				eqnname(eqnl(eqn)) ) )
			{
				/* matches now do substitution */

				for(i=0;i<num_eqns;++i)
				{
					substitute(eqns[i],eqn);
				}
				return;
			}
		}
	}
	else if( sub_flag && eqnop(eqn) == '=' && eqnop(eqnl(eqn)) == '*' 
		&& eqnop(eqnl(eqnl(eqn))) == NAME
		&& eqnop(eqnr(eqn)) != INTERVAL )
	{
		/* an asignment of type f(x) see if name in other eqns */

	        for(i=0;i<num_eqns;++i)
               		namelist = add_eqn_names(namelist,eqns[i]);

		n = num_parameters(namelist);
		for(i=0;i<n;++i)
		{
			if(!strcmp(get_parameter(namelist,i+1),
				eqnname(eqnl(eqnl(eqn)))) )
			{
				/* matches now do substitution */

				for(i=0;i<num_eqns;++i)
				{
					substitute(eqns[i],eqn);
				}
				return;
			}
		}
	}
			
	eqns[num_eqns++] = eqn;		/* add this to list of equations */
}

/*
 * Function:	ask
 * action:	called with the -A flag, repeatedly asks for all the
 *		values and then prints out the values of the rpe's
 */

ask()
{
	eqn_names *namelist = NULL;
	int	*rpes[MAX_EQNS];
	int	num_args[MAX_EQNS];
	char	*namearray[MAX_EQNS];
	double	vals[MAX_EQNS],*ptr;
	int	i,j,n,failed;

/* get the names from all the equations */

	for(i=0;i<num_eqns;++i)
		namelist = add_eqn_names(namelist,
				eqns[i]);

/* find the number of names */

	n = num_parameters(namelist);

/* construct an array of the names */

	for(i=0;i<n;++i)
	{
		namearray[i] = get_parameter(namelist,i+1);
	}
	
/* Build the reverse polish equations(rpe) */

	for(i=0;i<num_eqns;++i)
	{
		if(clean_flag) eval_ops(eqns[i]);
#ifdef USE_VRPE
		num_args[i] = count_eqn_args(eqns[i]);
		rpes[i] = make_vrpe(eqns[i],n,namearray);
#else
		rpes[i] = make_rpe(eqns[i],n,namearray);
#endif
	}

/* Loop till EOF */

	while(1)
	{
	/* Print out the names */
		for(i=0;i<n;++i)
		{
			printf("%s ",namearray[i]);
		}
		printf("?\n");
		failed = FALSE;
	/* Input the values */
		for(i=0;i<n;++i)
		{
			if(scanf("%lf",&vals[i]) == EOF)
			{
				failed = TRUE;
				break;
			}
		}
		if(failed) break;

	/* evaluate the rpe with these values */
		for(i=0;i<num_eqns;++i)
		{
#ifdef USE_VRPE
			ptr = eval_vrpe(rpes[i],vals);
			for(j=0;j<num_args[i];++j)
			{
				printf("%f ",*(ptr+j));
			}
#else
			printf("%f ",
			eval_rpe(rpes[i],vals));
#endif
		}
		printf("\n");
	}
	free_eqn_names(namelist);
}

/*
 * Function:	conv_poly
 * Action:	convert each equation into a polynomial and print it out
 */

conv_poly()
{
	eqn_names *namelist = NULL;
	int	*rpes[MAX_EQNS];
	char	*namearray[MAX_EQNS];
	int	i,n;
	double	poly1[MAXORDER];
	double	poly2[MAXORDER][MAXORDER];
	double	poly3[MAXORDER][MAXORDER][MAXORDER];

/* get the names from all the equations */
	for(i=0;i<num_eqns;++i)
		namelist = add_eqn_names(namelist,
				eqns[i]);
/* find the number of names */
	n = num_parameters(namelist);

	if( n < 1 || n > 3 )
	{
		fprintf(stderr,"Bad number of names %d for conversion into polynomial\n");
		fprintf(stderr,"should be one two or three\n",n);
		return;
	}

	for(i=0;i<n;++i)
	{
		namearray[i] = get_parameter(namelist,i+1);
	}
	if( n == 1 )
	{
		for(i=0;i<num_eqns;++i)
		{
			if(clean_flag) eval_ops(eqns[i]);
			expand(eqns[i]); /* equations must be expanded
						before converting to polys */
			init_poly1(poly1);
			add_to_poly1(eqns[i],poly1,namearray[0]);
			print_poly1(poly1);
		}
	}
	else if( n == 2 )
	{
		for(i=0;i<num_eqns;++i)
		{
			if(clean_flag) eval_ops(eqns[i]);
			expand(eqns[i]); /* equations must be expanded
						before converting to polys */
			init_poly2(poly2);
			add_to_poly2(eqns[i],poly2,namearray[0],namearray[1]);
			print_poly2(poly2);
		}
	}
	else if( n == 3)
	{
		for(i=0;i<num_eqns;++i)
		{
			if(clean_flag) eval_ops(eqns[i]);
			expand(eqns[i]); /* equations must be expanded
						before converting to polys */
			init_poly3(poly3);
			add_to_poly3(eqns[i],poly3,
				namearray[0],namearray[1],namearray[2]);
			print_poly3(poly3);
		}
	}
	free_eqn_names(namelist);
}

/*
 * Function:	main
 * Action:	the main routine, loop through arguments and do whats necessary
 */

main(argc,argv)
int argc; char **argv;
{
	int i,c,n,is_eqn;
	FILE *fp;
	eqnode *eqn;
	char   *str_ptr;
	int	output = FALSE;

	fun_list = add_standard_functions(NULL); /* create a list of functions */
	set_input_functions(fun_list);

	while(--argc)	/* Loop through arguments */
	{
		++argv;

		if(argv[0][0] == '-' && argv[0][1] == '\0')
		{
			/* arg is - read from stdin untill EOF */

			while( (eqn = scan_eqn()) != NULL)
				process_eqn(eqn);
		}
		else if( argv[0][0] == '-' && argv[0][2] == '\0')
		{
			switch(argv[0][1])
			{
			case 'c': /* clean */
				clean_flag = !clean_flag;
			/* if clean flag is set clean all equations */
				if(clean_flag)
				    for(i=0;i<num_eqns;++i)
					eval_funs(eqns[i]);
				break;

			case 'd': /* Differentiate */
				if( argc == 1 || argv[1][0] == '-')
				{
					fprintf(stderr,"must specify a name for differentiation\n");
					break;
				}
			/* For each equation differentiate wrt argv[1]
				and the optionally clean the equation */

				for(i=0;i<num_eqns;++i)
				{
					if(clean_flag) eval_ops(eqns[i]);
					diff_wrt(eqns[i],argv[1]);
					if(clean_flag) eval_funs(eqns[i]);
				}
				++argv; --argc;
				break;

			case 'e': /* Expand */
				expand_flag = !expand_flag;
			/* if expand flag is set expand all equations */
				if(expand_flag)
				    for(i=0;i<num_eqns;++i)
				    {
					if(clean_flag) eval_ops(eqns[i]);
					expand(eqns[i]);
				    }
				break;

			case 'f': /* Use functions */
				funct_flag = !funct_flag;
			/* if function flag is use the standard functions in
				all the equations */
				if(funct_flag)
					set_input_functions(fun_list);
				else
					set_input_functions(NULL);
				break;

			case 'i': /* Input one equation from stdin */
				if( (eqn = scan_eqn()) != NULL)
					process_eqn(eqn);
				break;

			case 's': /* flip substitution flag */
				sub_flag = !sub_flag;
				break;

			case 'A': /* Convert to rpe's and ask for values */
				output = TRUE;
				ask();
				break;

			case 'D': /* Display equations */
				output = TRUE;
				for(i=0;i<num_eqns;++i)
				{
					if(clean_flag) eval_ops(eqns[i]);
					display_eqn(eqns[i],0);
	 				printf("\n");
				}
				break;

			case 'F': /* Print function definitions */
				output = TRUE;
				fprint_funs(stdout,fun_list);
				break;

			case 'P': /* Convert to Polynomials */
				output = TRUE;
				conv_poly();
				break;

			case 'T': /* type(print) equations */
				output = TRUE;
				for(i=0;i<num_eqns;++i)
				{
					if(clean_flag) eval_ops(eqns[i]);
					print_eqn(eqns[i]);
	 				printf(";\n");
				}
				break;

			default:
				fprintf(stderr,"Bad option %s\n",argv[0]);
			}
		}

		else	/* Not an option hence a filename or equation */
		{
			i = 0;
			is_eqn = FALSE;
			while( ( c = argv[0][i++] ) != '\0' )
			{
				/* if argument contains space or tab must
					be a equation */
				if( c == ' ' || c == '\t' )
					is_eqn = TRUE;
			}
			if( is_eqn || ( fp = fopen(argv[0],"r") ) == NULL )
			{
			/* If we failed to open argument as a file we
				asume that it's an equation */

				str_ptr = argv[0];
				while( (eqn = sscan_eqn(str_ptr)) != NULL)
				{
					process_eqn(eqn);
					/* Next equation (if it exists)
						will start after semi-colon */
					str_ptr = strchr(str_ptr,';');
					if( str_ptr == NULL ) break;
					++str_ptr;
				}
			}
			else
			{
			/* read in equations from file till EOF */
				while( (eqn = fscan_eqn(fp)) != NULL)
					process_eqn(eqn);
				fclose(fp);
			}
		}
	}

	if( output ) exit(0);

	/* If no equations so far then read from standard input */
	if(num_eqns == 0)
		while( (eqn = scan_eqn()) != NULL)
				process_eqn(eqn);
	
	/* And then print them out */
	for(i=0;i<num_eqns;++i)
	{
		if(clean_flag) eval_ops(eqns[i]);
		print_eqn(eqns[i]);
	 	printf(";\n");
	}
}
