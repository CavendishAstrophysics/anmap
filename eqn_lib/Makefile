# This file contains the directorys for installed thing to go in.
# Please use full path-names.


CFLAGS  = -O -cc
YFLAGS  = -d
OBJ = eqnbase.o eqnexpand.o eqnnames.o eqnfunct.o eqndiff.o eqnrpe.o \
	eqnpoly.o eqnsubst.o yacc.o lex.o eqnvrpe.o

# system dependencies
sunos : libeqn.a
	ranlib ../lib/libeqn.a

solaris : libeqn.a

eqntool: eqntool.o $(OBJ)
	cc eqntool.o $(OBJ) -lm -o eqntool

libeqn.a: $(OBJ) 
	ar ru ../lib/libeqn.a $(OBJ)

clean:
	rm -f $(OBJ) eqntool.o eqntool eval1 eval2

