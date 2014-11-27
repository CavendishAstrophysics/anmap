# Makefile for ANMAP and related programs
#
# Copyright Paul Alexander, MRAO Cambridge
#
# Makefile for Version 7.5a/X0.5 of ANMAP
#
# last updated 22/11/93

LIBS = \
	anm_lib		eqn_lib		plot_lib	spec_lib\
	mapcat	clean_lib	graphic_lib	image_lib\
	ra_lib		tcl_lib

TOOLS = \
	anm_tools	nmr_tools	ra_tools	spec_tools\
	docs

# default target to make the entire package
all : libs tools anmap
anmap : Anmap Xanmap

# library build
libs : $(LIBS)

# target for each library
$(LIBS) : null
	cd $@ ; make solaris


# tool-set build
tools : $(TOOLS)

# target for each tool set
$(TOOLS) : null
	cd $@ ; make

Xanmap : 
	cd src ; rm -f Xanmap
	cd src ; make Xanmap

Anmap : 
	cd src ; rm -f Anmap
	cd src ; make Anmap

Static-Xanmap : 
	cd src ; rm -f SXanmap
	cd src ; make Static-Xanmap

Static-Anmap : 
	cd src ; rm -f SAnmap
	cd src ; make Static-Anmap

# general targets
anmap : libs Xanmap Anmap 

static-anmap : libs Static-Xanmap Static-Anmap

# targets to tidyup after a build
clean :
	rm -f */*.o
	rm -f */*/*.o

cleanAll : clean
	rm -f lib/lib*.a
	cd anm_tools ; make cleanAll
	cd nmr_tools ; make cleanAll
	cd spec_tools ; make cleanAll
	cd ra_tools ; make cleanAll
	cd tcl_tools ; make clean
	cd bin ; rm -f Anmap Xanmap_exec
	cd docs ; make clean

null :
