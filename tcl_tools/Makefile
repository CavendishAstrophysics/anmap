#
# Makefile for the desktop suite of applications
#

# setup a path to tclsh
ANMAP=/mrao/anmap
TCLSH=/soft/tcl/bin/tclsh
WISH=/soft/tcl/bin/wish
WISHX=/soft/tcl/bin/wishx
WISHXB=/soft/tcl/bin/ndt_wish
SRC=/soft/tcl/ndt

#
#
# Do not modify anything below here
#
# ========================================================================
#
#
# list supported operating systems:
OS_LIST=\
	solaris	sunos	linux


APPLICS=\
	AnmapInit\
	graph_tool\
	graph_config\
	pshow

UTIL=\
	Xanmap		Anmap \
	Sanmlink	anmlink		speclink \
	tklink

# default should specify the user must name an OS
default :
	@ echo "Specify an Operating system i.e. make OS where OS is:"
	@ echo "${OS_LIST}" 

# targets to rebuild opearting system dependencies
${OS_LIST} : applics
	${TCLSH} ./make_sys $@ ${ANMAP} ${UTIL}
	cd ../bin ; rm -f anmap ; ln -s ./Anmap ./anmap
	cd ../bin ; rm -f xanmap ; ln -s ./Xanmap ./xanmap

applics: 
	${TCLSH} ./make_src ${ANMAP} ${WISH} ${WISHX} ${WISHXB} \
	${SRC} ${APPLICS}

clean:
	cd ../bin ; rm -f ${APPLICS} ${UTIL}

