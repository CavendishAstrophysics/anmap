#
# System wide init_anmap :::: initialisation file for Anmap
#
# In this file we define commands not regarded as part of the base
# anmap system, but additions, aliases etc.
#

# Setup welcome message to Anmap
set Anmap(Message) \
"\t Welcome to Version $Anmap(Version) See Help/News for details
\t =>\t New command angular-flux in map-analysis
\t =>\t Additional command line options
\t =>\t New version of map-analysis multi-fit"
if {!$Xanmap} {puts $Anmap(Message)}

# setup source path the Anmap help
set env(ANMHELP) $Anmap(src)/help
set env(ANMSRC) $Anmap(src)

# setup map directory environment
set env(MAPSDIR) $env(HOME)/images

# setup environment for spawned processes (implementation dependent)
set env(PRINTER) hplaser
set env(R_DISPOSE) "lpr -Phplaser -r %s"

# load modules for essential sub-systems
module load map-catalogue
module load edit-redtape
module load graphics

# define default catalogue handling parameters
imcat_setdef $env(HOME)/images
imcat_reportMode on

# help / information commands
  command_create list-commands -global -command command_list

# load modules for optional sub-systems
module load ra
module load clean
module load data-display
module load edit-image
module load map-analysis
module load nmr

# run user initialisation file if present
if [file exists $Anmap_UserDir/init_anmap.tcl] then {
  source $Anmap_UserDir/init_anmap.tcl
}

# move to user specific directory
# this is done now so user can overide defaults
if { !$Anmap(init:cwd) } then {
   if { [string length $Anmap(init:dir)] } then {
	cd $Anmap(init:dir)
   } else {
	cd ~/mrao
   }
}
