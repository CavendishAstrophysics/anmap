#
# Basic Initialisation file for all Anmap tools
# 
#
global Anmap
global AnmapSource Anmap_UserDir Desktop auto_path

# define search paths and elements of the Anmap global array
set AnmapSource /mrao/anmap
set Anmap_UserDir $env(HOME)/mrao
set Anmap(sysOptions) anmaprc
set Anmap(userOptions) ~/.anmaprc
set Anmap(src) $AnmapSource
set Anmap(lib) $AnmapSource/tcl_lib
set Anmap(UserDir) $Anmap_UserDir
set Anmap(imcat) $Anmap_UserDir/map-catalogue.mctg

# prepare the map catalogue
if ![file exists $Anmap(imcat)] then {
	iclPriv open
	iclPriv close

}
if [catch {file readlink $Anmap(imcat)}] then {
	if ![file exists $Anmap(UserDir)/default.mctg] then {
		exec mv $Anmap(imcat) $Anmap(UserDir)/default.mctg
		exec ln -s $Anmap(UserDir)/default.mctg $Anmap(imcat)
	}
}

# update search path for executables and tcl libraries
set env(PATH) $AnmapSource/bin:/mrao/bin/:$env(PATH)
set src /soft/tcl/ndt7.4 ; lappend auto_path $src/lib
lappend auto_path $Anmap(src)/tcl_lib
set env(SPECTRUM) $Anmap(src)/etc

# define static Anmap globals
set Anmap(Ted) {}

# define Anmap version number
set Anmap(Version) X0.54a
set Anmap(version) X0.54a
set Anmap(date) 9/2/96

# load default desktop initialisation file
if $Xanmap then {
  if [file exists ~/.wishrc] then {source ~/.wishrc}
}

# initialise user I/O
io_initio

