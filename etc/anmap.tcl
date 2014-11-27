#
# Initialisation file for Anmap X and non-X versions
# 
#
# parse options to Anmap
set Anmap(mapsize) 256
set Anmap(nmaps) 20
set Anmap(buffsize) 1024
set Anmap(nbuff) 4
set Anmap(X) 0
set Anmap(init:dir) {}
set Anmap(init:cwd) 0
for {set n 0} {$n < [llength $argv]} {incr n} {
   switch -- [lindex $argv $n] {
	{-mapsize}	{incr n; set Anmap(mapsize) [lindex $argv $n]}
	{-buffersize}	{incr n; set Anmap(buffsize) [lindex $argv $n]}
	{-nmaps}	{incr n; set Anmap(nmaps) [lindex $argv $n]}
	{-nbuff}	{incr n; set Anmap(nbuff) [lindex $argv $n]}
	{-cd}		{incr n; set Anmap(init:dir) [lindex $argv $n]}
	{-cwd}		{set Anmap(init:cwd) 1}
	{-swd}		{set Anmap(init:cwd) 0}
   }
}

# load basic initialisation
source /mrao/anmap/etc/basic.tcl

if $Xanmap then {
  set Anmap(X) 1 ; Desktop_Init Anmap $src -command Anmap
} else {
  set Anmap(X) 0 ; Desktop_Init Anmap $src -command Anmap -noX
}

# make available standard commands
create_interp "Anmap> "
module load anmap

# initialise anmap
anmap_init	$Anmap(nmaps) [expr $Anmap(mapsize) * $Anmap(mapsize) ] \
		$Anmap(nbuff) $Anmap(buffsize)
iclPriv close
iclPriv open

# startup windows version if required
if $Xanmap then {
  Anmap_ControlPanel
}
# run local initialisation file if present
if [file exists $Anmap(src)/etc/init_anmap.tcl] then {
  source  $Anmap(src)/etc/init_anmap.tcl
}
