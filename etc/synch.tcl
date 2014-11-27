#
# Initialisation file for Anmap X and non-X versions
# 
#

source /mrao/anmap/etc/basic.tcl
# make available standard commands
source $Anmap(src)/tcl_lib/match.tcl

# initialise userr I/O
io_initio

# initialise
anmap_init 20 [expr 256*256] 4 1024
iclPriv close
iclPriv open
for {set n 0} {$n < [llength $argv]} {incr n} {
    switch -- [lindex $argv $n] {
	{-command}	{incr n; puts [eval [lindex $argv $n]]}
	{-do}		{incr n; puts [eval [lindex $argv $n]]}
	{-run}		{incr n; puts [eval [lindex $argv $n]]}
    }
}
exit
