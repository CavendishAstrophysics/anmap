#
# The routines implemented in this file provide a tcl programmers interface
# to the map catalogue within Anmap.
#

proc imcat { option args } {

global iclRec iclPriv

# sort out option and take appropriate action
switch -- $option {
{open}	{
	iclPriv open
	}
{close}	{
	iclPriv close
	}
{get}	{
	if {[llength $args] != 2} then {
		return -code error \
			{wrong # arguments; should be \"get ce variable\"}
	}
	set ce [lindex $args 0] ; set var [lindex $args 1]
	iclPriv read $ce
	upvar 1 $var v
	foreach n [array names iclRec] {set v($n) $iclRec($n)}
	return {}
	}
{put}	{
	if {[llength $args] != 2} then {
		return -code error \
			{wrong # arguments; should be \"put ce variable\"}
	}
	set ce [lindex $args 0] ; set var [lindex $args 1]
	upvar 1 $var v
	foreach n [array names iclRec] {set iclRec($n) $v($n)}
	iclPriv write $ce
	return {}
	}
{maxce} {
	return $iclPriv(maxce)
	}
{delete} {
	if {[llength $args] != 1} then {
		return -code error \
			{wrong # arguments; should be \"delete ce\"}
	}
	set ce [lindex $args 0] ; iclPriv read $ce
	exec rm -f $iclRec(file)
	imcat remove $ce
	return {}
	}
{remove} {
	if {[llength $args] != 1} then {
		return -code error \
			{wrong # arguments; should be \"remove ce\"}
	}
	set ce [lindex $args 0] ; iclPriv read $ce
	foreach n {data access unit open type page} {set iclRec($n) 0}
	foreach n {file source program} {set iclRec($n) {}}
	iclPriv write $ce
	return {}
	}
{rename} {
	if {[llength $args] < 2} then {
		return -code error \
		  {wrong # arguments; should be \"rename ce source ?program?\"}
	}
	set ce [lindex $args 0] ; iclPriv read $ce
	set source [lindex $args 1]
	if [string length $source] then {
		set iclRec(source) $source
	}
	if {[llength $args] > 2} then {
		set program [lindex $args 2]
		if [string length $program] then {
			set iclRec(program) $program
		}
	}
	iclPriv write $ce
	return {}
	}
{export}
	{
	if {[llength $args] != 2} then {
		return -code error \
			{wrong # arguments; should be \"export ce file\"}
	}
	set ce [lindex $args 0] ; set file [lindex $args 1] ; iclPriv read $ce
	set cwd [pwd] ; cd [file dirname $iclRec(file)]
	exec mv $iclRec(file) $file
	set file [glob $file]
	if ![string match "/" [string range $file 0 0]] then {
		set file [pwd]/$file
	}
	set iclRec(file) [glob $file] ; cd $cwd
	set iclRec(page) 0 ; iclPriv write $ce
	return {}
	}
{clear} {
	if {[llength $args] != 1} then {
		return -code error \
			{wrong # arguments; should be \"clear ce\"}
	}
	set ce [lindex $args 0] ; iclPriv read $ce
	set iclRec(access) 0 ; iclPriv write $ce
	return {}
	}
{initialise} {
	for {set ce 1} {$ce <= $iclPriv(maxce)} {incr ce} {
		iclPriv read $ce
		foreach n {data access unit open type page} {set iclRec($n) 0}
		foreach n {file source program} {set iclRec($n) {}}
		iclPriv write $ce
	}
	return {}
	}
{list} {
	set ceFrom 1 ; set ceTo $iclPriv(maxce) ; set out stdout
	if {[llength $args] >= 1} {set ceFrom [lindex $args 0]} 
	if {[llength $args] >= 2} {set ceTo [lindex $args 1]} 
	if {[llength $args] == 3} {set out [lindex $args 2]} 
	for {set ce $ceFrom} {$ce <= $ceTo} {incr ce} {
		iclPriv read $ce
		if { $iclRec(data) } then {
			puts $out [format "%3s    %-26s  %8s   %4s x %4s   %s" \
			$ce $iclRec(source) $iclRec(program) \
			$iclRec(xdim) $iclRec(ydim) \
			$iclPriv(page:$iclRec(page))$iclPriv(access:$iclRec(access)) ]
		}
	}
	return {}
	}
{displayCe} {
	set ceFrom 1 ; set ceTo $iclPriv(maxce) ; set out stdout
	if {[llength $args] >= 1} {set ceFrom [lindex $args 0]} 
	if {[llength $args] >= 2} {set ceTo [lindex $args 1]} 
	if {[llength $args] == 3} {set out [lindex $args 2]} 
	for {set ce $ceFrom} {$ce <= $ceTo} {incr ce} {
		iclPriv read $ce
		puts $out [format "%3s    %-26s  %8s   %4s x %4s   %s" \
		$ce $iclRec(source) $iclRec(program) \
		$iclRec(xdim) $iclRec(ydim) \
		$iclPriv(page:$iclRec(page))$iclPriv(access:$iclRec(access)) ]
		puts $out [format "  file %s" $iclRec(file)]
		puts $out [format "  data %d access %d unit %d open %d type %d page %d"\
			$iclRec(data) $iclRec(access) $iclRec(unit) \
			$iclRec(open) $iclRec(type) $iclRec(page)]
	}
	return {}
	}
{temp-delete} {
	for {set ce 1} {$ce <= $iclPriv(maxce)} {incr ce} {
		iclPriv read $ce
		if {$iclRec(page)} then {
			exec rm -f $iclRec(file)
			foreach n {data access unit open type page} {
				set iclRec($n) 0
			}
			foreach n {file source program} {set iclRec($n) {}}
			iclPriv write $ce
		}
	}
	return {}
	}
{move}	{
	if {[llength $args] != 2} then {
		return -code error \
			{wrong # arguments; should be \"move ceFrom ceTo\"}
	}
	set ceFrom [lindex $args 0] ; set ceTo [lindex $args 1]
	iclPriv read $ceTo
	if { $iclRec(data) == 0 } then {
		iclPriv read $ceFrom ; iclPriv write $ceTo
		imcat remove $ceFrom
	} else {
		return -code error {destination CE is not free}
	}
	return {}
	}
{next}	{
	set next 0
	for {set n 1} {$n <= $iclPriv(maxce)} {incr n} {
		iclPriv read $n
		if {$iclRec(data) == 0} then {
			set next $n ; break
		}
	}
	return $next
	}
{verify} {
	for {set ce 1} {$ce <= $iclPriv(maxce)} {incr ce} {
		iclPriv read $ce
		if {$iclRec(data)} then {
		   if ![file exists $iclRec(file) ] then {
			foreach n {data access unit open type page} {
				set iclRec($n) 0
			}
			foreach n {file source program} {set iclRec($n) {}}
			iclPriv write $ce
		   }
		}
	}
	return {}
	}
{match} {
	if {[llength $args] != 1} then {
		return -code error \
			{wrong # arguments; should be \"match ce_test\"}
	}
	set r [lindex $args 0]
	if [catch {format %d $r}] then {
		set ll {}
		for {set n 1} {$n <= $iclPriv(maxce)} {incr n} {
		    iclPriv read $n
		    if {$iclRec(data)} then {
			if [io_cmatch $r $iclRec(source)-$iclRec(program)] then {
			    lappend ll $n 
			}
		    }
		}
		if [llength $ll] then {
			return $ll
		} else {
			return 0
		}
	} else {
		if { [expr $r > 0] && [expr $r <= $iclPriv(maxce)] } then {
			return $r
		} else {
			return 0
		}
	}
	}
{result} {
	if {[llength $args] < 1} then {
		return -code error \
			{wrong # arguments; should be \"result option ?vals?\"}
	}
	switch -- [lindex $args 0] {
		{initialise}	{iclPriv result-init}
		{set}		{iclPriv result-set [lindex $args 2]}
		{last}		{return [lindex [iclPriv result-enq] 0]}
		{list}		{return [iclPriv result-enq]}
	}
	}
{stack} {
	if {[llength $args] < 1} then {
		return -code error \
			{wrong # arguments; should be \"stack option ?vals?\"}
	}
	switch -- [lindex $args 0] {
		{initialise}	{catch {iclPriv stack-initialise}}
		{display-stack}	{catch {iclPriv stack-display 1}}
		{display-maps}	{catch {iclPriv stack-display 2}}
		{display-size}	{catch {iclPriv stack-display 3}}
	}
	}
{default} {
	return -code error \
		"unknown option $option; should be \"get, put,\
		maxce, delete, remove, rename, export,\
		move, temp-delete, initialise, or clear\""
	}
}
}

#
# utility routine to prompt user for a valid catalogue entry
#
proc imcat_getCat { prompt } {

# prompt for a  string
  set r [io_getword $prompt [iclPriv enqdef]]
  return [imcat match $r]
}

#
# list-catalogue
#
proc imcat_list { args } {
  io_setcli $args
  imcat list [io_geti "From-CE %s : " 1] [io_geti "To-CE %s : " [imcat maxce]]
}

proc imcat_remove { args } {
  io_setcli $args
  set lce [imcat_getCat "Catalogue entry \[%s\] : "]
  if {[llength $lce] > 1} then {
	return -code error "Ambiguous catalogue entry"
  }
  if {$lce > 0} then {imcat remove $lce}
}

proc imcat_delete { args } {
  io_setcli $args
  set lce [imcat_getCat "Catalogue entry \[%s\] : "]
  if {[llength $lce] > 1} then {
	return -code error "Ambiguous catalogue entry"
  }
  if {$lce > 0} then {imcat delete $lce}
}

proc imcat_rename { args } {
  io_setcli $args
  set lce [imcat_getCat "Catalogue entry \[%s\] : "]
  if {[llength $lce] > 1} then { return -code error "Ambiguous catalogue entry" }
  if {$lce > 0} then {
	imcat rename $lce [io_getword "New source name : " {}]
  }
}

proc imcat_examine { args } {
  io_setcli $args
  set lce [imcat_getCat "Matching catalogue entries : "]
  foreach ce $lce {
	if {$ce > 0} then {imcat list $ce $ce}
  }
}

proc imcat_export { args } {
  io_setcli $args
  set lce [imcat_getCat "Catalogue entry \[%s\] : "]
  if {[llength $lce] > 1} then { return -code error "Ambiguous catalogue entry" }
  if {$lce > 0} then {imcat export $lce [io_getword "File name : " {}]}
}

proc imcat_clear { args } {
  io_setcli $args
  set lce [imcat_getCat "Catalogue entry \[%s\] : "]
  foreach ce $lce {
	if {$ce > 0} then {imcat clear $ce}
  }
}

proc imcat_defmap { args } {
  io_setcli $args
  set lce [imcat_getCat "Catalogue entry \[%s\] : "]
  if {[llength $lce] > 1} then { return -code error "Ambiguous catalogue entry" }
  if {$lce > 0} then {imcat setdef $lce}
}

proc imcat_displayCe { args } {
  io_setcli $args
  imcat displayCe [io_geti "From-CE %s : " 1] [io_geti "To-CE %s : " [imcat maxce]]
}

proc imcat_loadCat { args } {
  global Anmap
  io_setcli $args
  imcat close ; imcat stack initialise
  set cat [io_getword "Catalogue-file \[%s\] : " "default.mctg"]
  set fc [file_fullName $cat $Anmap(UserDir) mctg]
  if [file exists $fc] then {
	iclPriv close
	exec rm -f $Anmap(imcat)
	exec ln -s $fc $Anmap(imcat)
	iclPriv open
  } else {
	iclPriv close
	exec rm -f $Anmap(imcat)
	iclPriv open ; iclPriv close
	exec mv $Anmap(imcat) $fc
	exec ln -s $fc $Anmap(imcat)
	iclPriv open
  }
}


proc imcat_reportMode { args } {
  global env iclPriv
  io_setcli $args
  set iclPriv(report) [io_onoff "Report-Mode (on/off) \[%s\] : " "on"]
  catch {iclPriv report $iclPriv(report)}
  set env(anm_ReportMode) $iclPriv(report)
}

proc imcat_maxArea { args } {
  global iclPriv
  io_setcli $args
  set iclPriv(maxArea) [io_getr "Max-area-read \[%s\] : " "0.1"]
  catch {iclPriv set-area $iclPriv(maxArea)}
}

proc imcat_setdef { args } {
  global iclPriv
  io_setcli $args
  set iclPriv(defdir) [io_getword "Default-map-directory \[%s\] : "\
                       $iclPriv(defdir) ]
  iclPriv set-directory $iclPriv(defdir)
}

proc imcat_enqdef { } {
  global iclPriv
  set iclPriv(defdir) [iclPriv enq-directory]
  return $iclPriv(defdir)
}

proc imcat_addToCat { args } {
  global iclPriv iclRec
  io_setcli $args
  set lf [io_getword "File matching \[%s\] : " [imcat_enqdef]]
  foreach lf1 $lf {
	foreach file [glob $lf1] {
		set n [imcat next]
		if { $n > 0 } then {
			if ![catch {iclPriv enqmap $file}] then {
				set iclRec(data) 1
				regsub -all {\.} [file tail $file] {-} src
				regsub -all {_} $src {-} iclRec(source)
				iclPriv write $n
				if { $iclPriv(report) } then {
				   puts "$file added as CE $n $iclRec(source)"}
			}
		}
	}
  }
}

