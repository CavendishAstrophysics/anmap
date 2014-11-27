#
# Tcl User interaction library modelled on the MRAO FORTRAN iolib library
#
proc io_initio { } {
   global Iolib
   set Iolib(cl) {}
}

proc io_setcli { args } {
   global Iolib
   if [llength $args] then {
     eval set Iolib(cl) $args
   } else {
     set Iolib(cl) {}
   }
}

proc io_enqcli { args } {
   global Iolib
   return $Iolib(cl)
}

proc io_getline { prompt default } {
   global Iolib
   if ![llength $Iolib(cl)] then {
	set pr [format $prompt $default]
	puts -nonewline $pr ; set rt [gets stdin]
	if [llength $rt] then {
	     set Iolib(cl) $rt
	} elseif [llength $default] then {
	     set Iolib(cl) $default
	}
   }
}

proc io_getnext { prompt default } {
   global Iolib
   io_getline $prompt $default
   regsub {,} $Iolib(cl) { } Iolib(cl)
   set word [lindex $Iolib(cl) 0]
   set Iolib(cl) [lrange $Iolib(cl) 1 [llength $Iolib(cl)]]
   return $word
}

proc io_getword { prompt default } {
   global Iolib
   io_getline $prompt $default
   set word [lindex $Iolib(cl) 0]
   set Iolib(cl) [lrange $Iolib(cl) 1 [llength $Iolib(cl)]]
   return $word
}

proc io_geti { prompt default } {
   set doPr 1
   while { $doPr } {
     set doPr [catch {format %d [io_getnext $prompt $default]} result]
     if { $doPr } {puts "*** integer value required ($result)"}
   }
   return $result
}

proc io_getnvals { prompt default n proc} {
   for {set i 0} {$i < $n} {incr i} {
	if { $i > 0 } then {
	   set pr "\t ... [expr $n - $i] more values required \[%s\] : "
	   set def [lrange $default $i [llength $default]]
	} else {
	   set pr $prompt ; set def $default
	}
	lappend result [$proc $pr $def]
   }
   return $result
}

proc io_getni { prompt default n } {
   return [io_getnvals $prompt $default $n io_geti]
}

proc io_getnr { prompt default n } {
   return [io_getnvals $prompt $default $n io_getr]
}

proc io_getnwords { prompt default n } {
   return [io_getnvals $prompt $default $n io_getnext]
}

proc io_getr { prompt default } {
   set doPr 1
   while { $doPr } {
     set doPr [catch {format %g [io_getnext $prompt $default]} result]
     if { $doPr } {puts "*** real value required ($result)"}
   }
   return $result
}

proc io_getstring { prompt default } {
   global Iolib
   io_getline $prompt $default
   set result $Iolib(cl) ; set Iolib(cl) {}
   return $result
}

proc io_getopt { prompt default options } {
   set doPr 1
   while {$doPr} {
	set opt [io_getnext $prompt $default]
	if { "$opt" == "?" } then {
	     puts "Options:"
	     for {set n 0} {$n < [llength $options]} {incr n} {
		puts [lindex $options $n]
	     }
	} else {
	     set iamb 0 ; set mt {}
	     for {set n 0} {$n < [llength $options]} {incr n} {
		if [io_cmatch $opt [lindex [lindex $options $n] 0]] then {
		   lappend mt [lindex [lindex $options $n] 0]
		   incr iamb
		}
	     }
	     if { $iamb == 1 } then {
	        return $mt
	     } elseif { $iamb > 1 } then {
		puts "*** Ambiguous option $opt; matching options are:"
		for {set n 0} {$n < [llength $mt]} {incr n} {
		    puts -nonewline "[lindex $mt $n]\t"
		} ; puts {}
	     } else {
		puts "*** Unknown option $opt"
	     }
	}
   }
}

proc io_onoff { prompt default } {
   set doPr 1
   while { $doPr } {
	set res [io_getnext $prompt $default]
	switch -glob -- $res {
	    {y*}   {set result 1 ; set doPr 0}
	    {n*}   {set result 0 ; set doPr 0}
	    {on}   {set result 1 ; set doPr 0}
	    {of*}  {set result 0 ; set doPr 0}
	    {1}    {set result 1 ; set doPr 0}
	    {0}    {set result 0 ; set doPr 0}
	}
	if { $doPr } then {
	    puts "*** Invalid response $res need 1/0 or yes/no or on/off"
	}
   }
   return $result
}

proc io_cmatch { s1 s2 } {
  set l1 [split $s1 -]
  set l2 [split $s2 -]
  if "[llength $l1] > [llength $l2]" then {
    return 0
  }
  for {set i 0} {$i < [llength $l1]} {incr i} {
    set p1 [lindex $l1 $i]
    set p2 [lindex $l2 $i]
    if [string length $p1] then {
      if [string first $p1 $p2] "return 0"
    }
  }
  return 1
}
