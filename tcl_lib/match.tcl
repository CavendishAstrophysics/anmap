#
# tcl functions to do compound matching
#
# cmatch :::: do compound matching, return true is s1 is a valid
#             abreviation for s2, normal rules for compound matches apply.
#  Parameters:
#     s1  ==  sub string to test
#     s2  ==  full string to test against
proc cmatch { s1 s2 } {
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

# lcmatch :::: do compound matching against a list returning number
#              of matches and first matching element
# Parameters:
#     s    ==  sub-string for match
#     ls   ==  list of strings to match against
#     ms   ==  variable name to contain returned match from string
# Returned:
#     count of matches of sub-string in list
proc lcmatch { s ls ms } {
  upvar 1 $ms m
  set iamb 0
  set inf 1
  for {set i 0} {$i < [llength $ls]} { incr i } {
    set p [lindex $ls $i]
    set iamb [expr "$iamb + [cmatch $s $p]"]
    if "$iamb == 1" then {
      if $inf then {
        set m $p
        set inf 0
      }
    }
  }
  return $iamb
}

# prmatch :::: do compound matching against a list printing to stdout
#              all matches 
# Parameters:
#     s    ==  sub-string for match
#     ls   ==  list of strings to match against
proc prmatch { s ls } {
  for {set i 0} {$i < [llength $ls]} { incr i } {
    set p [lindex $ls $i]
    if [cmatch $s $p] then {
      puts $p
    }
  }
}

# lcexec :::: execute command if a compound match is found to the
#             command_array list, else execute system command
# Parameters:
# command_array   ===   array of commands and execution parameters
# command         ===   command name test and execute
# args            ===   arguments passed to command
proc lcexec { command_array command default args } {
  upvar 1 $command_array ca
  set cl [array names ca]
  set iamb [lcmatch $command $cl com]
  if "$iamb == 0" then {
    return 0
  } else {
    if "$iamb > 1" then {
      return $iamb
    } else {
      if [string length $ca($com)] then {
        eval $ca($com) $args
      } else {
        eval $default $com $args
      }
      return 1
    }
  }
}






