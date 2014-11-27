# interp_command :::: perform compound string matching on commands
#                     and execute matching commands
# Parametsrs:
#   system  ==  anmap (sub-)system in which commands are searched
#   args    ==  arguments to pass to command
#
# The global command name space is also searched for matches
#
proc interp_command {system args} {
  global   global.commands shellmode
  upvar #0 $system.commands ca
  set command [lindex $args 0]
  set pars [lrange $args 1 [llength $args]]
  set iamb 0
  if !$iamb then {
    if [llength $pars] then {
      set iamb [eval lcexec ca $command $system.command $pars]
    } else {
      set iamb [eval lcexec ca $command $system.command]
    }
  }
  if !$iamb then {
    if [llength $pars] then {
      set iamb [eval lcexec global.commands $command global.command $pars]
    } else {
      set iamb [eval lcexec global.commands $command global.command]
    }
  }
  if "$iamb > 1" then {
    puts "*** Ambiguous command"
    prmatch $command [array names ca]
    prmatch $command [array names global.commands]
  } else {
    if !$iamb then {
      if $shellmode then {
        if [auto_execok $command] {
          return [uplevel anmap_system \"$command $pars\"]
        }
      }
      puts "*** Unknown command $command"
    }
  }
}

#
# Create new minimal-match commands
#
proc command_create { name args } {
  global  anmap_sys
  set sys $anmap_sys
  set command {}
  for {set n 0} {$n < [llength $args]} {incr n} {
    set m [lindex $args $n]
    switch -- $m {
       {-system}  {incr n ; set sys [lindex $args $n]}
       {-global}  {set sys global}
       {-local}   {set sys $anmap_sys}
       {-command} {incr n ; set command [lindex $args $n]}
    }
  }

# create variables in global context
  upvar #0 $sys.commands com

# update command string
  set com($name) "$command"
}

#
# procedure to define prompt strings
proc anmap_prompt {prompt_string} {
  global tcl_prompt1
  set tcl_prompt1 "puts -nonewline \"$prompt_string\""
}

#
# Provide external application launching
#
proc anmap_external { application } {
  iocmd save
  anmap_system $application
  iocmd recover
}

#
# Anmap module system maintenance command 
#
proc module { option args } {
   global Anmap

   switch $option {
     {load}    {set name [lindex $args 0]
 		set sys_file $Anmap(src)/etc/${name}.module
		set user_file $Anmap(UserDir)/${name}.module
                if [file exists $user_file] then {
                  uplevel #0 "source $user_file"
                  lappend Anmap(modList) $name
                } else {
                  if [file exists $sys_file] then {
                    uplevel #0 "source $sys_file"
                    lappend Anmap(modList) $name
                  } else {
                    puts "***(MODULE) Unable to find module $name"
                  }
                 }
               }
     {list}    {puts "\nAvailable modules:"
		puts "------------------"
		foreach file [glob -nocomplain $Anmap(src)/etc/*.module] {
			set name [file tail [file root $file]]
			puts [format "  System:  %-40s" $name]
		}
		foreach file [glob -nocomplain $Anmap(UserDir)/*.module] {
			set name [file tail [file root $file]]
			puts [format "  User:    %-40s" $name]
		}
		puts {}
	       }
     {loaded}  {puts "\nLoaded modules:"
		puts "---------------"
		foreach n $Anmap(modList) {puts [format "  Loaded:  %-40s" $n]}
		puts {}
	       }
     {default} {
                puts "***(MODULE) Unknown option to module procedure"
               }
   }
   return {}
}

