#
# Procedures to implement a minimally matched command interpretor on top
# of tcl.  This package extends the command matching of Tcl by adding 
# minimal matching of compound-type commands, as for example:
#
#   list-command-files
#
# which would be mathed by any of:
#   l-c-f   li-com-f   list-com   lis-c-fil
#
# Commands can be organised into sub-systems, each identified with its
# own prompt.
#
#

# interp_create
# -------------
# Create a top-level interpretor.  This defines a set of strcutures to
# implement minimal-matched commands and creates a top-level system which
# is called main together with a global namespace of commands.
#
proc interp_create { prompt src } {
  global Interp

# setup global variables used to define environment
  set Interp(src) $src
  set Interp(UserSrc) [glob ~]/.modules

# Setup default prompt
  interp_prompt $prompt

# redefine system exit command
  rename exit _exit

# define global variables
  global global.commands sysnames
  set Interp(sys) main
  set Interp(owner) main
  set Interp(parent) none
  set Interp(shellmode) 1

# define global commands and procedures used to implement them
   set global.commands(abort) {_exit}
   set global.commands(exit)  {global.exit}
   set global.commands(quit)  {global.exit}
   set global.commands(list-commands) {command_list}
   proc global.command {args} { eval $args }

# define a basic set of commands at the top (main) level
   global main.prompt main.commands
   set main.prompt $prompt
   set main.commands(exit) {_exit}
   set main.commands(quit) {_exit}
   proc main.command {args} { eval $args }

# initialise user I/O
  io_initio

}


# interp_command 
# --------------
# perform compound string matching on commands and execute matching
# commands at the current level (sub-system) or at the global level.
#
# Parametsrs:
#   system  ==  (sub-)system in which commands are searched
#   args    ==  arguments to pass to command
#
proc interp_command {system args} {
  global   Interp global.commands
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
      if $Interp(shellmode) then {
        if [auto_execok $command] {
          return [uplevel iolib_system \"$command $pars\"]
        }
      }
      puts "*** Unknown command $command"
    }
  }
}


# command_create
# --------------
#
# Create new minimal-match commands
#
proc command_create { name args } {
  global  Interp
  set sys $Interp(sys)
  set command {}
  for {set n 0} {$n < [llength $args]} {incr n} {
    set m [lindex $args $n]
    switch -- $m {
       {-system}  {incr n ; set sys [lindex $args $n]}
       {-global}  {set sys global}
       {-local}   {set sys $Interp(sys)}
       {-command} {incr n ; set command [lindex $args $n]}
    }
  }

# create variables in global context
  upvar #0 $sys.commands com

# update command string
  set com($name) "$command"
}


# interp_prompt
# -------------
#
# procedure to define prompt strings
#
proc interp_prompt {prompt_string} {
  global tcl_prompt1
  set tcl_prompt1 "puts -nonewline \"$prompt_string\""
}

# module
# ------
#
# module system maintenance command 
#
# Modules are sets of commands often but not exclusively forming a
# sub-system.  The module system is intended to be available to end
# users to load modules pertenant to the worrk they are performing.
#
proc module { option args } {
   global Interp

   switch $option {
     {load}    {set name [lindex $args 0]
 		set sys_file $Interp(src)/${name}.module
		set user_file $Interp(UserSrc)/${name}.module
                if [file exists $user_file] then {
                  uplevel #0 "source $user_file"
                  lappend Interp(modList) $name
                } else {
                  if [file exists $sys_file] then {
                    uplevel #0 "source $sys_file"
                    lappend Interp(modList) $name
                  } else {
                    puts "***(MODULE) Unable to find module $name"
                  }
                 }
               }
     {list}    {puts "\nAvailable modules:"
		puts "------------------"
		foreach file [glob -nocomplain $Interp(src)/*.module] {
			set name [file tail [file root $file]]
			puts [format "  System:  %-40s" $name]
		}
		foreach file [glob -nocomplain $Interp(UserSrc)/*.module] {
			set name [file tail [file root $file]]
			puts [format "  User:    %-40s" $name]
		}
		puts {}
	       }
     {loaded}  {puts "\nLoaded modules:"
		puts "---------------"
		foreach n $Interp(modList) {puts [format "  Loaded:  %-40s" $n]}
		puts {}
	       }
     {default} {
                puts "***(MODULE) Unknown option to module procedure"
               }
   }
   return {}
}

# create_subsys
# -------------
#
# create a new sub-system
#
proc create_subsys { name mm_name prompt def_command } {
   global $name.prompt
   global $name.commands
   global global.commands Interp sysnames

   set global.commands($mm_name) "main.system $mm_name"
   set sysnames($mm_name) $name
   set $name.prompt $prompt
   proc $name.command {args} "eval $def_command \$args"
}

#
# Procedures used in the above definitions
#
proc main.system { sysname args} {
   global Interp sysnames
   set sys $sysnames($sysname)
   if [llength $args] then {
     eval interp_command $sys $args
   } else {
     uplevel #0 "interp_prompt $\{$sys.prompt\}"
     set Interp(parent) main
     set Interp(sys) $sys
   }
}

proc global.exit {args} {
  global Interp
  if [string match $Interp(parent) main] then {
    set Interp(sys) main
    set Interp(parent) none
  } else {
    set Interp(sys) $Interp(parent)
    set Interp(parent) main
  }
  uplevel #0 "interp_prompt $\{$Interp(sys).prompt\}"

}

#
# unknown
# -------
#
# unknown ::: modified unknown to check for matches to compound command names
#
#	1. See if the autoload facility can locate the command in a
#	   Tcl script file.  If so, load it and execute it.
#       2. Check for a match among defined compound-name commands.
#	3. See if the command exists as an executable UNIX program.
#	   If so, "exec" the command.
#
proc unknown args {
    global Interp
    global auto_noexec auto_noload env unknown_pending
    set name [lindex $args 0]
    if ![info exists auto_noload] {
	#
	# Make sure we're not trying to load the same proc twice.
	#
	if [info exists unknown_pending($name)] {
	    unset unknown_pending($name)
	    if ![array size unknown_pending] {
		unset unknown_pending
	    }
	    error "self-referential recursion in \"unknown\" for command \"$name\"";
	}
	set unknown_pending($name) pending;
	set ret [auto_load $name];
	unset unknown_pending($name);
	if ![array size unknown_pending] {
	    unset unknown_pending
	}
	if $ret {
	    return [uplevel $args]
	}
    }
    uplevel interp_command $Interp(sys) $args
}

#
# Implement command listing commands
#
#
proc ? { args } {
  global Interp
  if {[string length [info global $Interp(sys).info]] > 0} then {
     upvar #0 $Interp(sys).info info
  } else {
    upvar #0 $Interp(sys).commands info
  }
  upvar #0 global.commands ginfo
  if [llength $args] then {
    set com [lindex $args 0]
    foreach name [array names info] {
      if [cmatch $com $name] then {
         puts "[format {  %-45s} $name]"
      }
    }
  } else { 
    foreach name [array names info] {
         puts "[format {  %-45s} $name]"
    }
  }
}

proc command_list { args } {
  global Interp
  if {[string length [info global $Interp(sys).info]] > 0} then {
     upvar #0 $Interp(sys).info info
  } else {
    upvar #0 $Interp(sys).commands info
  }
  upvar #0 global.commands ginfo
  if [llength $args] then {
    set com [lindex $args 0]
    puts " "
    puts "Matching commands at this level:"
    puts "--------------------------------"
    foreach name [array names info] {
      if [cmatch $com $name] then {
         puts "[format {  %-35s} $name]"
      }
    }
    puts " "
    puts "Matching global commands:"
    puts "-------------------------"
    foreach name [array names ginfo] {
      if [cmatch $com $name] then {
         puts "[format {  %-35s} $name]"
      }
    }
  } else { 
    puts " "
    puts "Commands available at this level:"
    puts "---------------------------------"
    foreach name [array names info] {
         puts "[format {  %-35s} $name]"
    }
    puts " "
    puts "Available global commands:"
    puts "--------------------------"
    foreach name [array names ginfo] {
         puts "[format {  %-35s} $name]"
    }
  }
}

