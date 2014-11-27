#
# Tcl library procedures to create a basic tcl minimal-match command interpretor
# 
#

#
# Create a top-level interpretor.  This defines a set of strcutures to
# implement minimal-matched commands.
#
proc create_interp { prompt } {

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
    global Anmap
    global auto_noexec auto_noload env unknown_pending anmap_sys
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
    uplevel interp_command $anmap_sys $args
}

#
# Setup default prompt
anmap_prompt $prompt

# redefine system exit command
rename exit _exit

# define global variables used for anmap
global global.commands sysnames anmap_sys anmap_parent anmap_owner shellmode
global anmapnames
  set anmap_sys main
  set anmap_owner main
  set anmap_parent none
  set shellmode 1

# define global commands and procedures used to implement them
proc global.command {args} { eval anmap_command $args }
proc global.exit {args} {
  global anmap_sys anmap_parent
  if [string match $anmap_parent main] then {
    set anmap_sys main
    set anmap_parent none
  } else {
    set anmap_sys $anmap_parent
    set anmap_parent main
  }
  uplevel #0 "anmap_prompt $\{$anmap_sys.prompt\}"

}
set global.commands(abort) {_exit}
set global.commands(exit) {global.exit}
set global.commands(quit) {global.exit}
set global.commands(news) {main.news}
set global.commands(help) {main.help}
set global.commands(list-commands) {command_list}

# define a basic set of commands at the top (main) level
set anmapnames(main) {}
global main.prompt main.commands
set main.prompt $prompt
set main.commands(exit) {_exit}
set main.commands(quit) {_exit}
proc main.command {args} { eval anmap_command $args }
proc main.system { sysname args} {
   global anmap_sys anmap_parent sysnames
   set sys $sysnames($sysname)
   if [llength $args] then {
     eval interp_command $sys $args
   } else {
     uplevel #0 "anmap_prompt $\{$sys.prompt\}"
     set anmap_parent main
     set anmap_sys $sys
   }
}

}

proc create_subsys { name mm_name prompt def_command } {
   global $name.prompt
   global $name.commands
   global global.commands sysnames anmap_sys anmap_parent anmap_owner shellmode
   global anmapnames

   set global.commands($mm_name) "main.system $mm_name"

   set anmapnames($name) $mm_name
   set sysnames($mm_name) $name
   set $name.prompt $prompt
   proc $name.command {args} "eval $def_command \$args"
}
