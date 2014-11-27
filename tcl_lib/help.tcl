#
# Implement command listing commands
#
#
proc ? { args } {
  global anmap_sys
  if {[string length [info global $anmap_sys.info]] > 0} then {
     upvar #0 $anmap_sys.info info
  } else {
    upvar #0 $anmap_sys.commands info
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
  global anmap_sys
  if {[string length [info global $anmap_sys.info]] > 0} then {
     upvar #0 $anmap_sys.info info
  } else {
    upvar #0 $anmap_sys.commands info
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

#
# procedures to implement help
proc main.news { } {
   global AnmapSource
   anmap_system "/mrao/bin/help @$AnmapSource/help/anmap.news"
}
proc main.help { args } {
   global AnmapSource
   anmap_system "/mrao/bin/help @$AnmapSource/help/anmap.help $args"
}
