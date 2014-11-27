#
# procedure to manage colour maps from Anmap map-display system
#
proc cmap { opt args } {
  global Anmap_UserDir
  global pgcmap
  global colour_map

# decode command line options
  if ![info exists pgcmap(default)] then {
    set pgcmap(default) $colour_map
  }
  set name default
  set file $Anmap_UserDir/cmap.tcl
  for {set n 0} {$n < [llength $args]} {incr n} {
    set m [lindex $args $n]
    case $m in {
      {-name}     {incr n ; set name [lindex $args $n]}
      {default}   {puts "*** Unknown qualifier $m" ; return ""}
    }
  }

# take action on option specified to command
  case $opt in {
    {dump}    { set pgcmap($name) "$colour_map" }
    {load}    { if [info exists pgcmap($name)] then {
                  eval map-display modify-lookup-table $pgcmap($name)
                }
              }
    {save}    { set fid [open $file w]
                foreach n [array names pgcmap] {
                  puts $fid "set pgcmap($n) \{$pgcmap($n)\}"
                }
                close $fid
              }
    {recover} { if [file exists $file] then {
                  source $file
                }
              }
    {restore} { if [file exists $file] then {
                  source $file
                }
              }
    {delete}  { if [info exists pgcmap($name)] {
                  unset pgcmap($name)
                }
              }
    {default}   {puts "*** Unknown option $opt" ; return ""}

  }
  return ""
}

               

