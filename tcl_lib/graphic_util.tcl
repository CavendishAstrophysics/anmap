#
# procedures used to implement additional graphic functions
#
proc refresh_all { } {

# global pointers to Anmap data structures
  global graphic_len_list graphic_list graphic_type graphic_index 
  global current_index current_type
  global anmapnames anmap_sys

# save environment on entry
  set sys $anmap_sys
  set sysname $anmapnames($sys)

# move to graphic sub-system and find state
  graphic
    get current-index
    get current-type
    get active-list

# refresh graphics in order
  for {set n 0} {$n < [llength $graphic_list]} {incr n} {
     get graphic [lindex $graphic_list $n]
     if {$graphic_type == "image"} then {
        select $graphic_type $graphic_index
        map-display plot refresh
     }
  }
  for {set n 0} {$n < [llength $graphic_list]} {incr n} {
     get graphic [lindex $graphic_list $n]
     if {$graphic_type == "graph"} then {
        select $graphic_type $graphic_index
        data-display plot refresh
     }
  }
  for {set n 0} {$n < [llength $graphic_list]} {incr n} {
     get graphic [lindex $graphic_list $n]
     if {$graphic_type == "drawing"} then {
        select $graphic_type $graphic_index
        drawing-display plot refresh
     }
  }
  select $current_type $current_index
  if {$sys == "main" } then {
    exit
  } else {
    eval $sysname
  }
}

proc cursor_position { } {
  global anmap_sys x y
  case $anmap_sys in {
    {draw graph scratch annot} { get cursor-position
                                 puts "Cursor: X = $x    Y = $y"
                               } 
    {default}                  {map-display cursor-position}
  }
}

proc postscript_dump { args } {
  global Anmap_UserDir current_device 

# sort out options
  set file anmap.ps
  for {set n 0} {$n < [llength $args]} {incr n} {
    set m [lindex $args $n]
    case $m in {
      {-file}    {incr n ; set file [lindex $args $n]}
    }
  }

# select output device to be a postscript device and plot
  graphic get current_device
  graphic device-select 3
  graphic output-device $file/ps
  graphic open-device
    refresh_all
  graphic close-device
  graphic device-select $current_device
  if {"[glob $Anmap_UserDir]" != "[pwd]"} then {
     exec mv $file $Anmap_UserDir
  }
}

proc postscript_view { args } {
  global Anmap_UserDir current_device 

# sort out options
  set file anmap.ps
  for {set n 0} {$n < [llength $args]} {incr n} {
    set m [lindex $args $n]
    case $m in {
      {-file}    {incr n ; set file [lindex $args $n]}
    }
  }
  exec ghostview -magstep -5 $Anmap_UserDir/$file -quiet &
}

proc postscript_save { args } {
  global Anmap_UserDir current_device 

# sort out options
  set file anmap.ps
  iocmd clear-cli
  if [llength $args] then {
    eval iocmd set-cli $args
  }
  set nfile [iocmd get-word 'File-name : ' ' ']
  exec mv $file $nfile
}

proc Anmap_postscript { } {

  # create a top level window which can be used to setup postscript
  # output from anmap
  global Anmap_UserDir
  global ps_preview_command env pfile

  set w .anmap_ps
  set pfile $Anmap_UserDir/anmap.ps
  set ps_preview_command 1

  catch {destroy $w}
  toplevel $w
  wm title $w "Anmap Postscript Output"
  wm iconname  $w "Anmap Postscript Output"

  # create and populate the window 
  pack append $w \
    [frame $w.commands -relief flat ] {top fillx pady 2} \
    [frame $w.options -relief flat ] {top expand fill}
  pack append $w.commands \
    [button $w.commands.dismiss -text "Dismiss" \
     -width 10 -command "destroy $w"] {left frame center} \
    [button $w.commands.dump -text "Dump" -width 10 \
     -command "anmap_ps_print $w"] {left frame center} \
    [button $w.commands.print -text "Print" -width 10 \
     -command postscript-print ] \
     {left frame center} \
    [button $w.commands.preview -text "Preview" -width 10 \
     -command anmap_ps_preview ] {left frame center}
  pack append $w.options \
    [frame $w.options.printer -relief flat] {top fillx pady 4 padx 2} \
    [frame $w.options.preview -relief flat] {top fillx pady 4 padx 2}
  pack append $w.options.printer \
    [label $w.options.printer.l -text "Printer: " -width 10 ] \
    {left frame center} \
    [entry $w.options.printer.e -relief sunken -width 14] \
    {left frame w fillx}
  pack append $w.options.preview \
    [radiobutton $w.options.preview.gv -variable ps_preview_command \
     -text "Ghostview" -value 1 \
     -width 15 -relief flat -anchor w] \
     {left frame center} \
    [radiobutton $w.options.preview.gs -variable ps_preview_command \
     -text "Ghostscript" -value 0 \
     -width 15 -relief flat -anchor w] \
     {left frame center}
  $w.options.printer.e insert end $env(PRINTER)
  proc anmap_ps_preview { } {
   global ps_preview_command pfile
   if $ps_preview_command then {
     exec /usr/local/bin/ghostview -magstep -5 $pfile &
   } else {
     exec /usr/local/bin/gs $pfile >> /dev/null &
   }
  }
  proc anmap_ps_print { w } {
  global pfile
  exec lpr -P[$w.options.printer.e get] $pfile &
  }
}







