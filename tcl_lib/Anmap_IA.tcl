#
# Anmap script to create a top-level data-analysis window
#
proc Anmap_IA { } {

  global AnmapSource Desktop aia_message
  global aia_pixel_input map_value map_pos 
  global pix_pos pix_value pix_length pix_angle

  catch {destroy .analysis}
  set map_value ""
  set pix_pos ""
  set pix_value ""
  set pix_length ""
  set pix_angle ""

  set w .analysis
  toplevel $w
  wm title $w "Interactive Analysis"
  wm iconname $w "Interactive Analysis"
  wm iconbitmap $w @$AnmapSource/etc/bitmaps/Anmap.xbm

  bind $w <Enter> { global aia_pixel_input ; set aia_pixel_input 0}

# pack main window
  frame $w.left ; frame $w.right
  frame $w.coms
  pack append $w $w.coms {fillx pady 8 top} \
                 $w.left {left filly} $w.right {left filly padx 10}


# item to get catalogue entry
  frame $w.ce
  pack append $w.ce \
   [label $w.ce.l -text "Catalogue-entry: " -anchor w] {left} \
   [entry $w.ce.e -relief sunken -width 30] {left}
  bind $w.ce.e <Return> {Aia_display}

# full image statistics
  frame $w.istat ; frame $w.istat.top ; frame $w.istat.bot
  pack append $w.istat.top \
   [label $w.istat.l1 -text "Image Min: " -anchor w -width 12] {left} \
   [label $w.istat.min  -anchor w -width 16] {left} \
   [label $w.istat.l2 -text "Max: " -anchor w -width 10] {left} \
   [label $w.istat.max  -anchor w -width 16 -anchor w] {left}
  pack append $w.istat.bot \
   [label $w.istat.l3 -text "SD: " -anchor w -width 12] {left} \
   [label $w.istat.sd  -anchor w -width 16 -anchor w] {left} \
   [label $w.istat.l4 -text "UV-range: " -anchor w -width 10] {left} \
   [label $w.istat.uvr  -anchor w -width 16 -anchor w] {left}
  pack append $w.istat $w.istat.top {fillx} $w.istat.bot {fillx} 

# region statistics
  frame $w.rstat ; frame $w.rstat.top ; frame $w.rstat.bot
  pack append $w.rstat.top \
   [label $w.rstat.l1 -text "Region Min: " -anchor w -width 12] {left} \
   [label $w.rstat.min  -anchor w -width 16] {left} \
   [label $w.rstat.l2 -text "Max: " -anchor w -width 10] {left} \
   [label $w.rstat.max  -anchor w -width 16 -anchor w] {left}
  pack append $w.rstat.bot \
   [label $w.rstat.l3 -text "SD: " -anchor w -width 12] {left} \
   [label $w.rstat.sd  -anchor w -width 16 -anchor w] {left} \
   [label $w.rstat.l4 -text "UV-range: " -anchor w -width 10] {left} \
   [label $w.rstat.uvr  -anchor w -width 16 -anchor w] {left}
  pack append $w.rstat $w.rstat.top {fillx} $w.rstat.bot {fillx} 

# pixel information
  frame $w.pix
  pack append $w.pix \
    [label $w.pix.l1 -text Pixel: -anchor w -width 12] {left} \
    [label $w.pix.pixel -width 16 -anchor w -textvariable pix_pos] {left} \
    [label $w.pix.l2 -text Value: -anchor w -width 10] {left} \
    [label $w.pix.value -width 16 -anchor w -textvariable pix_value] {left} 

# angle and length information
  frame $w.la
  pack append $w.la \
    [label $w.la.l1 -text Length: -anchor w -width 12] {left} \
    [label $w.la.length -width 16 -anchor w -textvariable pix_length] {left} \
    [label $w.la.l2 -text Angle: -anchor w -width 10] {left} \
    [label $w.la.angle -width 16 -anchor w -textvariable pix_angle] {left} 

# integration
  frame $w.int ; frame $w.int.top ; frame $w.int.bot
  pack append $w.int.top \
   [label $w.int.l1 -text "Integrate: " -anchor w -width 12] {left} \
   [label $w.int.value -width 16 -anchor w] {left} \
   [label $w.int.l2 -text Corrected: -anchor w -width 10] {left} \
   [label $w.int.cvalue -width 16 -anchor w] {left}
  pack append $w.int.bot \
   [label $w.int.l3 -text "UV-range: " -anchor w -width 12] {left} \
   [label $w.int.uvr -width 16 -anchor w] {left}
  pack append $w.int $w.int.top {fillx} $w.int.bot {fillx} 

# information
  frame $w.info
  label $w.info.name -width 30 -anchor w
  label $w.info.lname -text "Name: " -width 12 -anchor w
  pack append $w.info $w.info.lname {filly pady 2 left} \
                      $w.info.name {filly pady 2 left} 

# item to display grey-scale
  frame $w.gs -relief sunken -borderwidth 1
  pack append $w.gs \
   [frame $w.gs.opts] {top pady 10 padx 5 fillx} \
   [scale $w.gs.min -label "Image Minimum" -sliderforeground lightblue\
          -length 150 -from 0 -to 100 -orient horizontal] {top padx 5} \
   [scale $w.gs.max -label "Image Maximum" -sliderforeground lightblue\
          -length 150 -from 0 -to 100 -orient horizontal] {top padx 5}
   $w.gs.min set 0
   $w.gs.max set 100
   pack append $w.gs.opts \
   [label $w.gs.l -text "Grey Scale" -width 10 -anchor w] {left} \
   [button $w.gs.update -text "Update" -width 8 -command Aia_display] \
   {right padx 5} \
   [button $w.gs.colour -text "Colour" -width 8 -command Anmap_LTControl]\
   {right padx 5}

# information
  set aia_message "Select option"

# setup window for histogram information
  frame $w.hist -relief sunken -borderwidth 1
  frame $w.hist.top ; frame $w.hist.bot
  pack append $w.hist.top \
   [label $w.hist.l -text Histogram -width 13 -anchor w]\
   {left} \
   [button $w.hist.ok -text OK -width 8 -command Aia_histogram] \
   {right padx 5}
  pack append $w.hist.bot \
   [label $w.hist.minl -text "Min: " -width 7 -anchor w] {left fillx} \
   [entry $w.hist.min -width 9 -relief sunken] {left fillx} \
   [label $w.hist.maxl -text "Min: " -width 7 -anchor w] {left fillx} \
   [entry $w.hist.max -width 9 -relief sunken] {left fillx} 
  pack append $w.hist $w.hist.top {top padx 5 fillx pady 10}\
                      $w.hist.bot {top padx 5 pady 10}

# setup window for integration
  frame $w.integ -relief sunken -borderwidth 1
  frame $w.integ.top ; frame $w.integ.bot
  pack append $w.integ.top \
   [label $w.integ.l -text Integration -width 13 -anchor w]\
   {left} \
   [button $w.integ.ok -text OK -width 8 -command Aia_integration] \
   {right padx 5}
  pack append $w.integ.bot \
   [label $w.integ.l1 -text "Gate: " -width 7 -anchor w] {left fillx} \
   [entry $w.integ.gate -width 9 -relief sunken] {left fillx} 
  pack append $w.integ $w.integ.top {top padx 5 fillx pady 10}\
                       $w.integ.bot {top padx 5 pady 10 frame w}
  $w.integ.gate insert end "0.0"

# setup window for ring integration
  frame $w.rinteg -relief sunken -borderwidth 1
  frame $w.rinteg.top ; frame $w.rinteg.bot
  pack append $w.rinteg.top \
   [label $w.rinteg.l -text "Ring Integration" -width 16 -anchor w]\
   {left} \
   [button $w.rinteg.ok -text OK -width 8 -command Aia_rintegration] \
   {right padx 5}
  pack append $w.rinteg.bot \
   [label $w.rinteg.l1 -text "Bins: " -width 7 -anchor w] {left fillx} \
   [entry $w.rinteg.bins -width 9 -relief sunken] {left fillx} \
   [label $w.rinteg.l2 -text "Width: " -width 7 -anchor w] {left fillx} \
   [entry $w.rinteg.width -width 9 -relief sunken] {left fillx}
  pack append $w.rinteg $w.rinteg.top {top padx 5 fillx pady 10}\
                        $w.rinteg.bot {top padx 5 pady 10}
  $w.rinteg.bins insert end "50"
  $w.rinteg.width insert end "1.0"

#option buttons
  frame $w.opts
  pack append $w.opts \
   [button $w.opts.slice  -text "Slice" -width 8 -relief raised \
                          -command Aia_slice] {left padx 5} \
   [button $w.opts.pixel  -text "Pixel" -width 8 -relief raised \
                          -command Aia_pixel] {left padx 5} \
   [button $w.opts.region -text "Region" -width 8 -relief raised \
                          -command Aia_region] {left padx 5} \
   [button $w.opts.length -text "Length" -width 8 -relief raised \
                          -command Aia_length] {left padx 5} \
   [button $w.opts.angle -text "Angle" -width 8 -relief raised \
                          -command Aia_angle] {left padx 5} 
# command button
  pack append $w.coms \
   [button $w.coms.dismiss -text "Dismiss" -width 8 -relief raised \
                        -command {destroy .analysis} ] {left padx 10} \
   [menubutton $w.coms.image -text "Image..." -width 8 -relief raised \
                        -menu $w.coms.image.menu ] {left padx 10} \
   [button $w.coms.help -text "Help" -width 8 -relief raised \
                        -command Aia_help] {right padx 16} \
   [label $w.coms.m -width 50 -textvariable aia_message] {left fillx} 
   menu $w.coms.image.menu
   $w.coms.image.menu add command -label "Resize displayed region" \
                         -command Aia_resize
   $w.coms.image.menu add command -label "Fullsize displayed region" \
                         -command Aia_fullsize

# construct window
  pack append $w.left $w.ce {pady 4 top fillx} \
              $w.info {top fillx pady 10} \
              $w.istat {top fillx pady 10} \
              $w.rstat {top fillx pady 10} \
              $w.pix {top fillx pady 10} \
              $w.la {top fillx pady 10} \
              $w.int {top fillx pady 10} \
              $w.opts {bottom pady 10 fillx}

  pack append $w.right $w.gs   {top frame n fillx} \
                       $w.hist {top pady 10 fillx} \
                       $w.integ {top pady 10 fillx} \
                       $w.rinteg {top pady 10 fillx}


}

proc Aia_help { } {
  global AnmapSource
  exec \
    /soft/tcl/bin/xhelp $AnmapSource/help/Interactive_Analysis.help \
     -title {Help Interactive Analysis} &
}

proc Aia_display { } {
  global uv_range min max sd source_name
  set w .analysis
  set ce [$w.ce.e get]
  if [string length $ce] then {
    anmap_exec map-display set-map $ce
    set uv_range [image_enq $ce uv-range]
    set source_name [image_enq $ce source]
    anmap_exec map-analysis get $ce statistics $uv_range
    set rmin [$w.gs.min get]
    set rmax [$w.gs.max get]
    set lmin [expr "$min + $rmin*($max-$min)/100.0"]
    set lmax [expr "$min + $rmax*($max-$min)/100.0"]
    anmap_exec map-display grey-scale on $lmin $lmax
    anmap_exec map-display plot grey
    $w.istat.min configure -text $min
    $w.hist.min delete 0 end
    $w.hist.min insert end $min
    $w.istat.max configure -text $max
    $w.hist.max delete 0 end
    $w.hist.max insert end $max
    $w.istat.sd configure -text $sd
    $w.istat.uvr configure -text $uv_range
    $w.info.name configure -text $source_name
  }
}

proc Aia_slice { } {
  global uv_range min max sd source_name u v
  global aia_message
  set w .analysis
  set ce [$w.ce.e get]
  if [string length $ce] then {
    anmap_exec graphic select scratch 9
    anmap_exec graphic opaque
    anmap_exec graphic view-port 0.55,1.0,0.55,1.0
    anmap_exec scratch view-port 0.2,0.8,0.2,0.8
    set aia_message "Select ends of slice" ; update
    anmap_exec map-display get map-read
    set pos1 "$u $v"
    anmap_exec map-display get map-read
    set pos2 "$u $v"   
    anmap_exec map-analysis plot-slice $ce $pos1 $pos2
    set aia_message "Select option" ; update
  }
}


proc Aia_histogram { } {
  global uv_range min max sd source_name uv_pos u v
  global aia_message
  set w .analysis
  set ce [$w.ce.e get]
  if [string length $ce] then {
    anmap_exec map-display get uv-range $ce c
    anmap_exec map-analysis get $ce statistics $uv_range
    anmap_exec graphic select scratch 9
    anmap_exec graphic opaque
    anmap_exec graphic view-port 0.55,1.0,0.55,1.0
    anmap_exec scratch view-port 0.2,0.8,0.2,0.8
    set lmin [$w.hist.min get] ; set lmax [$w.hist.max get]
    set aia_message "Select region for histogram" ; update
    anmap_exec map-display get map-read
    set u1 $u
    set v1 $v
    anmap_exec map-display get map-read
    set u2 $u
    set v2 $v
    anmap_exec map-analysis map-histogram $ce $u1 $u2 $v1 $v2 $lmin $lmax
    set aia_message "Select option" ; update
  }
}

proc Aia_pixel { } {
  global map_value map_pos uv_pos gr_char pix_pos pix_value
  global aia_message aia_pixel_input
  set w .analysis
  set ce [$w.ce.e get]
  if [string length $ce] then {
    set aia_message "Left-button Select;   Right-button Stop" ; update
    set gr_char "A"
    while {$gr_char == "A"} {
       anmap_exec map-display get map-read
       set pix_pos $uv_pos ; set pix_value $map_value
       update
    }
    set aia_message "Select option" ; update
  }
}

proc Aia_region { } {
  global uv_range min max sd source_name uv_pos
  global aia_message
  set w .analysis
  set ce [$w.ce.e get]
  if [string length $ce] then {
    set aia_message "Select region for analysis"
    anmap_exec map-display get uv-range $ce c
    anmap_exec map-analysis get $ce statistics $uv_range
    $w.rstat.min configure -text $min
    $w.rstat.max configure -text $max
    $w.rstat.sd configure -text $sd
    $w.rstat.uvr configure -text $uv_range
    set aia_message "Select option"
  }
}


proc Aia_integration { } {
  global uv_range min max sd source_name uv_pos flux
  global aia_message
  set w .analysis
  set ce [$w.ce.e get]
  if [string length $ce] then {
    set aia_message "Select region for integration"
    anmap_exec map-display get uv-range $ce c
    anmap_exec map-analysis get $ce integrate $uv_range [$w.integ.gate get]
    $w.int.value configure -text $flux
    $w.int.cvalue configure -text $flux
    $w.int.uvr configure -text $uv_range
    set aia_message "Select option"
  }
}


proc Aia_rintegration { } {
  global uv_range min max sd source_name uv_pos u v
  global aia_message
  set w .analysis
  set ce [$w.ce.e get]
  if [string length $ce] then {
    set aia_message "Select centre for integration"
    set uv_range [image_enq $ce uv-range]
    set aia_message "Select centre of ring integration"
    anmap_exec map-display get map-read
    set pos1 "$u $v"
    set aia_message "Working"
    anmap_exec map-analysis ring-flux $ce [$w.rinteg.bins get ] \
               [$w.rinteg.width get] $uv_range $pos1 1.0 0.0
    graphic select graph 9
    graphic view-port 0.55,1.0,0.55,1.0
    data-display line 1 file ~/mrao/anmap_results.dat
    data-display view-port 0.2,0.8,0.2,0.8
    data-display init plot
    data-display x-title Radius (pixels)
    data-display y-title Intensity
    data-display set-text 2 1.5 white 1
    data-display plot all
    set aia_message "Select option"
  }
}


proc Aia_length { } {
  global pix_length
  image_show_marks 0
  set pix_length [image_length]
}

proc Aia_angle { } {
  global pix_angle
  image_show_marks 0
  set pix_angle [image_angle]
}

proc Aia_resize { } {
  global u v
  set w .analysis
  set ce [$w.ce.e get]
  set aia_message "Select region of map to display"
  map-display get map-read ; set u1 $u ; set v1 $v
  map-display get map-read ; set u2 $u ; set v2 $v
  set aia_message "Working"
  eval map-display set-uv $u1 $u2 $v1 $v2
  map-display plot refresh
  set aia_message "Select option"

}

proc Aia_fullsize { } {
  global uv_range
  set w .analysis
  set ce [$w.ce.e get]
  set uv_range [image_enq $ce uv-range]
  map-display set-uv $uv_range
  map-display plot refresh
  set aia_message "Select option"

}





