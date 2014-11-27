#
# Procedures to implement facilities in the synchrotron-analysis
# sub-system amd an X-windows front-end to these facilities.
#

proc fit_map_spectrum { args } {

global pixel gr_char

for {set i 1} {$i < 8} {incr i} {
  set ce($i) "" 
  set frq($i) "" 
  set err($i) ""
  set val($i) ""
}

if [llength $args] then {
  iocmd set-cli $args
}
set nmaps [iocmd geti 'Number of maps : ' '3']
set type  [iocmd geti 'Model type : ' '20']
for {set i 1} {$i <= $nmaps} {incr i} {
    set ce($i)  [iocmd geti 'Catalogue-entry $i : ' '$i']
    set frq($i) [iocmd getr 'Frequency (GHz) $i : ' '$i']
    if {$type > 100} then {
      set err($i) [iocmd getr 'Noise $i : ' '$i']
    }
}

set gr_char A
while {$gr_char == "A"} {
  set uv [image_pixel]
  for {set i 1} {$i <= $nmaps} {incr i} {
    map-analysis get $ce($i) pixel-value [lindex $uv 0] [lindex $uv 1]
    set val($i) $pixel
  }

  if {$type > 100} then {
    synchrotron fit-synchrotron-spectrum $type $nmaps \
       $frq(1) $val(1) $err(1) \
       $frq(2) $val(2) $err(2) \
       $frq(3) $val(3) $err(3) \
       $frq(4) $val(4) $err(4) \
       $frq(5) $val(5) $err(5) \
       $frq(6) $val(6) $err(6) \
       $frq(7) $val(7) $err(7)
   } else {
    synchrotron fit-synchrotron-spectrum $type $nmaps \
       $frq(1) $val(1) \
       $frq(2) $val(2) \
       $frq(3) $val(3) \
       $frq(4) $val(4) \
       $frq(5) $val(5) \
       $frq(6) $val(6) \
       $frq(7) $val(7)
   }

  data-display
    graphic select graph 10
    graphic view-port 0.6,1,0.6,1
    initialise all
    initialise setup
    initialise plot
    view-port 0.1,0.9,0.1,0.9
    line 1 reset-style
    line 1 file ~/mrao/anmap_results.dat
    line 1 x-log-scale on
    line 1 y-log-scale on
    line 1 symbol-type 0
    line 1 line-type 1
    set-frame log-x-labels on
    set-frame log-y-labels on
    line 2 reset-style
    line 2 file ~/mrao/anmap_data.dat
    line 2 x-log-scale on
    line 2 y-log-scale on
    line 2 symbol-type 2
    line 2 symbol-style 1 4 red 1
    line 2 line-type 0
    initialise plot
    plot all

  synchrotron-analysis
}
}

proc synch_plotfit { args } {
  data-display
    graphic select graph 10
    graphic view-port 0.1,0.9,0.1,0.9
    initialise all
    initialise setup
    initialise plot
    view-port 0.1,0.9,0.1,0.9
    line 1 reset-style
    line 1 file ~/mrao/anmap_results.dat
    line 1 x-log-scale on
    line 1 y-log-scale on
    line 1 symbol-type 0
    line 1 line-type 1
    set-frame log-x-labels on
    set-frame log-y-labels on
    line 2 reset-style
    line 2 file ~/mrao/anmap_data.dat
    line 2 x-log-scale on
    line 2 y-log-scale on
    line 2 symbol-type 2
    line 2 symbol-style 1 4 red 1
    line 2 line-type 0
    initialise plot
    plot all
  synchrotron-analysis
}

# X-window interface to synchrotron analysis tools
proc Xsynch { } {
  global Synch Anmap
  set Synch(sptype) 5
  set Synch(gamma) 2.0
  set Synch(type) 20
  set Synch(chi2) 0
  set w .synch
  catch {destroy $w}
  toplevel $w 
  wm title $w "Synchrotron Analysis"
  wm iconname $w "Synchrotron Analysis"
  wm iconbitmap $w @$Anmap(src)/etc/bitmaps/Anmap.xbm

  # setup command bar
  frame $w.bar
  set width 8
  pack [button $w.bar.dismiss -text Dismiss \
        -command "destroy $w" -width $width -borderwidth 1] \
        -side left -anchor w
  pack [button $w.bar.plot -text "Plot" \
        -command "Xsynch_plotsp" -width $width -borderwidth 1] \
        -side left -anchor w
  pack [button $w.bar.fitpt -text "Fit Point" \
        -command "Xsynch_fitpt" -width $width -borderwidth 1] \
        -side left -anchor w
  pack [button $w.bar.fitmaps -text "Fit Maps" \
        -command "Xsynch_fitpt" -width $width -borderwidth 1] \
        -side left -anchor w

# add a help button
  pack [button $w.bar.help -text "Help" \
        -command "Xsynch_help" -width 8 -borderwidth 1] \
        -side right -anchor e

  # setup a list of maps
  frame $w.ml -relief groove -borderwidth 2
    pack [frame $w.ml.list -relief flat] \
         -side top -expand 1 -fill both -pady 2
    pack [listbox $w.ml.list.list  -relief sunken \
         -setgrid 1 -borderwidth 2\
         -font -*-courier-bold-r-*-*-12-*] \
         -side left -expand 1 -fill both -padx 4 -pady 4
    pack [frame $w.ml.entry -relief flat] \
         -side top -fill x -pady 2 -padx 18
    pack [menubutton $w.ml.entry.menu -text "Option..." \
          -menu $w.ml.entry.menu.m ] \
          -side left -anchor w
    menu $w.ml.entry.menu.m
      $w.ml.entry.menu.m add command -label "Add to list" \
                            -command Xsynch_add
      $w.ml.entry.menu.m add command -label "Remove from list" \
                            -command Xsynch_listRemove
      $w.ml.entry.menu.m add command -label "Display Image" \
                            -command Xsynch_display
    pack [label $w.ml.entry.l1 -text "CE: " -width 6 -relief flat] \
          -side left -anchor w
    pack [entry $w.ml.entry.e1 -width 8 -relief sunken] \
          -side left -anchor w
    pack [label $w.ml.entry.l2 -text "Freq: " -width 6 -relief flat] \
          -side left -anchor w
    pack [entry $w.ml.entry.e2 -width 8 -relief sunken] \
          -side left -anchor w
    pack [label $w.ml.entry.l3 -text "Noise: " -width 6 -relief flat] \
          -side left -anchor w
    pack [entry $w.ml.entry.e3 -width 8 -relief sunken] \
          -side left -anchor w
    pack [label $w.ml.entry.l4 -text "Gate: " -width 6 -relief flat] \
          -side left -anchor w
    pack [entry $w.ml.entry.e4 -width 8 -relief sunken] \
          -side left -anchor w

  frame $w.dis -relief groove -borderwidth 2
    pack [frame $w.dis.top -relief flat] -side top -fill x -padx 14
    pack [frame $w.dis.bot -relief flat] -side top -fill x -padx 14
    pack [button $w.dis.top.b -text "Display" -command Xsynch_display \
          -width 8 -borderwidth 2 -relief flat] \
          -side left -anchor w
    pack [entry $w.dis.top.e1 -relief sunken -width 20] \
          -side right -anchor e
    pack [label $w.dis.top.l1 -text "UV-range:" -width 12 \
          -relief flat -anchor w] \
          -side right -anchor e
    pack [label $w.dis.bot.l1 -text "" -width 8 -relief flat] \
          -side left -anchor w
    pack [entry $w.dis.bot.e2 -relief sunken -width 20] \
          -side right -anchor e
    pack [label $w.dis.bot.l2 -text "Data-range:" -width 12 \
          -relief flat -anchor w] \
          -side right -anchor e

  frame $w.type -relief groove -borderwidth 2
    pack [frame $w.type.top -relief flat] -side top -fill x -padx 14
    pack [menubutton $w.type.top.menu -text "Fit type" -width 8\
          -menu $w.type.top.menu.m] \
          -side left -anchor w
    menu $w.type.top.menu.m
    $w.type.top.menu.m add radiobutton -label "11  ... Power Law" \
                       -variable Synch(type) -value 11
    $w.type.top.menu.m add radiobutton -label "12  ... Power Law + Thermal" \
                       -variable Synch(type) -value 12
    $w.type.top.menu.m add radiobutton \
                       -label "13  ... Power Law + Thermal Alpha0 fixed" \
                       -variable Synch(type) -value 13
    $w.type.top.menu.m add radiobutton \
                       -label "14  ... Power Law + Thermal Alpha constrained" \
                       -variable Synch(type) -value 14
    $w.type.top.menu.m add radiobutton -label "20  ... Aged Spectrum" \
                       -variable Synch(type) -value 20
    $w.type.top.menu.m add radiobutton \
                       -label "21  ... Aged Spectrum + Thermal" \
                       -variable Synch(type) -value 21
    $w.type.top.menu.m add radiobutton \
                       -label "22  ... Aged Spectrum + Thermal, fixed x0" \
                       -variable Synch(type) -value 22
    $w.type.top.menu.m add radiobutton \
                       -label "31  ... Aged Spectrum + Thermal, constrained 1" \
                       -variable Synch(type) -value 31
    $w.type.top.menu.m add radiobutton \
                       -label "32  ... Aged Spectrum + Thermal, constrained 2" \
                       -variable Synch(type) -value 32
    $w.type.top.menu.m add radiobutton \
                       -label "33  ... Aged Spectrum + Thermal, constrained 3" \
                       -variable Synch(type) -value 33
    pack [checkbutton $w.type.top.c1 -text "Chi^2 fit" -width 16  \
          -relief flat -variable Synch(chi2)] -side left -anchor w
    pack [entry $w.type.top.e1 -relief sunken -width 20] \
          -side right -anchor e
    pack [label $w.type.top.l1 -text "Constraint: " -width 12 \
          -relief flat -anchor w] \
          -side right -anchor e
    pack [frame $w.type.bot -relief flat] -side top -fill x -padx 14
    pack [button $w.type.bot.b -text "Spectrum" -command Xsynch_selectsp \
          -width 8 -borderwidth 2 -relief flat] \
          -side left -anchor w
    pack [menubutton $w.type.bot.menu -text "Type" -width 16\
          -menu $w.type.bot.menu.m] \
          -side left -anchor w
    menu $w.type.bot.menu.m
    $w.type.bot.menu.m add radiobutton -label "1  ... aged isotropic KP" \
                       -variable Synch(sptype) -value 1
    $w.type.bot.menu.m add radiobutton -label "2  ... aged anisotropic KP" \
                       -variable Synch(sptype) -value 2
    $w.type.bot.menu.m add radiobutton \
                       -label "3  ... continuous injection isotropic KP" \
                       -variable Synch(sptype) -value 3
    $w.type.bot.menu.m add radiobutton \
                       -label "4  ... continuous injection anisotropic KP" \
                       -variable Synch(sptype) -value 4
    $w.type.bot.menu.m add radiobutton \
                       -label "5  ... aged JP" \
                       -variable Synch(sptype) -value 5
    pack [entry $w.type.bot.e1 -relief sunken -width 20] \
          -side right -anchor e
    pack [label $w.type.bot.l1 -text "Gamma: " -width 12 \
          -relief flat -anchor w] \
          -side right -anchor e

  bind $w.ml.list.list <1> "Xsynch_clickon $w %y"
  bind $w.ml.entry.e1 <Return> Xsynch_add
  bind $w.ml.entry.e2 <Return> Xsynch_add
  bind $w.ml.entry.e3 <Return> Xsynch_add
  bind $w.ml.entry.e4 <Return> Xsynch_add
  bind $w.dis.top.e1 <Return> Xsynch_display
  bind $w.dis.bot.e2 <Return> Xsynch_display
  bind $w.type.top.e1 <Return> {}
  bind $w.type.bot.e1 <Return> Xsynch_selectsp

  pack $w.bar -side top -padx 2 -pady 2 -fill x
  pack $w.ml -side top -fill both -expand 1 -padx 2 -pady 2
  pack $w.dis -side top -fill both -expand 1 -padx 2 -pady 2
  pack $w.type -side top -fill both -expand 1 -padx 2 -pady 2
}

proc Xsynch_update { } {
  set w .synch
  set ce [$w.ml.entry.e1 get]
  set uv_range [image_enq $ce uv-range]
  $w.dis.top.e1 delete 0 end ; $w.dis.top.e1 insert end $uv_range
  $w.dis.bot.e2 delete 0 end
}



proc Xsynch_clickon { w pos } {
  set w .synch
  $w.ml.list.list select from [ $w.ml.list.list nearest $pos]
  set ee [set n [$w.ml.list.list curselect] 
                 $w.ml.list.list get $n ]
  set ce [lindex $ee 0]
  set freq [lindex $ee 2]
  set noise [lindex $ee 4]
  set gate [lindex $ee 6]
  $w.ml.entry.e1 delete 0 end
  $w.ml.entry.e1 insert end $ce
  $w.ml.entry.e2 delete 0 end
  $w.ml.entry.e2 insert end $freq
  $w.ml.entry.e3 delete 0 end
  $w.ml.entry.e3 insert end $noise
  $w.ml.entry.e4 delete 0 end
  $w.ml.entry.e4 insert end $gate
  Xsynch_update
}

proc Xsynch_listRemove { } {
  set w .synch
  set wlist $w.ml.list.list
  set e [$w.ml.entry.e1 get]
  if [string length $e] then {
    for {set n 0} {$n < [$wlist size]} {incr n} {
      set entry [$wlist get $n]
      set m [lindex $entry 0]
      if {$e == $m} then {
        $wlist delete $n
      }
    }
  }
}

proc Xsynch_add { } {
  set w .synch
  set added 0
  set e [$w.ml.entry.e1 get]
  set text [format {%-10s  Frequency:  %8s    Noise:  %8s   Gate: %8s} \
      [$w.ml.entry.e1 get] [$w.ml.entry.e2 get] \
      [$w.ml.entry.e3 get] [$w.ml.entry.e4 get]  ]
  set wlist $w.ml.list.list
  set e [$w.ml.entry.e1 get]
  if [string length $e] then {
    for {set n 0} {$n < [$wlist size]} {incr n} {
      set entry [$wlist get $n]
      set m [lindex $entry 0]
      if {$e == $m} then {
        $wlist delete $n
      }
    }
  }
  $wlist insert end $text
  Xsynch_update
}

proc Xsynch_display { } {
  set w .synch
  set uv_range [$w.dis.top.e1 get]
  set range [$w.dis.bot.e2 get]
  map-display 
    set-map [$w.ml.entry.e1 get]
    eval set-uv-range $uv_range
    eval grey-scale on $range,,,,,,
    plot grey
  synchrotron-analysis
}

proc Xsynch_selectsp { } {
  global Synch
  set w .synch
  set Synch(gamma) [$w.type.bot.e1 get]
  if [string length $Synch(gamma)] then {
    synchrotron-analysis define-synchrotron-spectrum \
        $Synch(sptype) $Synch(gamma)
  }
}

proc Xsynch_plotsp { } {
  Xsynch_selectsp
  plot_synchrotron_spectrum 0.1 10,,,,,,
}

proc Xsynch_fitpt { args } {
  global Synch pixel gr_char
  set w .synch
  set wlist $w.ml.list.list
  set nmaps [$wlist size]
  for {set n 0} {$n < 10} {incr n} {
      set ce($n) {}
      set frq($n) {}
      set err($n) {}
      set val($n) {}
  }
  for {set n 0} {$n < $nmaps} {incr n} {
      set entry [$wlist get $n]
      set ce($n) [lindex $entry 0]
      set frq($n) [lindex $entry 2]
      set err($n) [lindex $entry 4]
  }
  set type $Synch(type)
  if {$Synch(chi2)} then {
    set type [expr "10 * $type"]
  }
  set uv [image_pixel]
  for {set i 0} {$i < $nmaps} {incr i} {
    map-analysis get $ce($i) pixel-value [lindex $uv 0] [lindex $uv 1]
    set val($i) $pixel
  }
  puts "Got type = $type"
  if {$type > 100} then {
    eval synchrotron fit-synchrotron-spectrum [Xsynch_getType] $nmaps \
       $frq(0) $val(0) $err(0) \
       $frq(1) $val(1) $err(1) \
       $frq(2) $val(2) $err(2) \
       $frq(3) $val(3) $err(3) \
       $frq(4) $val(4) $err(4) \
       $frq(5) $val(5) $err(5) \
       $frq(6) $val(6) $err(6) \
       $frq(7) $val(7) $err(7)
   } else {
    eval synchrotron fit-synchrotron-spectrum [Xsynch_getType] $nmaps \
       $frq(0) $val(0) \
       $frq(1) $val(1) \
       $frq(2) $val(2) \
       $frq(3) $val(3) \
       $frq(4) $val(4) \
       $frq(5) $val(5) \
       $frq(6) $val(6) \
       $frq(7) $val(7)
   }

  data-display
    graphic select graph 10
    graphic view-port 0.6,1,0.6,1
    initialise all
    initialise setup
    initialise plot
    view-port 0.1,0.9,0.1,0.9
    line 1 reset-style
    line 1 file ~/mrao/anmap_results.dat
    line 1 x-log-scale on
    line 1 y-log-scale on
    line 1 symbol-type 0
    line 1 line-type 1
    set-frame log-x-labels on
    set-frame log-y-labels on
    line 2 reset-style
    line 2 file ~/mrao/anmap_data.dat
    line 2 x-log-scale on
    line 2 y-log-scale on
    line 2 symbol-type 2
    line 2 symbol-style 1 4 red 1
    line 2 line-type 0
    initialise plot
    plot all

  synchrotron-analysis
}

proc Xsynch_fitmaps { args } {
  global Synch
  set w .synch
  set wlist $w.ml.list.list
  set nmaps [$wlist size]
  for {set n 0} {$n < 10} {incr n} {
      set ce($n) {}
      set gate($n) {}
      set err($n) {}
  }
  for {set n 0} {$n < $nmaps} {incr n} {
      set entry [$wlist get $n]
      set ce($n) [lindex $entry 0]
      set err($n) [lindex $entry 4]
      set gate($n) [lindex $entry 6]
  }
  eval synchrotron fit-maps [Xsynch_getType] $nmaps \
       $ce(0) $err(0) gate(0) \
       $ce(1) $err(1) gate(1) \
       $ce(2) $err(2) gate(2) \
       $ce(3) $err(3) gate(3) \
       $ce(4) $err(4) gate(4) \
       $ce(5) $err(5) gate(5) \
       $ce(6) $err(6) gate(6) \
       $ce(7) $err(7) gate(7)
}

proc Xsynch_getType { } {
  global Synch
  set w .synch
  set type $Synch(type)
  switch -regexp -- $type {
    {13|14|22} {set extra [$w.type.top.e1 get]}
    {default}  {set extra {}}
  }

  if {$Synch(chi2)} then {
    set type [expr "10 * $type"]
  }
  return "$type $extra"
}

proc Xsynch_help { } {
  global Anmap Desktop
  exec $Desktop(src)/xhelp \
    -helpfile $Anmap(src)/help/Synchrotron_Analysis.help \
    -title "Help: Synchrotron Analysis" &
}
