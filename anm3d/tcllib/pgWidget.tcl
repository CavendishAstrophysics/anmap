#
# Construct a pgplot colour table widget
#
proc pgct { w } {
  global pgctOpt
  catch {destroy $w}
  toplevel $w
  set title "Lookup Table"
  wm title $w $title ;   wm iconname $w $title

  frame $w.bar -relief flat
  pack [button $w.bar.dismiss -text Dismiss -command "destroy $w" -width 10] \
   -side left -anchor w
  pack [button $w.bar.apply -text Apply -command "pgctForceApply $w" -width 10] \
   -side left -anchor w

  frame $w.sc -relief groove -borderwidth 2
  frame $w.opt -relief groove -borderwidth 2
  pack [scale $w.sc.rs -from 0 -to 100 -label "Red start" \
  -length 250 -orient horizontal -command "pgctApply $w"] \
  -side top -fill x -padx 5 -pady 1
  pack [scale $w.sc.bs -from 0 -to 100 -label "Blue start"\
  -length 250 -orient horizontal -command "pgctApply $w"] \
  -side top -fill x -padx 5 -pady 1
  pack [scale $w.sc.gs -from 0 -to 100 -label "Green start"\
  -length 250 -orient horizontal -command "pgctApply $w"] \
  -side top -fill x -padx 5 -pady 1
  pack [scale $w.sc.rp -from 0 -to 100 -label "Red/Grey power"\
  -length 250 -orient horizontal -command "pgctApply $w"] \
  -side top -fill x -padx 5 -pady 1
  pack [scale $w.sc.bp -from 0 -to 100 -label "Blue power"\
  -length 250 -orient horizontal -command "pgctApply $w"] \
  -side top -fill x -padx 5 -pady 1
  pack [scale $w.sc.gp -from 0 -to 100 -label "Green power"\
  -length 250 -orient horizontal -command "pgctApply $w"] \
  -side top -fill x -padx 5 -pady 1
  pack [scale $w.sc.low -from 0 -to 100 -label "Low"\
  -length 250 -orient horizontal -command "pgctApply $w"] \
  -side top -fill x -padx 5 -pady 1
  pack [scale $w.sc.high -from 0 -to 100 -label "High"\
  -length 250 -orient horizontal -command "pgctApply $w"] \
  -side top -fill x -padx 5 -pady 1


  pack [radiobutton $w.opt.op0 -text "Enhanced colour" -variable pgctOpt(type) -value 0 \
   -anchor w -command "pgctApply $w"] -side top -fill x -padx 5 -pady 3
  pack [radiobutton $w.opt.op1 -text "Standard colour" -variable pgctOpt(type) -value 1 \
    -anchor w -command "pgctApply $w"] -side top -fill x -padx 5 -pady 3
  pack [radiobutton $w.opt.op2 -text "Rainbow colour" -variable pgctOpt(type) -value 2 \
    -anchor w -command "pgctApply $w"] -side top -fill x -padx 5 -pady 3
  pack [radiobutton $w.opt.op3 -text "Enhanced greyscale" -variable pgctOpt(type) -value 3 \
    -anchor w -command "pgctApply $w"] -side top -fill x -padx 5 -pady 3
  pack [radiobutton $w.opt.op4 -text "Linear greyscale" -variable pgctOpt(type) -value 4 \
    -anchor w -command "pgctApply $w"] -side top -fill x -padx 5 -pady 3
  pack [radiobutton $w.opt.op5 -text "Inverted enhanced" -variable pgctOpt(type) -value 5 \
    -anchor w -command "pgctApply $w"] -side top -fill x -padx 5 -pady 3
  pack [radiobutton $w.opt.op6 -text "Inverted linear" -variable pgctOpt(type) -value 6 \
    -anchor w -command "pgctApply $w"] -side top -fill x -padx 5 -pady 3
  pack [radiobutton $w.opt.im0 -text "Deferred Update" -variable pgctOpt(mode) -value 0 \
    -anchor w -command "pgctApply $w"] -side top -fill x -padx 5 -pady 3
  pack [radiobutton $w.opt.im1 -text "Immediate Update" -variable pgctOpt(mode) -value 1 \
    -anchor w -command "pgctApply $w"] -side top -fill x -padx 5 -pady 3

  $w.sc.rs set 0  ; $w.sc.gs set 40 ; $w.sc.bs set 60
  $w.sc.rp set 25 ; $w.sc.gp set 25 ; $w.sc.bp set 25
  $w.sc.low set 0 ; $w.sc.high set 100
  set pgctOpt(mode) 1
  set pgctOpt(type) 0
  pack $w.bar -side top -fill x -padx 4 -pady 4
  pack $w.sc -side left -fill y -padx 4 -pady 4
  pack $w.opt -side left -padx 4 -pady 4 -fill y
  pgctApply $w
}

proc pgctForceApply { w args } {
   global pgctOpt
   set mode $pgctOpt(mode)
   set pgctOpt(mode) 1
   pgctApply $w
   set pgctOpt(mode) $mode
}

proc pgctApply { w args } {
   global pgctOpt
   if !$pgctOpt(mode) return {}
   set rs [expr "[$w.sc.rs get]/100.0"] ; set rp [expr "(([$w.sc.rp get])/25.0)"];  
   set bs [expr "[$w.sc.bs get]/100.0"] ; set bp [expr "(([$w.sc.bp get])/25.0)"];  
   set gs [expr "[$w.sc.gs get]/100.0"] ; set gp [expr "(([$w.sc.gp get])/25.0)"];  
   set low [expr "[$w.sc.low get]/100.0"]
   set high [expr "[$w.sc.high get]/100.0"]
   switch $pgctOpt(type) {
      0  {pg ct -ecolour -rpower $rp -gpower $gp -bpower $bp \
                -rstart $rs -gstart $gs -bstart $bs -lower $low -upper $high}
      1  {pg ct -colour }
      2  {pg ct -standard -rindex $rp -gindex $gp -bindex $bp} 
      3  {pg ct -egrey -power $rp -lower $low -upper $high}
      4  {pg ct -grey}
      5  {pg ct -einvgrey -power $rp -lower $low -upper $high}
      6  {pg ct -invgrey}
   }
}
