proc vis_editImage { } {
   global Imid Imid2 Imdisp Imdef Imdef1 Imdef2 Vis3d Andef
   global edim
   set w .edim
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Edit Image(s)"
   wm iconname $w "Edit Image(s)"
   pack [frame $w.bar -relief flat] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.bar.dismiss -command "destroy $w" -text Dismiss \
           -width 10 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 2 -pady 2
   pack [frame $w.top -relief groove -borderwidth 2] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.top.load1 -text "Load Im1" -width 10 \
	   -relief raised -borderwidth 2\
	   -command vis_load] -side left -padx 2 -pady 2
     pack [button $w.top.load2 -text "Load Im2" -width 10 \
	   -relief raised -borderwidth 2\
	   -command vis_load2] -side left -padx 2 -pady 2
     pack [button $w.top.switch -text "Switch Im1/2" -width 10 \
	   -relief raised -borderwidth 2\
	   -command edim_switch] -side left -padx 2 -pady 2
   pack [frame $w.combine -relief groove -borderwidth 2] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.combine.b0 -text "Im1 = Im2" -width 10 \
	   -relief raised -borderwidth 2\
	   -command "edim_combine ="] -side left -padx 2 -pady 2
     pack [button $w.combine.b1 -text "Im1 += Im2" -width 10 \
	   -relief raised -borderwidth 2\
	   -command "edim_combine +="] -side left -padx 2 -pady 2
     pack [button $w.combine.b2 -text "Im1 -= Im2" -width 10 \
	   -relief raised -borderwidth 2\
	   -command "edim_combine -="] -side left -padx 2 -pady 2
     pack [button $w.combine.b3 -text "Im1 *= Im2" -width 10 \
	   -relief raised -borderwidth 2\
	   -command "edim_combine *="] -side left -padx 2 -pady 2
     pack [button $w.combine.b4 -text "Im1 /= Im2" -width 10 \
	   -relief raised -borderwidth 2\
	   -command "edim_combine /="] -side left -padx 2 -pady 2
   pack [frame $w.op1 -relief groove -borderwidth 2] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.op1.b1 -text "log(Im1)" -width 10 \
	   -relief raised -borderwidth 2\
	   -command "edim_op1 log"] -side left -padx 2 -pady 2
     pack [button $w.op1.b2 -text "exp(Im1)" -width 10 \
	   -relief raised -borderwidth 2\
	   -command "edim_op1 exp"] -side left -padx 2 -pady 2
     pack [button $w.op1.b3 -text "1.0/Im1" -width 10 \
	   -relief raised -borderwidth 2\
	   -command "edim_op1 inv"] -side left -padx 2 -pady 2
   pack [frame $w.op2 -relief groove -borderwidth 2] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.op2.b2 -text "binary(Im1)" -width 10 \
	   -relief raised -borderwidth 2\
	   -command {edim_op1 binary $edim(binary)}] \
	   -side left -padx 2 -pady 2
     pack [label $w.op2.l1 -text "level:" -anchor e -width 8 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [entry $w.op2.e1 -textvariable edim(binary) -width 10 \
	   -relief sunken -borderwidth 2] -side left -padx 2 -pady 2
     pack [label $w.op2.lp1 -text {} -width 4 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [button $w.op2.b3 -text "Offset(Im1)" -width 10 \
	   -relief raised -borderwidth 2\
	   -command {edim_op1 offset $edim(offset)}] \
	   -side left -padx 2 -pady 2
     pack [label $w.op2.l2 -text "offset:" -anchor e -width 8 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [entry $w.op2.e2 -textvariable edim(offset) -width 10 \
	   -relief sunken -borderwidth 2] -side left -padx 2 -pady 2
   pack [frame $w.op3 -relief groove -borderwidth 2] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.op3.b2 -text "Scale(Im1)" -width 10 \
	   -relief raised -borderwidth 2\
	   -command {edim_op1 scale $edim(scale)}] \
	   -side left -padx 2 -pady 2
     pack [label $w.op3.l1 -text "scale:" -anchor e -width 8 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [entry $w.op3.e1 -textvariable edim(scale) -width 10 \
	   -relief sunken -borderwidth 2] -side left -padx 2 -pady 2
   pack [frame $w.op4 -relief groove -borderwidth 2] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.op4.b2 -text "Pass(Im1)" -width 10 \
	   -relief raised -borderwidth 2\
	   -command {edim_op1 pass $edim(pass1) $edim(pass2) $edim(pass3)}] \
	   -side left -padx 2 -pady 2
     pack [label $w.op4.l1 -text "from:" -anchor e -width 8 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [entry $w.op4.e1 -textvariable edim(pass1) -width 10 \
	   -relief sunken -borderwidth 2] -side left -padx 2 -pady 2
     pack [label $w.op4.l2 -text "to:" -anchor e -width 8 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [entry $w.op4.e2 -textvariable edim(pass2) -width 10 \
	   -relief sunken -borderwidth 2] -side left -padx 2 -pady 2
     pack [label $w.op4.l3 -text "replace:" -anchor e -width 8 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [entry $w.op4.e3 -textvariable edim(pass3) -width 10 \
	   -relief sunken -borderwidth 2] -side left -padx 2 -pady 2
   pack [frame $w.op5 -relief groove -borderwidth 2] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.op5.b2 -text "Range(Im1)" -width 10 \
	   -relief raised -borderwidth 2\
	   -command {edim_op1 range $edim(range1) \
		     $edim(range2) $edim(range3) $edim(range4)} ] \
	   -side left -padx 2 -pady 2
     pack [label $w.op5.l1 -text "from:" -anchor e -width 8 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [entry $w.op5.e1 -textvariable edim(range1) -width 10 \
	   -relief sunken -borderwidth 2] -side left -padx 2 -pady 2
     pack [label $w.op5.l2 -text "to:" -anchor e -width 8 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [entry $w.op5.e2 -textvariable edim(range2) -width 10 \
	   -relief sunken -borderwidth 2] -side left -padx 2 -pady 2
     pack [label $w.op5.l3 -text "operator:" -anchor e -width 8 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [entry $w.op5.e3 -textvariable edim(range3) -width 10 \
	   -relief sunken -borderwidth 2] -side left -padx 2 -pady 2
     pack [label $w.op5.l4 -text "value:" -anchor e -width 8 -relief flat] \
	   -side left -padx 2 -pady 2
     pack [entry $w.op5.e5 -textvariable edim(range4) -width 10 \
	   -relief sunken -borderwidth 2] -side left -padx 2 -pady 2

}

proc edim_switch { } {
   global Imid Imid2 Imdisp Imdef Imdef1 Imdef2 Vis3d Andef
   set im $Imid ; set im2 $Imid2
   set f $Vis3d(file) ; set f2 $Vis3d(file2)
   vis_imgReset2 $im $f
   vis_imgReset $im2 $f2
}

proc edim_combine { op } {
   global Imid Imid2 Imdisp Imdef Imdef1 Imdef2 Vis3d Andef
   img_apply $Imid $op $Imid2
}

proc edim_op1 { op args} {
   global Imid Imid2 Imdisp Imdef Imdef1 Imdef2 Vis3d Andef
   eval img_apply $Imid $op $args
}

