#
# Procedures to implement statistics of region analysis in vis3d
#
proc reg_makeClass { } {
   global Vis3d

   set w .regcl ; set Vis3d(save_lists) 0
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Classify Identified Image"
   wm iconname $w "Classify Image"
   pack [frame $w.bar -relief flat] -side top -fill x
     pack [button $w.bar.dismiss -text Dismiss -command "destroy $w" \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.constr -text Construct -command reg_classConstruct \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.destroy -text Delete -command reg_classDelete \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2
   pack [frame $w.info -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [message $w.info.m -relief flat \
          -text "Before statistical analysis and display of a identified volume\
		can be done the volume must be classified to identify the\
		individual regions and make lookups on these regions efficient.\
		Click on Constuct to make the classified volume if you have not\
		already done so."\
	   -aspect 500] \
           -side top -fill x -padx 4 -pady 4
   pack [frame $w.lst -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [checkbutton $w.lst.cb -relief flat -text "Save surface lists" \
           -width 20 -anchor w -variable Vis3d(save_lists)] \
           -side left -anchor w -padx 4 -pady 4
   pack [frame $w.x1 -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.x1.l -relief flat -text "Minimum to classify: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.x1.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(regan:x1) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.x1.e <Return> { }
   pack [frame $w.x2 -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.x2.l -relief flat -text "Maximum to classify: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.x2.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(regan:x2) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.x2.e <Return> { }
   pack [frame $w.xi -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.xi.l -relief flat -text "Increment to classify: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.xi.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(regan:xi) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.xi.e <Return> { }
}

proc reg_classConstruct { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   wm_cursor . -watch
   if { $Vis3d(save_lists) } then {
      img_region classify $Imid $Vis3d(regan:x1) \
	   $Vis3d(regan:x2) $Vis3d(regan:xi)
   } else {
      img_region classify $Imid $Vis3d(regan:x1) \
	   $Vis3d(regan:x2) $Vis3d(regan:xi) -nolist
   }
   wm_cursor . -restore
}

proc reg_classDelete { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   img_region clear-classification $Imid
}

proc reg_analRegs { } {
   global Vis3d

   set w .reganal
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Analyse Classified Image"
   wm iconname $w "Analyse Classified Image"
   pack [frame $w.bar -relief flat] -side top -fill x
     pack [button $w.bar.dismiss -text Dismiss -command "destroy $w" \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.savecl -text Save-Cl -command reg_saveClassify \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2
   pack [frame $w.i1 -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [message $w.i1.m -relief flat \
          -text "Firstly analyse the image and then select a display option."\
	   -aspect 2000] \
           -side top -fill x -padx 4 -pady 4

     pack [frame $w.i1.f -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [radiobutton  $w.i1.f.i -text "Analyse current image" -width 20\
	   -variable Vis3d(regan:doinfile) -value 0 -relief flat] \
           -side left -anchor w -padx 4 -pady 4
     pack [radiobutton  $w.i1.f.f -text "Read saved data file" -width 20\
	   -variable Vis3d(regan:doinfile) -value 1 -relief flat] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i1.f.ef -relief sunken -borderwidth 2 \
           -width 15 -textvariable Vis3d(regan:infile) ] \
           -side left -anchor w -padx 4 -pady 4

     pack [frame $w.i1.x -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.i1.x.l1 -relief flat -text "Classified region" \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [label $w.i1.x.lx1 -relief flat -text "Start: " \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i1.x.ex1 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:i1) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.i1.x.ex1 <Return> { }
     pack [label $w.i1.x.lx2 -relief flat -text "End: " \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i1.x.ex2 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:i2) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.i1.x.ex2 <Return> { }
     pack [frame $w.i1.ee -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [checkbutton $w.i1.ee.c1 -text "Select region" \
	 -variable Vis3d(regan:excl) -width 12]\
         -side left -anchor w -padx 10 -pady 4
     pack [radiobutton $w.i1.ee.rx -text "From-X" \
	 -variable Vis3d(regan:axis) -value x -width 8]\
         -side left -anchor w -padx 10 -pady 4
     pack [radiobutton $w.i1.ee.ry -text "From-Y" \
	 -variable Vis3d(regan:axis) -value y -width 8]\
         -side left -anchor w -padx 10 -pady 4
     pack [radiobutton $w.i1.ee.rz -text "From-Z" \
	 -variable Vis3d(regan:axis) -value z -width 8]\
         -side left -anchor w -padx 10 -pady 4

     pack [frame $w.i1.ee1 -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.i1.ee1.l1 -relief flat -text "Annular selection" \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [label $w.i1.ee1.lx1 -relief flat -text "Inner: " \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i1.ee1.ex1 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:r1) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.i1.ee1.ex1 <Return> { }
     pack [label $w.i1.ee1.lx2 -relief flat -text "Outer: " \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i1.ee1.ex2 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:r2) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.i1.ee1.ex2 <Return> { }

     pack [frame $w.i1.ee2 -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.i1.ee2.l1 -relief flat -text "Annular Centre" \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [label $w.i1.ee2.lxx -relief flat -text "Xc: " \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i1.ee2.exx -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:xc) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.i1.ee2.exx <Return> { }
     pack [label $w.i1.ee2.lxy -relief flat -text "Yc: " \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i1.ee2.exy -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:yc) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.i1.ee2.exy <Return> { }
     pack [label $w.i1.ee2.lxz -relief flat -text "Zc: " \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i1.ee2.exz -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:zc) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.i1.ee2.exz <Return> { }

     pack [frame $w.i1.ee3 -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.i1.ee3.l1 -relief flat -text "Slice" \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [label $w.i1.ee3.lxx -relief flat -text "From: " \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i1.ee3.exx -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:slmin) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.i1.ee3.exx <Return> { }
     pack [label $w.i1.ee3.lxy -relief flat -text "To: " \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i1.ee3.exy -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:slmax) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.i1.ee3.exy <Return> { }

     pack [frame $w.i1.bb -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [button $w.i1.bb.b1 -relief raised -borderwidth 2 \
         -text "Analyse" -command reg_doAnal -width 12] \
         -side left -anchor w -padx 10 -pady 4

   pack [frame $w.hist -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.hist.l -relief flat \
           -borderwidth 2 -text "Output of Results"] \
           -side top -fill x -padx 4 -pady 4
     pack [frame $w.hist.nh -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.hist.nh.l1 -relief flat -text "Number of bins" \
           -width 15 -anchor w] \
           -side left -anchor w -padx 4
     pack [label $w.hist.nh.l2 -relief flat -text "Number:" \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4
     pack [entry $w.hist.nh.e -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:nh) ] \
           -side left -anchor w -padx 4
     pack [checkbutton $w.hist.nh.cbs -relief flat -borderwidth 2 \
           -width 10 -variable Vis3d(regan:setbinsize) -text "Set bin size:"\
	   -anchor w] \
           -side left -anchor w -padx 4
     pack [entry $w.hist.nh.sbse -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:binsize) ] \
           -side left -anchor w -padx 4
     pack [checkbutton $w.hist.nh.cb1 -relief flat -borderwidth 2 \
           -width 15 -variable Vis3d(regan:vv) -text "Volume weighted"] \
           -side left -anchor w -padx 4
     bind $w.hist.nh.e <Return> { }

     set Vis3d(regan:rgx1) {}
     set Vis3d(regan:rgx2) {}
     set Vis3d(regan:rgy1) {}
     set Vis3d(regan:rgy2) {}
     pack [frame $w.hist.rg1 -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.hist.rg1.l1 -relief flat -text "Range to plot" \
           -width 15 -anchor w] \
           -side left -anchor w -padx 4
     pack [label $w.hist.rg1.l2 -relief flat -text "X-range:" \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4
     pack [entry $w.hist.rg1.ex1 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:rgx1) ] \
           -side left -anchor w -padx 4
     pack [entry $w.hist.rg1.ex2 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:rgx2) ] \
           -side left -anchor w -padx 4
     pack [button $w.hist.rg1.bb -relief raised -borderwidth 2 \
           -width 8 -text Reset -command {
		set  Vis3d(regan:rgx1) {};
		set  Vis3d(regan:rgx2) {}; } ] \
           -side left -anchor w -padx 4

     pack [frame $w.hist.rg -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.hist.rg.l1 -relief flat -text { } \
           -width 15 -anchor w] \
           -side left -anchor w -padx 4
     pack [label $w.hist.rg.l3 -relief flat -text "Y-range:" \
           -width 8 -anchor e] \
           -side left -anchor w -padx 4
     pack [entry $w.hist.rg.ey1 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:rgy1) ] \
           -side left -anchor w -padx 4
     pack [entry $w.hist.rg.ey2 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:rgy2) ] \
           -side left -anchor w -padx 4
     pack [button $w.hist.rg.bb -relief raised -borderwidth 2 \
           -width 8 -text Reset -command {
		set  Vis3d(regan:rgy1) {};
		set  Vis3d(regan:rgy2) {}; } ] \
           -side left -anchor w -padx 4

     pack [frame $w.hist.bb -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 2
     pack [button $w.hist.bb.b1 -relief raised -borderwidth 2 \
         -text "Area" -width 12 \
	 -command {reg_doAreaHist}] \
         -side left -anchor w -padx 10 -pady 4
     pack [button $w.hist.bb.b2 -relief raised -borderwidth 2 \
         -text "Volume" -width 12 \
	 -command {reg_doVolumeHist}] \
         -side left -anchor w -padx 10 -pady 4
     pack [button $w.hist.bb.b3 -relief raised -borderwidth 2 \
         -text "Pore/Throat" -width 12 \
	 -command {reg_doHist PrThr minP maxP {Throat/Pore} \
		  {Number of Regions} 0}] \
         -side left -anchor w -padx 10 -pady 4
     pack [button $w.hist.bb.b4 -relief raised -borderwidth 2 \
         -text "Coordination" -command reg_doHistCoord -width 12] \
         -side left -anchor w -padx 10 -pady 4
     pack [frame $w.hist.bc -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 2
     pack [button $w.hist.bc.b1 -relief raised -borderwidth 2 \
         -text "Radius" -width 12 \
	 -command {reg_doRadiusHist}] \
         -side left -anchor w -padx 10 -pady 4
     pack [button $w.hist.bc.b1a -relief raised -borderwidth 2 \
         -text "Constr." -width 12 \
	 -command {reg_doConstrHist}] \
         -side left -anchor w -padx 10 -pady 4
     pack [button $w.hist.bc.b2 -relief raised -borderwidth 2 \
         -text "Fractal" -width 12 \
	 -command {reg_doFrac Area "log( Area )"}] \
         -side left -anchor w -padx 10 -pady 4
     pack [button $w.hist.bc.b3 -relief raised -borderwidth 2 \
         -text "Fractal/Free" -width 12 \
	 -command {reg_doFrac AreaF "log( Free Area)"}] \
         -side left -anchor w -padx 10 -pady 4

   set Vis3d(regan:2drnbin) 20
   set Vis3d(regan:2drstart) 0.0
   set Vis3d(regan:2drbin) 0.0
   set Vis3d(regan:2dcnbin) 20
   set Vis3d(regan:2dcstart) 0
   set Vis3d(regan:2dcbin) 1
   set Vis3d(regan:2dfile) hist2d.dat
   pack [frame $w.twod -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.twod.l -relief flat \
           -borderwidth 2 -text "Write 2D histograms"] \
           -side top -fill x -padx 4 -pady 4
     pack [frame $w.twod.l1 -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.twod.l1.l -relief flat -text "Radius:" \
           -width 15 -anchor w] \
           -side left -anchor w -padx 4
     pack [label $w.twod.l1.l1 -relief flat -text "Number bins:" \
           -width 12 -anchor e] \
           -side left -anchor w -padx 4
     pack [entry $w.twod.l1.e1 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:2drnbin) ] \
           -side left -anchor w -padx 4
     bind $w.twod.l1.e1 <Return> { }
     pack [label $w.twod.l1.l2 -relief flat -text "Start:" \
           -width 10 -anchor e] \
           -side left -anchor w -padx 4
     pack [entry $w.twod.l1.e2 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:2drstart) ] \
           -side left -anchor w -padx 4
     bind $w.twod.l1.e2 <Return> { }
     pack [label $w.twod.l1.l3 -relief flat -text "Bin size:" \
           -width 10 -anchor e] \
           -side left -anchor w -padx 4
     pack [entry $w.twod.l1.e3 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:2drbin) ] \
           -side left -anchor w -padx 4
     bind $w.twod.l1.e3 <Return> { }
     if { 0  } {
     pack [frame $w.twod.l2 -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.twod.l2.l -relief flat -text "Coordination:" \
           -width 15 -anchor w] \
           -side left -anchor w -padx 4
     pack [label $w.twod.l2.l1 -relief flat -text "Number bins:" \
           -width 12 -anchor e] \
           -side left -anchor w -padx 4
     pack [entry $w.twod.l2.e1 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:2dcnbin) ] \
           -side left -anchor w -padx 4
     bind $w.twod.l2.e1 <Return> { }
     pack [label $w.twod.l2.l2 -relief flat -text "Start:" \
           -width 10 -anchor e] \
           -side left -anchor w -padx 4
     pack [entry $w.twod.l2.e2 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:2dcstart) ] \
           -side left -anchor w -padx 4
     bind $w.twod.l2.e2 <Return> { }
     pack [label $w.twod.l2.l3 -relief flat -text "Bin size:" \
           -width 10 -anchor e] \
           -side left -anchor w -padx 4
     pack [entry $w.twod.l2.e3 -relief sunken -borderwidth 2 \
           -width 8 -textvariable Vis3d(regan:2dcbin) ] \
           -side left -anchor w -padx 4
     bind $w.twod.l2.e3 <Return> { }
     }
     pack [frame $w.twod.l3 -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.twod.l3.l -relief flat -text "Save As:" \
           -width 15 -anchor w] \
           -side left -anchor w -padx 4
     pack [entry $w.twod.l3.e1 -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(regan:2dfile) ] \
           -side left -anchor w -padx 4
     bind $w.twod.l3.e1 <Return> { }
     pack [button $w.twod.l3.b1 -text "Make/Save" -width 8 \
	   -borderwidth 2 -relief raised -command reg_doTwoDHist] \
           -side left -anchor w -padx 20


   pack [frame $w.out -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.out.l -relief flat \
           -borderwidth 2 -text "Graphics Output File"] \
           -side top -fill x -padx 4 -pady 4
     pack [frame $w.out.bb -relief flat -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [radiobutton $w.out.bb.b1 -relief flat -borderwidth 2 \
         -text "Screen" -width 12 -variable Vis3d(regan:out) -value 1] \
         -side left -anchor w -padx 10 -pady 4
     pack [radiobutton $w.out.bb.b2 -relief flat -borderwidth 2 \
         -text "PS File" -width 12 -variable Vis3d(regan:out) -value 0] \
         -side left -anchor w -padx 10 -pady 4
     pack [radiobutton $w.out.bb.b3 -relief flat -borderwidth 2 \
         -text "Save As" -width 12 -variable Vis3d(regan:out) -value 2] \
         -side left -anchor w -padx 10 -pady 4
     pack [entry $w.out.bb.file -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(regan:file) ] \
           -side left -anchor w -padx 8 -pady 4
     bind $w.out.bb.file <Return> { }

}
proc reg_doAreaHist { } {
  global Vis3d
  if { $Vis3d(regan:vv) } then {
      reg_doHist Area minA maxA {Area} {Volume of Regions} 1
  } else {
      reg_doHist Area minA maxA {Area} {Number of Regions} 0
  }
}

proc reg_doConstrHist { } {
  global Vis3d
  reg_doHist Constr minCs maxCs {Constriction} {Number of Constrictions} 0
}


proc reg_doVolumeHist { } {
  global Vis3d
  if { $Vis3d(regan:vv) } then {
     reg_doHist Volume minV maxV {Volume} {Volume of Regions} 1
  } else {
     reg_doHist Volume minV maxV {Volume} {Number of Regions} 0
  }
}

proc reg_doRadiusHist { } {
  global Vis3d
  if { $Vis3d(regan:vv) } then {
     reg_doHist Rad minR maxR {Radius} {Volume of Regions} 1
  } else {
     reg_doHist Rad minR maxR {Radius} {Number of Regions} 0
  }
}

proc reg_saveClassify { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Volume Area AreaF Coord Rad PrThr Constr\
	  minA maxA minV maxV minC maxC minP maxP minR maxR minCs maxCs

   set nc 0
   set fid [open classify.out w]
   for {set i $Vis3d(regan:i1)} {$i <= $Vis3d(regan:i2)} {incr i} {
	set res [img_region enqshape $Imid $i] ;
	if { [lindex $res 0] > 0 } then {incr nc}
   }

   puts $fid $nc
   for {set i $Vis3d(regan:i1)} {$i <= $Vis3d(regan:i2)} {incr i} {
	set res [img_region enqshape $Imid $i]
	if { [lindex $res 0] > 0 } then {
	    set r($i) $res
	    puts $fid "$i [join [lrange $res 0 2]] [expr [llength $res] - 3]"
	    for {set n 3} {$n < [llength $res]} {incr n} {
		puts $fid "        [lindex $res $n]"
		set nn [lindex [lindex $res $n] 0]
		if { $nn > 0 } {lappend jl($i) $nn}
	    }
	}
   }

   set ng 0
   for {set i $Vis3d(regan:i1)} {$i <= $Vis3d(regan:i2)} {incr i} {
	if {[info exists r($i)]} then {
	   incr ng ; lappend jjl($ng) $i ; set ll {}
	   if { [info exists jl($i)] } {set ll $jl($i)}
	   unset r($i) ; catch {unset jl($i)}
	   while { [llength $ll] > 0 } {
		set lll {}
		foreach l $ll {
		   if { [info exists jl($l)] } {set lll [concat $lll $jl($l)]}
		   if { [info exists r($l)] } then {
			lappend jjl($ng) $l ; unset r($l) ; catch {unset jl($l)}
		   }
		}
		set ll $lll
	   }
	   set vt 0 ; set at 0 ; set aw 0 ; set as 0
	   for {set n 0} {$n < [llength $jjl($ng)]} {incr n} {
		set res [img_region enqshape $Imid [lindex $jjl($ng) $n]]
		set vt [expr $vt + [lindex $res 0]]
		set at [expr $at + [lindex $res 1]]
		for {set m 3} {$m < [llength $res]} {incr m} {
		   set nn [lindex [lindex $res $m] 0]
		   if { $nn == 0 } then {
			set as [expr $as + [lindex [lindex $res $m] 1]]
		   } elseif { $nn == -1 } then {
			set aw [expr $aw + [lindex [lindex $res $m] 1]]
		   }
		}
	   }
	   set jjr($ng) "$vt $at $as $aw"
	}
   }
   puts $fid $ng
   for {set n 1} {$n <= $ng} {incr n} {
	puts $fid "$n $jjr($n) [llength $jjl($n)]"
	puts $fid $jjl($n)
   }
   close $fid
}

proc reg_doAnal { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Volume Area AreaF Coord Rad PrThr Constr\
	  minA maxA minV maxV minC maxC minP maxP minR maxR minCs maxCs
   catch {unset  Volume}
   catch {unset  Area}
   catch {unset  AreaF}
   catch {unset  Coord}
   catch {unset  Rad}
   catch {unset  PrThr}
   catch {unset  Constr}
   set minV 10000000 ; set maxV 0
   set minA 10000000 ; set maxA 0
   set minR 1000 ; set maxR 0
   set minC 1000 ; set maxC 0
   set minCs 10000000 ; set maxCs 0
   set maxP 1.0  ; set minP 0.0
   wm_cursor . -watch
   set RR1 [expr pow( $Vis3d(regan:r1), 2 )]
   set RR2 [expr pow( $Vis3d(regan:r2), 2 )]
   set ii 0
   if { $Vis3d(regan:doinfile) } then { source $Vis3d(regan:infile) }
   for {set i $Vis3d(regan:i1)} {$i <= $Vis3d(regan:i2)} {incr i} {
	if { $Vis3d(regan:doinfile) } then {
	   if [info exists resin($i)] then {
	      set res $resin($i)
	   } else {
	      set res {}
	   }
	} else {
	   set res [img_region enqshape $Imid $i]
	}
	if { [lindex $res 0] > 0 } then {
	if { $Vis3d(regan:excl) } then {
	    set x [lindex [lindex $res 2] 0]
	    set y [lindex [lindex $res 2] 1]
	    set z [lindex [lindex $res 2] 2]
	    switch $Vis3d(regan:axis) {
		{x}	{set rr [expr pow(($y-$Vis3d(regan:yc)),2) + \
				     pow(($z-$Vis3d(regan:zc)),2) ]
			 set dd $x}
		{y}	{set rr [expr pow(($x-$Vis3d(regan:xc)),2) + \
				     pow(($z-$Vis3d(regan:zc)),2) ]
			 set dd $y}
		{z}	{set rr [expr pow(($y-$Vis3d(regan:yc)),2) + \
				     pow(($x-$Vis3d(regan:xc)),2) ]
			 set dd $z}
	    }
	    set doinc [expr ($rr >= $RR1) && ($rr <= $RR2) \
			&& ($dd >= $Vis3d(regan:slmin)) \
			&& ($dd <= $Vis3d(regan:slmax)) ]
	} else {
	    set doinc 1
	}
	if { $doinc } then {
	set Volume($i) [lindex $res 0]
	set Area($i) [lindex $res 1]
	set Rad($i) [lindex [lindex $res 2] 3]
	set Coord($i) [expr [llength $res] - 4]
	set thr 0.0
	for {set n 3} {$n < [llength $res]} {incr n} {
	    set xx [lindex $res $n]
	    if {[lindex $xx 0] != 0} then {
		incr ii ; set Constr($ii) [lindex $xx 1]
		set thr [expr $thr + [lindex $xx 1]]
		if { $Constr($ii) < $minCs } {set minCs $Constr($ii)}
		if { $Constr($ii) > $maxCs } {set maxCs $Constr($ii)}
	    } else {
		set AreaF($i) [lindex $xx 1]
	    }
	}
	if { $Volume($i) < $minV } {set minV $Volume($i)}
	if { $Volume($i) > $maxV } {set maxV $Volume($i)}
	if { $Area($i) < $minA } {set minA $Area($i)}
	if { $Area($i) > $maxA } {set maxA $Area($i)}
	if { $Coord($i) < $minC } {set minC $Coord($i)}
	if { $Coord($i) > $maxC } {set maxC $Coord($i)}
	if { $Rad($i) < $minR } {set minR $Rad($i)}
	if { $Rad($i) > $maxR } {set maxR $Rad($i)}
	set PrThr($i) [expr $thr / $Area($i) ]
	}
	}
	update idletasks
   }
   wm_cursor . -restore
}

proc reg_doHist { val min max xlab ylab wt} {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Volume Area AreaF Coord Rad PrThr Constr\
	  minA maxA minV maxV minC maxC minP maxP minR maxR minCs maxCs


# setup options
   upvar #0 $min minX
   upvar #0 $max maxX
   upvar #0 $val valX

# setup histogram
   wm_cursor . -watch
   set ndata [expr $Vis3d(regan:nh) + 1]
   set nh $Vis3d(regan:nh)
   if { $Vis3d(regan:setbinsize) } then {
	set xi $Vis3d(regan:binsize)
	set mX $minX
	if [string length $Vis3d(regan:rgx1)] then {
	    set mX $Vis3d(regan:rgx1)
	}
   } else {
	set xi [expr ($maxX - $minX)/${nh}.0]
	set mX $minX
   }
   for {set i 0} {$i <= $Vis3d(regan:nh)} {incr i} {
	set h($i) 0 ; set x($i) [expr $mX + $i * $xi + 0.5 * $xi]
   }
   foreach i [array names valX] {
	set j [expr round(($valX($i)-$mX)/$xi)]
	if { [expr $j >= 0] && [expr $j <= $ndata]} {
	    if [info exists h($j)] then {
	      if { $wt } {
		  set h($j) [expr $h($j) + $Volume($i)]
	      } else {
		  incr h($j)
	      }
	    }
	}
   }
   set maxH 0
   catch {unset hist}
   for {set i 0} {$i <= $Vis3d(regan:nh)} {incr i} {
	lappend hist $x($i) $h($i)
	if {$h($i) > $maxH} {set maxH $h($i)}
   }
   set maxH [expr $maxH * 5 / 4]

# plot or save histogram
   if { $Vis3d(regan:out) == 1 } then {
      pg stream select 2
   } elseif { $Vis3d(regan:out) == 0 } then {
      pg stream open 3 $Vis3d(regan:file)/ps
      pg stream select 3
   } else {
      set fid [open $Vis3d(regan:file) w]
      puts $fid "%ndata [expr $Vis3d(regan:nh) + 1]"
      puts $fid "%ncols 2"
      for {set i 0} {$i <= $Vis3d(regan:nh)} {incr i} {
	puts $fid "$x($i) $h($i)"
      }
      close $fid
      wm_cursor . -restore
      return {}
   }
   set mxX [expr $x($Vis3d(regan:nh)) + $xi]
   pg clear screen
   pg viewport 0.15 0.88 0.15 0.8
   pg window $minX $mxX 0 $maxH
   pg box -size 1.5 -font 2
   pg label $xlab -disp 2. -side b -font 2 -size 2
   pg label $ylab -disp 2. -side l -font 2 -size 2
   pg label "$xlab Histgram" -disp 2.0 -side t -font 2 -size 2
   pg curve -data  "$ndata $hist" -binned 1 -symbol 0
   if { $Vis3d(regan:out) == 1 } then {
      pg stream select 1
   } else {
      pg stream close 3
      pg stream select 1
   }
   wm_cursor . -restore
}


proc reg_doHistCoord { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Volume Area AreaF Coord Rad PrThr Constr\
	  minA maxA minV maxV minC maxC minP maxP minR maxR minCs maxCs

# setup histogram
   wm_cursor . -watch
   for {set i $minC} {$i <= $maxC} {incr i} {
	set h($i) 0
   }
   foreach i [array names Coord] {
	incr h($Coord($i))
   }
   set ndata [expr $maxC - $minC + 1]
   set maxH 0
   catch {unset hist}
   for {set i $minC} {$i <= $maxC} {incr i} {
	lappend hist $i $h($i)
	if {$h($i) > $maxH} {set maxH $h($i)}
   }
   set maxH [expr $maxH * 5 / 4]

# plot or save histogram
   if { $Vis3d(regan:out) == 1 } then {
      pg stream select 2
   } elseif { $Vis3d(regan:out) == 0 } then {
      pg stream open 3 $Vis3d(regan:file)/ps
      pg stream select 3
   } else {
      set fid [open $Vis3d(regan:file) w]
      puts $fid "%ndata [expr $Vis3d(regan:nh) + 1]"
      puts $fid "%ncols 2"
      for {set i $minC} {$i <= $maxC} {incr i} {
	puts $fid "$i $h($i)"
      }
      close $fid
      wm_cursor . -restore
      return {}
   }

   pg clear screen
   pg viewport 0.15 0.88 0.15 0.8
   pg window -0.5 [expr $maxC + 1] 0 $maxH
   pg box -size 1.5 -font 2
   pg label "Coordination" -disp 2. -side b -font 2 -size 2
   pg label "Number of Regions" -disp 2. -side l -font 2 -size 2
   pg label "Histogram" -disp 2.0 -side t -font 2 -size 2
   pg curve -data  "$ndata $hist" -binned 1 -symbol 0
   if { $Vis3d(regan:out) == 1 } then {
      pg stream select 1
   } else {
      pg stream close 3
      pg stream select 1
   }
   wm_cursor . -restore
}

proc reg_doFrac { area lab } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Volume Area AreaF Coord Rad PrThr Constr\
	  minA maxA minV maxV minC maxC minP maxP minR maxR minCs maxCs

# setup options
   upvar #0 $area ar ; upvar #0 Volume V

# fractal calculation
   wm_cursor . -watch

# plot or save fractal
   if { $Vis3d(regan:out) == 1 } then {
      pg stream select 2 ; set plot 1
   } elseif { $Vis3d(regan:out) == 0 } then {
      pg stream open 3 $Vis3d(regan:file)/ps
      pg stream select 3 ; set plot 1
   } else {
      set fid [open $Vis3d(regan:file) w]
      puts $fid "%ndata [expr $Vis3d(regan:i2) - $Vis3d(regan:i1) + 1]"
      puts $fid "%ncols 2"
      set plot 0
   }

   set minX [expr log10( $minV )/3.0]
   set maxX [expr log10( $maxV )/3.0]
   set minY [expr log10( $minA )]
   set maxY [expr log10( $maxA )]
   if [string length $Vis3d(regan:rgx1)] {set minX $Vis3d(regan:rgx1)}
   if [string length $Vis3d(regan:rgx2)] {set maxX $Vis3d(regan:rgx2)}
   if [string length $Vis3d(regan:rgy1)] {set minY $Vis3d(regan:rgy1)}
   if [string length $Vis3d(regan:rgy2)] {set maxY $Vis3d(regan:rgy2)}
   if { $plot } then {
     pg clear screen
     pg viewport 0.15 0.88 0.15 0.8
     pg window $minX $maxX $minY $maxY
     pg box -size 1.5 -font 2
     pg label "log( V\\u1/3\\d )" -disp 2. -side b -font 2 -size 2
     pg label $lab -disp 2. -side l -font 2 -size 2
   }
   foreach i [array names ar] {
	set x [expr log10( $V($i) )/3.0]
	set y [expr log10( $ar($i) )]
	if { $plot } then {
	    pg symbol $x $y
	} else {
	    puts $fid "$x  $y"
	}
   }
   if { $plot } then {
     if { $Vis3d(regan:out) == 1 } then {
        pg stream select 1
     } else {
        pg stream close 3
        pg stream select 1
     }
   } else {
     close $fid
   }
   wm_cursor . -restore
}

proc reg_doTwoDHist { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Volume Area AreaF Coord Rad PrThr Constr\
	  minA maxA minV maxV minC maxC minP maxP minR maxR minCs maxCs


# setup options
   upvar #0 minR minX
   upvar #0 maxR maxX
   upvar #0 Rad valX

# setup histogram
   wm_cursor . -watch
   set wt $Vis3d(regan:vv)
   set ndata [expr $Vis3d(regan:2drnbin) + 1]
   set nh $Vis3d(regan:2drnbin)
   set xi $Vis3d(regan:2drbin)
   set mX $Vis3d(regan:2drstart)

   for {set i $minC} {$i <= $maxC} {incr i} {
      for {set j 0} {$j <= $nh} {incr j} {
	set h($i,$j) 0
      }
   }
   foreach k [array names valX] {
	set i $Coord($k)
	set j [expr round(($valX($k)-$mX)/$xi)]
	if { [expr $j >= 0] && [expr $j <= $nh]} {
	    if { $wt } {
		set h($i,$j) [expr $h($i,$j) + $Volume($k)]
	    } else {
		incr h($i,$j)
	    }
	}
   }

# save histogram
   set fid [open $Vis3d(regan:2dfile) w]
   for {set i $minC} {$i <= $maxC} {incr i} {
     for {set j 0} {$j <= $nh} {incr j} {
	puts $fid $h($i,$j)
     }
     puts $fid {}
   }
   close $fid
   wm_cursor . -restore
   return {}
}
