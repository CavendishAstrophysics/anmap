

proc vis_subImage { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Seldef
   set w .subim
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Select Subimage"
   wm iconname $w "Select Subimage"
   pack [frame $w.bar -relief flat] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.bar.dismiss -command "destroy $w" -text Dismiss \
           -width 8 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.select -command "vis_doSubImage" -text Select \
           -width 8 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 4 -pady 2
   pack [frame $w.reg1 -relief flat] \
	-side top -fill x -padx 2 -pady 2
   foreach t {x1 x2 y1 y2} {
     pack [label $w.reg1.${t} -text "$t:" -width 3] \
           -side left -anchor w -padx 2
     pack [entry $w.reg1.e${t} -textvariable Seldef(${t}) -width 4 \
           -relief sunken -borderwidth 2] \
           -side left -anchor w -padx 4
     set Seldef(${t}) $Imdef(${t})
     bind $w.reg1.e${t} <Return> { }
     pack [label $w.reg1.t${t} -text {} -width 1] \
           -side left -anchor w -padx 2
   }
   pack [frame $w.reg2 -relief flat] \
	-side top -fill x -padx 2 -pady 2
   foreach t {z1 z2 v1} {
     pack [label $w.reg2.${t} -text "$t:" -width 3] \
           -side left -anchor w -padx 2
     pack [entry $w.reg2.e${t} -textvariable Seldef(${t}) -width 4 \
           -relief sunken -borderwidth 2] \
           -side left -anchor w -padx 4
     set Seldef(${t}) $Imdef(${t})
     bind $w.reg2.e${t} <Return> { }
     pack [label $w.reg2.t${t} -text {} -width 1] \
           -side left -anchor w -padx 2
   }
}

proc vis_doSubImage { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Seldef
   wm_cursor . -watch
   set im [img_geom $Imid subimage -v1 $Seldef(v1) -v2 $Seldef(v1) \
	-x1 $Seldef(x1) -x2 $Seldef(x2) -y1 $Seldef(y1) -y2 $Seldef(y2) \
	-z1 $Seldef(z1) -z2 $Seldef(z2) ]
   img_image destroy $Imid
   vis_imgReset $im {}
   wm_cursor . -restore
}

proc vis_tile { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Seldef
   set w .subim
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Tile Subimages"
   wm iconname $w "Tile Subimages"
   pack [frame $w.bar -relief flat] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.bar.dismiss -command "destroy $w" -text Dismiss \
           -width 8 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.tile -command "vis_doTile" -text Tile \
           -width 8 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 4 -pady 2

   pack [label $w.regl -relief flat -text "Sub-image for each tile"] \
	-side top -fill x -padx 2 -pady 2
   pack [frame $w.reg1 -relief flat] \
	-side top -fill x -padx 2 -pady 2
   foreach t {x1 x2 y1 y2} {
     pack [label $w.reg1.${t} -text "$t:" -width 5] \
           -side left -anchor w -padx 2
     pack [entry $w.reg1.e${t} -textvariable Seldef(${t}) -width 4 \
           -relief sunken -borderwidth 2] \
           -side left -anchor w -padx 4
     set Seldef(${t}) $Imdef(${t})
     bind $w.reg1.e${t} <Return> { }
     pack [label $w.reg1.t${t} -text {} -width 1] \
           -side left -anchor w -padx 2
   }
   pack [frame $w.reg2 -relief flat] \
	-side top -fill x -padx 2 -pady 2
   foreach t {v1 v2} {
     pack [label $w.reg2.${t} -text "$t:" -width 5] \
           -side left -anchor w -padx 2
     pack [entry $w.reg2.e${t} -textvariable Seldef(${t}) -width 4 \
           -relief sunken -borderwidth 2] \
           -side left -anchor w -padx 4
     set Seldef(${t}) $Imdef(${t})
     bind $w.reg2.e${t} <Return> { }
     pack [label $w.reg2.t${t} -text {} -width 1] \
           -side left -anchor w -padx 2
   }

   pack [label $w.regll -relief flat -text "Tiling information"] \
	-side top -fill x -padx 2 -pady 2
   pack [frame $w.reg3 -relief flat] \
	-side top -fill x -padx 2 -pady 2
   foreach t {z1 z2 zincr} {
     pack [label $w.reg3.${t} -text "$t:" -width 5] \
           -side left -anchor w -padx 2
     pack [entry $w.reg3.e${t} -textvariable Seldef(${t}) -width 4 \
           -relief sunken -borderwidth 2] \
           -side left -anchor w -padx 4
     catch {set Seldef(${t}) $Imdef(${t})}
     bind $w.reg3.e${t} <Return> { }
     pack [label $w.reg3.t${t} -text {} -width 1] \
           -side left -anchor w -padx 2
   }
   set Seldef(zincr) 1
}

proc vis_doTile { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Seldef
   wm_cursor . -watch
   set im [img_tileXY $Imid \
	-v1 $Seldef(v1) -v2 $Seldef(v1) \
	-x1 $Seldef(x1) -x2 $Seldef(x2) \
	-y1 $Seldef(y1) -y2 $Seldef(y2) \
	-z1 $Seldef(z1) -z2 $Seldef(z2) -incr $Seldef(zincr)]
   img_image destroy $Imid
   vis_imgReset $im {}
   wm_cursor . -restore
}

proc vis_geomTranspose { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Seldef
   set w .gtrans
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Transpose Image"
   wm iconname $w "Transpose Image"
   pack [frame $w.bar -relief flat] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.bar.dismiss -command "destroy $w" -text Dismiss \
           -width 8 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.select -text Tanspose \
	   -command vis_doTranspose \
           -width 8 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 4 -pady 2
   pack [frame $w.tr -relief flat] \
	-side top -fill x -padx 2 -pady 2
     pack [label $w.tr.l -anchor w -text "Axis order:" \
	   -width 12] -side left -anchor w -padx 2 -pady 5
     pack [entry $w.tr.e -textvariable Vis3d(geom:axis) \
	   -relief sunken -width 12] \
	   -side left -anchor w -padx 2 -pady 5
}

proc vis_doTranspose { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Seldef
   wm_cursor . -watch
   set im [eval img_geom $Imid transpose $Vis3d(geom:axis)]
   img_image destroy $Imid
   vis_imgReset $im {}
   wm_cursor . -restore
}

proc vis_geomBin { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Seldef
   set w .gbin
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Rebin Image"
   wm iconname $w "Rebin Image"
   pack [frame $w.bar -relief flat] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.bar.dismiss -command "destroy $w" -text Dismiss \
           -width 8 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.select -text Rebin \
	   -command {vis_doBin}\
           -width 8 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 4 -pady 2
   pack [frame $w.binx -relief flat] \
	-side top -fill x -padx 2 -pady 2
     pack [label $w.binx.l -anchor w -text "Bin in X pixels:" \
	   -width 16] -side left -anchor w -padx 2 -pady 5
     pack [entry $w.binx.e -textvariable Vis3d(geom:binx) \
	   -relief sunken -width 8] \
	   -side left -anchor w -padx 2 -pady 5
   pack [frame $w.biny -relief flat] \
	-side top -fill x -padx 2 -pady 2
     pack [label $w.biny.l -anchor w -text "Bin in Y pixels:" \
	   -width 16] -side left -anchor w -padx 2 -pady 5
     pack [entry $w.biny.e -textvariable Vis3d(geom:biny) \
	   -relief sunken -width 8] \
	   -side left -anchor w -padx 2 -pady 5
   pack [frame $w.binz -relief flat] \
	-side top -fill x -padx 2 -pady 2
     pack [label $w.binz.l -anchor w -text "Bin in Z pixels:" \
	   -width 16] -side left -anchor w -padx 2 -pady 5
     pack [entry $w.binz.e -textvariable Vis3d(geom:binz) \
	   -relief sunken -width 8] \
	   -side left -anchor w -padx 2 -pady 5
}

proc vis_doBin { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Seldef
   wm_cursor . -watch
   set im [img_geom $Imid bin -x $Vis3d(geom:binx) \
	   -y $Vis3d(geom:biny) -z $Vis3d(geom:binz)]
   img_image destroy $Imid
   vis_imgReset $im {}
   wm_cursor . -restore
}

proc vis_geomProject { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Seldef
   set w .gproj
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Project Image"
   wm iconname $w "Project Image"
   pack [frame $w.bar -relief flat] \
	-side top -fill x -padx 2 -pady 2
     pack [button $w.bar.dismiss -command "destroy $w" -text Dismiss \
           -width 8 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.select -text project \
	   -command {vis_doProject}\
           -width 8 -relief raised -borderwidth 2] \
           -side left -anchor w -padx 4 -pady 2
   pack [radiobutton $w.projx -text "Along X" -variable Vis3d(geom:projdir)\
	-value x -width 12 -relief flat] -side top -fill x -padx 4 -pady 4
   pack [radiobutton $w.projy -text "Along Y" -variable Vis3d(geom:projdir)\
	-value y -width 12 -relief flat] -side top -fill x -padx 4 -pady 4
   pack [radiobutton $w.projz -text "Along Z" -variable Vis3d(geom:projdir)\
	-value z -width 12 -relief flat] -side top -fill x -padx 4 -pady 4
}

proc vis_doProject { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   global Seldef
   wm_cursor . -watch
   set im [img_geom $Imid project $Vis3d(geom:projdir)]
   img_image destroy $Imid
   vis_imgReset $im {}
   wm_cursor . -restore
}
