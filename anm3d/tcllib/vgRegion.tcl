#
# Display 3D shape
#
proc vis_visRegions { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef

   set w .vissh
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Visualize shapes"
   wm iconname $w "Visualize shapes"
   if { $Vis3d(vissh:init) == 0 } { 
	vg_init 10
	vg move 0 0 0
	set Vis3d(vissh:init) 1
   }
   set Vis3d(aw) $w
   set Vis3d(vissh:obj) -100
   pack [frame $w.bar -relief flat] -side top -fill x
     pack [button $w.bar.dismiss -text Dismiss -command "destroy $w" \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.update -text Update -command vg_visObject \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2

   pack [frame $w.i -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.i.l -relief flat -text "Region: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.i.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(vissh:i) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.i.e <Return> {vg_visObject}
   pack [frame $w.x -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [scale $w.x.angle -from 0 -to 360 -label "Angle X" \
           -showvalue 1 -tickinterval 45 -orient horizontal \
           -command vg_visDisplay] \
         -side top -fill x -padx 4 -pady 4
   pack [frame $w.y -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [scale $w.y.angle -from 0 -to 360 -label "Angle Y" \
           -showvalue 1 -tickinterval 45 -orient horizontal \
           -command vg_visDisplay] \
         -side top -fill x -padx 4 -pady 4
   pack [frame $w.z -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [scale $w.z.angle -from 0 -to 360 -label "Angle Z" \
           -showvalue 1 -tickinterval 45 -orient horizontal \
           -command vg_visDisplay] \
         -side top -fill x -padx 4 -pady 4
}

proc vg_visObject { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   set Vis3d(vissh:obj) [vg_makeobject $Imid $Vis3d(vissh:i)]
   vg_drawobject $Vis3d(vissh:obj)
}

proc vg_visDisplay { args } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   if { $Vis3d(vissh:obj) != -100 } then {
      vg pushmatrix
        vg rotate x [$Vis3d(aw).x.angle get]
        vg rotate y [$Vis3d(aw).y.angle get]
        vg rotate z [$Vis3d(aw).z.angle get]
        vg_drawobject $Vis3d(vissh:obj)
      vg popmatrix
   }
}

proc vg_init { s } {
  global Vis3d
  vg size 600 600
  vg stream open X11
  vg window -$s $s -$s $s $s -$s
  vg lookat 0 0 [expr 2 * $s] 0 0 0 0
  vg backbuffer
  set Vis3d(vissh:init) 1
}

proc vg_view { s } {
  vg window -$s $s -$s $s $s -$s
  vg lookat 0 0 [expr 2 * $s] 0 0 0 0
}


proc vg_drawobject { object } {
  vg clear
  vg object draw $object
  vg swapbuffers
}

proc vg_rotate { object axis angle } {
   vg pushmatrix
     vg rotate $axis $angle
     vg_drawobject $object
   vg popmatrix
}


proc vg_makeobject { imid i} {
  set ll [img_region getshape $imid $i -surface]
  set x 0.0 ; set y 0.0 ; set z 0.0 ; set nd 0
  wm_cursor . -watch
  for {set n 0} {$n < [llength $ll]} {incr n} {
     incr nd
     set xyz [lindex $ll $n]
     set x [expr $x + [lindex $xyz 0]]
     set y [expr $y + [lindex $xyz 1]]
     set z [expr $z + [lindex $xyz 2]]
  }
  set x [expr $x / $nd] ; set y [expr $y / $nd] ; set z [expr $z / $nd]
  set object [vg object next]
  vg object open $object
  for {set n 0} {$n < [llength $ll]} {incr n} {
     set xyz [lindex $ll $n]
     vg_makecube [expr [lindex $xyz 0] - $x] \
		 [expr [lindex $xyz 1] - $y] \
		 [expr [lindex $xyz 2] - $z] \
		 [lindex $xyz 3]
     update idletasks
  }
  vg object close
  wm_cursor . -restore
  return $object
}

proc vg_makecube { x y z col} {
     incr col
     vg style -fill 0 -backface 1 -color $col
     vg cube $x $y $z 1.0
}

#
# Widget and commands to produce a thinned image
#
proc reg_makeThin { } {
   global Vis3d

   set w .regth
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Construct Thinned Image"
   wm iconname $w "Thinned Image"
   pack [frame $w.bar -relief flat] -side top -fill x
     pack [button $w.bar.dismiss -text Dismiss -command "destroy $w" \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.thin -text Construct -command reg_doThin \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2
     pack [checkbutton $w.bar.type -text {Skeleton} \
	   -variable Vis3d(regth:type) \
           -borderwidth 2 -relief flat -width 10] \
           -side left -anchor w -padx 4 -pady 2
   pack [frame $w.g -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.g.l -relief flat -text "Gate for binary image: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.g.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(regth:gate) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.g.e <Return> { }
   pack [frame $w.ns -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [message $w.ns.m -relief flat \
          -text "Number of exposed faces is the number of faces of a pixel\
		which must be exposed before the pixel is thinned.\
		If this limit is reached before all the pixels are thinned\
		then it is reduced in integer steps to 1." -aspect 500] \
           -side top -fill x -padx 4 -pady 4
     pack [frame $w.ns.e -relief flat] \
         -side top -fill x -padx 4 -pady 4 -fill x
     pack [label $w.ns.e.l -relief flat -text "Number of faces: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.ns.e.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(regth:ns) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.ns.e.e <Return> { }


}
proc reg_doThin { } {
   global Vis3d Imid
   wm_cursor . -watch
   img_apply $Imid binary $Vis3d(regth:gate)
   if { $Vis3d(regth:type) == 1 } then {
      img_region thin $Imid -nsmax $Vis3d(regth:ns) -report -skeleton
   } else {
      img_region thin $Imid -nsmax $Vis3d(regth:ns) -report
   }
   set nreg [expr round( [lindex [img_anal $Imid maxmin] 1] ) ]
   wm_cursor . -restore
   tk_dialog .message "Thinning" \
	"A total of $nreg regions were used to thin the image.\
	The thinned image has replaced the original image in Vis3d." info 0 OK
}

#
# Widget and commands to perform region identification
#
proc reg_makeIdent { } {
   global Vis3d

   set w .regident
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Identify Thinned Image"
   wm iconname $w "Identify Image"
   pack [frame $w.bar -relief flat] -side top -fill x
     pack [button $w.bar.dismiss -text Dismiss -command "destroy $w" \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.plot -text Identify -command reg_doIdent \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2

   pack [frame $w.mg -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [message $w.mg.m -relief flat \
          -text "For a thinned image it is possible to merge \
                 thinned regions.  Merging regions which are thinned\
                 in the first stages of thinning should reduce the number of\
                 small pores identified in neck-like regions;\
                 merging regions thinned in the latter stages should improve\
                 the speed of the identification algorithm." -aspect 500] \
           -side top -fill x -padx 4 -pady 4
     pack [frame $w.mg.l -relief flat] \
         -side top -fill x -padx 4 -pady 4 -fill x
     pack [label $w.mg.l.l -relief flat -text "Merge below: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.mg.l.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(regth:mlow) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.mg.l.e <Return> { }
     pack [frame $w.mg.h -relief flat] \
         -side top -fill x -padx 4 -pady 4 -fill x
     pack [label $w.mg.h.l -relief flat -text "Merge above: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.mg.h.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(regth:mhigh) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.mg.h.e <Return> { }

}
proc reg_doIdent { } {
   global Vis3d Imid
   wm_cursor . -watch
   set xmax [lindex [img_anal $Imid maxmin] 1] ; set tmax $Vis3d(regth:mhigh)
   if { $Vis3d(regth:mlow) > 1.0 } {
	set xmin [expr round( $Vis3d(regth:mlow) )]
	img_apply $Imid range 0.5 [expr 0.5 + $xmin ] = 1.0 \
			range [expr $xmin + 0.5] [expr $xmax + 0.5] - $xmin
	set tmax [expr $Vis3d(regth:mhigh) - $xmin]
	set xmax [expr $xmax - $xmin]
   }
   if { $tmax < $xmax } {
	set xmin [expr round( $tmax )]
	img_apply $Imid range [expr $xmin - 0.5] [expr $xmax + 0.5] =  $xmin
   }
   img_region identify $Imid -report
   wm_cursor . -restore
   set nreg [expr round( [lindex [img_anal $Imid maxmin] 1] ) ]
   tk_dialog .message "Region Identification" \
	"A total of $nreg regions were identified.\
	The region-identified image has replaced the\
	original image in Vis3d." info 0 OK
}
