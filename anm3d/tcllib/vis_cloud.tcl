#
# Display 3D volume as a cloud image
#
proc vis_cloud { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef

   set w .viscl
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Visualize Cloud"
   wm iconname $w "Visualize Cloud"
   if { $Vis3d(vissh:init) == 0 } { 
	vg_init 80
	vg move 0 0 0
	set Vis3d(vissh:init) 1
   }
   set Vis3d(aw) $w
   set Vis3d(vissh:obj) -100
   pack [frame $w.bar -relief flat] -side top -fill x
     pack [button $w.bar.dismiss -text Dismiss -command "destroy $w" \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.display -text Display -command vis_cloudMake \
           -borderwidth 2 -relief raised -width 10] \
           -side left -anchor w -padx 4 -pady 2

   pack [frame $w.dr -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.dr.l -relief flat -text "Range: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.dr.min -width 8 -relief sunken -borderwidth 2 \
	   -textvariable Vis3d(viscl:min)] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.dr.max -width 8 -relief sunken -borderwidth 2 \
	   -textvariable Vis3d(viscl:max)] \
           -side left -anchor w -padx 4 -pady 4

   pack [frame $w.ax -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [scale $w.ax.angle -from 0 -to 360 -label "Angle X" \
           -showvalue 1 -tickinterval 45 -orient horizontal \
           -command vis_cloudDisplay] \
         -side top -fill x -padx 4 -pady 4
   pack [frame $w.ay -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [scale $w.ay.angle -from 0 -to 360 -label "Angle Y" \
           -showvalue 1 -tickinterval 45 -orient horizontal \
           -command vis_cloudDisplay] \
         -side top -fill x -padx 4 -pady 4
   pack [frame $w.az -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [scale $w.az.angle -from 0 -to 360 -label "Angle Z" \
           -showvalue 1 -tickinterval 45 -orient horizontal \
           -command vis_cloudDisplay] \
         -side top -fill x -padx 4 -pady 4
   pack [frame $w.x -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [scale $w.x.d -from -200 -to 200 -label "X" \
           -showvalue 1 -tickinterval 50 -orient horizontal \
           -command vis_cloudDisplay] \
         -side top -fill x -padx 4 -pady 4
   pack [frame $w.y -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [scale $w.y.d -from -200 -to 200 -label "Y" \
           -showvalue 1 -tickinterval 50 -orient horizontal \
           -command vis_cloudDisplay] \
         -side top -fill x -padx 4 -pady 4
   pack [frame $w.z -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [scale $w.z.d -from -200 -to 200 -label "Z" \
           -showvalue 1 -tickinterval 50 -orient horizontal \
           -command vis_cloudDisplay] \
         -side top -fill x -padx 4 -pady 4
}

proc vis_cloudMake { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   set object [vg object next]
   vg object open $object
     vg cloud $Imid $Vis3d(viscl:min) $Vis3d(viscl:max) -color 2
   vg object close
   set Vis3d(vissh:obj) $object
   vg_drawobject $Vis3d(vissh:obj)
}

proc vis_cloudDisplay { args } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   if { $Vis3d(vissh:obj) != -100 } then {
      vg pushmatrix
        vg rotate x [$Vis3d(aw).ax.angle get]
        vg rotate y [$Vis3d(aw).ay.angle get]
        vg rotate z [$Vis3d(aw).az.angle get]
      vg pushmatrix
      vg popmatrix
	vg translate \
	 [$Vis3d(aw).x.d get] [$Vis3d(aw).y.d get] [$Vis3d(aw).z.d get]
        vg_drawobject $Vis3d(vissh:obj)
      vg popmatrix
   }
}
