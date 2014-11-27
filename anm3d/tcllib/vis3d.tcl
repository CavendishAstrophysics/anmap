#
# Procedures for Vis3d
#
proc vis_load { } {
   fileselect .fs -command vis_loadFile -centre -pattern {*.dat */defn}\
	-title "Vis3d FileSelect"
}

proc vis_load2 { } {
   fileselect .fs -command vis_load2File -centre -pattern {*.dat */defn}\
	-title "Vis3d FileSelect 2"
}

proc vis_save { type } {
   if {$type == "image"} then {
      fileselect .fs -command vis_saveImid -centre -pattern {*.dat defn}\
	-title "Vis3d FileSelect"
   } else {
      fileselect .fs -command vis_saveImdisp -centre -pattern {*.dat defn}\
	-title "Vis3d FileSelect"
   }
}

proc vis_loadFile { file } {
   global Imid Imdisp Imdef Imdef1 Vis3d
   if {$file == "defn"} then {
	set file [pwd]
   } elseif {$file == "data"} then {
	set file [pwd]
   } else {
	set file [file dir $file]
   }

   catch { img_image destroy $Imid }
   wm_cursor . -watch
   set Imid [img_image read -file $file -type 1]
   vis_imgReset $Imid $file
   wm_cursor . -restore
}

proc vis_load2File { file } {
   global Imid Imid2 Imdisp Imdef Imdef1 Vis3d
   if {$file == "defn"} then {
	set file [pwd]
   } elseif {$file == "data"} then {
	set file [pwd]
   } else {
	set file [file dir $file]
   }

   catch { img_image destroy $Imid2 }
   wm_cursor . -watch
   set Imid2 [img_image read -file $file -type 1]
   vis_imgReset2 $Imid2 $file
   wm_cursor . -restore
}

proc vis_imgReset { im file } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
   set Imid $im
   img_image defget $Imid Imdef
   img_image defget $Imid Imdef1
   set Vis3d(anal:img) $Imid
   set Vis3d(movie:pl) xy
   set Vis3d(file) $file
   set Vis3d(anal:img) 1
   vis_setDefn
   vis_movieSetup
}

proc vis_imgReset2 { im file } {
   global Imid Imid2 Imdisp Imdef Imdef1 Imdef2 Vis3d Andef
   set Imid2 $im
   img_image defget $Imid2 Imdef2
   set Vis3d(file2) $file
}

proc vis_saveImid { file } { global Imid ; vis_saveFile $file $Imid }
proc vis_saveImdisp { file } {global Imdisp ;  vis_saveFile $file $Imdisp }

proc vis_saveFile { file img } {
   global Imid Imdisp Imdef Imdef1
   if {$file == "defn"} then {
	set file [pwd]
   } elseif {$file == "data"} then {
	set file [pwd]
   } else {
	set tail [file tail $file]
	if {$tail == "defn"} then {
		set file [file dir $file]
	} elseif {$tail == "data"} then {
		set file [file dir $file]
	}
   }
   img_image write -imid $img -file $file -type 1
}


proc vis_planeReset { } {
   global Imid Imdisp Imdef Imdef1
   img_image defget $Imid Imdef1
}

proc vis_MovieStart { } {
   global Imid Imdisp Imdef Imdef1
   set Vis3d(movie:sl) $Vis3d(movie:sl1)
}

proc vis_movieSetup { } {
   global Imid Imdisp Imdef Imdef1 Vis3d
   img_image defget $Imid defn
   switch $Vis3d(movie:pl) {
	{xy}	{set Vis3d(movie:sl1) $defn(z1)
		 set Vis3d(movie:sl2) $defn(z2)}
	{yz}	{set Vis3d(movie:sl1) $defn(x1)
		 set Vis3d(movie:sl2) $defn(x2)}
	{xz}	{set Vis3d(movie:sl1) $defn(y1)
		 set Vis3d(movie:sl2) $defn(y2)}
   }
   set Vis3d(movie:sl) $Vis3d(movie:sl1)
}

proc vis_moviePlay { } {
   global Imid Imdisp Imdef Imdef1 Vis3d
   set Vis3d(movie:play) 1
   img_image defget $Imid defn
   switch $Vis3d(movie:pl) {
	{xy}	{set Vis3d(movie:sl1) $defn(z1)
		 set Vis3d(movie:sl2) $defn(z2)}
	{yz}	{set Vis3d(movie:sl1) $defn(x1)
		 set Vis3d(movie:sl2) $defn(x2)}
	{xz}	{set Vis3d(movie:sl1) $defn(y1)
		 set Vis3d(movie:sl2) $defn(y2)}
   }
   if { [expr $Vis3d(movie:sl) < $Vis3d(movie:sl1)] ||
	[expr $Vis3d(movie:sl) > $Vis3d(movie:sl2)] } then {
	set Vis3d(movie:sl) $Vis3d(movie:sl1)
   }
   vis_movieDisplay
}

proc vis_movieStop { } {
   global Imid Imdisp Imdef Imdef1 Vis3d
   set Vis3d(movie:play) 0
}

proc vis_movieIncr { n } {
   global Imid Imdisp Imdef Imdef1 Vis3d
   incr Vis3d(movie:sl) $n
   if { [expr $Vis3d(movie:sl) < $Vis3d(movie:sl1)] } then {
	set Vis3d(movie:sl) $Vis3d(movie:sl2)
   } elseif { [expr $Vis3d(movie:sl) > $Vis3d(movie:sl2)] } then {
	set Vis3d(movie:sl) $Vis3d(movie:sl1)
   }
   vis_movieDisplay
}

proc vis_movieDisplay { } {
   global Imid Imdisp Imdef Imdef1 Vis3d ImdefD
   img_image defget $Imid defn
   catch { img_image destroy $Imdisp }
   switch $Vis3d(movie:pl) {
	{xy}	{set Imdisp [img_geom $Imid subimage \
			-v1 $Imdef1(v1) -v2 $Imdef1(v1) \
			-x1 $Imdef1(x1) -x2 $Imdef1(x2) \
			-y1 $Imdef1(y1) -y2 $Imdef1(y2) \
			-z1 $Vis3d(movie:sl) -z2 $Vis3d(movie:sl)]}
	{yz}	{set Imdisp [img_geom $Imid subimage \
			-v1 $Imdef1(v1) -v2 $Imdef1(v1) \
			-z1 $Imdef1(z1) -x2 $Imdef1(z2) \
			-y1 $Imdef1(y1) -y2 $Imdef1(y2) \
			-x1 $Vis3d(movie:sl) -x2 $Vis3d(movie:sl)]}
	{xz}	{set Imdisp [img_geom $Imid subimage \
			-v1 $Imdef1(v1) -v2 $Imdef1(v1) \
			-x1 $Imdef1(x1) -x2 $Imdef1(x2) \
			-z1 $Imdef1(z1) -z2 $Imdef1(z2) \
			-y1 $Vis3d(movie:sl) -y2 $Vis3d(movie:sl)]}
   }
   img_image defget $Imdisp ImdefD
   img_image collapse $Imdisp
   vis_movieUpdate
}

proc vis_movieUpdate { } {
   global Imid Imdisp Imdef Imdef1 Vis3d ImdefD
   img_image defget $Imdisp defn
   pg clear screen
   pg window 1 $defn(xdim) 1 $defn(ydim)
   set stats [img_anal $Imdisp statistics]
   set x1 [lindex $stats 0] ; set x2 [lindex $stats 1]
   if [string length $Vis3d(disp:x1)] {set x1 $Vis3d(disp:x1)}
   if [string length $Vis3d(disp:x2)] {set x2 $Vis3d(disp:x2)}
   pg image $Imdisp $x1 $x2 $Vis3d(tran) ; pg box -font 2 -size 1.5
   if { $Vis3d(movie:play) } then { after 1000 vis_movieIncr +1 }
}

proc vis_setDefn { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef ImdefD
   if { $Vis3d(anal:img) == 1 } then {
     img_image defget $Imid Andef
   } else {
     img_image defget $Imdisp Andef
   }
}

proc vis_analSlice { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef

# read cursor input
   pg stream select 1
   set pos1 [pg cursor -mode 7]
   set x1 [expr round( [lindex $pos1 0] ) + $Imdef1(x1) - 1]
   set y1 [expr round( [lindex $pos1 1] ) + $Imdef1(y1) - 1]
   set pos2 [pg cursor -xref $x1 -yref $y1 -mode 1]
   set x2 [expr round( [lindex $pos2 0] ) + $Imdef1(x1) - 1]
   set y2 [expr round( [lindex $pos2 1] ) + $Imdef1(y1) - 1]

# extract slice
   set slice [img_geom $Imdisp slice -x1 $x1 -x2 $x2 -y1 $y1 -y2 $y2 -ndata 100]

# plot slice
   pg stream select 2 ; pg clear screen
   set stx [img_anal $slice statistics -v1 1 -v2 1]
   set sty [img_anal $slice statistics -v1 2 -v2 2]
   pg viewport 0.15 0.88 0.15 0.8
   pg window [lindex $stx 0] [lindex $stx 1] [lindex $sty 0] \
             [expr [lindex $sty 1] * 1.25]
   pg box -size 1.5
   pg label "Displacement" -disp 2. -side b -font 2 -size 2
   pg label "Image intensity" -disp 2. -side l -font 2 -size 2
   pg label "Slice" -disp 2.0 -side t -font 2 -size 2
   pg label "From: $x1 $y1  To: $x2 $y2" -disp 0.5 -side t -font 2 -size 1.5 \
                           -just 0.0 -coord 0.0
   pg curve -id $slice
   pg stream select 1
   if { $Vis3d(anal:save) } then {
	set fid [open $Vis3d(anal:file) w]
	set xar [img_pixel $slice get -v1 1 -v2 1]
	set yar [img_pixel $slice get -v1 2 -v2 2]
	for {set i 0} {$i < [llength $xar]} {incr i} {
	   puts $fid "[lindex $xar $i ] [lindex $yar $i]"
	}
	close $fid
   }
   img_image destroy $slice
}

proc vis_analStats { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef

   if { $Vis3d(anal:img) == 1 } then {
     set id $Imid
   } else {
     set id $Imdisp
     if { $Vis3d(anal:reg) } then {vis_getRegion}
   }
   wm_cursor . -watch
   set st [img_anal $id statistics -x1 $Andef(x1) -x2 $Andef(x2) \
           -y1 $Andef(y1) -y2 $Andef(y2) -z1 $Andef(z1) -z2 $Andef(z2) \
           -v1 $Andef(v1) -v2 $Andef(v1)]
   wm_cursor . -restore
   set Vis3d(anal:min) [lindex $st 0]
   set Vis3d(anal:max) [lindex $st 1]
   set Vis3d(anal:mn) [lindex $st 2]
   set Vis3d(anal:sd) [lindex $st 3]
#   set Vis3d(anal:sum) [img_anal $id sum -x1 $Andef(x1) -x2 $Andef(x2) \
           -y1 $Andef(y1) -y2 $Andef(y2) -z1 $Andef(z1) -z2 $Andef(z2) \
           -v1 $Andef(v1) -v2 $Andef(v1)]
}

proc vis_getRegion { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef
  set c1 [pg cursor -mode 0]
  set c2 [pg cursor -mode 2 -xref [lindex $c1 0] -yref [lindex $c1 1]]
  set Andef(x1) [expr round ( [lindex $c1 0] )]
  set Andef(y1) [expr round ( [lindex $c1 1] )]
  set Andef(x2) [expr round ( [lindex $c2 0] )]
  set Andef(y2) [expr round ( [lindex $c2 1] )]
}

proc vis_analHist { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef

   set w .hist
   catch {destroy $w}
   toplevel $w ; wm iconbitmap $w @$Vis3d(src)/vis3d.xbm
   wm title $w "Image histogram"
   wm iconname $w "Image histogram"

   pack [frame $w.bar -relief flat] -side top -fill x
     pack [button $w.bar.dismiss -text Dismiss -command "destroy $w" \
           -borderwidth 2 -relief raised -width 8] \
           -side left -anchor w -padx 4 -pady 2
     pack [button $w.bar.plot -text Plot -command vis_analHistPlot \
           -borderwidth 2 -relief raised -width 8] \
           -side left -anchor w -padx 4 -pady 2
   pack [frame $w.ndata -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.ndata.l -relief flat -text "Number of bins: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.ndata.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(anal:histn) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.ndata.e <Return> {vis_analHistPlot}
   pack [frame $w.hist1 -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.hist1.l -relief flat -text "Minimum in range: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.hist1.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(anal:hist1) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.hist1.e <Return> {vis_analHistPlot}
   pack [frame $w.hist2 -relief groove -borderwidth 2] \
         -side top -fill x -padx 4 -pady 4
     pack [label $w.hist2.l -relief flat -text "Maximum in range: " \
           -width 20 -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [entry $w.hist2.e -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(anal:hist2) ] \
           -side left -anchor w -padx 4 -pady 4
     bind $w.hist2.e <Return> {vis_analHistPlot}
   pack [frame $w.opts -relief flat] \
         -side top -fill x -padx 4 -pady 4
     pack [checkbutton $w.opts.c -relief flat -text "Use full range" \
           -width 20 -variable Vis3d(anal:histmm) -anchor w] \
           -side left -anchor w -padx 4 -pady 4
     pack [checkbutton $w.opts.cbs -relief flat -borderwidth 2 \
           -width 18 -variable Vis3d(anal:save)\
	   -text "Save to file:" -anchor w] \
           -side left -anchor w -padx 4
     pack [entry $w.opts.sbse -relief sunken -borderwidth 2 \
           -width 20 -textvariable Vis3d(anal:file) ] \
           -side left -anchor w -padx 4
}

proc vis_analHistPlot { } {
   global Imid Imdisp Imdef Imdef1 Vis3d Andef

   if { $Vis3d(anal:img) == 1 } then {
     set id $Imid
   } else {
     set id $Imdisp
     if { $Vis3d(anal:reg) } then {vis_getRegion}
   }
   if { $Vis3d(anal:histmm) } then {
      set st [img_anal $id statistics -x1 $Andef(x1) -x2 $Andef(x2) \
           -y1 $Andef(y1) -y2 $Andef(y2) -z1 $Andef(z1) -z2 $Andef(z2) \
           -v1 $Andef(v1) -v2 $Andef(v1)]
      set Vis3d(anal:hist1) [lindex $st 0]
      set Vis3d(anal:hist2) [lindex $st 1]
   }
   wm_cursor . -watch
   set hist [img_anal $id histogram -x1 $Andef(x1) -x2 $Andef(x2) \
           -y1 $Andef(y1) -y2 $Andef(y2) -z1 $Andef(z1) -z2 $Andef(z2) \
           -v1 $Andef(v1) -v2 $Andef(v1) -ndata $Vis3d(anal:histn) \
           -low $Vis3d(anal:hist1) -high $Vis3d(anal:hist2)]
   wm_cursor . -restore

# plot histogram
   pg stream select 2 ; pg clear screen
   set stx [img_anal $hist statistics -v1 1 -v2 1]
   set sty [img_anal $hist statistics -v1 2 -v2 2]
   pg viewport 0.15 0.88 0.15 0.8
   pg window [lindex $stx 0] [lindex $stx 1] [lindex $sty 0] \
             [expr [lindex $sty 1] * 1.25]
   pg box -size 1.5
   pg label "Intensity" -disp 2. -side b -font 2 -size 2
   pg label "Number of pixels" -disp 2. -side l -font 2 -size 2
   pg label "Histogram" -disp 2.0 -side t -font 2 -size 2
   pg curve -id $hist -binned 1
   if { $Vis3d(anal:save) } then {
	set fid [open $Vis3d(anal:file) w]
	set xar [img_pixel $hist get -v1 1 -v2 1]
	set yar [img_pixel $hist get -v1 2 -v2 2]
	for {set i 0} {$i < [llength $xar]} {incr i} {
	   puts $fid "[lindex $xar $i ] [lindex $yar $i]"
	}
	close $fid
   }
   img_image destroy $hist
   pg stream select 1
}
