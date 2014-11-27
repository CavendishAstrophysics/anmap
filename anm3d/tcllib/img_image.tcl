#
# Tcl procedures to implement the "image" command
#
proc img_image { option args } {

global iclRec iclPriv imgDefn

# sort out option and take appropriate action
switch -- $option {
{read}	{set ce -1 ;
	 if [info exists iclRec(type)] then {
		set type $iclRec(type)
	 } else {
		set type 0
	 }
	 if [info exists iclRec(file)] then {
		set file $iclRec(file)
	 } else {
		set file {}
	 }
	 for {set n 0} {$n < [llength $args]} {incr n} {
	   switch -- [lindex $args $n] {
		{-ce}	{incr n ; set ce [lindex $args $n]
			 iclPriv read $ce
			 set type $iclRec(type) ; set file $iclRec(file)}
		{-file}	{incr n ; set file [lindex $args $n]}
		{-type}	{incr n ; set type [lindex $args $n]}
		{default}
			{set ce [lindex $args $n]
			 iclPriv read $ce
			 set type $iclRec(type) ; set file $iclRec(file)}
	   }
	 }
	 if [file exists $file] then {
		set n [imgPriv read $file $type]
		return $n
	 } else {
		return -code error "file $file not found"
	 }
	}
{write}	{set ce -1 ; set ih -1
	 if [info exists iclRec(type)] then {
		set type $iclRec(type)
	 } else {
		set type 0
	 }
	 if [info exists iclRec(file)] then {
		set file $iclRec(file)
	 } else {
		set file {}
	 }
	 for {set n 0} {$n < [llength $args]} {incr n} {
	   switch -- [lindex $args $n] {
		{-ce}	{incr n ; set ce [lindex $args $n]
			 iclPriv read $ce
			 set type $iclRec(type) ; set file $iclRec(file)}
		{-file}	{incr n ; set file [lindex $args $n]}
		{-type}	{incr n ; set type [lindex $args $n]}
		{-image}	{incr n ; set ih [lindex $args $n]}
		{-imid}		{incr n ; set ih [lindex $args $n]}
		{-imId}		{incr n ; set ih [lindex $args $n]}
		{default}	{set ih [lindex $args $n]}
	   }
	 }
	 if { $type == 1 } then {
	    if ![file exists $file] then {
		exec mkdir $file
	    }
	 }
	 return [imgPriv write $ih $file $type]
	}
{next}  	{eval imgPriv hash next}
{exists}  	{eval imgPriv hash exists $args}
{max}  		{eval imgPriv hash max $args}
{destroy}  	{set imId [lindex $args 0]
		 eval imgPriv destroy $imId
		 catch {rename img$imId {}}
		}
{defget}  	{if {[llength $args] != 2} then {
		   return -code error "wrong # args: defget imId defn" }
		 imgPriv definition get [lindex $args 0]
		 upvar 1 [lindex $args 1] defn
		 foreach n [array names imgDefn] {set defn($n) $imgDefn($n)}
		 return {}
		}
{defread}  	{if {[llength $args] != 2} then {
		   return -code error "wrong # args: defreadget file defn" }
		 imgPriv definition get [lindex $args 0]
		 upvar 1 [lindex $args 1] defn
		 foreach n [array names imgDefn] {set defn($n) $imgDefn($n)}
		 return {}
		}
{defset}  	{if {[llength $args] != 2} then {
		   return -code error "wrong # args: defset imId defn" }
		 upvar 1 [lindex $args 1] defn
		 foreach n [array names defn] {set imgDefn($n) $defn($n)}
		 imgPriv definition set [lindex $args 0] ; return {}
		}
{defputs}	{if {[llength $args] != 1} then {
		   return -code error "wrong # args: defputs defn" }
		 img_putsDefn $args
		}
{create}  	{if {[llength $args] != 1} then {
		   return -code error "wrong # args: create defn" }
		 upvar 1 [lindex $args 0] defn
		 foreach n [array names defn] {set imgDefn($n) $defn($n)}
		 return [imgPriv create]
		}
{collapse}	{if {[llength $args] != 1} then {
		   return -code error "wrong # args: collapse imId" }
		 set imId $args
		 imgPriv definition get $imId
		 for {set n 0} {$n < 4} {incr n} {
		    set d1 [lindex {x y z t} $n]
		    if { $imgDefn(${d1}dim) == 1 } {
			for {set m [expr $n + 1]} {$m < 4} {incr m} {
			    set d2 [lindex {x y z t} $m]
			    set imgDefn(${d1}dim) $imgDefn(${d2}dim) 
			    set imgDefn(${d1}1) $imgDefn(${d2}1) 
			    set imgDefn(${d1}2) $imgDefn(${d2}2) 
			    set d1 $d2
			}
			set imgDefn(tdim) 1
		    }
		 }
		 set imgDefn(ndims) 0
		 foreach d1 {x y z t} {
		    if { $imgDefn(${d1}dim) == 1 } {
			set imgDefn(${d1}1) 1 ; set imgDefn(${d1}2) 1
		    } else {
			incr imgDefn(ndims)
		    }
		 }
		 imgPriv definition set $imId
		}
{dimset}	{if {[llength $args] != 1} then {
		   return -code error "wrong # args: dimset imId" }
		 set imId $args
		 imgPriv definition get $imId
		 foreach d1 {x y z t} {
			set imgDefn(${d1}1) 1
			set imgDefn(${d1}2) $imgDefn(${d1}dim) 
		 }
		 imgPriv definition set $imId
		}
{default}	{puts "unknown option $option, should be:\
			\"read, write, next, exists, create, destroy,\
			definition, \""}
}

}

# procedure to display on stdout an image definition record
proc img_putsDefn { imId } {
    global imgDefn
    imgPriv definition get $imId
    puts "imId $imId ndata = $imgDefn(ndata) ndims = $imgDefn(ndims)"
    puts "imId $imId norm = $imgDefn(ndata) blank = $imgDefn(ndims)"
    puts "imId $imId x: dim = $imgDefn(xdim)  from $imgDefn(x1) to $imgDefn(x2)"
    puts "imId $imId y: dim = $imgDefn(ydim)  from $imgDefn(y1) to $imgDefn(y2)"
    puts "imId $imId z: dim = $imgDefn(zdim)  from $imgDefn(z1) to $imgDefn(z2)"
    puts "imId $imId t: dim = $imgDefn(tdim)  from $imgDefn(t1) to $imgDefn(t2)"
    puts "imId $imId v: dim = $imgDefn(vdim)  from $imgDefn(v1) to $imgDefn(v2)"
}



# simple display of images
proc img_display { n args } {
   global imgDefn

   imgPriv definition get $n
   pg clear screen
   pg window 1 $imgDefn(xdim) 1 $imgDefn(ydim)
   pg box
   set stats [img_anal $n statistics]
   set x1 [lindex $stats 0] ; set x2 [lindex $stats 1]
   for {set i 0} {$i < [llength $args]} {incr i} {
	switch -- [lindex $args $i] {
	   {-fg}	{incr i ; set x1 [lindex $args $i]}
	   {-bg}	{incr i ; set x1 [lindex $args $i]}
	}
   }
   pg image $n $x1 $x2 $args
}

proc img_movie { n } {
   global imgDefn
   img_image defget $n Defn
   for {set m $Defn(z1)} {$m <= $Defn(z2)} { } {
	set mm [img_geom $n subimage -z1 $m -z2 $m]
	img_display $mm
	img_image destroy $mm
	incr m
	set m [io_geti "Next (%d) from $Defn(z2) : " $m]
   }
}

# display support
proc img_TVregion { } {
  set c1 [pg cursor -mode 0]
  set c2 [pg cursor -mode 2 -xref [lindex $c1 0] -yref [lindex $c1 1]]
  return [list [lindex $c1 0] [lindex $c1 1] [lindex $c2 0] [lindex $c2 1]]
}
