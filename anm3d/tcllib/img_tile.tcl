
proc img_tileXY { imid args } {

# get dimensions of supplied image
  img_image defget $imid defn

# parse options
  foreach n {x1 x2 y1 y2 z1 z2} { set $n $defn($n) } ; set ic 1
  for {set n 0} {$n < [llength $args]} {incr n} {
	switch -- [lindex $args $n] {
	{-x1}	{incr n; set x1 [lindex $args $n]}
	{-x2}	{incr n; set x2 [lindex $args $n]}
	{-y1}	{incr n; set y1 [lindex $args $n]}
	{-y2}	{incr n; set y2 [lindex $args $n]}
	{-z1}	{incr n; set z1 [lindex $args $n]}
	{-z2}	{incr n; set z2 [lindex $args $n]}
	{-incr}	{incr n; set ic [lindex $args $n]}
	}
  }

# calculate geometry for output image
  set xsize [expr $x2 - $x1 + 1] ; set ysize [expr $y2 - $y1 + 1]
  set ntile [expr ($z2 - $z1)/$ic + 1]
  set nx [expr round( sqrt( ${ntile}.0 ) )]
  set ny [expr round($ntile/$nx)]
  if { [expr $nx * $ny] < $ntile} {incr ny}
  set defn(xdim) [expr $nx * $xsize]; set defn(x1) 1; set defn(x2) $defn(xdim)
  set defn(ydim) [expr $ny * $ysize]; set defn(y1) 1; set defn(y2) $defn(ydim)
  set defn(zdim) 1; set defn(z1) 1; set defn(z2) 1
  set defn(ndims) 2
  set defn(ndata) [expr $defn(xdim) * $defn(ydim) * $defn(tdim) * $defn(vdim)]
  set im [img_image create defn]

# loop and populate the new image
  for {set n 0; set zz $z1} {$n < $ntile} {incr n; incr zz $ic} {
	set ix [expr $n % $ny]; set iy [expr $n / $nx]
	set xx [expr $ix * $xsize + 1]; set yy [expr $iy * $ysize + 1]
	set ime [img_geom $imid subimage \
		 -x1 $x1 -x2 $x2 -y1 $y1 -y2 $y2 -z1 $zz -z2 $zz]
	img_apply $im \
	   -x1 $xx -x2 [expr $xx + $xsize - 1] \
	   -y1 $yy -y2 [expr $yy + $ysize - 1] == $ime
	img_image destroy $ime
   }

   return $im
}
