#
# Cube example in tcl!!
#

proc drawcube { } {
  global Object
  vg object plot $Object
}


proc vinit { } {
  vg size 300 300
  vg stream open X11
  vg window -5 5 -5 5 9.0 -5.0
  vg lookat 0 0 12 0 0 0 0
}

proc animate { } {
  global dr dt t r Run Object
  set dr 10.0
  set dt 0.2
  vg backbuffer
  set t 0.0
  set Run 1
  set Object [makecube]
  doAnimate
}

proc doAnimate { } {
  global dr dt t r Run
  for {set r 0.0} {$r < 360.0} {set r [expr "$r + $dr"]} {
	vg clear
	vg pushmatrix
	  vg translate 0 0 $t
	  vg rotate y $r ; vg rotate z $r ; vg rotate x $r
	  drawcube
	vg popmatrix
	set t [expr "$t + $dt"]
	if { [expr "$t > 3.0"] || [expr "$t < -18.0"]} then {
	   set dt -$dt
	}
	vg swapbuffers
  }
  if { $Run } {after 10 doAnimate}
}

proc makecube { } {
  set object [vg object next]
  vg object open $object
	vg style -fill 1 -backface 1
	vg style -color 1
	vg makepoly
		vg move -1 -1  1
		vg draw  1 -1  1
		vg draw  1  1  1
		vg draw -1  1  1
		vg draw -1 -1  1
	vg closepoly
	vg style -color 2
	vg makepoly
		vg move  1 -1 -1
		vg draw -1 -1 -1
		vg draw -1  1 -1
		vg draw  1  1 -1
		vg draw  1 -1 -1
	vg closepoly
	vg style -color 3
	vg makepoly
		vg move 1 -1  1
		vg draw 1 -1 -1
		vg draw 1  1 -1
		vg draw 1  1  1
		vg draw 1 -1  1
	vg closepoly
	vg style -color 4
	vg makepoly
		vg move -1 -1  1
		vg draw -1  1  1
		vg draw -1  1 -1
		vg draw -1 -1 -1
		vg draw -1 -1  1
	vg closepoly
	vg style -color 5
	vg makepoly
		vg move  1  1  1
		vg draw  1  1 -1
		vg draw -1  1 -1
		vg draw -1  1  1
		vg draw  1  1  1
	vg closepoly
	vg style -color 6
	vg makepoly
		vg move -1 -1  1
		vg draw -1 -1 -1
		vg draw  1 -1 -1
		vg draw  1 -1  1
		vg draw -1 -1  1
	vg closepoly
  vg object close
  return $object
}
