#
# Image analysis and display procedures
#

proc image_show_marks { opt } {
  global image_opt_mark
  set image_opt_mark $opt
  return ""
}

proc image_position { } {
  global u v map_value gr_char gr_x gr_y map_u map_v
  map-display get map-read
  return [list $map_u $map_v]
}

proc image_pixel { } {
  global u v map_value gr_char gr_x gr_y map_u map_v
  map-display get map-read
  return [list $u $v]
}

proc image_value { } {
  global u v map_value gr_char gr_x gr_y map_u map_v
  map-display get map-read
  return $map_value
}

proc image_length { } {
  global u v map_value gr_char gr_x gr_y map_u map_v image_opt_mark
  map-display get map-read
  set u1 $map_u ; set v1 $map_v
  map-display get map-read
  if $image_opt_mark then {image_draw_line $u1 $v1 $map_u $map_v}
  return [expr "sqrt(($map_u-$u1)*($map_u-$u1) +($map_v-$v1)*($map_v-$v1))"]
}

proc image_angle { } {
  global u v map_value gr_char gr_x gr_y map_u map_v image_opt_mark
  map-display get map-read
  set u1 $map_u ; set v1 $map_v
  map-display get map-read
  set u0 $map_u ; set v0 $map_v
  if $image_opt_mark then {image_draw_line $u1 $v1 $u0 $v0}
  map-display get map-read
  set u2 $map_u ; set v2 $map_v
  if $image_opt_mark then {image_draw_line $u0 $v0 $u2 $v2}
  set rr1 [expr "(($u0-$u1)*($u0-$u1) +($v0-$v1)*($v0-$v1))"]
  set rr2 [expr "(($u0-$u2)*($u0-$u2) +($v0-$v2)*($v0-$v2))"]
  set ll [expr "(($u1-$u2)*($u1-$u2) +($v1-$v2)*($v1-$v2))"]
  set c0 [expr "($rr1 + $rr2 - $ll)/(2.0*sqrt($rr1)*sqrt($rr2))"]
  if "[expr abs($c0)] > 1.0" then {set c0 1.0}
  return [expr "acos($c0)*180.0/3.14159265"]
}

proc image_draw_line { u1 v1 u2 v2 } {
  map-display annotate set-plot-options temporary
  map-display annotate set-plot-options command-input
  map-display annotate line-draw $u1 $v1 $u2 $v2
}

proc image_enq { image opt } {
  global iclRec
  set res {}
  switch -- $opt {
    {uv-range}   {
                  iclPriv read $image
                  set res [list $iclRec(u1) $iclRec(u2) $iclRec(v1) $iclRec(v2)]
                 }
    {range}      {
                  iclPriv read $image
                  set res [list $iclRec(u1) $iclRec(u2) $iclRec(v1) $iclRec(v2)]
                 }
    {source}     {iclPriv read $image ; set res $iclRec(source)}
    {program}     {iclPriv read $image ; set res $iclRec(program)}
  }
  return $res
}

