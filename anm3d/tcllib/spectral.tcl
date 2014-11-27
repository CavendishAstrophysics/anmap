proc read_spectrum { file xc yc xl yl } {
  upvar 1 $xl x
  upvar 1 $yl y
  catch {unset x ; unset y}
  set fid [open $file r]
  set line [gets $fid]
  while {[string length $line]} {
    set ll [split $line " "]
    case [lindex $ll 0] in {
      {%ndata}  {  set ndata [lindex $line 1]  }
      {%ncols}  {  set ncols [lindex $line 1]  }
      {default} {  if ![string match [string index $line 0] "%"] then {
                     lappend x [lindex $line [expr "$xc-1"]]
                     lappend y [lindex $line [expr "$yc-1"]] 
                   }
                }
    }
    set line ""
    set line [gets $fid]
  }
}

