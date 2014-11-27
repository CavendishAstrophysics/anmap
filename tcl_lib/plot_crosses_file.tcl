proc plot_crosses_file { file {cr 1} } {
  annotate
    set-plot temporary
    set f [open $file r]
    set docross 1
    while { $docross } {
	set i [gets $f line]
	if { $i > 0 } then {
		cross-draw [lindex $line 0] [lindex $line 1] $cr
	} else {
	   if { $i < 0 } then {
		set docross 0
	   }
	}
    }
    close $f
  exit
}
