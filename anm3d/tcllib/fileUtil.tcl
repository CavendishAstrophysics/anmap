proc file_fullName { file dir ext } {

# basic initialisation
   set HOME [glob ~]
# setup directory
   if ![string length $dir] then {set dir [pwd]}

# setup extension
   set e [file extension $file]
   if [string length $e] then {set ext [string range $e 1 1000]}

# setup file name
   set name [file root [file tail $file]]

# sort out a relative file name
   set d [file dirname $file]
   if {[string index $file 0] == "/"} then {
      set dir $d
   } else {
      if {[string range $file 0 2] == "../"} then {
	set d [string range $file 3 1000]
	set dir [file dirname $dir]
	return [file_fullName $d $dir $ext]
      } elseif {[string range $file 0 1] == "./"} then {
	set d [string range $file 2 1000]
	return [file_fullName $d $dir $ext]
      } elseif { "$d" != "." } then {
	set dir ${dir}/${d}
      }
   }
   if [string length $ext] then {
     return ${dir}/${name}.${ext}
   } else {
     return ${dir}/${name}
   }
}
