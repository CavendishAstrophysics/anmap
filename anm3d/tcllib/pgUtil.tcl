#
# Tcl support routines for A-graphics
#

# convert a named colour to a colour index
proc pg_col2ci { col } {
   set ci [lsearch -glob {black white red green blue cyan magenta yellow \
                          orange lawngreen springgreen royalblue slateblue\
                          purple darkgrey lightgrey} ${col}*]
   if {$ci < 0} then {
     if [text_cint $col] then {
        return $col
     } else {
        return 1
     }
   } else {
     return $ci
   }
}

# convert a colour index to a named colour
proc pg_ci2col { ci } {
   set col white
   if [text_cint $ci] then {
     set col [lindex {black white red green blue cyan magenta yellow \
                          orange lawngreen springgreen royalblue slateblue\
                          purple darkgrey lightgrey} $ci]
     if ![string length $col] then {set col $ci}
   } else {
     set i [lsearch -glob {black white red green blue cyan magenta yellow \
                           orange lawngreen springgreen royalblue slateblue\
                           purple darkgrey lightgrey} ${ci}*]
     if { $i > 0 } then {
        set col [lindex {black white red green blue cyan magenta yellow \
                           orange lawngreen springgreen royalblue slateblue\
                           purple darkgrey lightgrey} $i]
     }
   }
   return $col
}

# check that the text string is an integer
proc text_cint { text } {
  if [regexp {[1234567890]} $text] then {
    if ![regexp -nocase {[!-/,:-~]} $text] then {
       return 1
    }
  }
  return 0
}
