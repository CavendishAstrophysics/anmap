#
# define a line-style widget
#
proc linestyle { w args } {
 global AnmapSource
 set bm $AnmapSource/etc/bitmaps
 # sort out options
   set variable linestyle
   set text "" ; set width "" ; set textwidth ""
   for {set n 0} {$n < [llength $args]} {incr n} {
     set name [lindex $args $n]
     switch -- $name {
       {default}    {puts stderr "Unknown option to linestyle}
       {-text}      {incr n ; set text [lindex $args $n]}
       {-variable}  {incr n ; set variable [lindex $args $n]}
       {-width}     {incr n ; set width [lindex $args $n]}
       {-textwidth} {incr n ; set textwidth [lindex $args $n]}
     }
   }
   upvar #0 $variable linestyle

 # construct line-style widget
   set linestyle {1 1 white}
   frame $w
   label $w.l -text $text -relief flat -borderwidth 1 -anchor w
   menubutton $w.ls -menu $w.ls.m -bitmap @$bm/line11.xbm \
                    -foreground white -background black \
                    -relief raised -borderwidth 1 -height 10
   if [string length $textwidth] then {
     $w.l configure -width $textwidth
   }
   if [string length $width] then {
     $w.ls configure -width $width
   }
   menu $w.ls.m ; menu $w.ls.ls ; menu $w.ls.lw ; menu $w.ls.lc
   $w.ls.m add cascade -label "Line style"  -menu $w.ls.ls
   $w.ls.m add cascade -label "Line width"  -menu $w.ls.lw
   $w.ls.m add cascade -label "Line colour" -menu $w.ls.lc
     $w.ls.ls add command -bitmap @$bm/line11.xbm \
                 -command "linestyle_update $w $variable -ls 1"
     $w.ls.ls add command -bitmap @$bm/line21.xbm \
                 -command "linestyle_update $w $variable -ls 2"
     $w.ls.ls add command -bitmap @$bm/line31.xbm \
                 -command "linestyle_update $w $variable -ls 3"
     $w.ls.ls add command -bitmap @$bm/line41.xbm \
                 -command "linestyle_update $w $variable -ls 4"
     $w.ls.ls add command -bitmap @$bm/line51.xbm \
                 -command "linestyle_update $w $variable -ls 5"
     $w.ls.lw add command -bitmap @$bm/line11.xbm \
                 -command "linestyle_update $w $variable -lw 1"
     $w.ls.lw add command -bitmap @$bm/line12.xbm \
                 -command "linestyle_update $w $variable -lw 2"
     $w.ls.lw add command -bitmap @$bm/line13.xbm \
                 -command "linestyle_update $w $variable -lw 3"
     $w.ls.lw add command -bitmap @$bm/line14.xbm \
                 -command "linestyle_update $w $variable -lw 4"
     $w.ls.lw add command -bitmap @$bm/line15.xbm \
                 -command "linestyle_update $w $variable -lw 5"
     $w.ls.lc add command -label white \
                 -command "linestyle_update $w $variable -lc white"
     $w.ls.lc add command -label black \
                 -command "linestyle_update $w $variable -lc black"
     $w.ls.lc add command -label red \
                 -command "linestyle_update $w $variable -lc red"
     $w.ls.lc add command -label green \
                 -command "linestyle_update $w $variable -lc green"
     $w.ls.lc add command -label blue \
                 -command "linestyle_update $w $variable -lc blue"
     $w.ls.lc add command -label magenta \
                 -command "linestyle_update $w $variable -lc magenta"
     $w.ls.lc add command -label yellow \
                 -command "linestyle_update $w $variable -lc yellow"
     $w.ls.lc add command -label orange \
                 -command "linestyle_update $w $variable -lc orange"

  # pack widget
  pack append $w $w.l {left frame w} $w.ls {left fillx }
}

proc linestyle_update { w variable args } {
 global AnmapSource
 set bm $AnmapSource/etc/bitmaps
 upvar #0 $variable linestyle
 # sort out options
   for {set n 0} {$n < [llength $args]} {incr n} {
     set name [lindex $args $n]
     switch -- $name {
       {-ls}  {incr n ; set ls [lindex $args $n]
               set linestyle [lreplace $linestyle 0 0 $ls]}
       {-lw}  {incr n ; set lw [lindex $args $n]
               set linestyle [lreplace $linestyle 1 1 $lw]}
       {-lc}  {incr n ; set lc [lindex $args $n]
               set linestyle [lreplace $linestyle 2 2 $lc]}
     }
   }

 # update widget
   set ls [lindex $linestyle 0]
   set lw [lindex $linestyle 1]
   set lc [lindex $linestyle 2]
   $w.ls configure -bitmap @$bm/line${ls}${lw}.xbm \
        -foreground $lc -background black
}


#
# define a text-style widget
#
proc textstyle { w args } {
 global AnmapSource
 set bm $AnmapSource/etc/bitmaps
 # sort out options
   set variable textstyle
   set text "" ; set width "" ; set textwidth ""
   for {set n 0} {$n < [llength $args]} {incr n} {
     set name [lindex $args $n]
     switch -- $name {
       {default}    {puts stderr "Unknown option to textstyle}
       {-text}      {incr n ; set text [lindex $args $n]}
       {-variable}  {incr n ; set variable [lindex $args $n]}
       {-width}     {incr n ; set width [lindex $args $n]}
       {-textwidth} {incr n ; set textwidth [lindex $args $n]}
     }
   }
   upvar #0 $variable textstyle

 # construct text-style widget
   set textstyle {1 1.0 white 1}
   frame $w
   label $w.l  -text $text -relief flat -borderwidth 1 -anchor w
   label $w.ls -text "100%" -relief flat -width 4
   menubutton $w.ts -menu $w.ts.m -bitmap @/mrao/anmap_v7.5/etc/bitmaps/text11.xbm \
                    -foreground white -background black \
                    -relief raised -borderwidth 1 -height 20 -width 80
   if [string length $textwidth] then {
     $w.l configure -width $textwidth
   }
   if [string length $width] then {
     $w.ts configure -width $width
   }
   menu $w.ts.m ; menu $w.ts.tf ; menu $w.ts.ts ; menu $w.ts.tc ; menu $w.ts.tw
   $w.ts.m add cascade -label "Text font"   -menu $w.ts.tf
   $w.ts.m add cascade -label "Text size"   -menu $w.ts.ts
   $w.ts.m add cascade -label "Text colour" -menu $w.ts.tc
   $w.ts.m add cascade -label "Text width"  -menu $w.ts.tw
     $w.ts.tw add command -bitmap @$bm/text11.xbm \
                 -command "textstyle_update $w $variable -tw 1"
     $w.ts.tw add command -bitmap @$bm/text12.xbm \
                 -command "textstyle_update $w $variable -tw 2"
     $w.ts.tw add command -bitmap @$bm/text13.xbm \
                 -command "textstyle_update $w $variable -tw 3"
     $w.ts.tw add command -bitmap @$bm/text14.xbm \
                 -command "textstyle_update $w $variable -tw 4"
     $w.ts.tw add command -bitmap @$bm/text15.xbm \
                 -command "textstyle_update $w $variable -tw 5"

     $w.ts.tf add command  -bitmap @$bm/text11.xbm \
                 -command "textstyle_update $w $variable -tf 1"
     $w.ts.tf add command  -bitmap @$bm/text21.xbm \
                 -command "textstyle_update $w $variable -tf 2"
     $w.ts.tf add command  -bitmap @$bm/text31.xbm \
                 -command "textstyle_update $w $variable -tf 3"
     $w.ts.tf add command  -bitmap @$bm/text41.xbm \
                 -command "textstyle_update $w $variable -tf 4"

     $w.ts.ts add command -label "100%" \
                 -command "textstyle_update $w $variable -ts 1.00"
     $w.ts.ts add command -label "125%" \
                 -command "textstyle_update $w $variable -ts 1.25"
     $w.ts.ts add command -label "150%" \
                 -command "textstyle_update $w $variable -ts 1.50"
     $w.ts.ts add command -label "175%" \
                 -command "textstyle_update $w $variable -ts 1.75"
     $w.ts.ts add command -label "200%" \
                 -command "textstyle_update $w $variable -ts 2.00"
     $w.ts.ts add command -label "250%" \
                 -command "textstyle_update $w $variable -ts 2.50"
     $w.ts.ts add command -label "300%" \
                 -command "textstyle_update $w $variable -ts 3.00"
     $w.ts.ts add command -label "75%" \
                 -command "textstyle_update $w $variable -ts 0.75"
     $w.ts.ts add command -label "50%" \
                 -command "textstyle_update $w $variable -ts 0.50"
     $w.ts.ts add command -label "25%" \
                 -command "textstyle_update $w $variable -ts 0.25"
     $w.ts.ts add command -label "Other ..." \
                 -command "textstyle_size $w $variable"

     $w.ts.tc add command -label white \
                 -command "textstyle_update $w $variable -tc white"
     $w.ts.tc add command -label black \
                 -command "textstyle_update $w $variable -tc black"
     $w.ts.tc add command -label red \
                 -command "textstyle_update $w $variable -tc red"
     $w.ts.tc add command -label green \
                 -command "textstyle_update $w $variable -tc green"
     $w.ts.tc add command -label blue \
                 -command "textstyle_update $w $variable -tc blue"
     $w.ts.tc add command -label magenta \
                 -command "textstyle_update $w $variable -tc magenta"
     $w.ts.tc add command -label yellow \
                 -command "textstyle_update $w $variable -tc yellow"
     $w.ts.tc add command -label orange \
                 -command "textstyle_update $w $variable -tc orange"

  # pack widget
  pack append $w $w.l {left frame w}   \
                 $w.ls {left frame w}  $w.ts {left fillx }
}

proc textstyle_update { w variable args } {
 global AnmapSource
 set bm $AnmapSource/etc/bitmaps
 upvar #0 $variable textstyle
 # sort out options
   for {set n 0} {$n < [llength $args]} {incr n} {
     set name [lindex $args $n]
     switch -- $name {
       {-tf}  {incr n ; set tf [lindex $args $n]
               set textstyle [lreplace $textstyle 0 0 $tf]}
       {-ts}  {incr n ; set ts [lindex $args $n]
               set textstyle [lreplace $textstyle 1 1 $ts]}
       {-tc}  {incr n ; set tc [lindex $args $n]
               set textstyle [lreplace $textstyle 2 2 $tc]}
       {-tw}  {incr n ; set tw [lindex $args $n]
               set textstyle [lreplace $textstyle 3 3 $tw]}
     }
   }

 # update widget
   set tf [lindex $textstyle 0]
   set ts [lindex $textstyle 1]
   set tc [lindex $textstyle 2]
   set tw [lindex $textstyle 3]
   $w.ts configure -bitmap @$bm/text${tf}${tw}.xbm \
        -foreground $tc -background black
   $w.ls configure -text "[expr round(100*$ts)]%"
}


