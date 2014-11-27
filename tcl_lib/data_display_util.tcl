#
# Standard procedures for simplified display of graphs
#
proc graph_display { args } {

# define default options
  set line(1) 1       ; set line(2) 1     ; set line(3) 1      ; set line(4) 1
  set colour(1) white ; set colour(2) red ; set colour(3) blue ; set colour(4) green
  set line(5) 2       ; set line(6) 2     ; set line(7) 2      ; set line(8) 2
  set colour(5) white ; set colour(6) red ; set colour(7) blue ; set colour(8) green
  set xc 1 ; set yc 2
  set text ""
  set xrange "" ; set yrange ""
  set title "" ; set ytitle "" ; set xtitle ""
  set file(1) ""
  set function "" 
  set key 0 ; set histogram 0 ; set scatter 0 ; set lines 0 
  set type linlin
  for {set n 0} {$n < [llength $args]} {incr n} {
    set name [lindex $args $n]
    switch -- $name {
      {-type}      {incr n; set type [lindex $args $n]}
      {-histogram} {set histogram 1}
      {-scatter}   {set scatter 1}
      {-lines}     {set lines 1}
      {-xc}        {incr n; set xc [lindex $args $n]}
      {-yc}        {incr n; set yc [lindex $args $n]}
      {-file}      {incr n; set file(1) [lindex $args $n]}
      {-file1}     {incr n; set file(1) [lindex $args $n]}
      {-file2}     {incr n; set file(2) [lindex $args $n]}
      {-file3}     {incr n; set file(3) [lindex $args $n]}
      {-file4}     {incr n; set file(4) [lindex $args $n]}
      {-file5}     {incr n; set file(5) [lindex $args $n]}
      {-file6}     {incr n; set file(6) [lindex $args $n]}
      {-file7}     {incr n; set file(7) [lindex $args $n]}
      {-file8}     {incr n; set file(8) [lindex $args $n]}
      {-function}  {incr n; set function [lindex $args $n]}
      {-xrange}   {incr n; set xrange [lindex $args $n]}
      {-yrange}   {incr n; set yrange [lindex $args $n]}
      {-title}    {incr n; set title  [lindex $args $n]}
      {-xtitle}   {incr n; set xtitle [lindex $args $n]}
      {-ytitle}   {incr n; set ytitle [lindex $args $n]}
      {-key}      {set key 1}
      {default}   {set text "$text $name"}
    }
  }
  iocmd clear-cli
  if [string length $text] then {
    iocmd set-cli $text
  }
  if ![llength $file(1)] then {
    set file(1) [iocmd get-word 'Data-file-name : ' 'anmap_results.dat']
  }
  data-display
    initialise plot
    initialise all
    set-frame-style text-style 2 2 white 1
    set-text-style 2 2 white 1
    set-title-options title 1 1.5 0.5 0.5 
    set-title-options x-title 1 2.5 0.5 0.5 
    set-title-options y-title 1 2.5 0.5 0.5 
    foreach n [array names file] {
      line $n file $file($n)
      line $n x-col $xc
      line $n y-col $yc
      line $n line-style $line($n) 1 $colour($n)
      switch $type {
        {linlin}  {} 
        {linlog}  {line $n y-log-scale on} 
        {loglin}  {line $n x-log-scale on} 
        {loglog}  {line $n x-log-scale on ; line $n y-log-scale on}
        {default} {}
      } 
      if $key then {line $n key-display 1}
      if $scatter then {
         line $n line-type 0 
         line $n symbol-type [expr $n+1] 
         line $n symbol-style 1 2 colour($n) 1
      }
      if $histogram then {line $n line-type 2}
      if $lines then {line $n line-type 1}
    }
    switch $type {
      {linlin}  {} 
      {linlog}  {set-frame log-y-labels on} 
      {loglin}  {set-frame log-x-labels on} 
      {loglog}  {set-frame log-x-labels on ; set-frame log-y-labels on}
      {default} {}
    } 
    if [string length $function] then { eval line 9 function '$function' }
    if [string length $xrange] then { eval x-range $xrange }
    if [string length $yrange] then { eval y-range $yrange }
    if [string length $title] then { eval title $title }
    if [string length $xtitle] then { eval x-title $xtitle }
    if [string length $ytitle] then { eval y-title $ytitle }
    if $key then { 
      set-key on ; set-key frame on 
      set-key text-style 1 1.25 white 1 1.0
    }
    plot all
}


