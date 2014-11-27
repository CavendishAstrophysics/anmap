proc Anmap_LTControl { } {
  global ltcopt ltcimmed AnmapSource
  catch {destroy .ltc}
  toplevel .ltc
  wm title .ltc "Lookup Table"
  wm iconname .ltc "Lookup Table"
  wm iconbitmap .ltc @$AnmapSource/etc/bitmaps/Anmap.xbm
  frame .ltc.sc
  frame .ltc.opt
  proc anm_LTCApply { args } {
   global ltcopt ltcimmed
   if [llength $args] then {
      if !$ltcimmed return
   }
   set rs [expr "[.ltc.rs get]/100.0"] ; set rp [expr "(([.ltc.rp get])/25.0)"];  
   set bs [expr "[.ltc.bs get]/100.0"] ; set bp [expr "(([.ltc.bp get])/25.0)"];  
   set gs [expr "[.ltc.gs get]/100.0"] ; set gp [expr "(([.ltc.gp get])/25.0)"];  
   set low [expr "[.ltc.low get]/100.0"]
   set high [expr "[.ltc.high get]/100.0"]
   case $ltcopt in {
    0  {anmap_exec map-display modify-lookup-table enhanced-colour  \
                   $rp $gp $bp $rs $gs $bs $low $high}
    1  {anmap_exec map-display modify-lookup-table standard-colour }
    2  {anmap_exec map-display modify-lookup-table rainbow $rp $gp $bp} 
    3  {anmap_exec map-display modify-lookup-table enhanced-grey  \
                   $rp $low $high}
    4  {anmap_exec map-display modify-lookup-table linear-grey}
    5  {anmap_exec map-display modify-lookup-table inverted-enhanced-grey  \
                   $rp $low $high}
    6  {anmap_exec map-display modify-lookup-table inverted-grey }
   }
  }
  set ltcopt 0
  set ltcimmed 0
  pack append .ltc.sc \
  [scale .ltc.rs -from 0 -to 100 -label "Red start" \
  -length 250 -orient horizontal -command anm_LTCApply] {top fill} \
  [scale .ltc.bs -from 0 -to 100 -label "Blue start"\
  -length 250 -orient horizontal -command anm_LTCApply] {top fill} \
  [scale .ltc.gs -from 0 -to 100 -label "Green start"\
  -length 250 -orient horizontal -command anm_LTCApply] {top fill} \
  [scale .ltc.rp -from 0 -to 100 -label "Red/Grey power"\
  -length 250 -orient horizontal -command anm_LTCApply] {top fill} \
  [scale .ltc.bp -from 0 -to 100 -label "Blue power"\
  -length 250 -orient horizontal -command anm_LTCApply] {top fill} \
  [scale .ltc.gp -from 0 -to 100 -label "Green power"\
  -length 250 -orient horizontal -command anm_LTCApply] {top fill} \
  [scale .ltc.low -from 0 -to 100 -label "Low"\
  -length 250 -orient horizontal -command anm_LTCApply] {top fill} \
  [scale .ltc.high -from 0 -to 100 -label "High"\
  -length 250 -orient horizontal -command anm_LTCApply] {top fill}
  pack append .ltc.opt \
  [button .ltc.apply -text Apply -command anm_LTCApply] \
   {top fillx frame n padx 5} \
  [button .ltc.exit -text Dismiss -command {destroy .ltc}] \
   {top fillx pady 10 frame n padx 5} \
  [radiobutton .ltc.op0 -text "Enhanced colour" -variable ltcopt -value 0 \
   -anchor w -command anm_LTCApply] {top fillx pady 5 frame n padx 5} \
  [radiobutton .ltc.op1 -text "Standard colour" -variable ltcopt -value 1 \
    -anchor w -command anm_LTCApply] {top fillx pady 5 frame n padx 5} \
  [radiobutton .ltc.op2 -text "Rainbow colour" -variable ltcopt -value 2 \
    -anchor w -command anm_LTCApply] {top fillx pady 5 frame n padx 5} \
  [radiobutton .ltc.op3 -text "Enhanced greyscale" -variable ltcopt -value 3 \
    -anchor w -command anm_LTCApply] {top fillx pady 5 frame n padx 5} \
  [radiobutton .ltc.op4 -text "Linear greyscale" -variable ltcopt -value 4 \
    -anchor w -command anm_LTCApply] {top fillx pady 5 frame n padx 5} \
  [radiobutton .ltc.op5 -text "Inverted enhanced" -variable ltcopt -value 5 \
    -anchor w -command anm_LTCApply] {top fillx pady 5 frame n padx 5} \
  [radiobutton .ltc.op6 -text "Inverted linear" -variable ltcopt -value 6 \
    -anchor w -command anm_LTCApply] {top fillx pady 5 frame n padx 5} \
  [radiobutton .ltc.im0 -text "Deferred Update" -variable ltcimmed -value 0 \
    -anchor w -command anm_LTCApply] {top fillx pady 10 frame s padx 5} \
  [radiobutton .ltc.im1 -text "Immediate Update" -variable ltcimmed -value 1 \
    -anchor w -command anm_LTCApply] {top fillx frame s padx 5} \
  [frame .ltc.cmap -relief sunken -borderwidth 1] \
    {top fillx pady 10 frame s padx 5}
  pack append .ltc.cmap \
   [frame .ltc.cmap.dump -relief flat] {pady 5 padx 5 top} \
   [frame .ltc.cmap.load -relief flat] {pady 5 padx 5 top} \
   [button .ltc.cmap.save -relief raised -text "Save to file" \
           -width 15 -command "cmap save"] {pady 2 padx 5 top } \
   [button .ltc.cmap.restore -relief raised -text "Load from file" \
           -width 15 -command "cmap restore"] {pady 2 padx 5 top }
   pack append .ltc.cmap.dump \
     [button .ltc.cmap.dump.b -text "Save as: " -width 10 -relief flat \
      -command anm_LTCdump -anchor w] {left frame w} \
     [entry .ltc.cmap.dump.e  -width 10 -relief sunken ] {left frame w}
   pack append .ltc.cmap.load \
     [button .ltc.cmap.load.b -text "Load from: " -width 10 -relief flat \
      -command anm_LTCload -anchor w] {left frame w} \
     [entry .ltc.cmap.load.e  -width 10 -relief sunken ] {left frame w}
   bind .ltc.cmap.dump.e <Return> { anm_LTCdump }
   bind .ltc.cmap.load.e <Return> { anm_LTCload }
   .ltc.cmap.dump.e insert 0 default
   .ltc.cmap.load.e insert 0 default
   .ltc.rs set 0 ; .ltc.gs set 40 ; .ltc.bs set 60
   .ltc.rp set 25 ; .ltc.gp set 25 ; .ltc.bp set 25
   .ltc.low set 0 ; .ltc.high set 100
  pack append .ltc .ltc.sc {left filly} .ltc.opt {left padx 10 filly}
  anm_LTCApply
  proc anm_LTCdump { } {cmap dump -name [.ltc.cmap.dump.e get]}
  proc anm_LTCload { } {cmap load -name [.ltc.cmap.load.e get]}
}




