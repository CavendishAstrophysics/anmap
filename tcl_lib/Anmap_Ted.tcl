#
#
# Launch an Anmap Specific Ted Session
# 

proc Anmap_LaunchTed { w } {
  catch {destroy $w}
  global ted
  toplevel $w
  wm title $w "Anmap Editor Commands"
  wm iconname $w "Anmap Editor Commands"

  set ted {}
  pack [frame $w.head -relief flat] -side top -fill x -padx 2 -pady 4
  pack [button $w.head.exit -text "Exit Editor" \
        -width 16 -command "anmap_TedExit $w"] -side left -fill x
  pack [frame $w.bar -relief flat] -side top -fill x -padx 2 -pady 4
  pack [button $w.bar.ex1 -text "Execute All" \
        -width 16 -command anmap_TedExecAll] -side left -fill x
  pack [button $w.bar.ex2 -text "Execute Selection" \
        -width 16 -command anmap_TedExecSel] -side left -fill x

  exec /soft/tcl/bin/ted -title "Anmap Editor" \
       -command "Edit_InformApplic \{[winfo name .]\}" &
  set ted $ted
}

proc anmap_TedExecAll { } {
  global ted
  if [string length $ted] then {
    set text [send $ted Edit_TextGet .editor.text]
    eval $text
  }
}

proc anmap_TedExecSel { } {
  global ted
  catch {selection get} text
  eval $text
}


proc anmap_TedExit { w } {
  global ted
  catch {send $ted exit}
  destroy $w
}

