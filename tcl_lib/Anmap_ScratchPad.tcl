#!/usr/local/bin/wish -f
#
# Anmap Scratch pad
#
# Copyright Paul Alexander (MRAO and LPB technology).
#
# This is the Anmap scratch pad to provide facilities for editing commands
# and sending them to Anmap.
#
# Version requirements:
#   Tcl 6.4 or greater
#   Tk  Version 3.1 or later
#   TclX 6.4c or later
#

# global declarations
       global textbuff current_file

# scratch-pad bindings:
# ---------------------
#
# Calling:      editor_bindings
# Use:          setup all bindings for the editor
#
proc Asp_Bindings { } {  

# Default Tk TEXT widget bindings are used except where overidden below

  Text_bindings
  Entry_bindings
}


# Actions
# =======
#
# Define actions for the editor window
#
# Asp_Send_selection ::::
#             send the current selected text as a command to Anmap
#
# Asp_Send_window ::::
#             send the current window contents as a command to Anmap

proc Asp_ExecSelection { w } {
  if {[catch {selection get} s]} then { } else {
    eval $s
  }
}

proc Asp_ExecWindow { w } {
  set textbuff [$w.text get 0.0 end] 
  eval $textbuff
}

proc Launch_ScratchPad { } {
  if ![winfo exists .asp] then {
     Anmap_ScratchPad
  }
}

proc Anmap_ScratchPad { } {

  global Anmap

# setup bindings in window
  Asp_Bindings 

# create an appropriate top-level window
  catch {destroy .asp}
  toplevel .asp
  wm minsize .asp 500 50
  wm geometry .asp 500x150
  wm title .asp "Anmap scratch pad"
  wm iconname .asp "Anmap scratch pad"
  wm iconbitmap .asp @$Anmap(src)/etc/bitmaps/asp_logo
  Asp_Clipboard .asp .asp
}

proc Asp_Clipboard { wtop w } {
  global Desktop
  global cb_buffer cb_current cb_max cb_names clipboard_type
  set clipboard_type asp
  set cb_current 0
  set cb_max 0
  set cb_buffer($cb_current) " "
  set type asp
  clipboard_init $type $wtop $w.text cb_buffer cb_current cb_max cb_names
  frame $w.bar
  pack append $w.bar \
     [menubutton $w.bar.file -relief raised -borderwidth 1 \
        -text "File" -menu $w.bar.file.m -width 6] \
        {left frame w filly} \
     [button $w.bar.new -relief raised -borderwidth 1 -width 6\
        -text "New" -command "clipboard_new $type"] \
        {left frame w filly} \
     [button $w.bar.sends -text "Execute selection" \
      -relief raised -borderwidth 1\
      -command "Asp_ExecSelection $w"] {left frame w} \
     [button $w.bar.sendw -text "Execute all" \
      -relief raised -borderwidth 1\
      -command "Asp_ExecWindow $w"] {left frame w} \
     [button $w.bar.previous -relief raised -borderwidth 1 -width 30\
        -bitmap "@$Desktop(bitmaps)/Left.xbm" \
        -text "Previous" -command "clipboard_previous $type"] \
        {left frame w} \
     [button $w.bar.next -relief raised -borderwidth 1 -width 30\
        -bitmap "@$Desktop(bitmaps)/Right.xbm" \
        -text "Next" -command "clipboard_next $type"] \
        {left frame w} \
     [label $w.bar.page -borderwidth 1 -width 6 -text "Page"] {left frame w} \
     [label $w.bar.pagen -borderwidth 1 -width 3 \
            -textvariable cb_current] {left frame w} \
     [label $w.bar.of -borderwidth 1 -width 2 -text "of"] {left frame w} \
     [label $w.bar.ofn -borderwidth 1 -width 3 \
            -textvariable cb_max] {left frame w} 


   menu $w.bar.file.m
        $w.bar.file.m add command -label "Load" \
                         -command "clipboard_load $type" 
        $w.bar.file.m add command -label "Save As" \
                         -command "clipboard_save $type"
        $w.bar.file.m add command -label "Clear" \
                         -command "$w.text delete 0.0 end"
        $w.bar.file.m add command -label "Close" \
                         -command "clipboard_close $type"

   text $w.text  -yscrollcommand "$w.scroll set" -background white
   scrollbar $w.scroll -relief sunken -command "$w.text yview" \
             -width 3m -foreground Grey -activeforeground Grey 

# make complete frame
  pack append $w $w.bar {top fillx pady 2} \
                      $w.scroll {right filly} \
                      $w.text {expand fill}

}










