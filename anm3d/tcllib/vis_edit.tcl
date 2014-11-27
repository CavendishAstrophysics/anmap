proc vis_edit { } {
  global Vis3d Desktop Ted cb_CW

  set w .edit
  catch {destroy $w}
  toplevel $w
  wm title $w "Vis3d Procedure Editor"
  wm iconname $w "Vis3d Procedure Editor"
  wm iconbitmap $w @$Vis3d(src)/vis3d.xbm

# create editor widget for this tool
  set wt [edit_editor $w.edit -width 80 -height 20 -init 1 \
          -relief sunken -borderwidth 2]

# create command bar
  frame $w.bar0 -relief raised -borderwidth 2
  pack [menubutton $w.bar0.file -relief flat -borderwidth 2 \
        -text "File" -menu $w.bar0.file.m -width 8] \
        -side left -anchor w
   menu $w.bar0.file.m
        $w.bar0.file.m add command -label "Load" \
                          -command "cb_clipboardNew $wt ; editor_Load $wt" 
        $w.bar0.file.m add command -label "Save As" \
                          -command "editor_SaveAs $wt"
        $w.bar0.file.m add separator
        $w.bar0.file.m add command -label "Clear Current" \
                          -command "cb_clipboardClear $wt"
        $w.bar0.file.m add command -label "Clear All" \
                          -command "cb_clipboardClearAll $wt"
        $w.bar0.file.m add separator
        $w.bar0.file.m add command -label "Insert X selection" \
                          -command "cb_XSet PRIMARY"
        $w.bar0.file.m add command -label "Append X selection" \
                          -command "cb_XAppend PRIMARY"
        $w.bar0.file.m add command -label "Insert X clipboard" \
                          -command "cb_XSet CLIPBOARD"
        $w.bar0.file.m add command -label "Append X clipboard" \
                          -command "cb_XAppend CLIPBOARD"
        $w.bar0.file.m add separator
        $w.bar0.file.m add command -label "Search" \
                          -command "Edit_WindowSearch $wt"
        $w.bar0.file.m add command -label "Print" \
                          -command "Edittool_Print $wt"
        $w.bar0.file.m add separator
        $w.bar0.file.m add command -label "Dismiss" \
                          -command "cb_clipboardClose $wt"
  frame $w.bar
  pack [button $w.bar.run -relief raised -borderwidth 2 -width 8\
        -text "Run" -command "vised_run $wt"] \
        -side left -anchor w -padx 4
  pack [button $w.bar.new -relief raised -borderwidth 2 -width 8\
        -text "New" -command "cb_clipboardNew $wt"] \
        -side left -anchor w -padx 4
  pack [label $w.bar.l1 -relief flat -text {} -width 8] \
        -side left -anchor w
  pack [button $w.bar.previous -relief raised -borderwidth 2 -width 30\
        -bitmap "@$Desktop(bitmaps)/Left.xbm" \
        -text "Previous" -command "cb_clipboardPrev $wt"] \
        -side left -anchor w
  pack [button $w.bar.next -relief raised -borderwidth 2 -width 30\
        -bitmap "@$Desktop(bitmaps)/Right.xbm" \
        -text "Next" -command "cb_clipboardNext $wt"] \
        -side left -anchor w
  pack [label $w.bar.page -borderwidth 1 -width 6 -text "Page"]  \
        -side left -anchor w
  pack [label $w.bar.pagen -borderwidth 1 -width 3 \
        -textvariable $wt.current_buffer] -side left -anchor w
  pack [label $w.bar.of -borderwidth 1 -width 2 -text "of"]  \
        -side left -anchor w
  pack [label $w.bar.ofn -borderwidth 1 -width 3 -textvariable $wt.buffers] \
        -side left -anchor w
  pack [button $w.bar.up -relief raised -borderwidth 2 -width 30\
        -bitmap "@$Desktop(bitmaps)/up.xbm" \
        -command "Text_MoveUppage $wt"] \
        -side left -anchor w
  pack [button $w.bar.down -relief raised -borderwidth 2 -width 30\
        -bitmap "@$Desktop(bitmaps)/down.xbm" \
        -command "Text_MoveDownpage $wt"] \
        -side left -anchor w
  pack [button $w.bar.top -relief raised -borderwidth 2 -width 30\
        -bitmap "@$Desktop(bitmaps)/top.xbm" \
        -command "Text_MoveTop $wt"] \
        -side left -anchor w
  pack [button $w.bar.bottom -relief raised -borderwidth 2 -width 30\
        -bitmap "@$Desktop(bitmaps)/bottom.xbm" \
        -command "Text_MoveBottom $wt"] \
        -side left -anchor w



# make complete frame
  pack $w.bar0 -side top -fill x -pady 2 -padx 2
  pack $w.bar -side top -fill x -pady 2 -padx 2
  pack $w.edit -side top -fill both -expand 1

# setup text
  edit_Setup $wt
  set cb_CW $wt

# return widget name
  return $w
}

proc vised_run { wt } {
   vised_output [eval [$wt get 0.0 end]]
}

proc vised_output { text } {
   if ![winfo exists .res] then {vised_result .res}
   .res.txt.text insert end $text
   .res.txt.text insert end \n
   .res.txt.text see insert
}

proc vised_result { w } {
  global Vis3d Desktop

  catch {destroy $w}
  toplevel $w
  wm title $w "Vis3d Results"
  wm iconname $w "Vis3d Results"
  wm iconbitmap $w @$Vis3d(src)/vis3d.xbm

  pack [frame $w.bar -relief flat] \
	-side top -fill x -padx 4 -pady 4
  pack [button $w.bar.dismiss -text Dismiss -command "destroy $w" \
	-width 8 -borderwidth 2 -relief raised] \
	-side left -anchor w -padx 4 -pady 2
  pack [button $w.bar.clear -text Clear \
	-command "$w.txt.text delete 0.0 end" \
	-width 8 -borderwidth 2 -relief raised] \
	-side left -anchor w -padx 4 -pady 2
  pack [button $w.bar.save -text "Save As" \
	-command {fileselect .fs -command vised_save -centre \
		  -title "Vis3d FileSelect"} \
	-width 8 -borderwidth 2 -relief raised] \
	-side left -anchor w -padx 4 -pady 2

  pack [frame $w.txt -relief groove -borderwidth 2] \
	-side top -fill both -padx 4 -pady 4

# create and pack text window
   set width 80 ; set height 20
   text $w.txt.text  -yscrollcommand "$w.txt.scroll set" -background white \
                 -font -*-courier-medium-r-*-12-* -setgrid true \
		 -width $width -height $height
   $w.txt.text tag configure sel -relief flat \
          -foreground $Desktop(col:sfg) -background $Desktop(col:sbg) 
   scrollbar $w.txt.scroll -relief sunken -command "$w.txt.text yview" \
	    -width 3m 
   Desktop_exclUpdate font $w.text

# make complete frame
  pack $w.txt.scroll -side right -fill y -padx 2 -pady 2
  pack $w.txt.text -expand 1 -fill both -padx 2 -pady 2
}

proc vised_save { file } {
  set f [open $file w]
  puts $f [.res.txt.text get 0.0 end]
  close $f
}
