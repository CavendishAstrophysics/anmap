
# create main top-level window offering basic facilities
proc Anmap_ControlPanel { } {
global Desktop Anmap
wm title . "Anmap"
wm iconname . "Anmap"
wm iconbitmap . @$Anmap(src)/etc/bitmaps/Anmap_logo

image create photo logo -file $Anmap(src)/etc/bitmaps/logo.gif
frame .top
label .top.logo -image logo
message .top.message -width 500  -textvariable Anmap(Message)

Anmap_InitMenu
pack  .top.logo -side left -fill y -expand 0
pack  .top.message -side left -fill both -expand 0
pack  .top -fill x -expand 0 -side top
Anmap_MenuBar

}

proc Anmap_Help { } {
  global Desktop Anmap
  exec $Desktop(src)/xhelp $Anmap(src)/help/anmap.help &
}
proc Anmap_News { } {
  global Desktop Anmap
  exec $Desktop(src)/xhelp $Anmap(src)/help/anmap.news &
}

proc Anmap_MenuBar { } {
  global menus menu_names
  catch {destroy .menu}
  frame .menu
  menu_build_all_menus

# pack menu items into menu widget
  for {set i 1} {$i <= $menus} {incr i} {
    if [string length $menu_names($i)] then {
      pack append .menu .menu.menu$i left
    }
  }
  pack  .menu -side top -fill x -expand 0 -anchor s\
              -ipady 5 -ipadx 5

}

proc AnmapExit { } {
   if [tk_dialog .cs_exit "Exit Confirmation"\
              "Do you really want to exit Anmap?" \
              questhead 0 Cancel Exit] then {
     _exit
   }
}












