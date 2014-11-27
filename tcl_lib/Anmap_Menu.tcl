# menu_build_menu :::: setup menu items
#   construct a menu of the specified name implementing the listed commands
#        menu_name     name of the menu
#        commands      list of menu commands
proc menu_build_menu { menu_name commands } {
    upvar #0 $commands coms
  # clean up the menu
    if ![string match "none" [$menu_name index last]] then {
      set m_start 0
      set m_end [expr "[$menu_name index last] + 1"]
      for  {set m $m_start} {$m < $m_end} {incr m} {$menu_name delete last}
    }
  # find sorted list of array elements
    set l_end [llength [lsort [array name coms]]]
    for {set l 0} {$l < $l_end} {incr l} {
      set el [lindex [lsort [array name coms]] $l]
      $menu_name add command -label [lindex $coms($el) 0] \
                             -command [lindex $coms($el) 1]
    }
}

# menu_build_all_menus :::: setup all the menus in the application
proc menu_build_all_menus { } {
  global menu_names menus menu1 menu2 menu3 menu4 menu5 menu6 menu7 menu8 menu9

  # loop and construct all menus
    for {set m 1} {$m <= $menus} {incr m} {
      menubutton .menu.menu$m -text $menu_names($m) -menu .menu.menu$m.m \
               -relief raised -width 10 -borderwidth 1
      menu .menu.menu$m.m 
      menu_build_menu .menu.menu$m.m menu$m
    }
}

# menu_rebuild_menus :::: rebuild menus without recreating widgets
proc menu_rebuild_menus { } {
  global menu_names menus menu1 menu2 menu3 menu4 menu5 menu6 menu7 menu8 menu9

  # define entries in menu widgets
    for {set m 1} {$m <= $menus} {incr m} {
      menu_build_menu .menu.menu$m.m menu$m
    }
}


# menu_bind :::: define bindings and titles for menus
proc menu_bind { } {
  global menu_names menus menu1 menu2 menu3 menu4 menu5 menu6 menu7 menu8 menu9
  set w .menubind
  catch {destroy $w}
  toplevel $w
  wm title $w "Setup Menu options"
  wm iconname $w "Setup Menu options"
  pack append $w \
     [label $w.label -text "Select Menu option/command"] {top fillx pady 6}
  frame $w.select -relief raised -borderwidth 2
  for {set m 1} {$m <= $menus} {incr m} {
     pack append $w.select \
     [button $w.select.menu$m -relief flat -text $menu_names($m) -command \
       "defaults_select menu$m Anmap_InitMenu .comtpro Bindings temp"] \
       {top fillx pady 2} 
  }

   
  frame $w.commands
  pack append $w.commands \
   [button $w.commands.quit -text "Quit" -command "destroy $w"] \
     {bottom fillx} \
   [button $w.commands.reset -text "Reset" -command {menu_init_commands}] \
     {bottom fillx} \
   [button $w.commands.apply -text "Apply" -command {menu_rebuild_menus}] \
     {bottom fillx} \
   [button $w.commands.save -text "Save" -command {Anmap_SaveMenu}] \
     {bottom fillx} \
   [button $w.commands.new -text "New menu" -command {menu_new}] \
     {bottom fillx} \
   [button $w.commands.rename -text "Rename menu" -command {menu_rename}] \
     {bottom fillx}
  pack append $w $w.commands {left filly fillx expand} \
                 $w.select {right fillx filly pady 6}
}

# menu_new :::: create a popup window to define a new menu
proc menu_new { } {
  global menu_names menus menu1 menu2 menu3 menu4 menu5 menu6 menu7 menu8 menu9
  set w .menunew
  catch {destroy $w}
  toplevel $w
  wm title $w "New menu"
  wm iconname $w "New menu"
  pack append $w \
   [label $w.label -text "Menu name:"] {top fillx pady 1} \
   [entry $w.entry -relief sunken] {top fillx pady 2 padx 2} \
   [button $w.create -relief raised -text Create \
                     -command "menu_new_create $w"] {top fillx} \
   [button $w.cancel -relief raised -text Cancel \
                     -command "destroy $w"] {top fillx}
  bind $w.entry <Return> "menu_new_create $w"
  proc menu_new_create { w } {
    global menu_names menus menu1 menu2 menu3 menu4 menu5 menu6 menu7 menu8 menu9
    set name [$w.entry get]
    incr menus
    set menu_names($menus) $name
    Anmap_MenuBar
    destroy $w
    menu_bind
  }
}


# menu_rename :::: create a popup window to rename a menu
proc menu_rename { } {
  global menu_names menus menu1 menu2 menu3 menu4 menu5 menu6 menu7 menu8 menu9
  global menu_selected
  set w .menurename
  set menu_selected 0
  catch {destroy $w}
  toplevel $w
  wm title $w "Rename menu"
  wm iconname $w "Rename menu"
  pack append $w \
   [label $w.label -text "Select menu:"] {top fillx pady 2} 
  for {set m 1} {$m <= $menus} {incr m} {
   pack append $w \
     [button $w.menu$m -text $menu_names($m) -command "menu_rename_select $w $m" \
                       -relief flat] {top fillx}
  }
  pack append $w \
   [entry $w.entry -relief sunken] {top fillx pady 2 padx 2} \
   [button $w.create -relief raised -text Rename \
                     -command "menu_rename_rename $w"] {top fillx} \
   [button $w.cancel -relief raised -text Cancel \
                     -command "destroy $w"] {top fillx}
  bind $w.entry <Return> "menu_new_rename_rename $w"

  proc menu_rename_rename { w } {
    global menu_names menus menu1 menu2 menu3 menu4 menu5 menu6 menu7 menu8 menu9
    global menu_selected
    if $menu_selected then {
      set name [$w.entry get]
      set menu_names($menu_selected) $name
      Anmap_MenuBar
      destroy $w
      menu_bind
    } 
  }
  proc menu_rename_select { w m } {
    global menu_names menus menu1 menu2 menu3 menu4 menu5 menu6 menu7 menu8 menu9
    global menu_selected
    set menu_selected $m
    $w.entry delete 0 end
    $w.entry insert end $menu_names($menu_selected)
  }
}
proc Anmap_SaveMenu { } {
   global menu_names menus menu1 menu2 menu3 menu4 menu5 menu6 menu7 menu8 menu9
   global Anmap_UserDir AnmapSource

   set fid [open $Anmap_UserDir/anmap.menu w]
   puts $fid "set menus $menus"
   for {set m 1} {$m <= $menus} {incr m} {
      puts $fid "[format {set %s(%s)} menu_names $m] \{$menu_names($m)\}"
      foreach n [array names menu$m] {
          puts $fid "[format {set %s(%s)} menu$m $n] \{[enqel menu$m $n]\}"
      }
   }
   close $fid
}

proc enqel {array_name element} {
    upvar #0 $array_name local_array
    return $local_array($element)
}

proc Anmap_InitMenu { } {

# global variables
  global menu_names menus menu1 menu2 menu3 menu4 menu5 menu6 menu7 menu8 menu9
  global AnmapSource Anmap_UserDir Desktop

# system defaults
  if [file exists $AnmapSource/tcl_lib/anmap.menu]  then {
     source $AnmapSource/tcl_lib/anmap.menu
  }
# user defaults
  if [file exists $Anmap_UserDir/anmap.menu]  then {
    source $Anmap_UserDir/anmap.menu
  }
}


