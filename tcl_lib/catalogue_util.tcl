#
# Procedures to implement catalogue functions
#
#
proc catalogue_load { args } {
# procedure to load a map-catalogue; temporary replacement for a proper
# system within anmap
  global env
  catalogue re-initialise-stack
  iocmd set-cli [join $args { }]
  set cat [iocmd get-word 'Load-Catalogue-name : ' 'save-catalogue']
  set fc $env(HOME)/mrao/${cat}.mctg
  if [file exists $fc] then {
    cp $fc $env(HOME)/mrao/map-catalogue.mctg 
  } else { 
    cp $env(HOME)/mrao/dummy.mctg $env(HOME)/mrao/map-catalogue.mctg
  }
}

proc catalogue_save { args } {
# procedure to save a map-catalogue; temporary replacement for a proper
# system within anmap
  global env
  iocmd set-cli [join $args { }]
  set cat [iocmd get-word 'Save-Catalogue-name : ' 'save-catalogue']
  set fc $env(HOME)/mrao/${cat}.mctg
  cp $env(HOME)/mrao/map-catalogue.mctg $fc
}

proc catalogue_switch { args } {
  global env
  iocmd set-cli [join $args { }]
  set cat [iocmd get-word 'Save-Catalogue-name : ' 'save-catalogue']
  set fc $env(HOME)/mrao/${cat}.mctg
  cp $env(HOME)/mrao/map-catalogue.mctg $fc
  set cat [iocmd get-word 'Load-Catalogue-name : ' 'save-catalogue']
  set fc $env(HOME)/mrao/${cat}.mctg
  if [file exists $fc] then {
    cp $fc $env(HOME)/mrao/map-catalogue.mctg 
  } else { 
    cp $env(HOME)/mrao/dummy.mctg $env(HOME)/mrao/map-catalogue.mctg
  }
}

proc catalogue_reportMode { args } {
  global env
  set onoff(0) off ; set onoff(1) on
  set rm [iocmd onoff 'Report-Mode (on/off) : ' 'on']
  anmap_command catalogue-system report-mode $onoff($rm)
  set env(anm_ReportMode) $rm
}

proc imcat_enq { args } {
  return [anmap_command catalogue enq $args]
}

proc imcat_fileList { } {
  for {set n 1} {$n <= 256} {incr n} {
     set name [imcat_enq file $n]
     if [string length $name] then {
        lappend flist $name
     }
  }
  return $flist
}
