#
# procedures used to implement analysis commands
#
proc ricean_correction { args } {
  if [llength $args] then {
    eval iocmd set-cli $args
  } else {
    iocmd clear-cli
  }
  edit-image set-map
  edit-image apply-ricean-correction
  edit-image set-program "Ricean"
  edit-image save-edits
}

