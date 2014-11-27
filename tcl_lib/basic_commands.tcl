#
# Basic procedures for Anmap commands
#

# redtape command
proc redtape_display { args } {
  if [llength $args] then {
   eval iocmd set-cli $args
  }
  set imap [iocmd get-word 'Catalogue-entry : ' '0']
  edit-redtape read-redtape $imap
  edit-redtape display-redtape
}



