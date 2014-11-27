C
C
*+ mapcat_disopts

       subroutine mapcat_disopts(prompt_state,status)
C      ----------------------------------------------
C
C Display current options settings
C
C Given:
C   current prompt state
       logical    prompt_state
C Returned:
C   status value
       integer    status
C
C-

       include 'mapcat_cat.inc'

C local variables
       integer   iout

C check status on entry
       if (status.ne.0) return
C find output unit
       call io_enqout(iout)

C report options
       call io_wrout(' ')
       call io_wrout('Catalogue-System Options')
       call io_wrout('------------------------')
       if (prompt_state) then
         call io_wrout('Prompting for confirmation      :   On')
       else
         call io_wrout('Prompting for confirmation      :   Off')
       end if
       if (current_report) then
         call io_wrout('Report entry of new map         :   On')
       else
         call io_wrout('Report entry of new map         :   Off')
       end if
       if (current_check) then
         call io_wrout('Checking TEMPORARY maps on exit :   On')
       else
         call io_wrout('Checking TEMPORARY maps on exit :   Off')
       end if
       write(iout,10) area_max_read
10     format(1x,   'Maximum sub-area to read        : ',F6.2)
       write(iout,20) default_map
20     format(1x,   'Default map-catalogue entry     : ',I6)
       call io_wrout(' ')

       call mapcat_err(status,'mapcat_disopts',' ')
       end
