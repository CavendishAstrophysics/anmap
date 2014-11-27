C
C
*+ do_age_B_const

       subroutine do_age_B_const(map_array,status)
C      -------------------------------------------
C
C Calculate a map of synchrotron age assuming a constant B field
C
C Given:
C   map data
      real*4      map_array(*)
C Returned:
C   status
      integer     status
C
C A map of spectral-age is calculated from a map of break frequency 
C assuming a constrant magnetic field in space and time within the
C emitting region.
C-

C Assumed B-field
       real*4     B_field
C input map and pointer
       integer    imap, ip_map
C output map and pointer
       integer    iwork, ip_work
C blank value on input map
       real*4     blank
C map range
       integer    minirt(10)
C pointers and counters
       integer    i
C local function
       real*4     synch_age

C check status on entry
       if (status.ne.0) return

C read input map and data
       call map_getmap('Break-frequency-map : ','Default-Map','READ',
     *                 imap,status)
       call ennull(blank,status)
       call io_getr('B-field (nT) : ','1.0',B_field,status)

C find output map
       call map_alloc_out(0,0,'DIRECT',iwork,ip_work,status)

C read pointer to input map
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)

C read the UV range on which to work
       call enminirt(minirt,status)

C check status at this stage
       if (status.ne.0) goto 999

C do the calculation
       do i = 1,minirt(5)*minirt(6)
         if (map_array(ip_map+i-1).ne.blank) then
           map_array(ip_work+i-1) =
     *              synch_age(map_array(ip_map+i-1),B_field)
         else
           map_array(ip_work+i-1) = blank
         end if
       end do

C update the text of the redtape
       call adredt('created','AGE-MAP',status)
       call stnull(blank,status)

C tidy up, make output map new current map etc.
999    call map_end_alloc(imap,map_array,status)
       call map_end_alloc(iwork,map_array,status)

C check status value
       call iocmd_err(status,'AGE-MAP',' ')

       end

