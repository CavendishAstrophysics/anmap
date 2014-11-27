C
C
*+ do_chr_chblnk

       subroutine do_chr_chblnk(map_array,status)
C      --------------------------------------
C
C change the "BLANK" values in the map
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C All pixels on a map set to BLANK are reset to another value.  The value
C of BLANK is not changed in the map redtape.
*-
       include '/mrao/include/iolib_functions.inc'

C Local variables
C   map mini redtape
       integer    minirt(8)
C   counters and map pointers
       integer    n, iout
       integer    imap, imapo, ip_mapo
C   gate, value at pixel location and max/min data
       real*4     gate, value
       real*4     zmnx(4)
       integer    izmnx(4)
C   flag to indicate resetting blank
       logical    reset

C Read the buffer entry
       if (status.ne.0) return

       call map_getmap('Map : ','Default-Map','READ',imap,status)
C allocate input map to output map
       call map_alloc_toout(imap,'DIRECT',
     *                      map_array,imapo,ip_mapo,status)
       call enminirt(minirt,status)
C find statistics on the map
       call scnmap2(minirt,map_array(ip_mapo),minirt,zmnx,izmnx,status)
       if (status.ne.0) goto 999

C read new range for map
       reset = .false.
       call enminirt(minirt,status)
       if (status.ne.0) goto 999
       call plot_getuv('UV-range to change : ','*',minirt,status)
       if (status.ne.0) then
         call cmd_wrerr('CHANGE-BLANK','Invalid range on input map')
         goto 999
       end if
       call io_getr('Value to replace BLANK : ','0.0',value,status)
       if (io_yesno('Reset values below gate ? ','no',status)) then
         reset = .true.
         call io_enqout(iout)
         gate = 0.0
         write(iout,10)zmnx(2),zmnx(1)
10       format(1x/1x,'.. Min/Max on map = ',1p2e12.3/1x)
         call io_getr('Gate : ','*',gate,status)
       end if
       if (status.ne.0) goto 999

C blank the map region and update the computing redtape
       if (reset) then
         do n=0,minirt(5)*minirt(6)-1
           if (map_array(ip_mapo+n).lt.gate) then
             map_array(ip_mapo+n)=value
           else
             map_array(ip_mapo+n) = map_array(ip_mapo+n)
           end if
         end do
       end if
       call blkmap(map_array(ip_mapo),minirt,value,status)

C Update the TEXT of the redtape
       call adredt('created','CH-BLANK',status)

C Tidy up, make output map new current map etc.
999    call map_end_alloc(imapo,map_array,status)

C check STATUS value
       call cmd_err(status,'CHANGE-BLANK','Failed ')
       end
