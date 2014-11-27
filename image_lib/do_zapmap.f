C
C
*+ do_zapmap

       subroutine do_zapmap(map_array,status)
C      --------------------------------------
C
C Set a regon of a map to a specified value
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The specified region of the map is set to a specified value.
*-
       include '/mrao/include/iolib_functions.inc'
C
       integer    minirt(8), imap, imapo, ip_mapo
       real*4     value

c read the buffer entry
       if (status.ne.0) return

       call map_getmap('Map : ','Default-Map','READ',imap,status)
C allocate input map to output map
       call map_alloc_toout(imap,'DIRECT',
     *                      map_array,imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C read value to use as the replacement value on map to map
       call enminirt(minirt,status)
       call ennull(value,status)
       call io_getr('Value to use in zapped area [blank] : ',
     *           ' ',value,status)

C read region and "zap"
50     call plot_getuv('UV-range to zap : ','*',minirt,status)
       if (status.ne.0) then
         call cmd_wrerr('ZAP-MAP','Invalid range on input map')
         goto 999
       end if
       call filmap(map_array(ip_mapo),minirt,value,status)
       if (io_yesno('Another region on this map ? ','no',status))
     *     goto 50

C update the text of the redtape
       call adredt('CREATED','ZAP-MAP',status)

C tidy up, make output map new current map etc.
999    call map_end_alloc(imapo,map_array,status)

C check status value
       call cmd_err(status,'ZAP-MAP','Failed ')
       end
