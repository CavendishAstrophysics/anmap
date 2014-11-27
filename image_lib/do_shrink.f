C
C
*+ do_shrink

       subroutine do_shrink(map_array,status)
C      --------------------------------------
C
C Shrink the size of a map
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
*-

C Local variables
       integer    minirt(8), ip_map, imap, ip_mapo, imapo

C check status on entry
       if (status.ne.0) return

C find input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       if (status.ne.0) goto 999

C read new range for map
       call enminirt(minirt,status)
       call plot_getuv('UV-range on output : ','*',minirt,status)
       if (status.ne.0) then
         call cmd_wrerr('SHRINK','Invalid range on input map')
         goto 999
       end if

C extract the map region and update the computing redtape
       minirt(5)=minirt(2)-minirt(1)+1
       minirt(6)=minirt(3)-minirt(4)+1

C allocate output map
       call map_alloc_out(minirt(5),minirt(6),'DIRECT',
     *                    imapo,ip_mapo,status)
C read input map
       call map_alloc_area(imap,minirt,map_array,ip_map,status)
       if (status.ne.0) goto 999

C extract the map data to the output map
       call extmap(map_array(ip_map),minirt,0,map_array(ip_mapo),
     *             minirt(5),minirt(6),status)
       call stredt(minirt,3,status)

C update the text of the redtape
       call adredt('CREATED','SHRINK',status)

C Tidy up
999    continue
       call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'SHRINK','Failed ')
       end
