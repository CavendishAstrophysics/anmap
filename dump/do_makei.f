C
C
*+ do_makei

       subroutine do_makei(map_array,status)
C      -------------------------------------
C
C make an I map from an I+/-Q and Q combination
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The routine will prompt for an I+/-Q and a Q map. The redtape defining
C the STOKES parameter must be correct as this is used to determine the
C way in which the maps are combined.
*-

C Local variables
       integer    imapiq, ip_mapiq, imapq, ip_mapq,
     *            imapo, ip_mapo, iu, iv,
     *            iwarn, isign, minirt(8)
       integer    ipoln
       character  name*16, unit*16
       real*4     freq

C check status on entry
       if (status.ne.0) return

C find I+/-Q and Q maps
       call map_getmap('Map-I+/-Q : ','Default-Map','READ',imapiq,
     *                 status)
       call entype(freq,ipoln,name,unit,status)
       if (status.ne.0) goto 999
       if (ipoln.eq.5) then
         isign = +1
       else if (ipoln.eq.6) then
         isign = -1
       else
         call cmd_wrerr('MAKEI','Input map is not I+/-Q: Failed')
         goto 999
       end if

       call map_getmap('Map-Q : ','Default-Map','READ',
     *                  imapq,status)
       call entype(freq,ipoln,name,unit,status)
       if (ipoln.ne.2) then
         call cmd_wrerr('MAKEI','Map is not Q-map: Failes')
         goto 999
      end if

C check redtape
       iwarn = 2
       call redt_comp(imapiq,imapq,iwarn,status)
       if (status.ne.0. .or. iwarn.gt.5) goto 999

C allocate maps
       call map_alloc_in(imapiq,'SEQUENTIAL',map_array,ip_mapiq,status)
       call map_alloc_in(imapq,'SEQUENTIAL',map_array,ip_mapq,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapo,ip_mapo,status)

C do the addition
       call enminirt(minirt,status)
       do iv = minirt(3),minirt(4),-1
         call map_row_read(imapiq,iv,map_array,ip_mapiq,status)
         call map_row_read(imapq,iv,map_array,ip_mapq,status)
         do iu = 1,minirt(5)
           map_array(ip_mapo+iu-1) = map_array(ip_mapiq+iu-1)
     *                           + isign*map_array(ip_mapq+iu-1)
         end do
           call map_row_write(imapo,iv,map_array,ip_mapo,status)
       end do

C update the redtape
       call adredt('created','MAKEI',status)
       call redt_update('IMAP',1,-1.0,' ',' ',status)

C tidy up
999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imapiq,map_array,status)
       call map_end_alloc(imapq,map_array,status)

C check status value
       call cmd_err(status,'MAKEI','Failed ')
       end
