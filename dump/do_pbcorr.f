C
C
*+ do_pbcorr

       subroutine do_pbcorr(map_array,status)
C      --------------------------------------
C
C Apply a primary beam correction for the RT and CLFST
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The values for the primary beam correction are obtained from the
C MAPLIB routine PBCORR.
*-

C Local variables
       integer    tscope, minirt(8), imap, imapo, ip_mapo, iu, iv, ip
       integer    iuvmap
       real*8     u, v, ra, dec, rapnt, decpnt
       real*4     pb_value, freq

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call map_alloc_toout(imap,'DIRECT',map_array,
     *                      imapo,ip_mapo,status)
       call entpnt( tscope, freq, rapnt, decpnt, status )
       if (status.ne.0) goto 999

C read the map size etc.
       call enminirt(minirt,status)
       if (status.ne.0) goto 999

C scale the map
       do iv = minirt(3),minirt(4),-1
         do iu = minirt(1),minirt(2),1
           ip = iuvmap(iu,iv) - 1
           u = iu
           v = iv
           call uvtord( u, v, ra, dec, status )
           call pbcorr( ra, dec, rapnt, decpnt, tscope,
     *                  pb_value, status )
           if (pb_value.gt.0.0) then
             map_array(ip_mapo+ip) = map_array(ip_mapo+ip) /
     *                               pb_value
           else
             map_array(ip_mapo+ip) = 0.0
           end if
         end do
       end do

C Update the TEXT of the redtape
       call adredt('CREATED','PBCORR',status)

C Tidy up
999    call map_end_alloc(imap,map_array,status)
       call map_end_alloc(imapo,map_array,status)

C check status value
       call cmd_err(status,'PBCORR','Failed ')
       end
