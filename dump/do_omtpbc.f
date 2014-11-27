C
C
C
C
*+  do_omtpbc

       subroutine do_omtpbc(map_array,status)
C      --------------------------------------
C
C Apply a primary beam correction for the OMT at 1407MHz
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The functional form assumed is exp-(theta**2/theta0**2)
C                                    theta0 = 28.5 arcmin
*-

C Local variables
       real*4     u_centre, v_centre, theta, theta0
       integer    minirt(8), imap, imapo, ip_mapo, iu, iv, ip
       integer    iuvmap
       real*8     usamp, vsamp

       data       theta0 /1710.0/

C check status on entry
       if (status.ne.0) return

C read input map
       call io_wrout('.. Applying OMT PBCOR (approximate)')
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call map_alloc_toout(imap,'DIRECT',map_array,
     *                      imapo,ip_mapo,status)
       call ensamp(usamp,vsamp,status)
       if (status.ne.0) goto 999

C read the central pixel
       call enminirt(minirt,status)
       call io_getr('U-centre : ','0.0',u_centre,status)
       call io_getr('V-centre : ','0.0',v_centre,status)
       if (status.ne.0) goto 999

C scale the map
       do iv = minirt(3),minirt(4),-1
         do iu = minirt(1),minirt(2),1
           ip = iuvmap(iu,iv) - 1
           theta = sqrt( ((float(iu)-u_centre)*usamp)**2
     *                  +((float(iv)-v_centre)*vsamp)**2 )
           map_array(ip_mapo+ip) = exp((theta/theta0)**2) *
     *                             map_array(ip_mapo+ip)
         end do
       end do

C Update the TEXT of the redtape
       call adredt('CREATED','PB-OMT',status)

C Tidy up
999    call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'PBCOR-OMT','Failed ')
       end
