C
C
*+ do_add_flux

       subroutine do_add_flux(map_array,status)
C      ----------------------------------------
C
C determine the flux within a specified region
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C Only those points whose absolute value is above a specified gate are
C included in the sum. The sum normalized to the CLEAN-BEAM volume is
C also written out.
*-
C Local variables
C   mini redtape and uvrange variables
       integer    minirt(8), iuv(8)
C   pointers, counters etc.
       integer    imap, ip_map, i
       integer    iout, npts
C   results and gate on map
       real*4     flux, gate, clean_flux
C   data for cleaned maps
       real*4     flxnrm, beam(2), position_angle

C check status on entry
       if (status.ne.0) return

C find map
       call map_getmap('Map : ','Default-Map','READ',imap,status)
       call enbeam(flxnrm,beam,position_angle,status)

C get map region
       call enminirt(minirt,status)
       call enminirt(iuv,status)
       call plot_getuv('UV-range : ','*',iuv,status)
       call map_alloc_area(imap,iuv,map_array,ip_map,status)

C get gate
       gate = 0.0
       call io_getr('Gate : ','*',gate,status)

C do sum
       call image_addflux(minirt,map_array(ip_map),
     *                    iuv,gate,flux,npts,status)
       if (status.ne.0) goto 999

C now clean beam normalized flux
       clean_flux = flux*flxnrm

C write out results
       call io_enqout(iout)
       write(iout,100)imap,(iuv(i), i=1,4),flux,clean_flux
100    format(' Map=',i2,' UV=',4I6,' sum=',1PE12.3,
     *        ' CLEAN-Flux=',1PE12.3)

999    call map_end_alloc(imap,map_array,status)
       call cmd_err(status,'ADD-FLUX','Failed')

       end
