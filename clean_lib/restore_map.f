C
C
*+ restore_map

       subroutine restore_map(map, m1, m2, clean_area,
     *                        beam, n1, n2, beam_area,
     *                        cc_values, cc_posns, ncc,
     *                        status                   )
C      -------------------------------------------------
C
C restore sources to map with specified opions
C
C Given:
C    size of map
       integer      m1, m2
C    map area to restore
       integer      clean_area(4)
C    beam size and array
       integer      n1 ,n2
       real*4       beam(n1,n2)
C    area on beam
       integer      beam_area(4)
C    CC values
       real*4       cc_values(*)
C    CC positions
       integer*2    cc_posns(2,*)
C    number of CC's
       integer      ncc
C Updated:
C    map data
       real*4       map(m1,m2)
C Returned:
C    error status
       integer      status
C
C
C local variables
       integer    im1, im2, jm1, jm2, ib1, ib2, jb1, jb2,
     *            ib3, ib4, jb3, jb4, ibc, jbc, ius1, jus1
       integer    iout, npts
       real*4     xbar, xsd, bmin, bmax, angle, cbsig1, cbsig2

       include '../include/clean_record.inc'
       include '/mrao/include/maplib_redtape.inc'

C check status on entry
       if (status.ne.0) return
       im1 = clean_area(1)
       im2 = clean_area(2)
       jm1 = clean_area(3)
       jm2 = clean_area(4)
       ib1 = beam_area(1)
       ib2 = beam_area(2)
       jb1 = beam_area(3)
       jb2 = beam_area(4)

C output unit
       call io_enqout(iout)

C information on residual map
       call noise_map(map,m1,m2,im1,im2,jm1,jm2,xbar,xsd,npts,status)
       write(iout,601) 'Residual-Map',xbar,xsd
C
C find maximum value of beam and position corresponding (IBC,JBC)
       call maxmin(beam,n1,n2,ib1,ib2,jb1,jb2,bmax,ibc,jbc,
     *             bmin,ius1,jus1,status)
C
C Make clean beam.
C Find the noise on the residual map.
C Restore the sources to the residual map.
       ib3=ib1
       ib4=ib2
       jb3=jb1
       jb4=jb2
C
C Make clean beam
       if (ltrbm) then
         write(iout,'(''+'',a,20x)')'CLEAN: truncating dirty beam'
         call make_trbm(beam,n1,n2,bmax,ibc,jbc,ib3,ib4,jb3,jb4,
     *                  flxnrm,status)

       else if (.not.loldbm) then
         write(iout,'(''+'',a,20x)')'CLEAN: making clean beam'
         bmax = 1.0
         ibc = n1/2
         jbc = n2/2
         cbsig1 = beam_size_u*0.4246609/usamp
         cbsig2 = beam_size_v*0.4246609/vsamp
         angle  = beam_pa*3.14159265/180.0
         call make_clbm(beam,n1,n2,cbsig1,cbsig2,angle,
     *                  bmax,ibc,jbc,ib3,ib4,
     *                  jb3,jb4,flxnrm,status)
       end if
C
C Restore removed sources
       write(iout,'(''+'',a,20x)')'CLEAN: restoring sources'
       call restor(cc_values,cc_posns,ncc,map,m1,m2,im1,im2,jm1,jm2,
     *             beam,n1,n2,ib3,ib4,jb3,jb4,bmax,ibc,jbc,status)
C
       call noise_map(map,m1,m2,im1,im2,jm1,jm2,xbar,xsd,npts,status)
       write(iout,601)'Restored-Map',xbar,xsd
       write(iout,602)flxnrm
602    format(1h0,
     >    'CLEAN: Factor for converting sum of numbers to flux :',
     >     2x,f8.5)
       return
C
601    format(' CLEAN: Statistics [',A,'] Mean = ',1PE10.2,
     >        ' SD = ',1PE10.2/1X)
       end
