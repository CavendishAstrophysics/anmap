C Aphi_RealImag : convert Amplitude/Phase to/from Real/Imaginary
C
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 1/11/93
C
*-

C define work array
       integer       nm, nb
       integer        msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=16)
       parameter    (nb=3)
       real*4        map_array( nm*msize + nb*bsize )
C error status
       integer       status

C perform standard initialization
       call anm_start( 0,nm,msize,nb,bsize,status )

C read input map: imap is the catalogue identifier
       call do_aphi_realimag(map_array,status)

C check status value and report an error to the user
       call cmd_err(status,'Aphi_RealImag','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end
C
C
*+ do_segment_flux

       subroutine do_aphi_realimag(map_array,status)
C      -------------------------------------------------
C
C convert Amplitude/Phase to/from Real/Imaginary
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C Conversion between image pairs of Amplitude/Phase and Real/Imaginary
C is performed.
*-

       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/constants.inc'

C Local variables:
C   mini redtape
       integer    minirt(8), buffer(8)
C   map pointers
       integer    imap1, imap2, imapo1, imapo2,
     *            ip_map1, ip_map2, ip_mapo1, ip_mapo2
C   output map information
       character  out1*8, out2*8
C   counters etc
       real*4     bl1, bl2, bl, a, p, x, y
       integer    i
C   options
       integer    direction
       character*24       opt, options(2)
       data options /   'Aphi-to-real-imag',
     *                  'real-imag-to-Aphi'/

C check status
       if (status.ne.0) return

C get direction for conversion
       call io_getopt( 'Option (?=list) : ','Aphi-to-real-imag',
     *                  options,2,opt,status)
       if (chr_cmatch(opt,'Aphi-to-real-imag')) then
          direction = 1
          out1 = 'CNV-REAL'
          out2 = 'CNV-IMAG'
       else
          direction = -1
          out1 = 'CNV-AMPL'
          out2 = 'CNV-PHAS'
       endif

C read in the map
       call map_getmap('Real/Amplitude : ','Default_Map',
     *                 'READ',imap1,status)
       call map_alloc_in(imap1,'DIRECT',map_array,ip_map1,status)
       call enminirt(minirt,status)
       call ennull(bl1)
       call map_getmap('Imaginary/Phase : ','Default_Map',
     *                 'READ',imap2,status)
       call map_alloc_in(imap2,'DIRECT',map_array,ip_map2,status)
       call enminirt(buffer,status)
       call ennull(bl1)
       bl = bl1
       call map_alloc_out(0,0,'DIRECT',imapo1,ip_mapo1,status)
       call map_alloc_out(0,0,'DIRECT',imapo2,ip_mapo2,status)
       if (buffer(5).ne.minirt(5) .or.
     *     buffer(6).ne.minirt(6) ) then
         print *,'***(Aphi_RealImag) ERROR  -- Images differ in size'
         print *,'***(Aphi_RealImag) FAILED -- Use consistent images'
         goto 999
       endif

C do the conversion
       if (direction.eq.1) then
          do i=1,minirt(5)*minirt(6)
             a = map_array(ip_map1+i-1)
             p = map_array(ip_map2+i-1)
             if (a.ne.bl1 .and. p.ne.bl2) then
               map_array(ip_mapo1+i-1) = a*cos(p*const_d2r)
               map_array(ip_mapo2+i-1) = a*sin(p*const_d2r)
             else
               map_array(ip_mapo1+i-1) = bl
               map_array(ip_mapo2+i-1) = bl
             endif
          enddo
       else
          do i=1,minirt(5)*minirt(6)
             x = map_array(ip_map1+i-1)
             y = map_array(ip_map2+i-1)
             if (x.ne.bl1 .and. x.ne.bl2) then
               map_array(ip_mapo1+i-1) = sqrt(x**2 + y**2)
               map_array(ip_mapo2+i-1) = atan2(y,x)/const_d2r
             else
               map_array(ip_mapo1+i-1) = bl
               map_array(ip_mapo2+i-1) = bl
             endif
          enddo
       endif

999    call adredt('CREATED',out1,status)
       call map_end_alloc(imapo1,map_array,status)
       call adredt('CREATED',out2,status)
       call map_end_alloc(imapo2,map_array,status)
       call map_end_alloc(imap1,map_array,status)
       call map_end_alloc(imap2,map_array,status)
       end

