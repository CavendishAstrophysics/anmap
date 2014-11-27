C FFT -- Perform a 2D FFT of input data and the reverse transform
C ---
C
C FFT performs a 2D FFT of an input image and writes out two images of
C the amplitude and phase of the transform or alternatively the real
C and imaginary parts.  Alternatively two images may be read and the
C output image is the inverse FFT of the supplied images.
C-

       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/constants.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/chrlib_functions.inc'

C Local variables
C ---------------
C define work array
       integer            nm, nb
       integer            msize, bsize
       parameter         (msize = 512*512)
       parameter         (bsize=1024)
       parameter         (nm=8)
       parameter         (nb=3)
       real*4             map_array( nm*msize + nb*bsize )
C  data records
       integer            max_dim
       parameter         (max_dim = 1024)
       complex            array(max_dim*max_dim)
C  catalogue entry information
       integer            in1, in2, out1, out2, ip1, ip2
C  map mini redtape
       integer            minirt(10), buffer(10)
C  counters
       integer            i
C  redtape items
       real*4             freq
       integer            poln
       character          name*16, unit*16, source*80, program*80
C  status word
       integer            status
C  options for the transform
       integer            direction, mode, type
       character*24       opt, options(8)
       data options /   'forward-real-imaginary',
     *                  'inverse-real-imaginary',
     *                  'forward-amplitude-phase',
     *                  'inverse-amplitude-phase',
     *                  'real-imaginary-forward',
     *                  'real-imaginary-inverse',
     *                  'amplitude-phase-forward',
     *                  'amplitude-phase-inverse'/

C initialise
       status = 0
       call anm_start( 0,nm,msize,nb,bsize,status )

C get option (forward, reverse and type):
       call io_getopt( 'Option (?=list) : ','forward-real-imaginary',
     *                  options,8,opt,status)
       if (chr_cmatch(opt,'forward-real-imaginary')) then
         direction = 1
         mode = 2
         type = 1
       elseif (chr_cmatch(opt,'inverse-real-imaginary')) then
         direction = -1
         mode = 2
         type = 1
       elseif (chr_cmatch(opt,'forward-amplitude-phase')) then
         direction = 1
         mode = 1
         type = 1
       elseif (chr_cmatch(opt,'inverse-amplitude-phase')) then
         direction = -1
         mode = 1
         type = 1
       elseif (chr_cmatch(opt,'real-imaginary-forward')) then
         direction = 1
         mode = 1
         type = 2
       elseif (chr_cmatch(opt,'real-imaginary-inverse')) then
         direction = -1
         mode = 1
         type = 2
       elseif (chr_cmatch(opt,'amplitude-phase-forward')) then
         direction = 1
         mode = 2
         type = 2
       elseif (chr_cmatch(opt,'amplitude-phase-inverse')) then
         direction = -1
         mode = 2
         type = 2
       endif
       if (direction.eq.1) then
         if (type.eq.1) then
           call map_getmap('Catalogue-entry : ','Default_Map',
     *                     'READ',in1,status)
           call map_alloc_in(in1,'DIRECT',map_array,ip1,status)
           call enminirt(minirt,status)
           do i=1,minirt(5)*minirt(6)
             array(i) = cmplx(map_array(ip1+i-1),0.0)
           enddo
           call map_end_alloc(in1,map_array,status)
         elseif (type.eq.2) then
           call map_getmap('Real/Amplitude-entry : ','Default_Map',
     *                     'READ',in1,status)
           call map_getmap('Imaginary/Phase-entry : ','Default_Map',
     *                     'READ',in2,status)
           call map_alloc_in(in1,'DIRECT',map_array,ip1,status)
           call enminirt(buffer,status)
           call map_alloc_in(in2,'DIRECT',map_array,ip2,status)
           call enminirt(minirt,status)
           if (buffer(5).ne.minirt(5) .or.
     *         buffer(6).ne.minirt(6) ) then
             print *,'***(IMAGE-FFT) ERROR  -- Images differ in size'
             print *,'***(IMAGE-FFT) FAILED -- Use consistent images'
             goto 999
           endif
           if (mode.eq.2) then
             do i=1,minirt(5)*minirt(6)
               array(i) = cmplx(map_array(ip1+i-1),
     *                          map_array(ip2+i-1))
             enddo
           else
             do i=1,minirt(5)*minirt(6)
               array(i) = cmplx( map_array(ip1+i-1)*
     *                           cos(map_array(ip2+i-1)*const_d2r),
     *                           map_array(ip1+i-1)*
     *                           sin(map_array(ip2+i-1)*const_d2r))
             enddo
           endif
           call map_end_alloc(in1,map_array,status)
           call map_end_alloc(in2,map_array,status)
         endif
       else
         call map_getmap('Real/Amplitude-entry : ','Default_Map',
     *                   'READ',in1,status)
         call map_getmap('Imaginary/Phase-entry : ','Default_Map',
     *                   'READ',in2,status)
         call map_alloc_in(in1,'DIRECT',map_array,ip1,status)
         call enminirt(buffer,status)
         call map_alloc_in(in2,'DIRECT',map_array,ip2,status)
         call enminirt(minirt,status)
         if (buffer(5).ne.minirt(5) .or.
     *       buffer(6).ne.minirt(6) ) then
           print *,'***(IMAGE-FFT) ERROR  -- Images differ in size'
           print *,'***(IMAGE-FFT) FAILED -- Use consistent images'
           goto 999
         endif
         call image_fft_fiddle4(map_array(ip1),
     *              minirt(5),minirt(6),array,status)
         call image_fft_fiddle4(map_array(ip2),
     *              minirt(5),minirt(6),array,status)
         if (mode.eq.2) then
           do i=1,minirt(5)*minirt(6)
             array(i) = cmplx(map_array(ip1+i-1),
     *                        map_array(ip2+i-1))
           enddo
         else
           do i=1,minirt(5)*minirt(6)
             array(i) = cmplx( map_array(ip1+i-1)*
     *                         cos(map_array(ip2+i-1)*const_d2r),
     *                         map_array(ip1+i-1)*
     *                         sin(map_array(ip2+i-1)*const_d2r))
           enddo
         endif
         call map_end_alloc(in1,map_array,status)
         call map_end_alloc(in2,map_array,status)
       endif

       call complex_2dfft(array,minirt(5),minirt(6),direction,status)
       if (status.ne.0) goto 999

       if (direction.eq.1) then
         call map_alloc_out(0,0,'DIRECT',out1,ip1,status)
         call map_alloc_out(0,0,'DIRECT',out2,ip2,status)
         if (mode.eq.1) then
           do i=1,minirt(5)*minirt(6)
             map_array(ip1+i-1) = sqrt( real(array(i))**2 +
     *                                  imag(array(i))**2  )
             if (array(i).eq.(0.0,0.0)) then
               map_array(ip2+i-1) = 0.0
             else
               map_array(ip2+i-1) = atan2( imag(array(i)),
     *                                     real(array(i)) ) /
     *                                     const_d2r
             endif
           end do
         else
           do i=1,minirt(5)*minirt(6)
               map_array(ip1+i-1) = real(array(i))
               map_array(ip2+i-1) = imag(array(i))
           enddo
         endif
         call image_fft_fiddle4(map_array(ip1),
     *              minirt(5),minirt(6),array,status)
         call image_fft_fiddle4(map_array(ip2),
     *              minirt(5),minirt(6),array,status)
         call mapcat_enqsr(out1,source,program,status)
         i = chr_ilstc(source,'-')
         if (mode.eq.2) then
           source = source(1:i)//'REAL'
         else
           source = source(1:i)//'AMP'
         endif
         call mapcat_setsr(out1,source,program,status)
         call map_end_alloc(out1,map_array,status)
         call entype( freq, poln, name, unit, status )
         if (mode.eq.2) then
           source = source(1:i)//'IMAG'
         else
           source = source(1:i)//'PHA'
           name = 'PHASE'
           unit = 'DEGREES'
         endif
         call mapcat_setsr(out2,source,program,status)
         call sttype( freq, poln, name, unit, status )
         call map_end_alloc(out2,map_array,status)
       elseif (direction.eq.-1 .and. type.eq.1) then
         call map_alloc_out(0,0,'DIRECT',out1,ip1,status)
         do i=1,minirt(5)*minirt(6)
             map_array(ip1+i-1) = real(array(i))
         enddo
         call mapcat_enqsr(out1,source,program,status)
         i = chr_ilstc(source,'-')
         source = source(1:i)//'FFT'
         call mapcat_setsr(out1,source,program,status)
         call map_end_alloc(out1,map_array,status)
       else
         call map_alloc_out(0,0,'DIRECT',out1,ip1,status)
         call map_alloc_out(0,0,'DIRECT',out2,ip2,status)
         if (mode.eq.1) then
           do i=1,minirt(5)*minirt(6)
             map_array(ip1+i-1) = sqrt( real(array(i))**2 +
     *                                  imag(array(i))**2  )
             if (array(i).eq.(0.0,0.0)) then
               map_array(ip2+i-1) = 0.0
             else
               map_array(ip2+i-1) = atan2( imag(array(i)),
     *                                     real(array(i)) ) /
     *                                     const_d2r
             endif
           end do
         else
           do i=1,minirt(5)*minirt(6)
               map_array(ip1+i-1) = real(array(i))
               map_array(ip2+i-1) = imag(array(i))
           enddo
         endif
         call mapcat_enqsr(out1,source,program,status)
         i = chr_ilstc(source,'-')
         if (mode.eq.2) then
           source = source(1:i)//'REAL'
         else
           source = source(1:i)//'AMP'
         endif
         call mapcat_setsr(out1,source,program,status)
         call map_end_alloc(out1,map_array,status)
         call entype( freq, poln, name, unit, status )
         if (mode.eq.2) then
           source = source(1:i)//'IMAG'
         else
           source = source(1:i)//'PHA'
           name = 'PHASE'
           unit = 'DEGREES'
         endif
         call mapcat_setsr(out2,source,program,status)
         call sttype( freq, poln, name, unit, status )
         call map_end_alloc(out2,map_array,status)
       endif

C exit
999    call anm_end( status )
       end
C
C
       subroutine image_fft_fiddle4( x, m, n, work, s)
C      -----------------------------------------------
C      Perform a simple data translation
       integer    m,n,i,j, s
       integer    j1,j2
       real*4     x(*), work(*)
       do j=1,n/2
         j1 = (j-1)*m
         j2 = (j-1+n/2)*m
         do i=1,m/2
           work(i+j1) = x(i+m/2+j2)
           work(i+m/2+j2) = x(i+j1)
         end do
       end do
       do j=1+n/2,n
         j1 = (j-1)*m
         j2 = (j-1-n/2)*m
         do i=1,m/2
           work(i+j1) = x(i+m/2+j2)
           work(i+m/2+j2) = x(i+j1)
         end do
       end do
       do i=1,m*n
         x(i) = work(i)
       end do
       end

