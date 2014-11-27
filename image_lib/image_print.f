C
C
*+ image_print

       subroutine image_print(minirt,data,status)
C      ------------------------------------------
C
C Print values from an image
C
C Given:
C   redtape items defining image
       integer    minirt(*)
C   image data array
       real*4     data(*)
C Returned:
C   status variable
       integer    status
C
C To produce a formatted printed output of a region of the specified image.
C The user will be requested to define the format of the output printing.
C
C Output will be sent to the user defined output device.
C All user IO is handled using the IOLIB routine library.
C
C STATUS should be zero on entry to this routine and is not changed
C explicitly.
C
C The routine is NON-STANDARD a call is made to PLT_GETUV routine.
C
C-
C Version 1.0 10/10/89  [P. Alexander, MRAO]
C

C loop counters
       integer       i, ii, n, iu, iv, iuv(2), iu1, iu2
C local UV-range of map
       integer       uv_range(4)
C local image statistics
       real*4        image_stats_r(4), image_max
       integer       image_stats_i(4)
C parameters controlling type of output
       logical       real_output
       real*4        scale_factor, value(50)
       integer       field_size
       character     output_format*80, title_format*80, title*80
       character*1   dash(132)
       integer       len_format, ivalue(50)
       integer       npanel, ipanel, width
C output unit for terminal IO
       integer       iout
C output unit for printed output
       integer       iprint

C Functions
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
       logical   cmd_dblev


C check status on entry
       if (status.ne.0) return

C initialise character variables
       iprint = 0
       title_format = ' '
       output_format= ' '
       do i=1,132
         dash(i) = '-'
       end do

C find the range of image to display and max/min
       do i = 1,4
         uv_range(i) = minirt(i)
       end do
       call plot_getuv('UV-range : ','*',uv_range,status)
       call scnmap2(minirt,data,uv_range,image_stats_r,image_stats_i,
     *              status)
       image_max = max(abs(image_stats_r(1)),abs(image_stats_r(2)))
       if (status.ne.0) goto 999

C determine type of output
       call io_enqout(iout)
       write(iout,1) image_max
  1    format(1x,'Absolute maximum in range = ',1PE11.3)
       real_output = io_yesno(
     *      'Print values in floating point format ? ','no',status)
       if (status.ne.0) goto 999
       if (real_output) then
         call io_geti('Number of decimal places : ','2',
     *                field_size,status)
       else
         call io_geti('Size of each output field : ','8',
     *                field_size,status)
         scale_factor = 10.0**(nint(field_size - 2 - log10(image_max)))
         call io_getr('Scale-factor : ','*',scale_factor,status)
       end if

C open the output unit for image display
2      call io_geti('Output width (40<=width<=132) : ','80',
     *              width,status)
       if (status.ne.0) goto 999
       if (width.lt.40 .or. width.gt.132) then
         call cmd_wrerr('PRINT-IMAGE','Illegal device width')
         goto 2
       end if
       call io_getstr('Title : ',' ',title,status)
       call io_opeout(iprint,status)
       write(iprint,'(1X/1X,''PRINT-IMAGE Title: '',a)')
     *       title(1:chr_lenb(title))
       if (status.ne.0) goto 999

C loop through the output data printing the values in the requested
C format
       if (real_output) then

C real output
         n = (width - 6)/(field_size + 8)
         if (n.gt.50) n = 50
         npanel = (uv_range(2)-uv_range(1)-1)/n + 1
         if (iprint.ne.iout) then
           write(iprint,'(a,i2)')'Image displayed as ',npanel,' panels'
         end if
         print *,'.. Image displayed as ',npanel,' panels'
         if (field_size.ge.2) then
           write(output_format,3) 8+field_size,field_size
   3       format('(1X,I5,'':'',50(1PE',i2,'.',i1,'))')
           write(title_format,31) 8+field_size
   31      format('(1X/1X/1X,''Panel number: '',i3/',
     *            '1X,'' V/U :'',50I',i2,')')
         else
           write(output_format,4) 8+field_size,field_size
   4       format('(1X,I5,'':'',50(1PE',i1,'.',i1,'))')
           write(title_format,41) 8+field_size
   41      format('(1X/1X/1X,''Panel number: '',i3/',
     *            '1X,'' V/U :'',50I',i1,')')
         end if
         if (cmd_dblev(5)) then
           print *,output_format(1:chr_lenb(output_format))
           print *,title_format(1:chr_lenb(title_format))
         end if
         do ipanel = 1,npanel
           iu1 = uv_range(1) + (ipanel-1)*n
           iu2 = min(uv_range(2),iu1+n-1)
           ii = 0
           if (cmd_dblev(2)) then
             print *,'.. panel: ',ipanel
             print *,'.. range: ',iu1,iu2,uv_range(3), uv_range(4)
           end if
           do iu=iu1,iu2
             ii = ii + 1
             ivalue(ii) = iu
           end do
           len_format = chr_lenb(title_format)
           write(iprint,fmt=title_format(1:len_format))
     *           ipanel,(ivalue(i), i=1,ii)
           write(iprint,'(1X,132A)') (dash(i), i=1,width)
           do iv=uv_range(3),uv_range(4),-1
             ii = 0
             do iu=iu1,iu2
               ii = ii + 1
               iuv(1) = iu
               iuv(2) = iv
               call iuvval2(minirt,data,iuv,value(ii),status)
             end do
             len_format = chr_lenb(output_format)
             write(iprint,fmt=output_format(1:len_format))
     *             iv, (value(i), i=1,ii)
             if (status.ne.0) goto 999
           end do
           write(iprint,'(1X,132A)') (dash(i), i=1,width)
         end do
       else

C integer output
         n = (width - 6)/(field_size + 1)
         npanel = (uv_range(2)-uv_range(1)-1)/n + 1
         if (iprint.ne.iout) then
           write(iprint,'(a,i2)')'Image displayed as ',npanel,' panels'
         end if
         print *,'.. Image displayed as ',npanel,' panels'
         if ((field_size+1).lt.10) then
           write(output_format,5) field_size+1
   5       format('(1X,I5,'':'',50I',i1,')')
           write(title_format,51) field_size+1
   51      format(
     *      '(1X/1X/1X,''Panel number :'',i3,''  Scale: '',1PE12.3/',
     *      '1X,'' V/U :'',50I',i1,')')
         else
           write(output_format,6) field_size+1
   6       format('(1X,I5,'':'',50I',i2,')')
           write(title_format,61) field_size+1
   61      format(
     *      '(1X/1X/1X,''Panel number :'',i3,''  Scale: '',1PE12.3/',
     *      '1X,'' V/U :'',50I',i2,')')
         end if
         if (cmd_dblev(5)) then
           print *,output_format(1:chr_lenb(output_format))
           print *,title_format(1:chr_lenb(title_format))
         end if
         do ipanel = 1,npanel
           len_format = chr_lenb(title_format)
           iu1 = uv_range(1) + (ipanel-1)*n
           iu2 = min(uv_range(2),iu1+n-1)
           ii = 0
           do iu=iu1,iu2
             ii = ii + 1
             ivalue(ii) = iu
           end do
           if (cmd_dblev(2)) then
             print *,'.. panel: ',ipanel, n
             print *,'.. range: ',iu1,iu2,uv_range(3), uv_range(4)
           end if
           len_format = chr_lenb(title_format)
           write(iprint,fmt=title_format(1:len_format))
     *           ipanel,scale_factor,(ivalue(i), i=1,ii)
           write(iprint,'(1X,132A)') (dash(i), i=1,width)
           do iv=uv_range(3),uv_range(4),-1
             ii = 0
             do iu=iu1,iu2
               ii = ii + 1
               iuv(1) = iu
               iuv(2) = iv
               call iuvval2(minirt,data,iuv,value(ii),status)
             end do
             len_format = chr_lenb(output_format)
             write(iprint,fmt=output_format(1:len_format))
     *             iv, (nint(value(i)*scale_factor), i=1,ii)
             if (status.ne.0) goto 999
           end do
           write(iprint,'(1X,132A)') (dash(i), i=1,width)
         end do

       end if

999    if ((iprint.ne.iout).and.iprint.ne.0) then
         close (iprint)
         call io_setout(iout)
       end if
       call cmd_err(status,'IMAGE_PRINT','Failed')
       end
