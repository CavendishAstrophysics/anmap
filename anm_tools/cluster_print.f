C CLTRANS transform cluster results to a number of image file for
C         further analysis.
C
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

       integer     imax, jmax
       integer     clist(5000), dlist(5000), nl(5000), 
     *             imi1(100,5000), imi2(100,5000)
       real*4      slist(5000), imr(100,5000)
       real*4      val(2,5000)

       integer     iunit,i,j,s,ii,jj,i1,i2
       real*4      pp(3,5000)

       character  file*(iolen_file), ofile*(iolen_file)

C setup status and initialise image arrays
       s = 0
       do j=1,100
         do i=1,100
            imi1(i,j) = 0
            imi2(i,j) = 0
            imr(i,j)  = 0.0
         enddo
       enddo

C initialise I/O and get file name
       call io_initio
       file = ' '
       call io_getwrd('Results-file : ','results.dat',file,i,s)

C read data file
       call io_opefil(iunit,file(1:i),'READ',0,s)
       read(iunit,*) imax
       jmax = 0
       do i=1,imax
         read(iunit,*) ii,val(1,i),val(2,i),jj
         jmax = max(jmax,jj)
         read(iunit,*) (clist(j), j=1,jj)
         read(iunit,*) (dlist(j), j=1,jj)
         read(iunit,*) (slist(j), j=1,jj)
         read(iunit,*) (pp(j,i), j=1,3)
         do j=1,jj
           imi1(i,j) = clist(j)
           imi2(i,j) = dlist(j)
           imr(i,j)  = slist(j)
         enddo
       enddo
       close (iunit)

C determine data to write out
       do j=1,jmax
         nl(j) = 0
         do i=1,imax
           nl(j) = nl(j) + imi2(i,j)
         enddo
       enddo

C write out image files
       ii = imax/20 + 1
       ofile = 'cd_'//file(1:chr_lenb(file))
       call io_opefil(iunit,ofile(1:chr_lenb(ofile)),'WRITE',0,s)
       write(iunit,*)imax,jmax,' ',ofile(1:chr_lenb(ofile))
       do jj=1,ii
       i1 = (jj-1)*20 + 1
       i2 = min(imax,20*jj)
       write(iunit,'(1X)')
       write(iunit,'(''Page: '',I2)') jj
       write(iunit,'(6X,20F6.3)') (val(1,i),i=i1,i2)
       do j=1,jmax
         if (nl(j).gt.0) then
          write(iunit,'(I6,20I6)') j, (imi1(i,j), i=i1,i2)
         endif
       enddo
       enddo
       close(iunit)

       ofile = 'dd_'//file(1:chr_lenb(file))
       call io_opefil(iunit,ofile(1:chr_lenb(ofile)),'WRITE',0,s)
       write(iunit,*)imax,jmax,' ',ofile(1:chr_lenb(ofile))
       do jj=1,ii
       i1 = (jj-1)*20 + 1
       i2 = min(imax,20*jj)
       write(iunit,'(1X)')
       write(iunit,'(''Page: '',I2)') jj
       write(iunit,'(6X,20F6.3)') (val(1,i),i=i1,i2)
       do j=1,jmax
         if (nl(j).gt.0) then
          write(iunit,'(I6,20I6)') j, (imi2(i,j), i=i1,i2)
         endif
       enddo
       enddo
       close(iunit)

       ofile = 'sd_'//file(1:chr_lenb(file))
       call io_opefil(iunit,ofile(1:chr_lenb(ofile)),'WRITE',0,s)
       write(iunit,*)imax,jmax,' ',ofile(1:chr_lenb(ofile))
       do jj=1,ii
       i1 = (jj-1)*20 + 1
       i2 = min(imax,20*jj)
       write(iunit,'(1X)')
       write(iunit,'(''Page: '',I2)') jj
       write(iunit,'(6X,20F6.3)') (val(1,i),i=i1,i2)
       do j=1,jmax
         if (nl(j).gt.0) then
          write(iunit,'(I6,20F6.3)') j, (imr(i,j), i=i1,i2)
         endif
       enddo
       enddo
       close(iunit)

       ofile = 'pp_'//file(1:chr_lenb(file))
       call io_opefil(iunit,ofile(1:chr_lenb(ofile)),'WRITE',0,s)
       write(iunit,*)'%ndata ',imax
       write(iunit,*)'%ncols 4'
       write(iunit,*)'%title ',ofile(1:chr_lenb(ofile))
       do i=1,imax
         write(iunit,*)val(1,i), (pp(j,i), j=1,3)
       enddo
       close(iunit)

       end








