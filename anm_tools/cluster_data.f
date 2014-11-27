C CLUSTER_DATA transform cluster results to produce 1D data files
C
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

       integer     imax, jmax
       integer     clist(5000), dlist(5000)
       real*4      slist(5000)
       real*4      val(2,5000)
       real*4      cdist(5000), t1dist(5000)

       integer     iunit,i,j,s,ii,jj
       real*4      pp(3,5000)

       character  file*(iolen_file), ofile*(iolen_file)

C setup status and initialise image arrays
       s = 0
       do i=1,5000
         cdist(i) = 0.0
         t1dist(i) = 0.0
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
         if (jj.gt.0) then
           t1dist(i) = clist(1)
         else
           t1dist(i) = 0.0
         endif
         do j=1,jj
           cdist(j) = cdist(j)+dlist(j)
         enddo
       enddo
       close (iunit)

C write out data files
       ofile = 't1d_'//file(1:chr_lenb(file))
       call io_opefil(iunit,ofile(1:chr_lenb(ofile)),'WRITE',0,s)
       write(iunit,*)'%ndata ',imax
       write(iunit,*)'%ncols 2'
       do i=1,imax
          write(iunit,*) val(1,i), t1dist(i)
       enddo
       close(iunit)

       ofile = 'cld_'//file(1:chr_lenb(file))
       call io_opefil(iunit,ofile(1:chr_lenb(ofile)),'WRITE',0,s)
       write(iunit,*)'%ndata ',jmax
       write(iunit,*)'%ncols 3'
       do j=1,jmax
          write(iunit,*) j, cdist(j), cdist(j)/j
       enddo
       close(iunit)

       end











