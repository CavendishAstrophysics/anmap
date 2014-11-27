C
C F77 utility routines to access image files
C

       subroutine ic_imgfreaddefn( file, type, defn, status )
C      ------------------------------------------------------
C
C Read standard definition information
C
C Given:
C    file name
       character*128   file
C    data file type
       integer         type
C
C Updated:
C    definition information
       integer   defn(*)
C    error status
       integer   status
C
C-
       include 'ic.inc'
       integer   chr_lenb
       integer   iu, i, minirt(12)
       character ff*128

       if (status.ne.0) return

       if (type.eq.type_map .or. type.eq.0) then
          call opemap( iu, file(1:chr_lenb(file)), 'read', 0, status)
          call rdredt( iu, 0, status )
          call enminirt( minirt, status )
          if (status.eq.0) close( iu )
          ndims = 2
          ndata = minirt(5)*minirt(6)
          dtype = minirt(7)
          blank = minirt(8)
          xdim = minirt(5)
          ydim = minirt(6)
          zdim = 1
          tdim = 1
          vdim = 1
          x1 = minirt(1)
          x2 = minirt(2)
          y1 = min( minirt(3), minirt(4))
          y2 = max( minirt(3), minirt(4))
          z1 = 1
          z2 = 1
          t1 = 1
          t2 = 1
          v1 = 1
          v2 = 1
          do i=1,len_defn
             defn(i) = image_defn(i)
          enddo

       elseif (type.eq.type_df) then
          ff = file(1:chr_lenb(file))//'/defn'
          call io_opefil( iu, ff(1:chr_lenb(ff)), 'read', 0, status)
          read(iu,*, end=999) (image_defn(i), i=1,len_defn-2)
          read(iu,*, end=999) blank
          read(iu,*, end=999) norm
          do i=1,len_defn
             defn(i) = image_defn(i)
          enddo
          if (status.eq.0) close( iu )

       elseif (type.eq.type_fits) then
          call fits_opmap(iu,file(1:chr_lenb(file)),'READ',0,status)
          call fits_rredt(iu,1,0,status)
          call enminirt( minirt, status )
          if (status.eq.0) close( iu )
          ndims = 2
          ndata = minirt(5)*minirt(6)
          dtype = minirt(7)
          blank = minirt(8)
          xdim = minirt(5)
          ydim = minirt(6)
          zdim = 1
          tdim = 1
          vdim = 1
          x1 = minirt(1)
          x2 = minirt(2)
          y1 = min( minirt(3), minirt(4))
          y2 = max( minirt(3), minirt(4))
          z1 = 1
          z2 = 1
          t1 = 1
          t2 = 1
          v1 = 1
          v2 = 1
          do i=1,len_defn
             defn(i) = image_defn(i)
          enddo

       endif
       return
 999   status = 1
       end

       subroutine ic_imgfreaddata( file, type, defn, 
     *                             data, header, status )
C      --------------------------------------------------
C
C Read standard definition information
C
C Given:
C    file name
       character*128   file
C    data file type
       integer         type
C
C Updated:
C    definition information
       integer   defn(*)
C    data array
       real*4    data(*)
C    header record
       integer   header(*)
C    error status
       integer   status
C
C-
       include 'ic.inc'
       integer   chr_lenb
       integer   iu, iblock, iword
       character ff*128

       if (status.ne.0) return

       if (type.eq.type_map .or. type.eq.0) then
          call opemap( iu, file(1:chr_lenb(file)), 'read', 0, status)
          call rdredt( iu, 0, status )
          call rdmap( iu, data, status )
          call dpredt( header, status )
          if (status.eq.0) close( iu )

       elseif (type.eq.type_df) then
          ff = file(1:chr_lenb(file))//'/data'
          iword = defn(2)
          iblock = iword*4
          call io_operan( iu, ff(1:chr_lenb(ff)), 'read', 
     *                    iblock, 0, status)
          call io_rdfile( iu, 1, data, iword, status )
          if (status.eq.0) close( iu )

       elseif (type.eq.type_fits) then
          call fits_opmap(iu,file(1:chr_lenb(file)),'READ',1,status)
          call fits_rredt(iu,1,0,status)
          call fits_rdmap(iu,1,data,status)
          call dpredt( header, status )
          if (status.eq.0) close( iu )

       endif
       end

       subroutine ic_imgfwritedata( file, type, defn, 
     *                              data, header, status )
C      ---------------------------------------------------
C
C Read standard definition information
C
C Given:
C    file name
       character*128   file
C    data file type
       integer         type
C
C Updated:
C    definition information
       integer   defn(*)
C    data array
       real*4    data(*)
C    header record
       integer   header(*)
C    error status
       integer   status
C
C-
       include 'ic.inc'
       integer   chr_lenb
       integer   i, iu, iblock, iword
       character ff*128

       if (status.ne.0) return

       if (type.eq.type_map .or. type.eq.0) then
          call ldredt( header, status )
          call opemap( iu, file(1:chr_lenb(file)), 'write', 0, status)
          call wrredt( iu, 0, status )
          call wrmap( iu, data, status )
          if (status.eq.0) close( iu )

       elseif (type.eq.type_df) then
          ff = file(1:chr_lenb(file))//'/defn'
          call io_opefil( iu, ff(1:chr_lenb(ff)), 'write', 0, status)
          do i=1,len_defn
             image_defn(i) = defn(i)
          enddo
          do i=1,len_defn-2
             write(iu,*) image_defn(i)
          enddo
          write(iu,*) blank
          write(iu,*) norm
          if (status.eq.0) close( iu )
          ff = file(1:chr_lenb(file))//'/data'
          iword = defn(2)
          iblock = iword*4
          call io_operan( iu, ff(1:chr_lenb(ff)), 'write', 
     *                    iblock, 0, status)
          call io_wrfile( iu, 1, data, iword, status )
          if (status.eq.0) close( iu )

       elseif (type.eq.type_fits) then
          call ldredt( header, status )
          call fits_opmap(iu,file(1:chr_lenb(file)),'WRITE',1,status)
          call fits_wredt(iu,0,status)
          call fits_wrmap(iu,data,status)
          if (status.eq.0) close( iu )

       endif

       end
