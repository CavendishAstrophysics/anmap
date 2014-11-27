
C Clusters: Isolate and analyse clusters within an image
C
C This is a standalone program for use within Anmap.
C
C Clusters determines the number of clusters in an image within a user
C supplied range of image values and reports the results as statistics
C for those clusters.
C
C
*-
       include '../include/anmap_sys_pars.inc'
       include '../include/anmap_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C define work array
       integer       nm, nb, size
       parameter    (size =1024)
       integer       msize, bsize
       parameter    (msize = size*size)
       parameter    (bsize=size)
       parameter    (nm=1)
       parameter    (nb=1)
       real*4        map_array( nm*msize + nb*bsize )
       integer       image( size, size )
C error status
       integer       s
       integer       imap, ip_map
C option code
       integer       opt
C counter
       integer       i, j, imax, jmax, iunit
C mini redtape for map size etc.
       integer       minirt(8), izmnx(8)
       real*4        zmnx(4)
C list of increments
       real*4        start_val, end_val, incr_val, width_val,
     *               val1, val2, p0, p1, v
C flags and lists
       integer       max_list
       parameter    (max_list=128*128)
       integer       flag(msize)
       integer       list(2*max_list), nlist(2*max_list),
     *               clist(max_list)
       real*4        rlist(max_list)
C number of clusters
       integer       nc
C length of cummulative distribution
       integer       nl
C results file name
       character     file*(iolen_file)

C perform standard initialization
       s = 0
       call anm_start( 0,nm,msize,nb,bsize,s )

C read input map: imap is the catalogue identifier
*       call map_getmap('Mask-Catalogue-entry : ',
*     *                 'Default_Map','READ',imap,s)
*       call map_alloc_in(imap,'DIRECT',map_array,ip_map,s)
*       call enminirt(minirt,s)
*       call cluster_getmask(minirt,map_array(ip_map),image,s)
*       call map_end_alloc(imap,map_array,s)
       call map_getmap('Image-Catalogue-entry : ',
     *                 'Default_Map','READ',imap,s)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,s)
       call enminirt(minirt,s)
       call scnmap2(minirt,map_array(ip_map),minirt,zmnx,izmnx,s)
       if (s.ne.0) goto 999

C get user input
       start_val = zmnx(2)
       end_val = zmnx(1)
       call io_geti('Join-type (1|2) : ','1',opt,s)
       call io_getr('Start-level : ','*',start_val,s)
       call io_getr('End-level : ','*',end_val,s)
       incr_val = (end_val-start_val)/9.0
       call io_getr('Increment-level : ','*',incr_val,s)
       width_val = incr_val
       call io_getr('Width : ','*',width_val,s)
       file = ' '
       call io_getwrd('Results-file : ','results.dat',file,i,s)
       if (s.ne.0) goto 999

C setup for calculations
       imax = 0
       val1 = start_val
       val2 = val1 + width_val
       do while (val1.le.end_val)
         imax = imax + 1
         val1 = val1 + incr_val
       enddo
       val1 = start_val
       val2 = val1 + width_val
       i = 0
       jmax = 0
       call io_opefil(iunit,file(1:chr_lenb(file)),'WRITE',0,s)
       write(iunit,*) imax
       do while (val1.le.end_val)
         call cluster_setup(minirt,map_array(ip_map),val1,val2,flag,s)
         call cluster_determine(minirt(5),minirt(6),minirt,opt,max_list,
     *                          flag,clist,list,nlist,nc,s)
         call cluster_analyse(clist,nc,max_list,list,nl,s)
         call cluster_shape(minirt(5),minirt(6),
     *                minirt,flag,clist,nc,max_list,rlist,nl,s)
         i = i + 1
         jmax = max(jmax,nl)
         write(iunit,*) i, val1, val2, nl
         write(iunit,*) (list(j), j=1,nl)
         do j=1,nl-1
           list(j) = list(j) - list(j+1)
         enddo
         write(iunit,*) (list(j), j=1,nl)
         write(iunit,*) (rlist(j), j=1,nl)
*         call cluster_percol(minirt,map_array,image,flag,opt,
*     *                max_list,list,nlist,val1,zmnx(1),p0,p1,v,s)
*         write(iunit,*)p0,p1,v
         val1 = val1 + incr_val
         val2 = val1 + width_val
       enddo
*       call io_opefil(iunit,general_results_file,'WRITE',0,s)
       close(iunit)

C check status value and report an error to the user
999    call map_end_alloc(imap,map_array,s)
       call cmd_err(s,'Clusters','Failed ')

C finally perform standard shut-down
       call anm_end( s )

       end
C
C
*+ cluster_setup

       subroutine cluster_setup(minirt,map_array,val_min,val_max,flag,s)
C       ----------------------------------------------------------------
C
C Initialise the flag array for use in the cluster analysis
C
C Given:
C   image mini redtape
       integer     minirt(8)
C   data array
        real*4     map_array(*)
C   range of data to accept
        real*4     val_min, val_max
C Returned:
C   flag data array
        integer    flag(*)
C Updated:
C   error status
         integer   s
C
C The data array is scanned and data points in the specified range are
C flagged in flag.
C
*-
       integer     i, np

       if (s.ne.0) return
       np = 0
       do i=1,minirt(5)*minirt(6)
         if (map_array(i).ge.val_min .and. map_array(i).le.val_max) then
           flag(i) = 0
           np = np + 1
         else
           flag(i) = -1
         endif
       enddo
       print *,'CLUSTER: found for: ',val_min,val_max,np,' pixels'
       call cmd_err(s,'cluster_setup',' ')
       end
C
C
*+ cluster_determine

       subroutine cluster_determine(id1,id2,minirt,opt,max_list,
     *                              flag,clist,list,nlist,nc,s)
C      --------------------------------------------------------
C
C Determine clusters in the supplied flag array and label all clusters
C
C Given:
C   image mini redtape
       integer     minirt(8)
C   option code to specify type of cluster determination
       integer     opt
C   size of list arrays
       integer     max_list
C Updated:
C   flag data array
        integer    id1, id2
        integer    flag(id1,id2)
C   cluster array -- sizes of clusters
        integer    clist(max_list)
C   work array for list buffering
        integer    list(2,max_list), nlist(2,max_list)
C Returned:
C   number of clusters
        integer    nc
C Updated:
C   error status
         integer   s
C
C The clusters in the flag array are determined according to the model type
C specified by opt -- supported options:
C
C    opt = 1     ::    use near neighbour joined pixels
C    opt = 2     ::    use near neighbour joined pixels and corner pixels
C
C All clusters are labelled from 1 to nc, the total number of clusters
C in the image (returned).
C
*-
       include '../include/anmap_errors.inc'

C Local variables
C   counters
       integer     np, nl, i, j, n, nnl, nn, ii, jj, l
C   cell adjacency table
       integer     lt, ijt(2,8)
       if (s.ne.0) return

C test option code
       if (opt.eq.1) then
         lt = 4
         ijt(1,1) = -1
         ijt(2,1) = 0
         ijt(1,2) = 1
         ijt(2,2) = 0
         ijt(1,3) = 0
         ijt(2,3) = 1
         ijt(1,4) = 0
         ijt(2,4) = -1
       else
         lt = 8
         ijt(1,1) = -1
         ijt(2,1) = -1
         ijt(1,2) = -1
         ijt(2,2) = 0
         ijt(1,3) = -1
         ijt(2,3) = 1
         ijt(1,4) = 1
         ijt(2,4) = -1
         ijt(1,5) = 1
         ijt(2,5) = 0
         ijt(1,6) = 1
         ijt(2,6) = 1
         ijt(1,7) = 0
         ijt(2,7) = 1
         ijt(1,8) = 0
         ijt(2,8) = -1
       endif

C count number of valid pixels
       np = 0
       do j=1,minirt(6)
         do i=1,minirt(5)
           if (flag(i,j).eq.0) then
             np = np + 1
           endif
         enddo
       enddo

C loop over all non-flagged pixels and do search
       nc = 0
       do while (np.gt.0)

C .. find new cluster
         nc = nc + 1
         if (nc.gt.max_list) then
           print *,'***(CLUSTER) List-stack overflow [nc] =',max_list
           s = err_clstk
           goto 999
         endif
         j = 1
         nl = 0
         list(1,1) = 0
         list(2,1) = 0
         do while (j.le.minirt(6) .and. nl.eq.0)
           i = 1
           do while (i.le.minirt(5) .and. nl.eq.0)
             if (flag(i,j).eq.0) then
               list(1,1) = i
               list(2,1) = j
               flag(i,j) = nc
               clist(nc) = 1
               nl = 1
               np = np - 1
             endif
             i = i + 1
           enddo
           j = j + 1
         enddo

C .. perform search on this cluster
         do while (nl.gt.0)
           nnl = 0
           do n=1,nl
             i = list(1,n)
             j = list(2,n)
             do l=1,lt
               ii = i + ijt(1,l)
               jj = j + ijt(2,l)
               if (ii.ge.1 .and. ii.le.minirt(5) .and.
     *             jj.ge.1 .and. jj.le.minirt(6)) then
                  if (flag(ii,jj).eq.0) then
                    np = np - 1
                    flag(ii,jj) = nc
                    nnl = nnl + 1
                    clist(nc) = clist(nc) + 1
                    if (nnl.gt.max_list) then
                       print *,'***(CLUSTER) List-stack overflow ',
     *                         '[nnl] =',max_list
                       s = err_clstk
                       goto 999
                    endif
                    nlist(1,nnl) = ii
                    nlist(2,nnl) = jj
                  endif
               endif
             enddo
           enddo
C ... copy new list to old
           nl = nnl
           do nn=1,nnl
             do n=1,2
               list(n,nn) = nlist(n,nn)
             enddo
           enddo
         enddo
       enddo
999    call cmd_err(s,'cluster_determine',' ')
       end
C
C
*+ cluster_analyse

       subroutine cluster_analyse(clist,nc,max_list,list,nl,s)
C      --------------------------------------------------------
C
C Analyse clusters and return cluster table
C
C Given:
C   cluster array -- sizes of clusters
        integer    clist(max_list)
C   number of clusters
        integer    nc
C   size of list arrays
       integer     max_list
C Returned:
C   cumulatize distribution of cells in clusters
        integer    list(max_list)
C   length of cumulatize distribution
        integer    nl
C Updated:
C   error status
         integer   s
C
*-
       include '../include/anmap_errors.inc'

C Local variables
C   counters
       integer     n, m

       if (s.ne.0) return

       nl = 0
       do n=1,nc
         nl = max(clist(n),nl)
       enddo
       if (nl.gt.max_list) then
         print *,'***(CLUSTER) List-stack overflow [nl] =',max_list
         s = err_clstk
         goto 999
       endif
       do m=1,nl
         list(m) = 0
         do n=1,nc
           if (clist(n).ge.m) then
             list(m) = list(m) + clist(n)
           endif
         enddo
       enddo

999    call cmd_err(s,'cluster_analyse',' ')
       end
C
C
*+ cluster_shape

       subroutine cluster_shape(id1,id2,minirt,flag,clist,nc,
     *                          max_list,rlist,nl,s)
C      ----------------------------------------------
C
C Analyse shape of clusters and return statistics in table
C
C Given:
C   miniredtape
        integer    minirt(8)
C   cluster flag image
        integer    id1, id2
        integer    flag(id1, id2)
C   cluster array -- sizes of clusters
        integer    clist(max_list)
C   number of clusters
        integer    nc
C   size of list arrays
       integer     max_list
C Returned:
C   cluster shape statistic
        real*4     rlist(max_list)
C   length of cumulatize distribution
        integer    nl
C Updated:
C   error status
         integer   s
C
*-
       include '../include/anmap_errors.inc'

C Local variables
C   counters
       integer     n, m, i, j
C   statistics
       real*4      ri, rj, ss, ss1

       if (s.ne.0) return

       nl = 0
       do n=1,nc
         nl = max(clist(n),nl)
       enddo
       if (nl.gt.max_list) then
         print *,'***(CLUSTER) List-stack overflow [nl] =',max_list
         s = err_clstk
         goto 999
       endif
       do m=1,nl
         rlist(m) = 0.0
       enddo
       do n=1,nc
          if (clist(n).ge.3) then
             ri = 0.0
             rj = 0.0
             do j=1,minirt(6)
               do i=1,minirt(5)
                 if (flag(i,j).eq.n) then
                    ri = ri + i
                    rj = rj + j
                 endif
               enddo
             enddo
             ri = ri/float(clist(n))
             rj = rj/float(clist(n))
             ss = 0.0
             do j=1,minirt(6)
               do i=1,minirt(5)
                 if (flag(i,j).eq.n) then
                   ss1 = (float(i)-ri)**2 + (float(j)-rj)**2
                   ss = ss + ss1
                 endif
               enddo
             enddo
             ss = sqrt(ss)/float(clist(n))
             rlist(clist(n)) = rlist(clist(n)) + ss
          endif
       enddo
       do m=3,nl
         ss = 0.0
         do n=1,nc
           if (clist(n).eq.m) ss = ss + 1.0
         enddo
         if (ss.gt.0.0) then
           rlist(m) = rlist(m)/ss
         endif
       enddo
       rlist(1) = 0.0
       rlist(2) = 0.5/sqrt(2.0)
999    call cmd_err(s,'cluster_shape',' ')
       end
C
C
*+ cluster_getmask

       subroutine cluster_getmask(minirt,map_array,image,s)
C      ----------------------------------------------------
C
C Initialise the mask array, image,  for use in the cluster analysis
C
C Given:
C   image mini redtape
       integer     minirt(8)
C   data array
        real*4     map_array(*)
C Returned:
C   mask array
        integer    image(*)
C Updated:
C   error status
         integer   s
C
C-
C local variables -- counters
       integer     i

       if (s.ne.0) return
       do i=1,minirt(5)*minirt(6)
         if (map_array(i).lt.-0.5) then
           image(i) = -1
         elseif (map_array(i).gt.0.5) then
           image(i) = 1
         else
           image(i) = 0
         endif
       enddo
       call cmd_err(s,'cluster_getmask','Failed')
       end
C
C
*+ cluster_pflag

       subroutine cluster_pflag(minirt,map_array,image,v1,v2,flag,s)
C      -------------------------------------------------------------
C
C Initialise the flag array for percolation calculation
C
C Given:
C   image mini redtape
       integer     minirt(8)
C   data array
        real*4     map_array(*)
C   mask array
        integer    image(*)
C   range of acceptable data
        real*4     v1,v2
C Returned:
C   flag array
        integer    flag(*)
C Updated:
C   error status
         integer   s
C
C-
C local variables
       integer     i

       if (s.ne.0) return
       do i=1,minirt(5)*minirt(6)
         flag(i) = image(i)
         if (image(i).eq.0) then
           if (map_array(i).ge.v1 .and. map_array(i).le.v2) then
             flag(i) = 2
           endif
         endif
       enddo
       call cmd_err(s,'cluster_pflag','Failed')
       end
C
C
*+ cluster_pcheck

       subroutine cluster_pcheck(id1, id2, minirt,flag,opt,
     *                           max_list,list,nlist,iper,s)
C      -----------------------------------------------------
C
C Check for a percolating cluster in the flag array
C
C Given:
C   image mini redtape
       integer     minirt(8)
C   flag array
        integer    id1, id2
        integer    flag(id1, id2)
C   connection option
        integer    opt
C   maximum length of list arrays
        integer    max_list
C   list (work) arrays
        integer    list(2,max_list), nlist(2,max_list)
C Returned:
C   flag to indicate percolation (1) or not (0)
        integer    iper
C Updated:
C   error status
         integer   s
C
C-
       include '../include/anmap_errors.inc'

C Local variables
C   counters
       integer     nl, i, j, n, nnl, nn, ii, jj, l
C   cell adjacency table
       integer     lt, ijt(2,8)
       if (s.ne.0) return

C test option code
       if (opt.eq.1) then
         lt = 4
         ijt(1,1) = -1
         ijt(2,1) = 0
         ijt(1,2) = 1
         ijt(2,2) = 0
         ijt(1,3) = 0
         ijt(2,3) = 1
         ijt(1,4) = 0
         ijt(2,4) = -1
       else
         lt = 8
         ijt(1,1) = -1
         ijt(2,1) = -1
         ijt(1,2) = -1
         ijt(2,2) = 0
         ijt(1,3) = -1
         ijt(2,3) = 1
         ijt(1,4) = 1
         ijt(2,4) = -1
         ijt(1,5) = 1
         ijt(2,5) = 0
         ijt(1,6) = 1
         ijt(2,6) = 1
         ijt(1,7) = 0
         ijt(2,7) = 1
         ijt(1,8) = 0
         ijt(2,8) = -1
       endif

C construct initial list
       iper = 0
       nl = 0
       do j=1,minirt(6)
         do i=1,minirt(5)
           if (flag(i,j).eq.1) then
             nl = nl + 1
             if (nl.gt.max_list) then
               print *,'***(CLUSTER) List-stack overflow in PERCOL:',
     *                 ' [nl] =',max_list
               s = err_clstk
               goto 999
             endif
             list(1,nl) = i
             list(2,nl) = j
           endif
         enddo
       enddo

C loop and test for percolating cluster by repeated adjacency check
       do while (nl.gt.0 .and. iper.eq.0)
         nnl = 0
         do n=1,nl
           i = list(1,n)
           j = list(2,n)
           do l=1,lt
             ii = i + ijt(1,l)
             jj = j + ijt(2,l)
             if (ii.ge.1 .and. ii.le.minirt(5) .and.
     *           jj.ge.1 .and. jj.le.minirt(6)) then
                if (flag(ii,jj).eq.2) then
                    flag(ii,jj) = 1
                    nnl = nnl + 1
                    if (nnl.gt.max_list) then
                       print *,'***(CLUSTER) List-stack overflow ',
     *                         ' PERCOL-1 [nnl] =',max_list
                       s = err_clstk
                       goto 999
                    endif
                    nlist(1,nnl) = ii
                    nlist(2,nnl) = jj
                elseif (flag(ii,jj).eq.-1) then
                    iper = 1
                endif
             endif
           enddo
         enddo
C ... copy new list to old
         nl = nnl
         do nn=1,nnl
           do n=1,2
             list(n,nn) = nlist(n,nn)
           enddo
         enddo
       enddo
999    call cmd_err(s,'cluster_pcheck','Failed')
       end
C
C
*+ cluster_percol

       subroutine cluster_percol(minirt,map_array,image,flag,opt,
     *                           max_list,list,nlist,vmin,vmax,
     *                           p0,p1,v,s)
C      --------------------------------------------------------
C
C Initialise the flag array for percolation calculation
C
C Given:
C   image mini redtape
       integer     minirt(8)
C   data array
        real*4     map_array(*)
C   mask array
        integer    image(*)
C   flag array
        integer    flag(*)
C   network connection option
        integer    opt
C   work arrays -- lists
        integer    max_list, list(*), nlist(*)
C   minimum and maximum of range for percolation test
        real*4     vmin, vmax
C Returned:
C   percolation threshold -- within supplied range and total
        real*4     p0, p1, v
C Updated:
C   error status
         integer   s
C
C-
C local variables
        integer    ip, i
        real*4     v1, v2

C cgeck status on entry
       if (s.ne.0) return

C construct inital percolating test using full range
       p0 = -1.0
       p1 = -1.0
       v = -1.0
       call cluster_pflag(minirt,map_array,image,vmin,vmax,flag,s)
       call cluster_pcheck(minirt(5),minirt(6),
     *              minirt,flag,opt,max_list,list,nlist,ip,s)

C test to see if there is a percolating cluster
       if (ip.eq.1) then
C .. do binary chop to establish percolating range
         v1 = vmin
         v2 = vmax
         v = v1 + 0.5*(v2-v1)
         do i=1,10
           call cluster_pflag(minirt,map_array,image,
     *                        vmin,v,flag,s)
           call cluster_pcheck(minirt(5),minirt(6),minirt,flag,opt,
     *                         max_list,list,nlist,ip,s)
           if (ip.eq.1) then
             v2 = v
           else
             v1 = v
           endif
           v = v1 + 0.5*(v2-v1)
         enddo
         call cluster_pfrac(minirt,map_array,image,vmin,v,p0,p1,s)
       endif
999    call cmd_err(s,'cluster_percol','Failed')
       end
C
C
*+ cluster_pfrac

       subroutine cluster_pfrac(minirt,map_array,image,v1,v2,p0,p1,s)
C      --------------------------------------------------------------
C
C Calculate fraction of pixels in range v1 --> v2
C
C Given:
C   image mini redtape
       integer     minirt(8)
C   data array
       real*4      map_array(*)
C   mask array
       integer     image(*)
C   data range to use
       real*4      v1, v2
C Returned:
C   fraction:  p0 = In(v1 --> v2 )/In(>v1 )
C              p1 = In(v1 --> v2 )/In(all)
      real*4       p0, p1
C Updated:
C   error status
      integer      s
C
C-
C local variables
       integer     nr, n0, n1
       integer     i

       if (s.ne.0) return
       nr = 0
       n0 = 0
       n1 = 0
       do i=1,minirt(5)*minirt(6)
         if (image(i).eq.0) then
           n1 = n1 + 1
           if (map_array(i).ge.v1) then
              n0 = n0 + 1
              if (map_array(i).le.v2) then
                 nr = nr + 1
              endif
           endif
         endif
       enddo
       p0 = float(nr)/float(n0)
       p1 = float(nr)/float(n1)
999    call cmd_err(s,'cluster_pfrac','Failed')
       end
