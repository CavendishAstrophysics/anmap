C
C
*+ plot_ctlin

       subroutine plot_ctlin(status)
C      -----------------------------
C
C Define linear contours
C Updated:
C  error status
       integer    status
C
C The user is prompted to add contours to the contour list; linearly
C spaced contours are added to the list
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '/mrao/include/iolib_functions.inc'

C contour increment variables
       real*4     c_start, c_incre, c_level, c_max, c_max0
C test variables
       real*4     test_1, test_2
C counters
       integer    nc, n

C check status on entry
       if (status.ne.0) return

C read start, increment and end
       if (.not.map_defined) then
         call cmd_wrerr('LINEAR-CONTOURS','no map set')
         return
       end if
       c_start = -1.0e+30
       c_incre = -1.0e+30
       call io_getr('Start-level : ',' ',c_start,status)
       if (c_start.lt.-1.0e+25) return
       call io_getr('Increment : ',' ',c_incre,status)
       if (c_incre.lt.-1.0e+25) return
       if (abs(c_incre).lt.1.0e-10) return
       c_max = data_range(2,imap_current)
       c_max0 = c_max
       call io_getr('Maximum : ','*',c_max,status)
       if (abs(c_max-c_max0).lt.1.0e-20) then

C .. user has pressed return to the request for contour levels
C .. x 1.3 allows for possible interpolation onto finer grid.
         c_max = c_max*1.3

       end if

       test_1 = c_max-c_start
       test_2 = test_1*c_incre
       if (test_2.lt.0.0) then

C .. user has typed an inconsistent set of max/min and increment -- tell
C .. the user about their error:
         call cmd_wrerr('LINEAR-CONTOURS',
     >                   'Invalid MAX/MIN and increment')
         return

       end if

       if (status.ne.0) goto 999

C define contour levels
       c_level = c_start
       nc = 0
       do while (c_level.le.c_max .and. nc.ne.-1 )

C add level
         nc = -1
         do n=1,max_contours
           if (contour_status(n).ne.0) then
             if (c_level.eq.contour_list(n) .and.
     *           abs(contour_status(n)).eq.imap_current) then
               nc=n
             endif
           endif
         enddo
         if (nc.eq.-1) then
           n = 1
           do while (n.le.max_contours .and. nc.eq.-1)
             if (contour_status(n).eq.0) nc = n
             n = n + 1
            enddo
         endif
         if (nc.ne.-1) then
           contour_list(nc) = c_level
           if (contour_status(nc).eq.0) then
               contour_status(nc) = imap_current
           endif
           if (c_level.lt.0.0) then
             contour_type(nc) = negative_style
           elseif (c_level.gt.0.0) then
             contour_type(nc) = positive_style
           else
             if ( io_yesno('Include zero-level (yes/no) ? ','no',
     *            status )) then
                contour_type(nc) = zero_style
             else
                contour_status(nc) = 0
             endif
           endif
         endif
C .. increment contour level
         c_level = c_level + c_incre
       end do


999    continue
       call cmd_err(status,'LINEAR-CONTOURS',' ')
       end
