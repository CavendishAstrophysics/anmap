C
C
*+ plot_ctlog

       subroutine plot_ctlog(status)
C      -----------------------------
C
C Define logarithmic contours
C
C Updated:
C   error status
       integer      status
C
C The user is prompted to specify logarithmic-like contour
C levels.  A start, factor, end format is required; end may be
C omitted and defaults to the maximum value in the current map.
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C number of contour in contour list and style to use
       integer   nc
C current contour level
       real*4    level
C counter
       integer   n
C contour increment variables
       real*4   c_start, c_fact, c_max, c_max_o, c_level

C check status on entry
       if (status.ne.0) return

C read start, factor and end
       if (.not.map_defined) then
         call cmd_wrerr('LOGARITHMIC-CONTOURS','No map set')
         return
       end if
       c_start = -1.0e+30
       c_fact = -1.0e+30
       call io_getr('Start-level : ',' ',c_start,status)
       if (c_start.lt.-1.0e+25) return
       call io_getr(' Factor : ',' ',c_fact,status)
       if (c_fact.lt.-1.0e+25) return
       if (abs(c_fact).lt.1.0e-10) return
       c_max = data_range(2,imap_current)
       c_max_o = c_max
       call io_getr('Maximum : ','*',c_max,status)

C check for consistency in user input
       if (c_max.lt.c_start) then
         call cmd_wrerr('LOGARITHMIC-CONTOURS',
     *                   'Max-level < Min-level')
         return
       else if (c_fact.lt.1.0) then

         call cmd_wrerr('LOGARITHMIC-CONTOURS',
     *        'Factor < 1.0 (decreasing or oscillating values')
         return
       end if

       if (abs(c_max-c_max_o).lt.1.0e-20) then
C .. allow for possible interpolation of map in default case
         c_max = c_max*1.3
       end if

       if (status.ne.0) goto 999

C define contour levels
       c_level = abs(c_start)
       nc = 0
       do while (c_level .le. c_max .and. nc.ne.-1 )
C .. add negative level
         nc = -1
         do n=1,max_contours
           if (contour_status(n).ne.0) then
             if (level.eq.contour_list(n) .and.
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
           contour_list(nc) = -c_level
           if (contour_status(nc).eq.0) then
               contour_status(nc) = imap_current
           endif
           contour_type(nc) = negative_style
         endif
C .. add positive level
         nc = -1
         do n=1,max_contours
           if (contour_status(n).ne.0) then
             if (level.eq.contour_list(n) .and.
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
           contour_type(nc) = positive_style
         endif
C .. increment contour level
         c_level = abs(c_level*c_fact)
       end do

999    continue
       call cmd_err(status,'LOGARITHMIC-CONTOURS',' ')
       end
