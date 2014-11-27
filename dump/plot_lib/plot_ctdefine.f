C
C
*+ plot_ct_define

       subroutine plot_ctdefine(status)
C      --------------------------------
C
C Specify contour levels
C
C Updated:
C  error status
       integer    status
C
C The user is prompted to add contours to the contour list,
C the line style to be used for the contours is also supplies
C by the user specifying one of the "max_contour_styles" styles.
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C number of contour in contour list and style to use
       integer   nc, style
C current contour level
       real*4    level
C input loop control
       logical   input
C counter
       integer   n, nread

       if (status.ne.0) return

C read in style
       call io_geti( 'Style (1-5) : ','1',
     *               style, status )
       if (style.lt.1 .or. style.gt.5) then
         style = 1
       endif
C read in a list of levels
       input = .true.
       nread = 0
       do while (input)
         level = -1.0e+40
         call io_getr('Contour(s) : ',' ',level,status)
         input = level.gt.-1.0e+30 .and. status.eq.0

C .. store this contour level
         if (input) then
           nc = -1
           do n=1,max_contours
             if (contour_status(n).ne.0) then
               if (level.eq.contour_list(n) .and.
     *             abs(contour_status(n)).eq.imap_current) then
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
             contour_list(nc) = level
             if (contour_status(nc).eq.0) then
               contour_status(nc) = imap_current
             endif
             contour_type(nc) = style
           else
             input = .false.
             call cmd_wrerr('CONTOURS','No. levels exceeded')
           end if
         end if
       end do

       call cmd_err(status,'CONTOURS',' ')

       end
