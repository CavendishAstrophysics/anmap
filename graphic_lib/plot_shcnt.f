

*+ plot_shcnt

       subroutine plot_shcnt(status)
C      -----------------------------
C
C Display settting of contour levels
C
C Updated:
C   error status
       integer       status
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C counters
       integer    i, n, nn, ns, np, nc
C output buffer
       real*4     buffer(8)
C output unit
       integer    iout

C check status on entry
       if (status.ne.0) return

       call io_enqout(iout)
       do ns=1,max_contour_styles
         nc = 0
         np = 0
         do n=1,max_contours
           if (contour_status(n).ne.0 .and.
     *         contour_type(n).eq.ns) then
             nc = nc + 1
             if (contour_status(n).lt.0) then
               np = np + 1
             endif
           endif
         enddo
         if (nc.gt.0) then
           write(iout,100) ns, nc, np
 100       format(1X,'Contours of style ',I1/
     *            1X,'Number of contours = ',I3,' plotted = ',I3/
     *            1X,'--------------------------------------')
           call graphic_show_line_opt(contour_styles(1,ns),
     *                                status)
           nn = 0
           do n=1,max_contours
             if (contour_status(n).ne.0 .and.
     *           contour_type(n).eq.ns) then
               nn = nn + 1
               buffer(nn) = contour_list(n)
               if (nn.eq.8) then
                 write(iout,110) (buffer(i), i=1,8)
                 nn = 0
               endif
             endif
           enddo
           if (nn.gt.0) then
              write(iout,110) (buffer(i), i=1,nn)
           endif
         endif
       enddo
 110   format(1X,8(1PE10.2))

       if (image_opt) then
         write(iout,120)image_max,image_min
       end if
120    format(1X/1X,'Grey Scale Defined'/
     >           1X,'------------------'/
     >           1X,'  Minimum = ',1PE12.3/
     >           1X,'  Maximum = ',1PE12.3)

       end
