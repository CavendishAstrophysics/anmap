C
C
*+ plot_dotext

       subroutine plot_dotext(status)
C      ------------------------------
C
C Add text to contour/grey-scale plots
C
C Updated:
C   error status
       integer    status
C
C Standard text is added to contour/grey-scale plots; the standard text
C options are:
C   display of contour levels
C   display of date and program name
C   display of title
C
C Each option is controlled by the setting of *_opt and *_done
C flags passed in the standard data structure.  Plot refreshing is
C supported by this routine.
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C view-port and scaling
       real*4    vx1, vx2, vy1, vy2, vx_size, vy_size
C pointers to start of text output in world coordinates
       real*4    xstart, ystart, ydrop, xend
C loop counters
       integer   nc, ic
C lengths of character strings
       integer   len_tit, len_ctit, len_nam, len_uni, len_prog,
     *           len_s, len_date, chr_lenb
C contour information
       integer   index_cont(max_contours)
       real*4    clevs(max_contours)
C information from data
       real*4    freq
       integer   poln, hist_date(3)
       character name*16, exunit*16
C Character variables
       character string*20, c_title*80, c_date*80, program_name*40
C character size
       real*4    ch_abs
C test on drop in y
       real*4    wy_test

C check status on entry
       if (status.ne.0) return

C switch to deferred update state
       call pgbbuf

C define plot window and size
       call graphic_set_view_port( image_defn, status )
       call pgqvp(0,vx1,vx2,vy1,vy2)
       vy1 = frame_vp(3)
       vy2 = frame_vp(4)
       call pgsetvp(vx1,vx2,vy1,vy2)
       call pgsetwin(0.0,1.0,0.0,1.0)

C define extreme right hand side of the plot
       vx_size = vx2-vx1
       vy_size = vy2-vy1
       xend    = 0.98

C save and reset plot size and font
       call graphic_set_text_opt(annotations_text_style,status)
       call graphic_copy_text_opt(annotations_text_style,
     *                            text_opt,status)

C define inter-line spacing for text
       ch_abs = text_opt_size *
     *          max(1.0/vx_size,1.0/vy_size)/40.0
       ydrop  = ch_abs*annotation_drop*1.05

C contour levels
       if ( (cont_opt.and..not.cont_done) .or.
     *      (plot_refresh.and.cont_done.and.cont_opt) ) then
C .. output title then levels
         ystart  = 1.025 - ydrop
         wy_test = -(vy1-0.025)/vy_size
         call entype(freq,poln,name,exunit,status)
         len_nam = chr_lenb(name)
         len_uni = chr_lenb(exunit)
         call pgptext(xend,ystart,0.0,1.0,name(1:len_nam))
         ystart = ystart - ydrop
         write(c_title,100)exunit(1:len_uni)
100      format('(',a,')')
         len_ctit = chr_lenb(c_title)
         call pgptext(xend,ystart,0.0,1.0,c_title(1:len_ctit))
         ystart = ystart - ydrop

C .. sort levels
         nc = 0
         do ic=1,max_contours
           if (contour_status(ic).lt.0) then
             nc = nc + 1
             clevs(nc) = contour_list(ic)
             call util_qindxr(clevs,index_cont,nc)
           endif
         enddo

C .. output total of levels
         if (nc.gt.0) then
           write(string,150)nc
  150      format(i3,' contours')
           call pgptext(xend,ystart,0.0,1.0,string(1:12))
           ystart = ystart - ydrop
         endif

C .. do the plotting
         do ic = 1,nc
           if (ystart.gt.wy_test) then
             call char_setlev(clevs(index_cont(ic)),
     *                        string,len_s)
             call pgptext(xend,ystart,0.0,1.0,string(1:len_s))
             ystart = ystart - ydrop
           end if
         end do
         cont_done = .true.
       end if

C date
       if ((date_opt.and..not.date_done).or.
     *     (plot_refresh.and.date_done.and.date_opt)) then
         ystart = 1.025
         xstart = xend
         c_date = ' '
         call enhist(hist_date,program_name,status)
         len_prog = chr_lenb(program_name)
         write(c_date,200)uv_range,hist_date,program_name(1:len_prog)
200      format('UV-range: ',4I5,' Created: ',
     *          I2,':',I2,':',I4,' By: ',A)
         len_date = chr_lenb(c_date)
         call pgptext(xstart,ystart,0.0,1.0,c_date(1:len_date))
         date_done = .true.
       end if

C title
       call graphic_set_text_opt(title_text_style,status)
       if ((title_opt.and..not.title_done).or.
     *     (plot_refresh.and.title_done.and.title_opt)) then
         xstart = 0.0
         ystart = 1.025
         len_tit = chr_lenb(title_plot)
         call pgptext(xstart,ystart,0.0,0.0,title_plot(1:len_tit))
         title_done = .true.
       end if

C return to immediate update state
       call pgebuf

C tidy up and report any errors
       call plot_frset( status )
       call cmd_err(status,'PLOT_DOTEXT',' ')

       end
