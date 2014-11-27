C
C
*+ plot_crosses

       subroutine plot_crosses(status)
C      -------------------------------
C
C Plot crosses from a crosses file
C
C Updated:
C   error status
       integer    status
C
*-
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '../include/anmap_sys_pars.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C local variables
C   position of cross
       real*8       ra, dec, du, dv, ra0, dec0,
     *              epoch, skew, usamp, prang
       integer      iunit, iproj, istyle
       real*4       up, vp, rsize
C   text for cross
       character    text*80, line*80



C check status on entry
       if (status.ne.0) return
       if (crosses_opt.eq.0) return

C open crosses file
       call io_opefil(iunit,crosses_file,'read',0,status)
       if (status.ne.0) goto 999

C .. loop reading from input unit -- either terminal or the
C    crosses file. When the routine comes to the end of the
C    crosses file the file is automatically closed and the
C    input unit reset to terminal input.
       call enmapj( iproj, usamp, skew, epoch, prang, status )
       do while (status.eq.0)
         read(iunit,'(A)',end=100) line
         call io_setcli(line)
         if (chr_lenb(line).eq.0) goto 100
         call io_getra('RA : ',' ',ra,status)
         call io_getdec('DEC : ',' ',dec,status)
         if (crosses_opt.eq.2) then
           call io_geti('Cross-symbol : ','2',istyle,status)
           call io_getr('Symbol-size : ','1.0',rsize,status)
         endif
         call io_getstr('TEXT : ',' ',text,status)
         if (status.eq.0) then
             call precrd(1950.0d0,ra,dec,epoch,ra0,dec0)
             call rdtouv(ra0,dec0,du,dv,status)
C .. draw the cross
             up = du
             vp = dv
             call pgbbuf
             if (crosses_opt.eq.2) then
                call pgsch( rsize )
                call pgpoint( 1, up, vp, istyle )
             else
               call pgmove(up-crosses_size,vp)
               call pgdraw(up+crosses_size,vp)
               call pgmove(up,vp-crosses_size)
               call pgdraw(up,vp+crosses_size)
             endif
             call graphic_set_text_opt(annotations_text_style,status)
             if (text(1:1).ne.'%') then
               call pgtext(up+crosses_size,vp+crosses_size,
     *                     text(1:chr_lenb(text)) )
             end if
             call pgsch( 1.0 )
             call pgebuf
         end if
       end do
 100   continue
       close (iunit)
 999   call cmd_err(status,'PLOT_CROSSES',' ')

       end



