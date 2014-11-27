C DISPLAY: display spectra on specified axes
C
       implicit    NONE
C
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
C
C Local variables
C   maximum number of files to be processed
       integer    max_files
       parameter (max_files = 9)
C   file identifiers
       integer    id
C   data points and counters
       integer    ndata, ndt, n, n1
C   status return
       integer    status
C   columns to display
       integer    xc, yc
C   range on data
       real*4     xr(2), yr(2), xmin, xmax, ymin, ymax
C   data
       real*4     x(max_ndata), y(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*5, text*80
C   logical flag indicating a SET parameter item
       logical    set
C   option for multiple plots on separate regions of the same
       integer    multi_xy( 2 )
       logical    multi_plot
C   option to specify use of logarithmic axes
       logical    logx, logy
C   option to specify use of histogram type display
       logical    plot_histogram
C   option to control plotting of a key
       logical    plot_key, key_set
       character*80 key_text(max_files)
       integer    key_line(3,max_files)
       real*4     key_opts(6)
C   option to control scaling of multiple plots
       logical    do_scaling
       real*4     data_max(max_files)
C   option to control offset of multiple plots
       logical    do_offset
       real*4     offset
C   options for box and display device
       character  xtype*10, ytype*10, device*40
C   view port
       real*4     view_port(4)
C   character attributes
       integer    character_font
       real*4     character_size
C   line options
       integer    line_options(3)
C   plot titles
       character  title*80, xtitle*80, ytitle*80
C Functions
       integer    chr_lenb

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'display',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)
C find input file name and allocate file
       call cmd_items( 'file1', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)
C find columns to display and column type
       call cmd_itemi( 'xc', 1, xc, status )
       call cmd_itemi( 'yc', 1, yc, status )
       call cmd_itemset( 'logx', logx, status )
       call cmd_itemset( 'logy', logy, status )
C find titles
       call cmd_items( 'xtitle', xtitle, status )
       call cmd_items( 'ytitle', ytitle, status )
       call cmd_items( 'title',  title,  status )
C read data for the first spectrum
       call spec_get_data( id, xc, x, ndt, status)
       call spec_get_data( id, yc, y, ndt, status)
       if (logx .and. logy) then
         ndata = 0
         do n=1,ndt
           if (x(n).gt.0.0 .and. y(n).gt.0.0) then
             ndata = ndata + 1
             x(ndata) = log10(x(n))
             y(ndata) = log10(y(n))
           endif
         enddo
       elseif (logx) then
         ndata = 0
         do n=1,ndt
           if (x(n).gt.0.0) then
             ndata = ndata + 1
             x(ndata) = log10(x(n))
             y(ndata) = y(n)
           endif
         enddo
       elseif (logy) then
         ndata = 0
         do n=1,ndt
           if (y(n).gt.0.0) then
             ndata = ndata + 1
             y(ndata) = log10(y(n))
             x(ndata) = x(n)
           endif
         enddo
       else
         ndata = ndt
       endif
       if (chr_lenb(title).eq.0) then
         call spec_hd_enq( id, 'title-main', title, status )
       end if
       if (chr_lenb(xtitle).eq.0) then
         call spec_hd_enq( id, 'title-x', xtitle, status )
       end if
       if (chr_lenb(ytitle).eq.0) then
         call spec_hd_enq( id, 'title-y', ytitle, status )
       end if
C deallocate first spectrum
       call spec_deallocate( id, status )
       if (status.ne.0) goto 999
C find suitable ranges for X and Y axes
       xmin = 1.0E+30
       xmax = -1.0E+30
       ymin = 1.0E+30
       ymax = -1.0E+30
       do n=1,ndata
         if (x(n).gt.xmax) xmax = x(n)
         if (x(n).lt.xmin) xmin = x(n)
         if (y(n).gt.ymax) ymax = y(n)
         if (y(n).lt.ymin) ymin = y(n)
       end do
       call pgrnge(xmin,xmax,xr(1),xr(2))
       call pgrnge(ymin,ymax,yr(1),yr(2))
       call cmd_itemset( 'xrange', set, status )
       if (set) then
         call cmd_itemr( 'xrange', 2, xr, status )
         if (logx) then
           if (xr(1).gt.0.0 .and. xr(2).gt.0.0) then
               xr(1) = log10(xr(1))
               xr(2) = log10(xr(2))
           else
               call pgrnge(xmin,xmax,xr(1),xr(2))
           endif
         endif
       endif
       call cmd_itemset( 'yrange', set, status )
       if (set) then
         call cmd_itemr( 'yrange', 2, yr, status )
         if (logy) then
           if (yr(1).gt.0.0 .and. yr(2).gt.0.0) then
               yr(1) = log10(yr(1))
               yr(2) = log10(yr(2))
           else
               call pgrnge(ymin,ymax,yr(1),yr(2))
           endif
         endif
       endif
       if (status.ne.0) goto 999

C open plotting device
       call cmd_itemi( 'multi_plot', 2, multi_xy, status )
       call cmd_items( 'plot_device', device, status )
       multi_plot = multi_xy(1).gt.1 .and. multi_xy(2).gt.1
       if (chr_lenb(device).gt.0) then
         call pgbegin( 0, device(1:chr_lenb(device)),
     *                 multi_xy(1), multi_xy(2) )
       else
         call pgbegin( 0, '?',
     *                 multi_xy(1), multi_xy(2) )
       end if
       if (status.ne.0) goto 999

C setup default options for plot
C .. view port
       call cmd_itemr( 'plot_size', 4, view_port, status )
       if ( view_port(1).gt.1.0 .or. view_port(1).lt.0.0 .or.
     *      view_port(2).gt.1.0 .or. view_port(2).lt.0.0 .or.
     *      view_port(3).gt.1.0 .or. view_port(3).lt.0.0 .or.
     *      view_port(4).gt.1.0 .or. view_port(4).lt.0.0  )  then
         print *,'***(DISPLAY) Illegal view port: ',view_port
         print *,'***(DISPLAY) Using default'
         view_port(1) = 0.15
         view_port(3) = 0.15
         view_port(2) = 0.85
         view_port(4) = 0.85
       end if
C .. character size and font
       call cmd_itemr( 'character_size', 1, character_size, status )
       call pgsch( character_size )
       call cmd_itemi( 'character_font', 1, character_font, status )
       if (character_font.gt.4 .or. character_font.lt.0) then
         print *,'***(DISPLAY) Illegal character font = ',
     *           character_font
         print *,'***(DISPLAY) Using font number 1 as default '
         character_font = 1
       end if
       call pgscf( character_font )
C .. frame options
       xtype='BTN'
       ytype='BTN'
       call cmd_itemset( 'plot_frame', set, status )
       if (.not.set) then
         text = xtype(1:chr_lenb(xtype))//'C'
         xtype = text
         ytype = text
       end if
       call cmd_itemset( 'plot_axes', set, status )
       if (set) then
         text = xtype(1:chr_lenb(xtype))//'A'
         xtype = text
         ytype = text
       end if
       call cmd_itemset( 'plot_grid', set, status )
       if (set) then
         text = xtype(1:chr_lenb(xtype))//'G'
         xtype = text
         ytype = text
       end if
       call cmd_itemset( 'plot_ticks', set, status )
       if (set) then
         text = xtype(1:chr_lenb(xtype))//'S'
         xtype = text
         ytype = text
       end if
       call cmd_itemset( 'label_horizontal', set, status )
       if (set) then
         text = ytype(1:chr_lenb(ytype))//'V'
         ytype = text
       end if
       if (logx) then
         text = xtype(1:chr_lenb(xtype))//'L'
         xtype = text
       end if
       if (logy) then
         text = ytype(1:chr_lenb(ytype))//'L'
         ytype = text
       end if
       call cmd_itemset( 'plot_histogram', plot_histogram, status )
C control offsetting multiple spectra
       call cmd_itemset( 'offset', do_offset, status )
       if (do_offset) then
         call cmd_itemr( 'offset', 1, offset, status )
       endif
C control of spectral scaling
       call cmd_itemset( 'rescale', do_scaling, status )
C control of plotting key
       call cmd_itemset( 'plot_key', plot_key, status )
       if (plot_key) then
         call cmd_itemr( 'key_opts',6,key_opts,status)
       endif
       if (status.ne.0) goto 999

C set plotting view port and window
       if (.not. multi_plot) then
         call pgvport( view_port(1), view_port(2),
     *                 view_port(3), view_port(4) )
         call pgwindow( xr(1), xr(2), yr(1), yr(2) )
C draw box
         call pgbox( xtype(1:chr_lenb(xtype)), 0.0, 0,
     *               ytype(1:chr_lenb(ytype)), 0.0, 0 )
         call pglabel( xtitle(1:chr_lenb(xtitle)),
     *                 ytitle(1:chr_lenb(ytitle)),
     *                 title(1:chr_lenb(title)) )
       end if

C loop and plot each file in turn
       set = .true.
       n1 = 1
       do while (set .and. n1.le.max_files .and. status.eq.0)
         string = ' '
         write(string,'(A4,I1)') 'file',n1
         call cmd_itemset( string, set, status )
         if (set) then
           call cmd_items( string, file, status )
           call spec_allocate( file(1:chr_lenb(file)), 'read',
     *                         id, status )
           call spec_get_data( id, xc, x, ndt, status)
           call spec_get_data( id, yc, y, ndt, status)
           if (plot_key) then
             write(string,'(A3,I1)') 'key',n1
             call cmd_itemset( string(1:4), key_set, status )
             if (key_set) then
               call cmd_items( string(1:4), key_text(n1), status )
             else
               key_text(n1) = file
             endif
           endif
           if (status.ne.0) goto 999
           if (logx .and. logy) then
             ndata = 0
             do n=1,ndt
               if (x(n).gt.0.0 .and. y(n).gt.0.0) then
                 ndata = ndata + 1
                 x(ndata) = log10(x(n))
                 y(ndata) = log10(y(n))
               endif
             enddo
           elseif (logx) then
             ndata = 0
             do n=1,ndt
               if (x(n).gt.0.0) then
                 ndata = ndata + 1
                 x(ndata) = log10(x(n))
                 y(ndata) = y(n)
               endif
             enddo
           elseif (logy) then
             ndata = 0
             do n=1,ndt
               if (y(n).gt.0.0) then
                 ndata = ndata + 1
                 y(ndata) = log10(y(n))
                 x(ndata) = x(n)
               endif
             enddo
           else
             ndata = ndt
           endif
           if (do_scaling) then
             data_max(n1) = y(1)
             do n = 1,ndata
               data_max(n1) = max(data_max(n1),y(n))
             enddo
           endif
           if (multi_plot) then
C .. re-draw frame and label
             xmin = 1.0E+30
             xmax = -1.0E+30
             ymin = 1.0E+30
             ymax = -1.0E+30
             do n=1,ndata
               if (x(n).gt.xmax) xmax = x(n)
               if (x(n).lt.xmin) xmin = x(n)
               if (y(n).gt.ymax) ymax = y(n)
               if (y(n).lt.ymin) ymin = y(n)
             end do
             call pgrnge(xmin,xmax,xr(1),xr(2))
             call pgrnge(ymin,ymax,yr(1),yr(2))
             call cmd_itemset( 'xrange', set, status )
             if (set) then
               call cmd_itemr( 'xrange', 2, xr, status )
               if (logx) then
                 if (xr(1).gt.0.0 .and. xr(2).gt.0.0) then
                     xr(1) = log10(xr(1))
                     xr(2) = log10(xr(2))
                 else
                     call pgrnge(xmin,xmax,xr(1),xr(2))
                 endif
               endif
             endif
             call cmd_itemset( 'yrange', set, status )
             if (set) then
               call cmd_itemr( 'yrange', 2, yr, status )
               if (logy) then
                 if (yr(1).gt.0.0 .and. yr(2).gt.0.0) then
                     yr(1) = log10(yr(1))
                     yr(2) = log10(yr(2))
                 else
                     call pgrnge(ymin,ymax,yr(1),yr(2))
                 endif
               endif
             endif
             if (status.ne.0) goto 999
             call pgask( .false. )
             call pgadvance
             call pgvport( view_port(1), view_port(2),
     *                     view_port(3), view_port(4) )
             call pgwindow( xr(1), xr(2), yr(1), yr(2) )
             call pgbox( xtype(1:chr_lenb(xtype)), 0.0, 0,
     *                   ytype(1:chr_lenb(ytype)), 0.0, 0 )
             call pglabel( xtitle(1:chr_lenb(xtitle)),
     *                     ytitle(1:chr_lenb(ytitle)),
     *                     file(1:chr_lenb(file)) )
           end if
           string = ' '
           write(string,'(A,I1)') 'lopt',n1
           call cmd_itemi( string, 3, line_options, status )
           do n = 1,3
             key_line(n,n1) = line_options(n)
           enddo
           if (status.ne.0) goto 999
           call pgsls(abs(line_options(1)))
           call pgsci(line_options(2))
           if (do_offset .and. .not.multi_plot) then
             do n=1,ndata
               y(n) = y(n) + float(n1-1)*offset
             enddo
           endif
           if (do_scaling .and. .not.multi_plot) then
             if (data_max(n1).ne.0.0 .and.
     *           data_max(1).ne.0.0) then
               do n=1,ndata
                 y(n) = y(n) * data_max(1)/data_max(n1)
               enddo
             endif
           endif
           if (line_options(3).eq.0) then
             if (plot_histogram .or. line_options(1).lt.0) then
               call pgbin(ndata,x,y,.true.)
             else
               call pgline(ndata,x,y)
             end if
           else
             call pgpoint(ndata,x,y,abs(line_options(3)))
             if (line_options(3).lt.0) then
               call pgline(ndata,x,y)
             endif
           end if
           if (multi_plot .and. plot_key) then
             call plt_key(1,key_text(n1),key_line(1,n1),
     *                    key_opts,status)
           endif
           call spec_deallocate( id, status )
           set = .true.
         end if
         n1 = n1 + 1
       end do
       n1 = n1 - 2
       if (plot_key .and. .not.multi_plot) then
         call plt_key( n1,key_text,key_line,key_opts,status)
       endif

C end plot
999    continue
       call pgend
       end
C
C
       subroutine plt_key(nk,key_text,key_line,key_opts,status)
C      --------------------------------------------------------
C
C Plot a key
C
C Given:
C  number of items in the key
       integer        nk
C  text for each keyed item
       character*(*)  key_text(nk)
C  line-style optionts for each key item
       integer        key_line(3,nk)
C  options for the key
       real*4         key_opts(5)
C
C Updated:
C  error status
       integer        status
C
C-

       include '/mrao/include/chrlib_functions.inc'

C local variables
C   current window
       real*4         window(4)
C   position of start of key box
       real*4         xpos, ypos
C   font for keys
       integer        font
C   drop factor between successive lines
       real*4         drop
C   scale factor for key box
       real*4         scale
C   key to specify drawing a frame around the key
       integer        frame

C   counters and pointers
       integer        n, of
       real*4         x1, y1, x2, y2, x3, d, os
       real*4         x, y, xmax
C   text buffer
       character*120  string

C find current window, then reset
       call pgqwin( window(1), window(2), window(3), window(4) )
       call pgwindow( 0.0, 1.0, 0.0, 1.0 )

C sort out options
       xpos = key_opts(1)
       ypos = key_opts(2)
       font = nint(key_opts(3))
       drop = key_opts(4)
       scale = key_opts(5)
       frame = nint(key_opts(6))
       d = drop*scale*1.5/40.0
       call pgqcf( of )
       call pgqch( os )
       call pgscf( font )
       call pgsch( scale )

C loop for each item in the list
       y1 = ypos - 0.5*d - 0.025*scale
       y2 = ypos - d - 0.025*scale
       x1 = xpos + 0.025*scale
       x2 = xpos + 0.1*scale
       x3 = xpos + 0.125*scale
       do n = nk,1,-1
         call pgsls(abs(key_line(1,n)))
         call pgsci(key_line(2,n))
         if (key_line(3,n).eq.0) then
             call pgmove(x1,y1)
             call pgdraw(x2,y1)
         else
             call pgpoint(1,0.5*(x1+x2),y1,abs(key_line(3,n)))
             if (key_line(3,n).lt.0) then
               call pgmove(x1,y1)
               call pgdraw(x2,y1)
             endif
         end if
         string = key_text(n)
         call pgsls(1)
         call pgsci(1)
         call pgtext(x3,y2,string(1:chr_lenb(string)))
         call pgqpos(x,y)
         if (n.eq.nk) then
           xmax = x
         else
           xmax = max(xmax,x)
         endif
         y1 = y1 - d
         y2 = y2 - d
       enddo

C if required draw a frame around the key
       if (frame.eq.1) then
         xmax = xmax + 0.025*scale
         call pgsls(1)
         call pgsci(1)
         call pgmove(xpos,ypos)
         call pgdraw(xpos,y1)
         call pgdraw(xmax,y1)
         call pgdraw(xmax,ypos)
         call pgdraw(xpos,ypos)
       endif

C reset window and attributes
       call pgwindow( window(1), window(2), window(3), window(4) )
       call pgscf( of )
       call pgsch( os )

       end
