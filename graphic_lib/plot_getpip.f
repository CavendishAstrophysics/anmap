C
C
*+ plot_getpip

       subroutine plot_getpip(ask,status)
C      ----------------------------------
C
C  Input pip intervals, in RA and Dec.
C
C-

       integer    ic, status
       real*8     rac(4), decc(4), ra_range, dec_range
       real       facr, facd, pipinr, pipind
       character  prompt*45
       logical    ask, cmd_dblev

       include '/mrao/include/constants.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C redtape items
       real*8     usamp,skew,ramap,decmap,refdat,epoch
       integer    iproj, iprec

       if (status.ne.0) return

C check for writing out information from the routines
       if (cmd_dblev(1)) ic=1

C  Set up map projection parameters
       call enproj(iproj,iprec,usamp,skew,ramap,decmap,refdat,epoch)
       call stproj(iproj,0,usamp,skew,ramap,decmap,refdat,epoch,status)

C  Find suitable pip intervals
       call pipsa(uv_range,rac,decc,ra_range,dec_range,ic,status)
       call pipsb(uv_range,5,ra_range,dec_range,pipinr,pipind,0,status)

C  Offer these intervals
       facr=1.0
       if (pipinr.gt.500.) facr=60.0
       pipinr=pipinr/facr
       write(prompt,1)pipinr
       if (facr.eq.60.) prompt(33:35)='min'
       if (ask) call io_getr(prompt,' ',pipinr,status)
       grid_ra=pipinr*facr/60.0
C
       facd=1.0
       if (pipind.gt.500.) facd=60.0
       pipind=pipind/facd
       write(prompt,2)pipind
       if (facd.eq.60.) prompt(36:38)='min'
       if (ask) call io_getr(prompt,' ',pipind,status)
       grid_dec=pipind*facd/60.0
C
C  Reset map projection parameters
       call stproj(iproj,1,usamp,skew,ramap,decmap,refdat,epoch,status)
C
    1  format('Pip/grid interval in RA [',F5.1,'] secs: ')
    2  format(20X,'Dec [',F5.1,'] arcsecs: ')
C
       end
