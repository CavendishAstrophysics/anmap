C
C
*+ plot_dopips

       subroutine plot_dopips(status)
C      ------------------------------
C
C Annotate frame with pips
C
C 1  --   define plot regions to have sides 100x(100xaspect_ratio)
C 2  --   setup map projection parameters (preces/stproj)
C 3  --   calculate map corners/pip positions
C 4  --   plot pips
C 5  --   restore PGPLOT environment
C 6  --   restore map-projection environment
C
C (PA/EMW  17 Oct 87)
C (DJT     12 Apr 88)
C
*-
       include '/mrao/include/constants.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C redtape items
       real*8     usamp,skew,ramap,decmap,refdat,epoch
       integer    iproj, iprec
C local variables
       character  label*16
       real*8     pipval(32)
       real       pdist(32), pipinr, pipind
       integer    piptyp, prsec
       integer    status
       real*4     vpx1, vpx2, vpy1, vpy2, ratio_x, ratio_y, ratio_ch,
     *            ratio_u
       real*4     x1, x2, y1, y2, size_x, size_y, size_m,
     *            pipinr, pipind, pipsiz, xpos, ypos
       integer    npip, i, ll


       if (status.ne.0) return

C define region to plot
       call pgqwin(x1,x2,y1,y2)
       call pgqvp(0,vpx1,vpx2,vpy1,vpy2)
       ratio_x = (vpx2-vpx1)/0.9
       ratio_y = (vpy2-vpy1)/0.9
       ratio_u = max(ratio_y,ratio_x)
       size_x = x2-x1
       size_y = y2-y1
       size_m = max(size_x,size_y)
       size_x = 100.0*size_x/size_m
       size_y = 100.0*size_y/size_m
       call pgsetwin(0.0,size_x,0.0,size_y)
       call graphic_set_text_opt( frame_text_style, status )
       call graphic_copy_text_opt( frame_text_style, text_opt, status )
       ratio_ch = text_opt_size/1.0

C setup map projection parameters
       call enproj(iproj,iprec,usamp,skew,ramap,decmap,refdat,epoch)
       call stproj(iproj,0,usamp,skew,ramap,decmap,refdat,epoch,status)

C find pip intervals in sec/arcsec
       pipinr=grid_ra*60.0
       pipind=grid_dec*60.0

C SIDE 1
       call pipsc(uv_range,1,1,pipinr,pdist,pipval,npip,status)
       call graphic_set_line_opt( frame_line_style, status )
       if (status.eq.0) then
         do i=1,npip
           xpos=pdist(i)*size_x
           ypos=size_y
           piptyp=nint(pipval(i)/pipinr)
           piptyp=iabs(mod(piptyp,2))+1
           if (npip.lt.4) piptyp=1
           pipsiz=2.0*pip_size/float(piptyp)
           call pgmove(xpos,ypos)
           call pgdraw(xpos,ypos-pipsiz)
         enddo
       endif

C SIDE 2
       prsec=0
       if (pipind.lt.1.0) prsec=2
       call pipsc(uv_range,2,2,pipind,pdist,pipval,npip,status)
       if (status.eq.0) then
         do i=1,npip
           call graphic_set_line_opt( frame_line_style, status )
           xpos=size_x
           ypos=(1.0-pdist(i))*size_y
           piptyp=nint(pipval(i)/pipind)
           piptyp=iabs(mod(piptyp,2))+1
           if (npip.lt.4) piptyp=1
           pipsiz=2.02*pip_size/float(piptyp)
           call pgmove(xpos,ypos)
           call pgdraw(xpos-pipsiz,ypos)
           if (piptyp.eq.1) then
             call chr_chdtos(pipval(i)/3600.d0,prsec,label,ll)
             call graphic_set_text_opt( frame_text_style, status )
             if (text_opt_size.gt.0) 
     *            call pgptext(xpos+0.5*pip_off_x*ratio_ch/ratio_u,
     *                    ypos-1.2*ratio_ch/ratio_u,
     *                    0.0,0.0,label(1:ll))
           endif
         enddo
       endif

C SIDE 3
       prsec=0
       if (pipinr.lt.1.0) prsec=2
       call pipsc(uv_range,3,1,pipinr,pdist,pipval,npip,status)
       if (status.eq.0) then
         do i=1,npip
           call graphic_set_line_opt( frame_line_style, status )
           xpos=pdist(i)*size_x
           ypos=0.0
           piptyp=nint(pipval(i)/pipinr)
           piptyp=iabs(mod(piptyp,2))+1
           if (npip.lt.4) piptyp=1
           pipsiz=2.0*pip_size/float(piptyp)
           call pgmove(xpos,ypos)
           call pgdraw(xpos,ypos+pipsiz)
           if (piptyp.eq.1) then
             call chr_chdtos(pipval(i)/3600.d0,prsec,label,ll)
             call graphic_set_text_opt( frame_text_style, status )
             if (text_opt_size.gt.0) 
     *             call pgptext(xpos,-4.0*pip_off_y*ratio_ch/ratio_u,
     *                    0.0,0.5,label(1:ll))
           endif
         enddo
       endif

C SIDE 4
       call pipsc(uv_range,4,2,pipind,pdist,pipval,npip,status)
       call graphic_set_line_opt( frame_line_style, status )
       if (status.eq.0) then
         do i=1,npip
           xpos=0.0
           ypos=(1.0-pdist(i))*size_y
           piptyp=nint(pipval(i)/pipind)
           piptyp=iabs(mod(piptyp,2))+1
           if (npip.lt.4) piptyp=1
           pipsiz=2.0*pip_size/float(piptyp)
           call pgmove(xpos,ypos)
           call pgdraw(xpos+pipsiz,ypos)
         enddo
       endif

C reset map projection parameters for accurate position calculations
       call stproj(iproj,1,usamp,skew,ramap,decmap,refdat,epoch,status)

C restore PGPLOT environment
       call pgsetwin(x1,x2,y1,y2)
       call pgsch(1.0)
       call pgscf(1)

       end

