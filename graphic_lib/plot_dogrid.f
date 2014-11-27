*+ plot_dogrid

       subroutine plot_dogrid(status)
C      ------------------------------
C
C Add grid to frame
C
C 1  --   save PGPLOT environment
C 2  --   setup map projection parameters (preces/stproj)
C 3  --   set up grid intervals
C 4  --   calculate RA,Dec on a regular grid
C 5  --   plot Dec lines using contours
C 6  --   plot RA lines using polylines
C 7  --   restore map-projection environment
C 8  --   restore PGPLOT environment
C
C To cater for maps including the North-Pole, Dec grid lines are
C displayed by contouring a regular grid of Dec values, and RA grid
C lines by tracing polylines from minimum Dec to maximum Dec.
C
C (DJT, 17 March 88)
C
*-
       integer    nstep
       parameter (nstep=51)
       real*4     xc(nstep),yc(nstep)
       real*4     dec_grid(nstep,nstep)

       real*4     trans(6)
       data       trans / 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 /

       include '/mrao/include/constants.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

       REAL*8     RA,DEC,RACP,DECCP
       real*8     ra_max,ra_min,dec_max,dec_min,dec_pole
       real*8     ra_step,ra_zero,dec_step,dec_zero
       real*8     dx0,dy0,u,v,u1,v1,ustep,vstep
       integer    icol, isty, iu, iv, nlev, j
       real*4     pipinr, pipind
       integer    status, i, ira1, ira2, idec1, idec2
       real*4     x1, x2, y1, y2, size_x, size_y, size_m



       if (status.ne.0) return



       call pgqwin(x1,x2,y1,y2)
       call pgqci(icol)
       call pgqls(isty)
       call graphic_set_line_opt(grid_line_style,status)

C set up map projection parameters
       call stproj(iproj,0,usamp,skew,ramap,decmap,refdat,epoch,status)

C define region to plot
       size_x = x2-x1
       size_y = y2-y1
       size_m = max(size_x,size_y)
       size_x = 100.0*size_x/size_m
       size_y = 100.0*size_y/size_m
C
C find pip intervals, in secs/arcsecs
       pipinr=grid_ra*60.0
       pipind=grid_dec*60.0

C calculate RA, Dec on a regular grid.  DEC_GRID holds reduced
C values to improve precision.

       ra_min=1.e64
       ra_max=-ra_min
       dec_min=ra_min
       dec_max=ra_max
       ra_step=pipinr*const_st2r
       dec_step=pipind*const_sa2r

C allow for fractional map centre
       dx0=xmc+iumap1-1
       dy0=ymc-ivmap1-1
       u1=uv_range(1)-dx0
       v1=uv_range(4)+dy0
       ustep=dfloat(uv_range(2)-uv_range(1))/(nstep-1)
       vstep=dfloat(uv_range(3)-uv_range(4))/(nstep-1)

       do iu=1,nstep
         u=u1+(iu-1)*ustep
         do iv=1,nstep
           v=v1+(iv-1)*vstep
           call uvtord(u,v,racp,deccp,status)
           if (status.ne.0) return
           call preces(racp,deccp,ra,dec,1950.0d+0-epoch)
           if (iu.eq.1 .and. iv.eq.1) then
             ra_zero=dint(ra/ra_step)
             dec_zero=dint(dec/dec_step)
           endif
           ra_min=min(ra,ra_min)
           ra_max=max(ra,ra_max)
           dec_min=min(dec,dec_min)
           dec_max=max(dec,dec_max)
           dec_grid(iu,iv)=dec/dec_step-dec_zero
         enddo
       enddo

C set up contour level array for Dec grid lines
       idec1=dec_min/dec_step-dec_zero
       idec2=dec_max/dec_step-dec_zero
       if (dec_min.lt.0.d0) idec1=idec1-1
       if (dec_max.lt.0.d0) idec2=idec2-1
       nlev=min(idec2-idec1+1,nstep)
       do i=1,nlev
         xc(i)=idec1+i-1
       enddo

C set up window for contouring and draw Dec grid lines
       call pgbbuf
       size_x = float(nstep)
       size_y = float(nstep)
       call pgsetwin(1.0,size_x,1.0,size_y)
       call pgcont(dec_grid,nstep,nstep,1,nstep,1,nstep,xc,-nlev,trans)

C restore U,V window
       call pgsetwin(x1,x2,y1,y2)

C set up polylines and draw RA grid lines
       ira1=ra_min/ra_step
       ira2=ra_max/ra_step
       dec_pole=90.d0*const_d2r
       dec_min=max(-dec_pole,dec_min)
       dec_max=min(dec_pole,dec_max)
       dec_step=(dec_max-dec_min)/(nstep-1)
       do i=ira1,ira2
         ra=i*ra_step
         do j=1,nstep
           dec=dec_min+(j-1)*dec_step
           call preces(ra,dec,racp,deccp,epoch-1950.0d+0)
           call rdtouv(racp,deccp,u,v,status)
           xc(j)=u+dx0
           yc(j)=v-dy0
         enddo
         call pgline(nstep,xc,yc)
       enddo

C reset map projection parameters for accurate position calculations
       call stproj(iproj,1,usamp,skew,ramap,decmap,refdat,epoch,status)

C restore colour index
       call pgsci(icol)
       call pgsls(isty)
       call plot_frset(status)
       call pgebuf

       end
C
