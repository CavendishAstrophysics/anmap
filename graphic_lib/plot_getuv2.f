C
C
*+ plot_getuv2

       subroutine plot_getuv2 (minirt, prompt, default, iuv, status)
C      -------------------------------------------------------------
C
C  Prompts for a map U,V coordinate window.
C
C  Given:
C    mini redtape of map
       integer          minirt(8)
C    prompt
       character*(*)    prompt
C    default
       character*(*)    default
C Returned:
C    UV-coordinate window
       integer          iuv(4)
C
C Updated:
C   error status
       integer          status
C
C  Prompts on the input device for a map area, specified as four integers,
C  (IU1,IU2,IV1,IV2).  If only one integer is entered then the square area
C  (-IU1,IU1,IU1,-IU1) is returned.  Note that the ordering convention
C   IU1<IU2 and IV1>IV2  is enforced if necessary.  If the redtape common
C  blocks contain parameters for a current map, then checks are also made
C  that the area lies within the map bounds.  If the input string is null,
C  the DEFAULT string is used as input.  If the default string is null or
C  '*', the current values of IUV are used as default.
C
C  The user may optionally specify "CURSOR" in response to the prompt and
C  if then a map is plotted on the current plot device (and open) then
C  the UV-range may be specified by positioning the cursor on the graphics
C  screen in the usual manner.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.  Other possible values are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C      - unexpected system I/O error, or user break
C
C  (DJT, 14 October 86)
C  (PA,  22 January 88)
C  (PA,  17 December 91)
C  (PA,  17 January 93)
*-
       character*20   string
       integer        iuv_n(4), iu1, iu2, iv1, iv2
       integer        iu, iv, ls, errdev
       real*4         uv_c1(2), uv_c2(2)
       logical        batch, chr_cmatch, verify_value, cursor_available,
     *                cmd_dblev

       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/maplib_errors.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

        equivalence (iuv_n(1),iu1),(iuv_n(2),iu2),(iuv_n(3),iv1),
     *              (iuv_n(4),iv2)

    1  if (status.eq.0) then

         write(string,'(3(i4,'',''),i4)')iuv

C    Read U,V window from command line

         call io_getwrd(prompt,default,string,ls,status)
         if (status.ne.0) then
           call cmd_err(status,'PLOT_GETUV',' ')
           return
         end if

C Look for "CURSOR" response
         call chr_chucas(string)
         if (chr_cmatch(string(1:ls),'CURSOR')) then

           call pgqcur(cursor_available)
           if (frame_init .and. cursor_available) then

C ... look for cursor input
             verify_value = .true.
             if (cmd_dblev(1)) then
               call io_wrout(
     *         '.. mark the UV-range with the graphics cursor')
               call io_wrout(
     *         '   mark opposite corners of the window')
               call io_wrout(
     *         '   type any character to enter or a Q to quit')
             endif
             call plot_cursor_get( uv_c1, status )
             call plot_cursor_get( uv_c2, status )
             iuv_n(1) = min(nint(uv_c1(1)),nint(uv_c2(1)))
             iuv_n(2) = max(nint(uv_c1(1)),nint(uv_c2(1)))
             iuv_n(3) = max(nint(uv_c1(2)),nint(uv_c2(2)))
             iuv_n(4) = min(nint(uv_c1(2)),nint(uv_c2(2)))

          else

C ... no plot on output device therefore do not use cursor input
             call cmd_wrerr('UV-RANGE',
     *                       'NO plot frame or NO cursor available')
             status = ill_uvwind

           end if

         else if (ls.gt.0) then

C .. normal terminal prompt
           verify_value = .true.
           call chr_chctoi(string(1:ls),iu1,status)
           call io_nxtwrd(string,ls,status)
           if (ls.gt.0) then
             call chr_chctoi(string(1:ls),iu2,status)
             call io_nxtwrd(string,ls,status)
             call chr_chctoi(string(1:ls),iv1,status)
             call io_nxtwrd(string,ls,status)
             call chr_chctoi(string(1:ls),iv2,status)
           else
             iu1=-iu1
             iu2=-iu1
             iv1=iu2
             iv2=iu1
             ls=1
           endif
         end if
C
C    Check for validity
C
         if (verify_value) then
           if (ls.eq.0 .or. status.eq.bad_syntax) then
             call io_wrout('*** bad syntax,  U1,U2,V1,V2  wanted')
             status=ill_uvwind
           else
C
C    Enforce IU1 < IU2 and IV1 > IV2
C
             if (iu1.gt.iu2) then
               iu=iu1
               iu1=iu2
               iu2=iu
             endif
             if (iv1.lt.iv2) then
               iv=iv1
               iv1=iv2
               iv2=iv
             endif
C
C    Check against map bounds
               if (iu1.lt.minirt(1) .or. iu2.gt.minirt(2) .or.
     :             iv1.gt.minirt(3) .or. iv2.lt.minirt(4) ) then
                 call io_enqerr( errdev )
                 write(errdev,'(x,a,4i6)')
     :             '*** area outside map, restrict to :',
     :             minirt(1),minirt(2),minirt(3),minirt(4)
                 status=uv_outmap
               endif
           endif

           if (status.eq.ill_uvwind .or. status.eq.uv_outmap) then
             call io_enqbch( batch )
             if (.not.batch) status=0
             call io_setcli(' ')
             goto 1
           endif

           iuv(1)=iu1
           iuv(2)=iu2
           iuv(3)=iv1
           iuv(4)=iv2

         endif
       endif

       end

