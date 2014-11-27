C
C
*+ plot_getuv

       subroutine plot_getuv (prompt, default, iuv, status)
C      ----------------------------------------------------
C
C  Prompts for a map U,V coordinate window.
C
C  Given:
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
       integer   minirt(8)
       call enminirt( minirt, status )
       call plot_getuv2(minirt,prompt,default,iuv,status)
       end
