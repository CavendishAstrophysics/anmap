C
C
*+ map_stretch

       subroutine map_stretch(in_map,uv_range,out_map,status)
C      ------------------------------------------------------
C
C Stretch an equatorial map to sky projection
C
C Input
C  IN_MAP        -          R4(*)           -       input map data
C Updated
C  UV_RANGE      -          I4(4)           -       UV range on input/output
C Returned
C  OUT_MAP       -          R4(*)           -       Stretched map
C  STATUS        -          I4              -       Error return
C
C
C The input map is reprojected to sky coordinates from an equatorial
C plane projection. If the input map is already in sky or tangent plane
C projection then the routine performs a simple copy of the map data.
C
C This routine assumes the map redtape for input map is set up correctly
C an on completion the map redtape approriate to the output map is in
C the common blocks.
C
C Only that region of the input map specified by UV_RANGE is stretched.
C On completion UV_RANGE contains the equivalent range on the output map.
C
*-

C error return code
       integer     status
C input map data and UV-range
       real*4      in_map(1)
       integer     uv_range(4)
C output map data
       real*4      out_map(1)
C map projection parameters
       integer     in_iproj, out_iproj
       real*8      in_usamp, in_skew, in_epoch, in_prang,
     *             in_ramap, in_decmap, in_refdat, in_obsdat
       character   in_source*80

C check status on entry
       if (status.ne.0) return
C find the map projection parameters and map centre
       call enmapj(in_iproj,in_usamp,in_skew,in_epoch,
     *             in_prang,status)
       call enmapc(in_ramap,in_decmap,in_refdat,
     *             in_obsdat,in_source,status)

C check the the input map really needs stretching
       if (in_iproj.ne.1) then
C .. map is not equatorial: data will simply be shrunk
         out_iproj = in_iproj

       else
C .. prepare map for reprojection to sky plane
         in_iproj = 3
         uv_range(3) = nint(float(uv_range(3))/sin(in_decmap))
         uv_range(4) = nint(float(uv_range(4))/sin(in_decmap))

       end if

C do the reprojection
       call reproj(in_map,out_map,uv_range,out_iproj,0,
     *             in_usamp,in_skew,in_ramap,in_decmap,in_refdat,
     *             in_epoch,2,status)

C correct redtape
       call stmapj(out_iproj,in_usamp,in_skew,in_epoch,in_prang,status)
       call stredt(uv_range,3,status)

C report any error
       call cmd_err(status,'STRETCH',' ')

       end
