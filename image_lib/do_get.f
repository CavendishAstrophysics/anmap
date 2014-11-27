C
C
*+ do_get

       subroutine do_get(map_array,status)
C      -----------------------------------
C
C get information from a map
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C Various information is determined for an image and the results
C returned in command-language parameters.
*-
       include '/mrao/include/chrlib_functions.inc'

C define options and character strings to hold results
       character*60  opts(10), opt, string
       integer       ls

C define local variables
       integer   imap, ip_map, uvr(4), minirt(10), iuv(2), npts
       real*4    uv_pos(2), res(10), gate, flux
       real*8    ruv(2)
       integer   ires(10)

C counters
       integer   i

C test status on entry
       if (status.ne.0) return

C request map on which to work
       call map_getmap('Map-entry : ','Default-Map','READ',imap,status)
       call redt_load(imap,status)
       call enminirt(minirt,status)
       opts(1)='pixel-value ... value at specified pixel'
       opts(2)='local-maximum . maximum value in specified region'
       opts(3)='statistics .... image statistics in a region'
       opts(4)='integrate ..... integrate image in a region'
       opts(5)='moments ....... moments of image in a region'

       call io_getopt( 'Get-option (?=list) : ','pixel-value',
     *                  opts, 5, opt, status )
       if (status.ne.0) goto 999

       string = ' '
       if (chr_cmatch(opt,'pixel-value')) then
         call plot_getpos2(minirt,'UV-position : ','*',uv_pos,status)
         uvr(1) = nint(uv_pos(1))
         uvr(2) = nint(uv_pos(1))
         uvr(3) = nint(uv_pos(2))
         uvr(4) = nint(uv_pos(2))
         iuv(1) = uvr(1)
         iuv(2) = uvr(3)
         call map_alloc_area(imap,uvr,map_array,ip_map,status)
         call iuvval2(minirt,map_array(ip_map),iuv,res(1),status)
         call chr_chrtoc(res(1),string,ls)
         call cmd_setlocal('%pixel',string(1:ls),status)

       elseif (chr_cmatch(opt,'statistics') .or.
     *         chr_cmatch(opt,'local-maximum')) then
         do i=1,4
           uvr(i) = minirt(i)
         end do
         call plot_getuv2(minirt,'UV-range : ','*',uvr,status)
         call map_alloc_area(imap,uvr,map_array,ip_map,status)
         call scnmap2(minirt, map_array(ip_map), uvr,
     *                res, ires, status )
         if (chr_cmatch(opt,'local-maximum')) then
           call ruvmax2(minirt, map_array(ip_map), ires,
     *                  ruv, res(1), status )
           call chr_chrtoc(res(1),string,ls)
           call cmd_setlocal('%max',string(1:ls),status)
           call chr_chdtoc(ruv(1),string,ls)
           call cmd_setlocal('%u-max',string(1:ls),status)
           call chr_chdtoc(ruv(2),string,ls)
           call cmd_setlocal('%v-max',string(1:ls),status)
         else
           call chr_chrtoc(res(1),string,ls)
           call cmd_setlocal('%max',string(1:ls),status)
           call chr_chrtoc(res(2),string,ls)
           call cmd_setlocal('%min',string(1:ls),status)
           call chr_chrtoc(res(3),string,ls)
           call cmd_setlocal('%mean',string(1:ls),status)
           call chr_chrtoc(res(4),string,ls)
           call cmd_setlocal('%sd',string(1:ls),status)
           call chr_chitoc(ires(1),string,ls)
           call cmd_setlocal('%u-max',string(1:ls),status)
           call chr_chitoc(ires(2),string,ls)
           call cmd_setlocal('%v-max',string(1:ls),status)
           call chr_chitoc(ires(3),string,ls)
           call cmd_setlocal('%u-min',string(1:ls),status)
           call chr_chitoc(ires(4),string,ls)
           call cmd_setlocal('%v-min',string(1:ls),status)
         endif

       elseif (chr_cmatch(opt,'integrate')) then
         call plot_getuv2(minirt,'UV-range : ','*',uvr,status)
         call io_getr('Gate : ','0.0',gate,status)
         call map_alloc_area(imap,uvr,map_array,ip_map,status)
         call image_addflux(minirt,map_array(ip_map),
     *                      uvr,gate,flux,npts,status)
         call chr_chrtoc(flux,string,ls)
         call cmd_setlocal('%flux',string(1:ls),status)
         call chr_chitoc(npts,string,ls)
         call cmd_setlocal('%npts',string(1:ls),status)

       elseif (chr_cmatch(opt,'moments')) then
         call plot_getuv2(minirt,'UV-range : ','*',uvr,status)
         call map_alloc_area(imap,uvr,map_array,ip_map,status)
         call image_moments(minirt,map_array(ip_map),
     *                      uvr,res,status)
         call chr_chrtoc(res(1),string,ls)
         call cmd_setlocal('%m1-u',string(1:ls),status)
         call chr_chrtoc(res(2),string,ls)
         call cmd_setlocal('%m1-v',string(1:ls),status)
         call chr_chrtoc(res(3),string,ls)
         call cmd_setlocal('%m2-u',string(1:ls),status)
         call chr_chrtoc(res(4),string,ls)
         call cmd_setlocal('%m2-v',string(1:ls),status)

       endif

999    call map_end_alloc(imap,map_array,status)
       call cmd_err(status,'GET',' ')

       end




