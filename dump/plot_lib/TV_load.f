C
C
*+ TV_load

       subroutine TV_load( status )
C      ----------------------------
C
C load an image into the external TV device
C
C updated:
C   return code
       integer      status
C
C An image is loaded into the external TV device.
C
C PA, SUNOS routine, 01/04/92
C-

       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '../include/anmap_sys_pars.inc'

C local variables
       integer                 imap, minirt(8), x_pages, x_style
       real*4                  range(2)
       character*(iolen_file)  file, string

C prompt for the map to display
       call mapcat_getmap('Map-entry : ','Default-Map','READ',
     *                    imap, status)
       if (status.ne.0) goto 999

C find the full file-name fot this map
       call mapcat_enqrt(imap,file,minirt,status)
       call redt_load(imap,status)
       call enxrdt(x_pages,x_style,status)
       call enrnge(range,status)
       call io_getr('TV-display-minimum : ','*',range(1),status)
       call io_getr('TV-display-maximum : ','*',range(2),status)

C construct command string
       string = ' '
       write( string, 1 ) file(1:chr_lenb(file)),2048*(1+x_pages),
     *                    minirt(5), minirt(6),range(1),range(2)
1      format( '/usr/local/bin/saoimage ',a,
     * ' -quiet -skip ',i6,' -r4 ',i5,i5
     * ' -upperleft -min ',1PE9.2,' -max ',1PE9.2,'>&! /dev/null &')

C execute command
       call io_system( string(1:chr_lenb(string)), status )


999    call cmd_err( status, 'TV-load', 'Unable to load map to TV' )
       end


