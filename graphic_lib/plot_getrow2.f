C
C
*+ plot_getrow2

       subroutine plot_getrow2(minirt,prompt,default,uv_row,status)
C      ------------------------------------------------------------
C
C Read a row position - optionally using the cursor
C
C Given:
C   mini-redtape
       integer        minirt(*)
C   prompt
       character*(*)  prompt
C   default string
       character*(*)  default
C
C Returned:
C   UV-position
       real*4         uv_row
C
C Updated:
C   error status
       integer        status
C
C The user is prompted for a UV row. If the answer supplied is
C CURSOR then the UV row is read from the creen by positioning the
C graphics CURSOR.
C
C [PA, January 1993]
*-
       include '../include/plt_error_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

       character*80    string
       real*4          xy(2)

C read the command line
       if (status.ne.0) return
       string = ' '
       write(string,'(F8.1)') uv_row
       call io_getstr(prompt,default,string,status)
       if (status.ne.0) then
         call cmd_err(status,'UV-row',' ')
         return
       end if

C test for 'cursor' input mode
       call chr_chucas(string)
       if (chr_cmatch(string(1:chr_lenb(string)),'CURSOR')) then
C .. use cursor input
         call plot_cursor_get( xy, status )

       else
C .. normal input
         call io_setcli(string(1:chr_lenb(string)))
         call io_getr(' ',' ',xy(2),status)
         if (status.ne.0) then
           call cmd_err(status,'UV-row','Error in input position')
           status = ill_pos
           return
         end if
       end if
       if (xy(2).lt.float(minirt(4)).or.xy(2).gt.float(minirt(3))) then
         status = ill_pos
         call cmd_wrerr('UV-row','Position outside map')
         return
       end if
       uv_row = xy(2)

       end
