C
C
*+ plot_getcol2

       subroutine plot_getcol2(minirt,prompt,default,uv_col,status)
C      ------------------------------------------------------------
C
C Read a column position - optionally using the cursor
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
       real*4         uv_col
C
C Updated:
C   error status
       integer        status
C
C The user is prompted for a UV column. If the answer supplied is
C CURSOR then the UV column is read from the creen by positioning the
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
       write(string,'(F8.1)') uv_col
       call io_getstr(prompt,default,string,status)
       if (status.ne.0) then
         call cmd_err(status,'UV-column',' ')
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
         call io_getr(' ',' ',xy(1),status)
         if (status.ne.0) then
           call cmd_err(status,'UV-column','Error in input position')
           status = ill_pos
           return
         end if
       end if
       if (xy(1).lt.float(minirt(1)).or.xy(1).gt.float(minirt(2))) then
         status = ill_pos
         call cmd_wrerr('UV-column','Position outside map')
         return
       end if
       uv_col = xy(1)

       end
