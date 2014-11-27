C
C
*+ mapcat_close

       subroutine mapcat_close(status)
C      -----------------------------
C
C Close the map catalogue file
C
C Returned:
C     Status
       integer      status, idir, n
C
*-
       include '/mrao/include/iolib_constants.inc'
C
       character loc_user*(iolen_user), loc_file*(iolen_file)
       integer   len_lf, loc_mode, loc_termno

       include 'mapcat_cat.inc'

       if (status.ne.0) return
       if (cat_unit.gt.0) then
         close (cat_unit)
         cat_unit = 0
         call io_enqexe(loc_user,loc_mode,loc_termno)
         loc_user = '~/mrao'
         call io_makfil(loc_user,cat_dir_file,'mctg',loc_file,len_lf)
         call io_opefil(idir,loc_file(1:len_lf),'WRITE',0,status)
         do n=1,10
           write (idir,*) defined_cat_file(n)
           write (idir,'(A)') list_cat_file(n)
         end do
         close (idir)
       end if

       call mapcat_err(status,'mapcat_close','Fatal Stack-System Error')

       end
