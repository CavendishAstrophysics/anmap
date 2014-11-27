C
C
*+ plot_shsymb

       subroutine plot_shsymb(status)
C      ------------------------------
C
C Display symbol plot options
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

C error flag
       integer    status
C output unit
       integer    iout
C character conversion
       integer*2  i2

C check status on entry
       if (status.ne.0) return

C get output unit
       call io_enqout(iout)
       write(iout,10)
10     format(1X/1X,'Symbol Plot Option for MAP-DISPLAY'/
     *           1X,'----------------------------------'/1X)

C map read in
       if (symbol_opt) then
         write(iout,'(1X,A)') '  Plotting of Symbols Enabled'
       else
         write(iout,'(1X,A)') '  Plotting of Symbols NOT Enabled'
       end if
       if (symb_blank.gt.0) then
         i2 = symb_blank
         write(iout,100) char(i2)
 100     format(1X,'  Marking blank pixels character     : ',A1)
       end if
       if (symb_min.gt.0) then
         i2=symb_min
         write(iout,110) char(i2),val_symb_min
 110     format(1X,'  Marking minimum pixels character   : ',A1/
     *          1X,'  Lower gate for pixels to be marked : ',1PE12.3)
       end if
       if (symb_max.gt.0) then
         i2=symb_max
         write(iout,120) char(i2),val_symb_max
 120     format(1X,'  Marking maximum pixels character   : ',A1/
     *          1X,'  Upper gate for pixels to be marked : ',1PE12.3)
       end if
       write(iout,*) ' '
       end
