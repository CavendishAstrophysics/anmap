C
C
       CHARACTER*3 FUNCTION CHAR_io_onoff(OPTION)
C      ---------------------------------------
C
C output character representation for logical option
*-
       logical option

       if (option) then
         char_io_onoff = 'on '
       else
         char_io_onoff = 'off'
       end if

       end
