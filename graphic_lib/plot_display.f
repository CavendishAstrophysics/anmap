C
C
*$ Information Routines
*  --------------------
C
*+ plot_display(status)

       subroutine plot_display(status)
C      -------------------------------
C
C Display options and contour levels
C
C Updated:
C   error status
       integer       status
*-

       include '/mrao/include/chrlib_functions.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

       integer       number_options
       parameter    (number_options = 5)

       character*60  display_options(number_options)
       character*60  option
       integer       len_opt, iout

       if (status.ne.0) return

       display_options(1) = 'options ....... parameter settings'
       display_options(2) = 'levels ........ contour levels'
       display_options(3) = 'vectors ....... vector plot options'
       display_options(4) = 'pointers ...... pointers to structure'
       display_options(5) = 'all ........... all information'

       call io_getopt('Option (?=list) : ','options',display_options,
     *             number_options,option,status)
       if (status.eq.0) then
         len_opt = chr_lenb(option)
         if (chr_cmatch(option(1:len_opt),'OPTIONS')) then
           call plot_shopt(status)

         else if (chr_cmatch(option(1:len_opt),'LEVELS')) then
           call plot_shcnt(status)

         else if (chr_cmatch(option(1:len_opt),'VECTORS')) then
           call plot_shvec(status)

         else if (chr_cmatch(option(1:len_opt),'SYMBOLS')) then
           call plot_shsymb(status)

         else if (chr_cmatch(option(1:len_opt),'POINTERS')) then
           call io_enqout(iout)
           write(iout,10) len_image_defn, len_image_defn_used
  10       format(' Image-defn=',I4,' Used=',I4)

         else if (chr_cmatch(option(1:len_opt),'ALL')) then
           call plot_shopt(status)
           call plot_shvec(status)
           call plot_shsymb(status)
           call plot_shcnt(status)

         end if

       end if

       end
