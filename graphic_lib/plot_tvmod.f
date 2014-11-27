C
C
C
*$ Colour Table Setting Routines
*  -----------------------------
C
C P. Alexander, MRAO, Cambridge
C
C Interactive routine and support functions to modify the lookup table on
C a TV type device using the standard PGPLOT interface.
C
*+ plot_TVmod

       subroutine plot_TVmod(status)
C      ----------------------------
C
C Interactive modification of the TV parameters
C
C Returned:
C    Status
       integer     status
C
C The user is prompted for the type of transfer function and
C associated parameters.
*-
       include '/mrao/include/chrlib_functions.inc'

C function declarations
       integer        number_funcs
       parameter     (number_funcs = 7)
       character*80   funcs(number_funcs), chosen_function
       character*256  command_string, command
       character*20   st(8)
       integer        ll(8)
       integer        len_func
C real value buffer
       real*4         value
C loop counter
       integer        i
C external funcs used to define the lookup table
       real*4         pgct01, ct_lin_grey, ct_enh_grey, ct_enh_col
       real*4         ct_lin_inv_grey, ct_enh_inv_grey
       external       pgct01, ct_standard
       external       ct_lin_grey, ct_enh_grey
       external       ct_enh_col, ct_lin_inv_grey, ct_enh_inv_grey

       include '../include/tv_modify.inc'
C
C Define options
       data funcs(1)
     * /'standard-colour ............. standard colour lookup table'/
       data funcs(2)
     * /'linear-grey-scale ........... linear grey-scale'/
       data funcs(3)
     * /'enhanced-grey-scale ......... enhanced grey-scale'/
       data funcs(4)
     * /'inverted-grey-scale ......... inverted linear grey-scale'/
       data funcs(5)
     * /'inverted-enhanced-grey-scale  inverted enhanced grey-scale'/
       data funcs(6)
     * /'enhanced-colour-scale ....... enhanced colour representation'/
       data funcs(7)
     * /'rainbow-standard ............ standard rainbow representation'/

C check status on entry
       if (status.ne.0) return
C initialise arrays
       do i=1,3
         col_starts(i) = -1.0
       end do
       call io_getopt('Function-type (?=list) : ',
     *             'linear-grey',funcs,number_funcs,
     *             chosen_function,status)
       command_string = ' '
       if (status.ne.0) goto 999

C decode options
       len_func = chr_lenb(chosen_function)
       if (chr_cmatch(chosen_function(1:len_func),funcs(1))) then
         call pgsct(pgct01)

       elseif (
     *   chr_cmatch(chosen_function(1:len_func),funcs(2))) then
         call pgsct(ct_lin_grey)

       elseif (
     *   chr_cmatch(chosen_function(1:len_func),funcs(3)) .or.
     *   chr_cmatch(chosen_function(1:len_func),funcs(5))) then
         if (power_law.le.0.0) power_law = 1.0
         value = power_law
         call io_getr('Power-law (>0) : ','*',power_law,status)
         if (power_law.le.0.0) power_law = value
         if (lower_bound.le.0.0 .or. lower_bound.gt.1.0) then
           lower_bound = 0.0
         end if
         call io_getr('Lower-bound (fraction) : ','*',
     *                lower_bound,status)
         if (lower_bound.le.0.0 .or. lower_bound.gt.1.0) then
           lower_bound = 0.0
         end if
         if (upper_bound.le.0.0 .or. upper_bound.gt.1.0) then
           upper_bound = 1.0
         end if
         call io_getr('Upper-bound (fraction) : ','*',
     *                upper_bound,status)
         if (upper_bound.le.0.0 .or. upper_bound.gt.1.0) then
           upper_bound = 1.0
         end if
         if (status.ne.0) goto 999
         if (chr_cmatch(chosen_function(1:len_func),funcs(3))) then
           call pgsct(ct_enh_grey)
         else
           call pgsct(ct_enh_inv_grey)
         end if
         call chr_chrtoc(power_law,st(1),ll(1))
         call chr_chrtoc(lower_bound,st(2),ll(2))
         call chr_chrtoc(upper_bound,st(3),ll(3))
         write(command_string,'(A,1X,A,1X,A)')
     *        (st(i)(1:ll(i)), i=1,3)

       elseif (
     *   chr_cmatch(chosen_function(1:len_func),funcs(4))) then
         call pgsct(ct_lin_inv_grey)

       elseif (
     *   chr_cmatch(chosen_function(1:len_func),funcs(6))) then
         do i=1,3
           if (col_power_law(i).le.0.0) col_power_law(i) = 1.0
         end do
         call io_getnr('Colour-power-laws (rgb) : ','*',col_power_law,
     *              3,status)
         do i=1,3
           if (col_power_law(i).le.0.0) col_power_law(i) = 1.0
           call chr_chrtoc(col_power_law(i),st(i),ll(i))
         end do
         do i=1,3
           if (col_starts(i).lt.0.0) col_starts(i) = -float(i-3)*0.2
         end do
         call io_getnr('Colour-starts (rgb) : ','*',col_starts,
     *              3,status)
         do i=1,3
           if (col_starts(i).lt.0.0) col_starts(i) = -float(i-3)*0.2
           call chr_chrtoc(col_starts(i),st(i+3),ll(i+3))
         end do
         if (lower_bound.le.0.0 .or. lower_bound.gt.1.0) then
           lower_bound = 0.0
         end if
         call io_getr('Lower-bound (fraction) : ','*',
     *                lower_bound,status)
         if (lower_bound.le.0.0 .or. lower_bound.gt.1.0) then
           lower_bound = 0.0
         end if
         if (upper_bound.le.0.0 .or. upper_bound.gt.1.0) then
           upper_bound = 1.0
         end if
         call io_getr('Upper-bound (fraction) : ','*',
     *                upper_bound,status)
         if (upper_bound.le.0.0 .or. upper_bound.gt.1.0) then
           upper_bound = 1.0
         end if
         if (status.ne.0) goto 999
         call pgsct(ct_enh_col)
         call chr_chrtoc(lower_bound,st(7),ll(7))
         call chr_chrtoc(upper_bound,st(8),ll(8))
         write(command_string,'(8(1X,A))')
     *        (st(i)(1:ll(i)), i=1,8)

       elseif (
     *   chr_cmatch(chosen_function(1:len_func),funcs(7))) then
         do i=1,3
           if (col_index(i).le.0.0) col_index(i) = 1.0
         end do
         call io_getnr('Colour-indices (rgb) : ','*',col_index,
     *              3,status)
         do i=1,3
           if (col_index(i).le.0.0) col_index(i) = 2*i + 1
           call chr_chrtoc( col_index(i), st(i), ll(i))
         end do
         write(command_string,'(A,1X,A,1X,A)')
     *        (st(i)(1:ll(i)), i=1,3)
         if (status.ne.0) goto 999
         call pgsct(ct_standard)
       end if
       command=chosen_function(1:chr_lenw(chosen_function)) //
     *         ' '//command_string(1:chr_lenb(command_string))
       call cmd_setlocal( 'colour-map',
     *                    command(1:chr_lenb(command)),
     *                    status )
999    call cmd_err(status,'TV-modify',' ')

       end


