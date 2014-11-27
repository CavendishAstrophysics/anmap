C
C
*+ stack_fndspc

       subroutine stack_fndspc(size,type,is,np,status)
C      -----------------------------------------------
C
C Find space in the stack for the requested internal file
C
C Input:
C    Size of internal file requested
       integer        size
C    Type of internal file
       integer        type
C Returned:
C    Start stack entry for internal file
       integer        is
C    Number of stack entries from is
       integer        np
C    Status
       integer        status
C
C Space is found in the stack array for the requested internal file.
C No allocation is made by this routine.
*-

       include 'mapcat_stack.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/iolib_constants.inc'

C arrays to hold "scores"
       integer    stack_test(max_number_stack_entries)
       integer    buff_test(max_number_stack_entries)
C mini redtape and filename
       integer    minirt(10)
       character  filename*(iolen_file)
C counters etc.
       integer    i, ii, i1, i2, n
C length of buffer
       integer    len_buff
C number of buffers
       integer    nb
C best buffer
       integer    nb_best, buff_best
C debug information
       logical    cmd_dblev

       if (status.ne.0) return

C type specific initialisation
       if (cmd_dblev(7)) then
         print *,'..(STACK_FNDSPC) type/size = ',type,size
       end if
       if (type.eq.type_redtape) then
         i1 = 1
         i2 = number_redtape_entries
         len_buff = 1
       else if (type.eq.type_map) then
         i1 = number_redtape_entries + 1
         i2 = number_redtape_entries + number_map_entries
         len_buff = size/size_map_entry
         if ((size-size_map_entry*len_buff) .gt. 0) then
           len_buff = len_buff+1
         end if
         if (len_buff.gt.12) then
           len_buff = 16
         else if (len_buff.gt.8) then
           len_buff = 12
         else if (len_buff.gt.4) then
           len_buff = 8
         else if (len_buff.gt.2) then
           len_buff = 4
         end if
       else if (type.eq.type_work) then
         i1 = number_redtape_entries + 1
         i2 = number_redtape_entries + number_work_entries
         len_buff = size/size_work_entry
         if ((size-size_work_entry*len_buff) .gt. 0) then
           len_buff = len_buff+1
         end if
       end if
       if (cmd_dblev(7)) then
         print *,'..(STACK_FNDSPC) i1/2 & len_buff = ',i1,i2,len_buff
       end if

C construct test array of "scores" for each stack entry
       do i = i1,i2
         stack_test(i) = -1000000000
         if (stack_status(ip_access,i).eq.access_clear) then
           if (stack_status(ip_data,i).eq.false) then
             stack_test(i) = 0
           else
             call mapcat_enqrt(stack_status(ip_allocated,i),
     *                         filename,minirt,status)
             stack_test(i) = minirt(5)*minirt(6)
           end if
         end if
       end do

C search the "scores" array and construct "scores" for each buffer
       nb = 0
       do i = i1,i2-len_buff+1
         nb = nb + 1
         buff_test(nb) = 0
         do ii = i,i+len_buff-1
           buff_test(nb) = buff_test(nb) + stack_test(ii)
         end do
       end do

C find best buffer
       nb_best = -1
       buff_best = -100000000
       do n = 1,nb
         if (abs(buff_test(n)).lt.abs(buff_best)) then
           buff_best = buff_test(n)
           nb_best = n
         end if
       end do
       if (nb_best.gt.0) then
         is = (nb_best-1) + i1
         np = len_buff
       else
         status = ill_stkalloc
       end if
       call mapcat_err(status,'stack_fndspc','Unable to find Space')
       if (cmd_dblev(7)) then
         print *,'..(STACK_FNDSPC) is/np = ',is,np
       end if
       end
