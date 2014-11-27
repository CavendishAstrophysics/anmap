C
C
*+ stack_access

       subroutine stack_access(imapi,access,data_type,size,is,status)
C      --------------------------------------------------------------
C
C Perform access function on the stack
C
C Input:
C    Map entry or file ID code
       integer          imapi
C    Access state (READ, WRITE, SCRATCH, CLEAR, ALLOCATE)
       character*(*)    access
C    Type of data
       character*(*)    data_type
C    Size of imap (words) or map data to allocate to
       integer          size
C
C Returned or Given:
C    Stack entry (returned) ; Unit number (given)
       integer          is
C Returned:
C    Status code
       integer          status
C
C Space in the internal data arrays is found and allocated.
C
C Status should be zero on entry
*-
C
C Local variables
       integer          n, np, type, mode, imap
       integer          i, ii, j, jj, ib, ib0, ic, ic0
       logical          allocated, re_allocate
C debug variables
       logical          cmd_dblev
C
C stack status word definition
       include 'mapcat_stack.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C test status on entry and define allocation
       if (cmd_dblev(7)) then
         print *,'..(STACK_ACCESS) access requested: ',imapi,' ',access
       end if
       if (status.ne.0) return
       imap = imapi
       allocated = .false.

C check type
       type = type_unknown
       if (chr_cmatch(data_type,'MAP')) type=type_map
       if (chr_cmatch(data_type,'REDTAPE')) type=type_redtape
       if (type.eq.type_unknown) then
         status = ill_stktyp
         goto 999
       end if

C test for re-allocation of an existing map in core access=ALLOCATE
       re_allocate = .false.
       if (chr_cmatch(access,'ALLOCATE')) then
         imap = size
         re_allocate = .true.
       end if

C test imap for validity
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C find entry in list of active maps if the data type is map
       if (type.eq.type_map) then
         ib0=-1
         do ib=max_active_maps,1,-1
           if (active_list(1,ib).eq.0) then
             ib0=ib
           end if
         end do
         if (ib0.lt.0) then
           status = ill_active
           goto 999
         end if
       end if

C look for existing allocation
       do n = 1,number_stack_entries
         if (.not.allocated) then
           if (stack_status(ip_type,n).eq.type) then
             if (stack_status(ip_allocated,n).eq.imap) then
               allocated = .true.
               is = n
               np = stack_status(ip_interlock,n)
             end if
           end if
         end if
       end do

C if re-allocation required IMAP must have been in core:
       if (re_allocate .and. (.not.allocated) ) then
         status = ill_realloc
         return
       end if

       if (.not.allocated .and.
     *     .not.chr_cmatch(access,'SEQUENTIAL')) then
C .. if no existing allocation find space for the file unless the
C    requested access is purely sequential
         if (cmd_dblev(10)) then
           print *,'..(STACK_ACCESS) About to call STACK_FNDSPC'
           if (io_yesno('..(DEBUG) Display stack ? ','no',status)) then
             call stack_display('STACK',status)
           end if
         end if
         mode = mode_direct
         call stack_fndspc(size,type,is,np,status)
C .. clear space
         do i = is,is+np-1
           j = stack_status(ip_allocated,i)
           do ii = 1,number_stack_entries
             if (stack_status(ip_type,ii).eq.type) then
               if (stack_status(ip_allocated,ii).eq.j) then
                 do jj = 1,length_stack_status - 3
                   stack_status(jj,ii) = 0
                 end do
               endif
             endif
           end do
         end do
C .. allocate space
         call stack_io_setacc(is,access,status)
         if (status.eq.0) then
           do i = is,is+np-1
             stack_status(ip_access,i) = stack_status(ip_access,is)
             stack_status(ip_data,i) = false
             stack_status(ip_interlock,i) = np
             stack_status(ip_allocated,i) = imap
           end do
         end if
         if (cmd_dblev(10)) then
           print *,'..(STACK_ACCESS) Allocated space'
           print *,'..(STACK_ACCESS) is/np = ',is,np
           if (io_yesno('..(DEBUG) Display stack ? ','no',status)) then
             call stack_display('STACK',status)
           end if
         end if

       else if (allocated) then
C .. set the access state -- change access to READ if map in core and
C    sequential access requested
         if (chr_cmatch(access,'SEQUENTIAL')) then
           call stack_io_setacc(is,'READ',status)
         else if (chr_cmatch(access,'ALLOCATE')) then
           imap = imapi
           call stack_io_setacc(is,'WRITE',status)
           do i=is,is+np-1
             stack_status(ip_allocated,i) = imap
           end do
         else
           call stack_io_setacc(is,access,status)
         end if
         mode = mode_direct

       else
C .. sequential access required define pageing buffer
         mode=mode_sequential
         active_list(3,ib0) = is
         ic0 = -1
         do ic=number_map_buffers,1,-1
           if (buffer_status(ic).eq.0) then
             ic0 = ic
           end if
         end do
         if (ic0.lt.0) then
           status = ill_buffers
           goto 999
         end if
         buffer_status(ic0) = 1
         active_list(4,ib0) = number_map_entries*size_map_entry +
     *                        (ic0-1)*size_map_buffer + 1
         active_list(6,ib0) = ic0
         active_zmnx(1,ib0) = -1.0E+30
         active_zmnx(2,ib0) =  1.0E+30
       end if

       if (type.eq.type_map) then
         active_list(1,ib0) = imap
         active_list(2,ib0) = mode
         active_list(5,ib0) = number_redtape_entries*size_redtape_entry+
     *                        (ib0-1)*512 + 1
         if (mode.eq.mode_direct) then
           active_list(6,ib0) = is
         end if
       end if

999    call mapcat_err(status,'stack_access',' ')
       end


