C
C
*+ stack_init

       subroutine stack_init(nmaps,smap,nbuffers,sbuffer,status)
C      ----------------------------------------------------------
C
C Initialise the stack
C
C Given:
C     number of and size of map stack entries
        integer      nmaps
        integer      smap
C     number of and size of buffer entries
        integer      nbuffers
        integer      sbuffer
C
C Returned:
C     Status
        integer      status
C
C Initialise the map stack. Two functions are performed by the routine:
C    (a) define the total number of map slots and buffers and sizes
C    (b) initialise all the pointers and map_status block
C
C If the number of maps specified in the call to this routine is less
C than zero then only function (b) is carried out -- in this way the
C stack can be re-initialised to the same size as in the initial
C definition.
*-
       include 'mapcat_stack.inc'

       integer   i, j, is, ip, irange

       if (status.ne.0) return

       if (nmaps.gt.0) then
         number_map_entries   = nmaps
         number_stack_entries = number_redtape_entries +
     *                          number_map_entries +
     *                          number_work_entries
         size_map_entry       = smap
         size_work_entry      = smap
         number_map_buffers   = nbuffers
         size_map_buffer      = sbuffer
       end if
       do i=1,max_active_maps
         active_list(1,i) = 0
       end do
       do i=1,number_map_buffers
         buffer_status(i) = 0
       end do
       is = 0
       ip = 1
       do i = 1,number_redtape_entries
         is = is + 1
         do j = 1,length_stack_status - 3
           stack_status(j,is) = 0
         end do
         stack_status(ip_type,is)     = type_redtape
         stack_status(ip_pointer,is)  = ip
         stack_status(ip_size,is)     = size_redtape_entry
         ip = ip + stack_status(ip_size,is)
       end do
       ip = 1
       do i = 1,number_map_entries
         is = is + 1
         do j = 1,length_stack_status - 3
           stack_status(j,is) = 0
         end do
         stack_status(ip_type,is)     = type_map
         stack_status(ip_pointer,is)  = ip
         stack_status(ip_size,is)     = size_map_entry
         ip = ip + stack_status(ip_size,is)
       end do
       ip = 1
       if (number_work_entries.ge.1) then
         irange = number_work_entries
         do i = 1,irange
           is = is + 1
           do j = 1,length_stack_status - 3
             stack_status(j,is) = 0
           end do
           stack_status(ip_type,is)     = type_work
           stack_status(ip_pointer,is)  = ip
           stack_status(ip_size,is)  = size_work_entry
           ip = ip + stack_status(ip_size,is)
         end do
       end if
       call mapcat_err(status,'stack_init',
     *                 'Stack Initialisation Failed')
       end
