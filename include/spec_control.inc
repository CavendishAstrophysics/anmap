C Control information for the spectrum package
C
C Define the type of each entry in the control block
C   allocation type
      integer      alloc
      parameter   (alloc = 1)
C   allocation values
      integer      alloc_none
      parameter   (alloc_none = 0)
      integer      alloc_read
      parameter   (alloc_read = 1)
      integer      alloc_write
      parameter   (alloc_write = 2)
      integer      alloc_update
      parameter   (alloc_update = 3)
      integer      alloc_scratch
      parameter   (alloc_scratch = 4)
C   file unit number
      integer      unit
      parameter   (unit = 2)
C
C Control records in common
      integer      control_records(max_entries, max_buff)
      common /spec_control_block/ control_records
