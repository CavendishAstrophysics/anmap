C
C
*+ plot_dosymb

       subroutine plot_dosymb(map_array,status)
C      ----------------------------------------
C
C Produce a symbol plot specifying certain data types
C
C Updated:
C   Error status
       integer    status
C
C Plot an (overlayed) symbol plot
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C data arrays
       real*4    map_array(*)
C counters
       integer   iu, iv
C data value
       real*4    x
C null data value
       real*4    blank_value
C character attributes
       real*4    size, old_size

C check status on entry
       if (status.ne.0) return

C check for grey-scale option
       if (.not.symbol_opt) return

C turn off immediate update buffering
       call pgbbuf

C do the plot
       call graphic_set_text_opt(symbol_text_style,status)
       call ennull(blank_value,status)
       call pgqch( size )
       size = size*35.0/float(uv_range(3)-uv_range(4))
       call pgsch( size )
       if (status.ne.0) goto 999
       do iv=uv_range(3),uv_range(4),-1
         do iu=uv_range(1),uv_range(2)
           call iuvval(map_array,iu,iv,x,status)
           if (status.ne.0) goto 999
           if (x.eq.blank_value .and. symb_blank.gt.0) then
             call pgpoint(1,float(iu),float(iv),symb_blank)
           else if (x.ge.val_symb_max .and. symb_max.gt.0) then
             call pgpoint(1,float(iu),float(iv),symb_max)
           else if (x.le.val_symb_min .and. symb_min.gt.0) then
             call pgpoint(1,float(iu),float(iv),symb_min)
           end if
         end do
       end do
       call pgsch( old_size )

C return to immediate update state
999    call pgebuf

       call cmd_err(status,'PLOT_DOSYMB',' ')

       end
