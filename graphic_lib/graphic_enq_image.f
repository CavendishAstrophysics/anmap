*+ graphic_enq_image

       subroutine graphic_enq_image( interp, var, defn, s )
C      ----------------------------------------------------
C
C Enquire the status information for the supplied image graphic
C
C Given:
C   pointer to tcl interpreter
       integer    interp(*)
C   variable name to hold graphic structure
       character  var*(*)
C   definition to enquire information of
       integer    defn(*)
C
C Update:
C   errot status
       integer    s
C
C The current image graphic record is enquired and the results
C placed in the name variable.
C
C-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C local variables
       integer          l
       character        name*64, str*256

       if (s.ne.0) return

C copy graphic structure
       do l=1,len_image_graphic
          image_defn(l) = defn(l)
       enddo

       call iocmd_tclsetl(interp,frame_init,var,'frame_init',s)
       call iocmd_tclseti(interp,uv_range,4,var,'uv_range',s)
       call iocmd_tclseti(interp,imap,1,var,'imap',s)
       call iocmd_tclseti(interp,imap_overlay,1,var,'imap_overlay',s)
       call iocmd_tclseti(interp,imap_current,1,var,'imap_current',s)
       call iocmd_tclsetl(interp,map_defined,var,'map_defined',s)
       call iocmd_tclsetl(interp,overlay_defined,var,'overlay_defined',s)
       call iocmd_tclsetl(interp,overlay_map,var,'overlay_map',s)
       call iocmd_tclsetl(interp,interpolate_opt,var,'interpolate_opt',s)
       call iocmd_tclsetr(interp,data_range,4,var,'data_range',s)


       call cmd_err(s,'graphic_enq_image','failed')
       end

