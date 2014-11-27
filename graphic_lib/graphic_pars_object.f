*+ graphic_pars_object

       subroutine graphic_pars_object( opt, s )
C      ----------------------------------------
C
C Define command-language parameters for object-option structure
C
C Given:
C    structure
       integer    opt
C
C Updated:
C    error status
       integer    s
C
C-
       include '../include/plt_basic_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

       character  text*80, name*14
       integer    lt, n

C check status on entry
       if (s.ne.0) return

C copy structure
       call graphic_copy_object( opt, object, s )

C setup parameters
       call chr_chitoc( object_status, text, lt )
       call cmd_setlocal('%object-status',text(1:lt), s )
       call cmd_setlocal('%object-type',object_name(object_type),s)
       call chr_chitoc( object_fill, text, lt )
       call cmd_setlocal('%object-fill',text(1:lt), s )
       call cmd_defparam(.false.,'%object-dim','real',6,s)
       do n=1,6
         write(name,'(''%object-dim{'',i1,''}'')') n
         call chr_chitoc( object_dim(n), text, lt )
         call cmd_setlocal(name,text(1:lt), s )
       enddo
       call cmd_err(s,'graphic_pars_object',' ')
       end
