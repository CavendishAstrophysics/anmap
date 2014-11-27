C ra_dpm : calculate depolarization measure and error
C
       implicit    NONE
C
C Include information on standard array sizes etc.
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
C
C Local variables
C   file identifiers
       integer    id_dpm, id_err 
       integer    id_hi, id_hmi, id_hdmi
       integer    id_li, id_lmi, id_ldmi
C   scale factors etc.
       real*4     scale_hi, scale_hmi, scale_hdmi
       real*4     scale_li, scale_lmi, scale_ldmi
       real*4     xscale
C   status return
       integer    status
C   number of data points
       integer    ndata
C   array to hold data values
       real*4     x(max_ndata)
       real*4     hi(max_ndata), hmi(max_ndata), hdmi(max_ndata)
       real*4     li(max_ndata), lmi(max_ndata), ldmi(max_ndata)
       real*4     dpm(max_ndata), err(max_ndata)
C   file and directory name(s) and strings
       character  file*(iolen_file)
C   list of columns to operate on
       integer    xc, yc
C   counters
       integer    n


C setup for input
       call io_initio
       call cmd_getiline( 'ra_dpm',define_analysis, status)
C find columns
       call cmd_itemi( 'xc', 1, xc, status )
       call cmd_itemi( 'yc', 1, yc, status )
C find input file names and allocate files
       call cmd_items( 'hi', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_hi, status)
       call spec_get_data( id_hi, xc, x, ndata, status)
       call spec_get_data( id_hi, yc, hi, ndata, status)
       call cmd_items( 'dpm', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'write', id_dpm, status)
       call spec_hd_copy( id_hi, id_dpm, status )
       call cmd_items( 'err', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'write', id_err, status)
       call spec_hd_copy( id_hi, id_err, status )
       call spec_deallocate(id_hi,status)
       call cmd_items( 'hmi', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_hmi, status)
       call spec_get_data( id_hmi, yc, hmi, ndata, status)
       call spec_deallocate(id_hmi,status)
       call cmd_items( 'hdmi', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_hdmi, status)
       call spec_get_data( id_hdmi, yc, hdmi, ndata, status)
       call spec_deallocate(id_hdmi,status)
       call cmd_items( 'li', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_li, status)
       call spec_get_data( id_li, yc, li, ndata, status)
       call spec_deallocate(id_li,status)
       call cmd_items( 'lmi', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_lmi, status)
       call spec_get_data( id_lmi, yc, lmi, ndata, status)
       call spec_deallocate(id_lmi,status)
       call cmd_items( 'ldmi', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_ldmi, status)
       call spec_get_data( id_ldmi, yc, ldmi, ndata, status)
       call spec_deallocate(id_ldmi,status)

C read control data
       call cmd_itemr( 'scale_hi', 1, scale_hi, status )
       call cmd_itemr( 'scale_hmi', 1, scale_hmi, status )
       call cmd_itemr( 'scale_hdmi', 1, scale_hdmi, status )
       call cmd_itemr( 'scale_li', 1, scale_li, status )
       call cmd_itemr( 'scale_lmi', 1, scale_lmi, status )
       call cmd_itemr( 'scale_ldmi', 1, scale_ldmi, status )
       call cmd_itemr( 'xscale', 1, xscale, status )

C scale the input data
       if (status.ne.0) goto 999
       do n=1,ndata
*         print *,n,x(n),hi(n),li(n)
         x(n) = xscale*x(n)
         hi(n) = scale_hi*hi(n)
         hmi(n) = scale_hmi*hmi(n)
         hdmi(n) = scale_hdmi*hdmi(n)
         li(n) = scale_li*li(n)
         lmi(n) = scale_lmi*lmi(n)
         ldmi(n) = scale_ldmi*ldmi(n)
       enddo

C form depolarization measure and error
       do n=1,ndata
         err(n) = 0.0
         dpm(n) = 0.0
         if (hi(n).ne.0.0) then
           hmi(n) = hmi(n)/hi(n)
           hdmi(n) = hdmi(n)/hi(n)
           if (li(n).ne.0.0) then
             lmi(n) = lmi(n)/li(n)
             ldmi(n) = ldmi(n)/li(n)
             err(n) = (hdmi(n)+ldmi(n))/(hmi(n)+lmi(n))
             dpm(n) = (hmi(n)-lmi(n))/(hmi(n)+lmi(n))
           endif
         endif
       enddo

C deallocate files
       call spec_put_data( id_dpm, 1, x, ndata, status)
       call spec_put_data( id_dpm, 2, dpm, ndata, status)
       call spec_put_data( id_err, 1, x, ndata, status)
       call spec_put_data( id_err, 2, err, ndata, status)
       call spec_hd_set( id_dpm, 'ncols', '2', status)
       call spec_hd_set( id_err, 'ncols', '2', status)
999    continue
       call spec_deallocate( id_dpm, status )
       call spec_deallocate( id_err, status )

       end




