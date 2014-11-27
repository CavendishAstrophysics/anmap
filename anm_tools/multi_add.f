C MULTI-ADD: Program to add many maps together
C ---------
C
C Multi add takes as input a file name and reads from the redtape a
C list of maps in the standard map catalogue to add together.
C Output is to the map specified in the call to the routine.
C-

       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/chrlib_functions.inc'

C Local variables
C ---------------
C  exit command
       character*80       command
C  data records
       real*4             in_rec(1024), out_rec(1024)
       real*4             out_image(1024*1024)
C  redtape buffer
       real*4             redtape_buffer(1024)
C  record containing list of maps; this will be a record in the extra
C  redtape slot of a standard format map.  The first 16 bytes of the
C  record are therefore reserved words.
       integer            map_list(128)
C    number of maps in list
       integer            nmaps
       equivalence       (map_list(5), nmaps)
C    start record for maps in list
       integer            start
       equivalence       (map_list(6), start)
C    length of each record in list
       integer            len_record
       equivalence       (map_list(7), len_record)
C    output map in catalogue
       integer            map_out
       equivalence       (map_list(8), map_out)
C  maximum number of maps to be added to list
       integer            max_maps
       parameter         (max_maps = 30)
C  map mini redtape
       integer            minirt(10)
C  define entry for each map
       integer            map_record(4)
       integer            map_entry
       equivalence       (map_record(1), map_entry)
       integer            map_directory
       equivalence       (map_record(2), map_directory)
       real*4             scale_factor
       equivalence       (map_record(3), scale_factor)
       real*4             zero_level
       equivalence       (map_record(4), zero_level)
C  arrays of information for each map
       integer            iunit(max_maps)
       real*4             scale(max_maps)
       real*4             zero(max_maps)
       real*4             blank(max_maps)
       integer            redtape(512,max_maps)
C  "single-valued" variables
       character*(iolen_file)
     *                    out_file, filename
       integer            iout
       integer            uv_range(4), data_type, usize,
     *                    iu, iv, i, n, jj
       real*4             zmnx(2)
       integer            izmnx(4)
C  character variables
       character          source*80, program*20
C  status word
       integer            status

C initialise
       status = 0
       call io_setexc(.true.,status)
       call io_initio
       call io_setesc(.true.)
       call cmd_init(status)
       call mapcat_open(status)

C read output map from command line, open and read redtape
       call io_getfil('Output-Map-File : ',' ',out_file,status)
       call opemap(iout,out_file,'WRITE',0,status)
       call rdredt(iout,0,status)
       call enredt(uv_range,data_type,status)
       call enxrec('ADD-MAPS',map_list,status)
       call dpredt(redtape_buffer,status)
       if (status.ne.0) goto 999

C loop, open input maps
       do n=1,nmaps
         do i=1,len_record
           map_record(i) = map_list(i + start-1 + (n-1)*len_record )
         end do
         call mapcat_enqrt(map_entry,filename,minirt,status)
         call opemap(iunit(n),filename,'READ',0,status)
         call rdredt(iunit(n),0,status)
         call ennull(blank(n),status)
         call dpredt(redtape(1,n),status)
         if (status.ne.0) goto 999
         scale(n) = scale_factor
         zero(n) = zero_level
       end do
       if (status.ne.0) goto 999

C do the addition
       zmnx(1) = -1.0E+30
       zmnx(2) =  1.0E+30
       usize = uv_range(2) - uv_range(1) + 1
       jj = 0
       do iv = uv_range(3),uv_range(4),-1
         do iu=1,usize
           out_rec(iu) = 0.0
         end do
         do n=1,nmaps
           call ldredt(redtape(1,n),status)
           call rdrow(iunit(n),iv,in_rec(1),status)
           do iu = 1,usize
             if (in_rec(iu).eq.blank(n)) then
               out_rec(iu) = blank(1)
             end if
             if (out_rec(iu).ne.blank(1)) then
               out_rec(iu) = out_rec(iu) +
     *                       (in_rec(iu)-zero(n))*scale(n)
             end if
           end do
         end do
         do iu = 1,usize
            if (out_rec(iu).gt.zmnx(1)) then
              zmnx(1) = out_rec(iu)
              izmnx(1) = uv_range(1) + iu - 1
              izmnx(2) = iv
            end if
            if (out_rec(iu).lt.zmnx(2)) then
              zmnx(2) = out_rec(iu)
              izmnx(3) = uv_range(1) + iu - 1
              izmnx(4) = iv
            end if
         end do
         call ldredt(redtape_buffer,status)
         if (status.ne.0) goto 999
         do iu=1,usize
           jj = jj + 1
           out_image(jj) = out_rec(iu)
         end do
       end do

C Update the TEXT of the redtape and map catalogue entry
       call ldredt(redtape_buffer,status)
       call adredt('created','MULTIADD',status)
       call mapcat_enqsr(map_out,source,program,status)
       call mapcat_setsr(map_out,source,program,status)
       call stnull(blank(1),status)
       call stscal(zmnx,izmnx,status)
       call wrredt(iout,0,status)
       call wrmap(iout,out_image,status)

C clear allocation to map
       call mapcat_acc(map_out,'CLEAR',status)

C close all maps
999    continue
       close (iout)

C close input maps
       do n=1,nmaps
         close (iunit(n))
       end do

C close map catalogue
       call mapcat_close(status)

C enable escape character
       call io_setesc( .false. )

C check STATUS value and report completion to the output device
       call cmd_err(status,'MULTI-ADD','Failed ')
       if (status.eq.0) then
         call io_enqout(iout)
         write(iout,'(1X,A,I3)')
     *         '.. Multi-Add completed to catalogue entry ',map_out
       else
         call io_enqout(iout)
         write(iout,'(1X,A,I3)')
     *         '.. Multi-Add Failed to construct map ',map_out
       end if
       call cmd_end( status )
       status = 0
       end
