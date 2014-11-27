*+ spec_getcols

       subroutine spec_getcols( id, ncols, clist, status )
C      ---------------------------------------------------
C
C Get list of columns to work on
C
       implicit   NONE
C
C Given:
C    spectrum identifier
       integer    id
C Returned:
C    number of columns
       integer    ncols
C    column control list
       integer    clist(*)
C Updated:
C    error return code
       integer    status
C
C Use the standard parameters XC, COLS to decide which columns of
C spectrum id to use in analysis.
C
C    XC specifies the X-column ; coded with clist(xc) = 0
C    COLS specify a list of columns to use ; coded with clist() = 1
C    columns not to use are coded with clist() = -1
C    COLS = -1 or ALL specifies all columns are to be used
C    The default for XC is 1 and COLS is ALL
C
C-
       include    'spec_global.inc'
C
       integer        xc, list(max_ncols), m, n
       character      cols*20
       integer        chr_lenb

       if (status.ne.0) return
       call spec_hd_enqi( id, 'ncols', ncols, status )
       call cmd_itemi( 'XC', 1, xc, status )
       call cmd_items( 'COLS', cols, status )
       call chr_chucas( cols )
       if (cols(1:3).eq.'ALL') then
         do n=1,ncols
           clist(n) = 1
         end do
       else
         call chr_chlsti( cols(1:chr_lenb(cols)),
     *                    list, ncols, m, status )
         do n=1,ncols
           clist(n) = -1
         end do
         do n=1,m
           clist(list(n)) = 1
         end do
       end if
       if (xc.gt.0 .and. xc.le.ncols) then
         clist(xc) = 0
       end if
       end
