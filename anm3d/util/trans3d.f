



C   size, centre pixel of output region and loop counters
       integer    nx, ny, nz
       integer    nxi, nyi, nzi, nii
*       parameter (nddd = 256*256*256)
       parameter (nddd = 256*256*256)
       real*4     dout( nddd )
       integer    din( nddd )
       integer    status

C prompt for file and open
       status = 0
       call io_geti( 'X-dimension : ','256',nx,status)
       call io_geti( 'X-sub-dimension : ','256',nxi,status)
       call io_geti( 'Y-dimension : ','256',ny,status)
       call io_geti( 'Y-sub-dimension : ','256',nyi,status)
       call io_geti( 'Z-dimension : ','256',nz,status)
       call io_geti( 'Z-sub-dimension : ','256',nzi,status)
       nii = (nx * ny * nz ) / (nxi *nyi *nzi )
       print *,'Found ',nii,' sub-images'
       call dowork( nx, ny, nz, nxi, nyi, nzi, nii, dout, din, status )
       end
C
C
       subroutine dowork( nx, ny, nz, nxi, nyi, nzi, nii,
     *                    dout, din, status )
C   file name and user name
       character  infile*(64), outfile*(64)
       integer    iun, iuo
       integer    i, j, k, l, i1, j1, k1, ii, jj, kk
       integer    nx, ny, nz, n
       integer    nxi, nyi, nzi, nii, ix1, iy1, iz1
       real*4     dout(nx, ny, nz)
       integer    din(nxi, nyi, nzi, nii)
       integer    chr_lenb, status

       call io_getfil('Input file name : ',' ',infile,status)
       call io_getfil('Output file name : ',' ',outfile,status)
       n = nx*ny*nz
       call io_operan(iun,infile(1:chr_lenb(infile)),
     *                  'READ',n*4,0,status)
       call io_rdfile(iun,1,din,n,status)
       close (iun)

       ix1 = nx/nxi
       iy1 = ny/nyi
       iz1 = nz/nzi

       l = 0
       do k1=1,iz1
         do j1=1,iy1
           do i1=1,ix1
              l = l + 1
              do k=1,nzi
                do j=1,nyi
                  do i=1,nxi
                     ii = i + (i1-1)*nxi
                     jj = j + (j1-1)*nyi
                     kk = k + (k1-1)*nzi
                     dout(ii,jj,kk) = din(i,j,k,l)
                  enddo
                enddo
              enddo
           enddo
         enddo
       enddo
       call io_operan(iuo,outfile(1:chr_lenb(outfile)),
     *                  'WRITE',n*4,0,status)
       call io_wrfile(iun,1,dout,n,status)
       close (iuo)

       end
