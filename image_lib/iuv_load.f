C
C
*+ iuv_load

       subroutine iuv_load(iuv,status)
C      -------------------------------
C
C load the current map uv-range into the array IUV
C
C Returned
C   IUV     -    I4(4)        -     integer array holding map uv-range
C   STATUS  -    I4           -     error return
C
C STATUS should be 0 on entry and is not changed
*-
       include '/mrao/include/maplib_redtape.inc'

       integer     iuv(4),status
       if (status.ne.0) return
       iuv(1)=iumap1
       iuv(2)=iumap2
       iuv(3)=ivmap1
       iuv(4)=ivmap2
       end
