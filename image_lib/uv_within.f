C
C
*+ uv_within

       logical function uv_within(uv_1,uv_2)
C      -------------------------------------
C
C Test UV_1 is within UV_2 in terms of MAP UV-ranges
*-
       integer     uv_1(4),uv_2(4)
       uv_within = uv_1(1).ge.uv_2(1) .and. uv_1(2).le.uv_2(2) .and.
     *             uv_1(3).le.uv_2(3) .and. uv_1(4).ge.uv_2(4)
       end
