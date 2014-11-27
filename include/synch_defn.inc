C  Include file for spectral analysis Sub-System
C
       integer      iars1
       parameter   (iars1=1500)

C define parameters for the current spectrum
       integer      synch_curr_sp(220)
       real*8       bak(100)
       real*4       bdata(10)
       real*4       gamma, gam2, alpha
       integer      iflag, igamma, kpb1,ibindx
       equivalence (synch_curr_sp(1),  iflag)
       equivalence (synch_curr_sp(2),  igamma)
       equivalence (synch_curr_sp(3),  gamma)
       equivalence (synch_curr_sp(4),  gam2)
       equivalence (synch_curr_sp(5),  alpha)
       equivalence (synch_curr_sp(6),  ibindx)
       equivalence (synch_curr_sp(7),  kpb1)
       equivalence (synch_curr_sp(11), bdata(1))
       equivalence (synch_curr_sp(21), bak(1))

C define parameters for the current two-point alpha-X relation
       integer      i2pt
       parameter   (i2pt = 100)
       real*4       xx(i2pt), aa(i2pt), af1, af2

C define arrays for construction of the spectrum
       real*8       b(iars1),xt(iars1)
       integer      isize
C
       common /synch_sp_defn/   synch_curr_sp
       common /synch_data/      xt, b, isize
       common /synch_2p/        xx, aa, af1, af2
C
