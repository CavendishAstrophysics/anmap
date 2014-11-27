C
C F77 header file for image catalogue structures
C

C standard data definition structure
       integer   len_defn
       parameter (len_defn = 20)
       integer   image_defn(len_defn)
       integer   ndims, ndata, xdim, ydim, zdim, tdim,
     *           vdim,x1,x2,y1,y2,z1,z2,t1,t2,v1,v2,dtype
       real*4    blank, norm

       equivalence (image_defn(1),ndims)
       equivalence (image_defn(2),ndata)
       equivalence (image_defn(3),xdim)
       equivalence (image_defn(4),ydim)
       equivalence (image_defn(5),zdim)
       equivalence (image_defn(6),tdim)
       equivalence (image_defn(7),vdim)
       equivalence (image_defn(8),x1)
       equivalence (image_defn(9),x2)
       equivalence (image_defn(10),y1)
       equivalence (image_defn(11),y2)
       equivalence (image_defn(12),z1)
       equivalence (image_defn(13),z2)
       equivalence (image_defn(14),t1)
       equivalence (image_defn(15),t2)
       equivalence (image_defn(16),v1)
       equivalence (image_defn(17),v2)
       equivalence (image_defn(18),dtype)
       equivalence (image_defn(19),blank)
       equivalence (image_defn(20),norm)

C standard position vector (real)
       integer   len_vecf
       parameter (len_vecf = 5)
       integer   image_vecf(len_vecf)
       real*4    xp, yp, zp, tp
       integer   vp

       equivalence (image_vecf(1),xp)
       equivalence (image_vecf(2),yp)
       equivalence (image_vecf(3),zp)
       equivalence (image_vecf(4),tp)
       equivalence (image_vecf(5),vp)
C
C Data types
       integer    type_map
       parameter (type_map  = 0)
       integer    type_df
       parameter (type_df   = 1)
       integer    type_fits
       parameter (type_fits = 2)
