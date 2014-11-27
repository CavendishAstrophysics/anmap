
*+ic2_scnmap

       subroutine ic2_scnmap(redtape, data, iuv, zmnx, izmnx, status)
C      --------------------------------------------------------------
C
C  Scans a map area for data maximum and minimum.
C
C  Given:
C   Abbreviated version of map redtape.
       integer   redtape(*)
C   map data
       real*4    data(*)
C   U,V coordinate window
       integer   iuv(4)
C
C  Returned:
C   maximum, minimum data values
       real*4    zmnx(4)
C   U,V coordinates of max, min points
       integer   izmnx(4)
C   status value
       integer   status
C
C  Scans the map data held by rows in array DATA, within the U,V window
C  specified by the array IUV (IU1,IU2,IV1,IV2) and finds the maximum and
C  minimum data values and their U,V coordinates.  The mean and standard
C  deviation of the data within the specified window are also returned:
C
C      ZMNX(1)  maximum data value
C      ZMNX(2)  minimum data value
C      ZMNX(3)  mean data value
C      ZMNX(4)  standard deviation
C      IZMNX(1,2)  U,V coordinates of data maximum
C      IZMNX(3,4)  U,V coordinates of data minimum
C
C  The redtape parameter is assumed to be set up appropriately for
C  the input data array.
C
C  For aperture data, the map array should be set up to provide complex
C  data values:  the routine returns the max, min, etc amplitude values.
C
C  The STATUS values should be zero on entry, and is normally unchanged
C  on exit.  Possible error conditions are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C
*-
       call scnmap2(redtape, data, iuv, zmnx, izmnx, status)
       end
