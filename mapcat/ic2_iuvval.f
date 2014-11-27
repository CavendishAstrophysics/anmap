

*+ic2_iuvval

       subroutine ic2_iuvval(redtape, data, iuv, value, status)
C      --------------------------------------------------------
C
C  Returns the map value at a given, integral u, v point.
C
C  Given:
C   Abbreviated version of map redtape.
       integer   redtape(*)
C   map data
       real*4    data(*)
C   U and V coordinate on map.
       integer   iuv(2)
C
C  Returned:
C   real or complex map value
       real*4    value
C   status value - should be unchanged.
       integer   status
C
C  Returns the value in external units of the data point IUV from
C  map data held by rows in array DATA.  The abbreviated redtape is
C  assumed to be set up appropriately for the data array.  If the point
C  IUV lies outside the data array, the 'undefined data' value is
C  returned. If the data type of the map is complex, then the value
C  returned is complex.
C
C
*-
       call iuvval2(redtape, data, iuv, value, status)
       end
