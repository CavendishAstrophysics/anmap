


C+ic2_scnsrc

       subroutine ic2_scnsrc( redtape, map, uv, stats, posns, s )
C      ----------------------------------------------------------

C     Returns statistics for a source region

C     Given
C         Abbreviated form of map redtape
              integer     redtape(*)
C         Map data and uv window
              real        map(*)
              integer     uv(4)

C     Returned
C         Map statistics
              real        stats(6)
              integer     posns(4)
C         Status - must be zero on entry
              integer     s

C     Exactly the same as the maplib routine scnmap except there
C     are an additional two statistics returned - stats(5) contains
C     the zero level and stats(6) the map noise.
C
C     These are evaluated using one of two methods:
C     1.  If there are greater than 1000 points a standard iterative
C         technique is used where points are clipped if they are greater
C         than n times the noise in the previous iteration. The value
C         of n used is 4, and termination occurs when the noise from
C         succesive iterates differs by less than 10%.
C     2.  If there are less than 1000 points then the zero is determined
C         via a mode calculation. The points are sorted, and the zero
C         is the average of the maximum number of points which occur
C         in a flux range equal to the noise calculated from two
C         iterations of method 1. The noise is then recalculated using
C         only points less than this zero.
C
C-
      call scnsrc2( redtape, map, uv, stats, posns, s )
      end
