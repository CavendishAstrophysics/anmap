C
C
*+ iuv_convert

       subroutine iuv_convert(map_range,map_area)
C      ------------------------------------------
C
C Return range as pixel offsets from UV representation
C
C Input
C    MAP_RANGE        -    I4(4)      -     region in UV
C
C Returned
C    MAP_AREA         -    I4(4)      -     region in pixel coords.
C
C Performs a standard conversion from UV to pixel coordinates given the
C standard redtape which should be set up in /mrao/include/maplib_redtape.inc
C
*-
       include '/mrao/include/maplib_redtape.inc'

       integer     map_range(4),map_area(4)

       map_area(1) = map_range(1) - iumap1 + 1
       map_area(2) = map_range(2) - iumap1 + 1
       map_area(3) = - map_range(3) + ivmap1 + 1
       map_area(4) = - map_range(4) + ivmap1 + 1
       end
