C
C
*+ uv_posconv

       subroutine uv_posconv(map_pos,dat_pos)
C      --------------------------------------
C
C Return position in pixel coordinates
C
C Input
C    MAP_POS         -    I4(4)      -     position in UV
C
C Returned
C    DAT_POS         -    I4(4)      -     position in pixel coords.
C
C Performs a standard conversion from UV to pixel coordinates given the
C standard redtape which should be set up in /mrao/include/maplib-redtape.inc
C
*-
       include '/mrao/include/maplib_redtape.inc'

       real*4    map_pos(2), dat_pos(2)

       dat_pos(1) = map_pos(1) - iumap1 + 1
       dat_pos(2) = - map_pos(2) + ivmap1 + 1

       end
