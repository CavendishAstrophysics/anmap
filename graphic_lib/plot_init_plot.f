C
C
*+ plot_init_plot

       subroutine plot_init_plot( s )
C      ------------------------------
C
C Initialise the current plot for (re-)plotting
C
C Updated:
C   error status
       integer    s
C
C Initialise the image-display plot so that the next plot-all command
C will plot/re-plot all elements in the image-display (image_defn) struture.
C
C-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

       integer   n

       if (s.ne.0) return

C initialise for setting up the frane
       frame_init = .false.

C reset contours to not plotted
       do n=1,max_contours
         contour_status(n) = abs(contour_status(n))
       enddo

C initialise options
       image_done = .false.
       vectors_done = .false.
       symbol_done = .false.
       title_done = .false.
       date_done = .false.
       cont_done = .false.
       text_done = .false.
       pframe_done = .false.
       crosses_done = .false.

C initialise plot title
       title_plot = ' '

C initialise the plotting region itself
       call graphic_copy_graphic( image_graphic, graphic, s )
       graphic_status = 0
       call graphic_copy_graphic( graphic, image_graphic, s )


       end

