* Plot/graphic system errors
* --------------------------
C
C
C Basic error associated with the graphics system
      integer    ill_device
      parameter (ill_device = -8800) ! Unable to open graphics device
      integer    no_grcurr
      parameter (no_grcurr  = -8801) ! No current graphic defined
      integer    ill_grdev
      parameter (ill_grdev  = -8802) ! Illegal logical-device No.

C Cursor/input related errors
      INTEGER    ILL_ONPIC, ILL_CURSE, ILL_INTER, ILL_POS
      PARAMETER (ILL_ONPIC = -8810)  ! Point not on frame
      PARAMETER (ILL_CURSE = -8820)  ! Error reading cursor
      PARAMETER (ILL_INTER = -8821)  ! Cursor reading abandoned
      PARAMETER (ILL_POS   = -8825)  ! Error in input position

C drawing-system related errors
      integer    no_object, no_itext
      parameter (no_object = -8831)  ! No more objects for this drawing
      parameter (no_itext = -8832)   ! Not enough space to hold text

