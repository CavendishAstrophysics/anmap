C
C
*+

       SUBROUTINE PLT_DRAWTO(X,Y)
C      --------------------------
C
C Draw to X,Y
C
*-
       REAL*4  X, Y, X_P, Y_P
       COMMON /PLT_COM1/ X_P,Y_P
       X_P = X
       Y_P = Y
       CALL PGDRAW(X_P,Y_P)
       END
