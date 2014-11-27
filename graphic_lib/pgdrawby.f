C
C
*+

       SUBROUTINE PLT_DRAWBY(X,Y)
C      --------------------------
C
C Draw by X,Y
C
*-
       REAL*4  X, Y, X_P, Y_P
       COMMON /PLT_COM1/ X_P,Y_P
       X_P = X_P + X
       Y_P = Y_P + Y
       CALL PGDRAW(X_P,Y_P)
       END
