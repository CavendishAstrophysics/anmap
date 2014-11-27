C
C
*+

       SUBROUTINE PLT_MOVBY(X,Y)
C      -------------------------
C
C Move pen by X,Y
C
*-
       REAL*4  X, Y, X_P, Y_P
       COMMON /PLT_COM1/ X_P,Y_P
       X_P = X_P + X
       Y_P = Y_P + Y
       CALL PGMOVE(X_P,Y_P)
       END
