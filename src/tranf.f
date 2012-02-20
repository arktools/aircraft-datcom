      SUBROUTINE TRANF (NPT,X,Y,DYL,DYR,XVAL,YVAL)
C
C***  COMPUTES TRANSONIC VENTRAL FIN CL BY NON-LINEAR INTERPOLATION
C
      INTEGER RP
      DIMENSION X(NPT),Y(NPT),YP(2)
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD,KAND
C
C   METHOD - SLOPES ARE DEFINED AT EACH DATA POINT AS A VALUE HALF WAY
C            BETWEEN THE LINEAR SLOPES TO THE RIGHT AND LEFT OF THE
C            POINT. A THIRD ORDER POLYNOMIAL IS DETERMINED FOR EACH
C            INTERVAL BETWEEN DATA POINTS BY USING THE TWO END POINTS
C            AND TWO END POINT SLOPES. THE LEFT AND RIGHT END SLOPES
C            ARE INPUT. THE RESULTING CURVE AND ITS DERIVATIVES ARE
C            CONTINUOUS.
C
C   NPT      NUMBER OF POINTS IN X AND Y ARRAYS
C   X        ABSCISSAS OF INPUT DATA
C   Y        ORDINATES OF INPUT DATA
C   DYL      LEFT END SLOPE
C   DYR      RIGHT END SLOPE
C
C   XVAL     ABSCISSA VALUE AT WHICH INTERPOLATION TO BE PERFORMED
C   YVAL     INTERPOLATED ORDINATE VALUE
C
      NPT1 = NPT-1
      DO 1000 J = 2,NPT1
        IF(XVAL .LT. X(J)) GO TO 1010
 1000 CONTINUE
      J = NPT
 1010 CONTINUE
      J = J-1
      K = J+1
C
      DO 1040 N = 1,2
        MP = J+N-1
        LP = MP-1
        RP = MP+1
C
        IF(MP .NE. 1) GO TO 1020
          YP(N)     = DYL
 1020   IF(MP .EQ. 1 .OR. MP .EQ. NPT) GO TO 1030
          SL    = (Y(LP)-Y(MP))/(X(LP)-X(MP))
          SR    = (Y(MP)-Y(RP))/(X(MP)-X(RP))
          ANGL  = ATAN(SL)
          ANGR  = ATAN(SR)
          ANGAV = (ANGL+ANGR)/2.0
          YP(N) = SIN(ANGAV)/COS(ANGAV)
           IF(Y(MP) .LE. 0.0) YP(N) = 0.0
          IF((NPT .GT. 10) .AND. (ABS(SL) .LE. UNUSED .OR.
     1        ABS(SR) .LE. UNUSED)) YP(N) = 0.0
          IF(SR .EQ. 0.0) GO TO 1030
            IF((SL/SR) .LT. 0.0) YP(N) = 0.0
 1030   IF(MP .NE. NPT) GO TO 1040
          YP(N) = DYR
 1040 CONTINUE
C
      X1S  = X(J)*X(J)
      X2S  = X(K)*X(K)
      X12F = X(J)-X(K)
      Y12F = Y(J)-Y(K)
      X12S = X1S-X2S
      X12C = X1S*X(J)-X2S*X(K)
      Y12P = YP(1)-YP(2)
C
      RED = 2.0*X(K)*X12F - X12S
      GRN = 3.0*X2S*X12F - X12C
      YEL = YP(2)*X12F-Y12F
      E   = 3.0*X12S*RED - 2.0*X12F*GRN
      A   = Y12P*RED - 2.0*X12F*YEL
      A   = A/E
      B   = 3.0*X12S*YEL - Y12P*GRN
      B   = B/E
      C   = (Y12F-A*X12C-B*X12S)/X12F
      D   = Y(K)-A*X2S*X(K)-B*X2S-C*X(K)
C
      YVAL = (((A*XVAL +B)*XVAL)+C)*XVAL+D
      IF(YVAL .LT. 0.0) YVAL = 0.0
C
      RETURN
      END
