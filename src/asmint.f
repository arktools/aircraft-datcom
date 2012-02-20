      SUBROUTINE ASMINT (NPT,X,Y,NVAL,XVAL,YVAL)
C
C***  NON-LINEAR INTERPOLATION ROUTINE FOR AIRFOIL MODULE
C
      INTEGER RP
      DIMENSION X(1),Y(1),XVAL(1),YVAL(1),YP(2)
C
C   METHOD - SLOPES ARE DEFINED AT EACH DATA POINT AS A VALUE HALF WAY
C            BETWEEN THE LINEAR SLOPES TO THE RIGHT AND LEFT OF THE
C            POINT. A THIRD ORDER POLYNOMIAL IS DETERMINED FOR EACH
C            INTERVAL BETWEEN DATA POINTS BY USING THE TWO END POINTS
C            AND TWO END POINT SLOPES. SECOND ORDER POLYNOMIALS ARE
C            DETERMINED FOR THE LEFT AND RIGHT END INTERVALS (AND FOR
C            LEFT AND RIGHT EXTRAPOLATION) USING THE TWO END POINTS
C            AT EACH END OF THE DATA AND THE SLOPES AT THE NEXT-TO-THE
C            LAST DATA POINTS AT EACH END OF THE DATA. HENCE, THE OVER-
C            ALL CURVE CONSISTS OF A LEFT END PARABOLA JOINED TO A
C            SERIES OF JOINED CUBIC CURVES FINALLY JOINED TO A RIGHT
C            END PARABOLA. THE OVERALL CURVE IS CONTINUOUS EVERYWHERE,
C            HAS CONTINUOUS DERIVATIVES EVERYWHERE, AND CAN BE EXTRAP-
C            OLATED TO THE RIGHT AND LEFT INDEFINITELY.
C
C   NPT      NUMBER OF POINTS IN X AND Y ARRAYS
C   X        ABSCISSAS OF INPUT DATA
C   Y        ORDINATES OF INPUT DATA
C
C   NVAL     NUMBER OF POINTS TO BE INTERPOLATED
C   XVAL     ABSCISSA VALUES AT WHICH INTERPOLATION TO BE PERFORMED
C   YVAL     INTERPOLATED ORDINATE VALUES
C
      NP1= NPT-1
      NP2= NPT-2
      DO 1080 I=1,NVAL
         IF(XVAL(I).LT.X(2)) GO TO 1020
         IF(XVAL(I).GT.X(NP1)) GO TO 1030
            DO 1010 J=2,NP2
            IF(XVAL(I).NE.X(J)) GO TO 1000
            YVAL(I)= Y(J)
            GO TO 1080
 1000       K= J+1
            IF(XVAL(I).GE.X(K))GO TO 1010
            LOCATE= 2
            GO TO 1040
 1010    CONTINUE
         YVAL(I)= Y(K)
         GO TO 1080
 1020    J=2
         K=3
         LOCATE= 1
         GO TO 1040
 1030    J=NP1
         K=NPT
         LOCATE= 3
         GO TO 1040
 1040    DO 1050 N=1,2
            LP= J+N-2
            MP= J+N-1
            RP= K+N-1
            SL= (Y(LP)-Y(MP))/(X(LP)-X(MP))
            SR= (Y(MP)-Y(RP))/(X(MP)-X(RP))
            ANGL= ATAN(SL)
            ANGR= ATAN(SR)
            ANGAV=(ANGL+ANGR)/2.0
            YP(N)= SIN(ANGAV)/COS(ANGAV)
            GO TO (1070,1050,1070),LOCATE
 1050    CONTINUE
C
         X1S = X(J)*X(J)
         X2S = X(K)*X(K)
         X12F = X(J)-X(K)
         Y12F = Y(J)-Y(K)
         X12S = X1S-X2S
         X12C = X1S*X(J)-X2S*X(K)
         Y12P = YP(1)-YP(2)
C
         TW = 2.0
         TH = 3.0
         RED = TW*X(K)*X12F - X12S
         GRN = TH*X2S*X12F - X12C
         YEL = YP(2)*X12F-Y12F
         E = TH*X12S*RED - TW*X12F*GRN
         A = Y12P*RED - TW*X12F*YEL
         A = A/E
         B = TH*X12S*YEL - Y12P*GRN
         B = B/E
         C = (Y12F-A*X12C-B*X12S)/X12F
         D = Y(K)-A*X2S*X(K)-B*X2S-C*X(K)
C
 1060    YVAL(I)=(((A*XVAL(I) +B)*XVAL(I))+C)*XVAL(I)+D
         GO TO 1080
C
 1070    J=1
         IF(LOCATE.EQ.3) J=NP1
         K=J+1
         L=2
         IF(LOCATE.EQ.3) L=NP1
         Z=(Y(J)-Y(K))/(X(J)-X(K))
         A=0.0
         B=(YP(1)-Z)/(2.0*X(L)-X(J)-X(K))
         C=YP(1)-2.0*B*X(L)
         D=Y(J)-((B*X(J)+C)*X(J))
         GO TO 1060
 1080 CONTINUE
      RETURN
      END
