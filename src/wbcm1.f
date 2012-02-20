      SUBROUTINE WBCM1(A,WINGIN,BD,WB)
C
C*** COMPUTES (XAC/CBARR)B(W) FOR MACH=0.60
C
      COMMON /OPTION/ SREF,CBARR,ROUGFC,BLREF
      DIMENSION ROUTID(2),Q2136B(3),Q21C(3)
      DIMENSION A(195),WINGIN(4),WB(39),BD(87)
      DIMENSION C21C(6),X21C(13),Y21C(13)
      DIMENSION X38B(3),Y38B(3),C38B(6)
      DATA ROUTID/4HWBCM,4H1   /,Q2136B/4H4.3.,4H2.2-,4H36-B/,
     1     Q21C/4H4.2.,4H2.2-,4H35   /
C
C*********          PAGE 5-8 CALCULATIONS         **********************
C*********          FIGURE 4.3.2.2-36-B           **********************
C
      DATA X38B/0.0,1.0,1.0E06/
      DATA Y38B/0.0,0.50,0.50/
      DATA I38B/0/
C
C                  FIGURE 4.3.2.2-35
C
      DATA IN21C/0/
      DATA X21C/0.0,.025,.050,.075,.10,.15,.20,.30,.40,.50,.60,.70,.80/
      DATA Y21C/0.0,.056,.101,.130,.152,.190,.22,.266,.301,.33,.348,
     1.365,.375/
C
      SSPN = WINGIN(4)
      TEMP0= 0.25*A(7)*(1.+A(27))*A(38)
C
C*********     SELECT VALUE OF(X-PRIME)AC/(C)RE AT (BETA*A)= ZERO
C
      WB(15)= 0.50
      IF(TEMP0.GE.1.00) GO TO 1000
C
C*********          FIGURE 4.3.2.2-36-B
C
      CALL TBFUNX(TEMP0,WB(15),DYDX,3,X38B,Y38B,C38B,I38B,MI,NG,
     1            0,0,Q2136B,3,ROUTID)
C
C*********     COMPUTE VALUE OF (X-PRIME)AC/(C)RE AT (BETA*A)= 4.0
C
 1000 RATIO= BD(87)/(2.*SSPN)
      CALL TBFUNX(RATIO,BRAC,DYDX,13,X21C,Y21C,C21C,IN21C,MI,NG,
     1            0,0,Q21C,3,ROUTID)
      TEMP4= 0.25+(2.*SSPN-BD(87))/(2.*A(10))*A(44)*BRAC
C
C*********     IF (BETA*A).GT.ZERO AND .LT.4.0  USE ELLIPSE CURVE FIT
C         REVISIONS TO WBCM SUBROUTINE
C
      ARG=0.80*A(7)
      IF(ARG.LT.4.0) GO TO 1010
      WB(14)= TEMP4
      GO TO 1040
C
C*********     COMPUTE (X-PRIME)AC/(C)RE USING ELLIPSE CURVE FIT
C
 1010 X1= 4.0
      Y1= WB(15)
      Y= TEMP4
      P= 4.0
      Q= ABS(Y-Y1)
      BB= -2.0*Y1
      CC= (Y1*Y1-Q*Q+((Q/P)**2)*(ARG-X1)**2)
      IF((BB*BB-4.*CC).GT.0.0) GO TO 1030
 1020 FORMAT (30X,45H ELLIPSE CURVE FIT IN ERROR, WBCM SUBROUTINE  )
      WRITE(6,1020)
      RETURN
 1030 CONTINUE
C
C*********     SOLVE QUADRATIC EQUATION FOR WB(14), (NOTE, AA=1)
C
      YMAX= (-BB+SQRT(BB*BB-4.*CC))/2.0
      YMIN= (-BB-SQRT(BB*BB-4.*CC))/2.0
      WB(14)= YMAX
      IF(Y.LT.Y1) WB(14)= YMIN
 1040 WB(13)= WB(14)*A(10)/CBARR
      RETURN
      END
