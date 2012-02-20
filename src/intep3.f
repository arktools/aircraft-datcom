      SUBROUTINE INTEP3(ARG1,ARG2,LAMDA,A1,A2,YA,N1A,N2A,QA,B1,B2,YB,
     1            N1B,N2B,QB,C1,C2,YC,N1C,N2C,QC,D1,D2,YD,N1D,N2D,QD,
     2            E1,E2,YE,N1E ,N2E,QE,ANS)
C
C***  TABLE LOOKUP ROUTINE FOR A SPECIFIC TABLE
C
      DIMENSION A1(2),A2(2),YA(2),QA(3),B1(2),B2(2),YB(2),QB(3),C1(2),
     1          C2(2),YC(2),QC(3),D1(2),D2(2),YD(2),QD(3),
     2          E1(2),E2(2),YE(2),QE(3) ,AA(2)
      REAL LAMDA
      DIMENSION ROUTID(2)
      DATA ROUTID/4HINTE,4HP3  /
      IF(LAMDA.EQ.0.)IT=1
      IF(LAMDA.GT.0.0.AND.LAMDA.LE.0.25)IT=2
      IF(LAMDA.GT.0.25.AND.LAMDA.LE.0.50)IT=3
      IF(LAMDA.GT.0.50.AND.LAMDA.LE.0.75)IT=4
      IF(LAMDA.GT.0.75.AND.LAMDA.LE.1.00)IT=5
      IF((IT.EQ.4.OR.IT.EQ.5).AND.N2D.EQ.1)IT=6
      DO 1050 K=1,2
         GO TO(1000,1010,1020,1030,1040,1040),IT
C
C    ----HERE FOR 0.0 LAMDA
C
 1000    CONTINUE
         IF(K.EQ.1)X1=0.0
         CALL TLIP2X(A1,A2,YA,N1A,N2A,ARG1,ARG2,AA(K),
     1               0,0,0,0,QA,3,ROUTID)
         X2=X1+1.
         GO TO 1050
C
C    ----HERE FOR 0.25 LAMDA
C
 1010    IF(IT.EQ.2.AND.K.EQ.1)GO TO 1000
         IF(K.EQ.1)X1=0.25
         CALL TLIP2X(B1,B2,YB,N1B,N2B,ARG1,ARG2,AA(K),
     1               0,0,0,0,QB,3,ROUTID)
         X2=0.25
         GO TO 1050
C
C    ----HERE FOR 0.50 LAMDA
C
 1020    IF(IT.EQ.3.AND.K.EQ.1)GO TO 1010
         IF(K.EQ.1)X1=0.50
         CALL TLIP2X(C1,C2,YC,N1C,N2C,ARG1,ARG2,AA(K),
     1               0,0,0,0,QC,3,ROUTID)
         X2=0.50
         GO TO 1050
C
C    ----HERE FOR 0.75 LAMDA
C
 1030    IF(IT.EQ.4.AND.K.EQ.1)GO TO 1020
         IF(K.EQ.1)X1=0.75
         CALL TLIP2X(D1,D2,YD,N1D,N2D,ARG1,ARG2,AA(K),
     1               0,0,0,0,QD,3,ROUTID)
         X2=0.75
         GO TO 1050
C
C    ----HERE FOR 1.0 LAMDA
C
 1040    IF(IT.EQ.5.AND.K.EQ.1)GO TO 1030
         IF(IT.EQ.6.AND.K.EQ.1)GO TO 1020
         CALL TLIP2X(E1,E2,YE,N1E,N2E,ARG1,ARG2,AA(K),
     1               0,0,0,0,QE,3,ROUTID)
         X2=1.00
 1050 CONTINUE
      ARG=(LAMDA-X1)/(X2-X1)
      ANS= AA(1)+(AA(2)-AA(1))*ARG
      RETURN
      END
