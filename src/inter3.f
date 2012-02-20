      SUBROUTINE INTER3(ARG1,ARG2,RL   ,A1,A2,YA,N1A,N2A,QA,B1,B2,YB,
     1            N1B,N2B,QB,C1,C2,YC,N1C,N2C,QC,D1,D2,YD,N1D,N2D,QD,
     2            E1,E2,YE,N1E,N2E,QE,ANS)
C
C***  TABLE LOOKUP ROUTINE FOR SPECIFIC TABLE
C
      DIMENSION A1(2),A2(2),YA(2),QA(3),B1(2),B2(2),YB(2),QB(3),C1(2),
     1          C2(2),YC(2),QC(3),D1(2),D2(2),YD(2),QD(3),E1(2),E2(2),
     2          YE(2),QE(3),AA(2)
      DIMENSION ROUTID(2)
      DATA ROUTID/4HINTE,4HR3  /
      IF(RL.LE.1E5)IT=1
      IF(RL.GT.1E5.AND.RL.LE.1E6)IT=2
      IF(RL.GT.1E6.AND.RL.LE.1E7)IT=3
      IF(RL.GT.1E7.AND.RL.LE.1E8)IT=4
      IF(RL.GT.1E8.AND.RL.LE.1E9)IT=5
      IF(RL.GT.1E9)IT=6
      DO 1050 K=1,2
         GO TO(1000,1010,1020,1030,1040,1040),IT
C
C    ----HERE FOR RL=1E5
C
 1000   CONTINUE
         IF(K.EQ.1)X1=1E5
         CALL TLINEX(A1,A2,YA,N1A,N2A,ARG1,ARG2,AA(K),
     1               0,0,0,0,QA,3,ROUTID)
         X2=X1+1.
         GO TO 1050
C
C    ----HERE FOR RL=1E6
C
 1010    IF(IT.EQ.2.AND.K.EQ.1)GO TO 1000
         IF(K.EQ.1)X1=1E6
         CALL TLINEX(B1,B2,YB,N1B,N2B,ARG1,ARG2,AA(K),
     1               0,0,0,0,QB,3,ROUTID)
         X2=1E6
         GO TO 1050
C
C    ----HERE FOR RL=1E7
C
 1020    IF(IT.EQ.3.AND.K.EQ.1)GO TO 1010
         IF(K.EQ.1)X1=1E7
         CALL TLINEX(C1,C2,YC,N1C,N2C,ARG1,ARG2,AA(K),
     1               0,0,0,0,QC,3,ROUTID)
          X2=1E7
         GO TO 1050
C
C    ----HERE FOR RL=1E8
C
 1030    IF(IT.EQ.4.AND.K.EQ.1)GO TO 1020
         IF(K.EQ.1)X1=1E8
         CALL TLINEX(D1,D2,YD,N1D,N2D,ARG1,ARG2,AA(K),
     1               0,0,0,0,QD,3,ROUTID)
         X2=1E8
         GO TO 1050
C
C    ----HERE FOR RL=1E9
C
 1040    IF(IT.EQ.5.AND.K.EQ.1)GO TO 1030
         IF(K.EQ.1)X1=1E9
         CALL TLINEX(E1,E2,YE,N1E,N2E,ARG1,ARG2,AA(K),
     1               0,0,0,0,QE,3,ROUTID)
         X2=1E9
         IF(IT.EQ.6)ANS=AA(1)
         IF(IT.EQ.6)RETURN
 1050 CONTINUE
      ARG=(RL-X1)/(X2-X1)
      ANS= AA(1)+(AA(2)-AA(1))*ARG
      RETURN
      END
