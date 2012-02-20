      SUBROUTINE VFDRAG(I)
C
C***  COMPUTES VENTRAL FIN DRAG COEFFICIENT
C
      DIMENSION ROUTID(2)
      DIMENSION Q15127(3),Q5128B(3),Q128BD(5)
      COMMON /OPTION/ SREF,CBARR,ROUGFC,BLREF
      COMMON /FLGTCD/ FLC(160)
      COMMON /WHAERO/ C2(51),D2(55),CHT2(51),DHT2(55),DVT2(55),DVT(55)
      COMMON /VTDATA/ AAA(195),AVT(195)
      COMMON /VTI/    VV(154),TVTIN(8),VT(154)
      DIMENSION D(55),A(195),B(93)
      EQUIVALENCE (A(1),AVT(1)),(D(1),DVT(1)),(B(1),FLC(1))
      DIMENSION WTYPE(4)
      DIMENSION X228BD(9),X128BD(4),Y28BD(36)
      DIMENSION X228B(11), X128B(4), Y28B(44)
      DIMENSION X27M(4),X27I(4),C27(6)
      DATA ROUTID/4HVTDR,4HAG  /
      DATA Q15127 /4H4.1.,4H5.1-,4H27  /,Q5128B/4H4.1.,4H5.1-,4H28-B/,
     1     Q128BD /4H4.1.,4H5.1-,4H28-B,4H(DAS,4HHED)/
      DATA X27M   /0.0,1.0,2.0,3.0/
      DATA X27I   /1.57780,1.67221,1.98509,2.28874/
      DATA I27    /0/
C
C---------          FIGURE 4.1.5.1-28B            ----
C----     X228B=COS(SWEEP ANGLE(T/C)MAX)   X128B= MACH NUMBER
C----     Y28B= (R)LS
C
      DATA WTYPE/ 4HSTRA ,4HDOUB ,4HCRAN ,4HCURV /
      DATA X228B/0.5,.55,.60,.65,.70,.75,.80,.85,.90,.95,1.0/
      DATA X128B/0.9,.80,.60,.25/
      DATA Y28B/ 1.1,1.13,1.17,1.20,1.24,1.27,1.3,1.33,1.34,1.35,1.36 ,
     11.0,1.04,1.08,1.11,1.15,1.18,1.21,1.23,1.25,1.25,1.26 , 0.88, .92,
     2.96,1.0,1.04,1.08,1.11,1.13,1.14,1.14,1.15 , 0.81,.85,.89,.925,
     3.96,1.0,1.03,1.05,1.06,1.06,1.07/
C
C---------          FIGURE 4.1.5.1-28B-DASHED     ---------
C----          X228BD= COS(T/C)MAX      X128BD= MACH NUMBER
C----          Y28BD= (R)LS
C
      DATA X228BD/0.45,.65,.70,.75,.80,.85,.90,.95,1.0/
      DATA X128BD/0.9,.80,.60,.25/
      DATA Y28BD/1.20,1.20,1.24,1.27,1.30,1.33,1.34,1.35,1.36 , 1.11,
     11.11,1.15,1.18,1.21,1.23,1.25,1.25,1.26 , 1.0,1.0,1.04,1.08,1.11,
     21.13,1.14,1.14,1.15 , 0.925,0.925,.96,1.0,1.03,1.05,2*1.06,1.07/
C
      D(2)=12.0*A(16)/ROUGFC
C
C---------     DETERMINE WING CONFIGURATION       ----------------------
C              IF NOT STRAIGHT-TAPERED, GO TO PAGE 3-2
C
      IF(VT(15).NE.WTYPE(1)) GO TO 1010
C
C---------          PAGE 3-1 CALCULATIONS         ----------------------
C*********     FOR STRAIGHT-TAPER WING CONFIGURATION        ************
C
C---------     COMPUTE REYNOLDS NUMBER, (RN)
C
 1000 D(17)=A(16)*A(129)
C
C---------     COMPUTE RL FROM EQUATION FOR FIG.4.1.5.1-27
C*********          FIGURE 4.1.5.1-27 EQUATION
C
      CALL TBFUNX(B(I+2),CEPT,DYDX,4,X27M,X27I,C27,I27,MI,NG,
     1            0,0,Q15127,3,ROUTID)
      D(14)=D(2)**1.0482*10.0**CEPT
C
C*********          FIGURE 4.1.5.1-26 EQUATIONS (SUBROUTINE FIG26 )
C
      IF( D(14).LT.D(17)) D(17)=D(14)
      CALL FIG26  (D(17),B(I+2),D(10) )
C
C*********          FIGURE 4.1.5.1-28-B
C
      CALL TLINEX(X128B,X228B,Y28B,4,11,B(I+2),A(178),D(13),
     1            0,2,0,2,Q5128B,3,ROUTID)
      CAPL=2.0
      IF(VT(18).GE.0.30) CAPL=1.20
      D(20)=D(10)*(1.+CAPL*VT(16)+100.*VT(16)**4)*D(13)*(2.*A(3)/SREF)
      GO TO 1030
C
C                  ***** INBOARD CALCUALTIONS *****
C
 1010 D(18)=A(15)*A(129)
C
C*********     FIGURE 4.1.5.1-27 EQUATION
C
      CALL TBFUNX(B(I+2),CEPT,DYDX,4,X27M,X27I,C27,I27,MI,NG,
     1            0,0,Q15127,3,ROUTID)
      D(15)=D(2)**1.0482*10.0**CEPT
C
C*********     IF RLI.LT.RNI  SET RNI=RLI
C
      IF( D(15).LT.D(18) ) D(18)= D(15)
C
C*********     FIGURE 4.1.5.1-26 SUBROUTINE
C
      CALL FIG26 ( D(18),B(I+2),D(11) )
C
C---------          FIGURE 4.1.5.1-28-B (DASHED)
C
      CALL TLINEX(X128BD,X228BD,Y28BD,4,9,B(I+2),A(190),D(23),
     1            0,0,0,0,Q128BD,5,ROUTID)
C
C*********     IF XTI GREATER TAN 0.30, THEN CAPL= 2.0          *
C
 1020 CAPL= 1.20
      IF(VT(18).LT.0.30) CAPL=2.00
      D(21)=D(11)*(1.+CAPL*VT(16)+100.*VT(16)**4)*D(23)*(2.*A(1)/SREF)
C
C                   ***** OUTBOARD CALCULATIONS *****
C
      D(19)=A(17)*A(129)
C
C*********     FIGURE 4.1.5.1-27 EQUATION
C
      D(16)=D(15)
C
C*********   IF RLO .LT. RNO, SET RNO= RLO
C
      IF(D(16).LT.D(19)) D(19)=D(16)
C
C*********     FIGURE 4.1.5.1-26 SUBROUTINE
C
      CALL FIG26( D(19),B(I+2),D(12) )
C
C*********     FIGURE 4.1.5.1-28-B
C
      CALL TLINEX(X128B,X228B,Y28B,4,11,B(I+2),A(184),D(24),
     1            0,2,0,2,Q5128B,3,ROUTID)
C
C*********     IF XTO GREATER THAN 0.30, THEN CAPL= 2.0
C
      CAPL=1.20
      IF(VT(66).LT.0.30) CAPL=2.00
      D(22)=D(12)*(1.+CAPL*VT(65)+100.*VT(65)**4)*D(24)*(2.*A(2)/SREF)
C
C*********     COMPUTE CDO FROM PAGE 3-2
C
      D(20)=D(21)+D(22)
 1030 CONTINUE
      RETURN
      END
