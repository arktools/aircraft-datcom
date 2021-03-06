      SUBROUTINE SUPDRG
C
C***  CALCULATES SUPERSONIC WING DRAG
C
      COMMON /OVERLY/ NLOG,NMACH,N,NALPHA,IG
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD
      COMMON /FLGTCD/ FLC(93)
      COMMON /OPTION/ SR,CBARR,RUFF,BLREF
      COMMON /WINGD/  A(195),B(48)
      COMMON /WINGI/  WINGIN(77)
      COMMON /SUPWH/  SLG(141)
      DIMENSION VAB(4),LGB(4)
      REAL MACH,KSHARP
      REAL LHS,LERBW,LERI,LERO
      DIMENSION ROUTID(2),Q15258(3)
      DIMENSION WTYPE(4)
      DIMENSION T15258(5),DS5258(5),DR5258(5)
      DIMENSION X27M(4),X27I(4),C27(6)
      EQUIVALENCE (RNFS,A(129)),(RNN,SLG(90)),(RLCOFF,SLG(89)),(CF,
     1SLG(88)),(CDF,SLG(87)),(SRSTAR,A(3)),(CBARI,A(15))
     2,(CBARO,A(17)),(RNI,SLG(86)),(RNO,SLG(85)),(CFI,SLG(84)),(CFO,
     3SLG(83)),(SISTAR,A(1)),(SOSTAR,A(2)),(P,SLG(82)),(DRAGC,SLG(81)
     4),(CDO,SLG(80)),(CDW,SLG(79))
      EQUIVALENCE (SBW,SLG(119)),(CR,WINGIN(6))
      EQUIVALENCE  (SIGMA,A(18))
      EQUIVALENCE (LERO,WINGIN(63)),(LERI,WINGIN(62)),(KSHARP,
     1WINGIN(71)),(TCEFF,WINGIN(70)),(CB,WINGIN(5)),
     2(CT,WINGIN(1)),(SPANS,WINGIN(3))
      EQUIVALENCE (BETA,SLG(1)),(BOVERT,SLG(2)),(COSLE,A(61)),
     1(DETACH,SLG(94))
      EQUIVALENCE (TANLEO,A(86)),(TANLEI,A(62)),(COSLEO,A(85)),
     1(ARSTAR,A(7)),(CRSTAR,A(10))
      DATA Q15127 /4HCEPT/, Q15258 /4H4.1.,4H5.2-,4H58  /
      DATA ROUTID /4HSUPD,4HRG  /
      DATA WTYPE  /4HSTRA,4HDOUB,4HCRAN,4HCURV/
      DATA X27M   /0.0,1.0,2.0,3.0/
      DATA X27I   /1.57780,1.67221,1.98509,2.28874/
      DATA I27    /0/
C
C              FIGURE 4.1.5.2-58
C
      DATA T15258/0.0,.28,.40,.50, 1.87/
      DATA DS5258/.54,.54,.559,.60,1.95/
      DATA DR5258/.54,.54,.593,.68,2.0/
C
      IF(TANLEO.EQ.0.0)TANLEO=.00001
      IF(TANLEI.EQ.0.0)TANLEI=.00001
C
C  ***WING SUPERSONIC ZERO-LIFT SKIN FRICTION DRAG,STRAIGHT TAPERED WING
C
      MACH=FLC(N+2)
      BETA=SQRT(MACH**2-1.)
      IF(WINGIN(15).NE.WTYPE(1))GO TO 1020
      CBAR=CBARI
 1000 RNN=CBAR*RNFS
      IF(RUFF.EQ.0.0)GO TO 1010
      ARG=12.*CBAR/RUFF
C
C                   FIGURE 4.1.5.1-27 (EQUATION FOR RLCOFF)
C
      RACH = MACH
      IF(RACH.GT.3.0)RACH=3.0
      CALL TBFUNX(RACH,CEPT,DYDX,4,X27M,X27I,C27,I27,MI,NG,
     1            0,0,Q15127,3,ROUTID)
      RLCOFF=ARG**1.0482*10.0**CEPT
      IF(RLCOFF.LT.RNN)RNN=RLCOFF
 1010 CALL FIG26(RNN,RACH,CF)
      IF(WINGIN(15).NE.WTYPE(1))GO TO 1030
      CDF=CF*SRSTAR/SR*2.
      GO TO 1050
C
C  ***WING SUPERSONIC ZERO-LIFT SKIN FRICTION DRAG,NON-STRAIGHT WING***
C
 1020 CONTINUE
      CBAR=CBARI
      GO TO 1000
 1030 CONTINUE
      IF(CBAR.EQ.CBARO)GO TO 1040
      RNI=RNN
      CFI=CF
      CBAR=CBARO
      GO TO 1000
 1040 RNO=RNN
      CFO=CF
      CDF=(CFI*SISTAR+CFO*SOSTAR)/SR*2.
 1050 CONTINUE
C
C   ***SUPERSONIC WING ZERO-LIFT WAVE DRAG,SHARP L.E.,ALL PLANFORMS***
C
      IF(WINGIN(15).EQ.WTYPE(1))GO TO 1060
      TA=TANLEO
      S=SBW
      CA=COSLEO
      LERBW=LERO*((CR+CT+2.*CB)/4.)
      GO TO 1070
 1060 TA=TANLEI
      S=A(3)
      CA =COSLE
      LERBW=LERI*((CR+CT)/2.)
 1070 CONTINUE
      BOVERT=BETA/TA
      IF(KSHARP.EQ.UNUSED)GO TO 1080
      ARG=KSHARP*TCEFF**2*S  /SR
      CDW=ARG/BETA
      IF(BOVERT.LT.1.)CDW=ARG/TA
      GO TO 1090
 1080 CONTINUE
C
C  ***SUPERSONIC WING ZERO-LIFT WAVE DRAG,ROUND L.E.,ALL PLANFORMS***
C
      ARG1=1.28*MACH**3*CA    **6/(1.+MACH**3*CA    **3)
      ARG2=2.*LERBW*(2.*SPANS)/(SR*CA)
      CDLE=ARG1*ARG2
      ARG=     16.*TCEFF**2*S  /(3.*SR)
      IF(BOVERT.GE.1.)CDW=CDLE+ARG/BETA
      IF(BOVERT.LT.1.)CDW=CDLE+ARG/TA
 1090 CONTINUE
      CDO=CDF+CDW
C
C   ***SUPERSONIC INDUCED DRAG,STRAIGHT TAPERED PLANFORM***
C
      IF(WINGIN(15).NE.WTYPE(1))RETURN
      RLW=CT+SIGMA*CRSTAR
      P=SRSTAR/(RLW*2.*SPANS)
      ARG=(1.+P)/(PI*ARSTAR*P)
      ARG1=BETA*SPANS/RLW
C
C                   FIGURE 4.1.5.2-58 (PI*A*(CDL/CL**2)*(P/(1+P)))
C
      VAB(1)=ARG1
      LGB(1)=5
      IF(KSHARP.EQ.UNUSED) GO TO 1100
      CALL INTERX(1,T15258,VAB,LGB,DS5258,DRAGC,5,5,
     1            0,0,0,0,1,0,0,0,Q15258,3,ROUTID)
      GO TO 1110
 1100 CALL INTERX(1,T15258,VAB,LGB,DR5258,DRAGC,5,5,
     1            0,0,0,0,1,0,0,0,Q15258,3,ROUTID)
 1110 CONTINUE
      IF(TANLEO.EQ.0.00001)TANLEO=0.0
      IF(TANLEI.EQ.0.00001)TANLEI=0.0
      RETURN
      END
