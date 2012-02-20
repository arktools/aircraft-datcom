      SUBROUTINE HBTRAN(I)
C
C*** COMPUTES (XAC/CBARR)B(H) FOR MACH=1.40,TRANSONIC H.T.-BODY CLALPHA
C
      DIMENSION ROUTID(2),Q1211A(3),Q1211B(3),Q31210(3),Q2137A(3),
     1          Q2137B(3)
      REAL MACH,NXX,KWB,KBW,IVBW  ,KKBW,KKWB
      LOGICAL       FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1              HEAD,PRPOWR,JEQPOW,LOASRT,TVTPAN,
     2              SUPERS,SUBSON,TRANSN,HYPERS
      DIMENSION CDW(20),CDB(20),WTYPE(4),IVBW(20),GAMMA(20)
      DIMENSION T4337A(11),D4337A(24),T4337B(12),D4337B(20)
      DIMENSION T4311A(38),D4311A(285),DUMYA(150),DUMYB(135)
      DIMENSION TFIG10(11),DKWB10(11),DKBW10(11)
      DIMENSION T4311B(24),D4311B(135)
      DIMENSION LGH(4),VAR(4),CD(20),CN(20),CA(20),CL(20),CLB(20),
     1          CLW(20),ALPHAB(20)
      COMMON /FLOLOG/ FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1                HEAD,PRPOWR,JEQPOW,LOASRT,TVTPAN,
     2                SUPERS,SUBSON,TRANSN,HYPERS
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD
      COMMON /OVERLY/ NLOG,NMACH,M,NALPHA
      COMMON /OPTION/ SREF,CBARR,ROUGFC,BLREF
      COMMON /FLGTCD/ FLC(93)
      COMMON /SYNTSS/ XCG,AA(4),XW
      COMMON /BODYI/  NXX,XCOOR(20)
      COMMON /HTI/    WINGIN(154)
      COMMON /HTDATA/ A(195)
      COMMON /BDATA/  BD(762)
      COMMON /SUPBOD/ SBD(227)
      COMMON /SUPWBB/  SSS(61), SWB(61)
      COMMON /SBETA/  STB(243), TRA(108)
      COMMON /IBODY/  PB, BODY(400)
      COMMON /IHT/    PW, WING(380)
      COMMON /IBH/    PBW, BW(380)
      EQUIVALENCE (D4311A(1),DUMYA(1)),(D4311A(151),DUMYB(1))
      EQUIVALENCE (CA(1),BW(81)),(CR,WINGIN(6)),(CDW(1),WING(1)),
     1            (KKWB,SWB(2)),(SPANS,WINGIN(3)),(ARSTAR,A(7)),
     2            (CD(1),BW(1)),(DD,SWB(5)),(CLB(1),BODY(21)),
     3            (SPAN,WINGIN(4)),(XACBW,SWB(8))
      EQUIVALENCE (CL(1),BW(21)),(KBW,SWB(11)),(TANL,A(62)),
     1            (IVBW(1),SWB(12)),(RKBW,SWB(32)),(CRSTAR,A(10)),
     2            (TAPEXP,A(27)),(CN(1),BW(61)),(CDB(1),BODY(1)),
     3            (CLW(1),WING(21)),(KWB,SWB(35)),
     4            (DN,SBD(4)),(D1,SBD(5)),(KKBW,SWB(37))
      EQUIVALENCE (XACA,SWB(39)),(GAMMA(1),SWB(40)),
     1            (TRINO,SWB(60)),(ALPHAB(1),BD(255))
      EQUIVALENCE (CLAB,BODY(101)),(CLAW,WING(101)),(CLA,BW(101)),
     1            (CLABW,TRA(72)),(CLAWB,TRA(71))
      DATA Q1211A/4H4.3.,4H1.2-,4H11A /,Q2137A/4H4.3.,4H2.2-,4H37A /,
     1     Q1211B/4H4.3.,4H1.2-,4H11B /,Q2137B/4H4.3.,4H2.2-,4H37B /,
     2     Q31210/4H4.3.,4H1.2-,4H10  /,ROUTID/4HWBTR,4HAN  /
      DATA WTYPE/4HSTRA,4HDOUB,4HCRAN,4HCURV/
C
C                   FIGURE 4.3.1,2-10 KWB
C
      DATA TFIG10/0.0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0/
      DATA DKWB10/1.0,1.08,1.16,1.26,1.36,1.46,1.56,1.67,1.78,1.89,2.0/
C
C                   FIGURE 4.3.1.2-10 KBW
C
      DATA DKBW10/0.0,.13,.29,.45,.62,.80,1.0,1.22,1.45,1.70,2.0/
C
C                   FIGURE 4.3.1.2-11A  KBW(WITH AFTERBODY)
C
      DATA T4311A/
     1.0,.2,.4,.6,.8,1.,1.2,1.4,1.6,1.8,2.,2.4,2.8,3.2,4.,  4*0.,
     20.,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.,1.2,1.6,2.,3.,4.,8.,10.,999999./
      DATA DUMYA/15*0.,.75,.58,.47,.38,.33,.32,.3,.3,.29,
     1 .28, .27, .25, .23, .21, .19,
     2 1.45, 1.2, 1., .87, .78, .75, .7, .68, .64, .6, .58, .55,
     3    .5,.48,.4,
     4 1.95, 1.7, 1.48, 1.33, 1.2, 1.12, 1.07, 1., .97, .92,
     5    .88,.82,.75,.72,.64,
     6 2.5, 2.2, 1.93, 1.74, 1.58, 1.5, 1.42, 1.38, 1.3, 1.25,
     7    1.2,1.1,1.,.93,.8,
     8 2.95, 2.6, 2.25, 2.05, 1.89, 1.8, 1.7, 1.65, 1.57, 1.5,
     9    1.48,1.36,1.27,1.2,1.03,
     A 3.3,  2.92, 2.6, 2.35, 2.18, 2.04, 1.95, 1.86, 1.8,
     B    1.73,1.7,1.59,1.49,1.4,1.21,
     C 3.7, 3.32, 2.99, 2.7, 2.5, 2.33, 2.23, 2.12, 2.03,
     D    1.97,1.9,1.8,1.7,1.6,1.42,
     E 4.1, 3.64, 3.25, 2.97, 2.77, 2.6, 2.45, 2.32, 2.23,
     F    2.15,2.1,1.95,1.85,1.76,1.62,
     G 4.3, 3.83, 3.49, 3.2, 2.99, 2.8, 2.65, 2.5, 2.42, 2.33,
     H    2.29,2.14,2.,1.89,1.74/
      DATA DUMYB/4.6,4.08,3.69,3.35,3.14,2.95,2.8,
     1 2.69, 2.6, 2.49, 2.4, 2.25, 2.16, 2.02, 1.85,
     2 5., 4.37, 3.98, 3.64, 3.39, 3.19, 3.03, 2.89, 2.8,
     3    2.7, 2.61, 2.43, 2.33, 2.21, 2.,
     4 5.45, 4.75, 4.29, 3.95, 3.7, 3.5, 3.34, 3.2, 3.1, 3.,
     5    2.91,2.75,2.6,2.48,2.28,
     6 5.9, 5.1, 4.63, 4.25, 4., 3.8, 3.65, 3.5, 3.33, 3.23,
     7    3.14,2.95,2.8,2.67,2.4,
     8 6.2, 5.5, 5., 4.65, 4.35, 4.15, 3.98, 3.8, 3.67,
     9    3.53,3.4,3.24,3.1,2.92,2.68,
     A 6.6, 5.75, 5.25, 4.9, 4.6, 4.4, 4.22, 4.05, 3.91,
     B    3.78,3.68,3.45,3.26,3.1,2.83,
     C 7.05, 6.15, 5.6, 5.2, 4.95, 4.7, 4.49, 4.3, 4.13,
     D    4.,3.85,3.68,3.5,3.34,3.1,
     E 7.4, 6.35, 5.8, 5.4, 5.12, 4.85, 4.67, 4.49, 4.33,
     F    4.2,4.07,3.85,3.65,3.5,3.24,
     G 8., 6.73, 6.2, 5.8, 5.5, 5.25, 5., 4.82, 4.67,
     H    4.5,4.35,4.12,3.9,3.8,3.59/
C
C                   FIGURE 4.3.1.2-11B  KBW(NO AFTERBODY)
C
      DATA T4311B/0.,.2,.4,.6,.8,1.,1.2,1.4,1.6,1.8,2.,2.4,2.8,3.2,4.,
     1 0.,.2,.4,.6,.8,1.,2.,4.,999999./
      DATA D4311B/15*0.,1.2,1.,.75,.58,.45,.37,.3,.28,.24,.2,.18,
     1    .15,.13,.11,.1,
     2 2.4, 1.85, 1.42, 1.1, .92, .77, .66, .6, .51, .48, .41,
     3    .31,.26,.22,.22,
     4 3.5, 2.6, 2., 1.58, 1.28, 1.07, .91, .78, .7, .6, .52,
     5    .42,.39,.33,.3,
     6 4.3, 3.1, 2.35, 1.8, 1.45, 1.2, 1.04, .92, .83, .75,
     7    .68,.55,.48,.4,.38,
     8 5., 3.65, 2.78, 2.22, 1.82, 1.5, 1.3, 1.15, 1., .92,
     9    .82,.69,.58,.5,.42,
     A 5.75, 4.55, 3.66, 2.9, 2.3, 1.92, 1.62, 1.42, 1.25, 1.1,
     B    .97,.82,.7,.63,.52,
     C 6.7, 5.25, 4.18, 3.32, 2.63, 2.2, 1.88, 1.62, 1.45,
     D    1.28,1.15,1.,.85,.75,.62,
     E 7.6, 6.2, 4.91, 3.95, 3.18, 2.6, 2.22, 1.91, 1.7, 1.5,
     F    1.35,1.12,.97,.85,.7/
C
C                   FIGURE 4.3.2.2-37A
C
      DATA T4337A/0.0,.4,.8,1.2,1.6,2.,2.4,2.8,
     1           .1,1.0,999999./
      DATA D4337A/
     1.5,.72,.900,1.08,1.24,1.39,1.53,1.68,
     2.5,.72,.910,1.09,1.25,1.41,1.57,1.72,
     3.5,.73,.920,1.11,1.27,1.43,1.59,1.74/
C
C                   FIGURE 4.3.2.2-37B
C
      DATA T4337B/0.0,.1,.2,.3,.4,.5,.6,.8,1.0,2.8,
     1            0.2,999999./
      DATA D4337B/
     10.5,.56,.595,.62,.64,.65,.66,.669,.669,.671,
     20.5,.54,.578,.60,.62,.638,.649,.66,.669,.671/
C
      NX=NXX+.5
      DCYL=(DN+D1)/2.
      MACH=FLC(I+2)
      RLB=XCOOR(NX)
      DD=2.0*(SPAN-SPANS)
      TANLE=TANL
      IF(TANLE.EQ.0.0)TANLE=.00001
      IF(MACH.EQ.1.)BETA=.0000001
      IF(MACH .EQ. 1.) GO TO 1030
      IF(MACH.GT.1.)BETA=SQRT(MACH**2-1.)
      IF(MACH.LT.1.)BETA=SQRT(1.-MACH**2)
C
C   ***SUPERSONIC WING-BODY LIFT CURVE SLOPE,BODY IN PRESENCE OF WING***
C                         NON-TRIANGULAR WINGS
C
      IF(TAPEXP.EQ.0.0)GO TO 1050
      ARG1=BETA*ARSTAR*(1.0+TAPEXP)
      ARG2=1.+TANLE/BETA
      TRINO=ARG1*ARG2
      IF(TRINO.LE.4.)GO TO 1030
 1000 LGH(1)=15
      ARG=(XW+CR)/RLB
      VAR(1)=BETA* DD/CRSTAR
      VAR(2)=BETA/TANLE
      IF(ARG.GT.1.)GO TO 1010
      LGH(2)=19
C
C                   FIGURE 4.3.1.2-11A
C
      CALL INTERX(2,T4311A,VAR,LGH,D4311A,RKBW,19,285,
     1            2,0,0,0,1,0,0,0,Q1211A,3,ROUTID)
      GO TO 1020
 1010 LGH(2)=9
C
C                   FIGURE 4.3.1.2-11B
C
      CALL INTERX(2,T4311B,VAR,LGH,D4311B,RKBW,15,135,
     1            2,0,0,0,1,0,0,0,Q1211B,3,ROUTID)
 1020 KBW=RKBW/(RAD*BETA*(SREF/A(3))*CLAW*(TAPEXP+1.)*(2.*SPAN/DD-1.))
      GO TO 1040
 1030 CONTINUE
      LGH(1)=11
      VAR(1)=DD/(2.*SPAN)
C
C                   FIGURE 4.3.1.2-10 KBW
C
      CALL INTERX(1,TFIG10,VAR,LGH,DKBW10,KBW,11,11,
     1            0,0,0,0,0,0,0,0,Q31210,3,ROUTID)
 1040 CONTINUE
      GO TO 1060
C
C  ***SUPERSONIC WING-BODY LIFT CURVE SLOPE,BODY IN PRESENCE OF WING***
C                         TRIANGULAR WING
 1050 CONTINUE
      ARG=BETA*ARSTAR
      IF(ARG.GT.1.)GO TO 1000
      GO TO 1030
 1060 CONTINUE
C
C  ***SUPERSONIC WING-BODY LIFT CURVE SLOPE
C
      ALBO=BD(81)
      IF(BD(81).EQ.UNUSED)ALBO=0.0
      DO 1070 J=1,NALPHA
 1070 ALPHAB(J)=FLC(J+22)+ALBO
      VAR(1)=(SPAN-SPANS)/SPAN
      LGH(1)=11
C
C                   FIGURE 4.3.1.2-10 KWB
C
      CALL INTERX(1,TFIG10,VAR,LGH,DKWB10,KWB,11,11,
     1            0,0,0,0,0,0,0,0,Q31210,3,ROUTID)
      CLAWB=CLAW*KWB
      CLABW=CLAW*KBW
      CLA=CLABW+CLAWB+CLAB
C
C  ***SUPERSONIC WING-LIFT CARRYOVER ON BODY***
C
      BETA=.98
      VAR(1)=BETA*DD/CRSTAR
      VAR(2)=BETA/TANLE
      ARG=(XW+CR)/RLB
      IF(ARG.GT.1.)GO TO 1080
C
C                   FIGURE 4.3.2.2-37A(XAC)B(W)
C
      LGH(1)=8
      LGH(2)=3
      CALL INTERX(2,T4337A,VAR,LGH,D4337A,XACA,8,24,
     1            0,0,0,0,1,0,0,0,Q2137A,3,ROUTID)
      GO TO 1090
C
C                   FIGURE 4.3.2.2-37B(XAC)B(W)
C
 1080 LGH(1)=10
      LGH(2)=2
      CALL INTERX(2,T4337B,VAR,LGH,D4337B,XACA,10,20,
     1            0,0,0,0,0,0,0,0,Q2137B,3,ROUTID)
 1090 XACBW=XACA*CRSTAR/CBARR
      RETURN
      END
