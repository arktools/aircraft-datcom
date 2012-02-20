      SUBROUTINE FLAPCM
C
C***  COMPUTES WING CM DUE TO FLAPS
C
      COMMON /OVERLY/ NLOG,NMACH,II,NALPHA
      COMMON /CONSNT/ PI,DR,UNUSED,RAD
      COMMON /FLOLOG/ FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1                HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,
     2                SUPERS,SUBSON,TRANSN,HYPERS,
     3                SYMFP,ASYFP,TRIMC,TRIM
      COMMON /FLGTCD/ FLC(93)
      COMMON /FLAPIN/ F(69)
      COMMON /SYNTSS/ SYNA(19)
      COMMON /POWR/   PW(104),FLP(189)
      COMMON /SUPDW/  DW(35),TCD(58)
      COMMON /SUPWH/  FCM(282)
      COMMON /WINGD/  A(195)
      COMMON /HTDATA/ AHT(195)
      COMMON /OPTION/ SREF,CBARR
      COMMON /IWING/  PWING, WING(400)
      COMMON /WINGI/  WINGIN(77)
      COMMON /HTI/    HTIN(131)
      REAL MACH
      REAL KK(14)
      DIMENSION CFOC(14),DELTP(14)
      DIMENSION ETAK(14),CK(14)
      DIMENSION ETAG(4), BOC(4),CC(4),GDFULL(14),GDINBD(14),GDOUTB(14)
      DIMENSION VAR(4),LGH(4)
      DIMENSION DELCM(10),SCMD(10)
      DIMENSION SDCL(10),DELTA(10),ALDAG(40),ALPDEL(10),ETA(5),DELTGD(
     114),CL(14),XCP(14),XLE(14),DXCP(140),DCMF(14),XC(14),DELCMF(14),
     2SWEPB(14),CHRD(5)
      DIMENSION X5126A(13),Y5126A(13),F5126A(3)
      DIMENSION CLOALD(14)
      DIMENSION X2126B(11),X1126B(4),Y5126B(44),F5126B(3)
      DIMENSION X1215B(5),X2215B(7),Y1215B(35),F1215B(3)
      DIMENSION GDI(14),GDO(14),ET(14),CPRMEI(10),CPRMEO(10)
      DIMENSION ROUTID(2),CPKK(5),X12136(9),Y12136(9),F12136(3)
      LOGICAL FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1        HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,
     2        SUPERS,SUBSON,TRANSN,HYPERS,
     3        SYMFP,ASYFP,TRIMC,TRIM
      EQUIVALENCE (GDI(1),TCD(1)),(GDO(1),TCD(15)),(GDFULL(1),TCD(29))
      EQUIVALENCE (XCG,SYNA(1)),(CPRMEI(1),F(39)),(CPRMEO(1),F(49))
      EQUIVALENCE (ETA(1),FLP(1)),(ALDAG(1),FLP(150))
      EQUIVALENCE (SDCL(1),F(19)),(DELTA(1),F(1)),(SCMD(1),F(29))
      EQUIVALENCE (DELN4,FLP(60)),(CPKK(1),FLP(28))
      EQUIVALENCE (ETAK(1),FCM(7))
      EQUIVALENCE (CFI,F(12)),(CFO,F(13))
      EQUIVALENCE (SWEEPB,FCM(1)),(BOC(1),FCM(2)),(CAVG,FCM(6)),
     1            (GDINBD(1),FCM(35)),(GDOUTB(1),FCM(49)),
     2             (ALPDEL(1),FCM(63)),(CK(1),FCM(73)),
     3             (DELTGD(1),FCM(87)),(KK(1),FCM(101)),
     4             (XLE(1),FCM(115)),(CFOC(1),FCM(129)),
     5             (DELCM(1),WING(211)),(DXCP(1),FCM(143)),
     6             (CLOALD(1),FCM(21))
      DATA ROUTID /4HFLAP,4HCM  /
      DATA ETAG   /0.924,.707,.383,0.0/
      DATA ET     /0.0,.1423,.2817,.4153,.5407,.6549,.7557,.8412,.9097,
     1             .9595,.9898,1.0,.5,.75/
C
C     FIGURE 6.1.5.1-67A
C
      DATA F5126A/4H6.1.,4H5.1-,4H67A /
      DATA X5126A/
     1 -.02,   0.,     .02,    .04,    .06,    .08,    .10,    .12,
     2 .14,    .16,    .18,    .20,    .22/
      DATA Y5126A/
     1 1.,     1.,     .955,   .845,   .665,   .485,   .33,    .21,
     2 .12,    .06,    .018,   0.,     0./
C
C     FIGURE 6.1.5.1-67B
C
      DATA F5126B/4H6.1.,4H5.1-,4H67B /
      DATA X2126B/
     1 0.,     .1,     .2,     .3,     .4,     .5,     .6,     .7,
     2 .8,     .9,     1./
      DATA X1126B/0.,20.,20.01,60./
      DATA Y5126B/
     1 .745,   .698,   .65,    .6,     .55,    .5,     .45,    .4,
     2 .35,    .3,     .25,
     3 .745,   .698,   .65,    .6,     .55,    .5,     .45,    .4,
     4 .35,    .3,     .25,
     5 .75,    .704,   .675,   .63,    .6,     .593,   .59,    .592,
     6 .598,   .6,     .6,
     7 .75,    .704,   .675,   .63,    .6,     .593,   .59,    .592,
     8 .598,   .6,     .6/
C
C     ----FIGURE 6.1.2.1-35B PLAIN FLAP
C
      DATA X1215B/.1,.2,.25,.3,.5/,X2215B/0.,10.,20.,30.,40.,50.,70./,
     1 Y1215B/0.,-.05,-.105,-.14,-.163,-.175,-.180 , 0.,-.086,-.16,-.20,
     2-.219,-.23,-.24 , 0.,-.11,-.195,-.245,-.27,-.28,-.29 , 0.,-.09,
     3-.165,-.22,-.24,-.26,-.26 , 0.,-.078,-.145,-.2,-.235,-.26,-.28 /,
     4 F1215B/4H6.1.,4H2.1-,4H35B  /
      DATA X12136/0.,.05,.1,.2,.3,.35,.4,.45,.5/
      DATA Y12136/0.,-.00025,-.00065,-.00165,-.0026,-.0031,-.00345,
     1 -.0036,-.00375/
      DATA F12136/4H6.1.,4H2.1-,4H36  /
C
C    -----SET GEOMETRY FOR H.L. AND CONTROL DEVICE ON WING OR HORZ. TAIL
C
      IF(HTPL)GO TO 1000
C
C    -----HERE FOR DEVICE ON WING
C
      CLASEC=WINGIN(II+20)
      TANC4=A(68)
      SWSTR=A(3)
      BSTO2=WINGIN(3)
      TANTE=A(80)
      TANLE=A(62)
      XW=SYNA(2)
      CR=A(10)
      TAPEXP=A(27)
      ARSTAR=A(7)
      BO2=WINGIN(4)
      ALPO=A(134)
      CMO=WINGIN(61)
      GO TO 1010
C
C    -----HERE FOR DEVICE ON HORZ. TAIL
C
 1000 TANC4=AHT(68)
      CLASEC=HTIN(II+20)
      SWSTR=AHT(3)
      BSTO2=HTIN(3)
      BO2=HTIN(4)
      TANTE=AHT(80)
      TANLE=AHT(62)
      XW=SYNA(6)
      CR=AHT(10)
      TAPEXP=AHT(27)
      ARSTAR=AHT(7)
      ALPO=AHT(134)
      CMO=HTIN(61)
 1010 CONTINUE
      MACH=FLC(II+2)
      BETA=SQRT(1.-MACH**2)
      NDELTA=F(16)+0.5
      ARG1=TANC4/BETA
      SWEEPB=ATAN(ARG1)*RAD
      CAVG=SWSTR/(2.*BSTO2)
      IFTYPE=F(17)+.5
C
C    ----CALCULATE CHORDS FOR G/DELTA SUBROUTINE CALL FOR ETA STATIONS
C        OF 0.0,.383,.707,.924
C
      ARG1=(TANTE-TANLE)*BSTO2
      DO 1020 K=1,4
         CC(K)=CR+ETAG(K)*ARG1
 1020 BOC(K)=2.*BETA*BSTO2/CC(K)
      DO 1030 J=1,12
         IF(ETA(1).EQ.ET(J))ETA(1)=ETA(1)+.0001
 1030 IF(ETA(5).EQ.ET(J))ETA(5)=ETA(5)-.0001
      CALL GDELTA(GDFULL,GDI   ,GDO   ,ETA(1),ETA(5),BOC,SWEEPB)
      ET  (13)=ETA(1)
      ET  (14)=ETA(5)
C
C    -----CALCULATE AVERAGE (ALPHA)DELTA OVER FLAP SPAN
C
      IF(SDCL(1).EQ.UNUSED)GO TO 1050
      DO 1040 I=1,NDELTA
 1040 ALPDEL(I)=-SDCL(I)/(DELTA(I)*CLASEC)
      GO TO 1080
 1050 CONTINUE
      NN=0
      DO 1070 I=1,NDELTA
         SUM=0.0
         DO 1060 K=1,4
            NN=NN+1
 1060    SUM=SUM+ALDAG(NN)
 1070 ALPDEL(I)=SUM/4.
 1080 CONTINUE
C
C    -----CALCULATE CHORDS CORRESPONDING TO SPAN STATIONS OF G/D CALCU
C     INSERT INBOARD AND OUTBOARD ETA STATIONS IN PROPER SEQUENCE
C
      KOUT=1
      DO 1140 J=1,11
         ETAK(J)=ET(J)
         GDINBD(J)=GDI(J)
         GDOUTB(J)=GDO(J)
         IF(ETA(1).GT.ET(J).AND.ETA(1).LT.ET(J+1))GO TO 1090
         GO TO 1140
 1090    KINBD=J+1
         ETAK(KINBD)=ETA(1)
         GDINBD(KINBD)=GDI(13)
         GDOUTB(KINBD)=GDO(13)
         JJ=J+2
         N=1
 1100    DO 1110 K=JJ,14
            ETAK(K)=ET(K-N)
            GDINBD(K)=GDI(K-N)
 1110    GDOUTB(K)=GDO(K-N)
         IF(KOUT.EQ.2)GO TO 1150
         K=J+1
         ET(K)=ETA(1)
         DO 1130 I=K,11
            IF(ETA(5).GT.ET(I).AND.ETA(5).LT.ET(I+1))GO TO 1120
            GO TO 1130
 1120       KOUTBD=I+2
            ETAK(KOUTBD)=ETA(5)
            KOUT=2
            GDINBD(KOUTBD)=GDI(14)
            GDOUTB(KOUTBD)=GDO(14)
            JJ=I+3
            N=2
            GO TO 1100
 1130    CONTINUE
 1140 CONTINUE
 1150 CONTINUE
      ARG=(CFI-CFO)/(4.*DELN4)
      ARG7=BSTO2*TANLE
      DO 1160 K=1,14
         CFOC(K)=0.0
         CK(K)=CR+ETAK(K)*ARG1
         IF(K.GE.KINBD.AND.K.LE.KOUTBD)
     1      CFOC(K)=(CFI-ARG*(ETAK(K)-ETA(1)))/CK(K)
         XLE(K)=XW+(BO2-BSTO2)*TANLE+ETAK(K)*ARG7
         DELTGD(K)=GDOUTB(K)-GDINBD(K)
 1160 CONTINUE
      ARG1=BSTO2*TANLE
      ARG2=(1.-TAPEXP)/(1.+TAPEXP)
      KOUNT=1
      INDEX=KINBD
      NN=0
      DO 1400 I=1,NDELTA
         ARGZ=ABS(DELTA(I))
         K=KINBD
         CL(K)=-4.*BSTO2*DELTGD(K)*ALPDEL(I)*DELTA(I)/(CK(K)*RAD)
         K=KOUTBD
         CL(K)=-4.*BSTO2*DELTGD(K)*ALPDEL(I)*DELTA(I)/(CK(K)*RAD)
         DO 1390 K=1,13
            L=K
            CL(K)=-4.*BSTO2*DELTGD(K)*ALPDEL(I)*DELTA(I)/(CK(K)*RAD)
            IF(I.EQ.1)CLOALD(K)=-4.*BSTO2*DELTGD(K)/(CK(K)*RAD)
            IF(CL(K).EQ.0.0)GO TO 1370
C
C    ---- CHECK IF ETA IS OUTSIDE FLAP REGION OF INFLUENCE ----
C
            IF (ETAK(K).LE.(ETA(1)-0.2).OR.ETAK(K).GT.(ETA(5)+0.2))
     1        GO TO 1170
C
C     ---- CALCULATE DELTA CMF2 ----
C
            ARG=CFOC(KINBD)
 1170       IF (ETAK(K).LT.ETA(1).OR.ETAK(K).GT.ETA(5)) GO TO 1180
            KOUNT=3
            ARG=CFOC(K)
 1180       IF (ETAK(K).LE.ETA(5)) GO TO 1190
            KOUNT=2
            ARG=CFOC(KOUTBD)
 1190       CONTINUE
C
C    -----IF PLAIN,SPLIT,OR L.E. FLAPS CALL FIG. 6.1.5.1-67B.
C    -----FOR ALL SLOTTED DEVICES,SET XCPBI=0.54
C
            IF(IFTYPE.NE.1.AND.IFTYPE.NE.5)GO TO 1200
C
C       FIGURE 6.1.5.1-67B  (XCP)B
C
            CALL TLINEX(X1126B,X2126B,Y5126B,4,11,ARGZ    ,ARG,XCPBI,
     1                  0,0,0,0,F5126B,3,ROUTID)
            GO TO 1210
 1200       XCPBI=0.54
 1210       ARG3=4.0*(XCPBI-0.25)*ARG2/ARSTAR
            ARG4=TANC4-ARG3
            SWEPBI=ATAN(ARG4)*RAD
            ARG4=COS(SWEPBI/RAD)
            DELTPI=ATAN(TAN(ARGZ    /RAD)/ARG4)*RAD
            IF(KOUNT.EQ.3)GO TO 1240
            IF(KOUNT.EQ.2)INDEX=KOUTBD
 1220       IF(KK(K) .NE. UNUSED)GO TO 1230
C
C              FIGURE 6.1.5.1-67A (K)K
C
            VAR(1)=ABS(ETAK(K)-ETAK(INDEX))
            LGH(1)=13
            CALL INTERX(1,X5126A,VAR,LGH,Y5126A,KK(K),13,13,
     1                  0,0,0,0,0,0,0,0,F5126A,3,ROUTID)
 1230       CONTINUE
C
C     ---- ETA OUTSIDE FLAP REGION, SET KP=2 ----
C
            SWEPB(K)=SWEPBI
            DELTP(K)=DELTPI
            IF(KOUNT.EQ.2)L=KOUTBD
            COSSB2=COS(SWEPB(K)/RAD)**2
            KP=2
            GO TO 1250
C
C     ---- ETA INSIDE FLAP REGION, SET KP=1 ----
C
 1240       DELTP(K)=DELTPI
            SWEPB(K)=SWEPBI
            COSSB2=COS(SWEPB(K)/RAD)**2
            KP=1
 1250       CONTINUE
            IF (SCMD(1).EQ.UNUSED) GO TO 1280
C
C     ---- COMPUTE SECTION XCP IF DELTA CM IS INPUT ----
C
            DELCMF(K)=SCMD(I)
            IF (KP.EQ.1) GO TO 1260
            XC(K)=0.25+ABS(KK(K)*DELCMF(K)/CL(INDEX)*COSSB2)
            XCP(K)=XLE(K)+XC(K)*CK(K)
            GO TO 1270
 1260       XC(K)=0.25+ABS(DELCMF(K)*COSSB2/CL(K))
            XCP(K)=XLE(K)+XC(K)*CK(K)
C
 1270       GO TO 1370
C
C     ---- SECTION XCP FOR PLAIN FLAPS ----
C
 1280       IF (IFTYPE.GT.1) GO TO 1310
C
C     ---- SECTION XCP FOR PLAIN FLAPS ----
C
C              FIGURE 6.1.2.1-35B PLAIN FLAP
C
            CALL TLINEX(X1215B,X2215B,Y1215B,5,7,CFOC(L),DELTP(K),
     1                 DELCMF(K),0,0,0,0,F1215B,3,ROUTID)
            IF (KP.EQ.1) GO TO 1290
            XC(K)=0.25+ABS(KK(K)*DELCMF(K)/CL(INDEX)*COSSB2)
            XCP(K)=XLE(K)+XC(K)*CK(K)
            GO TO 1300
 1290       XC(K)=0.25+ABS(DELCMF(K)*COSSB2/CL(K))
            XCP(K)=XLE(K)+XC(K)*CK(K)
 1300       GO TO 1370
 1310       CONTINUE
C
C     ---- SECTION XCP FOR SLOTTED,SPLIT,AND FOWLER FLAPS ----
C
            XREFOC= .25
            XCPOCP= .44
            IF(IFTYPE .EQ. 5) XCPOCP=0.5-0.25*CFOC(L)
            CPOC=-((CPRMEO(I)-CPRMEI(I))/(ETA(1)-ETA(5))*(ETAK(K)-
     1            ETA(5))- CPRMEO(I))/CK(K)
            IF(ETAK(K) .LT. ETA(1) .OR. ETAK(K) .GT. ETA(5)) CPOC=1.0
            IF(IFTYPE .GE. 5) GO TO 1320
            IND=4*(I-1)
            SCL=(FLP(110+IND)+FLP(111+IND)+FLP(112+IND)+FLP(113+IND))/4.
            DELCMF(K)=SCL*(XREFOC-XCPOCP*CPOC)
            GO TO 1330
 1320       CONTINUE
C
C     ---- SECTION XCP FOR LE FLAPS,SLATS AND KRUEGER FLAPS ----
C
            IF(IFTYPE.EQ.6) CPOC=1.0
C
C           FIGURE 6.1.2.1-36  CM DELTA LE
C
            VAR(1)=CFOC(L)/CPOC
            LGH(1)=9
            CALL INTERX(1,X12136,VAR,LGH,Y12136,CMDLEP,9,9,
     1                  0,0,0,0,0,0,0,0,F12136,3,ROUTID)
            IND=4*(I-1)
            SCL=(FLP(110+IND)+FLP(111+IND)+FLP(112+IND)+FLP(113+IND))/4.
            DELCMF(K)=CMDLEP*CPOC**2*DELTA(I)+(XREFOC+CPOC-1.)*SCL
     1                +.75*CLASEC*ALPO*CPOC*(CPOC-1.)+(CPOC**2-1.)*CMO
 1330       IF (KP.EQ.1) GO TO 1340
            XC(K)=0.25-(KK(K)*DELCMF(K)/CL(INDEX)*COSSB2)
            XCP(K)=XLE(K)+XC(K)*CK(K)
            GO TO 1350
 1340       XC(K)=0.25-(DELCMF(K)*COSSB2/CL(K))
            XCP(K)=XLE(K)+XC(K)*CK(K)
 1350       GO TO 1370
C
C     ---- CALCULATE DELTA CMF1 ----
C
 1360       XCP(K)=XLE(K)+0.25*CK(K)
 1370       NN=NN+1
            IF (CL(K).EQ.0.0) GO TO 1380
            DXCP(NN)=(XCG-XCP(K))/CBARR
 1380       DCMF(K)=CL(K)*DXCP(NN)*CK(K)*SWSTR/(CAVG*SREF)
 1390    CONTINUE
         DCMF(14)=0.0
         NN=NN+1
         DXCP(NN)=DXCP(NN-1)
         CLOALD(14)=0.0
C
C    ----INTEGRATE DCMF OVER FLAP SPAN
C
         CALL TRAPZ(DCMF,14,ETAK,DELCM(I),1)
 1400 CONTINUE
      RETURN
      END
