      SUBROUTINE LIFTFP
C
C***  COMPUTES INCREMENTAL WING LIFT DUE TO FLAPS
C
      COMMON /OVERLY/ NLOG,NMACH,M,NALPHA
      COMMON /FLGTCD/ FLC(93)
      COMMON /FLOLOG/ FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1                HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,
     2                SUPERS,SUBSON,TRANSN,HYPERS,
     3                SYMFP,ASYFP,TRIMC,TRIM
      COMMON /CONSNT/ PI,DR,UNUSED,RAD
      COMMON /FLAPIN/ F(116)
      COMMON /POWR/   PW(104),FLP(189)
      COMMON /OPTION/ SREF,CBARR,RUFF,BLREF
      COMMON /WINGI/  WINGIN(77)
      COMMON /SUPWH/  FCM(287)
      COMMON /HTI/    HTIN(131)
      COMMON /WINGD/  A(195)
      COMMON /HTDATA/ AHT(195)
      COMMON /IWING/  PWING, WING(400)
      COMMON /IHT/    PHT, HT(380)
      COMMON /SBETA/  STB(135), TRA(108), TRAH(108)
      REAL KFPRM,MACH
      REAL KSWEEP
      LOGICAL TRANSL
      LOGICAL EXPDCL
      DIMENSION ROUTID(2),DSCLMX(10),ALDAG(40)
      DIMENSION CPI(10),CPO(10),DALPDE(5)
      DIMENSION SDCL(10),ADCAD(10)
      DIMENSION RK2(10),DELCL2(10),DCLMAX(10)
      DIMENSION VAR(4),LGH(4),RKB(5),CF(5),ETA(5),CHRD(5),DKB(4),
     1          CFOC(5),DELTA(10),DELCL(5),CP(5),ALDAVG(4),ADCADS(5),
     2          DCL(10),DCLK(5)
      DIMENSION SWF(4),DELCLA(40),CPOCF(4),CLAD(10),CFACTR(4),CFACT(10)
      DIMENSION CLDTHY(5),CLDOCT(5),ALPHAD(5)
      DIMENSION ITRANS(5)
      DIMENSION X2128A(11),X1128A(3),Y1128A(33),F1128A(3)
      DIMENSION X1125A(8),X2125A(8),Y1125A(64),F1125A(3)
      DIMENSION X1125B(16),X2125B( 6),Y1125B(96),F1125B(3)
      DIMENSION X11126(7),X21126(14),Y11126(98),F11126(3)
      DIMENSION X11419(3),X21419(12),Y61419(36),F61419(3)
      DIMENSION X11127(5),X21127(13),Y11127(65),F11127(3)
      DIMENSION X11150(7),Y11150(7),F11150(3)
      DIMENSION X11418(10),X21418(10),Y61418(100),F61418(3)
      DIMENSION X137AD(17),Y137AD(17),F137AD(3)
      DIMENSION X137B2(2),Y137B2(2),F137B2(3)
      DIMENSION X137B1(13),Y137B1(13),F137B1(3)
      DIMENSION X137AB(12),Y137AB(12),F137AB(3)
      DIMENSION X137AC(12),Y137AC(12),F137AC(3)
      DIMENSION X1138A(12),Y138A1(12),Y138A2(12),Y138A3(12),Y138A4(12),
     1          F138A1(3),F138A2(3),F138A3(3),F138A4(3)
      DIMENSION X1138B(9),Y138B1(9),Y138B2(9),F138B1(3),F138B2(3)
      DIMENSION X1418A(12),Y1418A(12),F1418A(3)
      DIMENSION CF2(5),X61142(11),Y61142(11),F61142(3),
     1          X6143A(14),Y6143A(21),F6143A(3),CF2OC(5),DF2(10),
     2          CAPI(10),CAPO(10),X6143B(14),Y6143B(21),F6143B(3)
      DIMENSION X11147(20),Y11147(40),F11147(3)
      LOGICAL   FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1          HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,
     2          SUPERS,SUBSON,TRANSN,HYPERS,
     3          SYMFP,ASYFP,TRIMC,TRIM
      EQUIVALENCE (CFOC(1),FLP(62)),(CF2I,F(115)),(CF2O,F(116)),
     1            (CAPI(1),F(85)),(CAPO(1),F(95)),(DF2(1),F(105))
      EQUIVALENCE (FTYPE,F(17)),(BOF,F(15)),(BIF,F(14)),(CFI,F(12))
     1,(CFO,F(13)),(RDELTA,F(16)),(SDCL(1),F(19)),(CPI(1),F(39))
     2,(CPO,F(49)),(TANPHE,F(11)),(DELTA(1),F(1))
      EQUIVALENCE (FLP(1),ETA(1)),(CHRD(1),FLP(6)),(CF(1),FLP(11))
     1,(DKB(1),FLP(20)),(SWF(1),FLP(24)),(CP(1),FLP(28))
     2,(CLOCLT,FLP(33)),(CLDOCT(1),FLP(34)),(CLDTHY(1),FLP(39)),
     3(ALDAVG(1),FLP(16)),(DALPDE(1),FLP(54)),(DELCLA(1),FLP(110))
     4,(CFACT(1),FLP(71)),(ADCADS(1),FLP(67)),(DCL(1),WING(201))
     5,(DCLMAB,FLP(102)),(RK2(1),FLP(91)),(DELCL2(1),FLP(44))
     6,(CLAD(1),WING(241))
      EQUIVALENCE (DELN4,FLP(60)),(DELCL(1),FCM(283))
      EQUIVALENCE (ALDAG(1),FLP(150)),(CFOCA,FLP(61))
     1           ,(RK1,FLP(101)),(RK3,FLP(103)),(DSCLMX(1),FLP(81)),
     2 (KSWEEP,FLP(104)),(DCLMAX(1),WING(221)),(ALPHAD(1),FLP(105))
      DATA ROUTID/4HLIFT,4HFP  /
C
C                   FIGURE 4.1.1.2-8A
C
      DATA X2128A
     1  /0., 0.02,  0.04,  0.06,  0.08,  0.10,  0.12,  0.14,  0.16,
     2       0.18,  0.20 /
      DATA X1128A
     1        /6.,     7.,    8./
      DATA Y1128A
     1 / 0.900,  0.878,  0.858,  0.836,  0.815,  0.794,  0.772,  0.750,
     2   0.728,  0.708,  0.685,
     3   0.950,  0.938,  0.924,  0.907,  0.894,  0.878,  0.860,  0.842,
     4   0.822,  0.802,  0.780,
     5   0.966,  0.957,  0.947,  0.936,  0.924,  0.910,  0.896,  0.880,
     6   0.862,  0.842,  0.822 /
      DATA F1128A/4H4.1.,4H1.2-,4H8A  /
C
C                   FIGURE 6.1.1.1-39A
C
      DATA X2125A
     1 / 0.05,  0.10,  0.15,  0.20,  0.25,  0.30,  0.40,  0.50 /
      DATA X1125A
     1 / 0.00,  0.02,  0.04,  0.06,  0.08,  0.10,  0.12,  0.15 /
      DATA Y1125A
     1 / 1.770,  2.500,  3.000,  3.460,  3.820,  4.160,  4.690,  5.140,
     2   1.770,  2.515,  3.030,  3.500,  3.873,  4.220,  4.780,  5.240,
     3   1.770,  2.530,  3.060,  3.540,  3.926,  4.290,  4.870,  5.350,
     4   1.770,  2.545,  3.090,  3.580,  3.979,  4.350,  4.950,  5.460,
     5   1.770,  2.560,  3.120,  3.620,  4.032,  4.400,  5.040,  5.560,
     6   1.770,  2.575,  3.150,  3.660,  4.085,  4.480,  5.120,  5.690,
     7   1.770,  2.590,  3.180,  3.700,  4.138,  4.550,  5.210,  5.790,
     8   1.770,  2.600,  3.220,  3.740,  4.190,  4.620,  5.330,  5.960/
      DATA F1125A/4H6.1.,4H1.1-,4H39A /
C
C                   FIGURE 6.1.1.1-39B
C
      DATA X2125B
     1  / 0.05,  0.10,  0.15,  0.20,  0.25,  0.50 /
      DATA X1125B
     1  / 0.70,  0.72,  0.74,  0.76,  0.78,  0.80,  0.82,  0.84,  0.86,
     2    0.88,  0.90,  0.92,  0.94,  0.96,  0.98,  1.00 /
      DATA Y1125B
     1 /  .356,    .382,    .409,    .431,    .452,    .548,
     2    .399,    .426,    .452,    .477,    .498,    .583,
     3    .442,    .471,    .499,    .523,    .543,    .619,
     4    .485,    .521,    .548,    .569,    .589,    .659,
     5    .530,    .569,    .594,    .613,    .630,    .693,
     6    .578,    .614,    .639,    .657,    .671,    .729,
     7    .619,    .655,    .678,    .692,    .709,    .761,
     8    .659,    .696,    .713,    .733,    .746,    .793,
     9    .700,    .734,    .750,    .765,    .778,    .819,
     A    .742,    .771,    .789,    .800,    .810,    .850,
     B    .784,    .809,    .824,    .838,    .843,    .875,
     C    .826,    .843,    .860,    .865,    .873,    .900,
     D    .865,    .885,    .895,    .900,    .903,    .921,
     E    .910,    .921,    .928,    .931,    .933,    .938,
     F    .951,    .962,    .964,    .966,    .967,    .968,
     G   1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      DATA F1125B/4H6.1.,4H1.1-,4H39B /
C
C                   FIGURE 6.1.1.1-40
C  CTAB4(1,I)  = FLAP DEFLECTION ANGLE  (DEG)
C  CTAB4(2,I)  = FLAP TO A1RFOIL CHORD RATIO
C  KPRIME(I)   = EMPIRICAL CORRECTION FACTOR
C
      DATA X21126
     1 / 0., 10., 12., 14., 16., 18., 20., 23., 27., 30., 35., 40.,
     2  50., 60. /
      DATA X11126
     1 / 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50 /
      DATA Y11126
     1 /  1.000,  1.000,  0.994,  0.989,  0.970,  0.938,  0.900, 0.829,
     2    0.755,  0.722,  0.672,  0.641,  0.596,  0.562,
     3    1.000,  1.000,  0.994,  0.989,  0.970,  0.937,  0.890, 0.809,
     4    0.737,  0.698,  0.650,  0.618,  0.569,  0.531,
     5    1.000,  1.000,  0.994,  0.989,  0.968,  0.936,  0.870, 0.783,
     6    0.710,  0.673,  0.630,  0.595,  0.542,  0.500,
     7    1.000,  1.000,  0.994,  0.989,  0.965,  0.935,  0.850, 0.740,
     8    0.677,  0.644,  0.600,  0.569,  0.518,  0.480,
     9    1.000,  1.000,  0.994,  0.989,  0.963,  0.905,  0.800, 0.700,
     A    0.643,  0.610,  0.570,  0.541,  0.496,  0.461,
     B    1.000,  1.000,  0.993,  0.969,  0.924,  0.860,  0.750, 0.656,
     C    0.606,  0.579,  0.540,  0.513,  0.471,  0.440,
     D    1.000,  1.000,  0.981,  0.943,  0.880,  0.790,  0.695, 0.625,
     E    0.571,  0.542,  0.512,  0.490,  0.450,  0.423/
      DATA F11126/4H6.1.,4H1.1-,4H40  /
C
C     FIGURE 6.1.4.1-15
C     TIKB(1,I) ETA
C     TIKB(2,I) LAMBDA
C     TDKB(I) K(B)
C
      DATA X21419/
     1 0.0,    0.1,    0.2,    0.3,    0.4,    0.5,    0.6,    0.7,
     2 0.8,    0.85,   0.9,    1.0/
      DATA X11419/
     1 0.0,    0.5,    1.0/
      DATA Y61419/
     1 0.0,    0.160,  0.305,  0.440,  0.560,  0.670,  0.772,  0.860,
     2 .930,   .96,    .981,   1.0,
     3 0.0,    0.140,  0.270,  0.400,  0.515,  0.630,  0.735,  0.830,
     4 0.912,  0.947,  0.972,  1.0,
     5 0.,     .125,   .255,   .37,    .49,    .6,     .705,   .8,
     6 0.885,  0.921,  0.955,  1.0/
      DATA F61419/4H6.1.,4H4.1-,4H15  /
C
C     ----FIGURE 6.1.1.1-41
C
      DATA Y11127/-.373,-.373,-.368,-.345,-.32,-.296,-.268,-.23,-.19,
     1-.165,-.151,-.142,-.140 , -.45,-.446,-.43,-.412,-.39,-.364,-.328,
     2-.29,-.24,-.207,-.188,-.172,-.167 , -.512,-.51,-.491,-.48,-.458,
     3-.43,-.398,-.356,-.30,-.247,-.229,-.20,-.190,-.55,-.55,-.53 ,-.52,
     4-.5,-.472,-.44,-.396,-.34,-.28,-.25,-.224,-.21, -.60,-.60,-.585,
     5-.57,-.55,-.521,-.495,-.455,-.4,-.33,-.29,-.25,-.23 /
      DATA X21127/0.,10.,20.,25.,30.,35.,40.,45.,50.,55.,60.,70.,80./
      DATA X11127/.15,.20,.25,.30,.40/,F11127/4H6.1.,4H1.1-,4H41  /
C
C     FIGURE 6.1.4.1-14
C     TIADER(1,I) A(W)
C     TIADER(2,I) (ALFA(DELTA)) CL
C     TDADER(I) (ALFA(DELTA)) CL RATIO
C
      DATA X21418/
     1 0.0,    0.25,   0.50,   1.0,    2.0,    3.0,    4.0,    5.0,
     2 6.0,    8.0/
      DATA X11418/
     1 -1.0,   -.9,    -.8,    -.7,    -.6,    -.5,    -.4,    -.3,
     2 -.2,    -.1/
      DATA Y61418/10*1.,
     1 1.11,   1.075,  1.06,   1.049,  1.03,   1.019,  1.011,  1.01,
     2 1.009,  1.007,
     3 1.2,    1.15,   1.125,  1.095,  1.06,   1.04,   1.03,   1.02,
     4 1.019,  1.015,
     5 1.365,  1.245,  1.20,   1.145,  1.095,  1.065,  1.05,   1.04,
     6 1.032,  1.025,
     7 1.5,    1.35,   1.285,  1.205,  1.132,  1.095,  1.072,  1.06,
     8 1.05,   1.039,
     9 1.7,    1.5,    1.4,    1.29,   1.185,  1.135,  1.10,   1.085,
     A 1.071,  1.055,
     B 2.0,    1.63,   1.53,   1.39,   1.25,   1.185,  1.145,  1.118,
     C 1.1,    1.079,
     D 2.4,    1.90,   1.70,   1.52,   1.35,   1.26,   1.20,   1.165,
     E 1.142,  1.11,
     F 2.73,   2.43,   2.10,   1.73,   1.49,   1.375,  1.30,   1.24,
     G 1.2,    1.16,
     H 3.08,   2.85,   2.55,   2.18,   1.79,   1.58,   1.46,   1.385,
     I 1.325,  1.245/
      DATA F61418/4H6.1.,4H4.1-,4H14  /
C
C     ----FIGURE 6.1.1.3-12AD SPLIT AND PLAIN
C
      DATA X137AD/0.,2.,4.,5.,6.,8.,9.,10.,11.,12.,14.,15.,16.,17.,18.,
     1 19.,20./,F137AD/4H6.1.,4H1.3-,4H12AD/,
     2Y137AD/2*1.,.979,.95,.92,.82,.80,.82,.85,.91,1.09,1.19,1.31,1.43,
     3 1.51,1.57,1.60/
C
C     ----FIGURE 6.1.1.3-12B2 2-SLOT AND FOWLER
C
      DATA X137B2/0.,30./,Y137B2/0.,1.2/,F137B2/4H6.1.,4H1.3-,4H12B2/
C
C     ----FIGURE 6.1.1.3-12B1 SPLIT,PLAIN AND 1-SLOT
C
      DATA X137B1/0.,2.,4.,6.,8.,10.,12.,14.,16.,20.,24.,28.,30./,
     1Y137B1/0.,.2,.34,.47,.57,.65,.72,.78,.83,.92,.99,1.04,1.06/   ,
     2F137B1/4H6.1.,4H1.3-,4H12B1/
C
C     ----FIGURE 6.1.1.3-12AB AVERAGE 2-SLOT,FOWLER
C
      DATA X137AB/0.,2.,5.,7.,9.,11.,13.,15.,16.,17.,18.,19./,
     1Y137AB/1.0,1.0,1.04,1.09,1.17,1.29,1.45,1.64,1.73,1.77,1.80,1.82/,
     2F137AB/4H6.1.,4H1.3-,4H12AB/
C
C     ----FIGURE 6.1.1.3-12AC NACA 2-SLOT,NACA 1-SLOT
C
      DATA X137AC/0.,5.,6.,8.,10.,12.,14.,16.,17.,18.,19.,20./,
     1Y137AC/1.,1.,1.02,1.08,1.17,1.30,1.47,1.67,1.71,1.73,1.715,1.68/,
     2F137AC/4H16.1,4H.1.3,4H-12A/
C
C     ----FIGURE 6.1.1.3-13A
C     ----A1=FOWLER , A2=1-SLOT , A3=2-SLOT , A4=SPLIT AND PLAIN
C
      DATA X1138A /0.,5.,10.,15.,20.,25.,30.,35.,40.,45.,50.,60./,
     1 Y138A1/.4,.5,.61,.71,.79,.87,.94,.98,4*1.0/,
     2 Y138A2/.18,.33,.47,.59,.70,.79,.87,.93,.97,3*1.0/,
     3 Y138A3/.18,.32,.44,.56,.66,.76,.84,.90,.95,.99,2*1.0/,
     4 Y138A4/0.,.17,.33,.46,.57,.67,.76,.83,.87,.92,.95,1.0/,
     5 F138A1/4H6.1.,4H1.3-,4H13A1 /,
     6 F138A2/4H6.1.,4H1.3-,4H13A2 /,
     7 F138A3/4H6.1.,4H1.3-,4H13A3 /,
     8 F138A4/4H6.1.,4H1.3-,4H13A4 /
C
C     ----FIGURE 6.1.1.3-13B
C     ----B1=1-SLOT , 4-BAR , FOWLER , CIRCULAR ARC
C     ----B2= 2-SLOT, 4-BAR MOTION
C
      DATA X1138B/0.,.2,.3,.4,.45,.55,.6,.8,1.0/,F138B1/4H6.1.,4H1.3-,
     1 4H13B1 /,F138B2/4H6.1.,4H1.3-,4H13B2 /,
     2 Y138B1/0.,.26,.39,.50,.57,.66,.70,.87,1.0/,
     3 Y138B2/0.,.11,.23,.40,.52,.66,.70,.87,1.0/
C
C     ----FIGURE 6.1.4.1-14A
C
      DATA X1418A/0.,.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0/,
     1 Y1418A/0.,-.26,-.40,-.55,-.66,-.75,-.82,-.88,-.93,-.97,-.99,-1./,
     2 F1418A /4H6.1.,4H4.1-,4H13A  /
      DATA X61142/0.,.05,.1,.15,.2,.25,.3,.35,.4,.45,.5/
      DATA Y61142/0.,.03,.042,.052,.06,.068,.072,.078,.082,.086,.09/
      DATA X6143A/15.,20.,30.,40.,50.,60.,70.,.1,.25,.4,4*0./
      DATA Y6143A/2*.66,.645,.505,.5,.4,.35,2*.73,.7,.645,.55,.43,
     1 .38,2*.78,.745,.68,.585,.475,.4/
      DATA X6143B/0.,15.,20.,30.,40.,60.,90.,0.,10.,20.,4*0./
      DATA Y6143B/9*1.,.945,.89,.885,.875,.865,2*1.,.9,.79,.75,
     1 .725,.72/
      DATA F61142/4H6.1.,4H1.1-,4H42  /
      DATA F6143A/4H6.1.,4H1.1-,4H43A /
      DATA F6143B/4H6.1.,4H1.1-,4H43B /
      DATA X11147/5.,10.,15.,20.,30.,40.,50.,60.,70.,75.,
     1 .1,.2,.3,.4,6*0./
      DATA Y11147/-.36,-.295,-.24,-.22,-.195,-.18,-.17,-.155,-.14,
     1 -.13,-.435,-.37,-.32,-.29,-.26,-.245,-.23,-.215,-.195,-.185,
     2 -.57,-.48,-.415,-.375,-.33,-.305,-.28,-.27,-.255,-.25,-.6,
     3 -.545,-.48,-.44,-.378,-.35,-.325,-.3,-.275,-.26/
      DATA F11147/4H6.1.,4H1.1-,4H47  /
      DATA X11150/0.,.05,.1,.2,.3,.4,.5/
      DATA Y11150/0.,-.0005,-.0016,-.0044,-.0082,-.0108,-.02/
      DATA F11150/4H6.1.,4H1.1-,4H50  /
      DATA ITRANS/2,3,4,7,8/
C
      IF(HTPL)GO TO 1000
C
C    -----HERE FOR H.L. OR  CONTROL DEVICE ON WING
C
      CBAREX=A(16)
      BO2=WINGIN(3)
      TANTE=A(80)
      TANLE=A(62)
      CR=WINGIN(6)
      TAPRI=A(25)
      BSTRO2=WINGIN(2)
      TANTEO=A(104)
      TANLEO=A(86)
      CB=WINGIN(5)
      TAPRO=A(28)
      SR=SREF
      AW=A(120)
      BTHEO=WINGIN(4)
      TOVC=WINGIN(16)
      TOVCO=WINGIN(66)
      COSC4=A(67)
      CLASEC=WINGIN(M+20)
      CLAW=WING(101)
      IF(TRANSN)CLAW=TRA(70)
      IF(TRANSN)CLASEC=WINGIN(69)/.8
      GO TO 1010
 1000 CBAREX=AHT(16)
      BO2=HTIN(3)
      TANTE=AHT(80)
      TANLE=AHT(62)
      CR=HTIN(6)
      TAPRI=AHT(25)
      BSTRO2=HTIN(2)
      TANTEO=AHT(104)
      TANLEO=AHT(86)
      CB=HTIN(5)
      TAPRO=AHT(28)
      SR=SREF
      AW=AHT(120)
      BTHEO=HTIN(4)
      TOVC=HTIN(16)
      TOVCO=HTIN(66)
      COSC4=AHT(67)
      CLASEC=HTIN(M+20)
      CLAW=HT(101)
      IF(TRANSN)CLAW=TRAH(70)
      IF(TRANSN)CLASEC=HTIN(69)/.8
 1010 CONTINUE
C
C*****CHECK FOR TRANSALATING DEVICE,IF SO CALCULATE SWF
C
      TRANSL=.FALSE.
      IFTYPE=FTYPE+0.5
      NDELTA=RDELTA+0.5
      RF=FLC(M+42)
      MACH=FLC(M+2)
      IF(TRANSN)MACH=.6
C
C-----IF DELTA EQUALS ZERO,SET TO 0.01 DEG
C
      DO 1020 J=1,NDELTA
 1020 IF(DELTA(J).EQ.0.0)DELTA(J)=0.01
      DO 1030 K=1,5
 1030 IF(IFTYPE.EQ.ITRANS(K))TRANSL=.TRUE.
C
C*****GEOMETRIC PARAMETERS PER FLAP SEGMENT
C
      DELN4=0.25*(BOF-BIF)/BTHEO
      ETA(1)=BIF/BTHEO
      VAR(1)=ETA(1)
      CF(1)=CFI
      ARG1=(CFI-CFO)/(4.*DELN4)
      ARG2=(TANTE-TANLE)*BTHEO
      ARG3=(BOF-BIF)*CR /4.0
      ARG4=CR
      ARG5=TAPRI
      TC=TOVC
      IF(BIF.LT.(BTHEO-BSTRO2))GO TO 1040
      ARG2=(TANTEO-TANLEO)*BO2
      ARG3=(BOF-BIF)*CB /4.0
      ARG4=CB
      ARG5=TAPRO
      TC=TOVCO
 1040 CONTINUE
C
C              FIGURE 6.1.4.1-15 (KB)
C
      CALL TLINEX(X11419,X21419,Y61419,3,12,ARG5,ETA(1),RKB(1),
     1            0,0,0,0,F61419,3,ROUTID)
      CHRD(1)=ARG4+ETA(1)*ARG2
C
C              FIGURE 6.1.4.1-14 (ALPHA)DELTA
C
      CFOC(1)=CF(1)/CHRD(1)
      VAR(1)=CFOC(1)
      LGH(1)=12
      CALL INTERX(1,X1418A,VAR,LGH,Y1418A,ALPHAD(1),12,12,
     1            0,0,0,0,0,0,0,0,F1418A,3,ROUTID)
      DO 1050 K=2,5
         N=K-1
         ETA(K)=ETA(N)+DELN4
         CF(K)=CFI-ARG1*(ETA(K)-ETA(1))
C
C              FIGURE 6.1.4.1-15 (KB)
C
         CALL TLINEX(X11419,X21419,Y61419,3,12,ARG5,ETA(K),RKB(K),
     1               0,0,0,0,F61419,3,ROUTID)
         DKB(N)=RKB(K)-RKB(N)
         CHRD(K)=ARG4+ETA(K)*ARG2
C
C              FIGURE 6.1.4.1-14 (ALPHA)DELTA
C
         CFOC(K)=CF(K)/CHRD(K)
         VAR(1)=CFOC(K)
         LGH(1)=12
         CALL INTERX(1,X1418A,VAR,LGH,Y1418A,ALPHAD(K),12,12,
     1               0,0,0,0,0,0,0,0,F1418A,3,ROUTID)
         ALDAVG(N)=0.50*(ALPHAD(K)+ALPHAD(N))
         SWF(N)=ARG3*(2.-(1.-ARG5)*(ETA(N)+ETA(K)))
 1050 CONTINUE
C
C*****CALCULATE SECTION LIFT INCREMENT FOR EACH SPANWISE SEGMENT
C
      EXPDCL=.FALSE.
      NN=0
C
C              FIGURE 4.1.1.2-8A
C
      ARG1=ALOG10(RF*CBAREX)
      IF(IFTYPE.EQ.1)CALL TLINEX(X1128A,X2128A,Y1128A,3,11,ARG1,TANPHE,
     1                           CLOCLT,1,0,0,1,F1128A,3,ROUTID)
      DO 1200 I=1,NDELTA
         ARG8=DELTA(I)*CLASEC
         IF(SDCL(I).NE.UNUSED)EXPDCL=.TRUE.
         IF(EXPDCL.AND.(.NOT.TRANSL))GO TO 1190
         CP(1)=CPI(I)
         DO 1170 K=1,5
            IF(K.EQ.1)GO TO 1060
            IF(.NOT.TRANSL)GO TO 1060
            ARG1=       (CPI(I)-CPO(I))/(4.*DELN4)
            CP(K)=CPI(I)-ARG1*(ETA(K)-ETA(1))
 1060       CONTINUE
            IF(EXPDCL)GO TO 1150
            ARGZ=ABS(DELTA(I))
            GO TO (1070,1090,1090,1150,1100,1110,1120,1130),IFTYPE
 1070       CONTINUE
C
C*****PLAIN TRAILING-EDGE FLAPS
C
            IF(I.GT.1)GO TO 1080
C
C              FIGURE 6.1.1.1-39B
C
            CALL TLINEX(X1125B,X2125B,Y1125B,16,6,CLOCLT,CFOC(K),
     1                  CLDOCT(K),0,0,0,0,F1125B,3,ROUTID)
C
C              FIGURE 6.1.1.1-39A
C
            CALL TLINEX(X1125A,X2125A,Y1125A,8,8,TC,CFOC(K),CLDTHY(K),
     1                  0,0,0,0,F1125A,3,ROUTID)
C
C              FIGURE 6.1.1.1-40
C
 1080       CALL TLINEX(X11126,X21126,Y11126,7,14,CFOC(K),ARGZ,KFPRM,
     1                  0,0,0,0,F11126,3,ROUTID)
            DELCL(K)=DELTA(I)*CLDOCT(K)*CLDTHY(K)*KFPRM/RAD
            GO TO 1140
 1090 CONTINUE
C
C*****SINGLE-SLOTTED AND FOWLER TRAILING-EDGE FLAPS
C              FIGURE 6.1.1.1.-41
C
            CALL TLINEX(X11127,X21127,Y11127,5,13,CFOC(K),ARGZ,
     1                  ALPHAD(K),0,0,0,0,F11127,3,ROUTID)
            DELCL(K)=-CLASEC*ALPHAD(K)*DELTA(I)
            GO TO 1140
 1100       CONTINUE
C
C*****SPLIT TRAILING-EDGE FLAPS
C
            VAR(1)=DELTA(I)
            VAR(2)=CFOC(K)
            LGH(1)=10
            LGH(2)=4
            CALL INTERX(2,X11147,VAR,LGH,Y11147,ALFAD,10,40,
     1                  1,1,0,0,1,1,0,0,F11147,3,ROUTID)
            DELCL(K)=-CLASEC*ALFAD*DELTA(I)
            GO TO 1140
 1110       CONTINUE
C
C*****LEADING-EDGE FLAPS
C
            VAR(1)=CFOC(K)
            LGH(1)=7
            CALL INTERX(1,X11150,VAR,LGH,Y11150,CLDK,7,7,
     1                  0,0,0,0,1,0,0,0,F11150,3,ROUTID)
            DELCL(K)=CLDK*DELTA(I)
            GO TO 1140
 1120       CONTINUE
C
C*****LEADING-EDGE SLATS
C
            VAR(1)=CFOC(K)
            LGH(1)=7
            CALL INTERX(1,X11150,VAR,LGH,Y11150,CLDK,7,7,
     1                  0,0,0,0,1,0,0,0,F11150,3,ROUTID)
            DELCL(K)=CLDK*DELTA(I)*CP(K)/CHRD(K)
            GO TO 1140
 1130       CONTINUE
C
C*****KRUEGER L.E. FLAPS
C
            VAR(1)=CFOC(K)
            LGH(1)=7
            CALL INTERX(1,X11150,VAR,LGH,Y11150,CLDK,7,7,
     1                  0,0,0,0,1,0,0,0,F11150,3,ROUTID)
            DELCL(K)=CLDK*DELTA(I)*CP(K)/CHRD(K)
 1140       CONTINUE
C
C*****COMPUTE AVERAGE SECTION DELTA CL OVER SPANWISE SEGMENT
C
            N=K-1
            IF(N.EQ.0) GO TO 1170
            NN=NN+1
            DELCLA(NN)=(DELCL(K)+DELCL(N))/2.
            ALDAG(NN)=-DELCLA(NN)/ARG8
C
C*****COMPUTE AVERAGE CP/CK OVER SPANWISE SEGMENT
C
            IF(IFTYPE .EQ. 4) GO TO 1160
            IF(.NOT.TRANSL)GO TO 1160
 1150       CONTINUE
            CPOCF(N)=(CP(K)/CHRD(K)+CP(N)/CHRD(N))/2.
            CFACTR(N)=(CPOCF(N)-1.)*SWF(N)/SR
            IF(EXPDCL)GO TO 1170
            IF(IFTYPE .NE. 4) GO TO 1160
C
C*****HERE FOR SECTION DELTA-CL FOR DOUBLE SLOTTED FLAPS
C
            AARG1=(CF2I-CF2O)/(4.*DELN4)
            AARG2=(CAPI(I)-CAPO(I))/(4.*DELN4)
            CAPR=CAPI(I)-AARG2*(ETA(K)-ETA(1))
            CF2(K)=CF2I-AARG1*(ETA(K)-ETA(1))
            CF2OC(K)=CF2(K)/CHRD(K)
            PHI1=DELTA(I)+ATAN(TANPHE)*RAD
            PHI2=PHI1+DF2(I)
            VAR(1)=PHI1
            IF(CFOC(K) .LE. .10) VAR(2)=.10
            IF(CFOC(K) .GE. .40) VAR(2)=.40
            IF(CFOC(K) .GT. .10 .AND. CFOC(K) .LT. .40) VAR(2)=CFOC(K)
            LGH(1)=7
            LGH(2)=3
            CALL INTERX(2,X6143A,VAR,LGH,Y6143A,ATEA1,7,21,
     1                  1,1,0,0,1,1,0,0,F6143A,3,ROUTID)
            LGH(1)=11
            VAR(1)=CFOC(K)
            CALL INTERX(1,X61142,VAR,LGH,Y61142,CLDF1,11,11,
     1                  1,0,0,0,1,0,0,0,F61142,3,ROUTID)
            VAR(1)=PHI2
            LGH(1)=7
            IF(CF2OC(K) .LE. .10) VAR(2)=.10
            IF(CF2OC(K) .GE. .40) VAR(2)=.40
            IF(CF2OC(K) .GT. .10 .AND. CF2OC(K) .LT. .4)VAR(2)=CF2OC(K)
            CALL INTERX(2,X6143A,VAR,LGH,Y6143A,ATEA2,7,21,
     1                  1,1,0,0,1,1,0,0,F6143A,3,ROUTID)
            LGH(1)=11
            VAR(1)=CFOC(K)
            CALL INTERX(1,X61142,VAR,LGH,Y61142,CLDF2,11,11,
     1                  1,0,0,0,1,0,0,0,F61142,3,ROUTID)
            IF((CFOC(K)/CF2OC(K)) .LE. 0.60) DELCL(N)=ATEA1*CLDF1*DELTA
     1      (I)*(1.+CFOC(K))+ATEA2*CLDF2*(DELTA(I)+DF2(I))*CP(K)/CHRD(K)
            IF((CFOC(K)/CF2OC(K)) .LE. 0.60) GO TO 1140
            VAR(1)=DF2(I)
            VAR(2)=DELTA(I)
            LGH(1)=7
            LGH(2)=3
            CALL INTERX(2,X6143B,VAR,LGH,Y6143B,ATEAT,7,21,
     1                  1,1,0,0,1,1,0,0,F6143B,3,ROUTID)
            DELCL(N)=ATEA1*CLDF1*DELTA(I)*CAPR/CHRD(K)+ATEA2*ATEAT
     1               *CLDF2*DF2(I)*(1.+(CP(K)-CAPR)/CHRD(K))
            GO TO 1140
 1160       CONTINUE
C
C*****SPANWISE SEGMENT DELTA CL
C              FIGURE 6.1.4.1-8
C
            CALL TLINEX(X11418,X21418,Y61418,10,10,ALDAVG(N),AW,
     1                  ADCADS(N),0,0,0,1,F61418,3,ROUTID)
            DCLK(N)=DELCLA(NN)*ADCADS(N)*DKB(N)*CLAW/CLASEC
 1170    CONTINUE
         IF(EXPDCL)GO TO 1180
         DCL(I)=DCLK(1)+DCLK(2)+DCLK(3)+DCLK(4)
         IF(.NOT.TRANSL)GO TO 1200
 1180    CONTINUE
         CFACT(I)=(CFACTR(1)+CFACTR(2)+CFACTR(3)+CFACTR(4))/4.0
         CLAD(I)=CFACT(I)*CLAW+CLAW
         IF(.NOT.EXPDCL)GO TO 1200
 1190    CONTINUE
C
C*****HERE IF SECTION DELTA CL WAS INPUT
C
      VAR(1)=SDCL(I)/(CLASEC*DELTA(I))
      VAR(2)=AW
C
C              FIGURE 6.1.4.1-8
C
         CALL TLINEX(X11418,X21418,Y61418,10,10,VAR(1),VAR(2),ADCAD(I),
     1               0,0,0,1,F61418,3,ROUTID)
         DCL(I)=SDCL(I)*ADCAD(I)*(RKB(5)-RKB(1))*CLAW/CLASEC
 1200 CONTINUE
      IF(IFTYPE.GE.6) GO TO 1380
      DO 1370 I=1,NDELTA
         IF(I.GT.1)GO TO 1270
C
C*****CALCULATE SECTION DELTA (CL)MAX
C
         TROFS=CHRD(5)/CHRD(1)
         CBARFS=2./3.*CHRD(1)*(1.+TROFS*(1.+TROFS))/(1.+TROFS)
         ETAFS=(1.+2.*TROFS)/(3.*(1.+TROFS))
         CFOCA=(CFI-ETAFS*(CFI-CFO))/CBARFS
         VAR(1)=TC*100.
         GO TO(1220,1210,1230,1230,1220,1380,1380,1380),IFTYPE
 1210 CONTINUE
C
C              FIGURE 6.1.1.3-12AC (SINGLE SLOT) (DCL)BASE
C
         LGH(1)=12
         CALL INTERX(1,X137AC,VAR,LGH,Y137AC,DCLMAB   ,12,12,
     1               0,0,0,0,0,0,0,0,F137AC,3,ROUTID)
         GO TO 1240
 1220    CONTINUE
C
C              FIGURE 6.1.1.3-12AD (SPLIT AND PLAIN) (DCL)BASE
C
         LGH(1)=17
         CALL INTERX(1,X137AD,VAR,LGH,Y137AD,DCLMAB   ,17,17,
     1               0,0,0,0,0,0,0,0,F137AD,3,ROUTID)
         GO TO 1240
 1230    CONTINUE
C
C              FIGURE 6.1.1.3-12AB (2-SLOT AND FOWLER)  (DCL)BASE
C
         LGH(1)=12
         CALL INTERX(1,X137AB,VAR,LGH,Y137AB,DCLMAB   ,12,12,
     1               0,0,0,0,0,0,0,0,F137AB,3,ROUTID)
 1240    CONTINUE
         VAR(1)=CFOCA *100.
         GO TO(1250,1250,1260,1260,1250,1380,1380,1380),IFTYPE
 1250    CONTINUE
C
C              FIGURE 6.1.1.3-12B1 (SPLIT,PLAIN,AND 1-SLOT) K1
C
         LGH(1)=13
         CALL INTERX(1,X137B1,VAR,LGH,Y137B1,RK1,13,13,
     1               0,0,0,0,1,0,0,0,F137B1,3,ROUTID)
         GO TO 1270
 1260    CONTINUE
C
C              FIGURE 6.1.1.3-12B2 (2-SLOT AND FOWLER) K1
C
         LGH(1)=2
         CALL INTERX(1,X137B2,VAR,LGH,Y137B2,RK1,2,2,
     1               0,0,0,0,1,0,0,0,F137B2,3,ROUTID)
 1270    CONTINUE
C
C*****SET REFERENCE FLAP DEFLECTIONS FROM FIG.6.1.1.3-13A
C
      DREFF=40.
      DREF1S=45.
      DREF2S=50.
      DREFSP=60.
      VAR(1)=ABS(DELTA(I))
      LGH(1)=12
      GO TO(1280,1290,1300,1310,1280,1380,1320,1380),IFTYPE
 1280 CONTINUE
C
C              FIGURE 6.1.1.3-13A (SPLIT AND PLAIN) K2
C
      CALL INTERX(1,X1138A,VAR,LGH,Y138A4,RK2(I),12,12,
     1            0,0,0,0,0,0,0,0,F138A4,3,ROUTID)
      GO TO 1330
 1290 CONTINUE
C
C              FIGURE 6.1.1.3-13A (1-SLOT) K2
C
         CALL INTERX(1,X1138A,VAR,LGH,Y138A2,RK2(I),12,12,
     1               0,0,0,0,0,0,0,0,F138A2,3,ROUTID)
      GO TO 1330
 1300 CONTINUE
C
C              FIGURE 6.1.1.3-13A (FOWLER) K2
C
         CALL INTERX(1,X1138A,VAR,LGH,Y138A1,RK2(I),12,12,
     1               0,0,0,0,0,0,0,0,F138A1,3,ROUTID)
         GO TO 1330
 1310    CONTINUE
C
C              FIGURE 6.1.1.3-13A (2-SLOT) K2
C
         CALL INTERX(1,X1138A,VAR,LGH,Y138A3,RK2(I),12,12,
     1               0,0,0,0,0,0,0,0,F138A3,3,ROUTID)
         GO TO 1330
 1320    CONTINUE
C
C*****DELTA CLMAX FOR L.E. SLATS
C
         DCLMAX(I)=1.28*CFOC(K)/.18*COSC4**2*((BOF-BIF)/BTHEO)**2
         GO TO 1370
 1330    CONTINUE
         IF(IFTYPE.EQ.1.OR.IFTYPE.EQ.5) GO TO 1350
         IF(IFTYPE.EQ.4) GO TO 1340
C
C*****HERE FOR 1-SLOT OR FOWLER
C
         VAR(1)=ABS(DELTA(I))/DREFF
         IF(IFTYPE.EQ.2)VAR(1)=ABS(DELTA(I))/DREF1S
C
C              FIGURE 6.1.1.3-13B1 (1-SLOT AND FOWLER ) K3
C
         LGH(1)=9
         CALL INTERX(1,X1138B,VAR,LGH,Y138B1,RK3,9,9,
     1               0,0,0,0,0,0,0,0,F138B1,3,ROUTID)
         GO TO 1360
 1340    CONTINUE
C
C*****HERE FOR 2-SLOT
C
         VAR(1)=ABS(DELTA(I))/DREF2S
C
C              FIGURE 6.1.1.3-13B2 (2-SLOT) K3
C
         LGH(1)=9
         CALL INTERX(1,X1138B,VAR,LGH,Y138B2,RK3,9,9,
     1               0,0,0,0,0,0,0,0,F138B2,3,ROUTID)
         GO TO 1360
 1350    RK3=1.
 1360    CONTINUE
         DSCLMX(I)=RK1*RK2(I)*RK3*DCLMAB
C
C*****INCREMENT IN MAXIMUM LIFT COEFFICIENT DUE TO TE FLAP DEFLECTION
C
         SWFT=SWF(1)+SWF(2)+SWF(3)+SWF(4)
         KSWEEP=(1.-0.08*COSC4**2)*COSC4**0.75
         DCLMAX(I)=DSCLMX(I)*SWFT*KSWEEP/SR
 1370 CONTINUE
 1380 CONTINUE
      RETURN
      END
