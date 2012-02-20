      SUBROUTINE LATFLP
      REAL KYAW,KYAZ
      REAL MACH  ,KPRML(5),KPRMR(5)
      REAL KFACTR(10),KSSD,KC
      LOGICAL TIPCAL
      DIMENSION CLDLT(10),CLDRT(10),CLROLT(10),CNTEMP(2,200)
      DIMENSION ROUTID(2)
      DIMENSION X1132B(4),X2132B(11),Y1132B(44),F1132B(3)
      DIMENSION CLROLL(10),ANS(5),BCLOK(5),DBCLOK(5),CLDPM(5)
      DIMENSION CHRD(5),VAR(4),LGH(4),ETA(5),DELTAS(10),CLSSDF(10)
      COMMON /OVERLY/NLOG,NMACH,I,NALPHA
      COMMON /CONSNT/ PI,DR,UNUSED,RAD
      COMMON /OPTION/ SREF,CBARR,ROUGFC,BLREF
      COMMON /FLGTCD/ FLC(93)
      COMMON /IHT/    PHT,HT(380)
      COMMON /HTI/    HTIN(131)
      COMMON /IBODY/  PBODY,BODY(400)
      COMMON /HTDATA/ AHT(195),BHT(49)
      COMMON /WINGI/  WINGIN(77)
      COMMON /POWR/   PW(59),FLA(45)
      COMMON /FLAPIN/ F(69)
      COMMON /WINGD/  A(195),B(49)
      COMMON /WBHCAL/ WBT(155)
      COMMON /IDWASH/ PDWASH,DWASH(60)
      COMMON /IWING/  PWING,WING(400)
      COMMON /SUPDW/  DW(35),TCD(58)
      COMMON /FLOLOG/ FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1                HEAD,P RPOWR,JETPOW,LOASRT,TVTPAN,
     2                SUPERS,SUBSON,TRANSN,HYPERS,
     3                SYMFP,ASYFP,TRIMC,TRIM
      LOGICAL         FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1                HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,
     2                SUPERS,SUBSON,TRANSN,HYPERS,
     3                SYMFP,ASYFP,TRIMC,TRIM
      DIMENSION X11126(7),X21126(14),Y11126(98),F11126(3)
      DIMENSION X1125A(8),X2125A(8),Y1125A(64),F1125A(3)
      DIMENSION X1125B(16),X2125B( 6),Y1125B(96),F1125B(3)
      DIMENSION X2128A(11),X1128A(3),Y1128A(33),F1128A(3)
      DIMENSION X21123(11),X11123(4),X31123(3),X41123(3),Y21123(396),
     1          F21123(3),Y1(44),Y2(44),Y3(44),Y4(44),Y5(44),Y6(44),
     2          Y7(44),Y8(44),Y9(44),YG(396)
      DIMENSION X22219(7),X12219(4),X32219(4),Y62219(112),F62219(3)
      DIMENSION X2110A(6),X1110A(5),Y2110A(30),F2110A(3)
      DIMENSION X2110B(6),X1110B(4),Y2110B(24),F2110B(3)
      DIMENSION X2110C(2),X1110C(2),Y2110C(4),F2110C(3)
      DIMENSION X2110D(2),X1110D(4),Y2110D(8),F2110D(3)
      DIMENSION X211A0(5),X111A0(4),Y111A0(20),F111A0(3)
      DIMENSION X211A1(3),X111A1(3),Y111A1(9),F111A1(3)
      DIMENSION X211A2(3),X111A2(2),Y111A2(6),F111A2(3)
      DIMENSION X111A3(3),Y111A3(3),F111A3(3)
      DIMENSION X1111B(3),X2111B(2),Y2111B(6),F2111B(3)
      DIMENSION X1111C(4),X2111C(2),Y2111C(8),F2111C(3)
      DIMENSION X1111D(3),X2111D(2),Y2111D(6),F2111D(3)
      DIMENSION X2111E(2),Y2111E(2),F2111E(3)
      DIMENSION X22112(12),Y22112(12),F22112(3)
      DIMENSION X1126A(14),Y1126A(14),F1126A(3)
      DIMENSION DDOC(10)
      DIMENSION X6226B(10),Y6226B(10),F6226B(3)
      EQUIVALENCE (DDOC(1),F(1))
      EQUIVALENCE (Y1(1),YG(1)),(Y2(1),YG(45)),(Y3(1),YG(89)),(Y4(1),
     1      YG(133)),(Y5(1),YG(177)),(Y6(1),YG(221)),(Y7(1),YG(265)),
     2      (Y8(1),YG(309)),(Y9(1),YG(353)),(YG(1),Y21123(1))
      DIMENSION X62122(6),Y62122(6),F62122(3)
      DIMENSION RIVBH(20),GAMVR(20),DEDALP(20)
      DIMENSION CLROL(200),CLDH(20),CF(5)
      DIMENSION CFOC(5),DELTAL(10),DELTAR(10),DCLL(5),CLDOCT(5),
     1          CLDTHY(5),DCLR(5),DLCL(4),DRCL(4),ALDL(4),
     2          ALDR(4),CLDL(10),CLDR(10),CLW(20),CLDELT(10),CN(200),
     3          XSOC(10),CNODS(10),DSOC(10),CNSPOL(10),HSOC(10)
      DIMENSION WTYPE(4)
      EQUIVALENCE (SWEEPB,FLA(1)),(BCLOKI,FLA(2)),(BCLOKO,FLA(3)),
     1            (BCLDOK,FLA(4)),(CLDPRM,FLA(5)),(CLDL(1),FLA(6)),
     2            (CLDR(1),FLA(16)),(CLROLL(1),HT(211)),
     3            (SBACKI,FLA(36)),(THETAI,FLA(37)),(DELETO,FLA(38)),
     4            (DELETI,FLA(39)),(ETAIEF,FLA(40)),(ETAOEF,FLA(41))
      EQUIVALENCE (BCLDI,FLA(42)),(BCLDO,FLA(43)),(KFACTR(1),FLA(26)),
     1            (CNSPOL(1),HT(221)),(KYAW,FLA(45))
      EQUIVALENCE (BO2H,HTIN(4)),(BO2HST,HTIN(3)),(AWHST,AHT(7)),
     1            (SHST,AHT(3)),(RIVBH(1),WBT(68)),(GAMVR(1),WBT(46)),
     2            (DEDALP(1),DWASH(41))
      EQUIVALENCE (DELTAL(1),F(19)),(DELTAR(1),F(29)),(XSPRME,F(59))
      EQUIVALENCE (BOF,F(15)) ,(TANLE,A(62)),(STYPE,F(18))
      EQUIVALENCE (CN(1),BODY(201)),(WING(201),CLROL(1))
      EQUIVALENCE (TANC4,A(68)),(AW,A(120)),(TAPRW,A(118)),(SWEPTE,A(76)
     1             ),(COSTE,A(79)),(CLW(1),WING(21))
      EQUIVALENCE (CR,WINGIN(6)),(COSC4,A(67)),(CBAREX,A(16)),
     1            (BO2,WINGIN(4)),(SWEPLE,A(58)),(TANTE,A(80)),
     2            (TOVC,WINGIN(16))
      EQUIVALENCE (BIF,F(14)),(TANPHE,F(11)),(CFI,F(12)),(CFO,F(13)),
     1            (XSOC(1),F(49)),(DSOC(1),F(39)),(HSOC(1),F(60)),
     2            (GD1,TCD(43)),(GD2,TCD(44)),(GD3,TCD(45)),
     3            (GD4,TCD(46))
      DATA WTYPE/4HSTRA,4HDOUB,4HCRAN,4HCURV/
      DATA ROUTID/4HLATF,4HLP  /
C
C                   FIGURE 4.1.1.2-8A
C
      DATA X2128A
     1  /0., 0.02,  0.04,  0.06,  0.08,  0.10,  0.12,  0.14,  0.16,
     2       0.18,  0.20 /
      DATA X1128A
     1        /1.,     10.,    100./
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
C                  FIGURE 6.2.1.1-23A-C
C
      DATA F21123/4H6.2.,4H1.1-,4H23  /
      DATA X21123
     1 / 0., .1, .2, .3,  .4, .5, .6, .7, .8, .9, 1. /
      DATA X11123
     1 / -40., 0., 40., 60. /
      DATA X31123
     1 / 2., 4., 8. /
      DATA X41123
     1 / 0., 0.5, 1. /
      DATA Y1
     1 / 0.000,   0.005,  0.014,  0.030,  0.055,  0.087,  0.126,
     2   0.163,   0.197,  0.222,  0.245,
     3   0.000,   0.005,  0.017,  0.036,  0.063,  0.100,  0.144,
     4   0.185,  0.224,  0.262,  0.282,
     5   0.000,  0.005,  0.017,  0.036,  0.064,  0.103,  0.149,
     6   0.192,  0.230,  0.264,  0.282,
     7   0.000,  0.005,  0.017,  0.036,  0.062,  0.098,  0.136,
     8   0.178,  0.215,  0.243,  0.259 /
      DATA Y2
     1 / 0.000,  0.007,  0.027,  0.054,  0.097,  0.145,  0.197,
     2   0.243,  0.292,  0.333,  0.363,
     3   0.000,  0.008,  0.029,  0.064,  0.113,  0.173,  0.235,
     4   0.300,  0.363,  0.414,  0.445,
     5   0.000,  0.008,  0.029,  0.064,  0.119,  0.180,  0.245,
     6   0.313,  0.374,  0.417,  0.443,
     7   0.000,  0.008,  0.029,  0.063,  0.108,  0.163,  0.220,
     8   0.273,  0.319,  0.350,  0.363 /
      DATA Y3
     1 / 0.000,  0.006,  0.032,  0.080,  0.145,  0.212,  0.275,
     2   0.336,  0.394,  0.446,  0.485,
     3   0.000,  0.012,  0.043,  0.093,  0.165,  0.255,  0.354,
     4   0.445,  0.525,  0.587,  0.627,
     5   0.000,  0.013,  0.045,  0.106,  0.189,  0.280,  0.375,
     6   0.465,  0.542,  0.592,  0.612,
     7   0.000,  0.013,  0.045,  0.106,  0.187,  0.258,  0.327,
     8   0.390,  0.443,  0.475,  0.485 /
      DATA Y4
     1 / 0.000,  0.006,  0.017,  0.038,  0.066,  0.100,  0.142,
     2   0.185,  0.232,  0.273,  0.293,
     3   0.000,  0.007,  0.023,  0.048,  0.079,  0.114,  0.154,
     4   0.200,  0.249,  0.284,  0.311,
     5   0.000,  0.007,  0.023,  0.048,  0.079,  0.114,  0.154,
     6   0.198,  0.240,  0.276,  0.304,
     7   0.000,  0.007,  0.023,  0.043,  0.072,  0.106,  0.148,
     8   0.190,  0.232,  0.264,  0.286 /
      DATA Y5
     1 / 0.000,  0.006,  0.025,  0.060,  0.100,  0.151,  0.214,
     2   0.294,  0.370,  0.430,  0.479,
     3   0.000,  0.009,  0.033,  0.075,  0.125,  0.185,  0.259,
     4   0.348,  0.427,  0.491,  0.538,
     5   0.000,  0.009,  0.033,  0.075,  0.125,  0.185,  0.259,
     6   0.340,  0.413,  0.472,  0.515,
     7   0.000,  0.008,  0.026,  0.065,  0.113,  0.170,  0.231,
     8   0.294,  0.350,  0.398,  0.432 /
      DATA Y6
     1 / 0.000,  0.007,  0.034,  0.072,  0.125,  0.187,  0.270,
     2   0.371,  0.480,  0.580,  0.662,
     3   0.000,  0.014,  0.052,  0.108,  0.180,  0.275,  0.390,
     4   0.510,  0.620,  0.718,  0.800,
     5   0.000,  0.014,  0.052,  0.113,  0.190,  0.280, 0.385,
     6   0.485,  0.578,  0.658,  0.729,
     7   0.000,  0.014,  0.052,  0.103,  0.163,  0.234,  0.308,
     8   0.379,  0.445,  0.504,  0.554 /
      DATA Y7
     1 / 0.000,  0.003,  0.015,  0.037,  0.065,  0.103,  0.147,
     2   0.190,  0.237,  0.280,  0.306,
     3   0.000,  0.004,  0.020,  0.045,  0.079,  0.118,  0.159,
     4   0.205,  0.252,  0.291,  0.315,
     5   0.000,  0.004,  0.020,  0.045,  0.079,  0.114,  0.156,
     6   0.200,  0.246,  0.286,  0.310,
     7   0.000,  0.004,  0.020,  0.045,  0.073,  0.108,  0.147,
     8   0.187,  0.230,  0.268,  0.291 /
      DATA Y8
     1 / 0.000,  0.008,  0.033,  0.063,  0.108,  0.162,  0.225,
     2   0.303,  0.389,  0.453,  0.500,
     3   0.000,  0.008,  0.033,  0.072,  0.122,  0.187,  0.260,
     4   0.348,  0.437,  0.504,  0.558,
     5   0.000,  0.008,  0.033,  0.072,  0.122,  0.187,  0.258,
     6   0.335,  0.413,  0.474,  0.522,
     7   0.000,  0.008,  0.033,  0.070,  0.110,  0.165,  0.229,
     8   0.290,  0.347,  0.400,  0.445 /
      DATA Y9
     1 / 0.000,  0.010,  0.035,  0.070,  0.118,  0.192,  0.291,
     2   0.405,  0.520,  0.626,  0.721,
     3   0.000,  0.014,  0.049,  0.104,  0.179,  0.275,  0.393,
     4   0.520,  0.653,  0.768,  0.850,
     5   0.000,  0.014,  0.050,  0.109,  0.186,  0.284,  0.393,
     6   0.505,  0.602,  0.690,  0.763,
     7   0.000,  0.014,  0.040,  0.094,  0.155,  0.230,  0.306,
     8   0.380,  0.458,  0.519,  0.578 /
C
C     ----FIGURE 6.2.2.1-9
C
      DATA X22219/.0,.2,.4,.6,.7,.8,.9/,X12219/3.,4.,6.,8./,
     1 X32219/.25,.5,.75,1.0/,F62219/4H6.2.,4H2.1-,4H9   /,
     2 Y62219/-.285,-.278,-.275,-.282,-.289,-.30,-.315 , -.234,-.223,
     3 -.217,-.220,-.223,-.226,-.230 , -.160,-.152,-.146,-.146,-.150,
     4 -.162,-.175 , -.120,-.116,-.110,-.106,-.112,-.125,-.140 ,
     5 -.338,-.336,-.338,-.340,-.347,-.355,-.364, -.250,-.250,-.252,
     6 -.258,-.264,-.270,-.280 , -.170,-.170,-.172,-.182,-.190,-.202,
     7 -.219 , -.130,-.131,-.132,-.136,-.145,-.158,-.175 ,-.327,-.327,
     8 -.330 , -.340,-.362,-.390,-.430 , -.261,-.261,-.265,-.280,-.294,
     9 -.307,-.323 , -.179,-.179,-.186,-.20,-.212,-.225,-.240 , -.138,
     A -.138,-.140,-.159,-.172,-.184,-.195 , 7*-.361 , -.262,-.267,-.279
     B,-.295,-.305,-.315,-.325 , -.182,-.190,-.20,-.219,-.232,-.248,
     C-.270 , -.145,-.150,-.160,-.175,-.187,-.202,-.223 /
C
C     ----FIGURE 6.2.2.1-10 A-E
C
      DATA X2110A/.0,.3,.5,.7,.9,1.0/,X1110A/.4,.6,.8,.9,1.0/,
     1 F2110A/4H6.2.,4H2.1-,4H10A /,
     2 Y2110A/.0,.2,.24,.32,.44,.49 , .0,.5,.8,1.20,1.53,1.70 , .0,.91,
     3 1.45,2.0,2.42,2.68 , .0,1.38,2.10,2.75,3.30,3.55 , 0.,1.68,2.60,
     4 3.50,4.20,4.40 /
      DATA X2110B/0.,1.,2.,3.,4.,5./,X1110B/2.,3.,6.,10./,
     1 F2110B/4H6.2.,4H2.1-,4H10B /,
     2 Y2110B/.0,.5,1.0,1.35,1.65,1.85 , .0,.7,1.35,1.85,2.23,2.48 ,
     3 .0,.98,1.75,2.45,3.05,3.44 , .0,1.0,1.95,2.95,3.90,4.88 /
      DATA X2110C/0.,5./,X1110C/.4,1.0/,F2110C/4H6.2.,4H2.1-,4H10C /,
     1 Y2110C/0.,5.05 , 0.,8.4 /
      DATA X2110D/0.,5./,X1110D/.5,.6,.7,.8/,F2110D/4H6.2.,4H2.1-,4H10D
     1 /,Y2110D/.0,.310 , .0,.268 , .0,.20 , .0,.146 /
C
C     -----FIGURE 6.2.2.1-11 A-E
C     ----FIGURE 6.2.2.1-11 A IS BROKEN INTO 4 SEPARATE PARTS
C
      DATA X211A0/.0,.1,.2,.3,.4/,X111A0 /.4,.6,.8,1.0/,F111A0/4H6.2.,
     1 4H2.1-,4H11A0/,Y111A0/.0,.55,1.0,1.35,1.50 , 0.,.79,1.45,2.0,2.5
     2 , 0.,1.0,2.0,2.8,3.55 , 0.,1.6,3.0,4.25,5.3 /
      DATA X211A1/.4,.5,.6/,X111A1/.6,.8,1.0/,F111A1/4H6.2.,4H2.1-,
     1 4H11A1/,Y111A1/2.5,2.75,2.80 ,3.55,4.2,4.4 , 5.3,6.,6.55/
      DATA X211A2/.6,.7,.8/,X111A2/.8,1.0/,F111A2/4H6.2.,4H2.1-,4H11A2/,
     1 Y111A2/4.4,4.5,4.3 , 6.55,6.86,7.0 /
      DATA X111A3/.8,.9,1.0/,Y111A3/7.0,6.8,6.28/,F111A3/4H6.2.,4H2.1-,
     1 4H11A3/
      DATA X1111B/2.,4.,6./,X2111B/0.,7./,Y2111B/0.,2.85 ,0.,3.50 , 0.,
     1 3.75/,F2111B/4H6.2.,4H2.1-,4H11B /
      DATA X1111C/0.,.4,.6,1.0/,X2111C/0.,5./,F2111C/4H6.2.,4H2.1-,4H11B
     1 /,Y2111C/0.,7.45 , 0.,8.6 , 0.,10.0 , 0.,16.5 /
      DATA X1111D/20.,45.,60./,X2111D/0.,8./,Y2111D/0.,.153 ,0.,.172 ,0.
     1,.193 /,F2111D/4H6.2.,4H2.1-,4H11D /
      DATA X2111E/0.,.10/,Y2111E/0.,.150/,F2111E/4H6.2.,4H2.1-,4H11E /
C
C     ----FIGURE 6.2.2.1-12 SPOILER-SLOT-DEFLECTOR
C
      DATA X22112/.175,.3,.4,.5,.6,.7,.8,.9,1.0,1.1,1.15,1.25/,
     1 Y22112/2.3,2.33,2.34,2.32,2.29,2.22,2.09,1.81,1.53,1.38,1.37,1.49
     2/,F22112/4H6.2.,4H2.1-,4H12  /
C
C     ----FIGURE 6.2.1.1-26A
C
      DATA X1126A/0.,2.5,5.,7.5,10.,12.5,15.,17.5,20.,25.,30.,35.,40.,
     1 45./,F1126A/4H6.2.,4H1.1-,4H26A /,
     2 Y1126A/0.,9.,13.,16.,18.5,20.6,22.4,23.7,25.0,27.3,29.0,30.4,31.2
     3,31.6/
C
C     ----FIGURE 6.1.1.1-51B SPOILER LIFT EFFECTIVENESS-LOW SPEEDS.
C
      DATA X1132B/.5,.6,.7,.8/,X2132B/.06,.07,.08,.09,.10,.11,.12,.14,
     1.16,.18,.19/,F1132B/4H6.1.,4H1.1-,4H51B /,Y1132B/
     2 .012,.046,.068,.086,.102,.116,.126,.143,.158,.167,.170 ,
     3 .046,.064,.080,.095,.108,.121,.130,.149,.164,.175,.180 ,
     4 .054,.072,.088,.102,.116,.128,.137,.154,.170,.181,.186 ,
     5 .080,.093,.105,.117,.126,.136,.144,.160,.175,.189,.196 /
      DATA F62122/4H6.2.,4H1.2-,4H22  /
C
C              FIGURE 6.2.1.1-26B
C
      DATA X6226B/0.68,.770,1.0,1.17,1.50,2.0,2.5,3.0,3.5,4.0/
      DATA Y6226B/0.70,1.0,1.36,1.50,1.65,1.73,1.72,1.64,1.50,1.33/
      DATA F6226B/4H6.2.,4H1.1-,4H26B /
      DATA X62122/.165,.2,.25,.3,.35,.40/
      DATA Y62122/.830,.818,.779,.738,.680,.615/
C
      TIPCAL=.FALSE.
      NDELTA=F(16)+0.5
      RF=FLC(I+42)
      MACH=FLC(I+2)
      IF(TRANSN)MACH=.6
      BETA=SQRT(1.-MACH**2)
      CLASEC=WINGIN(I+20)
      IF(TRANSN)CLASEC=WINGIN(69)/.8
      SCALE=(2*WINGIN(4)*A(4))/(BLREF*SREF)
      IF(STYPE.EQ.5.)GO TO 1270
      ARG1=TANC4/BETA
      SWEEPB=ATAN(ARG1)*RAD
      KC=CLASEC*BETA/(2.*PI) *RAD
      DELN4=0.25*(BOF-BIF)/BO2
      ETA(1)=BIF/BO2
      ETA(5)=BOF/BO2
      IF(STYPE.NE.4.)GO TO 1160
      ARG1=(CFI-CFO)/(4.*DELN4)
      ARG2=(TANTE-TANLE)*BO2
 1000 DO 1010 K=1,5
         N=K-1
         IF(N.NE.0)ETA(K)=ETA(N)+DELN4
         CF(K)=CFI-ARG1*(ETA(K)-ETA(1))
         CHRD(K)=CR+ETA(K)*ARG2
         CFOC(K)=CF(K)/CHRD(K)
 1010 CONTINUE
C
C              FIGURE 6.2.1.1-23A-C
C
      ARG1=BETA*AW/KC
C
C------CALCULATE THE ROLLING MOMENT EFFECTIVENESS PARAMETER
C
      ARG2=ETA(1)
      DO 1020 J=1,5
         CALL TLIN4X(X11123,X21123,X31123,X41123,Y21123,4,11,3,3,SWEEPB,
     1           ARG2,ARG1,TAPRW,ANS(J),0,0,0,0,0,0,0,0,F21123,3,ROUTID)
         BCLOK(J)=ANS(J)
         ARG2=ARG2+DELN4
 1020 CONTINUE
      DO 1030 J=1,4
         DBCLOK(J)=BCLOK(J+1)-BCLOK(J)
         CLDPM(J)=DBCLOK(J)*KC/BETA
 1030 CONTINUE
      BCLOKI=ANS(1)
      BCLOKO=ANS(5)
      BCLDOK=BCLOKI
      BCLOKO=BCLOKI-BCLOKI
      CLDPRM=BCLDOK*KC/BETA
      NN=0
      ARG1=ALOG10(RF*CBAREX)
C
C              FIGURE 4.1.1-2-8A
C
      CALL TLINEX(X1128A,X2128A,Y1128A,3,11,ARG1,TANPHE,CLOCLT,
     1            0,0,0,0,F1128A,3,ROUTID)
      DO 1060 J=1,NDELTA
         ARG=ABS(DELTAL(J))
         ARG1=ABS(DELTAR(J))
         DO 1040 K=1,5
C
C              FIGURE 6.1.1.1-39A
C
            CALL TLINEX(X1125A,X2125A,Y1125A,8,8,TOVC,CFOC(K),CLDTHY(K),
     1                  0,0,0,0,F1125A,3,ROUTID)
C
C              FIGURE 6.1.1.1-39B
C
            CALL TLINEX(X1125B,X2125B,Y1125B,16,6,CLOCLT,CFOC(K),
     1                 CLDOCT(K),0,0,0,0,F1125B,3,ROUTID)
C
C              FIGURE 6.1.1.1-40
C
            CALL TLINEX(X11126,X21126,Y11126,7,14,CFOC(K),ARG,KPRML(K),
     1                  0,0,0,0,F11126,3,ROUTID)
            ARG2=CLDOCT(K)*CLDTHY(K)
            DCLL(K)=ARG2               *KPRML(K)
C
C              FIGURE 6.1.1.1-40
C
            CALL TLINEX(X11126,X21126,Y11126,7,14,CFOC(K),ARG1,KPRMR(K),
     1                  0,0,0,0,F11126,3,ROUTID)
            DCLR(K)=ARG2*KPRMR(K)
            IF(K.EQ.1)GO TO 1040
            N=K-1
            DLCL(N)=(DCLL(K)+DCLL(N))/2.0
            DRCL(N)=(DCLR(K)+DCLR(N))/2.0
            ALDL(N)=ABS(DLCL(N)/CLASEC)/RAD
            ALDR(N)=ABS(DRCL(N)/CLASEC)/RAD
 1040    CONTINUE
         IF(.NOT.TIPCAL) GO TO 1050
         CLDLT(J)=(CLDPM(1)*ALDL(1)+CLDPM(2)*ALDL(2)+CLDPM(3)*ALDL(3)+
     1             CLDPM(4)*ALDL(4))
         CLDRT(J)=(CLDPM(1)*ALDR(1)+CLDPM(2)*ALDR(2)+CLDPM(3)*ALDR(3)+
     1            CLDPM(4)*ALDR(4))
         CLROLT(J)=(CLDLT(J)*DELTAL(J)/RAD-CLDRT(J)*DELTAR(J)/RAD)/2.
     1             *SCALE
         GO TO 1060
 1050    CLDL(J)=(CLDPM(1)*ALDL(1)+CLDPM(2)*ALDL(2)+CLDPM(3)*ALDL(3)+
     1            CLDPM(4)*ALDL(4))
         CLDR(J)=(CLDPM(1)*ALDR(1)+CLDPM(2)*ALDR(2)+CLDPM(3)*ALDR(3)+
     1           CLDPM(4)*ALDR(4))
         CLROLL(J)=(CLDL(J)*DELTAL(J)/RAD-CLDR(J)*DELTAR(J)/RAD)/2.
     1             *SCALE
 1060 CONTINUE
      IF(ETA(5).GE..98) GO TO 1100
      TIPCAL=.TRUE.
      CFIX=CFI
      CFOX=CFO
      BOFX=BOF
      BIFX=BIF
      BCLKI=BCLOKI
      BCLKO=BCLOKO
      BCLDK=BCLDOK
      CLDP=CLDPRM
      L=1
      DELN4=0.25*(BO2-BIF)/BO2
      ETA(1)=BIF/BO2
      ETA(5)=1.0
      CFT=CFI-(CFI-CFO)*(BO2-BIF)/(BOF-BIF)
      ARG1=(CFI-CFT)/(4.*DELN4)
      ARG2=(TANTE-TANLE)*BO2
      GO TO 1090
 1070 CFI=CFO
      DELN4=0.25*(BO2-BOF)/BO2
      ETA(1)=BOF/BO2
      ARG1=(CFI-CFT)/(4.*DELN4)
      ARG2=(TANTE-TANLE)*BO2
      DO 1080 J=1,NDELTA
 1080 CLROLL(J)=CLROLT(J)
 1090 CONTINUE
      GO TO 1000
 1100 CONTINUE
C
C------YAWING MOMENT COEFFICIENT DUE TO ASYMMETRIC FLAP DEFLECTION OF
C      PLAIN TRAILING-EDGE FLAPS
C              FIGURE 6.2.2.1-9  KYAW
C
      CALL TLIN3X(X12219,X22219,X32219,Y62219,4,7,4,AW,ETA(1),TAPRW,KYAW
     1            ,0,0,0,0,0,0,F62219,3,ROUTID)
      IF(.NOT.TIPCAL) GO TO 1140
      NN=0
      DO 1110 N=1,NALPHA
         DO 1110 J=1,NDELTA
            NN=NN+1
            CNTEMP(L,NN)=KYAW*CLW(N)*CLROLT(J)
            IF(ABS(CLW(N)) .EQ. UNUSED) CNTEMP(L,NN)=KYAW*FLC(N+22)*
     1                                    WING(101)*CLROLT(J)
 1110 CONTINUE
      L=L+1
      IF(L.EQ.2) GO TO 1070
      NN=0
      DO 1120 N=1,NALPHA
         DO 1120 J=1,NDELTA
            NN=NN+1
            CN(NN)=CNTEMP(1,NN)-CNTEMP(2,NN)
 1120 CONTINUE
      CFI=CFIX
      CFO=CFOX
      BOF=BOFX
      BIF=BIFX
      BCLOKI=BCLKI
      BCLOKO=BCLKO
      BCLDOK=BCLDK
      CLDPRM=CLDP
      DO 1130 J=1,NDELTA
 1130 CLROLL(J)=CLROLL(J)-CLROLT(J)
      GO TO 1290
 1140 NN=0
      DO 1150 N=1,NALPHA
         DO 1150 J=1,NDELTA
            NN=NN+1
            CN(NN)= KYAW*CLW(N)*CLROLL(J)
            IF(ABS(CLW(N)) .EQ. UNUSED) CN(NN)=KYAW*FLC(N+22)*
     1                                    WING(101)*CLROLL(J)
 1150 CONTINUE
      GO TO 1290
 1160 CONTINUE
C
C------ROLLING MOMENT COEFFICIENT FOR PLUG OR FLAP TYPE SPOILERS
C------CALCULATE ETA(EFFECTIVE)
C
      ARG1=0.75-(1.-XSPRME)
      ARG2=(1.-TAPRW)/(1.+TAPRW)
      TANSI=TANC4-4.0*ARG1/AW*ARG2
      SBACKI=ATAN(TANSI)*RAD
C
C              FIGURE 6.2.1.1-26A
C
      VAR(1)=SBACKI
      LGH(1)=14
      CALL INTERX(1,X1126A,VAR,LGH,Y1126A,THETAI,14,14,
     1            0,0,0,0,0,0,0,0,F1126A,3,ROUTID)
      ARG1=4.*(1.-XSPRME)/(AW*(1.+TAPRW))
      ARG2=1.-(1.-TAPRW)*ETA(1)
      ARG3=COSTE*SIN(THETAI/RAD)/COS((SWEPTE+THETAI)/RAD)
      DELETI=ARG1*ARG2*ARG3
      ARG2=1.-(1.-TAPRW)*ETA(5)
      DELETO=ARG1*ARG2*ARG3
      ETAIEF=ETA(1)+DELETI
      ETAOEF=ETA(5)+DELETO
C
C------CALCULATE CLD(PRIME) AND THE ROLLING MOMENT COEFICIENT
C
      ARG4=BETA*AW/KC
C
C              FIGURE 6.2.1.1-23A-C (INBOARD)
C
      CALL TLIN4X(X11123,X21123,X31123,X41123,Y21123,4,11,3,3,SBACKI,
     1       ETAIEF,ARG4,TAPRW,BCLDI,0,0,0,0,0,0,0,0,F21123,3,ROUTID)
C
C              FIGURE 6.2.1.1-23A-C (OUTBOARD)
C
      CALL TLIN4X(X11123,X21123,X31123,X41123,Y21123,4,11,3,3,SBACKI,
     1       ETAOEF,ARG4,TAPRW,BCLDO,0,0,0,0,0,0,0,0,F21123,3,ROUTID)
      CLDPRM=KC*(BCLDO-BCLDI)/BETA
      ARG1=CLDPRM/2.0
      DO 1170 J=1,NDELTA
C
C              FIGURE 6.1.1.1-51B
C
         CALL TLINEX(X1132B,X2132B,Y1132B,4,11,XSOC(J),HSOC(J),
     1               DELTAS(J),0,0,0,0,F1132B,3,ROUTID)
 1170    CLROLL(J)=ARG1*DELTAS(J) *SCALE
C
C------HERE FOR SPOILER-SLOT-DEFLECTOR
C
      IF(STYPE.NE.3.)GO TO 1190
      LGH(1)=10
      DO 1180 J=1,NDELTA
C
C              FIGURE 6.2.1.1-26B (K)
C
         VAR(1)=DSOC(J)/DDOC(J)
         CALL INTERX(1,X6226B,VAR,LGH,Y6226B,KFACTR(J),10,10,
     1               0,0,0,0,0,0,0,0,F6226B,3,ROUTID)
 1180 CLROLL(J)=KFACTR(J)*CLROLL(J)
C
C------YAWING MOMENT COEFFICIENT FOR PLUG OR FLAP TYPE SPOILERS
C
 1190 IF(ABS(SWEPLE-SWEPTE).GT.4.0)GO TO 1210
C
C------HERE FOR STRAIGHT TAPERED WINGS WITHOUT SWEEPBACK
C              FIGURE 6.2.2.1-10A-C
C
      CALL TLINEX(X1110A,X2110A,Y2110A,5,6,ETA(5),ARG1,DUMYA,
     1            0,0,0,0,F2110A,3,ROUTID)
      ARG1=4.*DELN4
      CALL TLINEX(X1110B ,X2110B,Y2110B,4,6,AW,DUMYA,DUMYB,
     1            0,0,0,0,F2110B,3,ROUTID)
      CALL TLINEX(X1110C,X2110C,Y2110C,2,2,TAPRW,DUMYB,DUMYC,
     1            0,0,0,0,F2110C,3,ROUTID)
      DO 1200 J=1,NDELTA
C
C              FIGURE 6.2.2.1-10D
C
         CALL TLINEX(X1110D,X2110D,Y2110D,4,2,XSOC(J),DUMYC,CNODS(J),
     1               0,0,0,1,F2110D,3,ROUTID)
 1200 CNSPOL(J)= CNODS(J)*DSOC(J) *SCALE
      GO TO 1250
 1210 CONTINUE
C
C------HERE FOR STRAIGHT TAPERED WINGS WITH SWEEPBACK
C
      BS=4.*DELN4
C
C              FIGURE 6.2.2.1-11A
C
      IF(BS.LE.0.4)
     1   CALL TLINEX(X111A0,X211A0,Y111A0,4,5,BS,ETA(5),DUMYA,
     2               0,0,0,0,F111A0,3,ROUTID)
      IF(BS.GT.0.4.AND.(BS.LE.0.6))
     1   CALL TLINEX(X111A1,X211A1,Y111A1,3,3,
     2               BS,ETA(5),DUMYA,0,0,0,0,F111A1,3,ROUTID)
      IF(BS.GT.0.6.AND.(BS.LE.0.8))
     1   CALL TLINEX(X111A2,X211A2,Y111A2,2,3,
     2               BS,ETA(5),DUMYA,0,0,0,0,F111A2,3,ROUTID)
      VAR(1)=BS
      LGH(1)=3
      IF(BS.GT.0.8)
     1   CALL INTERX(1,X111A3,VAR,LGH,Y111A3,DUMYA,3,3,
     2               0,0,0,0,0,0,0,0,F111A3,3,ROUTID)
C
C              FIGURE 6.2.2.1-11B
C
      CALL TLINEX(X1111B,X2111B,Y2111B,3,2,AW,DUMYA,DUMYB,
     1            0,0,0,0,F2111B,3,ROUTID)
C
C              FIGURE 6.2.2.1-11C
C
      CALL TLINEX(X1111C,X2111C,Y2111C,4,2,TAPRW,DUMYB,DUMYC,
     1            0,0,0,0,F2111C,3,ROUTID)
C
C              FIGURE 6.2.2.1-11D
C
      CALL TLINEX(X1111D,X2111D,Y2111D,3,2,SWEPLE,DUMYC,DUMYD,
     1            0,0,0,0,F2111D,3,ROUTID)
C
C------HERE FOR FLAP TYPE SPOILER
C
      IF(STYPE.EQ.1.)GO TO 1220
      CNODSB=DUMYD
      GO TO 1230
C
C              FIGURE 6.2.2.1-11E
C
 1220 VAR(1)=DUMYD
      LGH(1)=2
      CALL INTERX(1,X2111E,VAR,LGH,Y2111E,CNODSB,2,2,
     1            0,0,0,0,1,0,0,0,F2111E,3,ROUTID)
 1230 DO 1240 J=1,NDELTA
 1240 CNSPOL(J)=CNODSB*DSOC(J)  *SCALE
 1250 IF(STYPE.NE.3.0)GO TO 1290
C
C------YAWING MOMENT DUE TO SPOILER-SLOT-DEFLECTOR
C              FIGURE 6.2.2.1-12 (K)
C
      VAR(1)=MACH*COSC4
      LGH(1)=12
      CALL INTERX(1,X22112,VAR,LGH,Y22112,KSSD,12,12,
     1            0,0,0,0,0,0,0,0,F22112,3,ROUTID)
      DO 1260 J=1,NDELTA
 1260 CNSPOL(J)=CNSPOL(J)*KSSD
      GO TO 1290
 1270 CONTINUE
C
C    -----ROLLING-MOMENT DUE TO DIFFERENTIALLY DEFLECTED HORZ. STABILIZ
C
      ARG1=0.352*GD1+0.503*GD2+0.344*GD3+0.041*GD4
      ARG2=0.383*GD1+0.707*GD2+0.924*GD3+0.500*GD4
      ETACP=ARG1/ARG2
      DHO2=BO2H-BO2HST
      VAR(1)=DHO2/BO2H
      LGH(1)=6
      CALL INTERX(1,X62122,VAR,LGH,Y62122,EQHOQ,6,6,
     1            2,0,0,0,2,0,0,0,F62122,3,ROUTID)
      YH=ETACP*BO2HST+DHO2
      CLAHM=HT(101)
      CLAHS=CLAHM*SREF/AHT(3)
      A2=DHO2/BO2HST
      ARG1=EQHOQ*YH*SHST*CLAHS/(BLREF*SREF)
      A1=PI*AW/RAD
      DO 1280 J=1,NALPHA
         B1=1.-A1*DEDALP(J)
         B2=RIVBH(J)*GAMVR(J)*A2
         CLDH(J)=0.5*(B1+B2)*ARG1
         DO 1280 K=1,NDELTA
            NN=10*(J-1)+K
 1280 CLROL(NN)= CLDH(J)*DELTAL(K)-CLDH(J)*DELTAR(K)
 1290 CONTINUE
      DO 1300 J=1,NDELTA
 1300 HT(J+200) = DELTAL(J) - DELTAR(J)
      RETURN
      END
