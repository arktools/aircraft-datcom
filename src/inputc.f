      SUBROUTINE INPUTC(JRETRN,I,KOL,NAMORD,ISDIM,SSAVE)
C
C***  SUBROUTINE READS CONTROL CARDS AND SETS FLAGS
C
C    INPUT DATA BLOCKS
C
      COMMON /FLGTCD/ NMACH,NALPHA,MACH(20),ALSCHD(20),RNNUB(20),NGH
     1                ,GRDHT(10),PINF(20),STMACH,TSMACH,TR,ALT(20)
     2                ,TINF(20),VINF(20),WT,GAMMA,NALT,LOOP
      COMMON /OPTION/ SREF, CBARR, ROUGFC, BLREF
      COMMON /SYNTSS/ XCG, XW, ZW, ALIW, ZCG, XH, ZH, ALIH, XV,
     1                VERTUP, HINAX, XVF, SCALE, ZV, ZVF, YV,YF,
     2                PHIV, PHIF
      COMMON /BODYI/  NX,X(20),S(20),P(20),R(20),ZU(20),ZL(20),
     1                BNOSE,BTAIL,BLN,BLA,DS,ITYPE,METHOD,ELLIP
      COMMON /WINGI/  CHRDTP,SSPNOP,SSPNE,SSPN,CHRDBP,CHRDR,SAVSI,SAVSO,
     1                CHSTAT,ALPHAO,TWISTA,SSPNDD,DHDADI,DHDADO,
     2                TYPE,
     3                TOVC,DELTAY,XOVC,CLI,ALPHAI,CLALPA(20),
     4                CLMAX(20),CMO,LERI,LERO,CAMBER,TOVCO,XOVCO,CMOT,
     5                CLMAXL,CLAMO,TCEFF,KSHARP,XAC(20),ARCL,YCM,CLD,
     6                SLOPE(6),DWASH
      COMMON /HTI/    HTIN(154)
      COMMON /VTI/    VTIN(154),BVP,BV,BDV,BH,SV,VPHITE,VLP,ZP,
     1                VFIN(154)
      COMMON /POWER/  AIETLP,NENGSP,THSTCP,PHALOC,PHVLOC,PRPRAD,ENGFCT,
     1                BWAPR3,BWAPR6,BWAPR9,NOPBPE,BAPR75,
     2                AIETLJ,NENGSJ,THSTCJ,JIALOC,JEVLOC,JEALOC,
     3                JINLTA,JEANGL,JEVELO,AMBTMP,JESTMP,JELLOC,
     4                JETOTP,AMBSTP,JERAD,YP,CROT,
     5                LBIN(21)
      COMMON /FLAPIN/ F(116),TTYPE,CFITC,CFOTC,BITC,BOTC,CFITT,CFOTT,
     1                BITT,BOTT,B1,B2,B3,B4,D1,D2,D3,GCMAX,
     2                KS,RL,BGR,DELR
      COMMON /WINGD/  A(195)
      COMMON /IBW/    PBW(131),TYPEIN,NACAW(80),NPTS,XCORD(50),
     1                YUPPER(50),YLOWER(50)
      COMMON /IBH/    PBH(131),HTYPE,NACAH(80)
      COMMON /IBV/    PBV(131),VTYPE,NACAV(80)
      COMMON /IVF/    PVF(131),VFTYP,NACAF(80)
C
      REAL NTYPE,NDELTA,NPTS,MEAN,ITYPE,JETFLP
      REAL LERI,LERO
      REAL NT,ME,ISP,LFP
      REAL NENGSP,NOPBPE,NENGSJ,JIALOC,JEVLOC,JEALOC,JINLTA,JEANGL,
     1     JEVELO,JESTMP,JELLOC,JETOTP,JERAD,KSHARP
      REAL NMACH,MACH,NALPHA,NGH
      REAL NX,LAFB,METHOD,NALT,LOOP,KS
C
      LOGICAL LAMNRJ
      LOGICAL VERTUP
      LOGICAL CROT,LAMNR
      LOGICAL CAMBER,CAMH,CAMV
C
      DIMENSION MEAN(50),THICK(50)
      DIMENSION TIME(10),FC(10),ALPHA(10),LAMNRJ(10)
      DIMENSION DELTA(10),CPRMEI(10),CPRMEO(10),SCLD(10),SCMD(10)
      DIMENSION DELTAD(10),DELTAL(10),DELTAR(10),DELTAS(10),XSOC(10),
     1          HSOC(10),HDELTA(10)
      DIMENSION DELJET(10),EFFJET(10),CAPINB(10),CAPOUT(10),DOBDEF(10)
      EQUIVALENCE (CAMH,HTIN(64)),(CAMV,VTIN(64))
      EQUIVALENCE (DELTA(1),DELTAD(1),F(1)),(PHETE,F(11)),
     1            (CHRDFI,F(12)),(CHRDFO,F(13)),(SPANFI,F(14)),
     2            (SPANFO,F(15)),(NDELTA,F(16)),(FTYPE,F(17)),
     3            (STYPE,F(18)),(SCLD(1),DELTAL(1),F(19)),
     4            (SCMD(1),DELTAR(1),F(29)),(CPRMEI(1),DELTAS(1),F(39)),
     5            (CPRMEO(1),XSOC(1),F(49)),(CB,XSPRME,F(59)),
     6            (TC,HSOC(1),F(60)),(PHETEP,F(61)),(NTYPE,F(62))
      EQUIVALENCE (F(1),ALITD),(F(2),XHL),(F(3),TWOTI),(F(4),CF),
     1 (F(5),HDELTA(1)),(F(15),LAMNR),(F(16),HNDLTA)
      EQUIVALENCE (TIME(1),F(1)),(NT,F(11)),
     1 (FC(1),F(12)),(ALPHA(1),F(22)),    (ME,F(32)),(ISP,F(33)),
     2 (SPAN,F(34)),(PHE,F(35)),(GP,F(36)),(CC,F(37)),(LFP,F(38)),
     3 (LAMNRJ(1),F(39)),(CMU,F(63)),(DELJET(1),F(64)),
     4 (JETFLP,F(74)),(EFFJET(1),F(75)),(CAPINB(1),F(85)),
     5 (CAPOUT(1),F(95)),(DOBDEF(1),F(105)),(DOBCIN,F(115)),
     6 (DOBCOT,F(116))
      EQUIVALENCE (CMO,CM0),(CMOT,CM0T),(CLAMO,CLAM0)
      EQUIVALENCE (MEAN(1),YUPPER(1)),(THICK(1),YLOWER(1))
      EQUIVALENCE (ALPHAO,ALPHA0)
C
C***  COMPUTATIONAL BLOCKS
C
      COMMON /CASEID/ IDCSE(74), KOUNT, NAMSV(100), DIM
      COMMON /BDATA/  BD(762)
      COMMON /EXPER/  KLIST, NLIST(100), NNAMES, IMACH, MDATA,
     1                KBODY, KWING, KHT, KVT, KWB, KDWASH(3),
     2                ALPOW, ALPLW, ALPOH, ALPLH
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD,KAND
      COMMON /FLOLOG/ FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1                HEAD,PRPOWR,JETPOW,LOASRT,TVTPN,SUPERS,SUBSON,
     2                TRANSN,HYPERS,SYMFP,ASYFP,TRIMC,TRIM,DAMP,
     3                HYPEF,TRAJET,BUILD,FIRST,DRCONV,PART,
     4                VFPL,VFSC,CTAB,PLOT
      COMMON /ERROR/  IERR,GONOGO,IEND,DMPALL,DPB,DPA,DPBD,DPAVF,
     1                DPFACT,DPWBT,DPBHT,DPAVT,DPAHT,DPC,DPD,DPWB,
     2                DPCHT,DPDHT,DPDYNH,SAVE,DMPCSE,DPDVT,DPGR,DPLB,
     3                DPPW,DPSTB,DPSBD,DPSLG,DPSWB,DPSTP,DPDWA,DPSTG,
     4                DPSLA,DPTRA,DPEXPD,DPDVF,DPFLP,DPFHG,DPFCM,DPTCD,
     5                DPFLA,DPTRM,DPSPR,DPTRN,DPTRM2,DPHYP,DPDYN,DPJET,
     6                DPHB,DPSHB,DPTRAH,DPSTBH,DPSEC,DPSLAH,DPINPT,
     7                DPFLC,DPOPTN,DPSYN,DPBDIN,DPWGIN,DPVTIN,DPTVT,
     8                DPVFIN,DPHTIN,DPPWIN,DPLBIN,DPF,DPIOM,
     9                DPIBDY,DPIWG,DPIHT,DPIVT,DPIVF,DPIBW,DPIBH,DPIBV,
     A                DPIBWH,DPIBWV,DPITOT,DPIPWR,DPIDWH,LIST,LEXTRP
C
      LOGICAL  FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1         HEAD,PRPOWR,JETPOW,LOASRT,TVTPN,SUPERS,SUBSON,
     2         TRANSN,HYPERS,SYMFP,ASYFP,TRIMC,TRIM,DAMP,
     3         HYPEF,TRAJET,BUILD,FIRST,DRCONV,PART,
     4         VFPL,VFSC,CTAB,PLOT
      LOGICAL  IERR,GONOGO,IEND,DMPALL,DPB,DPA,DPBD,DPAVF,
     1         DPFACT,DPWBT,DPBHT,DPAVT,DPAHT,DPC,DPD,DPWB,
     2         DPCHT,DPDHT,DPDYNH,SAVE,DMPCSE,DPDVT,DPGR,DPLB,
     3         DPPW,DPSTB,DPSBD,DPSLG,DPSWB,DPSTP,DPDWA,DPSTG,
     4         DPSLA,DPTRA,DPEXPD,DPDVF,DPFLP,DPFHG,DPFCM,DPTCD,
     5         DPFLA,DPTRM,DPSPR,DPTRN,DPTRM2,DPHYP,DPDYN,DPJET,
     6         DPHB,DPSHB,DPTRAH,DPSTBH,DPSEC,DPSLAH,DPINPT,
     7         DPFLC,DPOPTN,DPSYN,DPBDIN,DPWGIN,DPVTIN,DPTVT,
     8         DPVFIN,DPHTIN,DPPWIN,DPLBIN,DPF,DPIOM,
     9         DPIBDY,DPIWG,DPIHT,DPIVT,DPIVF,DPIBW,DPIBH,DPIBV,
     A         DPIBWH,DPIBWV,DPITOT,DPIPWR,DPIDWH,LIST,LEXTRP
      LOGICAL TEST
      LOGICAL  EXPSWT,SSAVE,NONE
      LOGICAL  LOGCOM(10), LEQV(81)
      INTEGER  DIM, IDCSE, KASE(6)
      INTEGER EXTRAP
      EQUIVALENCE (BD(81),ALPCA)
      EQUIVALENCE (LEQV(1),IERR)
      EQUIVALENCE (LOGCOM(1),FLTC)
C
      DIMENSION EXTRAP(12)
      DIMENSION XTYPE(4),NUM(10),IDIM(3),IDIMT(8),NMLIST(8)
      DIMENSION NCHR(23),NLOC(23),NK(24)
      DIMENSION KOL(80),ISAVE(4),NEXT(9),KEY(138),ITRIM(4),IDAMP(4)
      DIMENSION IBUILD(5),INACA(4),NACAT(4),IDERD(9),IDERR(9)
      DIMENSION NAMORD(300),IPART(4)
      DIMENSION IDMP(8),IDMC(4),IPLOT(4)
      DIMENSION KEYDP(224),  NDP(75)
      DIMENSION KEYARY(136), NDPARY(48)
      DIMENSION KEYINP(47),  NDPINP(13)
      DIMENSION KEYIOM(41),  NDPIOM(14)
      EQUIVALENCE (KEYDP( 1 ),KEYARY(1)), (NDP(1 ),NDPARY(1))
      EQUIVALENCE (KEYDP(137),KEYINP(1)), (NDP(49),NDPINP(1))
      EQUIVALENCE (KEYDP(184),KEYIOM(1)), (NDP(62),NDPIOM(1))
      DATA XTYPE  /4HSTRA,4HDOUB,4HCRAN,4HCURV/
      DATA NCHR   /2*6,4,9*6,5,2*6,4,7*6/
      DATA NK     /24*0/
      DATA NLOC   /4,10,16,20,26,32,38,44,50,56,62,68,74,79,85,91,97,
     1             103,109,115,121,127,133 /
      DATA NNAM   /23/
C
      DATA EXTRAP /4HP   ,4HR   ,4HI   ,4HN   ,4HT   ,4H    ,
     1             4HE   ,4HX   ,4HT   ,4HR   ,4HA   ,4HP   /
      DATA IBUILD /4HB   ,4HU   ,4HI   ,4HL   ,4HD   /
      DATA INACA  /4HN   ,4HA   ,4HC   ,4HA   /
      DATA NACAT  /4HW   ,4HH   ,4HV   ,4HF   /
      DATA IDERD  /4HD   ,4HE   ,4HR   ,4HI   ,4HV   ,4H    ,
     1             4HD   ,4HE   ,4HG   /
      DATA IDERR  /4HD   ,4HE   ,4HR   ,4HI   ,4HV   ,4H    ,
     1             4HR   ,4HA   ,4HD   /
      DATA IPART  /4HP   ,4HA   ,4HR   ,4HT   /
      DATA IDIM   /4HD   ,4HI   ,4HM    /
      DATA IDIMT  /4HF   ,4HT   ,4HI   ,4HN   ,4HM   ,4H    ,
     1             4HC   ,4HM   /
      DATA NMLIST /4HN   ,4HA   ,4HM   ,4HE   ,4HL   ,
     1             4HI   ,4HS   ,4HT   /
      DATA KASE   /4HC   ,4HA   ,4HS   ,4HE   ,4HI   ,4HD   /
      DATA ISAVE  /4HS   ,4HA   ,4HV   ,4HE    /
      DATA NEXT   /4HN   ,4HE   ,4HX   ,4HT   ,4H    ,4HC   ,4HA   ,
     1             4HS   ,4HE   /
      DATA IDMP   /4HD   ,4HU   ,4HM   ,4HP   ,4H    ,4HA   ,4HL   ,
     1             4HL   /
      DATA IDMC   /4HC   ,4HA   ,4HS   ,4HE   /
      DATA IPLOT  /4HP   ,4HL   ,4HO   ,4HT   /
      DATA IBLNK  /4H    /,KOMMA /4H,   /
      DATA ITRIM  /4HT   ,4HR   ,4HI   ,4HM   /
      DATA IDAMP  /4HD   ,4HA   ,4HM   ,4HP   /
      DATA NUM    /4H0   ,4H1   ,4H2   ,4H3   ,4H4   ,4H5   ,4H6   ,
     1             4H7   ,4H8   ,4H9   /
      DATA KEYARY  /
     1       4HB   ,4HA   ,4HB   ,4HD   ,4HA   ,4HV   ,4HF   ,4HF   ,
     2       4HA   ,4HC   ,4HT   ,4HW   ,4HB   ,4HT   ,4HB   ,4HH   ,
     3       4HT   ,4HA   ,4HV   ,4HT   ,4HA   ,4HH   ,4HT   ,4HC   ,
     4       4HD   ,4HW   ,4HB   ,4HC   ,4HH   ,4HT   ,4HD   ,4HH   ,
     5       4HT   ,4HD   ,4HY   ,4HN   ,4HH   ,4HD   ,4HV   ,4HT   ,
     6       4HG   ,4HR   ,4HL   ,4HB   ,4HP   ,4HW   ,4HS   ,4HT   ,
     7       4HB   ,4HS   ,4HB   ,4HD   ,4HS   ,4HL   ,4HG   ,4HS   ,
     8       4HW   ,4HB   ,4HS   ,4HT   ,4HP   ,4HD   ,4HW   ,4HA   ,
     9       4HS   ,4HT   ,4HG   ,4HS   ,4HL   ,4HA   ,4HT   ,4HR   ,
     A       4HA   ,4HX   ,4HP   ,4HD   ,4HD   ,4HV   ,4HF   ,4HF   ,
     B       4HL   ,4HP   ,4HF   ,4HH   ,4HG   ,4HF   ,4HC   ,4HM   ,
     C       4HT   ,4HC   ,4HD   ,4HF   ,4HL   ,4HA   ,4HT   ,4HR   ,
     D       4HM   ,4HS   ,4HP   ,4HR   ,4HT   ,4HR   ,4HN   ,4HT   ,
     E       4HR   ,4HM   ,4H2   ,4HH   ,4HY   ,4HP   ,4HD   ,4HY   ,
     F       4HN   ,4HJ   ,4HE   ,4HT   ,4HH   ,4HB   ,4HS   ,4HH   ,
     G       4HB   ,4HT   ,4HR   ,4HA   ,4HH   ,4HS   ,4HT   ,4HB   ,
     H       4HH   ,4HS   ,4HE   ,4HC   ,4HS   ,4HL   ,4HA   ,4HH   /
      DATA NDPARY / 1,1,2,3,4,4*3,2*1,2,2*3,4,3,3*2,19*3,4,3*3,2,3,
     1              4,4,3,4 /
C
      DATA KEYINP  /
     1       4HI   ,4HN   ,4HP   ,4HT   ,4HF   ,4HL   ,4HC   ,4HO   ,
     2       4HP   ,4HT   ,4HN   ,4HS   ,4HY   ,4HN   ,4HA   ,4HB   ,
     3       4HD   ,4HI   ,4HN   ,4HW   ,4HG   ,4HI   ,4HN   ,4HV   ,
     4       4HT   ,4HI   ,4HN   ,4HT   ,4HV   ,4HT   ,4HV   ,4HF   ,
     5       4HI   ,4HN   ,4HH   ,4HT   ,4HI   ,4HN   ,4HP   ,4HW   ,
     6       4HI   ,4HN   ,4HL   ,4HB   ,4HI   ,4HN   ,4HF   /
      DATA NDPINP / 4,3,5*4,3,4*4,1 /
C
      DATA KEYIOM  /
     1       4HI   ,4HO   ,4HM   ,4HB   ,4HO   ,4HD   ,4HY   ,4HW   ,
     2       4HI   ,4HN   ,4HG   ,4HH   ,4HT   ,4HV   ,4HT   ,4HV   ,
     3       4HF   ,4HB   ,4HW   ,4HB   ,4HH   ,4HB   ,4HV   ,4HB   ,
     4       4HW   ,4HH   ,4HB   ,4HW   ,4HV   ,4HB   ,4HW   ,4HH   ,
     5       4HV   ,4HP   ,4HO   ,4HW   ,4HR   ,4HD   ,4HW   ,4HS   ,
     6       4HH   /
      DATA NDPIOM / 3,4,4,6*2,3,3,4,4,4 /
      DATA KEY     /
     1       4HE   ,4HN   ,4HD   ,4HF   ,4HL   ,4HT   ,4HC   ,4HO   ,
     2       4HN   ,4HO   ,4HP   ,4HT   ,4HI   ,4HN   ,4HS   ,4HB   ,
     3       4HO   ,4HD   ,4HY   ,4HW   ,4HG   ,4HP   ,4HL   ,4HN   ,
     4       4HF   ,4HW   ,4HG   ,4HS   ,4HC   ,4HH   ,4HR   ,4HS   ,
     5       4HY   ,4HN   ,4HT   ,4HH   ,4HS   ,4HH   ,4HT   ,4HP   ,
     6       4HL   ,4HN   ,4HF   ,4HH   ,4HT   ,4HS   ,4HC   ,4HH   ,
     7       4HR   ,4HV   ,4HT   ,4HP   ,4HL   ,4HN   ,4HF   ,4HV   ,
     8       4HT   ,4HS   ,4HC   ,4HH   ,4HR   ,4HP   ,4HR   ,4HO   ,
     9       4HP   ,4HW   ,4HR   ,4HJ   ,4HE   ,4HT   ,4HP   ,4HW   ,
     A       4HR   ,4HL   ,4HA   ,4HR   ,4HW   ,4HB   ,4HG   ,4HR   ,
     B       4HN   ,4HD   ,4HE   ,4HF   ,4HT   ,4HV   ,4HT   ,4HP   ,
     C       4HA   ,4HN   ,4HE   ,4HX   ,4HP   ,4HR   ,4H    ,4H    ,
     D       4HS   ,4HY   ,4HM   ,4HF   ,4HL   ,4HP   ,4HA   ,4HS   ,
     E       4HY   ,4HF   ,4HL   ,4HP   ,4HH   ,4HY   ,4HP   ,4HE   ,
     F       4HF   ,4HF   ,4HT   ,4HR   ,4HN   ,4HJ   ,4HE   ,4HT   ,
     G       4HV   ,4HF   ,4HP   ,4HL   ,4HN   ,4HF   ,4HV   ,4HF   ,
     H       4HS   ,4HC   ,4HH   ,4HR   ,4HC   ,4HO   ,4HN   ,4HT   ,
     I       4HA   ,4HB   /
C
C     ----TEST FOR NEXT CASE OR DUMP REQUEST CARD.
C
      DO 1000 I=1,9
         IF(KOL(I).NE.NEXT(I)) GO TO 1010
 1000 CONTINUE
      GO TO 1450
C
 1010 DO 1020 I=1,8
         IF(KOL(I).NE.IDMP(I)) GO TO 1040
 1020 CONTINUE
C
C     ----TEST TO BE SURE THE REST OF THE CARD IS BLANK
C
      DO 1030 I=9,80
         IF(KOL(I).NE.IBLNK) GO TO 1070
 1030 CONTINUE
      DMPALL=.TRUE.
      GO TO 1430
C
C     ----TEST FOR KEY WORD DUMP
C
 1040 IF(I.LT.6) GO TO 1160
C
C     ----TEST FOR DUMP CASE CARD
C
      J=1
      DO 1050 L=6,9
         IF(KOL(L).NE.IDMC(J)) GO TO 1070
 1050 J=J+1
C
C     ----TEST FOR REST OF CARD BLANK
C
      DO 1060 I=10,80
         IF(KOL(I).NE.IBLNK) GO TO 1070
 1060 CONTINUE
      DMPCSE=.TRUE.
      GO TO 1430
 1070 I=5
 1080 I=I+1
      IF(I.GT.80) GO TO 1430
      IF(KOL(I).EQ.IBLNK.OR.KOL(I).EQ.KOMMA)GO TO 1080
      NS=I
      NCHAR=1
 1090 I=I+1
      IF(I.GT.80)GO TO 1100
      IF(KOL(I).EQ.IBLNK.OR.KOL(I).EQ.KOMMA) GO TO 1100
      NCHAR=NCHAR+1
      GO TO 1090
 1100 IF(NCHAR.LT.7) GO TO 1120
C
C     ----HERE FOR ERROR
C
 1110 M=I-1
      WRITE(6,1490)(KOL(J),J=1,80),(KOL(J),J=NS,M)
      GO TO 1150
 1120 ISAV=KOL(NS+NCHAR)
      KOL(NS+NCHAR)=IBLNK
      LOCDP = 1
      DO 1130 J=1,75
         IF(TEST(KOL(NS),KEYDP(LOCDP),NDP(J)))GO TO 1140
         LOCDP = LOCDP+NDP(J)
 1130 CONTINUE
      KOL(NS+NCHAR)=ISAV
      GO TO 1110
 1140 IF(J.GT.15)J=J+2
      LEQV(J+4)=.TRUE.
      KOL(NS+NCHAR)=ISAV
 1150 IF(I.GT.80)GO TO 1430
      GO TO 1080
C
C     ----TEST FOR SAVE CARD
C
 1160 DO 1170 I=1,4
         IF(KOL(I).NE.ISAVE(I))GO TO 1180
 1170 CONTINUE
      SAVE=.TRUE.
      GO TO 1430
C
C     ----TEST FOR CASE IDENTIFICATION CARDS
C
 1180 DO 1190 I=1,6
         IF(KOL(I).NE.KASE(I))GO TO 1210
 1190 CONTINUE
C
C     ----STORE IDENTIFICATION
C
      DO 1200 I=7,80
 1200 IDCSE(I-6)=KOL(I)
      HEAD=.TRUE.
      GO TO 1430
 1210 DO 1220 I=1,4
         IF(KOL(I).NE.ITRIM(I))GO TO 1230
 1220 CONTINUE
      TRIMC=.TRUE.
      GO TO 1430
 1230 DO 1240 I=1,4
         IF(KOL(I).NE.IDAMP(I))GO TO 1250
 1240 CONTINUE
      DAMP=.TRUE.
      GO TO 1430
 1250 DO 1260 I=1,4
         IF(KOL(I) .NE. IPART(I)) GO TO 1270
 1260 CONTINUE
      PART=.TRUE.
      GO TO 1430
C
C***  TEST FOR - PRINT EXTRAP CARD
C
 1270 DO 1280 I=1,12
         IF(KOL(I) .NE. EXTRAP(I)) GO TO 1290
 1280 CONTINUE
      LEXTRP=.TRUE.
      GO TO 1430
 1290 DO 1300 I=1,5
         IF(KOL(I) .NE. IBUILD(I)) GO TO 1310
 1300 CONTINUE
      BUILD=.TRUE.
      GO TO 1430
 1310 DO 1320 I=1,4
         IF(KOL(I) .NE. INACA(I)) GO TO 1330
 1320 CONTINUE
      I=24
      GO TO 1440
 1330 DO 1340 I=1,9
         IF(KOL(I) .NE. IDERD(I)) GO TO 1350
 1340 CONTINUE
      DRCONV = .FALSE.
      GO TO 1430
 1350 DO 1360 I=1,9
         IF(KOL(I) .NE. IDERR(I)) GO TO 1370
 1360 CONTINUE
      DRCONV = .TRUE.
      GO TO 1430
 1370 CONTINUE
      DO 1380 I=1,3
         IF(KOL(I) .NE. IDIM(I)) GO TO 1390
 1380 CONTINUE
      DIM = 0
      IF((KOL(5) .EQ. IDIMT(1)) .AND. (KOL(6) .EQ. IDIMT(2))) DIM = 1
      IF((KOL(5) .EQ. IDIMT(3)) .AND. (KOL(6) .EQ. IDIMT(4))) DIM = 2
      IF((KOL(5) .EQ. IDIMT(5)) .AND. (KOL(6) .EQ. IDIMT(6))) DIM = 3
      IF((KOL(5) .EQ. IDIMT(7)) .AND. (KOL(6) .EQ. IDIMT(8))) DIM = 4
      IF(SSAVE .AND. (ISDIM .NE. DIM)) WRITE(6,1470)
      IF(DIM .NE. 0) GO TO 1430
      GO TO 1410
 1390 CONTINUE
      DO 1400 I=1,8
         IF(KOL(I) .NE. NMLIST(I)) GO TO 1410
 1400 CONTINUE
      LIST = .TRUE.
      GO TO 1430
 1410 CONTINUE
      DO 1420 I=1,4
         IF(KOL(I) .NE. IPLOT(I)) GO TO 1460
 1420 CONTINUE
      PLOT = .TRUE.
 1430 JRETRN=1
      RETURN
 1440 JRETRN=2
      RETURN
 1450 JRETRN=3
      RETURN
 1460 CONTINUE
      GONOGO = .FALSE.
      WRITE(6,1480)(KOL(I),I=1,80)
      JRETRN=4
      RETURN
C
 1470 FORMAT(41H0 *** WARNING *** THIS IS A SAVE CASE AND,
     1       38H THE DIMENSION SYSTEM HAS BEEN CHANGED/)
 1480 FORMAT(20H ERROR IN CARD BELOW/1H ,80A1)
 1490 FORMAT(21H ERROR-THE CARD BELOW/1H ,80A1/53H CONTAINS ILLEGAL VARI
     1ABLE NAME TO BE DUMPED.NAME IS ,30A1)
      END
