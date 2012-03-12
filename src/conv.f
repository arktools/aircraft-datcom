      SUBROUTINE CONV(IDIM,SCALE)
C
C***  THIS SUBROUTINE PERFORMES THREE FUNCTIONS
C***     1. CONVERTS INPUT DIMENSIONS TO FT-LB-SEC SYSTEM
C***     2. SCALES DATA
C***     3. SHIFTS COORDINATE SYSTEM TO X = 0 AT BODY NOSE
C
      COMMON /FLGTCD/ FLC(42), RN(20), NGH, GRDH(10), PINF(20), FLC1(3),
     1                ALT(20), TINF(20), VINF(20), WT
      COMMON /OPTION/ SREF, CBARR, ROUGFC, BLREF
      COMMON /SYNTSS/ SYNA(19)
      COMMON /BODYI/  XNX, X(20), S(20), P(20), R(20), ZU(20),
     1                ZL(20), BTY(2), BL(3)
      COMMON /WINGI/  WGIN(101)
      COMMON /HTI/    HTIN(154)
      COMMON /VTI/    VTIN(154), TVTIN(8), VFIN(154)
      COMMON /POWER/  PWIN(29), LBIN(21)
      COMMON /FLAPIN/ F(138)
C
      COMMON /BDATA/  BD(762)
      COMMON /CONSNT/ PI, DEG, UNUSED, RAD
      COMMON /FLOLOG/ FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1                HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,SUPERS,SUBSON,
     2                TRANSN,HYPERS,SYMFP,ASYFP,TRIMC,TRIM,DAMP,
     3                HYPEF,TRAJET,BUILD,FIRST,DRCONV,PART,
     4                VFPL,VFSC,CTAB
C
      REAL LBIN
      LOGICAL  FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1         HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,SUPERS,SUBSON,
     2         TRANSN,HYPERS,SYMFP,ASYFP,TRIMC,TRIM,DAMP,
     3         HYPEF,TRAJET,BUILD,FIRST,DRCONV,PART,
     4         VFPL,VFSC,CTAB
C
      EQUIVALENCE (BD(11),X0)
C
      DIMENSION IDIMT(8)
      DATA IDIMT / 4HF   ,4HT   ,4HI   ,4HN   ,
     1             4HM   ,4H    ,4HC   ,4HM   /
C
C
C***  SET CONVERSION FACTORS
C
      XL = 1.0
      XA = 1.0
      XR = 1.0
      XP = 1.0
      XT = 1.0
      XW = 1.0
      XF = 1.0
      IF(IDIM .NE. 2) GO TO 1000
        XL = 12.0
        XA = 144.0
        XP = 144.0
 1000 CONTINUE
      IF(IDIM .LT. 3) GO TO 1010
        XL = 0.3048
        XA = 0.09290304
        XR = 0.3048
        XP = 0.0208854
        XT = 1.8
        XW = 0.2248089
        XF = 2.54
 1010 CONTINUE
      IF(IDIM .NE. 4) GO TO 1020
        XL = 30.48
        XA = 929.0304
        XP = 208.854
 1020 CONTINUE
      ASCALE = SCALE**2
C
C***  INPUT UNITS CONVERSION AND SCALING
C
      IF((IDIM .EQ. 1) .AND. (SCALE .EQ. 1.0)) GO TO 1160
        DO 1030 I=1,20
          IF(RN(I)   .NE. UNUSED) RN(I)   = RN(I)*XR
          IF(ALT(I)  .NE. UNUSED) ALT(I)  = ALT(I)/XL
          IF(PINF(I) .NE. UNUSED) PINF(I) = PINF(I)*XP
          IF(TINF(I) .NE. UNUSED) TINF(I) = TINF(I)*XT
          IF(VINF(I) .NE. UNUSED) VINF(I) = VINF(I)/XL
C AJT     IF(I .LE. 10 .AND. GRDH(I) .NE. UNUSED) GRDH(I) = GRDH(I)/XL
 1030   CONTINUE
C AJT  GRDH TAKEN OUT OF DO 1030 LOOP TO STOP ARRAY BOUND FAIL
         DO 1031 I=1,10
          GRDH(I) = GRDH(I)/XL
 1031   CONTINUE
        IF(WT .NE. UNUSED) WT = WT*XW
C
        IF(ROUGFC .NE. UNUSED) ROUGFC = ROUGFC/XF
        IF(SREF   .NE. UNUSED) SREF   = SREF*ASCALE/XA
        IF(CBARR  .NE. UNUSED) CBARR  = CBARR*SCALE/XL
        IF(BLREF  .NE. UNUSED) BLREF  = BLREF*SCALE/XL
C
        DO 1040 I=1,19
          IF(I .EQ. 4 ) GO TO 1040
          IF(I .EQ. 8 ) GO TO 1040
          IF(I .EQ. 10) GO TO 1040
          IF(I .EQ. 13) GO TO 1040
          IF(I .EQ. 18) GO TO 1040
          IF(I .EQ. 19) GO TO 1040
          IF(SYNA(I) .EQ. UNUSED) GO TO 1040
            SYNA(I) = SYNA(I)*SCALE/XL
 1040   CONTINUE
        IF(BD(33) .NE. UNUSED) BD(33) = BD(33)*SCALE/XL
        IF(BD(65) .NE. UNUSED) BD(65) = BD(65)*SCALE/XL
        IF(BD(74) .NE. UNUSED) BD(74) = BD(74)*SCALE/XL
        IF(BD(82) .NE. UNUSED) BD(82) = BD(82)*SCALE/XL
C
        DO 1050 I=1,20
          IF(X(I)  .NE. UNUSED) X(I)  = X(I)*SCALE/XL
          IF(S(I)  .NE. UNUSED) S(I)  = S(I)*ASCALE/XA
          IF(P(I)  .NE. UNUSED) P(I)  = P(I)*SCALE/XL
          IF(R(I)  .NE. UNUSED) R(I)  = R(I)*SCALE/XL
          IF(ZU(I) .NE. UNUSED) ZU(I) = ZU(I)*SCALE/XL
          IF(ZL(I) .NE. UNUSED) ZL(I) = ZL(I)*SCALE/XL
C AJT     IF((I .LE. 3) .AND. (BL(I) .NE. UNUSED))
C AJT     1                          BL(I) = BL(I)*SCALE/XL
 1050   CONTINUE
C AJT  BL TAKEN OUT OF DO 1030 LOOP TO STOP ARRAY BOUND FAIL
        DO 1051 I=1,3
            BL(I) = BL(I)*SCALE/XL
 1051   CONTINUE 
C
        DO 1060 I=1,6
          IF(WGIN(I) .NE. UNUSED) WGIN(I) = WGIN(I)*SCALE/XL
          IF(HTIN(I) .NE. UNUSED) HTIN(I) = HTIN(I)*SCALE/XL
          IF(VTIN(I) .NE. UNUSED) VTIN(I) = VTIN(I)*SCALE/XL
          IF(VFIN(I) .NE. UNUSED) VFIN(I) = VFIN(I)*SCALE/XL
 1060   CONTINUE
        IF(WGIN(12) .NE. UNUSED) WGIN(12) = WGIN(12)*SCALE/XL
        IF(HTIN(12) .NE. UNUSED) HTIN(12) = HTIN(12)*SCALE/XL
        IF(VTIN(12) .NE. UNUSED) VTIN(12) = VTIN(12)*SCALE/XL
        IF(VFIN(12) .NE. UNUSED) VFIN(12) = VFIN(12)*SCALE/XL
        DO 1070 I=95,154
          IF(VTIN(I) .NE. UNUSED) VTIN(I) = VTIN(I)*ASCALE/XA
          IF(VFIN(I) .NE. UNUSED) VFIN(I) = VFIN(I)*ASCALE/XA
          IF((I .GE. 115) .AND. (HTIN(I) .NE. UNUSED))
     1                            HTIN(I) = HTIN(I)*ASCALE/XA
          IF((I .LE. 114) .AND. (HTIN(I) .NE. UNUSED))
     1                            HTIN(I) = HTIN(I)*SCALE/XL
 1070   CONTINUE
C
        IF(PWIN(4)  .NE. UNUSED) PWIN(4)  = PWIN(4)*SCALE/XL
        IF(PWIN(5)  .NE. UNUSED) PWIN(5)  = PWIN(5)*SCALE/XL
        IF(PWIN(6)  .NE. UNUSED) PWIN(6)  = PWIN(6)*SCALE/XL
        IF(PWIN(8)  .NE. UNUSED) PWIN(8)  = PWIN(8)*SCALE/XL
        IF(PWIN(9)  .NE. UNUSED) PWIN(9)  = PWIN(9)*SCALE/XL
        IF(PWIN(10) .NE. UNUSED) PWIN(10) = PWIN(10)*SCALE/XL
        IF(PWIN(16) .NE. UNUSED) PWIN(16) = PWIN(16)*SCALE/XL
        IF(PWIN(17) .NE. UNUSED) PWIN(17) = PWIN(17)*SCALE/XL
        IF(PWIN(18) .NE. UNUSED) PWIN(18) = PWIN(18)*SCALE/XL
        IF(PWIN(19) .NE. UNUSED) PWIN(19) = PWIN(19)*ASCALE/XA
        IF(PWIN(21) .NE. UNUSED) PWIN(21) = PWIN(21)/XL
        IF(PWIN(22) .NE. UNUSED) PWIN(22) = PWIN(22)*XT
        IF(PWIN(23) .NE. UNUSED) PWIN(23) = PWIN(23)*XT
        IF(PWIN(24) .NE. UNUSED) PWIN(24) = PWIN(24)*SCALE/XL
        IF(PWIN(25) .NE. UNUSED) PWIN(25) = PWIN(25)*XP
        IF(PWIN(26) .NE. UNUSED) PWIN(26) = PWIN(26)*XP
        IF(PWIN(27) .NE. UNUSED) PWIN(27) = PWIN(27)*SCALE/XL
        IF(PWIN(28) .NE. UNUSED) PWIN(28) = PWIN(28)*SCALE/XL
C
        DO 1080 I=1,4
          IF(TVTIN(I) .NE. UNUSED) TVTIN(I) = TVTIN(I)*SCALE/XL
 1080   CONTINUE
        IF(TVTIN(5) .NE. UNUSED) TVTIN(5) = TVTIN(5)*ASCALE/XA
        IF(TVTIN(7) .NE. UNUSED) TVTIN(7) = TVTIN(7)*SCALE/XL
        IF(TVTIN(8) .NE. UNUSED) TVTIN(8) = TVTIN(8)*SCALE/XL
C
        IF(.NOT. SYMFP) GO TO 1100
          DO 1090 I=12,125
            IF((I .GE. 16)  .AND. (I .LE. 38))  GO TO 1090
            IF((I .GE. 61)  .AND. (I .LE. 84))  GO TO 1090
            IF((I .GE. 105) .AND. (I .LE. 114)) GO TO 1090
            IF( I .EQ. 117)                     GO TO 1090
            IF(F(I) .NE. UNUSED) F(I) = F(I)*SCALE/XL
 1090     CONTINUE
          IF(F(133) .NE. UNUSED) F(133) = F(133)*XL
          IF(F(134) .NE. UNUSED) F(134) = F(134)*XL
          IF(F(135) .NE. UNUSED) F(135) = F(135)*XP
 1100   CONTINUE
        IF(.NOT. ASYFP) GO TO 1120
          DO 1110 I=12,15
            IF(F(I) .NE. UNUSED) F(I) = F(I)*SCALE/XL
 1110     CONTINUE
 1120   CONTINUE
        IF(.NOT. TRAJET) GO TO 1140
          DO 1130 I=12,21
            IF(F(I) .NE. UNUSED) F(I) = F(I)*XW
 1130     CONTINUE
          IF(F(34) .NE. UNUSED) F(34) = F(34)/XL
          IF(F(38) .NE. UNUSED) F(38) = F(38)/XL
 1140   CONTINUE
        IF(.NOT. HYPEF) GO TO 1150
          IF(F(1) .NE. UNUSED) F(1) = F(1)/XL
          IF(F(2) .NE. UNUSED) F(2) = F(2)/XL
          IF(F(4) .NE. UNUSED) F(4) = F(4)/XL
 1150   CONTINUE
C
        IF(LBIN(2)  .NE. UNUSED) LBIN(2)  = LBIN(2)/XA
        IF(LBIN(4)  .NE. UNUSED) LBIN(4)  = LBIN(4)/XA
        IF(LBIN(9)  .NE. UNUSED) LBIN(9)  = LBIN(9)/XA
        IF(LBIN(11) .NE. UNUSED) LBIN(11) = LBIN(11)/XA
        IF(LBIN(18) .NE. UNUSED) LBIN(18) = LBIN(18)/XA
        IF(LBIN(19) .NE. UNUSED) LBIN(19) = LBIN(19)/XA
        IF(LBIN(1)  .NE. UNUSED) LBIN(1)  = LBIN(1)/XL
        IF(LBIN(8)  .NE. UNUSED) LBIN(8)  = LBIN(8)/XL
        IF(LBIN(10) .NE. UNUSED) LBIN(10) = LBIN(10)/XL
        IF(LBIN(12) .NE. UNUSED) LBIN(12) = LBIN(12)/XL
        IF(LBIN(13) .NE. UNUSED) LBIN(13) = LBIN(13)/XL
        IF(LBIN(15) .NE. UNUSED) LBIN(15) = LBIN(15)/XL
        IF(LBIN(20) .NE. UNUSED) LBIN(20) = LBIN(20)/XL
        IF(LBIN(21) .NE. UNUSED) LBIN(21) = LBIN(21)/XL
 1160 CONTINUE
C
C***  SET NOSE LOCATION
C
      X0 = X(1)
      IF(ABS(X0) .LE. UNUSED) GO TO 1180
        NX = XNX+0.5
        DO 1170 I=1,NX
          X(I) = X(I)-X0
 1170   CONTINUE
        SYNA(1)  = SYNA(1) -X0
        SYNA(2)  = SYNA(2) -X0
        SYNA(6)  = SYNA(6) -X0
        SYNA(9)  = SYNA(9) -X0
        IF(SYNA(11) .NE. UNUSED) SYNA(11) = SYNA(11)-X0
        IF(SYNA(12) .NE. UNUSED) SYNA(12) = SYNA(12)-X0
        PWIN(4)  = PWIN(4) -X0
        PWIN(16) = PWIN(16)-X0
        PWIN(18) = PWIN(18)-X0
        BD(33)   = BD(33)  -X0
        BD(65)   = BD(65)  -X0
 1180 CONTINUE
      I1 = 1+2*(IDIM-1)
      I2 = I1+1
      WRITE(6,1190) (IDIMT(I),I=I1,I2), SCALE
      RETURN
 1190 FORMAT(1H0,25H INPUT DIMENSIONS ARE IN ,2A1,
     1           18H, SCALE FACTOR IS ,F6.4/)
      END
