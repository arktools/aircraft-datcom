      SUBROUTINE SETUP2
C
C***  SETUP FOR TRANSONIC CONFIGURATION ANALYSIS
C
      COMMON /IWING/  PWNG, WING(400)
      COMMON /IHT/    PHT,  HT(380)
      COMMON /IBW/    PBW,  BW(380)
      COMMON /IBH/    PBH,  BH(380)
      COMMON /FLGTCD/ FLC(96)
      COMMON /WINGI/  WINGIN(100)
      COMMON /HTI/    HTIN(154)
      COMMON /WINGD/  A(195), B(49)
      COMMON /SBETA/  STB(135), TRA(108), TRAH(108), STBH(135)
      COMMON /HTDATA/ AHT(195), BHT(49)
      COMMON /WBHCAL/ WBT(155)
      COMMON /LEVEL2/ SECOND(23)
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD
      COMMON /OVERLY/ NLOG,NMACH,I,NALPHA,IG,NF
      COMMON /FLOLOG/ FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1                HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,SUPERS,SUBSON,
     2                TRANSN,HYPERS,SYMFP,ASYFP,TRIMC,TRIM,DAMP,
     3                HYPEF,TRAJET,BUILD,FIRST,DRCONV,PART
C
      LOGICAL  FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1         HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,SUPERS,SUBSON,
     2         TRANSN,HYPERS,SYMFP,ASYFP,TRIMC,TRIM,DAMP,
     3         HYPEF,TRAJET,BUILD,FIRST,DRCONV,PART
      LOGICAL FLAG, DONE
      REAL MFBW, MFBH
      DIMENSION STP(155)
      EQUIVALENCE (TRA(1),FLAG), (SECOND(15),DONE)
      EQUIVALENCE (STP(1),WBT(1)), (SECOND(17),SMACH)
      EQUIVALENCE (SECOND(18),MFBW), (SECOND(19),MFBH)
C
 1000 N = -NF
      GO TO (1010, 1020, 1030, 1040, 1050, 1060, 1070), N
 1010 CONTINUE
        SMACH  = FLC(I+2)
        SUBSON = .TRUE.
        TRANSN = .FALSE.
        MFBW = TRA(6)
        MFBH = TRAH(6)
        FLC(I+2) = 0.6
        B(1) = 0.6
        B(2) = 0.8
        BHT(1) = B(1)
        BHT(2) = B(2)
        WINGIN(I+20) = WINGIN(69)/B(2)
        WINGIN(I+40) = WINGIN(68)
        HTIN(I+20) = HTIN(69)/B(2)
        HTIN(I+40) = HTIN(68)
        FLAG = WGPL .OR. HTPL
        GO TO 1090
 1020 CONTINUE
        SECOND(11) = WBT(67)
        CALL CLBCLC(SECOND(1),WING)
        CALL CLBCLC(SECOND(3),HT)
        FLC(I+2) = 0.7
        B(1) = 0.7
        B(2) = 0.71414284
        BHT(1) = B(1)
        BHT(2) = B(2)
        WINGIN(I+20) = WINGIN(69)/B(2)
        HTIN(I+20) = HTIN(69)/B(2)
        FLAG = BO .AND. WGPL .AND. HTPL
        GO TO 1090
 1030 CONTINUE
        SECOND(12) = WBT(67)
        IF(MFBW .GT. 0.95) MFBW = 0.95
        FLC(I+2) = MFBW
        B(1) = MFBW
        B(2) = SQRT(1.-B(1)**2)
        BHT(1) = B(1)
        BHT(2) = B(2)
        WINGIN(I+20) = WINGIN(69)/B(2)
        HTIN(I+20) = HTIN(69)/B(2)
        FLAG = BO .AND. WGPL
        GO TO 1090
 1040 CONTINUE
        CALL CLBCLC(SECOND(5),BW)
        IF(MFBH .GT. 0.95) MFBH = 0.95
        FLC(I+2) = MFBH
        B(1) = MFBH
        B(2) = SQRT(1.-B(1)**2)
        BHT(1) = B(1)
        BHT(2) = B(2)
        WINGIN(I+20) = WINGIN(69)/B(2)
        HTIN(I+20) = HTIN(69)/B(2)
        FLAG = BO .AND. HTPL
        GO TO 1090
 1050 CONTINUE
        CALL CLBCLC(SECOND(7),BH)
        SUBSON = .FALSE.
        SUPERS = .TRUE.
        FLC(I+2) = 1.4
        FLAG = WGPL .OR. HTPL
        GO TO 1090
 1060 CONTINUE
        CALL CLBCLC(SECOND(2),WING)
        CALL CLBCLC(SECOND(4),HT)
        CALL CLBCLC(SECOND(6),BW)
        CALL CLBCLC(SECOND(8),BH)
        SECOND(9) = BW(101)
        SECOND(10) = BH(101)
        SECOND(14) = STP(155)
        FLC(I+2) = 1.1
        FLAG = BO .AND. WGPL .AND. HTPL
        GO TO 1090
 1070 CONTINUE
        SECOND(13) = STP(155)
        FLC(I+2) = SMACH
        NALPHA   = FLC(2)+0.5
        DONE   = .TRUE.
        SUPERS = .FALSE.
        TRANSN = .TRUE.
        IF(.NOT. WGPL) SECOND(1) = UNUSED
        IF(.NOT. WGPL) SECOND(2) = UNUSED
        IF(.NOT. HTPL) SECOND(3) = UNUSED
        IF(.NOT. HTPL) SECOND(4) = UNUSED
        IF(.NOT. (BO .AND. WGPL)) SECOND(5) = UNUSED
        IF(.NOT. (BO .AND. WGPL)) SECOND(6) = UNUSED
        IF(.NOT. (BO .AND. WGPL)) SECOND(9) = UNUSED
        IF(.NOT. (BO .AND. HTPL)) SECOND(7) = UNUSED
        IF(.NOT. (BO .AND. HTPL)) SECOND(8) = UNUSED
        IF(.NOT. (BO .AND. HTPL)) SECOND(10)= UNUSED
        FLAG = BO .AND. WGPL .AND. HTPL
        IF(.NOT. FLAG) SECOND(11) = UNUSED
        IF(.NOT. FLAG) SECOND(12) = UNUSED
        IF(.NOT. FLAG) SECOND(13) = UNUSED
        IF(.NOT. FLAG) SECOND(14) = UNUSED
        DO 1080 J=17,23
           SECOND(J) = UNUSED
 1080   CONTINUE
        FLAG = .TRUE.
 1090 CONTINUE
      NF = NF-1
      IF(.NOT. FLAG) GO TO 1000
      RETURN
      END
