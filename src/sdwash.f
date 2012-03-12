      SUBROUTINE SDWASH(NZ,KEPSLN)
C
C***  COMPUTES DE/DA AND VISCOUS Q/QINF AT THE HORIZONTAL TAIL
C
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD
      COMMON /OPTION/ SREF,CBARRW,ROUGFC,BLREF
      COMMON /OVERLY/ NLOG,NMACH,IMACH,NALPHA,IG,NF
      COMMON /SYNTSS/ XCG,XW,ZW,ALIW,ZCG,XH,ZH,ALIH,XV,VERTUP,HINAX,
     1                XVF,SCALE,ZV,ZVF,YV,YF,PHIV,PHIF
      COMMON /FLGTCD/ FLC(160)
      COMMON /WINGD/  A(195)
      COMMON /HTDATA/ AHT(195)
      COMMON /SUPDW/  MACH, BETA, X(2), Y(2), Z(2), ALPHA(20), ZEFF(40),
     1                DHB(2), DUM(38), DEPAVG(20), DEPX(40), CLANL(20),
     2                M(20), ZWAKEC, ZC(20), DELQO, DELE, DELTAZ,
     3                XSUR, THETA, DELTE, THETE, JDETCH
      COMMON /WINGI/  WINGIN(101)
      COMMON /HTI/    HTIN(154)
      COMMON /SUPWH/  SLG(141)
      COMMON /IWING/  PWING, WING(400)
      COMMON /IDWASH/ PDWASH, QQINFY(20), DWANGL(20), DEPDA(20)
C
      DIMENSION TAPER(4), ROUTID(2), CC(6)
      DIMENSION DEP(4), SDW(2), CLW(20), DEPA(21), ALP(21), DWA(20)
C
      EQUIVALENCE (CR,WINGIN(6)), (CT,WINGIN(1))
      EQUIVALENCE (AW,A(120)), (SWEPLE,A(58)), (SWEPC2,A(70))
      EQUIVALENCE (SWEPTE,A(76)), (TAPR,A(118))
      EQUIVALENCE (CLW(1),WING(21)), (CLAW,WING(101))
      EQUIVALENCE (CDOW,SLG(80))
      EQUIVALENCE (RL2,A(24)), (GAMMA,A(11))
      EQUIVALENCE (SPAN,WINGIN(4)),(TANLEH,AHT(62)),(DIHEOH,HTIN(14))
      EQUIVALENCE (SPANH,HTIN(4)),(DIHEIH,HTIN(13)),(SPANDI,HTIN(12))
C
      LOGICAL VISDW, FLAG
      REAL    MACH, M
      DATA ROUTID / 4HSDWA, 4HSH   /
      DATA TAPER  / 0.00, 0.25, 0.50, 1.00 /
C
C***  INITIALIZE DATA
C
      CALL INFTGM
      JDETCH = -1
      MACH   = FLC(NZ+2)
      BETA   = SQRT(MACH**2-1.0)
      ABETA  = AW*BETA
      ARG1   = TAN(SWEPLE/RAD)/BETA
      ARG2   = TAN(SWEPTE/RAD)/BETA
      VISDW  = (ARG1 .GT. 1.0) .OR. (ARG2 .GT. 1.0)
      DO 1000 J=1,NALPHA
        ALPHA(J) = FLC(J+22)+ALIW
        CLANL(J) = WING(J+100)
 1000 CONTINUE
      IF(VISDW .OR. KEPSLN .NE. 0) GO TO 1080
C
      X(1) = (XH-XW)/(SPAN*BETA)
      X(2) = (XH-XW+TANLEH*SPANH)/(SPAN*BETA)
      Y(1) = 0.0
      Y(2) = SPANH/SPAN
      Z(1) = ZH-ZW
      Z(2) = Z(1)+TAN(DIHEIH/RAD)*(SPANH-SPANDI)+TAN(DIHEOH/RAD)*SPANDI
      Z(1) = Z(1)/SPAN
      Z(2) = Z(2)/SPAN
C
C***  SET ICASE AND GET DHB
C
      ICASE = 0
      IF(SWEPTE .EQ. 0.0) ICASE = 1
      IF(SWEPTE .GT. 0.0) ICASE = 4
      IF((SWEPTE .LT. 0.0) .AND. (SWEPC2 .LE. 0.0)) ICASE = 2
      IF((SWEPTE .LT. 0.0) .AND. (SWEPC2 .GT. 0.0)) ICASE = 3
C
      SWEPR = ATAN((CR-CT)/(2.0*SPAN))*RAD
      CALL SDDVC(X, ABETA, SWEPTE, SWEPR, TAPR, TAPER, DHB, ICASE)
C
C***  LOOP ON ALPHA AND LOCATION
C
      N = 0
      DO 1070 J=1,NALPHA
        NA = ABS(2.0*ALPHA(J))+1.5
        IF(NA .LT. 2)  NA = 2
        IF(NA .GT. 21) NA = 21
        XNA = NA-1
        DO 1060 K=1,NA
          IF(NF .GE. 0) NF = -1
          IF((K .EQ. NA) .AND. (NF .EQ. -1)) NF = 0
          ALP(K) = ALPHA(J)*(K-1)/XNA
          DO 1050 I=1,2
            IF(K .EQ. NA) N = N+1
            ZE   = Z(I) + DHB(I)*BETA*ALP(K)/RAD
            IF(K .EQ. NA) ZEFF(N) = ZE
            ZE   = ABS(ZE)
            FLAG = (ICASE .EQ. 1) .OR. (ICASE .EQ. 3)
            IF(.NOT. FLAG) GO TO 1010
              CALL SDWA(X(I), Y(I), ZE, ABETA, DEP(1))
              CALL SDWC(X(I), Y(I), ZE, ABETA, DEP(2))
              CALL SDWB(X(I), Y(I), ZE, ABETA, DEP(4))
              IN = 0
              CALL TBFUNX(TAPR, SDW(I), DYDX, 4, TAPER, DEP, CC,
     1                    IN, MI, NG, 0, 0, 4HSDW1, 1, ROUTID)
              IF(ICASE .EQ. 3) SDW3 = SDW(I)
 1010       CONTINUE
            FLAG = (ICASE .EQ. 2) .OR. (ICASE .EQ. 3)
            IF(.NOT. FLAG) GO TO 1030
              CALL SDWD(X(I), Y(I), ZE, ABETA, DEP(1))
              CALL SDWB(X(I), Y(I), ZE, ABETA, DEP(4))
              IN = 0
              CALL TBFUNX(TAPR, SDW(I), DYDX, 4, TAPER, DEP, CC,
     1                    IN, MI, NG, 0, 0, 4HSDW2, 1, ROUTID)
              IF(ICASE .NE. 3) GO TO 1020
               SDW(I) = SDW(I) + SWEPC2*(SDW3-SDW(I))/SWEPR
 1020         CONTINUE
 1030       CONTINUE
            IF(ICASE .NE. 4) GO TO 1040
              CALL SDWE(X(I), Y(I), ZE, ABETA, TAPR, SDW(I))
 1040       CONTINUE
            IF(K .EQ. NA) DEPX(N) = SDW(I)
 1050     CONTINUE
          IN = 0
          CALL TBFUNX(ALP(K), CLANLJ, DYDX, NALPHA, ALPHA, WING(101),
     1                CC, IN, MI, NG, 0, 0, 4HCLAW, 1, ROUTID)
          DEPA(K) = (SDW(1)+SDW(2))/2.0
          DEPA(K) = DEPA(K)*CLANLJ/CLAW
 1060   CONTINUE
        DEPAVG(J) = DEPA(NA)*CLAW/CLANLJ
        DEPDA(J)  = DEPA(NA)
        CALL TRAPZ(DEPA,NA,ALP,DWANGL(J),1)
 1070 CONTINUE
 1080 CONTINUE
      DO 1110 J=1,NALPHA
        IF(KEPSLN.EQ.0 .AND. VISDW) DWANGL(J) =1.62*CLW(J)/(PI*AW)
     1                                         *RAD*SREF/A(3)
        ARG    = (DWANGL(J)-ALPHA(J))/RAD+GAMMA
        ARG4   = RL2*COS(ARG)/(COS(GAMMA)*A(16))
        ARG1   = CDOW*(ARG4+0.15)*SREF/A(3)
        ZWAKEC = 0.68*SQRT(ARG1)
        ZWAKET = ZWAKEC*A(16)
        DELQO  = 2.42*SQRT(CDOW*SREF/A(3))/(ARG4+0.3)
        ZC(J)  = ARG4*TAN(ARG)
        DWA(J) = DWANGL(J)/RAD
        TEST   = ABS(ZC(J)/ZWAKEC)
        IF(TEST.GT.1.)GO TO 1090
C
C***    CALCULATE VISCOUS DYNAMIC PRESSURE
C
          DELTAJ    = PI*TEST/2.0
          QQINFY(J) = 1.-DELQO*COS(DELTAJ)**2
          M(J)      = MACH
          GO TO 1100
 1090   CONTINUE
C
C***    CALCULATE NON-VISCOUS DYNAMIC PRESSURE
C
          ZTAIL = ZC(J)*A(16)
          CALL DPRESR(ZTAIL,ZWAKET,ALPHA(J),DWA(J),QQINFY(J),M(J))
          IF(M(J).GT.1.0) GO TO 1100
            JDETCH = J-1
            IF(VISDW) GO TO 1120
            RETURN
 1100   CONTINUE
 1110 CONTINUE
 1120 CONTINUE
      IF(VISDW .AND. KEPSLN .EQ. 0 .AND. JDETCH.GT.0) NALPHA = JDETCH
      IF(JDETCH .EQ. 0) NALPHA = 0
      IF(.NOT.VISDW .AND. KEPSLN .EQ. 0)RETURN
C AJT DONT CALL TBFUNX IF JDETCH=NALPHA=0, BUT SET SENSIBLE RESULT  
      IF(JDETCH .EQ. 0) GOTO 1140
      DO 1130 J=1,NALPHA
        IN = 0
        CALL TBFUNX(ALPHA(J), E, DEPDA(J), NALPHA, ALPHA, DWANGL, CC,
     1              IN, MI, NG, 0, 0, 4HDPDA, 1, ROUTID)
 1130 CONTINUE
      RETURN
C AJT SET SENSIBLE RESULT    
 1140 DEPDA(1) = 0
      E = DWANGL(1)
      RETURN
      END
