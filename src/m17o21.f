      SUBROUTINE M17O21
C
C***  EXEC FOR OVERLAY 17, SUBSONIC LATERAL STABILITY
C
      COMMON /IBODY/   PBODY,  BODY(400)
      COMMON /IWING/   PWING,  WING(400)
      COMMON /IHT/     PHT,    HT(380)
      COMMON /IVT/     PVT,    VT(380)
      COMMON /IVF/     PVF,    VF(380)
      COMMON /IBW/     PBW,    BW(380)
      COMMON /IBH/     PBH,    BH(380)
      COMMON /IBV/     PBV,    BV(380)
      COMMON /IBWH/    PBWH,   BWH(380)
      COMMON /IBWV/    PBWV,   BWV(380)
      COMMON /IBWHV/   PBWHV,  BWHV(380)
C
      COMMON /WINGI/  WINGIN(101)
      COMMON /VTI/    VTIN(154), TVTIN(8), VFIN(154)
      COMMON /HTI/    HTIN(154)
      COMMON /WINGD/  A(195), B(49)
      COMMON /SBETA/  STB(135), TRA(108), TRAH(108), STBH(135)
      COMMON /HTDATA/ AHT(195), BHT(49)
      COMMON /VTDATA/ AVT(195), AVF(195)
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD,KAND
      COMMON /OVERLY/ NLOG,NMACH,I,NALPHA,IG,NF,LF,K,NOVLY
      COMMON /FLOLOG/ FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1                HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,SUPERS,SUBSON,
     2                TRANSN,HYPERS,SYMFP,ASYFP,TRIMC,TRIM,DAMP,
     3                HYPEF,TRAJET,BUILD,FIRST,DRCONV,PART,
     4                VFPL,VFSC,CTAB
C
      LOGICAL  FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC,
     1         HEAD,PRPOWR,JETPOW,LOASRT,TVTPAN,SUPERS,SUBSON,
     2         TRANSN,HYPERS,SYMFP,ASYFP,TRIMC,TRIM,DAMP,
     3         HYPEF,TRAJET,BUILD,FIRST,DRCONV,PART,
     4         VFPL,VFSC,CTAB
      LOGICAL  FLAG, TEST
C
      NOVLY=17
      FLAG = .FALSE.
      TEST = WGPL .OR. VTPL .OR. TVTPAN
      IF(TEST) CALL SUBLAT(STB,WING,WINGIN,BW,A,B,AHT,
     1                     AVT,VT,VTIN,VTPL,TVTPAN,HTPL,WGPL,0)
      TEST = HTPL .OR. VFPL
      IF(TEST) CALL SUBLAT(STBH,HT,HTIN,BH,AHT,BHT,AHT,
     1                     AVF,VF,VFIN,VFPL,FLAG,FLAG,HTPL,1)
C
C     SET IOM VALUES FOR CYB, CNB, AND CLB
C
      DO 1050 J=1,NALPHA
C
C     VT DATA
C
         IF(J .EQ. 1) GO TO 1000
         VT(J+140) = -UNUSED
         VT(J+160) = -UNUSED
         VF(J+140) = -UNUSED
         VF(J+160) = -UNUSED
C
C     B-W  DATA
C
         BW(J+140) = -UNUSED
         BW(J+160) = -UNUSED
C
C     B-H  DATA
C
         BH(J+140) = -UNUSED
         BH(J+160) = -UNUSED
 1000    CONTINUE
C
C     B-V  DATA
C
         BV(J+140) = -UNUSED
         BV(J+160) = -UNUSED
         IF(J .GT. 1) GO TO 1010
         BV(J+140) = BODY(J+140) + VT(J+140) + VF(J+140)
         BV(J+160) = BODY(J+160) + VT(J+160) + VF(J+160)
 1010    CONTINUE
         BV(J+180) = BODY(J+180) + VT(J+180) + VF(J+180)
C
C     B-W-H  DATA
C
         BWH(J+140) = -UNUSED
         BWH(J+160) = -UNUSED
         IF(J .GT. 1) GO TO 1020
         BWH(J+140) = BW(J+140) + HT(J+140)
         BWH(J+160) = BW(J+160) + HT(J+160)
 1020    BWH(J+180) = BW(J+180) + HT(J+180)
C
C     B-W-V  DATA
C
         BWV(J+140) = -UNUSED
         BWV(J+160) = -UNUSED
         IF(J .GT. 1) GO TO 1030
         BWV(J+140) = BW(J+140) + VT(J+140) + VF(J+140)
         BWV(J+160) = BW(J+160) + VT(J+160) + VF(J+160)
 1030    CONTINUE
         BWV(J+180) = BW(J+180) + VT(J+180) + VF(J+180)
C
C     B-W-H-V  DATA
C
         BWHV(J+140) = -UNUSED
         BWHV(J+160) = -UNUSED
         IF(J .GT. 1) GO TO 1040
         BWHV(J+140) = BWH(J+140) + VT(J+140) + VF(J+140)
         BWHV(J+160) = BWH(J+160) + VT(J+160) + VF(J+160)
 1040    CONTINUE
         BWHV(J+180) = BWH(J+180) + VT(J+180) + VF(J+180)
 1050 CONTINUE
      IF(.NOT. TRANSN) GO TO 1070
      VT(141) = UNUSED
      VT(161) = UNUSED
      VF(141) = UNUSED
      VF(161) = UNUSED
      BV(141) = UNUSED
      BV(161) = UNUSED
      BWH(141) = UNUSED
      BWH(161) = UNUSED
      BWHV(141) = UNUSED
      BWHV(161) = UNUSED
      WING(141) = UNUSED
      WING(161) = UNUSED
      HT(141) = UNUSED
      HT(161) = UNUSED
      DO 1060 J=1,NALPHA
         BV(J+180) = UNUSED
         BWH(J+180) = UNUSED
         BWV(J+180) = UNUSED
         BWHV(J+180) = UNUSED
 1060 CONTINUE
 1070 CONTINUE
      RETURN
      END
