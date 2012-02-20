      SUBROUTINE M08O10
C
C***  EXEC FOR OVERLAY 8, SUBSONIC VERTICAL TAIL DRAG
C
      COMMON /IVT/     PVT,    VT(380)
      COMMON /IVF/     PVF,    VF(380)
      COMMON /VTDATA/ AVT(195), AVF(195)
      COMMON /WHAERO/ C(51), D(55), CHT(51), DHT(55), DVT(55), DVF(55)
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
C
      NOVLY=8
      IF(VTPL) CALL VTDRAG(I)
      IF(VFPL) CALL VFDRAG(I)
C
C     SETUP VT IOM
C
      VT(1) = DVT(20)
      VT(21) = 0.0
      VT(41) = 0.0
      VT(61) = 0.0
      VT(101) = 0.0
      VT(121) = 0.0
      DO 1010 J=2,NALPHA
         VT(J) = -UNUSED
         VT(J+20) = -UNUSED
         VT(J+40) = -UNUSED
         VT(J+60) = -UNUSED
         VT(J+80) = -UNUSED
         VT(J+100) = -UNUSED
         VT(J+120) = -UNUSED
 1010 CONTINUE
C
C     SETUP VF IOM
C
      VF(1) = DVF(20)
      VF(21) = 0.0
      VF(41) = 0.0
      VF(61) = 0.0
      VF(101) = 0.0
      VF(121) = 0.0
      DO 1020 J=2,NALPHA
         VF(J) = -UNUSED
         VF(J+20) = -UNUSED
         VF(J+40) = -UNUSED
         VF(J+60) = -UNUSED
         VF(J+80) = -UNUSED
         VF(J+100) = -UNUSED
         VF(J+120) = -UNUSED
 1020 CONTINUE
      RETURN
      END
