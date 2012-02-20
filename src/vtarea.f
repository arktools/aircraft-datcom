      SUBROUTINE VTAREA(VTIN,AVT,VERTUP,XV,ZV)
C
C *** EXECUTIVE TO COMPUTE VERTICAL TAIL AREA SHADOWED BY MACH LINES
C
      COMMON /OVERLY/ NLOG,NMACH,I,NALPHA,IG
      COMMON /WINGD/  A(195)
      COMMON /HTDATA/ AHT(195)
      COMMON /WINGI/  WINGIN(100)
      COMMON /HTI/    HTIN(154)
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD
      COMMON /FLGTCD/ FLC(73)
      COMMON /SYNTSS/ SYNA(19)
      COMMON /FLOLOG/ FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC
      LOGICAL EFFECT,VERTUP
      LOGICAL HTAIL,FLIP,ANGCHG
      DIMENSION XP(5),YP(5),XX(4),YY(4),AA(3),XI(4),YI(4),SVWB(2),
     1          SVHB(2),AREA(2),X(5),Y(5)
      LOGICAL FLTC,OPTI,BO,WGPL,WGSC,SYNT,HTPL,HTSC,VTPL,VTSC
      DIMENSION VTIN(154),AVT(195)
      DATA STRA/4HSTRA/
C
      HTAIL=.FALSE.
      ANGCHG=.FALSE.
      NCON=0
      SVWB(1)=0.
      SVWB(2)=0.
      SVHB(1)=0.
      SVHB(2)=0.
C
C     COORDINATES OF INBOARD VT PANEL
C
 1000 XP(1)=XV+(VTIN(4)-VTIN(3))*AVT(62)
      YP(1)=VTIN(4)-VTIN(3)+ZV
      IF(.NOT.VERTUP) YP(1)=VTIN(4)-VTIN(3)-ZV
      XP(2)=XV+AVT(21)*AVT(62)
      YP(2)=AVT(21)
      XP(3)=XP(2)+VTIN(5)
      YP(3)=AVT(21)
      XP(4)=XP(1)+AVT(10)
      YP(4)=YP(1)
      XP(5)=XP(1)
      YP(5)=YP(1)
      XMACH=FLC(I+2)
C
C     TEST FOR VT POSITION
C
      IF(VERTUP) GO TO 1020
      DO 1010 N=1,5
 1010 YP(N)=-YP(N)
      XMACH=-XMACH
 1020 AMUU=ATAN(1./SQRT(XMACH**2-1.))
C
C     CALCULATE LE AND TE MACH LINE INTERSECTIONS WITH VT PANEL
C
      DO 1220 J=1,2
         FLIP=.FALSE.
         IF(HTAIL) GO TO 1030
         AA(1)=SYNA(2)+(WINGIN(4)-WINGIN(3))*A(62)*COS(SYNA(4)/RAD)
         AA(2)=SYNA(3)-(AA(1)-SYNA(2))*SIN(SYNA(4)/RAD)/
     1         COS(SYNA(4)/RAD)
         AA(3)=SYNA(4)
         IF(J.EQ.1) GO TO 1050
         AA(1)=AA(1)+A(10)*COS(SYNA(4)/RAD)
         AA(2)=AA(2)-A(10)*SIN(SYNA(4)/RAD)
         GO TO 1050
 1030    IF(SYNA(4).EQ.SYNA(8).OR.ANGCHG) GO TO 1040
C
C     CALCULATE XH AND ZH FOR HT WITH INCIDINCE ANGLE EQUAL TO THAT OF
C     WING  (ROTATION ABOUT 1/4 MAC)
C
         HACLE=(HTIN(4)-HTIN(3))*AHT(62)+AHT(30)-AHT(16)/4.
         XHAC=SYNA(6)+HACLE*COS(SYNA(8)/RAD)
         ZHAC=SYNA(7)-HACLE*SIN(SYNA(8)/RAD)
         SYNA(6)=XHAC-HACLE*COS(SYNA(4)/RAD)
         SYNA(7)=ZHAC+HACLE*SIN(SYNA(4)/RAD)
         ANGCHG=.TRUE.
 1040    AA(1)=SYNA(6)+(HTIN(4)-HTIN(3))*AHT(62)*COS(SYNA(4)/RAD)
         AA(2)=SYNA(7)-(AA(1)-SYNA(6))*SIN(SYNA(4)/RAD)/
     1         COS(SYNA(4)/RAD)
         AA(3)=SYNA(4)
         IF(J.EQ.1) GO TO 1050
         AA(1)=AA(1)+AHT(10)*COS(SYNA(4)/RAD)
         AA(2)=AA(2)-AHT(10)*SIN(SYNA(4)/RAD)
 1050    CALL PTINT1(XP,YP,AA,AMUU,X,Y,XI,YI,NSUM,EFFECT,KK,J,NCON
     1              ,VERTUP)
C
C     CHECK FOR MACH LINE NOT INTERSECTING VT
C
         IF(J.NE.1) GO TO 1060
         IF(EFFECT) GO TO 1070
         GO TO 1230
 1060    IF(EFFECT) GO TO 1070
         AREA(J)=0.
         IF(KK.EQ.1) AREA(J)=AVT(1+NCON)-AREA(J)
         GO TO 1200
 1070    IF(NSUM.NE.0) GO TO 1080
         AREA(J)=0.
         IF(J.NE.1) FLIP=.TRUE.
         GO TO 1180
 1080    CONTINUE
         GO TO (1090,1100,1110,1120,1130,1150,1160) NSUM
 1090    CONTINUE
 1100    CONTINUE
C
C     HERE FOR MACH LINE INTERSECTING LE AND TIP CHORD
C
 1110    XX(1)=XI(1)
         XX(2)=X(2)
         XX(3)=XI(2)
         YY(1)=YI(1)
         YY(2)=Y(2)
         YY(3)=YI(2)
         GO TO 1170
C
C     HERE FOR MACH LINE INTERSECTING LE AND TE
C
 1120    XX(1)=XI(1)
         XX(2)=X(2)
         XX(3)=X(3)
         XX(4)=XI(3)
         YY(1)=YI(1)
         YY(2)=Y(2)
         YY(3)=Y(3)
         YY(4)=YI(3)
         IF(ABS(XI(1)).GT.ABS(XI(3))) FLIP=.TRUE.
         GO TO 1170
 1130    IF(KK.EQ.4) GO TO 1140
C
C     HERE FOR MACH LINE INTERSECTING TE AND TIP CHORD
C
         XX(1)=XI(2)
         XX(2)=X(3)
         XX(3)=XI(3)
         YY(1)=YI(2)
         YY(2)=Y(3)
         YY(3)=YI(3)
         FLIP=.TRUE.
         GO TO 1170
C
C     HERE FOR MACH LINE INTERSECTING ROOT CHORD AND LE
C
 1140    XX(1)=X(1)
         XX(2)=XI(1)
         XX(3)=XI(4)
         YY(1)=Y(1)
         YY(2)=YI(1)
         YY(3)=YI(4)
         GO TO 1170
C
C     HERE FOR MACH LINE INTERSECTING ROOT CHORD AND TIP CHORD
C
 1150    XX(1)=X(1)
         XX(2)=X(2)
         XX(3)=XI(2)
         XX(4)=XI(4)
         YY(1)=Y(1)
         YY(2)=Y(2)
         YY(3)=YI(2)
         YY(4)=YI(4)
         GO TO 1170
C
C     HERE FOR MACH LINE INTERSECTING ROOT CHORD AND TE
C
 1160    XX(1)=XI(3)
         XX(2)=X(4)
         XX(3)=XI(4)
         YY(1)=YI(3)
         YY(2)=Y(4)
         YY(3)=YI(4)
         FLIP=.TRUE.
 1170    CALL AREA1 (XX,YY,SAREA,NSUM)
         AREA(J)=SAREA
 1180    IF(J.NE.1) GO TO 1190
         IF(.NOT.FLIP) GO TO 1220
         AREA(1)=AVT(1+NCON)-AREA(1)
         GO TO 1220
 1190    IF(FLIP) GO TO 1200
         AREA(2)=AVT(1+NCON)-AREA(2)
 1200    IF(.NOT.HTAIL) GO TO 1210
         SVHB(1+NCON)=AVT(1+NCON)-(AREA(1)+AREA(2))
         GO TO 1220
 1210    SVWB(1+NCON)=AVT(1+NCON)-(AREA(1)+AREA(2))
 1220 CONTINUE
      IF(NCON.EQ.1) GO TO 1230
      NCON=1
C
C     TEST FOR NON-STRAIGHT TAPERED VT
C
      IF(VTIN(15).EQ.STRA) GO TO 1230
C
C     COORDINATES OF OUTBOARD VT PANEL
C
      XP(1)=XP(2)
      YP(1)=YP(2)
      XP(2)=XP(2)+VTIN(2)*AVT(86)
      YP(2)=VTIN(4)
      XP(3)=XP(2)+VTIN(1)
      YP(3)=VTIN(4)
      XP(4)=XP(1)+VTIN(5)
      YP(4)=YP(1)
      XP(5)=XP(1)
      YP(5)=YP(1)
      GO TO 1020
 1230 IF(.NOT.HTAIL) GO TO 1240
      VTIN(134+I)=SVHB(1)+SVHB(2)
      GO TO 1250
 1240 VTIN(94+I)=SVWB(1)+SVWB(2)
 1250 CONTINUE
      IF(HTAIL) GO TO 1260
C
C     TEST FOR HT
C
      IF(.NOT.HTPL) GO TO 1260
      HTAIL=.TRUE.
      NCON=0
      GO TO 1000
 1260 VTIN(114+I)=AVT(3)-(VTIN(94+I)+VTIN(134+I))
      RETURN
      END
