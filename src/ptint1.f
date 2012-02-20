      SUBROUTINE PTINT1(XP,YP,A,AMU,X,Y,XI,YI,NSUM,EFFECT,K,J,NCON,
     1VERTUP)
C
C***  CALCULATES THE BOUNDRIES OF THE MACH LINE ON THE VERTICAL TAIL
C
      DIMENSION XP(1),YP(1),A(1),X(1),Y(1),XI(1),YI(1),XDIF(4),YDIF(4)
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD
      COMMON /VTDATA/ AVT(195)
      LOGICAL EFFECT,VERTUP
C
      ICON=0
      NSUM=0
      INDEX=0
      PIO2=PI/2.0
      LEORTE=0
      IF(J.EQ.2) LEORTE=18
      INOROT=0
      IF(NCON.EQ.1) INOROT=24
      DO 1000 KLM=1,5
         X(KLM)=(XP(KLM)-A(1))*COS(-A(3)/RAD)+(YP(KLM)-A(2))
     1          *SIN(-A(3)/RAD)
         Y(KLM)=(YP(KLM)-A(2))*COS(-A(3)/RAD)-(XP(KLM)-A(1))
     1          *SIN(-A(3)/RAD)
 1000 CONTINUE
      DO 1130 K=1,4
         XDIF(K)=X(K+1)-X(K)
         YDIF(K)=Y(K+1)-Y(K)
         IF(XDIF(K) .EQ. 0.0 .AND. YDIF(K) .EQ. 0.0) GO TO 1130
         TAMU=SIN(AMU)/COS(AMU)
         XI(K)=(Y(K)*XDIF(K)-X(K)*YDIF(K))/(XDIF(K)*TAMU-YDIF(K))
         YI(K)=XI(K)*TAMU
         IF(K.EQ.2.OR.K.EQ.4) GO TO 1100
         IF(VERTUP) GO TO 1050
         IF(K.NE.1) GO TO 1030
         IF(YI(1).LE.Y(1)) GO TO 1010
         IF((PIO2+AMU).LT.(AVT(59+INOROT)+A(3)/RAD)) GO TO 1020
         GO TO 1130
 1010    IF(YI(1).GE.Y(2)) GO TO 1120
         IF((PIO2+AMU).LT.(AVT(59+INOROT)+A(3)/RAD)) GO TO 1130
 1020    IF(J.EQ.2) GO TO 1140
         INDEX=INDEX+1
         GO TO 1140
 1030    IF(YI(3).GE.Y(3).AND.YI(3).LE.Y(4)) GO TO 1120
         IF(YI(3).LT.Y(3)) GO TO 1040
         IF(J.EQ.2) INDEX=INDEX+1
         GO TO 1140
 1040    IF((PIO2+AMU).GT.(AVT(77+INOROT)+A(3)/RAD)) GO TO 1130
         IF(J.EQ.2) INDEX=INDEX+1
         GO TO 1140
 1050    CONTINUE
         IF(K.NE.1) GO TO 1080
         IF(YI(1).GE.Y(1)) GO TO 1060
         IF((PIO2-AMU).LT.(AVT(59+INOROT)-A(3)/RAD)) GO TO 1070
         GO TO 1130
 1060    IF(YI(1).LE.Y(2)) GO TO 1120
         IF((PIO2-AMU).LT.(AVT(59+INOROT)-A(3)/RAD)) GO TO 1130
 1070    IF(J.EQ.2) GO TO 1140
         INDEX=INDEX+1
         GO TO 1140
 1080    IF(YI(3).LE.Y(3).AND.YI(3).GE.Y(4)) GO TO 1120
         IF(YI(3).GT.Y(3)) GO TO 1090
         IF(J.EQ.2) INDEX=INDEX+1
         GO TO 1140
 1090    IF((PIO2-AMU).GT.(AVT(77+INOROT)-A(3)/RAD)) GO TO 1130
         IF(J.EQ.2) INDEX=INDEX+1
         GO TO 1140
 1100    IF(K.NE.2) GO TO 1110
         IF(XI(2).GE.X(2).AND.XI(2).LE.X(3)) GO TO 1120
        GO TO 1130
 1110    IF(XI(4).GE.X(5).AND.XI(4).LE.X(4)) GO TO 1120
         GO TO 1130
 1120    ICON=ICON+1
         NSUM=NSUM+K
         INDEX=INDEX+1
         IF(ICON.EQ.2) GO TO 1140
 1130 CONTINUE
 1140 CONTINUE
      EFFECT=.FALSE.
      IF(INDEX.GT.0)EFFECT=.TRUE.
      RETURN
      END
