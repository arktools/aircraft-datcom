      SUBROUTINE DECFIG(IFIG,N,IFIGN)
C
C   SUBROUTINE TO READ HOLLERITH FIGURE NUMBER AND TRANSLATE
C   INTO A NINE CHARACTER INTEGER WORD
C
      LOGICAL LAST,IALPHA,ILET,ILETT
      INTEGER BLANK,COMMA,POINT
      DIMENSION IFIG(N)
      DIMENSION NUM(10),LET(26)
      DATA NUM/4H0   , 4H1   , 4H2   , 4H3   , 4H4   , 4H5   ,
     1         4H6   , 4H7   , 4H8   , 4H9   /
      DATA LET/4HA   , 4HB   , 4HC   , 4HD   , 4HE   , 4HF   ,
     1         4HG   , 4HH   , 4HI   , 4HJ   , 4HK   , 4HL   ,
     2         4HM   , 4HN   , 4HO   , 4HP   , 4HQ   , 4HR   ,
     3         4HS   , 4HT   , 4HU   , 4HV   , 4HW   , 4HX   ,
     4         4HY   , 4HZ   /
      DATA BLANK,POINT,MINUS,COMMA/4H    , 4H.   , 4H-   , 4H,   /
      LAST=.FALSE.
      IALPHA=.FALSE.
      ILET=.FALSE.
      ILETT=.FALSE.
      IFIGN=0
      NCOUNT=0
      NSAVE=0
      KFIGN=0
      DO 1040 I=1,N
         IF(I.EQ.2.AND.ILET)ILETT=.TRUE.
         IF(IFIG(I).EQ.BLANK .OR. IFIG(I).EQ.POINT
     1      .OR. IFIG(I).EQ.COMMA) GO TO 1040
         IF(IFIG(I).EQ.MINUS .AND. LAST) GO TO 1040
         IF(IFIG(I).EQ.MINUS) LAST=.TRUE.
         IF(IFIG(I).EQ.MINUS) GO TO 1030
         DO 1000 J=1,10
            NN=J-1
            IF(IFIG(I).EQ.NUM(J)) GO TO 1020
 1000    CONTINUE
         IF(.NOT. LAST)IALPHA=.TRUE.
         IF(.NOT. LAST)NCOUNT=NCOUNT+1
         IF(ILET)GO TO 1040
         IF(LAST)ILET=.TRUE.
         DO 1010 J=1,26
            NN=J
            IF(IFIG(I).EQ.LET(J)) GO TO 1020
 1010    CONTINUE
         ILET=.FALSE.
         GO TO 1040
 1020    CONTINUE
         IF(ILET)KFIGN=NN
         IF(ILET.AND.ILETT)GO TO 1040
         NCOUNT=NCOUNT+1
         IF(IALPHA .AND. NCOUNT.GT.8)GO TO 1040
         IF(NCOUNT .GT. 8) NCOUNT=8
         IFIGN=IFIGN+NN*10**(9-NCOUNT)
         GO TO 1040
 1030    NCOUNT=4
         NSAVE=IFIGN
 1040 CONTINUE
C
C   POST-PROCESS FIGURE NUMBER
C
      JFIGN=IFIGN-NSAVE
      IF(NCOUNT.EQ.5)JFIGN=JFIGN/100
      IF(NCOUNT.EQ.6)JFIGN=JFIGN/10
      IFIGN=NSAVE+JFIGN+KFIGN
      RETURN
      END
