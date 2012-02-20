      SUBROUTINE VNAME(KOL,L,LEN,NUM,NAMES,I,FOUND,ARRAY,
     1            NDMS,NF,NUMBER,BLANK,EQUAL)
      DIMENSION KOL(1), LEN(1), NAMES(1), NUMBER(1)
      INTEGER  PARL, PARR, EQUAL, BLANK
      LOGICAL FOUND, ARRAY, NMTEST, FLAG
      DATA PARL, PARR /4H(   ,4H)    /
C
C***  TEST FOR VARIABLE NAME
C
      J = 1
      DO 1010 I=1,NUM
      IF(.NOT. NMTEST(KOL(L),NAMES(J),LEN(I))) GO TO 1000
      K = L+LEN(I)
      FLAG = (KOL(K) .EQ. BLANK) .OR. (KOL(K) .EQ. EQUAL) .OR.
     1       (KOL(K) .EQ. PARL)
      IF(FLAG) GO TO 1020
 1000 J = J+LEN(I)
 1010 CONTINUE
      FOUND = .FALSE.
      I = 0
      GO TO 1080
C
C***  DETERMINE IF NAME IS AN ARRAY, CHECK FOR ARRAY ELEMENT
C***  DESIGNATION, AND CHECK NAME DELIMITERS
C
 1020 CONTINUE
      FOUND = .TRUE.
      ARRAY = .FALSE.
      L = L+LEN(I)
      NDMS = 0
      IF(KOL(L) .NE. PARL) GO TO 1080
        ARRAY = .TRUE.
        L = L+1
 1030   DO 1040 J=1,10
        IF(L .GE. 79) GO TO 1065
        IF(KOL(L) .EQ. NUMBER(J)) GO TO 1060
        IF(KOL(L) .EQ. PARR)      GO TO 1070
 1040   CONTINUE
        NF = NF+1
 1050   L = L+1
        IF(KOL(L) .EQ. EQUAL) GO TO 1080
        GO TO 1050
 1060   NDMS = 10*NDMS+J-1
        L = L+1
        GO TO 1030
 1065   CONTINUE
        IF(KOL(L) .NE. PARR) NF = NF+1
 1070   CONTINUE
        L = L+1
        NDMS = NDMS-1
        IF(NDMS .GE. 0) GO TO 1080
        NF = NF+1
        NDMS = 0
 1080 CONTINUE
      RETURN
      END
