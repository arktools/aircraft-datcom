      SUBROUTINE XNAM6(IOP)
C
C***  READ OR WRITE NAMELIST SYNTHS
C
      COMMON /SYNTSS/ A7(19)
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD,KAND
      COMMON /BDATA/ BD(82)
C
      INTEGER SYNTHS
      LOGICAL EOF
C
      DIMENSION LEN7(19),LDM7(19),SYNTHS(60),LOC7(19)
      DIMENSION NLNAME(6)
C
      DATA NLNAME / 4HS   ,4HY   ,4HN   ,4HT   ,4HH   ,4HS   /
      DATA LEN7 / 3,2,2,4,3,2,2,4,2,6,5,3,5,2,3,2,2,4,4 /
      DATA LDM7 / 9*1,-1,9*1 /
      DATA LOC7 / 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19 /
      DATA SYNTHS /   4HX   ,4HC   ,4HG   ,4HX   ,4HW   ,4HZ   ,
     1  4HW   ,4HA   ,4HL   ,4HI   ,4HW   ,4HZ   ,4HC   ,4HG   ,
     2  4HX   ,4HH   ,4HZ   ,4HH   ,4HA   ,4HL   ,4HI   ,4HH   ,
     3  4HX   ,4HV   ,4HV   ,4HE   ,4HR   ,4HT   ,4HU   ,4HP   ,
     4  4HH   ,4HI   ,4HN   ,4HA   ,4HX   ,4HX   ,4HV   ,4HF   ,
     5  4HS   ,4HC   ,4HA   ,4HL   ,4HE   ,4HZ   ,4HV   ,4HZ   ,
     6  4HV   ,4HF   ,4HY   ,4HV   ,4HY   ,4HF   ,4HP   ,4HH   ,
     7  4HI   ,4HV   ,4HP   ,4HH   ,4HI   ,4HF   /
C
C**   IF IOP EQUAL ZERO READ NAMELIST SYNTHS
C**   IF IOP EQUAL ONE WRITE NAMELIST SYNTHS
C
      IF(IOP .EQ. 0)
     1 CALL NAMER(KAND,9,NLNAME,6,SYNTHS,60,LEN7,19,LDM7,A7,19,LOC7,EOF)
      IF(IOP .EQ. 1)
     1  CALL NAMEW(KAND,6,NLNAME,6,SYNTHS,60,LEN7,19,LDM7,A7,19,LOC7)
C
      BD(33)=A7(1)
      BD(65)=A7(2)
      BD(74)=A7(3)
      BD(77)=A7(4)
      BD(82)=A7(5)
      RETURN
      END
