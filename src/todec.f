      SUBROUTINE TODEC(ALPHA, ANS, IERR)
C
C***  CONVERT AN ALPHANUMERIC STRING INTO A FLOATING POINT CONSTANT
C
C***  INPUTS
C
C  ALPHA - ALPHANUMERIC STRING (MUST BE INTEGER)
C
C***  OUTPUTS
C
C    ANS - FLOATING POINT RESULT
C   IERR - ERROR CODE, 0=NO ERROR OR 1=CONSTANT NOT CORRECT TYPE
C
C**********************************************************************
C***  NOTE - INTEGER ARITHMETIC USED TO INCREASE NUMERICAL ACCURACY ***
C**********************************************************************
C
      INTEGER ALPHA,PLUS,PERIOD,EE,BLANK,POWER,SIGNP,PTEN,RVALUE
C
      DIMENSION ALPHA(80),NUM(10)
C
      LOGICAL FRAC
C
      DATA NUM / 4H0   ,4H1   ,4H2   ,4H3   ,4H4   ,4H5   ,4H6   ,
     1           4H7   ,4H8   ,4H9   /
      DATA PLUS, MINUS, PERIOD, EE, BLANK
     1  / 4H+   ,4H-   ,4H.   ,4HE   ,4H    /
C
C*****************************************
C***  INITIALIZE (DEFAULT ANSWER = 0.) ***
C*****************************************
C
      IERR=0
      FRAC=.FALSE.
      LVALUE=0
      RVALUE=0
      POWER=0
      SIGNV=0.
      SIGNP=0
      PTEN=0
C
C*********************
C***  GET MANTISSA ***
C*********************
C
      DO 1070 I=1,80
C
         ICOL=I
C
C ...    SKIP BLANKS
C
         IF(ALPHA(I) .EQ. BLANK)GO TO 1070
C
C ...    IF E IS DETECTED, START EXPONENT
C
         IF(ALPHA(I) .EQ. EE)GO TO 1080
C
C ...    CHECK FOR + SIGN ON NUMBER
C
         IF(ALPHA(I) .NE. PLUS)GO TO 1000
C
C ...    IF ALREADY DEFINED, EXIT
C
         IF(SIGNV .NE. 0.)GO TO 1130
C
C ...    SET PLUS SIGN
C
         SIGNV=1.
         GO TO 1070
C
C ...    CHECK FOR MINUS SIGN ON NUMBER
C
 1000    IF(ALPHA(I) .NE. MINUS)GO TO 1010
C
C ...    IF ALREADY DEFINED, EXIT
C
         IF(SIGNV .NE. 0.)GO TO 1130
C
C ...    SET MINUS SIGN
C
         SIGNV=-1.
         GO TO 1070
C
C ...    IF PERIOD, MUST DO FRACTIONAL PART OF NUMBER
C
 1010    IF(ALPHA(I) .NE. PERIOD)GO TO 1020
C
         FRAC=.TRUE.
         GO TO 1070
C
 1020    IF(FRAC)GO TO 1040
C
C ...    DECODE NUMBERS TO LEFT OF DECIMAL POINT
C
         DO 1030 J=1,10
            IF(ALPHA(I) .EQ. NUM(J))LVALUE=10*LVALUE+J-1
            IF(ALPHA(I) .EQ. NUM(J))GO TO 1070
 1030    CONTINUE
C
C ...    ERROR, CONSTANT NOT REAL OR INTEGER
C
         IERR=1
C
         GO TO 1130
C
C ...    DECODE NUMBERS TO RIGHT OF DECIMAL POINT
C
 1040    DO 1050 J=1,10
            IF(ALPHA(I) .EQ. NUM(J))RVALUE=10*RVALUE+J-1
            IF(ALPHA(I) .EQ. NUM(J))GO TO 1060
 1050    CONTINUE
C
C ...    ERROR, CONSTANT NOT REAL OR INTEGER
C
         IERR=1
C
         GO TO 1130
C
C ...    COUNT NUMBER OF DIGITS RIGHT OF DECIMAL POINT
C
 1060    PTEN=PTEN+1
C
 1070 CONTINUE
C
      GO TO 1130
C
C*********************
C***  GET EXPONENT ***
C*********************
C
 1080 ICOL=ICOL+1
      IF(ICOL .GT. 80)GO TO 1130
C
      DO 1120 I=ICOL,80
C
C ...    CHECK FOR PLUS SIGN ON EXPONENT
C
         IF(ALPHA(I) .NE. PLUS)GO TO 1090
C
C ...    IF ALREADY DEFINED, EXIT
C
         IF(SIGNP .NE. 0)GO TO 1130
C
C ...    SET POSITIVE EXPONENT
C
         SIGNP=1
         GO TO 1120
C
C ...    CHECK FOR NEGATIVE SIGN ON EXPONENT
C
 1090    IF(ALPHA(I) .NE. MINUS)GO TO 1100
C
C ...    IF ALREADY DEFINED, EXIT
C
         IF(SIGNP .NE. 0)GO TO 1130
C
C ...    SET NEGATIVE EXPONENT
C
         SIGNP=-1
         GO TO 1120
C
C ...    DECODE EXPONENT
C
 1100    DO 1110 J=1,10
            IF(ALPHA(I) .EQ. NUM(J))POWER=10*POWER+J-1
            IF(ALPHA(I) .EQ. NUM(J))GO TO 1120
 1110    CONTINUE
C
         GO TO 1130
C
 1120 CONTINUE
C
C***********************
C***  CONVERT NUMBER ***
C***********************
C
C ... VERIFY SIGN FOR MANTISSA AND EXPONENT DEFINED
C
 1130 IF(SIGNV .EQ. 0.)SIGNV=1.
      IF(SIGNP .EQ. 0 )SIGNP=1
C
      ANS=SIGNV*(FLOAT(LVALUE)+FLOAT(RVALUE)*10.**(-PTEN))
     1    *10.**(SIGNP*POWER)
C
      RETURN
      END
