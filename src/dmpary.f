      SUBROUTINE DMPARY(ARRAY, NPT, NAME, NLET)
C
C***  DUMP SPECIFIED ARRAY IN READABLE FORMAT
C
      DIMENSION ARRAY(NPT), IFOR(8), AFOR(4), BFOR(4)
      CHARACTER*4 IFOR, AFOR, BFOR
      DATA IFOR / '    ', '    ', ',1H(', ',I3,', '2H)=', '1P  ',
     1            ',E12', '.5))' /
      DATA AFOR / '(5(5', '(5(4', '(5(3', '(5(2'/
      DATA BFOR / 'X,A1', 'X,A2', 'X,A3', 'X,A4'/
C
C     SET FORMAT FOR THE NUMBER OF LETTERS IN NAME
C     AND WRITE OUT ARRAY
C
      IFOR(1) = AFOR(NLET)
      IFOR(2) = BFOR(NLET)
      WRITE(6,1000)
 1000 FORMAT(1H )
      WRITE(6,IFOR) (NAME, I, ARRAY(I), I=1,NPT)
      RETURN
      END
