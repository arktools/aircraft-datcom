      SUBROUTINE ARCLSS(A)
C
C     ----CLASSIFIES ASPECT RATIO
C
      DIMENSION ROUTID(2)
      DIMENSION Q3417B(3)
      DIMENSION X(11),Y(11),C(6),A(195)
      DATA ROUTID /4HARCL,4HSS  /
      DATA Q3417B /4H4.1.,4H3.4-,4H24B /
      DATA X /0.,.1,.2,.3,.4,.5,.6,.7,.8,.9,1./
      DATA Y /0.,.225,.47,.496,.43,.32,.21,.125,.075,.0475,.0/
      DATA IN /0/
      DATA NP /11/
C
C     ----FIGURE 4.1.3.4-24B
C
      CALL TBFUNX(A(27),A(123),DYDX,NP,X,Y,C,IN,MI,NG,
     1            0,0,Q3417B,3,ROUTID)
      CALL ANGLES(1,A(34))
      A(124)=(A(123)+1.)*A(37)
      A(125)=A(128)/A(124)
      RETURN
      END
