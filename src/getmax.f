      SUBROUTINE GETMAX(X,S,NX,XMAX,SMAX,IMAX)
C
C     ----SUBROUTINE FINDS MAXIMUM S AND X AND I ASSOCIATED WITH IT
C
      DIMENSION ROUTID(2)
      DIMENSION SMX(3),DSDX(3),XMX(3)
      DIMENSION X(1),S(1),CXA(6)
      DATA ROUTID/4HGETM,4HAX  /,SID/4HS   /
      IXA=0
      SM=S(1)
      IM = 0
      DO 1000 I=1,NX
         IF(S(I).LE.SM.AND.I.GT.1)GO TO 1000
         SM=S(I)
         XM=X(I)
         IM=I
         IMAX=IM
 1000 CONTINUE
      XMAX = XM
      SMAX = SM
      RETURN
      END
