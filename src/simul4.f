      SUBROUTINE SIMUL4(COFF,EQ,UNK)
C
C     ----SUBROUTINE SOLVES  4  SIMULTANEOUS LINEAR EQUATIONS
C
      DIMENSION COFF(16),EQ(4),UNK(4),DE(16)
      CALL DET4(COFF,D)
      DO 1020 M=1,4
         DO 1000 I=1,16
            DE(I)=COFF(I)
 1000    CONTINUE
         K=1
         DO 1010 I=M,16,4
            DE(I)=EQ(K)
            K=K+1
 1010    CONTINUE
         CALL DET4(DE,G)
         UNK(M)=G/D
 1020 CONTINUE
      RETURN
      END
