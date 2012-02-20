      SUBROUTINE CTABS
C
C***  CONTROL TAB METHODS
C
      REAL KS,KQ,MACH
      COMMON /IBW/    PBW,    BW(380)
      COMMON /IBH/    PBH,    BH(380)
      COMMON /IBV/    PBV,    BV(380)
      COMMON /IBWH/   PBWH,   BWH(380)
      COMMON /IBWHV/  PBWHV,  BWHV(380)
      COMMON /FLGTCD/ NNNNN,MMMMMM,MACH(20),ALPHA(20),RNNUB(20),NGH
     1                ,GRDHT(10),PINF(20),STMACH,TSMACH,TR,ALT(20)
     2                ,TINF(20),VINF(20),WT,GAMMA,NALT,LOOP
      COMMON /OPTION/ SREF, CBARR, ROUGFC, BLREF
      COMMON /FLAPIN/ F(116),TTYPE,CFITC,CFOTC,BITC,BOTC,CFITT,CFOTT,
     1                BITT,BOTT,B1,B2,B3,B4,D1,D2,D3,GCMAX,
     2                KS,RL,BGR,DELR
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD,KAND
      COMMON /OVERLY/ NLOG,NMACH,II,NALPHA,IG,NF,LF,K
      DIMENSION CHCTF(20,9),CHCTL(20,9),DELT(9),DCHCG(20,9)
      DIMENSION DTT(20,9),CFC(20,9),FC(20,9)
      EQUIVALENCE (FC(1,1),CFC(1,1),BW(201)),(CHCTF(1,1),BH(201)),
     1            (CHCTL(1,1),BV(201)),(DCHCG(1,1),BWH(201)),
     2            (DELT(1),F(1)),(DTT(1,1),BWHV(201))
C
C***  CALCULATE R1 AND R2
C
      STC=(CFITC+CFOTC)*(BOTC-BITC)
      CTC=2.0*(CFITC+CFOTC-CFITC*CFOTC/(CFITC+CFOTC))/3.0
      AC=STC*CTC/(SREF*CBARR)
      Q=0.0
      KQ=0.0
      IF(PINF(II) .EQ. UNUSED) GO TO 1000
         Q=0.7*PINF(II)*MACH(II)**2
         KQ=KS/Q
 1000 CONTINUE
      IF(RL .GE. 0.0) GO TO 1010
         R1 = 0.0
         R2 = 1.0
 1010 CONTINUE
      IF(RL .NE. 0.0) GO TO 1020
         D = B2/(AC*D2)+KQ*BGR/D2
         R1 = DELR/D
         R2 = -R1*(KQ/D2)
 1020 CONTINUE
      IF(RL .LE. 0.0) GO TO 1030
         D = RL+B2/(AC*D2)-KQ*(RL-BGR)/D2
         R1 = (RL+DELR)/D
         R2 = -R1*(KQ/D2)
 1030 CONTINUE
C
C***  CALCULATE CONTROL FORCE
C
      ITYPE = TTYPE+0.5
      DO 1050 I=1,NALPHA
         NDELTA = F(16)+0.5
         DO 1040 J=1,NDELTA
            CHCTF(I,J) = (B1+D1*B2/D2)*DELT(J)+(B3-D3*B2/D2)*ALPHA(I)
            CHCTL(I,J) = B1*DELT(J)+B3*ALPHA(I)
            DCHCG(I,J) = (BGR*B2+BGR*AC*D1+BGR*BGR*AC*D2)*DELT(J)
     1                   + BGR*AC*D3*ALPHA(I)
            CFC(I,J) = GCMAX*(R1*CHCTF(I,J)+R2*CHCTL(I,J)+R2*DCHCG(I,J))
            IF(ITYPE .EQ. 2) DTT(I,J) = -(B1*DELT(J)+B3*ALPHA(I))/B4
            IF(ITYPE .EQ. 3) DTT(I,J) = CFC(I,J)/(B4*(R1+R2)*GCMAX)
            IF(Q .GT. 0.0)   FC(I,J) = CFC(I,J)/(Q*SREF*CBARR)
 1040    CONTINUE
 1050 CONTINUE
      RETURN
      END
