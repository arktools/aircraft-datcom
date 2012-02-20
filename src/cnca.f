      SUBROUTINE CNCA
C
C***  CALCULATES CN AND CA
C
      COMMON /IBODY/  PBOD, BODY(400)
      COMMON /IWING/  PWNG, WING(400)
      COMMON /IHT/    PHT,  HT(380)
      COMMON /IBW/    PBW,  BW(380)
      COMMON /IBH/    PBH,  BH(380)
      COMMON /IBV/    PBV,  BV(380)
      COMMON /IBWH/   PBWH, BWH(380)
      COMMON /IBWV/   PBWV, BWV(380)
      COMMON /IBWHV/  PBWT, BWT(380)
      COMMON /FLGTCD/ FLC(22),ALP(20)
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD
      COMMON /OVERLY/ NLOG,NMACH,I,NA
      DIMENSION C(6),ROUTID(2)
      LOGICAL FLAG
      DATA ROUTID /4HCNCA, 4H    /
C
      DO 1000 J=1,NA
         SA = SIN(ALP(J)/RAD)
         CA = COS(ALP(J)/RAD)
C
         FLAG = ABS(BODY(J+20)) .NE. UNUSED
         IN = 0
         IF(FLAG .AND. J .GE. 2) CALL TBFUNX(ALP(J),Y,BODY(J+100),NA,
     1       ALP,BODY(21),C,IN,MI,NG,0,0,4HCLA ,1,ROUTID)
         FLAG = FLAG .AND. ABS(BODY(J)) .NE. UNUSED
         IF(FLAG) BODY(J+60) = BODY(J+20)*CA+BODY(J)*SA
         IF(FLAG) BODY(J+80) = BODY(J)*CA-BODY(J+20)*SA
C
         FLAG = ABS(WING(J+40)) .NE. UNUSED .AND.
     1          ABS(WING(42  )) .NE. UNUSED
         IN = 0
         IF(FLAG) CALL TBFUNX(ALP(J),Y,WING(J+120),NA,ALP,WING(41),C,IN,
     1         MI,NG,0,0,4HCMA ,1,ROUTID)
         FLAG = ABS(WING(J+20)) .NE. UNUSED
         IN = 0
         IF(FLAG .AND. J .GE. 2) CALL TBFUNX(ALP(J),Y,WING(J+100),NA,
     1      ALP,WING(21),C,IN,MI,NG,0,0,4HCLA ,1,ROUTID)
C
         FLAG = FLAG .AND. ABS(WING(J)) .NE. UNUSED
         IF(FLAG) WING(J+60) = WING(J+20)*CA+WING(J)*SA
         IF(FLAG) WING(J+80) = WING(J)*CA-WING(J+20)*SA
C
         FLAG = ABS(HT(J+20)) .NE. UNUSED
         IN = 0
         IF(FLAG .AND. J .GE. 2) CALL TBFUNX(ALP(J),Y,HT(J+100),NA,ALP,
     1      HT(21),C,IN,MI,NG,0,0,4HCLA ,1,ROUTID)
         FLAG = FLAG .AND. ABS(HT(J)) .NE. UNUSED
         IF(FLAG) HT(J+60) = HT(J+20)*CA+HT(J)*SA
         IF(FLAG) HT(J+80) = HT(J)*CA-HT(J+20)*SA
C
         FLAG = ABS(BW(J+20)) .NE. UNUSED
         IN = 0
         IF(FLAG .AND. J .GE. 2) CALL TBFUNX(ALP(J),Y,BW(J+100),NA,ALP,
     1      BW(21),C,IN,MI,NG,0,0,4HCLA ,1,ROUTID)
         FLAG = FLAG .AND. ABS(BW(J)) .NE. UNUSED
         IF(FLAG) BW(J+60) = BW(J+20)*CA+BW(J)*SA
         IF(FLAG) BW(J+80) = BW(J)*CA-BW(J+20)*SA
C
         FLAG = ABS(BH(J+20)) .NE. UNUSED
         IN = 0
         IF(FLAG .AND. J .GE. 2) CALL TBFUNX(ALP(J),Y,BH(J+100),NA,ALP,
     1      BH(21),C,IN,MI,NG,0,0,4HCLA ,1,ROUTID)
         FLAG = FLAG .AND. ABS(BH(J)) .NE. UNUSED
         IF(FLAG) BH(J+60) = BH(J+20)*CA+BH(J)*SA
         IF(FLAG) BH(J+80) = BH(J)*CA-BH(J+20)*SA
C
         FLAG = ABS(BV(J+20)) .NE. UNUSED
         IN = 0
         IF(FLAG .AND. J .GE. 2) CALL TBFUNX(ALP(J),Y,BV(J+100),NA,ALP,
     1      BV(21),C,IN,MI,NG,0,0,4HCLA ,1,ROUTID)
         FLAG = FLAG .AND. ABS(BV(J)) .NE. UNUSED
         IF(FLAG) BV(J+60) = BV(J+20)*CA+BV(J)*SA
         IF(FLAG) BV(J+80) = BV(J)*CA-BV(J+20)*SA
C
         FLAG = ABS(BWH(J+20)) .NE. UNUSED
         IN = 0
         IF(FLAG .AND. J .GE. 2) CALL TBFUNX(ALP(J),Y,BWH(J+100),NA,ALP,
     1      BWH(21),C,IN,MI,NG,0,0,4HCLA ,1,ROUTID)
         FLAG = FLAG .AND. ABS(BWH(J)) .NE. UNUSED
         IF(FLAG) BWH(J+60) = BWH(J+20)*CA+BWH(J)*SA
         IF(FLAG) BWH(J+80) = BWH(J)*CA-BWH(J+20)*SA
C
         FLAG = ABS(BWV(J+20)) .NE. UNUSED
         IN = 0
         IF(FLAG .AND. J .GE. 2) CALL TBFUNX(ALP(J),Y,BWV(J+100),NA,ALP,
     1      BWV(21),C,IN,MI,NG,0,0,4HCLA ,1,ROUTID)
         FLAG = FLAG .AND. ABS(BWV(J)) .NE. UNUSED
         IF(FLAG) BWV(J+60) = BWV(J+20)*CA+BWV(J)*SA
         IF(FLAG) BWV(J+80) = BWV(J)*CA-BWV(J+20)*SA
C
         FLAG = ABS(BWT(J+20)) .NE. UNUSED
         IN = 0
         IF(FLAG .AND. J .GE. 2) CALL TBFUNX(ALP(J),Y,BWT(J+100),NA,ALP,
     1      BWT(21),C,IN,MI,NG,0,0,4HCLA ,1,ROUTID)
         FLAG = FLAG .AND. ABS(BWT(J)) .NE. UNUSED
         IF(FLAG) BWT(J+60) = BWT(J+20)*CA+BWT(J)*SA
         IF(FLAG) BWT(J+80) = BWT(J)*CA-BWT(J+20)*SA
 1000 CONTINUE
      RETURN
      END
