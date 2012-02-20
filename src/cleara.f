      SUBROUTINE CLEARA(IFIG,ROUTL,MSSCLL)
C
C***  CLEAR STORAGE ARRAYS FOR EXTRAPOLATION MESSAGES
C
      INTEGER ROUT,ROUTL
      COMMON /IBODY/   PB, AFIG01(121), AFIG02(121)
      COMMON /IWING/   PW, AFIG03(121), AFIG04(121)
      COMMON /IHT/    PHT, AFIG05(121), AFIG06(121)
      COMMON /IVT/    PVT, AFIG07(121), AFIG08(121)
      COMMON /IVF/    PVF, AFIG09(121), AFIG10(121)
      COMMON /IBW/    PBW, AFIG11(121), AFIG12(121)
      COMMON /IBH/    PBH, AFIG13(121), AFIG14(121)
      COMMON /IBV/    PBV, JFIG01(121), JFIG02(121)
      COMMON /IBWH/  PBWH, JFIG03(121), JFIG04(121)
      COMMON /IBWV/  PBWV, JFIG05(121), JFIG06(121)
      COMMON /IBWHV/PBWHV, JFIG07(121), JFIG08(121)
      COMMON /WINGD/       JFIG09(121)
      COMMON /SBETA/       JFIG10(121), JFIG11(121), JFIG12(121)
      COMMON /BDATA/       JFIG13(121), JFIG14(121), JFIG15(121),
     1                     JFIG16(121), JFIG17(121)
      COMMON /WHWB/        JFIG18(121)
      COMMON /WBHCAL/      JFIG19(121)
      COMMON /HTDATA/      JFIG20(121)
      COMMON /VTDATA/ LFIGN(121),LFIGO(121),IOVLY,IOVL,NSTQ,NSTP,FINALR,
     1                NFIG,IFIGN
      COMMON /WHAERO/ LFIGS(121), IFIGST(20), IEXCD(4), MSSCL(2),
     1                ROUT(2), XLL(4), XUL(4), IEXTRL(4,2),
     2                IEXTRU(4,2), XVAL(4), LDUM(121)
      DIMENSION IFIG(20,121)
      DATA IHBL/4H    /
      DO 1000 I=1,121
      AFIG01(I)=0.
      AFIG02(I)=0.
      AFIG03(I)=0.
      AFIG04(I)=0.
      AFIG05(I)=0.
      AFIG06(I)=0.
      AFIG07(I)=0.
      AFIG08(I)=0.
      AFIG09(I)=0.
      AFIG10(I)=0.
      AFIG11(I)=0.
      AFIG12(I)=0.
      AFIG13(I)=0.
      AFIG14(I)=0.
      JFIG01(I)=0
      JFIG02(I)=0
      JFIG03(I)=0
      JFIG04(I)=0
      JFIG05(I)=0
      JFIG06(I)=0
      JFIG07(I)=0
      JFIG08(I)=0
      JFIG09(I)=0
      JFIG10(I)=0
      JFIG11(I)=0
      JFIG12(I)=0
      JFIG13(I)=0
      JFIG14(I)=0
      JFIG15(I)=0
      JFIG16(I)=0
      JFIG17(I)=0
      JFIG18(I)=0
      JFIG19(I)=0
      JFIG20(I)=0
      LFIGN(I)=0
      LFIGO(I)=0
      LFIGS(I)=0
      LDUM(121)=0
 1000 CONTINUE
      DO 1010 I=1,4
         IEXCD(I)=0
         XLL(I)=0.
         XUL(I)=0.
         IEXTRL(I,1)=0
         IEXTRU(I,1)=0
         IEXTRL(I,2)=0
         IEXTRU(I,2)=0
         XVAL(I)=0.
 1010 CONTINUE
      DO 1020 I=1,2
         ROUT(I)=IHBL
         MSSCL(I)=IHBL
 1020 CONTINUE
      DO 1030 I=1,20
         IFIGST(I)=IHBL
 1030 CONTINUE
      IOVLY=999
      IOVL=0
      NSTQ=0
      NSTP=0
      FINALR=0.
      NFIG=0
      IFIGN=0
      ROUTL=IHBL
      MSSCLL=IHBL
      DO 1040 I=1,121
         DO 1040 J=1,20
            IFIG(J,I)=IHBL
 1040 CONTINUE
      RETURN
      END
