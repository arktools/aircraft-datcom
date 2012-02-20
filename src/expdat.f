      SUBROUTINE EXPDAT
C
C     LOAD THE EXPERIMENTAL DATA NAMELISTS FOR THE CURRENT MACH
C     NUMBER ON TAPE-10
C
      COMMON /IBODY/ PB, CDB(20), CLB(20), CMB(20), CNB(20), CAB(20),
     1               CLAB(20), CMAB(20)
      COMMON /IWING/ PW, CDW(20), CLW(20), CMW(20), CNW(20), CAW(20),
     1               CLAW(20), CMAW(20)
      COMMON /IHT/   PH, CDH(20), CLH(20), CMH(20), CNH(20), CAH(20),
     1               CLAH(20), CMAH(20)
      COMMON /IVT/   PV, CDV
      COMMON /IBW/   PBW, CDWB(20), CLWB(20), CMWB(20), CNWB(20),
     1               CAWB(20), CLAWB(20), CMAWB(20)
      COMMON /IDWASH/ PDW, QOQINF(20), EPSLON(20), DEODA(20)
C
      COMMON /EXPER/ KLIST, NLIST(100), NNAMES, IMACH, MDATA,
     1               KBODY, KWING, KHT, KVT, KWB, KDWASH(3),
     2               ALPOW, ALPLW, ALPOH, ALPLH
      COMMON /OVERLY/ NLOG,NMACH,I,NALPHA
C
      COMMON /CONSNT/ PI,DEG,UNUSED,RAD
      COMMON /ERROR/  LOG(81), LIST
C
      DIMENSION KOL(20), KLOG(9)
      DIMENSION BO(400), WG(400), HT(380), VT(380), BW(380), DW(60)
      LOGICAL MDATA, KBODY, KWING, KHT, KVT, KWB, KDWASH, KLOG, LIST
      EQUIVALENCE (KLOG(1),MDATA), (BO(1),CDB(1)), (WG(1),CDW(1)),
     1         (HT(1),CDH(1)), (VT(1),CDV), (BW(1),CDWB(1)),
     2         (DW(1),QOQINF(1))
C
C     INITIALIZE FLAGS AND DATA
C
      DO 1000 J=1,9
         KLOG(J) = .FALSE.
 1000 CONTINUE
      REWIND 8
      REWIND 10
      NNAMES = 0
      ALPOW = UNUSED
      ALPOH = UNUSED
      ALPLW = UNUSED
      ALPLH = UNUSED
      DO 1040  J=1,KLIST
         NCARD = NLIST(J)/1000
         IMACH = NLIST(J)-1000*NCARD
         IF(IMACH .NE. I) GO TO 1020
C
C         LOAD THIS NAMELIST ON TAPE 10
C
         NNAMES = NNAMES+1
         DO 1010 K=1,NCARD
            READ(8,1080) (KOL(L),L=1,20)
            WRITE(10,1080) (KOL(L),L=1,20)
 1010    CONTINUE
 1020    CONTINUE
      IF(IMACH .EQ. I) GO TO 1040
C
C         SKIP PAST THIS NAMELIST
C
         DO 1030 K=1,NCARD
            READ(8,1080) KOL(1)
 1030    CONTINUE
 1040 CONTINUE
C
C     READ EXPERIMENTAL DATA AND SET FLAGS
C
      IF(NNAMES .EQ.0) GO TO 1060
          MDATA = .TRUE.
          CALL EXSUBT
          DO 1050 J=1,5
             K = 1+20*(J-1)
             IF(J .GE. 4) K = K+40
             IF(BO(K) .NE. UNUSED) KBODY = .TRUE.
             IF(WG(K) .NE. UNUSED) KWING = .TRUE.
             IF(HT(K) .NE. UNUSED) KHT   = .TRUE.
             IF(VT(K) .NE. UNUSED) KVT   = .TRUE.
             IF(BW(K) .NE. UNUSED) KWB   = .TRUE.
             IF(J .GT. 3) GO TO 1050
             IF(DW(K) .NE. UNUSED) KDWASH(J) = .TRUE.
 1050     CONTINUE
 1060 CONTINUE
C
C     WRITE OUT EXPERIMENTAL DATA FOR THIS MACH
C
      IF(.NOT. MDATA) GO TO 1070
      IF(.NOT. LIST ) GO TO 1070
          WRITE(6,1090)
          CALL XNAM23(1)
          WRITE(6,1090)
 1070 CONTINUE
 1080 FORMAT(20A4)
 1090 FORMAT(1H1)
      RETURN
      END
