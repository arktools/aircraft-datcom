      SUBROUTINE DFLCON(AA,DD,AFORLF,BETA,INBORD,TAPERD)
C
C***  CALCULATES SUPERSONIC LIFT, ROLL MOMENT AND HINGE MOMENT
C     DERIVATIVES
C
      COMMON /CONSNT/ PI,DR,UNUSED,RAD
      COMMON /POWR/   SPR(59)
      LOGICAL INBORD,TAPERD
      DIMENSION P(2),TCP(2),PA(2),TCPA(2),PB(2),RCPB(2),PC(2),TCPC(2)
      DIMENSION PR(16)
      EQUIVALENCE (SPR(12),CLD),(SPR(13),CLLD),(SPR(17),CMD),
     1            (SPR(18),CHD)
      EQUIVALENCE (PSL1,PR(1 )),(PSLX1,PR(2)),(PSLY1,PR(3)),(PSLX1A,
     1            PR(4)),(PSLX1B,PR(5)),(PSLX1C,PR(6)),(PSL2,PSL3,
     2            PR(7)),(PSLY2,PSLY3,PR(8)),(PSLX2A,PSLX3A,PR(9)),
     3           (PSLX2B,PSLX3B,PR(10)),(PSLX2C,PSLX3,PR(11)),(PSLX2,
     4            PR(12)),(PSL1A,PR(13)),(PSL1B,PR(14)),(PSLY1A,PR(15))
     5          ,(PSLY1B,PR(16))
C
      SQRA(X)=SQRT(ABS(X))
      P(1)=0.5
      P(2)=0.5
      P3=0.5
      A=AA
      D=DD
      TWO3RD=2./3.
      A2=A**2
      OPA2=1.+A2
      OMA2=1.-A2
      TCP0OD=4./(RAD*BETA*SQRA(OMA2))
      A4=A2**2
      SQ3=SQRA(OMA2)
      OMA4=1.-A4
      IF(.NOT.TAPERD)GO TO 1110
      FLD=AFORLF
C
C     ----HERE FOR TAPERED
C
      IF(ABS(D).LE.1.0)GO TO 1000
      GO TO 1320
 1000 IF(FLD.EQ.0.0)  FLD=1.E-04
      D2=D**2
      OMD2=1.-D2
      SQ1=SQRA(OMA2*OMD2)
      SQ2=SQRA(OMA2/OMD2)
      OPD2=1.+D2
      SAVFLD=FLD
      DO 1080 K=1,2
         APD=A+D
         OPA=1.+A
         OPD=1.+D
         AMD=A-D
         OMA=1.-A
         OMD=1.-D
         FLD2=FLD**2
         AD=A*D
         OPAD=1.+AD
         OMAD=1.-AD
         OMFLD=1.-FLD
         AMD2=AMD**2
C
C     ----SIGN LOGIC
C
         IF(AMD.LT.0.0)GO TO 1020
         SIGN=-1.
         SIGN2=1.
         GO TO 1030
 1020    SIGN=1.
         SIGN2=-1.
C
C     ----COMPUTE INTERMEDIATE STUFF.
C
 1030    G1=1./(FLD*OMD-OMA)
         G2=FLD*OMAD
         G3=FLD*AMD
         G4=FLD/PI*ARCCOS((OMA2-G2)/G3)
         G5=SQ2/PI*ARCCOS((OMAD-FLD*OMD2)/AMD)
         G6=2.*FLD*OMAD-FLD2*OMD2-OMA2
         G7=OMFLD*SQ3/PI*ALOG(ABS((A-FLD*D+SIGN*SQRA(G6))/OMFLD))
         G8=ARCCOS(D)/PI
         G9=ARCCOS(A)/PI
         G10=1.+3.*(AD-D2)-AD*D2
         G11=OPD/OMD*(OMA2*OMD2-2.*D*OMA**2)
         G115=.5/(FLD*OMD-OMA)
         G12=G115/OPD
         G13=AMD*OPD
         G14=2.*FLD*OPAD
         G15=SQRA(OMA2*G6)/PI
         G16=1./(OPD*AMD2)
         G17=SQ2*(1.-G8)
         P(K)=.5*(SQ1-OMA*OPD)/AMD
         PA(K)=OMD*G1*(G4-G5)
         PB(K)=(AMD*G4+G7)*G1
         PC(K)=OMD/AMD*(G17+G9-OMA/OMD)
         TCP(K)=.25/(P(K)*AMD2)*(SQ2*G10-G11)
         TCPA(K)=G12/(PA(K)*AMD)*(OMD2*(2.*OPAD-FLD*OPD2)*G4-G5*G10+
     1           SIGN*OPD2*G15)
         RCPB(K)=G115/PB(K)*(AMD*(2.*A-FLD*APD)*G4/OMFLD+SIGN *G15+A*G7)
         TCPC(K)=.5*G16/PC(K)*(G10*G17+AMD*OPD2*SQ3/PI-G11+OMD2*(1.+2.*
     1           AD-D2)*G9)
         IF(K.EQ.2)GO TO 1070
C
C     ----BYPASS 3S IF CONTROL INBOARD OF WING TIP.
C
         IF(INBORD)GO TO 1040
         T2=2.+A+D
         SQ4=SQRA(OPA*OPD)
         T1=1.0/(OPD-FLD*OPA)
         T3=ARCCOS((T2-2.*FLD*OPA)/AMD)/PI
         T4=FLD*ARCCOS((2.*OPD-FLD*T2)/(FLD*AMD))/PI
         T45=OMFLD*(FLD*OPA-OPD)
         SQ5=2.*SQRA(OPA*T45)/PI
         T5=(2.- AMD)*OMD2+2.*(AD*OPD+D*OPA)
         P3=(OPA-SQ4)/AMD
         P3A=T1*(OPD*T3-SQ4*T4)
         P3B=-T1*(AMD*T3+SIGN *SQ5)
         TCP3=.25*G16/P3 *(SQ4*T5-2.*(OMA2*OMD2+2.*D*(OPA**2)))
         TCP3A=-.25*T1/AMD*(2.*OPD/FLD*(2.*FLD*(1.+AD)-OPD2)*T3+SIGN2*
     1         OPD2*SQ5-T4*SQRA(OPA/OPD)*T5) /P3A
         RCP3B=1./(6.*P3B*T45)*(3.*AMD*(2.*A*FLD-(A+D))*T3+SIGN2*(2.*
     1         FLD*(1.-2.*A)-(2.-3.*A-D))*SQ5)
 1040    OPFLD=1.+FLD
         OMFLD2=1.-FLD2
         SL1=2.*AMD/(OMFLD2*OMD2)
         SL2=FLD2*SL1
         SL1A=FLD*OMD-OMA
         SL1B=SL1A/(OPFLD*AMD)
         SL1A=SL1A/(OMFLD2*OMD)
         SL3=.5*OMD2*SL2/OPD
         SL0=(OMA/OMD-FLD2*OPA/OPD)/OMFLD2
         H1=TWO3RD*AMD/(OMFLD*OPD2)
         SLBY1=SL1*H1*(TCP(1)-D)
         SLBY1A=SL1A*H1*(TCPA(1)-D)
         SLBY1B=TWO3RD*SL1B
         IF(INBORD)GO TO 1050
         SLBY3=SL3*(1.-H1*FLD*(TCP3+D))
 1050    SLY0=(OPA*OMFLD-AMD2*AMD/((OMFLD*OMD)**2)-(OPD-FLD*OPA)**3/
     1        ((OMFLD*OPD)**2))/(3.*AMD*OPFLD)
         FLD3=FLD2*FLD
         OMFLD3=1.-FLD3
         RAT=OMFLD2/OMFLD3
         RAT1=RAT/OPD2
         RAT2=AMD/(OMFLD3*OPD2)
         RAT3=1./(OMFLD3*OPD*OPD2)
         SLX0=.5*((OMA/OMD)**2-FLD3*(OPA/OPD)**2)/OMFLD3
         SLBX1=SL1*RAT1*(OPAD-AMD*TCP(1))
         SLBX1A=SL1A*RAT1*(OPAD-AMD*TCPA(1))
         SLBX1B=SL1B*RAT*(OMFLD*(RCPB(1)-A))/AMD
         SLBX1C=RAT2*(OPAD-AMD*TCPC(1))/OMD
         IF(INBORD)GO TO 1060
         SLBX3=FLD3*RAT2*(OPAD+AMD*TCP3)/OPD
         SLBX3A=RAT3*FLD2*(FLD*OPA-OPD)*(OPAD+AMD*TCP3A)
         SLBX3B=OMFLD**2*(FLD*OPA-OPD)*(RCP3B+A)/(OMFLD3*(AMD**2))
C
C     ----BYPASS 2S IF WING TIP CONTROL.
C
         GO TO 1160
 1060    FLD=1./FLD
 1070    A=-A
         D=-D
 1080 CONTINUE
      FLD=SAVFLD
      FLD2=FLD**2
      OMFLD=1.-FLD
      OPAD=1.+A*D
      AMD=A-D
      OPD2=1.+D**2
      OPA=1.+A
      OPD=1.+D
      SLBY2=SL2*(1.-H1*FLD*(TCP(2)+D))
      SLBX2=SL2*FLD*RAT*(OPAD+AMD*TCP(2))/OPD2
      TMP=FLD*OPA-OPD
      SLBX2A=FLD2*TMP*(OPAD+AMD*TCPA(2))*RAT3
      SLBX2B=OMFLD**2*TMP*(RCPB(2)+A)/(OMFLD3*(AMD**2))
      SLBX2C=FLD3*AMD*(OPAD+AMD*TCPC(2))*RAT3
      GO TO 1160
C
C     ----HERE FOR UNTAPERED.
C
 1110 FLD=AFORLF
      FLD2=FLD**2
      DO 1140 K=1,2
         OPA=1.+A
         OMA=1.-A
         G1=1.+2.*A*FLD-OMA2*FLD2
         G2=OMA2*FLD-A
         G3=ARCCOS(G2)
         G4=1./((1.-OMA*FLD)*PI)
         G5=FLD*SQ3
         G6=SQRA(G1)
         G7=SQRA(OMA2*G1)
         G8=G4/OPA
         G9=ALOG(ABS(1.+A*FLD-G6)/FLD)
         G10=ARCCOS(A)
         G11=1.+A/PI*G10-SQ3/PI
         G12=G8/(4.*OMA2)
         G13=7.*A2-2.*A4+1.
         G14=2.*A+OPA2*FLD
         G15=2.*FLD*OMA2**2
         G16=A*(7.-A2)
         G17=1./(OPA*OMA2)
         PA(K)=G8*(G7-G2*G3)
         PB(K)=G4*(G3+G5*G9)
         PC(K)=G11/OPA
         TCP(K)=(8.*A+OPA2)/(4.*OMA2)
         TCPA(K)=G12/PA(K)*((G13-G14*G15)*G3+(G16+OMA4*FLD)*G7)
         RCPB(K)=G4/(2.*PB(K))*((1.+2.*A*FLD)*G3/FLD-G7+A*G5*G9)
         TCPC(K)=G17/(4.*PC(K))*(8.*A+OPA2+G13/PI*G10-G16/PI*SQ3)
         IF(K.EQ.2) GO TO 1130
         IF(INBORD) GO TO 1120
         T1=1.-OPA*FLD
         T2=2.*FLD*OPA-1.
         T3=5.*A2+8.*A-3.
         T4=OPA*T1
         T45=SQRA(T4*FLD)
         T5=ARCCOS(T2)
         P3A=0.5/(T1*PI)*(2.*T45-T2*T5)
         P3B=1./(PI*T1)*(T5-2.*T45)
         TCP3=(5.-8.*A-3.*A2)/(8.*OPA)
         TCP3A=-1./(T4*16.*P3A*PI)*((T3+8.*FLD*OPA**2*(FLD*OPA2-2.*A))
     1      *T5+2.*(T3-2.*FLD*OPA*OPA2)*T45)
         RCP3B=1./(6.*P3B*FLD*T1*PI)*(3.*(1.-2.*A*FLD)*T5-2.*(1.+2.*FLD*
     1      (1.-2.*A))*T45)
         SL3=1./(2.*FLD*OPA)
         SLBY3=SL3*(1.-(5./(12.*FLD*OPA)))
         SLBX3=TWO3RD*SL3
         SLBX3A=(1.-FLD*OPA)/(3.*FLD*OPA)
         SLBX3B=FLD*(RCP3B+A)*(1.-FLD*OPA)/3.
 1120    SL1=1./(FLD*OMA2)
         SL1A=(1.-FLD*OMA)/(2.*FLD*OMA)
         SL1B=(1.-FLD*OMA)/2.
         SLBY1=SL1*(1.+4.*A)/(6.*FLD*OMA2)
         SLBY1A=SL1A*TWO3RD*(TCPA(1)-A)/(FLD*OPA2)
         SLBY1B=TWO3RD*SL1B
         SLBX1=TWO3RD*SL1
         SLBX1B=TWO3RD*SL1B*FLD*(RCPB(1)-A)
         SLBX1A=TWO3RD*SL1A
         SLBX1C=1./(3.*FLD*OMA)
         IF(.NOT.INBORD) GO TO 1150
         A=-A
         GO TO 1140
 1130    A=-A
         OMA=1.-A
         OPA=1.+A
         SL2=SL1
         SLBY2=SL2*(1.-(1.-4.*A)/(6.*FLD*OMA2))
         SLBX2=TWO3RD*SL2
         SLBX2A=(1.-FLD*OPA)/(3.*FLD*OPA)
         SLBX2B=FLD*(RCPB(2)+A)*(1.-FLD*OPA)/3.
         SLBX2C=1./(3.*FLD*OPA)
 1140 CONTINUE
 1150 SL0=(FLD*OMA2-1.)/(FLD*OMA2)
      SLY0=(3.*FLD*OMA*OMA2*(FLD*OPA-1.)   -4.*A)/(6.*FLD2*OMA2**2)
      SLX0=(3.*FLD*OMA2-4.)/(6.*FLD*OMA2)
C
C     ----SET ALL PARTS OF GENERAL EQUATION TO 0.
C
 1160 DO 1170 I=1,16
         PR(I)=0.
 1170 CONTINUE
      PSL1=P(1)*SL1
      PSLX1=P(1)*SLBX1
      PSLY1=P(1)*SLBY1
      PSLX1C=PC(1)*SLBX1C
      PSLX1A=PA(1)*SLBX1A
      PSLX1B=PB(1)*SLBX1B
      IF(.NOT.TAPERD) FLD=1./FLD
      TESTR1=1.-A
      TESTR2=1.+A
      IF(.NOT.TAPERD)GO TO 1180
      TESTR1=(1.-A)/(1.-D)
      TESTR2=(1.+D)/(1.+A)
 1180 IF(.NOT.INBORD)GO TO 1250
C
C     ----HERE FOR CONTROL LOCATED INBOARD FROM WING TIP.
C
      PSL2=P(2)*SL2
      PSLY2=P(2)*SLBY2
      PSLX2=P(2)*SLBX2
      PSLX2C=PC(2)*SLBX2C
      PSLX2A=PA(2)*SLBX2A
      PSLX2B=PB(2)*SLBX2B
      CLD=SL0+PSL1+PSL2
      CMD=-(SLX0+PSLX1+PSLX2)
      CLLD=SLY0+PSLY1+PSLY2
      CHD=-(SLX0+PSLX1C+PSLX2C)
      IF(.NOT.TAPERD) GO TO 1190
      IF(FLD.LT.1.) GO TO 1190
      IF(FLD.GE.TESTR1) GO TO 1210
      GO TO 1200
 1190 IF(FLD.LE.TESTR1) GO TO 1210
 1200 CHD=CHD-(PSLX1A-PSLX1B)
 1210 IF (.NOT.TAPERD) GO TO 1220
      IF(FLD.LT.1.) GO TO 1220
      IF(FLD.GE.TESTR2) GO TO 1240
      GO TO 1230
 1220 IF(TESTR2.GE. FLD) GO TO 1240
 1230 CHD=CHD-(PSLX2A-PSLX2B)
 1240 CONTINUE
      CMD=TCP0OD*CMD
      CLD=TCP0OD*CLD
      CLLD=TCP0OD*CLLD
      CHD=TCP0OD*CHD
      GO TO 1320
C
C     ----HERE FOR CONTROL LOCATED AT THE WING TIP.
C
 1250 PSL1A=PA(1)*SL1A
      PSL1B=PB(1)*SL1B
      PSL3=P3*SL3
      PSLX3=P3*SLBX3
      PSLY1A=PA(1)*SLBY1A
      PSLY1B=PB(1)*SLBY1B
      PSLY3=P3*SLBY3
      PSLX3A=P3A*SLBX3A
      PSLX3B=P3B*SLBX3B
      CLD=SL0+PSL1+PSL3
      CMD=-(SLX0+PSLX1+PSLX3)
      CLLD=SLY0+PSLY1+PSLY3
      CHD=-(SLX0+PSLX1C+PSLX3)
      IF(.NOT.TAPERD) GO TO 1270
      IF(FLD.LT.1.) GO TO 1270
      IF(FLD.GE.TESTR1) GO TO 1290
      GO TO 1280
 1270 IF(FLD.LE.TESTR1) GO TO 1290
 1280 CLD=CLD+PSL1A-PSL1B
      CMD=CMD-(PSLX1A-PSLX1B)
      CLLD=CLLD+PSLY1A-PSLY1B
      CHD=CHD-(PSLX1A-PSLX1B)
 1290 IF(.NOT.TAPERD) GO TO 1300
      IF(FLD.LT.1.) GOTO 1300
      IF(FLD.GE.TESTR2) GO TO 1240
      GO TO 1310
 1300 IF(TESTR2.GE.FLD) GO TO 1240
 1310 CHD=CHD-(PSLX3A-PSLX3B)
      GO TO 1240
 1320 RETURN
      END
