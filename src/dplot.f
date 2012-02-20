        PROGRAM DATPLOT
C**************************************************************************
C* DATPLOT - A PROCEDURE FOR PLOTTING DATCOM AIRCRAFT CONFIGURATIONS      *
C**************************************************************************
C* THIS PROGRAM WAS MODIFIED FROM THE SOURCE CODE CONTAINED IN            * 
C* NASA TM-84639 "A COMPUTER PROGRAM FOR OBTAINING AIRPLANE CONFIGURATION *
C* PLOTS FROM DIGITAL DATCOM INPUT DATA", M.L. Roy and S.M. Sliwa, 3/93   *
C*                                                                        *
C* The modifications were performed by: James M. Simon                    *
C*                                        WL/FIGC                         *
C*                           last update: 2/8/95                          *
C**************************************************************************
C
CPROGRAM DATPLTS ASSUMES THAT ALL VARIABLES AND ARRAYS ARE INITIALIZED TO
CTHIS IS ACCOMPLISHED THROUGH PRESET=ZERO, AT LOADING TIME.
C
C
        COMMON/WG/NWAF,NWAFOR,XW,ZW,ALIW
        COMMON/FUSL/NRADX,NFORX,NX,ZU,ZL,R,X,S
        COMMON/FIN/XV,ZV,NF,YV,PHIV
        COMMON/CANARD/XH,ZH,ALIH
        COMMON/JAYS/J0,J1,J2,J3,J4,J5,J6,J7
        COMMON/NS/NP,NPODOR,NCAN,TNAME
        COMMON/RUNS/RUNTYPE,DEFLT
        COMMON/JET/NENGSJ,JIALOC,JELLOC,JEVLOC,JEALOC,AIETLJ,JINLTA
        COMMON/PROPEL/PHALOC,YP,PHVLOC,PRPRAD,NENGSP
        COMMON/VEN/XVF,ZVF,YVF,PHIVF
        INTEGER RUNTYPE      
        CHARACTER*80 TNAME
        DIMENSION NRADX(4),NFORX(4)
        LOGICAL VERTUP
        REAL NX,NENGSJ,JIALOC,JELLOC,JEVLOC,JEALOC,JINLTA
        NAMELIST/SYNTHS/XCG,ZCG,XW,ZW,ALIW,XH,ZH,ALIH,XV,XVF,ZV,
     &ZVF,SCALE,VERTUP,HINAX,PHIV,YV,YVF,PHIVF
C OPENING INPUT FILE
      OPEN(4,FILE='FOR005.DAT',STATUS='OLD')
C CALL OPTIONS      
      CALL OPTIONS
C READ NAMELIST SYNTHS - REQUIRED INPUT!!      
        READ(4,SYNTHS)
        REWIND 4 
C
      WRITE(7,1001)         
1001  FORMAT(1X,15HVARIABLES=X,Y,Z)
        IF(J1.EQ.1)  CALL WRITWNG 
        IF(J2.NE.0)  CALL WRITFUS(J2) 
        IF(J3.EQ.1)  CALL WRITPOD(J6) 
        IF(J4.EQ.1)  CALL WRITFIN       
        IF(J5.EQ.1)  CALL WRITCAN
        IF(J7.EQ.1)  CALL WRITVEN
C
      STOP
100   FORMAT(7X,8A10)
101   FORMAT(A1)
102   FORMAT(A6,1X,A120)
110   FORMAT(24I3)
112   FORMAT(8A10)
      END
        SUBROUTINE WRITCAN
C SUBROUTINE WRITCAN WRITES DATA FOR THE VERTICAL PANELS ON THE HORIZONTAL TAIL
C AND FOR THE HORIZONTAL TAIL.
        COMMON/CANARD/XH,ZH,ALIH      
        REAL X(20,3),Y(20,3),Z(20,3)
        DIMENSION XPRCT(10),YPRCT(10),
     &  SHB(20),SEXT(20),RLPH(20),SVWB(20),SVB(20),SVHB(20)
        NAMELIST/HTPLNF/CHRDTP,SSPNOP,SSPNE,SSPN,CHRDBP,CHRDR,
     &  SAVSI,SAVSO,CHSTAT,TWISTA,SSPNDD,DHDADI,DHDADO,TYPE,
     &  SHB,SEXT,RLPH,SVWB,SVB,SVHB
      DATA XPRCT/0.,1.25,5.,10.,15.,20.,30.,50.,70.,100./
      DATA YPRCT/0.,.947,1.777,2.341,2.673,2.869,3.001,2.82,1.832,.063/
        REWIND 4
        READ(4,HTPLNF)
        YH=0
C PERCENT CHORD LOCATIONS ALONG AIRFOIL SECTION,WHERE
C AIRFOIL THICKNESS WILL BE DEFINED.
C        WRITE(7,130) WXORD
C        YW=0
C        WRITE(7,100)XW,YW,ZW,CHRDR
C CALCULATING THE X,Y, AND Z COORDINATES AT THE ROOT
      DO I=1,10
        X(I,1)=XH+(XPRCT(I)/100.)*CHRDR
        Y(I,1)=YH
        Z(I,1)=ZH+(YPRCT(I)/100.)*CHRDR
        X(21-I,1)=X(I,1)
        Y(21-I,1)=Y(I,1)
        Z(21-I,1)=ZH-(YPRCT(I)/100.)*CHRDR
      END DO
C IF THE WING HAS AN OUTBOARD SECTION, THE COORDINATES OF THE BREAKPOINT
C ARE CALCULATED AND WRITTEN ON TAPE7.
        IF(SSPNOP.EQ.0)GO TO 200
        SSPNI=SSPN-SSPNOP
        XBP=XH+SSPNI*(TAN(SAVSI/57.296)+CHSTAT*(CHRDR-CHRDBP)/SSPNI)
        YBP=SSPNI
        ZBP=ZH+SSPNI*TAN(DHDADI/57.296)
        DO I=1,10
          X(I,2)=XBP+(XPRCT(I)/100.)*CHRDBP
          Y(I,2)=YBP
          Z(I,2)=ZBP+(YPRCT(I)/100.)*CHRDBP
          X(21-I,2)=X(I,2)
          Y(21-I,2)=Y(I,2)
          Z(21-I,2)=ZBP-(YPRCT(I)/100.)*CHRDBP
        END DO
C        WRITE(7,100)XBP,YBP,ZBP,CHRDBP
C  THE COORDINATES OF THE WING TIP ARE CALCULATED, AND WRITTEN ON TAPE7.  
      XTIP=XBP+SSPNOP*(TAN(SAVSO/57.296)+CHSTAT*(CHRDBP-CHRDTP)/SSPNOP)
      YTIP=SSPN
      ZTIP=ZBP+SSPNOP*TAN(DHDADO/57.296)
        DO I=1,10
          X(I,3)=XTIP+(XPRCT(I)/100.)*CHRDTP
          Y(I,3)=YTIP
          Z(I,3)=ZTIP+(YPRCT(I)/100.)*CHRDTP
          X(21-I,3)=X(I,3)
          Y(21-I,3)=Y(I,3)
          Z(21-I,3)=ZTIP-(YPRCT(I)/100.)*CHRDTP
        END DO
        JMAX=3
        GO TO 201
C FOR NO OUTBOARD WING SECTION, THE WING TIP COORDINATES ARE CALCULATED
C AND WRITTEN ON TAPE7, TWICE, SIMULATING AN OUTBOARD SECTION OF ZERO LENGTH
200     XTIP=XH+SSPN*(TAN(SAVSI/57.296)+CHSTAT*(CHRDR-CHRDTP)/SSPN)
        YTIP=SSPN
        ZTIP=ZH+SSPN*TAN(DHDADI/57.296)
        DO I=1,10
          X(I,2)=XTIP+(XPRCT(I)/100.)*CHRDTP
          Y(I,2)=YTIP
          Z(I,2)=ZTIP+(YPRCT(I)/100.)*CHRDTP
          X(21-I,2)=X(I,2)
          Y(21-I,2)=Y(I,2)
          Z(21-I,2)=ZTIP-(YPRCT(I)/100.)*CHRDTP
        END DO
        JMAX=2
C        WRITE(7,100) XTIP,YTIP,ZTIP,CHRDTP
201     WRITE(7,100) JMAX
100   FORMAT(1X,21HZONE T="R HT" I=20 J=,I1,1X,3HK=1,1X,7HF=BLOCK)
      DO J=1,JMAX
        WRITE(7,*) (X(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (Y(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (Z(I,J),I=1,20)
      END DO  
      WRITE(7,103) JMAX                 
103   FORMAT(1X,21HZONE T="L HT" I=20 J=,I1,1X,3HK=1,1X,7HF=BLOCK)
      DO J=1,JMAX
        WRITE(7,*) (X(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (-1.*Y(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (Z(I,J),I=1,20)
      END DO  
        RETURN
        END
        SUBROUTINE WRITFIN
C  AIL DATA ON TAPE7.
C  SUBROUTINE WRITFIN WRITES VERTICAL TAIL DATA
        COMMON/FIN/XV,ZV,NF,YV,PHIV       
        REAL X(20,3),Y(20,3),Z(20,3)
        DIMENSION XPRCT(10),YPRCT(10),
     &  SHB(20),SEXT(20),RLPH(20),SVWB(20),SVB(20),SVHB(20)
        NAMELIST/VTPLNF/CHRDTP,SSPNOP,SSPNE,SSPN,CHRDBP,CHRDR,
     &  SAVSI,SAVSO,CHSTAT,TWISTA,SSPNDD,DHDADI,DHDADO,TYPE,
     &  SHB,SEXT,RLPH,SVWB,SVB,SVHB
      DATA XPRCT/0.,1.25,5.,10.,15.,20.,30.,50.,70.,100./
      DATA YPRCT/0.,.947,1.777,2.341,2.673,2.869,3.001,2.82,1.832,.063/
        REWIND 4
        READ(4,VTPLNF)
        REWIND 4
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        DO I=1,10
          X(I,1)=XV+(XPRCT(I)/100.)*CHRDR
          Y(I,1)=YV+(YPRCT(I)/100.)*CHRDR
          Z(I,1)=ZV
          X(21-I,1)=X(I,1)
          Z(21-I,1)=Z(I,1)
          Y(21-I,1)=YV-(YPRCT(I)/100.)*CHRDR
        END DO        
        IF(SSPNOP.EQ.0)GO TO 200
        SSPNI=SSPN-SSPNOP
        XBP=XV+SSPNI*(TAN(SAVSI/57.296)+CHSTAT*(CHRDR-CHRDBP)/SSPNI)
        YBP=YV+SSPNI*SIN(PHIV/57.296)
        ZBP=ZV+SSPNI*COS(PHIV/57.296)
        DO I=1,10
          X(I,2)=XBP+(XPRCT(I)/100.)*CHRDBP
          Y(I,2)=YBP+(YPRCT(I)/100.)*CHRDBP
          Z(I,2)=ZBP
          X(21-I,2)=X(I,2)
          Y(21-I,2)=YBP-(YPRCT(I)/100.)*CHRDBP
          Z(21-I,2)=Z(I,2)
        END DO
C        
      XTIP=XBP+SSPNOP*(TAN(SAVSO/57.296)+CHSTAT*(CHRDBP-CHRDTP)/SSPNOP)
      YTIP=YBP+SSPNOP*SIN(PHIV/57.296)
      ZTIP=ZBP+SSPNOP*COS(PHIV/57.296)
        DO I=1,10
          X(I,3)=XTIP+(XPRCT(I)/100.)*CHRDTP
          Y(I,3)=YTIP+(YPRCT(I)/100.)*CHRDTP
          Z(I,3)=ZTIP
          X(21-I,3)=X(I,3)
          Y(21-I,3)=YTIP-(YPRCT(I)/100.)*CHRDTP
          Z(21-I,3)=Z(I,3)
        END DO
        JMAX=3
        GO TO 201
C FOR NO OUTBOARD WING SECTION, THE WING TIP COORDINATES ARE CALCULATED
C AND WRITTEN ON TAPE7, TWICE, SIMULATING AN OUTBOARD SECTION OF ZERO LENGTH
200     XTIP=XV+SSPN*(TAN(SAVSI/57.296)+CHSTAT*(CHRDR-CHRDTP)/SSPN)
        YTIP=YV+SSPN*SIN(PHIV/57.296)
        ZTIP=ZV+SSPN*COS(PHIV/57.296)
        DO I=1,10
          X(I,2)=XTIP+(XPRCT(I)/100.)*CHRDTP
          Y(I,2)=YTIP+(YPRCT(I)/100.)*CHRDTP
          Z(I,2)=ZTIP
          X(21-I,2)=X(I,2)
          Y(21-I,2)=YTIP-(YPRCT(I)/100.)*CHRDTP
          Z(21-I,2)=Z(I,2)
        END DO
        JMAX=2
201     WRITE(7,100) JMAX                 
100   FORMAT(1X,23HZONE T="V TAIL" I=20 J=,I1,1X,3HK=1,1X,7HF=BLOCK)
      DO J=1,JMAX
        WRITE(7,*) (X(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (Y(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (Z(I,J),I=1,20)
      END DO                      
      IF(YV.NE.0)THEN
      WRITE(7,100) JMAX
        DO J=1,JMAX
          WRITE(7,*) (X(I,J),I=1,20)
        END DO  
        DO J=1,JMAX
          WRITE(7,*) (-Y(I,J),I=1,20)
        END DO  
        DO J=1,JMAX
          WRITE(7,*) (Z(I,J),I=1,20)
        END DO                      
      ENDIF
C        CALL WRITF2(XTIP,ZTIP)
        RETURN
        END
      SUBROUTINE WRITFUS(J2)
C SUBROUTINE WRITFUS WRITES FUSELAGE DATA ON TAPE7.
      DIMENSION Y(20,20),Z(20,20),ZU(20),ZL(20),R(20) 
      DIMENSION X(20),S(20),ZC(20),P(20)
      REAL NX
C      COMMON/FUSL/NRADX,NFORX,NX,ZU,ZL,R,X,S
      NAMELIST/BODY/NX,X,S,P,R,ZU,ZL,
     &   BNOSE,BTAIL,BLN,BLA,DS,ITYPE,METHOD
C READING IN NAMELIST BODY
      REWIND 4
      READ(4,BODY)
      REWIND 4      
C
      WRITE(7,150) INT(NX)                                             
150   FORMAT(1X,20HZONE T="FUSELAGE" I=,I2,1X,16HJ=20 K=1 F=BLOCK)
      DO KK=1,20 
        WRITE(7,160)(X(I),I=1,NX)
      END DO
160   FORMAT(10(1X,F7.2))      
C160   FORMAT(10(F7.2),T73,"X FUSLG")
C IF THE UPPER AND LOWER POINTS OF THE FUSELAGE ARE GIVEN, AN ELLIPTICAL
C CROSS SECTION WILL BE DRAWN, FITTED THROUGH THESE POINTS.
C OTHERWISE, A CIRCULAR FUSELAGE WILL BE DRAWN.
c
c      IF(J2.EQ.-1)GO TO 30
c
C THE CENTER OF EACH FUSELAGE CROSS SECTION IS CALCULATED.
      DO 302 I=1,NX
          ZC(I)=(ZU(I)+ZL(I))*.5
302   CONTINUE  
      PI=ACOS(-1.)
      DO 200 J=1,NX
        W=R(J)
        IF(W.EQ.0) W=SQRT(S(J)/PI)
        IF(ZU(J).EQ.0)THEN
          ZU(J)=W
          ZL(J)=-W
        ENDIF
        H=(ZU(J)-ZL(J))*.5
        WW=W*W
        IF(W.EQ.0)WW=W
        HH=H*H
        IF(H.EQ.0)HH=H
        WH=0
        IF(W.NE.0.AND.H.NE.0)WH=W*H
        GX2=3.1416/9
C THE Z AND Y COORDINATES OF 20 POINTS ALONG THE PERIPHERY OF THE
C HALF CROSS SECTION ARE CALCULATED.
        DO 201 K=1,10
          THETA=(K-1)*GX2
          RHO=0
          IF(WH.EQ.0)GO TO 13
          RHO=WH/((HH*(SIN(THETA)**2)+WW*(COS(THETA)**2))**0.5)
13        IF(J2.NE.-1)THEN
           Z(J,K)=RHO*COS(THETA)+ZC(J)
          ELSE
            Z(J,K)=SQRT(S(J)/ACOS(-1.))
          ENDIF
          Y(J,K)=RHO*SIN(THETA)
201     CONTINUE
200   CONTINUE        
        DO J=1,5
          WRITE(7,160) (Y(I,J),I=1,NX)
        END DO
        DO J=6,10
          WRITE(7,160) (Y(I,J), I=1,NX)
        END DO
        DO J=1,5
          WRITE(7,160) (-1.*Y(I,J), I=1,NX)
        END DO
        DO J=6,10
          WRITE(7,160) (-1.*Y(I,J), I=1,NX)
        END DO
        DO J=1,5
          WRITE(7,160) (Z(I,J),I=1,NX)
        END DO
        DO J=6,10
          WRITE(7,160) (Z(I,J), I=1,NX)
        END DO          
        DO J=1,5
          WRITE(7,160) (Z(I,J), I=1,NX)
        END DO
        DO J=6,10
          WRITE(7,160) (Z(I,J), I=1,NX)
        END DO
      RETURN
30    CONTINUE               
C WHEN ZU AND ZL ARE NOT DEFINED, CENTERS AND AREAS OF EACH FUSELAGE CROSS
C SECTION ARE WRITTEN ON TAPE7, FOR CIRCULAR CROSS SECTIONS.
C      WRITE(7,168)(ZC(I),I=1,NX)
C      WRITE(7,167)(S(I),I=1,NX)
      RETURN
      END
      SUBROUTINE OPTIONS
C SUBROUTINE OPTIONS DETERMINES THE CONTROL INTS WHEN THE DEFAULT VALUES
C FOR THE CONFIGURATION ARE NOT ACCEPTED.
        COMMON/FIN/XV,ZV,NF
        COMMON/NS/NP,NPODOR,NCAN,TNAME
        COMMON/JAYS/J0,J1,J2,J3,J4,J5,J6,J7
        COMMON/WG/NWAF,NWAFOR,XW,ZW,ALIW
        DIMENSION TNAME(8)
        PRINT*," TYPE THE PLOT TITLE:"
        READ 100,TNAME
100     FORMAT(8A10)
251     PRINT*," WING  : "
        PRINT*,"   TYPE 0  IF  NO WING ,"
        PRINT*,"     1  FOR WING ,"
        READ  *,J1
        IF(J1.EQ.1.OR.J1.EQ.0)GO TO 51
        PRINT*,J1," NOT VALID CHOICE.  PLEASE ENTER 0 OR 1:"
        GO TO 251
51      PRINT*," FUSELAGE"
        PRINT*," TYPE 0 IF NO FUSELAGE"
        PRINT*,"         1 FOR FUSELAGE,"
        READ*,JD
        IF(JD.EQ.1)J2=1
        IF(JD.EQ.0)J2=0
        IF(J2.EQ.0)GO TO 152
C IF FUSELAGE IS DESIRED, THE VALUE OF J2 DEFINED BY THE MAIN PROGRAM IS KEPT
        IF(JD.EQ.1)GO TO 152
        PRINT*,JD," NOT VALID CHOICE.  PLEASE ENTER 0 OR 1:"
        GO TO  51
152     PRINT*," POD  : "
        PRINT*,"   TYPE 0  IF  NO POD ,"
        PRINT*,"        1  FOR PODS,"
        READ*,J3
        IF(J3.EQ.0)GO TO 175
        IF(J3.EQ.1)GO TO 153
        PRINT*,J3," NOT VALID CHOICE.  PLEASE ENTER 0 OR 1:"
        GO TO 152
C        
153      CONTINUE
174     PRINT*, "  TYPE 1 FOR JET ENGINE POD ,"
        PRINT*, "       2 FOR PROP ENGINE POD "
        READ*, J6
        IF(J6.EQ.1)GO TO 175
        IF(J6.EQ.1)GO TO 175
        PRINT*, " NOT VALID CHOICE. PLEASE ENTER 1 OR 2:"
        GO TO 174
175     CONTINUE        
C IF(JD.EQ.1)GO  TO  154
         PRINT*," VERTICAL TAIL AND FINS :"
         PRINT*,"   TYPE 0  IF  NO VERTICAL TAIL, AND NO FIN , "
         PRINT*,"        1  FOR VERTICAL TAIL AND/OR FINS, "
         READ*,J4
         IF(J4.EQ.0)GO TO 75
         IF(J4.EQ.1)GO TO 75
         PRINT*,J4," NOT VALID CHOICE. PLEASE ENTER 0  OR  1:"
         GO TO  153
154      CONTINUE
75       PRINT*," VENTRAL FINS :"
         PRINT*,"   TYPE 0  IF NO VENTRAL FIN , "
         PRINT*,"        1  FOR VENTRAL FIN, "
         READ*, J7
         IF(J7.EQ.0)GO TO 54
         IF(J7.EQ.1)GO TO 54
         PRINT*,J7," NOT VALID CHOICE. PLEASE ENTER 0 OR 1:"
         GO TO 154
54       PRINT*," HORIZONTAL TAIL, OR CANARD "
         PRINT*,"   TYPE 0  IF  NO HORIZONTAL TAIL, AND NO CANARD,"
         PRINT*,"        1  FOR HORIZONTAL TAIL OR CANARD,"
         READ*,J5
         IF(J5.EQ.0)GO TO 155
         IF(J5.EQ.1)GO TO 155
         PRINT*,J5," NOT VALID CHOICE.  PLEASE ENTER 0 OR 1:"
        GO TO 54
155     CONTINUE
        RETURN
        END
      SUBROUTINE WRITPOD(J6)                         
      REAL X(4,10),Y(4,10),Z(4,10),Y2(4,10)
C SUBROUTINE WRITPOD WRITES POD DATA (1 OR 2 JET OR PROPELLER PODS) ON TAPE7. 
      COMMON/JET/NENGSJ,JIALOC,JELLOC,JEVLOC,JEALOC,AIETLJ,JINLTA 
      COMMON/PROPEL/PHALOC,YP,PHVLOC,PRPRAD,NENGSP
      REAL NENGSJ,JIALOC,JELLOC,JEVLOC,JEALOC,JINLTA
      NAMELIST/JETPWR/AIETLJ,NENGSJ,THSTCJ,JIALOC,JEVLOC,JEALOC,
     &JINLTA,JEANGL,JEVELO,AMBTMP,JESTMP,JELLOC,JETOTP,AMBSTP,
     &JERAD
C READING IN JETPWR NAMELIST
        REWIND 4
        READ(4,JETPWR)
        REWIND 4
C IF THE NUMBER OF JET ENGINES IS ZERO, ROUTINE READPOD THAT WRITES
C THE PROPELLER POD DATA IS CALLED.
      IF(J6.EQ.2)GO TO 200
      XO=JIALOC
      YO=JELLOC
      ZO=JEVLOC-(JEALOC-JIALOC)*TAN(AIETLJ/57.296) 
      XLOCS=(JEALOC-JIALOC)*COS(AIETLJ/57.296)
      RAD=(JINLTA/3.14)**.5
C WRITE THE COORDINATES OF THE JET ENGINE INLET.
      PI=ACOS(-1.)
      DO I=1,4
        DO K=1,10
          X(I,K)=XO+XLOCS*(I-1)/3.
          THET=2*PI*(K-1)/9.    
          IF(I.EQ.1)THEN
            Y(I,K)=YO+.8*RAD*SIN(THET)
            Z(I,K)=ZO+.8*RAD*COS(THET)
           Y2(I,K)=-Y(I,K) 
          ELSE IF(I.EQ.4)THEN
            Y(I,K)=YO+.7*RAD*SIN(THET)
            Z(I,K)=ZO+.7*RAD*COS(THET)
           Y2(I,K)=-Y(I,K)
          ELSE
            Y(I,K)=YO+RAD*SIN(THET)
            Z(I,K)=ZO+RAD*COS(THET)
           Y2(I,K)=-Y(I,K) 
          ENDIF          
        END DO
      END DO
C WRITING THE COORDINATES
      WRITE(7,150)                                         
150   FORMAT(1X,40HZONE T="ENGINE POD" I=4 J=10 K=1 F=BLOCK)                 
      DO J=1,10
        WRITE(7,*) (X(I,J), I=1,4)
      END DO
      DO J=1,10
        WRITE(7,*) (Y(I,J), I=1,4)
      END DO
      DO J=1,10
        WRITE(7,*) (Z(I,J), I=1,4)
      END DO
C IF MORE THAN ONE ENGINE WRITE OUT COORDINATES FOR OPPOSITE SIDE ENGINE
      WRITE(7,150)                                         
      DO J=1,10
        WRITE(7,*) (X(I,J), I=1,4)
      END DO
      DO J=1,10
        WRITE(7,*) (Y2(I,J), I=1,4)
      END DO
      DO J=1,10
        WRITE(7,*) (Z(I,J), I=1,4)
      END DO            
      RETURN
200   CALL WRITPRP
      RETURN
      END
        SUBROUTINE WRITPRP
        REAL X(4,10),Y(4,10),Y2(4,10),Z(4,10),NENGSP
C  SUBROUTINE WRITPRP WRITES PROPELLER POD DATA ON TAPE7.
      NAMELIST/PROPWR/AIETLP,NENGSP,THSTCP,PHALOC,PHVLOC,PRPRAD,ENGFCT
     &,BWAPR3,BWAPR6,BWAPR9,NOPBPE,BAPR75,YP,CROT
C READING IN NAMELIST PROPWR
        REWIND 4
        READ(4,PROPWR)
        REWIND 4
C                
        XO=PHALOC
        YO=YP
        ZO=PHVLOC
C
      PI=ACOS(-1.)
      DO I=1,4
        DO K=1,10
          X(I,K)=XO+PRPRAD*(I-1)
          THET=2*PI*(K-1)/9.    
          IF(I.EQ.3)THEN
            Y(I,K)=YO+.34*PRPRAD*SIN(THET)
            Z(I,K)=ZO+.34*PRPRAD*COS(THET)
           Y2(I,K)=-Y(I,K) 
          ELSE IF(I.EQ.4)THEN
            Y(I,K)=YO+.30*PRPRAD*SIN(THET)
            Z(I,K)=ZO+.30*PRPRAD*COS(THET)
           Y2(I,K)=-Y(I,K)
          ELSE
            Y(I,K)=YO+.35*PRPRAD*SIN(THET)
            Z(I,K)=ZO+.35*PRPRAD*COS(THET)
           Y2(I,K)=-Y(I,K) 
          ENDIF          
        END DO
      END DO
C WRITING THE COORDINATES
      WRITE(7,150)                                         
150   FORMAT(1X,40HZONE T="ENGINE POD" I=4 J=10 K=1 F=BLOCK)                 
      DO J=1,10
        WRITE(7,*) (X(I,J), I=1,4)
      END DO
      DO J=1,10
        WRITE(7,*) (Y(I,J), I=1,4)
      END DO
      DO J=1,10
        WRITE(7,*) (Z(I,J), I=1,4)
      END DO
C IF MORE THAN ONE ENGINE WRITE OUT COORDINATES FOR OPPOSITE SIDE ENGINE
      WRITE(7,150)                                         
      DO J=1,10
        WRITE(7,*) (X(I,J), I=1,4)
      END DO
      DO J=1,10
        WRITE(7,*) (Y2(I,J), I=1,4)
      END DO
      DO J=1,10
        WRITE(7,*) (Z(I,J), I=1,4)
      END DO            
C            
C            
C        WRITE(7,100)PHALOC,YP,PHVLOC
C        WRITE(7,101)0,PRPRAD,2*PRPRAD,3*PRPRAD
C        WRITE(7,101)0.35*PRPRAD,0.35*PRPRAD,0.34*PRPRAD,.30*PRPRAD
C100     FORMAT(3F7.1,T73,"PROP POD")
C101     FORMAT(4F7.1,T73,"PROP DATA")
        RETURN
        END
        SUBROUTINE WRITVEN
C  AIL DATA ON TAPE7.
C  SUBROUTINE WRITFIN WRITES VERTICAL T
        COMMON/VEN/XVF,ZVF,YVF,PHIVF       
        REAL X(20,3),Y(20,3),Z(20,3)
        DIMENSION XPRCT(10),YPRCT(10),
     &  SHB(20),SEXT(20),RLPH(20),SVWB(20),SVB(20),SVHB(20)
        NAMELIST/VFPLNF/CHRDTP,SSPNOP,SSPNE,SSPN,CHRDBP,CHRDR,
     &  SAVSI,SAVSO,CHSTAT,TWISTA,SSPNDD,DHDADI,DHDADO,TYPE,
     &  SHB,SEXT,RLPH,SVWB,SVB,SVHB
        DATA XPRCT/0.,1.25,5.,10.,15.,20.,30.,50.,70.,100./
      DATA YPRCT/0.,.947,1.777,2.341,2.673,2.869,3.001,2.82,1.832,.063/
        REWIND 4
        READ(4,VFPLNF)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        DO I=1,10
          X(I,1)=XVF+(XPRCT(I)/100.)*CHRDR
          Y(I,1)=YVF+(YPRCT(I)/100.)*CHRDR
          Z(I,1)=ZVF
          X(21-I,1)=X(I,1)
          Z(21-I,1)=Z(I,1)
          Y(21-I,1)=YVF-(YPRCT(I)/100.)*CHRDR
        END DO        
        IF(SSPNOP.EQ.0)GO TO 200
        SSPNI=SSPN-SSPNOP
        XBP=XVF+SSPNI*(TAN(SAVSI/57.296)+CHSTAT*(CHRDR-CHRDBP)/SSPNI)
        YBP=YVF+SSPNI*SIN(PHIVF/57.296)
        ZBP=ZVF-SSPNI*COS(PHIVF/57.296)
        DO I=1,10
          X(I,2)=XBP+(XPRCT(I)/100.)*CHRDBP
          Y(I,2)=YBP+(YPRCT(I)/100.)*CHRDBP
          Z(I,2)=ZBP
          X(21-I,2)=X(I,2)
          Y(21-I,2)=YBP-(YPRCT(I)/100.)*CHRDBP
          Z(21-I,2)=Z(I,2)
        END DO
C        
      XTIP=XBP+SSPNOP*(TAN(SAVSO/57.296)+CHSTAT*(CHRDBP-CHRDTP)/SSPNOP)
      YTIP=YBP+SSPNOP*SIN(PHIVF/57.296)
      ZTIP=ZBP-SSPNOP*COS(PHIVF/57.296)
        DO I=1,10
          X(I,3)=XTIP+(XPRCT(I)/100.)*CHRDTP
          Y(I,3)=YTIP+(YPRCT(I)/100.)*CHRDTP
          Z(I,3)=ZTIP
          X(21-I,3)=X(I,3)
          Y(21-I,3)=YTIP-(YPRCT(I)/100.)*CHRDTP
          Z(21-I,3)=Z(I,3)
        END DO
        JMAX=3
        GO TO 201
C FOR NO OUTBOARD WING SECTION, THE WING TIP COORDINATES ARE CALCULATED
C AND WRITTEN ON TAPE7, TWICE, SIMULATING AN OUTBOARD SECTION OF ZERO LENGTH
200     XTIP=XVF+SSPN*(TAN(SAVSI/57.296)+CHSTAT*(CHRDR-CHRDTP)/SSPN)
        YTIP=YVF+SSPN*SIN(PHIVF/57.296)
        ZTIP=ZVF-SSPN*COS(PHIVF/57.296)
        DO I=1,10
          X(I,2)=XTIP+(XPRCT(I)/100.)*CHRDTP
          Y(I,2)=YTIP+(YPRCT(I)/100.)*CHRDTP
          Z(I,2)=ZTIP
          X(21-I,2)=X(I,2)
          Y(21-I,2)=YTIP-(YPRCT(I)/100.)*CHRDTP
          Z(21-I,2)=Z(I,2)
        END DO
        JMAX=2
201     WRITE(7,100) JMAX                 
100   FORMAT(1X,23HZONE T="V TAIL" I=20 J=,I1,1X,3HK=1,1X,7HF=BLOCK)
      DO J=1,JMAX
        WRITE(7,*) (X(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (Y(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (Z(I,J),I=1,20)
      END DO                      
      IF(YVF.NE.0)THEN
      WRITE(7,100) JMAX
        DO J=1,JMAX
          WRITE(7,*) (X(I,J),I=1,20)
        END DO  
        DO J=1,JMAX
          WRITE(7,*) (-Y(I,J),I=1,20)
        END DO  
        DO J=1,JMAX
          WRITE(7,*) (Z(I,J),I=1,20)
        END DO                      
      ENDIF
C        CALL WRITF2(XTIP,ZTIP)
        RETURN
        END
      SUBROUTINE WRITWNG
C  SUBROUTINE WRITWNG WRITES WING DATA ON TAPE7.
      COMMON/WG/NWAF,NWAFOR,XW,ZW,ALIW
      REAL X(20,3),Y(20,3),Z(20,3)
      DIMENSION WXORD(10),WYORD(10),
     &SHB(20),SEXT(20),RLPH(20),SVWB(20),SVB(20),SVHB(20)
      NAMELIST/WGPLNF/CHRDTP,SSPNOP,SSPNE,SSPN,CHRDBP,CHRDR,
     &SAVSI,SAVSO,CHSTAT,TWISTA,SSPNDD,DHDADI,DHDADO,TYPE,
     &SHB,SEXT,RLPH,SVWB,SVB,SVHB
      DATA WXORD/0.,1.25,5.,10.,15.,20.,30.,50.,70.,100./
      DATA WYORD/0.,1.42,2.666,3.512,4.009,4.303,4.501,3.971,2.748,.095/
        REWIND 4
        READ(4,WGPLNF)
C PERCENT CHORD LOCATIONS ALONG AIRFOIL SECTION,WHERE
C AIRFOIL THICKNESS WILL BE DEFINED.
C        WRITE(7,130) WXORD
        YW=0
C        WRITE(7,100)XW,YW,ZW,CHRDR
C CALCULATING THE X,Y, AND Z COORDINATES AT THE ROOT
      DO I=1,10
        X(I,1)=XW+(WXORD(I)/100.)*CHRDR
        Y(I,1)=YW
        Z(I,1)=ZW+(WYORD(I)/100.)*CHRDR
        X(21-I,1)=X(I,1)
        Y(21-I,1)=Y(I,1)
        Z(21-I,1)=ZW-(WYORD(I)/100.)*CHRDR
      END DO
C IF THE WING HAS AN OUTBOARD SECTION, THE COORDINATES OF THE BREAKPOINT
C ARE CALCULATED AND WRITTEN ON TAPE7.
        IF(SSPNOP.EQ.0)GO TO 200
        SSPNI=SSPN-SSPNOP
        XBP=XW+SSPNI*(TAN(SAVSI/57.296)+CHSTAT*(CHRDR-CHRDBP)/SSPNI)
        YBP=SSPNI
        ZBP=ZW+SSPNI*TAN(DHDADI/57.296)
        DO I=1,10
          X(I,2)=XBP+(WXORD(I)/100.)*CHRDBP
          Y(I,2)=YBP
          Z(I,2)=ZBP+(WYORD(I)/100.)*CHRDBP
          X(21-I,2)=X(I,2)
          Y(21-I,2)=Y(I,2)
          Z(21-I,2)=ZBP-(WYORD(I)/100.)*CHRDBP
        END DO
C        WRITE(7,100)XBP,YBP,ZBP,CHRDBP
C  THE COORDINATES OF THE WING TIP ARE CALCULATED, AND WRITTEN ON TAPE7.  
      XTIP=XBP+SSPNOP*(TAN(SAVSO/57.296)+CHSTAT*(CHRDBP-CHRDTP)/SSPNOP)
      YTIP=SSPN
      ZTIP=ZBP+SSPNOP*TAN(DHDADO/57.296)
        DO I=1,10
          X(I,3)=XTIP+(WXORD(I)/100.)*CHRDTP
          Y(I,3)=YTIP
          Z(I,3)=ZTIP+(WYORD(I)/100.)*CHRDTP
          X(21-I,3)=X(I,3)
          Y(21-I,3)=Y(I,3)
          Z(21-I,3)=ZTIP-(WYORD(I)/100.)*CHRDTP
        END DO
        JMAX=3
        GO TO 201
C FOR NO OUTBOARD WING SECTION, THE WING TIP COORDINATES ARE CALCULATED
C AND WRITTEN ON TAPE7, TWICE, SIMULATING AN OUTBOARD SECTION OF ZERO LENGTH
200     XTIP=XW+SSPN*(TAN(SAVSI/57.296)+CHSTAT*(CHRDR-CHRDTP)/SSPN)
        YTIP=SSPN
        ZTIP=ZW+SSPN*TAN(DHDADI/57.296)
        DO I=1,10
          X(I,2)=XTIP+(WXORD(I)/100.)*CHRDTP
          Y(I,2)=YTIP
          Z(I,2)=ZTIP+(WYORD(I)/100.)*CHRDTP
          X(21-I,2)=X(I,2)
          Y(21-I,2)=Y(I,2)
          Z(21-I,2)=ZTIP-(WYORD(I)/100.)*CHRDTP
        END DO
        JMAX=2
C        WRITE(7,100) XTIP,YTIP,ZTIP,CHRDTP
201     WRITE(7,100) JMAX                 
100   FORMAT(1X,23HZONE T="R WING" I=20 J=,I1,1X,3HK=1,1X,7HF=BLOCK)
      DO J=1,JMAX
        WRITE(7,*) (X(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (Y(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (Z(I,J),I=1,20)
      END DO  
      WRITE(7,103) JMAX                 
103   FORMAT(1X,23HZONE T="L WING" I=20 J=,I1,1X,3HK=1,1X,7HF=BLOCK)
      DO J=1,JMAX
        WRITE(7,*) (X(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (-1.*Y(I,J),I=1,20)
      END DO  
      DO J=1,JMAX
        WRITE(7,*) (Z(I,J),I=1,20)
      END DO  
C XTIP,YTIP,ZTIP,CHRDTP
C THE THICKNESSES AT THE PERCENT CHORD LOCATIONS ARE WRITTEN ON TAPE7,
C FOR EACH OF THE THREE AIRFOILS.
C        WRITE(7,140)WYORD
C        WRITE(7,140)WYORD 
C        WRITE(7,140)WYORD
C100     FORMAT(4F7.2,T73,"WING DIMS")
C130     FORMAT(10F7.2,T73,"WING %CHORD")
C140     FORMAT(10F7.2,T73,"WNG THICKNESS")
        RETURN
        END
