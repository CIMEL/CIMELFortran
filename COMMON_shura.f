CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Program NAME: FSNIR2.f (SUBROUTINE)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OBJECT
C   Calculating Spectral Solar Iradiance for GLI Channels
C   With New Solar Irradiance
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C HISTORY:
C 2003.01 Made
C---INPUT
C IUSOL		I	Device number for New Solar irradiance data file
C IWS		I	Wave number
C FWL1		R	Spectral Wavelength[um]
C FWL1S		R	Sub-Spectral Wavelength[um](short)
C FWL1L		R	Sub-Spectral Wavelength[um](long)
C---OUTPUT
C FSOL		R	Spectral Solar Irradiance [W/m2/um]
C---PARAMETER
C NS		I	Total number of solar irradiance data
C FSWL	     R(20000)	Wavelength data [um] in IUSOL
C FSVAL	     R(20000)	Solar irradiance data [W/m2/um] in IUSOL
C ERR           R       Error Code
C IS            I       PARAMETER
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE FSNIR2(IUSOL,IWS,FWL1,FWL1S,FWL1L,FSOL)
      PARAMETER(NS0=20000)
      INTEGER IUSOL,NS
      REAL FWL1,FWL1S,FWL1L,FSOL
      REAL FSWL(NS0),FSVAL(NS0)
      CHARACTER ERR*80
      INTEGER IWS,IS
      SAVE
      IF(IWS .EQ. 1) THEN
         DO 120 IS=1,NS0
            FSWL(IS)=0.
            FSVAL(IS)=0.
  120    CONTINUE
         NS=0
C Data Read
         CALL RDNSOL(IUSOL,NS,FSWL,FSVAL)
C For test
C         DO 160 I2=1,NS
C            WRITE(6,206,ERR=970) FSWL(I2),FSVAL(I2)
C  160    CONTINUE
      END IF
C Calculation
      CALL CLNSL2(NS,FSWL,FSVAL,FWL1,FWL1S,FWL1L,FSOL)
      RETURN
C FORMAT
  206  FORMAT(1P,2E14.5)
C FOR CHECK
  970 CONTINUE
      ERR='Output Writing ERROR'
      GO TO 999
  999 CONTINUE
      WRITE(6,*) ERR
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       SUBROUTINE RDNSOL(IUSOL,NS,FSWL,FSVAL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OBJECT:
C   READ new solar irradiance data
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--INPUT
C IUSOL		I	Device number for New Solar irradiance data file
C--OUTPUT
C NS		I	Total data number in data file
C FSWL	     R(20000)	Wavelength[um] in IUSOL
C FSVAL	     R(20000)	Solar irradiance[W/m2/um] im IUSOL
C--PARAMETER
C ERR		CH	ERROR CODE
C FF1		R	Wavelength[um] in IUSOL file
C FF2		R	Solar irradiance[W/m2/um] in IUSOL file
C I1		I	NUMBER PARAMETER
C IS		I	Total data number (read from IUSOL file)
C IC            I       Data no. written in IUSOL file
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       PARAMETER(NS0=20000)
       INTEGER IUSOL,NS
       REAL FSWL(NS0),FSVAL(NS0)
       INTEGER I1,IS,I,IC
       REAL FF1,FF2
       CHARACTER ERR*80
C Read Hedder
       DO 100 I1=1,56
          READ(IUSOL,*,ERR=970)
  100  CONTINUE
       READ(IUSOL,*,ERR=971) IS
       DO 110 I1=1,7
          READ(IUSOL,*,ERR=970)
  110  CONTINUE
C Read Value
       NS=0
       DO 120 I=1,NS0
          READ(IUSOL,*,END=121,ERR=971) IC,FF1,FF2
C Original data (Thuillier2002) has some blank line.
          IF(FF1*100000. .GT. 1.) THEN
             NS=NS+1
             FSWL(I)=FF1
             FSVAL(I)=FF2
          END IF
 120   CONTINUE
 121   CONTINUE
C Checking total data number
       IF(NS.NE.IS) GO TO 980
       RETURN
  970  CONTINUE
       ERR='New Solar Irradiance Data Headder Read ERROR'
       GO TO 999
  971  CONTINUE
       ERR='New Solar Irradiance Data Read ERROR'
       GO TO 999
  980  CONTINUE
       ERR='Data Number is not correct.'
       GO TO 999
  999  CONTINUE
       WRITE(6,*) ERR
       STOP
       END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE CLNSL2(NS,FSWL,FSVAL,FWL1,FWL1S,FWL1L,FSOL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OBJECT��
C Calculating Spectral Solar Irradiance data
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--INPUT
C NS		I	Total number of solar irradiance data
C FSWL	     R(20000)	Wavelength data [um] in IUSOL
C FSVAL	     R(20000)	Solar irradiance data [W/m2/um] in IUSOL
C FWL1		R	Spectral Wavelength[um]
C FWL1S		R	Sub-Spectral Wavelength[um](short)
C FWL1L		R	Sub-Spectral Wavelength[um](long)
C--OUTPUT
C FSOL1	        R	Spectral solar irradiance[W/m2/um]
C--PARAMETER
C ERR		CH	ERROR CODE
C I2,IX		I	PARAMETER
C INUM		I	Position id for Critical sub-spectral wavelength 
C			in Solar irradiance data
C FSWMIN	R	Sub-spectral wavelength (short) [um]
C FSWMAX	R	Sub-spectral wavelength (long)  [um]
C FSMIN		R	Solar irradiance data at sub-spectral wavelength
C			(short) [W/m2/um]
C FSMAX		R	Solar irradiance data at sub-spectral wavelength
C			(long) [W/m2/um]
C FSSOL		R	Total solar irradiance at sub-spectral wavelength
C			area [W/m2/um]
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Value for setting Threshold data.
C IF NDNUM < KTHRSH  -> Lagrange Intrepolation for solar irradiance
C    NDNUM => KTHRSH -> Calculate the area of solar irradiance data
C                           and mean for the solar irradiance at the 
C                           specifiled wavelength
      PARAMETER(KTHRSH=25)
C
      INTEGER NS
      REAL FWL1,FSWL(NS),FSVAL(NS),FSOL,FWL1S,FWL1L
      INTEGER INUM,I2
      REAL FSWMIN,FSWMAX,FSMIN,FSMAX,FSSOL,FCLSOL
      INTEGER IX,NDNUM
      CHARACTER ERR*80
C Calculation start
      INUM=0
      IX=0
C Normalize
      NDNUM=0
C Find the position and start counting
      CALL TURNUP(FSWL,NS,FWL1S,IX)
      IF((IX .LE. 1) .OR. (IX .GE. NS)) THEN
         WRITE(6,*) 'IX=',IX
         ERR='CLNSL2: TURNUP judged IX is in wrong position.'
         GOTO 999
      END IF
      IF(FSWL(IX) .LT. FWL1S) IX=IX+1
      DO 110 I2=IX,NS
         IF(FSWL(I2) .LT. FWL1L) THEN
C            WRITE(6,*) FWL1S, ' < ', FSWL(I2),' < ',FWL1L
            NDNUM=NDNUM+1
         ELSE
C            WRITE(6,*) FWL1L, ' < ', FSWL(I2)
            GO TO 120
         END IF
  110    CONTINUE
  120    CONTINUE
         IX=I2

         IF(NDNUM .LT. KTHRSH) THEN
C            WRITE(6,*) 'TAIYO KIZAMIHABA IS LARGE',NDNUM
            FSOL1=FCLSOL(NS,FSWL,FSVAL,FWL1)
C            WRITE(6,*) 'FSOL1 =',FSOL1
         ELSE
C            WRITE(6,*) 'TAIYO KIZAMIHABA IS SMALL',NDNUM
C Start calculating area
C  1. Setting sub spectral wavelength
            FSWMIN=FWL1S
            FSWMAX=FWL1L
            FSMIN=FCLSOL(NS,FSWL,FSVAL,FSWMIN)
            FSMAX=FCLSOL(NS,FSWL,FSVAL,FSWMAX)

C  1.1 Checking wavelength
            IF(FSWMIN .GE. FSWMAX) THEN
               ERR='Response Function Data ERROR'
               GO TO 999
            END IF
C  2. Find m (FSWL(m)>FSWMIN)
C     Method of bisection
            CALL TURNUP(FSWL,NS,FSWMIN,INUM)
            IF((INUM .LE. 1) .OR. (INUM .GE. NS)) THEN
               ERR='CLNSL2: TURNUP judged NDID is wrong.'
               GOTO 999
            END IF
C            WRITE(6,*) 'INUM= ',INUM
            IF(FSWMIN .GT. FSWL(INUM)) INUM=INUM+1
C  2.1 compare the data grid of spectral wavelength with Solar irradiance grid
            IF(FSWMAX .LE. FSWL(INUM)) THEN
               FSOL1=(FSMAX+FSMIN)/2.
               GO TO 130
            END IF
C  3. Calculating the most short area
            FSSOL=0.
            FSSOL=FSSOL+(FSMIN+FSVAL(INUM))*(FSWL(INUM)-FSWMIN)/2.
C  4. Rectangle integlation
  140       CONTINUE
            INUM=INUM+1
            IF(FSWL(INUM) .LT. FSWMAX) THEN
               FSSOL=FSSOL+(FSVAL(INUM)+FSVAL(INUM-1))
     &              *(FSWL(INUM)-FSWL(INUM-1))/2.
               GO TO 140
            ELSE
               FSSOL=FSSOL+(FSMAX+FSVAL(INUM-1))
     &              *(FSWMAX-FSWL(INUM-1))/2.
            END IF
C  5. Meaning
C Check before calculation
            IF((FSWMAX-FSWMIN) .LE. 0.000001) THEN
               ERR='Wavelength Error'
               GO TO 999
            END IF
            FSSOL=FSSOL/(FSWMAX-FSWMIN)
            FSOL1=FSSOL
         END IF
  130 CONTINUE
      FSOL=FSOL1
      RETURN
  999 CONTINUE
      WRITE(6,*) ERR
      STOP
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FCLSOL(NS,FSWL,FSVAL,FWLV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OBJECT:
C Use 4th ordered Lagrange integration on array solar irradiance data and
C Calculate the solar irradianse at FWLV
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--INPUT
C NS		I	Total data number of solar irradiance data
C FSWL		R(NS)	Wavelength component of solar irradiance data[um]
C FSVAL		R(NS)	Solar irradiance data [W/m2/um]
C FWLV		R	Wavelength to calculate solar irradiance [um]
C--OUTPUT
C FCLSOL	R	Calculation result (Solar irradiance) [W/m2/um]
C--PARAMETER
C ERR		CH	Error code
C NDID		I	The place of FWLV in the table of FSWL
C RVAL1,2,3,4	R	Solar irradiance data for Lagrange interpolation
C			[W/m2/um]
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER NS
      REAL FSWL(NS),FSVAL(NS),FWLV
      INTEGER NDID
      REAL RVAL1,RVAL2,RVAL3,RVAL4,FCLSOL
      CHARACTER ERR*80
C Normalize
      FCLSOL=0.
C Find the location of FWLV
      CALL TURNUP(FSWL,NS,FWLV,NDID)
      IF((NDID .LE. 1) .OR. (NDID .GE. NS)) THEN
         GOTO 990
      END IF
      IF(FWLV .GT. FSWL(NDID)) NDID=NDID+1
C Checking range of NDID
      IF((NDID .LE. 1) .OR. (NDID .GE. NS-1)) THEN
         FCLSOL=0.
      ELSE IF(FWLV .GT. 100.) THEN
C For low frequencies use a power law appoximation
         FCLSOL=3.50187E-13 * (10000./FWLV) ** 3.93281
      ELSE
C---
C The 4th-ordered Lagrange interpolation
C---
         RVAL1 = ((FWLV-FSWL(NDID-1))*(FWLV-FSWL(NDID))
     1          *(FWLV-FSWL(NDID+1)))/((FSWL(NDID-2)
     2          -FSWL(NDID-1))*(FSWL(NDID-2)-FSWL(NDID))
     3          *(FSWL(NDID-2)-FSWL(NDID+1)))
     4          *FSVAL(NDID-2)

         RVAL2 = ((FWLV-FSWL(NDID-2))*(FWLV-FSWL(NDID))
     1          *(FWLV-FSWL(NDID+1)))/((FSWL(NDID-1)
     2          -FSWL(NDID-2))*(FSWL(NDID-1)-FSWL(NDID))
     3          *(FSWL(NDID-1)-FSWL(NDID+1)))
     4          *FSVAL(NDID-1)

         RVAL3 = ((FWLV-FSWL(NDID-2))*(FWLV-FSWL(NDID-1))
     1          *(FWLV-FSWL(NDID+1)))/((FSWL(NDID)
     2          -FSWL(NDID-2))*(FSWL(NDID)-FSWL(NDID-1))
     3          *(FSWL(NDID)-FSWL(NDID+1)))
     4          *FSVAL(NDID)

         RVAL4 = ((FWLV-FSWL(NDID-2))*(FWLV-FSWL(NDID-1))
     1          *(FWLV-FSWL(NDID)))/((FSWL(NDID+1)
     2          -FSWL(NDID-2))*(FSWL(NDID+1)-FSWL(NDID-1))
     3          *(FSWL(NDID+1)-FSWL(NDID)))
     4          *FSVAL(NDID+1)

         FCLSOL = RVAL1+RVAL2+RVAL3+RVAL4
      END IF
      RETURN
  990 CONTINUE
      ERR='FCLSOL: TURNUP judged NDID is in wrong position.'
  999 CONTINUE
      WRITE(6,*) ERR
      STOP
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE TURNUP(XX,N,X,J)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OBJECT:
C Finding a table entry by bisection.
C Reference: "NUMERICAL RECIPES in C"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--INPUT
C XX		R(N)	Array XX(1:n)
C N		I	Total data number
C X		R	Value to find the entry
C--OUTPUT
C J		I	X is between XX(N) and XX(N+1)
C--PARAMETER
C JL		I	Lower Limit
C JM		I	Midpoint
C JU		I	Upper Limit
C ERR		CH	Error Code
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER J,N
      REAL X,XX(N)
      INTEGER JL,JM,JU
      CHARACTER ERR*80
      IF((XX(1) .LE. 0.1) .OR. (XX(N) .LE. 0.1)) THEN
         GO TO 980
      END IF
C Initialize Lower and Upper limit.
      JL=0
      JU=N+1
   10 IF(JU-JL .GT. 1)THEN
C Compute a mid-point
         JM=(JU+JL)/2
C Check the array XX(1:N) is increasing or decreasing.
         IF(XX(N) .GE. XX(1)) THEN
C Replace either the lower limit or the upper limit, as appropriate.
            IF(X .GE. XX(JM)) THEN
               JL=JM
            ELSE
               JU=JM
            END IF
         ELSE
C Replace either the lower limit or the upper limit, as appropriate.
            IF(X .LT. XX(JM)) THEN
               JL=JM
            ELSE
               JU=JM
            END IF
         END IF
      GO TO 10
      END IF
C Set the output
      IF(X .EQ. XX(1))THEN
         J=1
      ELSE IF(X .EQ. XX(N))THEN
         J=N-1
      ELSE
         J=JL
      END IF
      IF(J .LE. 0) THEN
         GOTO 999
      END IF
      RETURN
  980 CONTINUE
      ERR='TURNUP: PARAMETER has no data.'
      GO TO 999
  990 CONTINUE
      ERR='TURNUP: Data location check error.'
  999 CONTINUE
      WRITE(6,*) ERR
      STOP
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE CANR(M,NDA,AMUA,WA,R,IUCD,IUCR,
     & R0B,KB,THB,ICANO,INITR,IBRF,IRFS)
C      SUBROUTINE CANR(M,NDA,AMUA,WA,R,IUCD)
C      SUBROUTINE OCNR11(M,NDA,AMUA,WA,CR,CI,U10,R)
C REFLECTION MATRIX OF CANOPY SURFACE 
C 'NADIM(New Advanced DIscrete Mode)' IS USED TO COMPUTE BRF. 
C THE ORIGINAL PACKAGE IS DEVELOPED 
C BY NADINE GOBRON,BERNARD PINTY,MICHEL M.VERSTRAETE AND YVES GOVAERTS.
C SEE,
C GOBRON,N.,B.PINTY,M.M.VERSTRAETE,AND Y.GOVAERTS,1997
C 'A SEMIDISCRETE MODEL FOR THE SCATTERING OF LIGHT BY VEGETATION'
C JURNAL OG GEOPHYSICAL RESEARCH,VOL102,9431-9446
C--- HISTORY
C 92. 9. 1 CREATED BY HASUMI
C    12.23 MODIFIED BY NAKAJIMA
C 95. 6. 2 Generated from OCNRF1
C 98. 4.16 FOR OCEAN SURFACE-->FOR CANOPY SUFACE BY SED
C--- INPUT
C M      I     FOURIER ORDER
C KNDM   I     DECLARED DIMENSION OF R AND AMUA
C NDA    I     USED DIMENSION OF R AND AMUA
C AMUA  R(NDA) QUADRATURE POINTS IN HEMISPHERE
C              DECREASING ORDER (ZENITH TO HORIZON, OR, 1 -> 0)
C WA    R(NDA) QUADRATURE WEIGHTS
C--- OUTPUT
C R    R(KNDA,NDA)  REFLECTION MATRIX FOR M-TH FOURIER ORDER
C                    ur = R * ui
C--LOCAL
C CBRF1  R(KNDM,KNEZA,KN)      DATABASE OF BRF, SAVED DATA
C EZA1   R(KNEZA,KNDM)    DEG  GRID POINTS OF EMERGING ZENITH ANGLE
C EZA    R(KNEZA)         DEG  GRID POINTS OF EMERGING ZENITH ANGLE  
C SRMAX  R                RAD  AMUA(NDA), VALUE FOR INTERPOLATION IN CANSR
C$END
      SAVE
      PARAMETER (KNDM  =16)
      PARAMETER(PI=3.141592654)
      DIMENSION AMUA(KNDM),WA(KNDM),R(KNDM, KNDM)
C*****FOR CAPONY BRDF******************************************
      real R0B,KB,THB
      integer ICANO
C*****FOR CAPONY BRDF****************************************** 
C LOCAL VARIABLES
C NUMBER OF GAUSS POINTS FOR AZIMUTHAL INTEGRATION 
      PARAMETER (KN=16,KNDM1=KNDM+1)
C NUMBER OF EMERGING DIRECTION FOR BRF DATABASE
      PARAMETER (KNEZA=KNDM*2+3)
      DIMENSION X(KNDM1),GX(KN),GW(KN),FI1(KN),COSM1(KN)
     & ,XB(5,2),NS(2),RR(KNDM, KNDM)
     &,CBRF1(KNDM,KNEZA,KN),EZA1(KNEZA,KNDM),EZA(KNEZA)
C--INIT
      DATA INIT/1/
      NEZA=NDA*2+3
cs      write(6,*)'IN CANR','   =',INITR
C
        IF(INITR.GT.0) THEN
        N=KN
cs        INITR=0
        CALL QGAUSN(GW, GX, N )
C 98.10.06
        DO K=1,N
        FI1(K)=PI*GX(K)
        ENDDO
C CBRF1
C        IUCDR=62
        SRMAX=ACOS(AMUA(NDA))
        CALL GETCN1(IUCD,IUCR,KNDM,KNEZA,KN,NDA,NEZA
C 99.01.13
C        CALL GETCN1(IUCD,IUCDR,KNDM,KNEZA,KN,NDA,NEZA
     &,AMUA,FI1,SRMAX,CBRF1,EZA1,
     & R0B,KB,THB,ICANO,IBRF,IRFS)    
        ENDIF
cs        write(6,*)'CBRF1'
cs        do i1=1,1
cs        do i2=1,NEZA
cs        write(6,*)(CBRF1(i1,i2,i3),i3=1,KN)
cs        enddo
cs        enddo
C Parameters for integration
C FOR FI-INTEGRATION
      DO 4 K=1,N
      FI1(K)=0.0+PI*GX(K)
      COSM1(K)=2*COS(FI1(K)*M)*PI *GW(K)
 4    CONTINUE
C FOR BOUNDARY
      X(1)=1
      IF(NDA.GE.2) THEN
        DO 1 I = 2, NDA
    1   X(I)=(AMUA(I-1)+AMUA(I))/2
      ENDIF
      X(NDA+1)=0
C
      DO 2 I = 1, NDA
CC SETING MU-BOUDARY FOR MU-INEGRATION
      NS1=1
      XB(1,1)=X(I)
      NS1=NS1+1
      XB(NS1,1)=AMUA(I)
      NS1=NS1+1
      XB(NS1,1)=X(I+1)
      NS(1)=NS1-1
      NS(2)=1
      XB(1,2)=X(I)
      XB(2,2)=X(I+1)
C
      DO 2 J = 1, NDA
      IF(I.EQ.J) THEN
        IEQ=1
       ELSE
        IEQ=2
      ENDIF
CC MU-INTEGRATION
      RIJ=0
      AMI = AMUA(J)
      THI=ACOS(AMI)*180.0/PI
C 2-D -> 1-D
      DO NE=1,NEZA
          EZA(NE)=EZA1(NE,J)
      ENDDO
C
        DO 3 IS=1,NS(IEQ)
        DX=XB(IS,IEQ)-XB(IS+1,IEQ)
        DO 3 II = 1, N
          AME=XB(IS+1,IEQ)+DX*GX(II)
          THE=ACOS(AME)*180.0/PI
          W=DX*GW(II)*AME
C FI LOOP
          DO 3 K = 1, N
C FI-INTEGRATION
C 98.10.06
          CALL CBRFA1(J,THE,K,EZA,CBRF1,KNDM,KNEZA,KN,NEZA,BRF)
cs          IF(BRF.LE.0.0) WRITE(6,*) 'ERROR IN CANR BRF<0.0'
          IF(BRF.LE.0.0) BRF=1e-10
         RIJ=RIJ+(COSM1(K)*BRF*AMI/PI)*W
 3       CONTINUE
C 98.09.14
C      R(I,J)=RIJ/WA(I)*WA(J)/AMUA(I)
      RR(I,J)=RIJ/WA(I)
 2    CONTINUE
C SYMMETRIC OPERATION
      DO 5 I=1,NDA
      DO 5 J=1,I
      RRR=(RR(I,J)+RR(J,I))/2
      R(I,J)=RRR/AMUA(I)*WA(J)
      R(J,I)=RRR/AMUA(J)*WA(I)
 5    CONTINUE
      RETURN
      END
      SUBROUTINE CANSR(M,NDA,AMUA,WA,AM0I,IAM0,AM0,NA0,SR,IUCD,IUCSR,
     & R0B,KB,THB,ICANO,INITR,IBRF,IRFS)
C      SUBROUTINE CANSR(M,NDA,AMUA,WA,AM0I,IAM0,AM0,NA0,SR,IUCD)
C      SUBROUTINE OCNR31(M,NDA,AMUA,WA,AM0,CR,CI,U10,SR)
C REFLECTION MATRIX OF CANOPY SURFACE 
C 'NADIM(New Advanced DIscrete Mode)' IS USED TO COMPUTE BRF. 
C THE ORIGINAL PACKAGE IS DEVELOPED 
C BY NADINE GOBRON,BERNARD PINTY,MICHEL M.VERSTRAETE AND YVES GOVAERTS.
C SEE,
C GOBRON,N.,B.PINTY,M.M.VERSTRAETE,AND Y.GOVAERTS,1997
C 'A SEMIDISCRETE MODEL FOR THE SCATTERING OF LIGHT BY VEGETATION'
C JURNAL OG GEOPHYSICAL RESEARCH,VOL102,9431-9446
C--- HISTORY
C 92. 9. 1 CREATED BY HASUMI
C    12.23 MODIFIED BY NAKAJIMA
C 93. 3.22 /WA(I) debugged by Takashi
C     3.29 AMI -> AM0
C 98. 4.   FOR OCEAN SURFACE -->>FOR CANOPY SURFACE BY SED
C--- INPUT
C M      I     FOURIER ORDER
C KNDM   I     DECLARED DIMENSION OF R AND AMUA
C NDA    I     USED DIMENSION OF R AND AMUA
C AMUA  R(NDA) QUADRATURE POINTS IN HEMISPHERE
C              DECREASING ORDER (ZENITH TO HORIZON, OR, 1 -> 0)
C WA    R(NDA) Quadrature weights
C AM0I    R     Cos (Solar Zenith Angle)
C AM0    R(KNA0)     Cos (Solar Zenith Angle)
C--- OUTPUT
C SR    SR(KNDA)  REFLECTION SOURCE MATRIX FOR M-TH FOURIER ORDER
C$END
      SAVE
      PARAMETER (KNDM  =16)
      PARAMETER(PI=3.141592654)
      DIMENSION AMUA(KNDM),WA(KNDM),SR(KNDM)
C*****FOR CAPONY BRDF******************************************
      real R0B,KB,THB
      integer ICANO
C*****FOR CAPONY BRDF******************************************      
C LOCAL VARIABLES
c      PARAMETER (KN=30,KNDM1=KNDM+1)
      PARAMETER (KN=16,KNDM1=KNDM+1)
      PARAMETER (KNA0=2)
C NUMBER OF EMERGING DIRECTION FOR BRF DATABASE
      PARAMETER (KNEZA=KNDM*2+3)
      DIMENSION X(KNDM1),GX(KN),GW(KN),FI1(KN),COSM1(KN)
     & ,XB(5,2),NS(2)
      DIMENSION AM0(KNA0),CBRF2(KNA0,KNEZA,KN),EZA2(KNEZA,KNA0)
     &,EZA(KNEZA)
C--INIT
      DATA INIT/1/
      NEZA=NDA*2+3
cs      write(6,*)'IN CANSR','   INITR=',INITR
C
        IF(INITR.GT.0) THEN
        N=KN
        INITR=0
        CALL QGAUSN(GW, GX, N )
        DO  K=1,N
          FI1(K)=0.0+PI*GX(K)
        ENDDO
C        IUCDSR=63
        SRMAX=ACOS(AMUA(NDA))
        CALL GETCN1(IUCD,IUCSR,KNA0,KNEZA,KN,NA0,NEZA
C 99.01.13
C        CALL GETCN1(IUCD,IUCDSR,KNA0,KNEZA,KN,NA0,NEZA
     &,AM0,FI1,SRMAX,CBRF2,EZA2,
     &R0B,KB,THB,ICANO,IBRF,IRFS)
        ENDIF
cs        write(6,*)'CBRF2'
cs        do i1=1,1
cs        do i2=1,NEZA
cs        write(6,*)(CBRF2(i1,i2,i3),i3=1,KN)
cs        enddo
cs        enddo
C 2-D -> 1-D
      DO J=1,NEZA
          EZA(J)=EZA2(J,IAM0)
      ENDDO
C Parameters for integration
C FOR FI-INTEGRATION
      DO 4 K=1,N
      FI1(K)=0.0+PI*GX(K)
      COSM1(K)=2*COS(FI1(K)*M)*PI *GW(K)
 4    CONTINUE
C 
C FOR BOUNDARY
      X(1)=1
      IF(NDA.GE.2) THEN
        DO 1 I = 2, NDA
    1   X(I)=(AMUA(I-1)+AMUA(I))/2
      ENDIF
      X(NDA+1)=0
      THI=ACOS(AM0I)*180.0/PI
C
      DO 2 I = 1, NDA
CC SETING MU-BAOUDARY FOR MU-INEGRATION
      NS1=1
      XB(1,1)=X(I)
      NS1=NS1+1
      XB(NS1,1)=AMUA(I)
      NS1=NS1+1
      XB(NS1,1)=X(I+1)
      NS(1)=NS1-1
      NS(2)=1
      XB(1,2)=X(I)
      XB(2,2)=X(I+1)
C
      IF(AMUA(I).EQ.AM0I) THEN
        IEQ=1
       ELSE
        IEQ=2
      ENDIF
CC MU-INTEGRATION
      RIJ=0
        DO 3 IS=1,NS(IEQ)
        DX=XB(IS,IEQ)-XB(IS+1,IEQ)
        DO 3 II = 1, N
          AME=XB(IS+1,IEQ)+DX*GX(II)
          W=DX*GW(II)*AME
          THE=ACOS(AME)*180.0/PI
C FI LOOP
          DO 3 K = 1, N
C FI-INTEGRATION
          CALL CBRFA1(IAM0,THE,K,EZA,CBRF2,KNA0,KNEZA,KN,NEZA,BRF)
cs          IF(BRF.LE.0.0) WRITE(6,*) 'ERROR IN CANSR BRF<0.0'
          IF(BRF.LE.0.0) BRF=1e-10
          RIJ=RIJ+(COSM1(K)*BRF*AM0I/PI)*W
 3        CONTINUE
      SR(I)=RIJ/AMUA(I)/WA(I)
 2    CONTINUE
      RETURN
      END
C
      SUBROUTINE GETCN1(IUCD,IUCD2,NSZA,NEZA,NAA,NX,NY,XX,FI1
     &,SRMAX,CBRF1,EZADEG,R0B,KB,THB,ICANO,IBRF,IRFS)
C GETTING CANOPY BRF DATABASE FOR "CANR" AND "CANSR"
C ICANO=0 : MAKE NEW "r.dump","sr.dump"
C ICANO=1 : MAKE NEW "sr.dump", AND READ "r.dump"
C ICANO=2 : MAKE NO NEW "*.dump", USE CALCULATED DATABASE "r.dump","sr.dump"
C-- INPUT
C IUCD  I            DEVISE NUMBER OF "cano.data"
C IUCD2 I            DEVISE NUMBER OF BRF DATABASE FILE
C NSZA  I            MATRIX SIZE
C NEZA  I            MATRIX SIZE
C NAA   I            MATRIX SIZE
C NX    I            NUMBER OF XX
C NY    I            NUMBER OF EZA
C XX    R(NSZA)      COS(ZENITH ANGLE)
C FI1   R(NAA)  RAD  AZIMUTHAL ANGLE
C SRMAX R       RAD  ACOS(AMUA(NDA)), AMUA IS THE SAME AMUA IN CANR,CANSR
C                    THIS IS ONLY FOR CANSR.
C-- OUTPUT
C CBRF1   R(NSZA,NEZA,NAA)  -  BRF
C EZADEG  R(NEZA,NSZA)         DEG EMERGING ZENITH ANGLE
C--HISTORY
C 2001.4.11 SED
C   Adding File Read Error check for canopy
C-- PARAMETER
      PARAMETER (PI=3.141592653589793)
C-- AREA    
      DIMENSION CBRF1(NSZA,NEZA,NAA),XX(NSZA),FI1(NAA)
     &,EZADEG(NEZA,NSZA)
C-- LOCAL
      REAL  SZARAD(NSZA),EZARAD(NEZA,NSZA),AARAD(NAA)
      REAL  EZA1(NY)
C*****FOR CAPONY BRDF******************************************
      real R0B,KB,THB
C*****FOR CAPONY BRDF****************************************** 
      CHARACTER CH11*11,CH27*27
C 99.01.13 open in main
C OPEN
C      OPEN(IUCD,FILE='cano.data',STATUS='OLD')
C      IF (IUCD2.EQ.62) OPEN(IUCD2,FILE='r.dump')
C      IF (IUCD2.EQ.63) OPEN(IUCD2,FILE='sr.dump')
C READ CANOPY DATA
CS      READ(IUCD,*,ERR=997) ICANO
C=======================================================
C MAKE BRF DATABASE : IF ICANO = 0(CANR),ICANO<>2(CANSR) 
C=======================================================
      IF(((IUCD2.EQ.62).AND.(ICANO.EQ.0)).OR.
     &   ((IUCD2.EQ.63).AND.(ICANO.NE.2))) THEN
C 2001. 4.11 SED
C      READ(IUCD,*) LAD
CS      READ(IUCD,*,ERR=997) LAD
C
cs       write(6,*)'NSZA',NSZA
cs       write(6,*)'NEZA',NEZA
cs       write(6,*)'NAA',NAA
C ANGLE POINTS
      DO I=1,NX
         SZARAD(I)=ACOS(XX(I))
      ENDDO
      DO K=1,NAA
         AARAD(K)=FI1(K)
      ENDDO
cs      write(6,*)'AARAD IN GETCN1',AARAD*180./PI
C IF CANR
      IF (IUCD2.EQ.62) THEN
          DO IS=1,NX
             N1=2*IS
             N2=2*(NX-IS)
             EZARAD(1,IS)=0.0
             EZARAD(NY,IS)=(PI/2+SZARAD(NX))/2
             DO K=1,N1-1
                EZARAD(K+1,IS)=SZARAD(IS)*K/N1
             ENDDO
             EZARAD(N1+1,IS)=SZARAD(IS)*2/3+EZARAD(N1,IS)*1/3
             EZARAD(N1+2,IS)=SZARAD(IS)
             IF (IS.LT.NX) THEN
                 DO K=1,N2-1
                    EZARAD(NY-K,IS)=EZARAD(NY,IS)
     &              -(EZARAD(NY,IS)-SZARAD(IS))*K/N2
                 ENDDO
                 EZARAD(NY-N2,IS)=SZARAD(IS)*2/3
     &                           +EZARAD(N1+4,IS)*1/3
             ENDIF
          ENDDO
      ENDIF
C IF CANSR
      IF (IUCD2.EQ.63) THEN
          NDEV=NY-3
          DEV=SRMAX/NDEV
          DO IS=1,NX
             EZARAD(1,IS)=0.0 
             EZARAD(NY,IS)=(SRMAX+PI/2)/2 
             IF(SZARAD(IS).LE.0.0) THEN
                N1=-1
                N2=NY-2
             ELSE
                DO J=1,NDEV
                   IF((DEV*(J-1).LT.SZARAD(IS)).AND.
     &                (SZARAD(IS).LE.DEV*J)) THEN
                       N1=J
                       N2=NDEV-N1
                   ENDIF
                ENDDO
             ENDIF
C
             IF(N1.GT.0) THEN
                IF(N1.GT.1) THEN
                   DO K=1,N1-1
                      EZARAD(K+1,IS)=SZARAD(IS)*K/N1
                   ENDDO
                ENDIF
                EZARAD(N1+1,IS)=SZARAD(IS)*2/3+EZARAD(N1,IS)*1/3
             ENDIF
             EZARAD(N1+2,IS)=SZARAD(IS)
             DO K=1,N2-1
                EZARAD(NY-K,IS)=EZARAD(NY,IS)
     &           -(EZARAD(NY,IS)-SZARAD(IS))*K/N2
             ENDDO
             EZARAD(NY-N2,IS)=SZARAD(IS)*2/3+EZARAD(NY-N2+1,IS)*1/3            
          ENDDO
      ENDIF
C RAD->DEG
      DO IS=1,NX
         DO K=1,NY
            EZADEG(K,IS)=EZARAD(K,IS)*180.0/PI
         ENDDO
      ENDDO
CC CANOPY BRF
C LOOP OF INCIDENT ZENITH ANGLE
      DO NI=1,NX
         THIRAD=SZARAD(NI)
         DO J=1,NY
         EZA1(J)=EZARAD(J,NI)
         ENDDO
C         WRITE(6,*) 'SOLAR ZENITH ANGLE=',SZARAD(NI)*180.0/PI
cs      write(6,*)'LAD,RS,HC,RPL,XLAI,RL,TL',LAD,RS,HC,RPL,XLAI,RL,TL
cs         CALL RPVBRF(R0B,KB,THB,THIRAD,EZA1,AARAD,NSZA,NEZA,NAA
cs     &  ,NI,NY,NAA,CBRF1,IBRF)
         CALL LRSBRF(R0B,KB,THB,THIRAD,EZA1,AARAD,NSZA,NEZA,NAA
     &  ,NI,NY,NAA,CBRF1,IBRF,1,IRFS)
      ENDDO
C WRITE
C 2001. 4.11 SED
C      WRITE(IUCD2,90)
C      WRITE(IUCD2,95) LAD,RS,HC,RPL,XLAI,RL,TL,NX
cs      WRITE(*,90,ERR=998)
cs      WRITE(*,95,ERR=998) LAD,RS,HC,RPL,XLAI,RL,TL,NX
C
      DO NI=1,NX
C 2001. 4.11 SED
C         WRITE(IUCD2,100) SZARAD(NI)
CC         WRITE(IUCD2,100) COS(SZARAD(NI))
C         WRITE(IUCD2,200) (EZADEG(NE,NI),NE=1,NY)
cs         WRITE(*,100,ERR=998) SZARAD(NI)
C         WRITE(IUCD2,100) COS(SZARAD(NI))
cs         WRITE(*,200,ERR=998) (EZADEG(NE,NI),NE=1,NY)
C
         DO NP=1,NAA
         AADEG=AARAD(NP)*180.0/PI
C 2001. 4.11 SED
cs         WRITE(*,300,ERR=998) AADEG,(CBRF1(NI,NE,NP),NE=1,NY)
C
         ENDDO
      ENDDO
C FORMAT FOR WRITING DATA
 90      FORMAT('LAD       RS         HC         RPL       XLAI      
     &   RL         TL     No.of Incident angle')  
 95      FORMAT (I2,',',6(1P,E11.5,','),I3)
100      FORMAT (1P,E12.6,' : Incident zenith angle[rad]')
200      FORMAT('Azimuth   ',',',40(1P,E12.6,','))
300      FORMAT(41(1P,E12.6,','))
C=====================================================================
C USE & LOAD PREVIOUS BRF DATABASE : IF ICANO = 1(CANR),ICANO=2(CANSR)
C====================================================================
      ELSE
C 2001. 4.11 SED
C      READ(IUCD2,350,ERR=999) CH27
C      READ(IUCD2,350,ERR=999) CH27
      READ(IUCD2,350,ERR=999) CH27
      READ(IUCD2,350,ERR=999) CH27
C
      DO 1 IS=1,NX
C 2001. 4.11 SED
C          READ(IUCD2,400) SZARAD(IS),CH27
C          READ(IUCD2,500) CH11,(EZADEG(IE,IS),IE=1,NY)
          READ(IUCD2,400,ERR=999) SZARAD(IS),CH27
          READ(IUCD2,500,ERR=999) CH11,(EZADEG(IE,IS),IE=1,NY)
C
         DO 2 IA=1,NAA
C 2001. 4.11 SED
C            READ(IUCD2,600) AA,(CBRF1(IS,IE,IA),IE=1,NY)
            READ(IUCD2,600,ERR=999) AA,(CBRF1(IS,IE,IA),IE=1,NY)
C
 2       CONTINUE
 1    CONTINUE
C FORMAT FOR LOADING DATA
 350      FORMAT(A27)
 400      FORMAT(E12.6,A27)
 500      FORMAT(A11,40(E12.6,1X))
 600      FORMAT(41(E12.6,1X))
      ENDIF
C 99.01.13 close->rewind
CS      REWIND IUCD
CS      REWIND IUCD2
C      CLOSE(IUCD)
C      CLOSE(IUCD2)
C 2001.4.11 SED
      RETURN
 997  WRITE(6,*) 'File Read Error (cano.data)'
      STOP
 998  WRITE(6,*) 'File Write Error (r.dump or sr.dump)'
      STOP
 999  WRITE(6,*) 'File Read Error (r.dump or sr.dump)'
      STOP
      END
C
      SUBROUTINE CBRFA1(IS,THE,IA,EZA,CBRFR,KNSZA,KNEZA,KNAA
     &,NEZA,BRF)
C INTERPOLATION FOR GETTING CANOPY BRF IN "CANR" AND "CANSR"
C INPUT
C IS    I                   (SOLAR ZENITH ANGLE)ORDER IN BRF DATABASE "CBRFR"
C THE   R          DEG       EMERGING ZENITH ANGLE
C IA    I                   (AZIMUTHA ZENITH ANGLE)ORDER IN BRF DATABASE"CBRFR"
C EZA   R(KNEZA)   DEG       EMERGING ZENITH ANGLE GRID POINTS OF BRF DATABASE
C CBRFR R(KNSZA,KNEZA,KNAA)  BRF DATABASE
C KNSZA I                    SIZE OF CBRFR
C KNEZA I                    SIZE OF CBRFR
C KNAA  I                    SIZE OF CBRFR
C NEZA  I                    NUMBER OF EMERGING ZENITH ANGLE
C OUTPUT
C BRF   R                    INTERPOLATED BRF
C AREA
      DIMENSION CBRFR(KNSZA,KNEZA,KNAA),EZA(KNEZA)
C EZA GRID
      IF((EZA(1).LT.THE).AND.(THE.LE.EZA(NEZA))) THEN
          IDONE=0
          DO JE=1,NEZA
             DIFF=EZA(JE)-THE
             IF((DIFF.GE.0.0).AND.(IDONE.EQ.0)) THEN
                 IGES=JE-1
                 IGEL=JE
                 IDONE=1
             ENDIF
          ENDDO
      ELSE
          IF(THE.GT.EZA(NEZA)) THEN
              IGES=NEZA-1
              IGEL=NEZA
          ENDIF
      ENDIF
CC INTERPOLATION
      YS=CBRFR(IS,IGES,IA)
      YL=CBRFR(IS,IGEL,IA)
      CALL BILIN(EZA(IGES),EZA(IGEL),YS,YL,THE,BRF)
      RETURN
      END
C
      SUBROUTINE GETCN2(IUCD,IUCO,KNA0,KNA1U,KNFI,NSZA,NEZA,NAA
     &,AM0,AM1U,FI,CBRF3,R0B,KB,THB,ICANO,IBRF,IRFS)
C      SUBROUTINE GETCN2(IUCD,KNA0,KNA1U,KNFI,NSZA,NEZA,NAA
C     &,AM0,AM1U,FI,CBRF3)
C GETTING CANOPY BRF DATA
C OPEN "cano.data" AND MAKE/LOAD CANOPY BRF DATABASE IN RTRN21
C WHEN ICANO=0, MAKE NEW BRF DATABASE
C WHEN ICANO=1, USE  BRF DATABASE "rtrn.dump"
C HISTORY
C 98.10.12
C 00.03.14 ARGUMENT KNA0,KNA1U,KNFI --> PARAMETER
C--INPUT
C IUCD   I             DEVISE NUMBER OF "cano.data"
C KNA0
C KNA1U
C KNFI
C NSZA   I             NUMBER OF SOLAR ZENITH ANGLE
C NEZA   I             NUMBER OF EMERGING ZENITH ANGLE
C NAA    I             NUMBER OF AZIMUTHAL ANGLE
C AM0    R(KNA0)   -   COS(SOLAR ZENITH ANGLE)
C AM1U   R(KNA1U)  -   COS(EMERGING ZENITH ANGLE)
C FI     R(KNFI)  DEG  AZIMUTHAL ANGLE
C--OUTPUT
C CBRF3 R(KNA0,KNA1U,KNFI)  -   CANOPY BRF 
C--HISTORY
C 2001. 4.11 SED
C   Adding File Read Check Error (Canopy) by SED
C--PARAMETER
      PARAMETER (PI=3.141592653589793)
C--AREA
      DIMENSION AM0(KNA0),AM1U(KNA1U),FI(KNFI)
C--LOCAL
      DIMENSION SZARAD(NSZA),EZARAD(NEZA),AARAD(NAA),EZA(NEZA)
      INTEGER ICANO
C*****FOR CAPONY BRDF******************************************
      real R0B,KB,THB,CBRF3(KNA0,KNA1U,KNFI)
C*****FOR CAPONY BRDF****************************************** 
      CHARACTER CH11*11,CH19*19      
C 99.01.13 open in main, not here.
C OPEN
C      IUCO=61
C      OPEN(IUCD,FILE='cano.data',STATUS='OLD')
C      OPEN(IUCO,FILE='rtrn.dump')
C READ CANOPY DATA
C 2001. 4.11 SED
CS      READ(IUCD,*,ERR=997) ICANO
C
C===================================
C MAKE BRF DATABASE : IF ICANO = 0,1
C===================================
      IF(ICANO.LT.2) THEN
C 2001. 4.11 SED
CS      READ(IUCD,*,ERR=997) LAD
C
C ANGLE GAUSS POINTS
		DO K=1,NSZA
		   SZARAD(K)=ACOS(AM0(K))
		ENDDO
cs      write(6,*)'SZARAD',SZARAD*180./PI
		Do K=1,NEZA
		   EZARAD(K)=ACOS(-AM1U(K))
		ENDDO
cs      write(6,*)'EZARAD',EZARAD*180./PI
		DO K=1,NAA
		   AARAD(K)=FI(K)*PI/180.0
		ENDDO
cs      write(6,*)'AARAD',AARAD*180./PI
CC CANOPY BRF
C 98.10.13
	    DO NI=1,NSZA
		   THIRAD=SZARAD(NI)
cs         WRITE(6,*) 'SOLAR ZENITH ANGLE=',SZARAD(NI)*180.0/PI
cs          write(6,*)'NSZA',NSZA
cs          write(6,*)'NEZA',NEZA
cs         write(6,*)'NAA',NAA
cs      CALL RPVBRF(R0B,KB,THB,THIRAD,EZARAD,AARAD,KNA0,KNA1U,KNFI
cs     &   ,NI,NEZA,NAA,CBRF3,IBRF)
			CALL LRSBRF(R0B,KB,THB,THIRAD,EZARAD,AARAD,KNA0,KNA1U,KNFI
     &		,NI,NEZA,NAA,CBRF3,IBRF,0,IRFS)
		ENDDO
C WRITE
C 2001. 4.11 SED
C      WRITE(IUCO,90)
C      WRITE(IUCO,95) LAD,RS,HC,RPL,XLAI,RL,TL,NSZA
cs       WRITE(*,90)
cs       WRITE(*,95) R0B,KB,THB,NSZA
C
		DO NI=1,NSZA
C 2001. 4.11 SED
C         WRITE(IUCO,100) SZARAD(NI)*180.0/PI
C         WRITE(IUCO,200) (EZARAD(NE)*180.0/PI,NE=1,NEZA)
cs          WRITE(*,100) SZARAD(NI)*180.0/PI
cs          WRITE(*,200) (EZARAD(NE)*180.0/PI,NE=1,NEZA)
C
			DO NP=1,NAA
				AADEG=AARAD(NP)*180.0/PI
C 2001. 4.11 SED
C          WRITE(IUCO,300) AADEG,(CBRF3(NI,NE,NP),NE=1,NEZA)
cs          WRITE(*,300) AADEG,(CBRF3(NI,NE,NP),NE=1,NEZA)
C
			ENDDO
		ENDDO
C FORMAT FOR WRITING BRF DATABASE
 90      FORMAT(' R0B       KB         THB
     &          No.of Solar zenith angle')  
 95      FORMAT (',',4(1P,E11.5,','),I3)
100      FORMAT (1P,E12.6,' : Solar Zenith angle')
200      FORMAT('Azimuth   ',',',20(1P,E12.6,','))
300      FORMAT(21(1P,E12.6,','))
C=====================================
C USE & LOAD BRF DATABASE : IF ICANO=2
C=====================================
		ELSE
C 2001. 4.11 SED
C      READ(IUCO,350) CH19
C      READ(IUCO,350) CH19
			READ(IUCO,350) CH19
			READ(IUCO,350) CH19
C
			DO 1 IS=1,NSZA
C 2001. 4.11 SED
C          READ(IUCO,400) SZA,CH19
C          READ(IUCO,500) CH11,(EZARAD(IE),IE=1,NEZA)
				READ(IUCO,400) SZA,CH19
				READ(IUCO,500) CH11,(EZARAD(IE),IE=1,NEZA)
C
				DO 2 IA=1,NAA
C 2001. 4.11 SED
C            READ(IUCO,600) AA,(CBRF3(IS,IE,IA),IE=1,NEZA)
					READ(IUCO,600) AA,(CBRF3(IS,IE,IA),IE=1,NEZA)
C
 2				CONTINUE
 1			CONTINUE
C FORMAT FOR LOADING BRF DATABASE
 350      FORMAT(A19)
 400      FORMAT(E12.6,A19)
 500      FORMAT(A11,20(E12.6,1X))
 600      FORMAT(21(E12.6,1X))
      ENDIF
C 99.01.13 close -> rewind
CS      REWIND IUCD
CS      REWIND IUCO
C      CLOSE(IUCD)
C      CLOSE(IUCO)
C 2001.4.11 SED
      RETURN
cs 997  WRITE(6,*) 'Input File Read Error (cano.data)'
cs      STOP
cs 998  WRITE(6,*) 'File Write Error (rtrn.dump)'
cs      STOP
cs 999  WRITE(6,*) 'File Read Error (rtrn.dump)'
cs      STOP
      END
C
C
C********1*************************************************6*********7
C Subroutines for SNOW BRDF
C
C 00.05.09 SED updated SBRFA1 and SBRFA2
C*********************************************************************
      SUBROUTINE SBRFA1(N,THI,THE,FI1,SBRF,SZA,EZA,AA,BRF1)
C GET SNOW BRF FROM SNOW_BRF_DATA : SBRF
C THIS ROUTINE IS USED IN "SNWR" and "SNWSR"
C SNOW BRF DATA were provided by Dr.Teruo Aoki
C SNOW BRF DATA SIZE is 16x16x33
C--HISTORY
C 98.08.05 MADE BY SED
C 00.05.08 UPDATED BY SED
C--INPUT
C N    I             NUMBER OF ELEMENTS in FI1()
C THI  R        DEG  INCIDENT ZENITH ANGLE   0<= THI < 90
C THE  R        DEG  EMERGING ZENITH ANGLE   0<= THE < 90
C FI1  R(N)     RAD  AZIMUTHAL ANGLE         
C SBRF R(20,20,40)   SNOW BRF DATA
C SZA  R(*)     DEG  SOLAR ZENITH ANGLE(INCIDENT ZENITH ANGLE) IN DATABASE
C                    SZA(1)>SZA(2)>....>SZA(16)
C EZA  R(*)     DEG  EMERGING ZENITH ANGLE IN DATABASE
C                    EZA(1)<EZA(2)<....<EZA(16)
C                    0 <= EZA < 90 ( Emerging_ZA ==  View zenith here ) 
C AA   R(*)     DEG  AZIMUTHAL ANGLE IN DATABASE
C                    AA(1)<AA(2)<....<AA(33)
C--OUTPUT
C BRF  R(N)          BRF determined by THI,THE,FI1(N)
C--
      PARAMETER(PI=3.1415926535897932385)
C--AREA
      DIMENSION SBRF(20,20,40),SZA(40),EZA(40),AA(40)
!===================== BY DUAN ==================================
      DIMENSION FI1(40),BRF1(40)
C      DIMENSION FI1(30),BRF1(30)
!===================== BY DUAN ==================================
      DIMENSION TSZA(3),TEZA(3),TAA(3),TAA2(3)
C
C GET GRID POINTS and RATIO for Linear Interpolation
C     JSZA: lower element of Array of SZA. SZA(JSZA)>SZA(JSZA+1) 
C     JEZA: lower element of Array of EZA. EZA(JEZA)<EZA(JEZA+1)
C           JEZA=0 when   0<= THE < EZA(1)
C     JFI : lower element of Arrat of AA.  AA(JAA) < AA(JAA+1)
C     TSZA: ratio for THI
C     TEZA: ratio for THE
C     TAA : ratio for FI1
CC GET JSZA 
      IF ((SZA(16).LT.THI).AND.(THI.LT.SZA(1))) THEN
           CALL LOCATU( SZA, 16, THI, JSZA )
      ELSE
           IF ((SZA(1).LE.THI).AND.(THI.LT.90.0)) THEN
               JSZA = 1
           ELSE IF ( 90.0.LE.THI ) THEN
                WRITE(6,*) 'NG.SBRFA1:THI=',THI,'RANGE:0.0<=THI<90.0'
                STOP
           ENDIF 
           IF ((0.0 .LE.THI).AND.(THI.LE.SZA(16))) THEN
                JSZA = 15
           ELSE IF ( THI.LT.0.0 ) THEN 
                WRITE(6,*) 'NG.SBRFA1:THI=',THI,'RANGE:0.0<=THI<90.0'
                STOP
           ENDIF           
      ENDIF
CC GET RATIO FOR THI
      TSZA(2) = ( THI-SZA(JSZA) )/( SZA(JSZA+1) - SZA(JSZA) )
      TSZA(1) = 1.0 - TSZA(2) 
CC GET JEZA
      IF ((EZA(1).LT.THE).AND.(THE.LT.EZA(16))) THEN
           CALL LOCATL( EZA, 16, THE, JEZA )
CC GET RATIO FOR THE
           TEZA(2) = ( THE-EZA(JEZA) )/( EZA(JEZA+1)-EZA(JEZA) )
           TEZA(1) = 1.0 - TEZA(2)
      ELSE
           IF ((0.0.LE.THE).AND.(THE.LE.EZA(1))) THEN
               JEZA = 0         
               TEZA(2) = ( THE+EZA(1) )/2/EZA(1)
               TEZA(1) = 1.0 - TEZA(2)      
           ELSE IF ( THE.LT.0.0) THEN
                WRITE(6,*) 'NG.SBRFA1:THE=',THE,'RANGE:0.0<=THE<90'
                STOP
           ENDIF
           IF ((EZA(16).LE.THE).AND.(THE.LT.90.0)) THEN
               JEZA = 15
CC GET RATIO FOR THE
               TEZA(2) = ( THE-EZA(JEZA) )/( EZA(JEZA+1)-EZA(JEZA) )
               TEZA(1) = 1.0 - TEZA(2)
           ELSE IF ( THE.GE.90.0 ) THEN
                WRITE(6,*) 'NG.SBRFA1:THE=',THE,'RANGE:0.0<=THE<90'
                STOP
           ENDIF
      ENDIF
C
C LOOP OF FI1:   FI1(1)-->FI1(N)
C WHEN JEZA .NE. 0
C
      IF ( JEZA .NE. 0 ) THEN
      DO 10 I=1,N
         BRF1(I) = 0.0
CC RAD => DEG
         FIDEG = FI1(I) * 180.0/PI
CC GET JAA
         IF ((AA(1).LT.FIDEG).AND.(FIDEG.LT.AA(33)) ) THEN
              CALL LOCATL( AA, 33, FIDEG, JAA )
         ELSE
              IF (FIDEG.LE.0.0) THEN 
                  JAA = 1
              ENDIF
              IF (FIDEG.GE.180.0 ) THEN
                  JAA = 32
              ENDIF
         ENDIF
CC GET RATIO FOR FI1
         TAA(2) = ( FIDEG - AA(JAA) )/( AA(JAA+1) - AA(JAA) )
         TAA(1) = 1.0 - TAA(2)         
C INTERPOLATION
         DO 40 IS=1,2
         DO 30 IE=1,2
         DO 20 IA=1,2
            BRF1(I) = BRF1(I) 
     &    + TSZA(IS)*TEZA(IE)*TAA(IA)
     &    * SBRF(JSZA-1+IS,JEZA-1+IE,JAA-1+IA)
 20         CONTINUE
 30         CONTINUE
 40         CONTINUE
C brf value check
         IF( BRF1(I) .LT.0.0 ) THEN
             WRITE(6,*) 'NG.SBRFA1: BRF1=',BRF1(I)
             STOP
         ENDIF
 10      CONTINUE
C
C LOOP OF FI1:   FI1(1)-->FI1(N)
C WHEN JEZA .EQ.0
C
      ELSE
      DO 50 I=1,N
         BRF1(I) = 0.0
CC RAD => DEG
         FIDEG = FI1(I) * 180.0/PI
         FIDEG2 = ACOS( COS( FI1(I)+PI ) )*180.0/PI
CC GET JAA for FIDEG
         IF ((AA(1).LT.FIDEG).AND.(FIDEG.LT.AA(33)) ) THEN
              CALL LOCATL( AA, 33, FIDEG, JAA )
         ELSE
              IF (FIDEG.LE.0.0) THEN 
                  JAA = 1
              ENDIF
              IF (FIDEG.GE.180.0 ) THEN
                  JAA = 32
              ENDIF
         ENDIF
CC GET JAA2 for FIDEG2
         IF ((AA(1).LT.FIDEG2).AND.(FIDEG2.LT.AA(33)) ) THEN
              CALL LOCATL( AA, 33, FIDEG2, JAA2 )
         ELSE
              IF (FIDEG2.LE.0.0) THEN 
                  JAA2 = 1
              ENDIF
              IF (FIDEG2.GE.180.0 ) THEN
                  JAA2 = 32
              ENDIF
         ENDIF
CC GET RATIO FOR FIDEG
         TAA(2) = ( FIDEG - AA(JAA) )/( AA(JAA+1) - AA(JAA) )
         TAA(1) = 1.0 - TAA(2)
CC GET RATIO FOR FIDEG2         
         TAA2(2) = ( FIDEG2-AA(JAA2) )/( AA(JAA2+1)-AA(JAA2) )
         TAA2(1) = 1.0 - TAA2(2)         
C INTERPOLATION on EZA(1) 
CC FOR FIDEG
         YBRF1 = 0.0
         DO 60 IS=1,2
         DO 70 IA=1,2
            YBRF1 = YBRF1 
     &      + TSZA(IS)*TAA(IA) * SBRF(JSZA-1+IS,1,JAA-1+IA)
 70         CONTINUE
 60         CONTINUE
CC FOR FIDEG2
         YBRF2 = 0.0
         DO 80 IS=1,2
         DO 90 IA=1,2
            YBRF2 = YBRF2 
     &      + TSZA(IS)*TAA2(IA) * SBRF(JSZA-1+IS,1,JAA2-1+IA)
 90         CONTINUE
 80         CONTINUE
C INTERPOLATION of EZA
          BRF1(I) = TEZA(2)*YBRF1+TEZA(1)*YBRF2
C brf value check
         IF( BRF1(I) .LT.0.0 ) THEN
             WRITE(6,*) 'NG.SBRFA1: BRF1=',BRF1(I)
             STOP
         ENDIF
 50      CONTINUE
      ENDIF
      RETURN
      END
C-
      SUBROUTINE SBRFA2(THI,THE,FI,SBRF,SZA,EZA,AA,BRF)
C GET SNOW BRF FROM SNOW_BRF_DATA : SBRF
C THIS ROUTINE IS USED IN "RTRN22"
C SNOW BRF DATA were provided by Dr.Teruo Aoki
C SNOW BRF DATA SIZE is 16x16x33
C--HISTORY
C 98.08.05 MADE BY SED
C 00.05.08 UPDATED BY SED
C--INPUT
C THI  R        DEG  USER DEFINED INCIDENT ZENITH ANGLE  0<= THI < 90
C THE  R        DEG  180[deg]-USER DEFINED EMERGING ZENITH ANGLE  0<= THE < 90
C FI   R        DEG  USER DEFINED AZIMUTHAL ANGLE        0<= FI <= 180
C SBRF R(20,20,40)   SNOW BRF DATA
C SZA  R(*)     DEG  SOLAR ZENITH ANGLE(INCIDENT ZENITH ANGLE) IN DATABASE
C                    SZA(1)>SZA(2)>....>SZA(16)
C EZA  R(*)     DEG  EMERGING ZENITH ANGLE IN DATABASE
C                    EZA(1)<EZA(2)<....<EZA(16)
C                    0 <= EZA < 90 ( Emerging_ZA ==  View zenith here ) 
C AA   R(*)     DEG  AZIMUTHAL ANGLE IN DATABASE
C                    AA(1)<AA(2)<....<AA(33)
C--OUTPUT
C BRF  R             BRF determined by THI,THE,FI
C--
      PARAMETER(PI=3.1415926535897932385)
C--AREA
      DIMENSION SBRF(20,20,40),SZA(20),EZA(20),AA(40)
      DIMENSION TSZA(3),TEZA(3),TAA(3),TAA2(3)
C
C GET GRID POINTS and RATIO for Linear Interpolation
C     JSZA: lower element of Array of SZA. SZA(JSZA)>SZA(JSZA+1) 
C     JEZA: lower element of Array of EZA. EZA(JEZA)<EZA(JEZA+1)
C           JEZA=0 when   0<= THE < EZA(1)
C     JFI : lower element of Arrat of AA.  AA(JAA) < AA(JAA+1)
C     TSZA: ratio for THI
C     TEZA: ratio for THE
C     TAA:  ratio for FI
CC GET JSZA 
      IF ((SZA(16).LT.THI).AND.(THI.LT.SZA(1))) THEN
           CALL LOCATU( SZA, 16, THI, JSZA )
      ELSE
           IF ((SZA(1).LE.THI).AND.(THI.LT.90.0)) THEN
               JSZA = 1
           ELSE IF ( 90.0.LE.THI ) THEN
                WRITE(6,*) 'NG.SBRFA2:THI=',THI,'RANGE:0.0<=THI<90.0'
                STOP
           ENDIF 
           IF ((0.0 .LE.THI).AND.(THI.LE.SZA(16))) THEN
                JSZA = 15
           ELSE IF ( THI.LT.0.0 ) THEN 
                WRITE(6,*) 'NG.SBRFA2:THI=',THI,'RANGE:0.0<=THI<90.0'
                STOP
           ENDIF           
      ENDIF
CC GET RATIO FOR THI
      TSZA(2) = ( THI-SZA(JSZA) )/( SZA(JSZA+1) - SZA(JSZA) )
      TSZA(1) = 1.0 - TSZA(2) 
CC GET JEZA
      IF ((EZA(1).LT.THE).AND.(THE.LT.EZA(16))) THEN
           CALL LOCATL( EZA, 16, THE, JEZA )
CC GET RATIO FOR THE
           TEZA(2) = ( THE-EZA(JEZA) )/( EZA(JEZA+1)-EZA(JEZA) )
           TEZA(1) = 1.0 - TEZA(2)
      ELSE
           IF ((0.0.LE.THE).AND.(THE.LE.EZA(1))) THEN
               JEZA = 0         
               TEZA(2) = ( THE+EZA(1) )/2/EZA(1)
               TEZA(1) = 1.0 - TEZA(2)      
           ELSE IF ( THE.LT.0.0) THEN
                WRITE(6,*) 'NG.SBRFA2:THE=',THE,'RANGE:0.0<=THE<90'
                STOP
           ENDIF
           IF ((EZA(16).LE.THE).AND.(THE.LT.90.0)) THEN
               JEZA = 15
CC GET RATIO FOR THE
               TEZA(2) = ( THE-EZA(JEZA) )/( EZA(JEZA+1)-EZA(JEZA) )
               TEZA(1) = 1.0 - TEZA(2)
           ELSE IF ( THE.GE.90.0 ) THEN
                WRITE(6,*) 'NG.SBRFA2:THE=',THE,'RANGE:0.0<=THE<90'
                STOP
           ENDIF
      ENDIF
C
C WHEN JEZA .NE. 0
C
      IF ( JEZA .NE. 0 ) THEN
         BRF = 0.0
CC GET JAA
         IF ((AA(1).LT.FI).AND.(FI.LT.AA(33)) ) THEN
              CALL LOCATL( AA, 33, FI, JAA )
         ELSE
              IF (FI.LE.0.0) THEN 
                  JAA = 1
              ENDIF
              IF (FI.GE.180.0 ) THEN
                  JAA = 32
              ENDIF
         ENDIF
CC GET RATIO FOR FI
         TAA(2) = ( FI - AA(JAA) )/( AA(JAA+1) - AA(JAA) )
         TAA(1) = 1.0 - TAA(2)         
C INTERPOLATION
         DO 40 IS=1,2
         DO 30 IE=1,2
         DO 20 IA=1,2
            BRF = BRF 
     &    + TSZA(IS)*TEZA(IE)*TAA(IA)
     &    * SBRF(JSZA-1+IS,JEZA-1+IE,JAA-1+IA)
 20         CONTINUE
 30         CONTINUE
 40         CONTINUE
C brf value check
         IF( BRF .LT.0.0 ) THEN
             WRITE(6,*) 'NG.SBRFA2: BRF=',BRF
             STOP
         ENDIF
C
C WHEN JEZA .EQ.0
C
      ELSE
         BRF = 0.0
CC FI2 = FI+180  FI2 never exceed 180.
         FI2 = ACOS( COS( FI*PI/180.0+PI ) ) *180.0/PI
CC GET JAA for FIDEG
         IF ((AA(1).LT.FI).AND.(FI.LT.AA(33)) ) THEN
              CALL LOCATL( AA, 33, FI, JAA )
         ELSE
              IF (FI.LE.0.0) THEN 
                  JAA = 1
              ENDIF
              IF (FI.GE.180.0 ) THEN
                  JAA = 32
              ENDIF
         ENDIF
CC GET JAA2 for FI2
         IF ((AA(1).LT.FI2).AND.(FI2.LT.AA(33)) ) THEN
              CALL LOCATL( AA, 33, FI2, JAA2 )
         ELSE
              IF (FI2.LE.0.0) THEN 
                  JAA2 = 1
              ENDIF
              IF (FI2.GE.180.0 ) THEN
                  JAA2 = 32
              ENDIF
         ENDIF
CC GET RATIO FOR FI
         TAA(2) = ( FI - AA(JAA) )/( AA(JAA+1) - AA(JAA) )
         TAA(1) = 1.0 - TAA(2)
CC GET RATIO FOR FIDEG2         
         TAA2(2) = ( FI2-AA(JAA2) )/( AA(JAA2+1)-AA(JAA2) )
         TAA2(1) = 1.0 - TAA2(2)         
C INTERPOLATION on EZA(1) 
CC FOR FIDEG
         YBRF1 = 0.0
         DO 60 IS=1,2
         DO 70 IA=1,2
            YBRF1 = YBRF1 
     &      + TSZA(IS)*TAA(IA) * SBRF(JSZA-1+IS,1,JAA-1+IA)
 70         CONTINUE
 60         CONTINUE
CC FOR FIDEG2
         YBRF2 = 0.0
         DO 80 IS=1,2
         DO 90 IA=1,2
            YBRF2 = YBRF2 
     &      + TSZA(IS)*TAA2(IA) * SBRF(JSZA-1+IS,1,JAA2-1+IA)
 90         CONTINUE
 80         CONTINUE
C INTERPOLATION of EZA
          BRF = TEZA(2)*YBRF1+TEZA(1)*YBRF2
C brf value check
         IF( BRF .LT.0.0 ) THEN
             WRITE(6,*) 'NG.SBRFA2: BRF=',BRF
             STOP
         ENDIF
      ENDIF
      RETURN
      END
C-
C-
      SUBROUTINE SNWR(M,NDA,AMUA,WA,SBRF,SZA,EZA,AA,R)
C REFLECTION MATRIX OF SNOW SURFACE
C-- HISTORY
C 98.08.31 CREATED BY SED
C---INPUT
C M      I     FOURIER ORDER
C NDA    I     USED DIMENSION OF R AND AMUA
C AMUA  R(NDA)  QUADRATURE POINTS IN HEMISPHERE
C               DECREASING ORDER (ZENITH TO HORIZON, OR, 1 -> 0)
C WA    R(NDA)  QUADRATURE WEIGHTS
C SBRF  R(*,*,*)       SNOW BRF
C SZA   R        DEG   INCIDENT ZENITH ANGLE
C EZA   R        DEG   EMERGING ZENITH ANGLE 
C AA    R        DEG   RELATIVE AZIMUTHAL ANGLE (O-180)
C--- OUTPUT
C R    R(KNDA,NDA)  REFLECTION MATRIX FOR M-TH FOURIER ORDER
C                    ur = R * ui
C$END
      SAVE
      PARAMETER (KNDM  =16)
      PARAMETER(PI=3.1415926535897932385)
      DIMENSION AMUA(KNDM),WA(KNDM),R(KNDM, KNDM)
     &,SZA(20),EZA(20),AA(40),SBRF(20,20,40)
C LOCAL VARIABLES
      PARAMETER (KN=40,KNDM1=KNDM+1)
      DIMENSION X(KNDM1),GX(KN),GW(KN),FI1(KN),COSM1(KN)
     & ,XB(5,2),NS(2),RR(KNDM, KNDM),BRF(KN)
C--INIT
      DATA INIT/1/
C--
      IF(INIT.GT.0) THEN
        N=KN
        INIT=0
        CALL QGAUSN(GW, GX, N )
      ENDIF
C Parameters for integration
C FOR FI-INTEGRATION
      DO 4 K=1,N
      FI1(K)=0.0+PI*GX(K)
      COSM1(K)=2*COS(FI1(K)*M)*PI *GW(K)
 4    CONTINUE
C FOR BOUNDARY
      X(1)=1
      IF(NDA.GE.2) THEN
        DO 1 I = 2, NDA
    1   X(I)=(AMUA(I-1)+AMUA(I))/2
      ENDIF
      X(NDA+1)=0
C
      DO 2 I = 1, NDA
CC SETING MU-BOUDARY FOR MU-INEGRATION
      NS1=1
      XB(1,1)=X(I)
      NS1=NS1+1
      XB(NS1,1)=AMUA(I)
      NS1=NS1+1
      XB(NS1,1)=X(I+1)
      NS(1)=NS1-1
      NS(2)=1
      XB(1,2)=X(I)
      XB(2,2)=X(I+1)
C
      DO 2 J = 1, NDA
      IF(I.EQ.J) THEN
        IEQ=1
       ELSE
        IEQ=2
      ENDIF
CC MU-INTEGRATION
      RIJ=0
      AMI = AMUA(J)
      THI=ACOS(AMI)*180.0/PI
        DO 3 IS=1,NS(IEQ)
        DX=XB(IS,IEQ)-XB(IS+1,IEQ)
        DO 3 II = 1, N
          AME=XB(IS+1,IEQ)+DX*GX(II)
          THE=ACOS(AME)*180.0/PI
          W=DX*GW(II)*AME
C 98.08.31 
C GET SNOW BRF USING INTERPOLATION OF ANGLE(THI, THE, FI)
          CALL SBRFA1(N,THI,THE,FI1,SBRF,SZA,EZA,AA,BRF)
C FI LOOP
          DO 3 K = 1, N
C FI-INTEGRATION
       RIJ=RIJ+(COSM1(K)*BRF(K)*AMI/PI)*W
 3     CONTINUE
      RR(I,J)=RIJ/WA(I)
 2    CONTINUE

C SYMMETRIC OPERATION
      DO 5 I=1,NDA
      DO 5 J=1,I
      RRR=(RR(I,J)+RR(J,I))/2
      R(I,J)=RRR/AMUA(I)*WA(J)
      R(J,I)=RRR/AMUA(J)*WA(I)
 5    CONTINUE
      RETURN
      END
C
      SUBROUTINE SNWSR(M,NDA,AMUA,WA,AM0,SBRF,SZA,EZA,AA,SR)
C SNOW SURFACE REFLECTION SOURCE MATRIX FOR M-TH FOURIER ORDER
C--HISTORY
C 98.08.31 CREATED BY SED
C--- INPUT
C M      I     FOURIER ORDER
C NDA    I     USED DIMENSION OF R AND AMUA
C AMUA  R(NDA) QUADRATURE POINTS IN HEMISPHERE
C              DECREASING ORDER (ZENITH TO HORIZON, OR, 1 -> 0)
C WA    R(NDA) Quadrature weights
C AM0    R     Cos (Solar Zenith Angle)
C SBRF  R(*,*,*)      SNOW BRF DATA
C SZA   R        DEG  SOLAR ZENITH ANGLE
C EZA   R        DEG  EMERGING ZENITH ANGLE
C AA    R        DEG  AZIMUTHAL ZENITH ANGLE
C--- OUTPUT
C SR    SR(KNDA)  REFLECTION SOURCE MATRIX FOR M-TH FOURIER ORDER
C$END
      SAVE
      PARAMETER (KNDM  =16)
      PARAMETER(PI=3.1415926535897932385)
      DIMENSION AMUA(KNDM),WA(KNDM),SR(KNDM)
     &,SBRF(20,20,40),SZA(20),EZA(20),AA(40)
C LOCAL VARIABLES
      PARAMETER (KN=30,KNDM1=KNDM+1)
      DIMENSION X(KNDM1),GX(KN),GW(KN),FI1(KN),COSM1(KN)
     & ,XB(5,2),NS(2),BRF(KN)
C--INIT
      DATA INIT/1/
C
      IF(INIT.GT.0) THEN
        N=KN
        INIT=0
        CALL QGAUSN(GW, GX, N )
      ENDIF
C Parameters for integration
C FOR FI-INTEGRATION
      DO 4 K=1,N
      FI1(K)=0.0+PI*GX(K)
      COSM1(K)=2*COS(FI1(K)*M)*PI *GW(K)
 4    CONTINUE
C FOR BOUNDARY
      X(1)=1
      IF(NDA.GE.2) THEN
        DO 1 I = 2, NDA
    1   X(I)=(AMUA(I-1)+AMUA(I))/2
      ENDIF
      X(NDA+1)=0
      THI=ACOS(AM0)*180.0/PI
C
      DO 2 I = 1, NDA
CC SETING MU-BAOUDARY FOR MU-INEGRATION
      NS1=1
      XB(1,1)=X(I)
      NS1=NS1+1
      XB(NS1,1)=AMUA(I)
      NS1=NS1+1
      XB(NS1,1)=X(I+1)
      NS(1)=NS1-1
      NS(2)=1
      XB(1,2)=X(I)
      XB(2,2)=X(I+1)
C
      IF(AMUA(I).EQ.AM0) THEN
        IEQ=1
       ELSE
        IEQ=2
      ENDIF
CC MU-INTEGRATION
      RIJ=0
        DO 3 IS=1,NS(IEQ)
        DX=XB(IS,IEQ)-XB(IS+1,IEQ)
        DO 3 II = 1, N
          AME=XB(IS+1,IEQ)+DX*GX(II)
          W=DX*GW(II)*AME
          THE=ACOS(AME)*180.0/PI
C 98.08.31
C GET SNOW BRF USING INTERPOLATION OF ANGLE(THI, THE, FI)
          CALL  SBRFA1(N,THI,THE,FI1,SBRF,SZA,EZA,AA,BRF)
C FI LOOP
          DO 3 K = 1, N
C FI-INTEGRATION
         RIJ=RIJ+(COSM1(K)*BRF(K)*AM0/PI)*W
 3       CONTINUE
      SR(I)=RIJ/AMUA(I)/WA(I)
 2    CONTINUE
      RETURN
      END
C
C
      SUBROUTINE GETSN(IUSN,SBRF,SZA,EZA,AA)
C LOAD SNOW BRF DATA
C BRF DATA : TABLE (SOLAR ZENITH, EMERGING ZENITH ANGLE AND AZIMUTHAL ANGLE)
C --HISTORY
C 99.01.13 open() is deleted BY SED. 
C          Snow brf data base : "snow.data" is not opened here. 
C          Snow brf data base is opened in main.
C 2001.04.11 Adding Fire Read Error By SED
C --INPUT
C IUSN  I    NO. OF DEVISE    
C--OUTPUT
C SBRF R(*,*,*) SNOW BRF
C SZA  R(*)  DEG SOLAR ZENITH ANGLE(INCIDENT ZENITH ANGLE)
C EZA  R(*)  DEG EMERGING ZENITH ANGLE
C AA   R(*)  DEG AZIMUTHAL ANGLE
C--LOCAL
C  
C--AREA
      DIMENSION SBRF(20,20,40),SZA(20),EZA(20),AA(40)
      CHARACTER CH12*12,CH15*15, CH21*21
C--INIT
      NSZA=16
      NEZA=16
      NAA=33
C
C 99.01.13 OPEN in main
C      OPEN(IUSN,FILE='snow.data',STATUS='OLD')
      DO 1 IS=1,NSZA
C HEADER
C 2001. 4.11 SED
C          READ(IUSN,100) CH15
          READ(IUSN,100,ERR=999) CH15
C
C SOLAR ZENITH ANGLE
C 2001. 4.11 SED
C          READ(IUSN,200) CH15,SZA(IS),CH21,WVL
C          READ(IUSN,300)CH12,(EZA(IE),IE=1,NEZA)
          READ(IUSN,200,ERR=999) CH15,SZA(IS),CH21,WVL
          READ(IUSN,300,ERR=999)CH12,(EZA(IE),IE=1,NEZA)
C
         DO 2 IA=1,NAA
C 2001. 4.11 SED
C            READ(IUSN,400) AA(IA),(SBRF(IS,IE,IA),IE=1,NEZA)
            READ(IUSN,400,ERR=999) AA(IA),(SBRF(IS,IE,IA),IE=1,NEZA)
C
 2       CONTINUE
 1    CONTINUE
C --FORMAT----------------------------
 100      FORMAT(A15)
 200      FORMAT(A15,E11.5,A21,E11.5)
 300      FORMAT(A12,16(E11.5,1X))
 400      FORMAT(17(E11.5,1X))
C-------------------------------------
C 2001.4.11 SED
      RETURN
 999  WRITE(6,*) 'File Read Error (snow.data)'
      STOP
      END    


C**********************************************************
C NADIMTOOLS.F
C***********************************************************
C                                                          *
C THIS FILE (NADIMTOOLS.F) CONTAINS ALL ROUTINES AND       * 
C FUNCTIONS USED IN THE NADIMBRF.F 			   *	
C							   *	
C***********************************************************
C
C THIS FUNCTION COMPUTES THE "ZERO ORDER" OF SCATTERING BY THE SOIL: 
C THE DOWNWARD RADIATION SCATTERED ONCE BY THE SOIL ONLY.
C
C      REAL FUNCTION RHO_0_NAD(TETA_E,PHI_E,X_LAMBDA_I)
      REAL FUNCTION RHO_0_NAD(TETA_E,PHI_E,X_LAMBDA_I,C1,RS,LAI
     & ,TETA_0,PHI_0,POINTS,WEIGHTS,NUMBER,AG,BG,CG,DG)
C
      PARAMETER (PI=3.141592653589793)
      REAL X_LAMBDA_I
      REAL TETA_E,PHI_E
      REAL KI,KE,XS1,XS2
      REAL XH_P,XLI
      REAL C1
      REAL RS
      REAL LAI
      INTEGER NUMBER
      REAL TETA_0,PHI_0
      REAL POINTS(32),WEIGHTS(32)
      REAL AG,BG,CG,DG 
C
C
      N_C=LAI/X_LAMBDA_I
cs      write(6,*)'N_C',N_C
cs      write(6,*)'X_LAMBDA_I',X_LAMBDA_I
cs      write(6,*)'BEFORE ROSS1'
      XG1=G_ROSS(TETA_0,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
cs      write(6,*)'BEFORE ROSS2'
      XG2=G_ROSS(TETA_E,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
cs      write(6,*)'AFTER ROSS2'
C      XG1=G_ROSS(TETA_0)
C      XG2=G_ROSS(TETA_E)
      KI=XG1/ABS(COS(TETA_0))
      KE=XG2/COS(TETA_E)
cs      write(6,*)'KE',KE
      XS1=(1.-X_LAMBDA_I*KI)**N_C
      XS2=1.
cs      write(6,*)'BEFORE XLI'
      XLI=C1/GEO(TETA_E,TETA_0,PHI_E,PHI_0)
cs      write(6,*)'AFTER XLI'
C
C  ACTUAL OPTICAL PATH TO ACCOUNT FOR THE HOT-SPOT EFFECT
C
      XH_P=HOT_SPOT(LAI,XLI)
cs      write(6,*)'XH_P',XH_P
C
      DO I=1,N_C
      XS2=XS2*(1.-X_LAMBDA_I*KE*XH_P)
cs      write(6,*)'XS2',XS2
      ENDDO
      RHO_0_NAD=RS*XS2*XS1
cs      write(6,*)'RHO_0_NAD=',RHO_0_NAD
      RETURN
      END
C*****************************************************
C
C THIS FUNCTION COMPUTES THE "ONE ORDER" OF SCATTERING BY THE LEAVES ONLY
C
C 
C 
C      REAL FUNCTION RHO_1_NAD(TETA_E,PHI_E,X_LAMBDA_I)
      REAL FUNCTION RHO_1_NAD(TETA_E,PHI_E,X_LAMBDA_I,C1,LAI
     & ,TETA_0,PHI_0,POINTS,WEIGHTS,NUMBER,AG,BG,CG,DG,RL,TL)
C
      PARAMETER (PI=3.141592653589793)
      REAL X_LAMBDA_I
      REAL TETA_E,PHI_E
      REAL XLI
C      REAL KI,KE,XGA,XC1,RC_D
      REAL KI,KE,XGA,XC1
      REAL XG1,XG2,SUM,X_HP,XL
      REAL C1
      REAL LAI,RL,TL
      INTEGER NUMBER
      REAL TETA_0,PHI_0
C--
      REAL POINTS(32),WEIGHTS(32)
      REAL AG,BG,CG,DG
C
C
      N_C=LAI/X_LAMBDA_I
C      XG1=G_ROSS(TETA_0)
C      XG2=G_ROSS(TETA_E)
      XG1=G_ROSS(TETA_0,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
      XG2=G_ROSS(TETA_E,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
      KI=XG1/ABS(COS(TETA_0))
      KE=XG2/COS(TETA_E)

C
C  3D PHASE FUNCTION
C
C      XGA=GAMMA_LEAF(TETA_0,PHI_0,TETA_E,PHI_E)
      XGA=GAMMA_LEAF(TETA_0,PHI_0,TETA_E,PHI_E
     & ,POINTS,WEIGHTS,NUMBER,RL,TL,AG,BG,CG,DG)
      XC1=(1.-X_LAMBDA_I*KI)
      XLI=C1/GEO(TETA_E,TETA_0,PHI_E,PHI_0)

      SUM=0.
      DO K=1,N_C
      XL=X_LAMBDA_I*K
      X_HP=HOT_SPOT(XL,XLI)
      SUM=SUM+XC1**K*X_LAMBDA_I*
     *(1.-X_LAMBDA_I*KE*X_HP)**K
      ENDDO
      RHO_1_NAD=SUM/(COS(TETA_E)*ABS(COS(TETA_0)))*XGA
      RETURN
      END
C***************************************************
C
C THIS FUNCTION COMPUTES THE MULTIPLE SCATTERING TERM 
C AVERAGED IN AZIMUTH. 
C	
C      REAL FUNCTION RHO_MULT_NAD(TETA_U)
      REAL FUNCTION RHO_MULT_NAD(TETA_U,TETA_0
     & ,POINTS,WEIGHTS,NUMBER,LAI,I0,XIF,XI1U,XI1,XIMT
     & ,AG,BG,CG,DG,RL,TL)
C
      PARAMETER (PI=3.141592653589793)
      REAL XMU0,XM,XR,MU,GU,XIM(21),M,DL
      REAL G0,TETA_U,SUM,SUM1,SUM2,X
      REAL XSU(21),XQ0U(21),XQ1(21),XGAMA_U(40)
CC      REAL FPAR_TUR,ALB_TUR,TR_TUR,TR_DIR,TRAN
      REAL WEIGHTS(32),POINTS(32)
      REAL TL,RL,LAI
      INTEGER NUMBER
      REAL I0(21,40),XIF(21,40),XI1U(21,40)
      REAL XI1,XIMT
      REAL TETA_0
C
C
      DO I=1,21
      DO J=1,20 
      ENDDO
      ENDDO
      XMU0=ABS(COS(TETA_0))
      M=20
      DL=LAI/M
C      G0=G_ROSS(TETA_0)
      G0=G_ROSS(TETA_0,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
      XM=0.5*(1.-1.)
      XR=0.5*(1.+1.)
C
C	GAMMA (TETA(J) --> TETA_U)
C
      DO J=1,NUMBER
      X=XM+XR*POINTS(J)
C      XGAMA_U(J)=FASE_LEAF(ACOS(X),TETA_U)
      XGAMA_U(J)=FASE_LEAF(ACOS(X),TETA_U,POINTS,WEIGHTS
     & ,NUMBER,RL,TL,AG,BG,CG,DG)
      ENDDO
C
C
C	MULTIPLE SOURCE S(K) FOR VIEWING ANGLE = TETA_U
C           
C
      DO K=1,M
      SUM=0.
      DO J=1,NUMBER
      X=XM+XR*POINTS(J)
      SUM=SUM+2.*XGAMA_U(J)*XR*
     *WEIGHTS(J)*(XIF(K+1,J)+XIF(K,J))/2.
      ENDDO
      XSU(K)=SUM
      ENDDO
C
C
C	ZERO ORDER SOURCE
C
C
      DO K=M,1,-1
      SUM1=0.
      DO J=NUMBER/2+1,NUMBER
      X=XM+XR*POINTS(J)
      SUM1=SUM1+WEIGHTS(J)*XR*2.*XGAMA_U(J)
     * *(I0(K+1,J)+I0(K,J))/2.
      ENDDO
      XQ0U(K)=SUM1
      ENDDO
C
C FIRST ORDER SOURCE
C
      DO K=1,M
      SUM2=0.
      DO J=1,NUMBER
      X=XM+XR*POINTS(J)
      SUM2=SUM2+2.*XGAMA_U(J)*XR*
     *WEIGHTS(J)*(XI1U(K+1,J)+XI1U(K,J))/2.
      ENDDO
      XQ1(K)=SUM2
      ENDDO
C
C      GU= G_ROSS(TETA_U)
      GU= G_ROSS(TETA_U,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
      MU= COS(TETA_U)
C 99.11.19 SED for SGI
      MMMM = M+1
      XIM(MMMM) = XIMT + XI1
C      XIM(M+1) = XIMT + XI1
      DO K=M,1,-1
      S1=XSU(K)+XQ1(K)+XQ0U(K)
      AA=GU/2.-MU/DL
      BB=GU/2.+MU/DL
      XIM(K)=(S1-XIM(K+1)*AA)/BB
      ENDDO
      RHO_MULT_NAD=XIM(1)/(2.*ABS(COS(TETA_0)))
      RETURN
      END
C***************************************************
C 	
C FUNCTION G_ROSS = ROSS FUNCTION
C
      REAL FUNCTION G_ROSS(TETA_P,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
C      REAL FUNCTION G_ROSS(TETA_P)
      PARAMETER (PI=3.141592653589793)
      REAL WEIGHTS(32),POINTS(32),AG,BG,CG,DG
      INTEGER NUMBER
      REAL A,B,TETA_P,SSC,XM,XR,X1,X2
      INTEGER I

      SSC=0.
      A=0.
      B=PI/2.
      XM=0.5*(B+A)
      XR=0.5*(B-A)
      DO 33 I=NUMBER/2+1,NUMBER
      DX=XR*POINTS(I)
C      X1=FG_ROSS(TETA_P,XM+DX)
C      X2=FG_ROSS(TETA_P,XM-DX)
      X1=FG_ROSS(TETA_P,XM+DX,AG,BG,CG,DG)
      X2=FG_ROSS(TETA_P,XM-DX,AG,BG,CG,DG)
      SSC=SSC+WEIGHTS(I)*(X1+X2)
 33   CONTINUE
      SSC=SSC*XR
      G_ROSS=SSC
      RETURN
      END
C
C            ------------------------
C
      REAL FUNCTION FG_ROSS(TETA_P,X,AG,BG,CG,DG)
C      REAL FUNCTION FG_ROSS(TETA_P,X)
      REAL TETA_I,X
      REAL TETA_P,AG,BG,CG,DG
      TETA_I=TETA_P
C      FG_ROSS=GL_BUN(X)*PSI_ROSS(TETA_I,X)
      FG_ROSS=GL_BUN(X,AG,BG,CG,DG)*PSI_ROSS(TETA_I,X)
      RETURN
      END
C
C***************************************************
C
C	FONCTIONS BUNNIK (OR GL_BUN(TETA_L)*SIN(TETA_L))
C    TO COMPUTE THE LAD 'S FUNCTIONS.
C
c      REAL FUNCTION GL_BUN(X)
      REAL FUNCTION GL_BUN(X,AG,BG,CG,DG)
      PARAMETER (PI=3.141592653589793)
      REAL AG,BG,CG,DG

      GL_BUN=2./PI*(AG+BG*COS(2.*CG*X))+DG*SIN(X)
      RETURN
      END
C
C****************************************************
C
C PHASE FUNCTION.
C
C      REAL FUNCTION GAMMA_LEAF(TETA_P,PHI_P,TETA_I,PHI_I)
C      RAEL AG,BG,CG,DG,RL,TL
      REAL FUNCTION GAMMA_LEAF(TETA_P,PHI_P,TETA_I,PHI_I
     & ,POINTS,WEIGHTS,NUMBER,RL,TL,AG,BG,CG,DG)
      PARAMETER (PI=3.141592653589793)
      REAL WEIGHTS(32),POINTS(32),AG,BG,CG,DG
C      REAL WEIGHTS,POINTS
      INTEGER NUMBER
      REAL TETA_I,PHI_I,TETA_P,PHI_P,GAUSG
      REAL TL,RL

      REAL YM,YR,XM,XR,SY,DX,DY,SD
      XM=0.5*(PI/2.+0.)
      XR=0.5*(PI/2.-0.)
      YM=0.5*(2.*PI+0.)
      YR=0.5*(2.*PI-0.)
      GAUSG=0.
      DO J=1,NUMBER
      DX=XM+XR*POINTS(J)
      SY=0.
      DO I=1,NUMBER
      DY=YM+YR*POINTS(I)
C      SD=FGAMMA_LEAF(TETA_P,PHI_P,DX,DY,TETA_I,PHI_I)
      SD=FGAMMA_LEAF(TETA_P,PHI_P,DX,DY,TETA_I,PHI_I
     & ,RL,TL,AG,BG,CG,DG)
      SY=SY+WEIGHTS(I)*SD*XR
      ENDDO
      GAUSG=GAUSG+WEIGHTS(J)*SY*YR
      ENDDO
      GAMMA_LEAF=GAUSG/2.
      RETURN
      END
C
C             --------------------------
C
C      REAL FUNCTION FGAMMA_LEAF(TETA_P,PHI_P,X,Y,TETA_I,PHI_I)
      REAL FUNCTION FGAMMA_LEAF(TETA_P,PHI_P,X,Y,TETA_I,PHI_I
     & ,RL,TL,AG,BG,CG,DG)
      PARAMETER (PI=3.141592653589793)
      REAL TETA_P,PHI_P,X,Y,TETA_I,PHI_I
      REAL TL,RL
      REAL AG,BG,CG,DG

      REAL F,G1,DI,DP,DPP
C      G1=GL_BUN(X)
      G1=GL_BUN(X,AG,BG,CG,DG)
      DP=COS(X)*COS(TETA_I)+SIN(X)*SIN(TETA_I)*COS(PHI_I-Y)
      DPP=COS(X)*COS(TETA_P)+SIN(X)*SIN(TETA_P)*COS(PHI_P-Y)
      DI=DP*DPP
      IF (DI.LT.0.) THEN
      F=RL
      ELSE
      F=TL
      ENDIF
      FGAMMA_LEAF=G1*F/PI*ABS(DP)*ABS(DPP)
      RETURN
      END
C
C               ------------------------------
C
      REAL FUNCTION PSI_ROSS(TETA_I,X)
      PARAMETER (PI=3.141592653589793)
      REAL XMU,SMU,XP,FIT
      XMU=COS(X)
      SMU=SIN(X)
      IF (XMU.EQ.1.) THEN
      PSI_ROSS=COS(TETA_I)
      ELSE
      IF (SIN(TETA_I).EQ.0.) THEN
      PSI_ROSS=XMU
      ELSE
      IF (SMU.EQ.0.) THEN
      XP=0.
      ELSE
      XP=1.*COS(TETA_I)/SIN(TETA_I)*XMU/SMU
      ENDIF
      IF (ABS(XP).GT.1.) THEN
      PSI_ROSS=COS(TETA_I)*XMU
      ELSE
      FIT=ACOS(-XP)
      PSI_ROSS=COS(TETA_I)*XMU*(2.*FIT/PI-1.)
     * +2./PI*SQRT(1.-COS(TETA_I)**2)
     * *SQRT(1.-COS(X)**2)*SIN(FIT)
      ENDIF
      ENDIF
      ENDIF
      PSI_ROSS=ABS(PSI_ROSS)
      RETURN
      END
C*****************************************************
C
C AZIMUTHALLY AVERAGED PHASE FUNCTION (AFTER SHULTIS ET AL)
C
C      REAL FUNCTION FASE_LEAF(TETA_I,TETA_E)
      REAL FUNCTION FASE_LEAF(TETA_I,TETA_E,POINTS,WEIGHTS
     & ,NUMBER,RL,TL,AG,BG,CG,DG)
      PARAMETER (PI=3.141592653589793)
      REAL WEIGHTS(32),POINTS(32)
C      REAL WEIGHTS,POINTS
      REAL TL,RL
      REAL TETA_I,TETA_E
      INTEGER NUMBER
      REAL AG,BG,CG,DG
      REAL XM,XR,SUM

      XM=0.5*(PI/2.+0.)
      XR=0.5*(PI/2.-0.)
      SUM=0.
      DO J=1,NUMBER
      X=XM+XR*POINTS(J)
C      SUM=SUM+WEIGHTS(J)*GL_BUN(X)*
      SUM=SUM+WEIGHTS(J)*GL_BUN(X,AG,BG,CG,DG)*
     * (TL*PSI_LEAF_P(TETA_E,TETA_I,X)+RL*PSI_LEAF_M(TETA_E,TETA_I,X))
      ENDDO
      FASE_LEAF=SUM*XR
      RETURN
      END
C
C----------------------------------------------------------
C
      REAL FUNCTION PSI_LEAF_P(TETA_E,TETA_I,TETA_L)
      REAL XH1,XH2,XH3,XH4
      REAL TETA_E,TETA_I,TETA_L,XMU_E,XMU_I,XMU_L

      XMU_E=COS(TETA_E)
      XMU_I=COS(TETA_I)
      XMU_L=COS(TETA_L)
      XH1=XH_LEAF(XMU_E,XMU_L)
      XH2=XH_LEAF(XMU_I,XMU_L)
      XH3=XH_LEAF(-XMU_E,XMU_L)
      XH4=XH_LEAF(-XMU_I,XMU_L)
      PSI_LEAF_P=XH1*XH2+XH3*XH4
      RETURN
      END
C
      REAL FUNCTION PSI_LEAF_M(TETA_E,TETA_I,TETA_L)
      REAL TETA_E,TETA_I,TETA_L,XMU_E,XMU_I,XMU_L

      XMU_E=COS(TETA_E)
      XMU_I=COS(TETA_I)
      XMU_L=COS(TETA_L)
      XH1=XH_LEAF(XMU_E,XMU_L)
      XH2=XH_LEAF(-XMU_I,XMU_L)
      XH3=XH_LEAF(-XMU_E,XMU_L)
      XH4=XH_LEAF(XMU_I,XMU_L)
      PSI_LEAF_M=XH1*XH2+XH3*XH4
      RETURN
      END
C
C       ------------------------------
C
      REAL FUNCTION XH_LEAF(XMU,XMU_L)
      PARAMETER (PI=3.141592653589793)
      REAL X1,XMU_L,XMU,XFI,XH1,XH2,XH3

      IF(XMU.EQ.1.) THEN
      IF(XMU_L.EQ.1.) THEN
      XH_LEAF=XMU*XMU_L
      RETURN
      ELSE
      IF(XMU_L.EQ.-1.) THEN
      XH_LEAF=0.
      RETURN
      ELSE	
      IF(XMU_L.GT.0.) THEN
      XH_LEAF=XMU*XMU_L
      RETURN		
      ELSE
      XH_LEAF=0.
      RETURN
      ENDIF
      ENDIF
      ENDIF
      ELSE
      IF(XMU_L.EQ.1.) THEN
      IF(XMU.EQ.1.) THEN
      XH_LEAF=XMU*XMU_L
      RETURN
      ELSE
      IF(XMU.EQ.-1.) THEN
      XH_LEAF=0.
      RETURN
      ELSE
      IF(XMU.GT.0.) THEN
      XH_LEAF=XMU*XMU_L
      RETURN
      ELSE
      XH_LEAF=0.
      ENDIF
      ENDIF
      ENDIF
      ELSE
      IF(XMU.EQ.-1.) THEN
      IF (XMU_L.LT.0.) THEN
      XH_LEAF=XMU*XMU_L
      RETURN
      ELSE
      XH_LEAF=0.
      RETURN
      ENDIF
      ELSE
      IF(XMU_L.EQ.-1.) THEN 
      IF(XMU.GT.0.) THEN
      XH_LEAF=0.
      RETURN
      ELSE
      XH_LEAF=XMU*XMU_L
      RETURN
      ENDIF
      ELSE
      X1=XMU*XMU_L/(SIN(ACOS(XMU))*SIN(ACOS(XMU_L)))
      IF(X1.GT.1.) THEN
      XH_LEAF=XMU*XMU_L
      RETURN
      ELSE
      IF(X1.LT.-1.) THEN
      XH_LEAF=0.
      RETURN	
      ELSE
      XFI=ACOS(-X1)
      XH1=XMU*XMU_L*XFI
      XH2=SQRT(1.-XMU**2.)*SQRT(1.-XMU_L**2.)
      XH3=SIN(XFI)
      XH_LEAF=ABS(XH1+XH2*XH3)/PI
      RETURN
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      END
C
C********************************************************
C
C HOT_SPOT FUNCTION (AFTER VERSTRAETE ET AL)
C
      REAL FUNCTION HOT_SPOT(X,XLI)
      PARAMETER (PI=3.141592653589793)
      REAL X,XLI
      IF (X.LT.XLI) THEN
      HOT_SPOT=(1.-4./(3.*PI))*X/XLI
      ELSE
      HOT_SPOT=1.-4./(3.*PI)*XLI/X
      ENDIF
      RETURN
      END
C*******************************************************
C
      REAL FUNCTION GEO(TETA_E,TETA_0,PHI_E,PHI_0)
      REAL LI1,LI2
      REAL TETA_E,TETA_0,PHI_0,PHI_E

      LI1=TAN(TETA_0)**2+TAN(TETA_E)**2
      LI2=-2.*TAN(TETA_E)*TAN(TETA_0)*COS(PHI_0-PHI_E)
      GEO=SQRT(ABS(LI1+LI2))
      IF(GEO.LT.1.E-35) GEO=1.E-35
      RETURN
      END
C
C*******************************************************
C
C HOT-SPOT DEEP
C
      REAL FUNCTION DEEP(TETA_0,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
C      REAL FUNCTION DEEP(TETA_0)
C--
      REAL POINTS(32),WEIGHTS(32)
      REAL AG,BG,CG,DG
      INTEGER NUMBER

      REAL TETA_0,X_A
      X_A=0.005
      DEEP=-DLOG(dble(X_A))*ABS(COS(TETA_0))
     & /G_ROSS(TETA_0,POINTS,WEIGHTS,NUMBER,AG,BG,CG,DG)
C    & /G_ROSS(TETA_0)
      RETURN
      END
C
C********************************************************
C
C RADIUS OF A SINGLE HOLE BETWEEN LEAVES 
C
C      REAL FUNCTION SUN_FLECK(TETA_0)
      REAL FUNCTION SUN_FLECK(TETA_0,X_LY,A_F,POINTS,WEIGHTS
     & ,NUMBER,AG,BG,CG,DG)
      PARAMETER (PI=3.141592653589793)
      REAL TETA_0,X_LY,X_L_OPT
      REAL POINTS(32),WEIGHTS(32)
      INTEGER NUMBER

      X_L_OPT=X_LY
C      X_R2=ABS(COS(TETA_0))/(G_ROSS(TETA_0)*X_L_OPT)
C      X_LAMBDA_TROU=-LOG(0.9)*ABS(COS(TETA_0))/G_ROSS(TETA_0)
C      X_TROU=X_LAMBDA_TROU/A_F
C      A_TROU=(1.-X_TROU*A_F*G_ROSS(TETA_0)/ABS(COS(TETA_0)))/X_TROU
       GRS=G_ROSS(TETA_0,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
      X_R2=ABS(COS(TETA_0))/(GRS*X_L_OPT)
      X_LAMBDA_TROU=-LOG(0.9)*ABS(COS(TETA_0))/GRS
      X_TROU=X_LAMBDA_TROU/A_F
      A_TROU=(1.-X_TROU*A_F*GRS/ABS(COS(TETA_0)))/X_TROU

      X_R1=SQRT(A_TROU/PI)
      SUN_FLECK=SQRT(X_R2)*X_R1
      RETURN
      END
C**********************************************************
C**********************************************************
C
C	SUBROUTINES
C
C*********************************************************
C*********************************************************
C
C	GEOMETRY OD THE VEGETATION CANOPY AND
C       COMPUTATION OF THE HOT-SPOT PARAMETER	
C
      SUBROUTINE ARCHI (X_LAI,TETA_S,DF,X_NF
     & ,A_F,H_C,X_LY,R,C1,POINTS,WEIGHTS,NUMBER,AG,BG,CG,DG)
C      SUBROUTINE ARCHI (X_LAI,TETA_S)
      PARAMETER (PI=3.141592653589793)
      REAL X_LAI,TETA_S
C      REAL WEIGHTS,POINTS
      REAL WEIGHTS(32),POINTS(32)
C      REAL TL,RL,LAI,RS
      REAL AG,BG,CG,DG
C      REAL TETA_0,PHI_0
      INTEGER NUMBER
      REAL C1
      REAL DF,X_NF,A_F,H_C,X_LY,R
C      REAL I0,XIF,XI1U,XI1,XIMT

      A_F=(DF/2.)**2*PI
      X_NF=X_LAI/(A_F*H_C)
C      X_LY=DEEP(TETA_S)
      X_LY=DEEP(TETA_S,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)

C      R=SUN_FLECK(TETA_S)
      R=SUN_FLECK(TETA_S,X_LY,A_F,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
      C1=2.*R*X_LAI/H_C
      RETURN
      END
C
C*********************************************************
C
C	MULTIPLE SCATTERING INTENSITIES IN THE DIRECTION
C       CORRESPONDING TO THE GAUSS QUADRATURE.
C	
C 
C      SUBROUTINE  MULTIPLE_DOM(TETA_0)
      SUBROUTINE  MULTIPLE_DOM(TETA_0,POINTS,WEIGHTS
     & ,NUMBER,RS,RL,TL,LAI,I0,XIF,XI1U,XI1,XIMT,AG,BG,CG,DG)
      PARAMETER (PI=3.141592653589793)
      REAL WEIGHTS(32),POINTS(32)
C      REAL WEIGHTS,POINTS
      REAL TL,RL,LAI,RS
      REAL AG,BG,CG,DG
      REAL TETA_0
      INTEGER NUMBER
C      REAL I0,XIF,XI1U,XI1,XIMT
      REAL I0(21,40),XIF(21,40),XI1U(21,40),XI1,XIMT
      REAL XMU0,G0,XM,XR,X
      REAL GG(40),S(21,40)
      REAL Q0M(21,40),XI(21,40)
      REAL Q1(21,40),XGAMA_XY(40,40),XGAMA_0(40)
      REAL I00
      XMU0=ABS(COS(TETA_0))
      M=20
      DL=LAI/FLOAT(M)
C      G0=G_ROSS(TETA_0)
      G0=G_ROSS(TETA_0,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
      XM=0.5*(1.+(-1.))
      XR=0.5*(1.-(-1.))
C
      I00=1.
C
C COMPUTATION OF THE G-FUNCTION
C
      DO J=1,NUMBER
      X=XM+XR*POINTS(J)
C      GG(J)=G_ROSS(ACOS(X))
      GG(J)=G_ROSS(ACOS(X),POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
      ENDDO
C
C INITIALIZATION OF SOURCES AND INTENSITIES = 0
C
      DO K=1,M
      DO J=1,NUMBER
      S(K,J)=0.
      XIF(K,J)=0.
      ENDDO
      ENDDO
C
C COMPUTATION OF THE UNCOLLIDED INTENSITIES
C
      DO K=1,M+1
      XL=FLOAT(K-1)*DL
      DO I=1,NUMBER/2
      X=XM+XR*POINTS(I)
      IF((ABS(X)).EQ.XMU0) THEN
      I0(K,I)=I00*EXP(-G0/XMU0*XL)
      ELSE
      I0(K,I)=0.
      ENDIF
      ENDDO	
      ENDDO
C
      DO K=M+1,1,-1
      XL=FLOAT(K-1)*DL
      DO I=NUMBER/2+1,NUMBER
      X=XM+XR*POINTS(I)
      I0(K,I)=2.*RS*I00*XMU0*EXP(-G0/XMU0*LAI)*
     *EXP(-GG(I)/X*(LAI-XL))
      ENDDO
      ENDDO
C	
C TABLE OF 2-D PHASE FUNCTION FOR THE GAUSS QUADRATURE ANGLES
C
      DO I=1,NUMBER
      Y=XM+XR*POINTS(I)
      DO J=1,NUMBER
      X=XM+XR*POINTS(J)
C      XGAMA_XY(I,J)=FASE_LEAF(ACOS(X),ACOS(Y))
      XGAMA_XY(I,J)=FASE_LEAF(ACOS(X),ACOS(Y),POINTS,WEIGHTS
     & ,NUMBER,RL,TL,AG,BG,CG,DG)
      ENDDO
      ENDDO
C
C COMPUTATION OF THE ZERO ORDER SOURCE
C
      DO K=M,1,-1
      DO I=1,NUMBER
      Y=XM+XR*POINTS(I)
      SUM=0.
      DO J=NUMBER/2+1,NUMBER
      X=XM+XR*POINTS(J)
      SUM=SUM+WEIGHTS(J)*XR*2.*XGAMA_XY(I,J)
     *  *(I0(K+1,J)+I0(K,J))/2.
      ENDDO
      Q0M(K,I)=SUM  
      ENDDO
      ENDDO
C
C TABLE OF 2-D PHASE FUNCTION FOR ILLUMINATION --> QUADRATURE ANGLES
C	
      DO I=1,NUMBER
      X=XM+XR*POINTS(I)
C      XGAMA_0(I)=FASE_LEAF(TETA_0,ACOS(X))
      XGAMA_0(I)=FASE_LEAF(TETA_0,ACOS(X),POINTS,WEIGHTS
     & ,NUMBER,RL,TL,AG,BG,CG,DG)
      ENDDO
C
C COMPUTATIONS OF THE FIRST ORDER INTENSITIES
C
      DO K=1,M+1
      XL=FLOAT(K-1)*DL
      DO J=1,NUMBER/2
      X=XM+XR*POINTS(J)
      IF((ABS(X)).NE.XMU0) THEN
      XI(K,J)=I00*2.*XGAMA_0(J)*XMU0*
     * (EXP(-G0/XMU0*XL)
     * -EXP(-GG(J)/ABS(X)*XL))/
     * (GG(J)*XMU0-G0*ABS(X))
      XI1U(K,J)=I00*2.*XGAMA_0(J)*XMU0*
     * (EXP(-G0/XMU0*XL)
     * -EXP(-GG(J)/ABS(X)*XL))/
     * (GG(J)*XMU0-G0*ABS(X))
      ELSE
      XI(K,J)=I00*XL*2.*XGAMA_0(J)
     * *EXP(-G0/XMU0*XL)/XMU0
      XI1U(K,J)=I00*XL*2.*XGAMA_0(J)
     * *EXP(-G0/XMU0*XL)/XMU0
      ENDIF
      ENDDO
      ENDDO	
C
      XI1=0.
      DO J=1,NUMBER/2
      X=XM+XR*POINTS(J)
      XI1=XI1+WEIGHTS(J)*XR*ABS(X)*2.*RS*XI(M+1,J)
      ENDDO
C
      DO K=M+1,1,-1
      XL=FLOAT(K-1)*DL
      DO J=NUMBER/2+1,NUMBER
      X=XM+XR*POINTS(J)
      XI(K,J)=I00*2.*XGAMA_0(J)*XMU0*
     * (EXP(-G0/XMU0*XL)-
     * EXP(-GG(J)/X*(LAI-XL))*EXP(-G0/XMU0*LAI))/
     * (G0*X+GG(J)*XMU0)
C
      XI1U(K,J)=I00*2.*XGAMA_0(J)*XMU0*
     * (EXP(-G0/XMU0*XL)-
     * EXP(-GG(J)/X*(LAI-XL))*EXP(-G0/XMU0*LAI))/
     * (G0*X+GG(J)*XMU0)
      ENDDO
      ENDDO
      DO J=NUMBER/2+1,NUMBER
      XI(M+1,J)=0.
      XI1U(M+1,J)=0.
      ENDDO
C
      DO K=1,M
      DO I=1,NUMBER
      Y=XM+XR*POINTS(I)
      SUM=0.
      DO J=1,NUMBER
      X=XM+XR*POINTS(J)
      SUM=SUM+WEIGHTS(J)*XR*2.*XGAMA_XY(I,J)
     * *(XI(K+1,J)+XI(K,J))/2.
      ENDDO
      Q1(K,I)=SUM
      ENDDO
      ENDDO
C
C MULTIPLE INTENSITIES
C
      DO K=1,M+1
      DO J=1,NUMBER/2
      XI(K,J)=0.
      ENDDO
      ENDDO
      L=0
  111 L=L+1
      DO K=1,M
      DO J=1,NUMBER/2
      X=XM+XR*POINTS(J)
      XI(K+1,J)=(S(K,J)+Q0M(K,J)+Q1(K,J)-
     * XI(K,J)*(GG(J)/2.+X/DL))/(GG(J)/2.-X/DL)
      ENDDO
      ENDDO
      XIMT=0.
      DO J=1,NUMBER/2
      X=XM+XR*POINTS(J)
      XIMT=XIMT+WEIGHTS(J)*XR*2.*RS*ABS(X)*XI(M+1,J)
      ENDDO
      DO J=NUMBER/2+1,NUMBER
      XI(M+1,J)=XIMT+XI1
      ENDDO
      DO K=M,1,-1
      DO J=NUMBER/2+1,NUMBER
      X=XM+XR*POINTS(J)
      XI(K,J)=(S(K,J)+Q0M(K,J)+Q1(K,J)-
     * XI(K+1,J)*(GG(J)/2.-X/DL))/(GG(J)/2.+X/DL)
      ENDDO
      ENDDO
C
      NT=0
      DO K=1,M+1
      DO J=1,NUMBER
      XNN=ABS(XIF(K,J)-XI(K,J))
      IF(XNN.LT.(1.E-4)) NT=NT+1
      XIF(K,J)=XI(K,J)
      ENDDO
      ENDDO
      IF ((L.LT.1000).AND.(NT.NE.(M+1)*NUMBER)) THEN
C
C MULTIPLE SOURCE 
C
      DO K=1,M
      DO I=1,NUMBER
      Y=XM+XR*POINTS(I)
      SUM=0.
      DO J=1,NUMBER
      X=XM+XR*POINTS(J)
      SUM=SUM+WEIGHTS(J)*XR*2.*XGAMA_XY(I,J)
     * *(XI(K+1,J)+XI(K,J))/2.
      ENDDO
      S(K,I)=SUM
      ENDDO
      ENDDO
      GOTO 111
      ENDIF
      RETURN
      END
C
C******************************************************
C
C	ENERGY BALANCE
C
C
C      SUBROUTINE ENERGIE(TETA_I,PHI_I,FPAR,ALBEDO_SYS,TRANS_TOTALE)
      SUBROUTINE ENERGIE(TETA_I,PHI_I,FPAR,ALBEDO_SYS,TRANS_TOTALE
     & ,WEIGHTS,POINTS,RS,TL,RL,LAI
     & ,AG,BG,CG,DG,NUMBER,I0,XIF,XI1U,XI1,XIMT,TETA_0,PHI_0)
      PARAMETER (PI=3.141592653589793)
C
      REAL WEIGHTS(32),POINTS(32)
C      REAL WEIGHTS,POINTS
      REAL RS
      REAL TL,RL,LAI
      REAL AG,BG,CG,DG
      INTEGER NUMBER
      REAL I0(21,40),XIF(21,40),XI1U(21,40)
C      REAL I0,XIF,XI1U
      REAL XI1,XIMT
      REAL TETA_0,PHI_0
C      REAL FPAR_TUR,ALB_TUR,TRAN
C
C
      REAL TETA_S,PHI_S,FPAR,ALBEDO_SYS,TRANS_TOTALE
      REAL GS , XMUS , XM , XR, YM ,YR
      REAL PHI_W,TETA_W,X,Y,X_LAMBDA(32,32),MM(32,32)
      REAL X_RHO_0(32,32),X_RHO_1(32,32),X_RHO_M(32,32)
      REAL SUM_Y,SUM_X,SUM_K,GAMMA_0(32,32)
      REAL G_W(32),T_I1D(32,32)
CC      REAL TR_TUR , TR_DIR_TUR
C
C       TETA_S = (180. - THETA_I )* PI / 180.
       TETA_S = (180. - TETA_I )* PI / 180.
       PHI_S = (180.- PHI_I)* PI / 180.
C       PHI_S = (180.- PHI_S)* PI / 180.
C
C BRF FOR THE GAUSS POINTS   
C
C      GS= G_ROSS (TETA_S)
      GS= G_ROSS (TETA_S,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
      XMUS = COS(TETA_S)
C    
      XM=0.5*(1.-1.)
      XR=0.5*(1.+1.)
      YM=0.5*(2.*PI-0.)
      YR=0.5*(2.*PI+0.)
C
C TABLE OF THE G_ROSS FUNCTION
C NUMBER OF LEVELS MM
C AND BRF FOR ALL POINTS OF THE GAUSS QUADRATURE
C
      DO J=1,NUMBER
      X=XM+XR*POINTS(J)
      TETA_W=ACOS(X)
C      G_W(J)= G_ROSS(TETA_W)
      G_W(J)= G_ROSS(TETA_W,POINTS,WEIGHTS,NUMBER
     & ,AG,BG,CG,DG)
      ENDDO 
C
      DO I=1,NUMBER
      Y=YM+YR*POINTS(I)
      PHI_W=Y
      DO J=1,NUMBER
      X=XM+XR*POINTS(J)
      TETA_W=ACOS(X)
      X_LAMBDA(I,J)=0.01*ABS(COS(TETA_S))/GS*
     *ABS(COS(TETA_W))/G_W(J)
      MM(I,J)=LAI/X_LAMBDA(I,J)
C      X_RHO_0(I,J)=RHO_0_NAD(TETA_W,PHI_W,X_LAMBDA(I,J))  
C      X_RHO_1(I,J)=RHO_1_NAD(TETA_W,PHI_W,X_LAMBDA(I,J))  
C      X_RHO_M(I,J)=RHO_MULT_NAD(TETA_W) 
      X_RHO_0(I,J)=RHO_0_NAD(TETA_W,PHI_W,X_LAMBDA(I,J),C1,RS,LAI
     & ,TETA_0,PHI_0,POINTS,WEIGHTS,NUMBER,AG,BG,CG,DG)
      X_RHO_1(I,J)=RHO_1_NAD(TETA_W,PHI_W,X_LAMBDA(I,J),C1,LAI
     & ,TETA_0,PHI_0,POINTS,WEIGHTS,NUMBER,AG,BG,CG,DG,RL,TL)  
      X_RHO_M(I,J)=RHO_MULT_NAD(TETA_W,TETA_0
     & ,POINTS,WEIGHTS,NUMBER,LAI,I0,XIF,XI1U,XI1,XIMT
     & ,AG,BG,CG,DG,RL,TL) 
      ENDDO
      ENDDO
C
C COMPUTATION OF THE ALBEDO
C
      SUM_Y=0.
      DO I=1,NUMBER
      Y=YM+YR*POINTS(I)
      SUM_X=0.
      DO J=NUMBER/2+1,NUMBER 
      X=XM+XR*POINTS(J)
      SUM_X=SUM_X+(X_RHO_0(I,J)+X_RHO_1(I,J)+X_RHO_M(I,J))*
     *WEIGHTS(J)*ABS(X)*XR
      ENDDO
      SUM_Y = SUM_Y + SUM_X*WEIGHTS(I)*YR
      ENDDO     
      ALBEDO_SYS = SUM_Y  / (PI)
C
C DOWNWARD FIRST COLLIDED INTENSITIES WITH DISCRETE APPROACH   
C
      DO I=1,NUMBER
      Y = YM + YR * POINTS (I)
      PHI_W = Y
      DO J=1,NUMBER/2
      X = XM + XR * POINTS (J)
      TETA_W = ACOS(X)
C      GAMMA_0(I,J) = GAMMA_LEAF (TETA_S,PHI_S,TETA_W,PHI_W)
      GAMMA_0(I,J) = GAMMA_LEAF (TETA_S,PHI_S,TETA_W,PHI_W
     & ,POINTS,WEIGHTS,NUMBER,RL,TL,AG,BG,CG,DG)
      ENDDO
      ENDDO
C        
      DO I=1,NUMBER
      Y = YM + YR * POINTS (I)
      PHI_W = Y
      DO J=1,NUMBER/2
      X = XM + XR * POINTS (J)
      TETA_W = ACOS(X)
      SUM_K = 0
      MMM=MM(I,J)
      DO K=1,MMM
      SUM_K = SUM_K + (1.-X_LAMBDA(I,J)*G_W(J)/ABS(X))**(MMM-K)*
     *X_LAMBDA(I,J)*
     *(1.- X_LAMBDA(I,J)*GS/ABS(XMUS)) **K 
      ENDDO 
      T_I1D (I,J) = SUM_K * GAMMA_0 (I,J) * (1./ (PI * ABS(X)))
      ENDDO 
      ENDDO 
C 
C TRANSMISSION OF THE FIRST COLLIDED INTENSITIES	
C
      SUM_Y=0.
      DO I=1,NUMBER
      Y=YM+YR*POINTS(I)
      SUM_X=0.
      DO J=1,NUMBER/2
      X=XM+XR*POINTS(J)
      SUM_X=SUM_X+ T_I1D(I,J)*
     +WEIGHTS(J)*ABS(X)*XR
      ENDDO
      SUM_Y = SUM_Y + SUM_X * WEIGHTS(I) * YR
      ENDDO     
      TRANS_1 = SUM_Y / ABS(XMUS) 
C
      SUM_Y=0.
      DO I=1,NUMBER
      Y=YM+YR*POINTS(I)
      SUM_X=0.
      DO J=1,NUMBER/2
      X=XM+XR*POINTS(J)
      SUM_X=SUM_X+ T_I1D(I,J)*RS*
     +WEIGHTS(J)*ABS(X)*XR
      ENDDO
      SUM_Y = SUM_Y + SUM_X * WEIGHTS(I) * YR
      ENDDO
      TRANS_1_IN = SUM_Y / ABS(XMUS)
C
C TRANSMISSION OF MULTIPLY SCATTERED INTENSITIES
C
      SUM_X=0.
      DO J=1,NUMBER/2
      X=XM+XR*POINTS(J)
      SUM_X=SUM_X+XIF(21,J)*
     *WEIGHTS(J)*ABS(X)*XR
      ENDDO
      TRANS_M = SUM_X / ( ABS (XMUS) )
C
      SUM_X=0.
      DO J=NUMBER/2+1,NUMBER
      X=XM+XR*POINTS(J)
      SUM_X=SUM_X+XIMT*
     *WEIGHTS(J)*ABS(X)*XR
      ENDDO
      TRANS_IN_M= SUM_X / ( ABS (XMUS) )
      X_LAMBDA_0 =  0.01 * *ABS(XMUS)/GS*
     *ABS(XMUS)/GS
      M0=LAI/X_LAMBDA_0
      XT_DIRECTE=(1.- X_LAMBDA_0*GS/ABS(XMUS))**M0 
      TRANS_TOTALE= XT_DIRECTE + TRANS_M + TRANS_1
      FPAR = 1.- ALBEDO_SYS - (1. - RS) * TRANS_TOTALE 
      RETURN
      END
C
C**********************************************************
C
C	COEFFICIENTS FOR THE BUNNIK'S FUNCTION
C
      SUBROUTINE BUNNIK(ILD,AG,BG,CG,DG)
C      SUBROUTINE BUNNIK(ILD)
      REAL AG,BG,CG,DG

      AG=1.
      BG=1.
      CG=1.
      DG=1.
      IF (ILD.EQ.1) THEN
      DG=0.
      ENDIF
      IF (ILD.EQ.2) THEN
      BG=-1.
      DG=0.
      ENDIF
      IF (ILD.EQ.3) THEN
      BG=-1.
      CG=2.
      DG=0.
      ENDIF
      IF (ILD.EQ.4) THEN
      CG=2.
      DG=0.
      ENDIF
      IF (ILD.EQ.5) THEN
      AG=0.
      BG=0.
      CG=0.
      ENDIF
      RETURN
      END
C
C****************************************************************
C 
C	CALCULATION OF THE QUADRATURE POINTS AND WEIGHTS (GAUSS)
C
      SUBROUTINE GAULEG(X1,X2,X,W,N)
      PARAMETER (PI=3.141592653589793)
C      REAL WEIGHTS,POINTS
c      REAL TL,RL,LAI,RS
c      REAL AG,BG,CG,DG
c      REAL TETA_0,PHI_0
c      REAL C1
c      REAL I0,XIF,XI1U,XI1,XIMT
c      REAL X_NF,A_F,H_C,X_LY,R
      INTEGER N
      REAL*8 P1,P2,P3,PP,XM,Z,Z1
      REAL X1,X2,X(N),W(N),XL
      PARAMETER (EPS=3.D-14)
      M=(N+1)/2
      XM=0.5D0*(X2+X1)
      XL=0.5D0*(X2-X1)
      DO 12 I=1,M
      Z=COS(3.141592654D0*(I-.25D0)/(N+.5D0))
1     CONTINUE
      P1=1.D0
      P2=0.D0
      DO 11 J=1,N
      P3=P2
      P2=P1
      P1=((2.D0*J-1.D0)*Z*P2-(J-1.D0)*P3)/J
11    CONTINUE
      PP=N*(Z*P1-P2)/(Z*Z-1.D0)
      Z1=Z
      Z=Z1-P1/PP
      IF(ABS(Z-Z1).GT.EPS)GO TO 1
      X(I)=XM-XL*Z
      X(N+1-I)=XM+XL*Z
      W(I)=2.D0*XL/((1.D0-Z*Z)*PP*PP)
      W(N+1-I)=W(I)
12    CONTINUE
      RETURN
      END
******************************************************************

C 2002.05 PT
      SUBROUTINE NAINT(M,L,NDA,NA0,I,FSOL,AM1U,AM0,W,T,T1,T2
     &     ,ZEE,QE,QIE,VPE,VME,PT10,PR10,PT1,PR1,ALFA,BETA
     &     ,NPLK1,DPE,DME,CPLK,AI,INIT)
C      SUBROUTINE AINT(M,L,NDA,NA0,I,FSOL,AM1U,AM0,W,T,T1,T2
C     &     ,ZEE,QE,QIE,VPE,VME,PT10,PR10,PT1,PR1,ALFA,BETA
C     &     ,NPLK1,DPE,DME,CPLK,AI)
C--- HISTORY
C 2002.05    Change AINT -> NAINT (because "AINT" is a same name with a 
C            built in function for FORTRAN
C            Carried out performance tuning
C--- INPUT
C M       I         FOURIER ORDER
C L       I         LAYER NUMBER
C NDA
C NA0
C FSOL
C I       I         STREAM NUMBER FOR AM1(I)
C AM1U    R         USER DEFINED DIRECTION
C AM0   R(KNA0)
C W       R         SINGLE SCATTERING ALBEDO
C T       R         OPTICAL DEPTH FOR INTERPOLATION
C T1      R         OPTICAL DEPTH AT THE TOP OF THE SUBLAYER
C T2      R         OPTICAL DEPTH AT THE BOTTOM OF THE SUBLAYER.
      PARAMETER (KNDM  =16)
      PARAMETER (KNA1U =100)
      PARAMETER (KNA1  =100)
      PARAMETER (KNA0  =2)
      PARAMETER (KNLN  =35)
      PARAMETER (KPLK1 =2)
C
      PARAMETER (PI=3.141592654)
      DIMENSION AM0(KNA0),ZEE(KNDM,KNLN),QE(KNDM,KNDM,KNLN)
     & ,QIE(KNDM,KNDM,KNLN),VPE(KNDM,KNA0,KNLN),VME(KNDM,KNA0,KNLN)
     & ,PT10(KNA1,KNA0),PR10(KNA1,KNA0),PT1(KNA1,KNDM),PR1(KNA1,KNDM)
     & ,DPE(KNDM,KPLK1,KNLN),DME(KNDM,KPLK1,KNLN),AI(KNA0)
     & ,ALFA(KNDM,KNA0),BETA(KNDM,KNA0),CPLK(KPLK1,KNLN)
C WORKING AREAS
      DIMENSION H1(KNDM),H2(KNDM),C(KNDM),SL(KNDM),SL1(KNDM),EI(KNA0)
     & ,PKI(KPLK1)
C 2002.05 PT
      DIMENSION TRNS1(KNA0)
      SAVE TRNS1
C
      TT1=0
      TT2=T2-T1
      TT =T -T1
      AM1=ABS(AM1U)

C 2002.05 PT
      IF (INIT .EQ. 1) THEN
        INIT=0
        DO 20 J=1,NA0
          TRNS0=-T1/AM0(J)
          TRNS1(j)=EXPFN(TRNS0)*FSOL
   20   CONTINUE
      ENDIF
C
      DO 1 J=1,NDA
        SUM1=0
        SUM2=0
        DO 2 K=1,NDA
        SUM1=SUM1+(PT1(I,K)+PR1(I,K))*QE (K,J,L)
    2   SUM2=SUM2+(PT1(I,K)-PR1(I,K))*QIE(J,K,L)
        H1(J)=W*SUM1
    1 H2(J)=W*SUM2
C DOWNWARD INTENSITY
      IF(AM1U.GT.0.0) THEN
        DO 3 J=1,NA0
    3   CALL EXINT(AM1U,AM0(J),TT,TT1,TT,EI(J))
        DO 4 J=1,NDA
    4   CALL CSINT(AM1U,ZEE(J,L),TT2,TT,TT1,TT,C(J),SL(J),SL1(J))
        PDU=0
        IF(M.EQ.0 .AND. NPLK1.GT.0) THEN
          CALL PKINT(AM1U,TT,TT1,TT,NPLK1,PKI)
          DO 6 J=1,NPLK1
          SUM1=0
          DO 5 K=1,NDA
    5     SUM1=SUM1+PT1(I,K)*DPE(K,J,L)+PR1(I,K)*DME(K,J,L)
    6     PDU=PDU+(W*SUM1+2*PI*(1-W)*CPLK(J,L))*PKI(J)
        ENDIF
        DO 7 J=1,NA0
C 2001.02 PT
        PVU=PT10(I,J)*TRNS1(J)
C        TRNS0=-T1/AM0(J)
C        TRNS0=EXPFN(TRNS0)*FSOL
C        PVU=PT10(I,J)*TRNS0
C
        HU=0
        DO 8 K=1,NDA
        HU=HU+(H1(K)*C  (K)-H2(K)*SL(K))*ALFA(K,J)
     &       +(H1(K)*SL1(K)-H2(K)*C (K))*BETA(K,J)
    8   PVU=PVU+PT1(I,K)*VPE(K,J,L)+PR1(I,K)*VME(K,J,L)
        PVU=W*PVU*EI (J)
        AI(J)=(HU+PVU+PDU)/AM1
    7   CONTINUE
       ELSE
C UPWARD INTENSITY
        DO 13 J=1,NA0
   13   CALL EXINT(AM1U,AM0(J),TT,TT,TT2,EI(J))
        DO 14 J=1,NDA
   14   CALL CSINT(AM1U,ZEE(J,L),TT2,TT,TT,TT2,C(J),SL(J),SL1(J))
        PDU=0
        IF(M.EQ.0 .AND. NPLK1.GT.0) THEN
          CALL PKINT(AM1U,TT,TT,TT2,NPLK1,PKI)
          DO 16 J=1,NPLK1
          SUM1=0
          DO 15 K=1,NDA
   15     SUM1=SUM1+PR1(I,K)*DPE(K,J,L)+PT1(I,K)*DME(K,J,L)
   16     PDU=PDU+(W*SUM1+2*PI*(1-W)*CPLK(J,L))*PKI(J)
        ENDIF
        DO 17 J=1,NA0
C 2002.05 PT
        PVU=PR10(I,J)*TRNS1(J)
C        TRNS0=-T1/AM0(J)
C        TRNS0=EXPFN(TRNS0)*FSOL
C        PVU=PR10(I,J)*TRNS0
C
        HU=0
        DO 18 K=1,NDA
        HU=HU+(H1(K)*C  (K)+H2(K)*SL(K))*ALFA(K,J)
     &       +(H1(K)*SL1(K)+H2(K)*C (K))*BETA(K,J)
   18   PVU=PVU+PR1(I,K)*VPE(K,J,L)+PT1(I,K)*VME(K,J,L)
        PVU=W*PVU*EI (J)
        AI(J)=(HU+PVU+PDU)/AM1
   17   CONTINUE
      ENDIF
      RETURN
      END
C 2002.05 PT
C 2001.04.05 SED : ADD DDLW For Water Leaving Radiance
      SUBROUTINE FTRN21(INDG,INIT,M,INDA,INDT,IMTHD,NA1U,AM1U,NA0,AM0
     &,NDA,AMUA,WA,NLN,NLGN1,G,DPT,OMG,NPLK1,CPLK,GALB,BGND
     &,FSOL,NTAU,UTAU,SCR,SCI,FLXD,FLXU,AI,ERR
     &,SBRF,SZA,EZA,AZANG,IUCD,IUCR,IUCSR,DDLW,
     &R0B,KB,THB,ICANO,INITR,IBRF,XLAND,XOCEAN,NW,ir,IRFS)
C 00.03.15 BY SED: 
C      SUBROUTINE FTRN21(INDG,INIT,M,INDA,INDT,IMTHD,NA1U,AM1U,NA0,AM0
C     &,NDA,AMUA,WA,NLN,NLGN1,G,DPT,OMG,NPLK1,CPLK,GALB,BGND
C     &,FSOL,NTAU,UTAU,SCR,SCI,FLXD,FLXU,AI,ERR
C     &,SBRF,SZA,EZA,AZANG,IUCD,IUCR,IUCSR)
C      SUBROUTINE FTRN21(INDG,INIT,M,INDA,INDT,IMTHD,NA1U,AM1U,NA0,AM0
C     &,NDA,AMUA,WA,NLN,NLGN1,G,DPT,OMG,NPLK1,CPLK,GALB,BGND
C     &,FSOL,NTAU,UTAU,SCR,SCI,FLXD,FLXU,AI,ERR)
C
C SOLVE THE RADIATIVE TRANSFER IN ATMOSPHERE SYSTEM FOR EACH FOURIER
C COMPONENT.
C  -DOM AND ADDING METHOD-
C BY TERUYUKI NAKAJIMA
C SINCE THIS SYSTEM DOES NOT INCLUDE A OCEAN SURFACE,
C KNLT=KNLN IN TRN1.
C
C--- HISTORY
C 89.11. 2 CREATED FROM STRN7 WITH THERMAL EMISSION
C 90.12. 1 CALL AINT(M,L,  ->  CALL AINT(M,LT,
C 92.12.   ADD OCEAN SURFACE
C 93. 1.   CHANGE UPWARD ADDING
C            USE CSPL1 AND CSPLI
C     3.10 BUG ( XX,YY USED K1 )
C     4. 5 NSF = 1 for initialization (Terry)
C     5. 4 INDG instead of NSF and LOSW
C          Subtracted single scattering if INDG>0
C 94. 5. 7 Bug for interpolating AIB(J)
C 95. 5.25 Replace GRNDL by GRNDL3
C 95. 6. 2 With GRNDO1 and with SCR and SCI
C 96. 3.17 Add ERT By Takashi Nakajima to debug thermal.
C 00. 3.15 Add canopy and snow brf 
C 2001.04.05 Add Water Leaving Radiance
C 2002.05  Change the name AINT->NAINT
C          Carried out Performance tuining
C--- INPUT
C INDG       I       0: Lambert surface
C                    1: Ocean surface initialized
C                    2: Ocean surface with no-initialization
C                    When INDG>0 and IMTHD>0 then single scattering correction
C                      for ocean surface reflection
C INIT       I       1: INITIALIZE THE PART (DEPENDENT AMUA, UTAU)
C                    0:  BYPASS THE M-INDEPENDENT PART.
C M          I       FORIER ORDER.
C INDA       I       0: FLUX ONLY.
C                    1: INTENSITY USING AM1U.
C                    2: NA1U AND AM1U ARE SUPPOSED TO BE
C                       2*NDA AND (-AMUA, +AMUA).
C INDT       I       0: SET USER DEFINED DEPTH.
C                    1: SAME AS ABOVE.
C                    2: SET NTAU AND UTAU AS NLN1 AND DPT.
C IMTHD      I      -1: NT,  0: DMS-METHOD  FOR INTENSITY/FLUX
C                    1: MS,  2:TMS,  3:IMS-METHOD FOR INTENSITY.
C                    When INDG>0 and IMTHD>0 then single scattering correction
C                      for ocean surface reflection
C NA1U       I       NUMBER OF EMERGENT ZENITH ANGLES IN THE SPHERE.
C                      NA1U=2*NDA WHEN INDA=2.
C AM1U    R(KNA1U)   CONSINE OF THE EMERGENT ZENITH ANGLES.
C                      + FOR DOWNWARD, - FOR UPWARD.
C                      AM1U = (-AMUA, +AMUA) WHEN INDA=2.
C NA0        I       NUMBER OF THE SOLAR INCIDENCES.
C AM0     R(NA0)     CONSINE OF SOLAR ZENITH ANGLES .GT.0.
C NDA        I       NO. OF ZENITH-QUADRATURE ANGLES IN THE HEMISPHERE.
C AMUA    R(KNDM)    COSINE OF THE ZENITH-QUADRATURE ANGLES. 1 TO 0.
C WA      R(KNDM)    CORRESPONDING QUADRATURE WEIGHTS.
C NLN        I       NUMBER OF ATMOSPHERIC SUBLAYERS.
C NLGN1   I(KNLN)    MAXIMUM ORDER OF THE LEGENDRE SESIES OF THE PHASE
C                      FUNCTION + 1
C G     R(KLGN1,     LEGENDRE MOMENTS OF PHASE FUNCTION.
C           KNLN)      G(1,L)=1
C DPT     R(KNLN1)   OPTICAL DEPTH AT THE INTERFACES BETWEEN SUBLAYERS.
C                      TOP TO BOTTOM (TOP = 0 FOR NORMAL APPLICATION).
C OMG     R(KNLN)    SINGLE SCATTERING ALBEDO.
C NPLK1      I       NUMBER OF ORDER TO APPROXIMATE PLANK + 1.
C                      IF 0 THEN NO THERMAL.
C CPLK    R(KPLK1    PLANK FUNCTION (B) =
C         ,KNLN)       SUM(IB=1,NPLK1) CPLK(IB,L) TAU**(IB-1).
C                      TAU IS OPTICAL DEPTH MEASURED FROM
C                      THE TOP OF THE SUBSURFACE.
C GALB       R       Ground albedo if INDG=0
C                    U10 (m/sec)   if INDG>0
C BGND       R       (1-GALB)*B when INDG=0
C                    B          when INDG>0
C                    where B is Plank function
C                      (SAME UNIT AS FSOL).
C FSOL       R       SOLAR IRRADIANCE (W/M2/MICRON).
C NTAU       I       NUMBER OF USER DEFINED OPTICAL DEPTHS.
C                      NTAU=NLN+1 WHEN INDT=2.
C UTAU    R(KNTAU)   OPTICAL DEPTHS WHERE THE FIELD IS CALCULTED.
C                      TOP TO BOTTOM.
C                      UTAU=DPT WHEN INDT=2.
C--- OUTPUT
C INDG               if 1 then 2
C INIT               0
C FLXD  R(KNA0,KNTAU) DOWNWARD FLUX AT UTAU.
C FLXU               SAME AS FLXD BUT FOR UPWARD FLUX.
C AI      R(KNA1U,   I(MU1U(I), MU0(J), L)
C           KNA0,    INTENSITY AT UTAU.
C           KNTAU)   Subtracted single scattering if INDG>0
C ERR      C*64      ERROR INDICATER.
C--- CORRESPONDENCE BETWEEN VARIABLES AND PARAMETERS
C KNA1U       NA1U
C KNA1        NA1
C KNA0        NA0
C KNDM        NDA
C KNLN        NLN
C KNLN1       KNLN+1
C KNTAU       NTAU
C KLGN1       NLGN1
C KPLK1       NPLK1
C--- NOTES FOR LOCAL VARIABLES
C NA1        I       NUMBER OF STREAMS FOR AM1.
C AM1     R(KNA1)    ABS(AM1U).
C IIAM1   I(KNA1U)   DIRECTION NUMBER OF AM1 FOR EACH AM1U.
C                      + FOR DOWNWARD, - FOR UPWARD.
C IITAU   I(KNTAU)   SUBLAYER NUMBER FOR USER DEFINED DEPTHS.
C$ENDI
      SAVE EPS,WMP,WMM,IITAU
C PARAMETERS
      PARAMETER (KNA1U =100)
      PARAMETER (KNA1  =100)
      PARAMETER (KNA0  =2)
      PARAMETER (KNDM  =16)
      PARAMETER (KNLN  =35)
      PARAMETER (KNTAU =2)
      PARAMETER (KPLK1 =2)
C
      PARAMETER (KNLN1=KNLN+1,KLGN1=2*KNDM)
      PARAMETER (PI=3.141592654,RAD=PI/180.0)
C AREAS FOR THIS ROUTINE
      CHARACTER ERR*(*)
      DIMENSION AM1U(KNA1U),AM0(KNA0),AMUA(KNDM),WA(KNDM)
     & ,NLGN1(KNLN),G(KLGN1,KNLN),DPT(KNLN1),OMG(KNLN)
     &,CPLK(KPLK1,KNLN),UTAU(KNTAU),FLXD(KNA0,KNTAU),FLXU(KNA0,KNTAU)
     &,AI(KNA1U,KNA0,KNTAU)
C WORKING AREAS
      DIMENSION AM1(KNA1),WMP(KNDM),WMM(KNDM)
     &,PL1(KNA1,KLGN1),PL0(KNA0,KLGN1),PLA(KNDM,KLGN1),GBUF(KLGN1)
     &,PT(KNDM,KNDM),PR(KNDM,KNDM),PT0(KNDM,KNA0),PR0(KNDM,KNA0)
     &,PR1(KNA1,KNDM),PT1(KNA1,KNDM),PR10(KNA1,KNA0),PT10(KNA1,KNA0)
     &,ALFA(KNDM,KNA0),BETA(KNDM,KNA0),UUP(KNDM,KNA0),UDN(KNDM,KNA0)
     &,AII(KNA1U,KNA0,KNLN1),AIB(KNA0),IIAM1(KNA1U),IITAU(KNTAU),CCP(3)
C FOR HOMOG2
      DIMENSION CPL(KPLK1),R(KNDM,KNDM),T(KNDM,KNDM)
     &,ER(KNDM,KNA0),ET(KNDM,KNA0),ZEIG(KNDM)
     &,Q(KNDM,KNDM),QI(KNDM,KNDM),C11(KNDM,KNDM),C22(KNDM,KNDM)
     &,VP(KNDM,KNA0),VM(KNDM,KNA0),DP(KNDM,KPLK1),DM(KNDM,KPLK1),
     &RRS(10,KNDM,KNDM,KNDM),SRS(10,KNDM,KNDM)
C
      DIMENSION ERT(KNDM,KNA0)
C FOR TRN1 (INCLUDE GROUND AS A SUBLAYER)
      PARAMETER (KNLNM=KNLN1,KNLNM1=KNLNM+1,KNLTM=KNLN1)
      DIMENSION IUP(KNLNM),IDN(KNLNM),NDD(KNLNM1)
     &, RE(KNDM,KNDM,KNLTM),  TE(KNDM,KNDM,KNLTM)
     &,SER(KNDM,KNA0,KNLNM),SET(KNDM,KNA0,KNLNM)
     &,RUP(KNDM,KNA0,KNLNM1),RDN(KNDM,KNA0,KNLNM1)
C FOR INTENSITY INTERPOLATION IN MULTI-LAYER SYSTEM
      DIMENSION QE(KNDM,KNDM,KNLN),QIE(KNDM,KNDM,KNLN)
     &,C1E(KNDM,KNDM,KNLN),C2E(KNDM,KNDM,KNLN),ZEE(KNDM,KNLN)
     &,DPE(KNDM,KPLK1,KNLN),DME(KNDM,KPLK1,KNLN)
     &,VPE(KNDM,KNA0,KNLN),VME(KNDM,KNA0,KNLN)
C SET AND CLEAR VARIABLES
C*****FOR CAPONY BRDF******************************************
      real R0B,KB,THB
      integer ICANO
C*****FOR CAPONY BRDF****************************************** 
      DIMENSION XX(KNDM),YY(KNDM),A(KNDM),B(KNDM),C(KNDM),D(KNDM)
C 00.03.15 BY SED: SNOW BRDF DATABASE
      DIMENSION SBRF(20,20,40),SZA(20),EZA(20),AZANG(40)
C 2001.04.05 SED FOR Water Leaving Radiance
      DIMENSION DDLW(KNA0)
C
      ERR=' '
cs      write(6,*)'IN FTRN','   INITR=',INITR
      IF(INIT.GT.0) THEN
        INIT=0
        CALL CPCON(CCP)
        EPS=CCP(1)*10
        DO 1 I=1,NDA
        WMP(I)=SQRT(WA(I)*AMUA(I))
    1   WMM(I)=SQRT(WA(I)/AMUA(I))
C SET SUBLAYER WHERE USER-DEFINED DEPTH RESIDES -IITAU-.
        DO 40 IT=1,NTAU
        ODP=UTAU(IT)
        EPS1=ODP*EPS
        DO 41 L=1,NLN
          IF((ODP-DPT(L))*(ODP-DPT(L+1)).LE.0.0) GOTO 42
   41   CONTINUE
        ERR='OUT OF BOUNDS IN LAYER SETTING'
        RETURN
   42   IF(ABS(ODP-DPT(L)).LE.EPS1) THEN
          IITAU(IT)=-L
         ELSE
          IF(ABS(ODP-DPT(L+1)).LE.EPS1) THEN
            IITAU(IT)=-(L+1)
           ELSE
            IITAU(IT)=L
          ENDIF
        ENDIF
   40   CONTINUE
      ENDIF
C SET AM1, IIAM1
      IF(INDA.GT.0) THEN
        NA1=1
        AM1(1)=ABS(AM1U(1))
        IIAM1(1)= 1
        IF(NA1U.GE.2) THEN
          DO 43 IU=2,NA1U
          X=ABS(AM1U(IU))
          DO 44 J=1,NA1
          IF(ABS(X-AM1(J)).LE.EPS) THEN
            IIAM1(IU)= J
            GOTO 43
          ENDIF
   44     CONTINUE
          NA1=NA1+1
          IF(NA1.GT.KNA1) THEN
            ERR='SETTING ERROR OF AM1U'
            RETURN
          ENDIF
          AM1(NA1)=X
          IIAM1(IU)= NA1
   43     CONTINUE
        ENDIF
      ENDIF
C CHECK MAXIMUM NLGN1
      MXLGN1=0
      DO 2 L=1,NLN
    2 MXLGN1=MAX(MXLGN1,NLGN1(L))
      IF(MXLGN1.LE.0) THEN
        ERR='NLGN1 ARE ALL ZERO'
        RETURN
      ENDIF
      M1=M+1
C SET LEGENDRE POLYNOMIALS.
      CALL PLGND(M1,MXLGN1,NA0,KNA0,AM0 ,PL0)
      CALL PLGND(M1,MXLGN1,NDA,KNDM,AMUA,PLA)
C SCALING FOR SYMMETLICITY
        DO 5 I=1,NDA
        DO 5 J=1,MXLGN1
    5   PLA(I,J)=WMM(I)*PLA(I,J)
C SOLVE THE EIGENVALUE PROBLEM OF ATMOSPHERIC SUBLAYERS.
      DO 6 L=1,NLN
        LB=L
        NAL1=NLGN1(L)
        NDD(L)=NDA
        IUP(L)=L
        IDN(L)=L
        T1=DPT(L)
        T2=DPT(L+1)
        W=OMG(L)
        IF(NPLK1.GT.0) THEN
          DO 7 I=1,NPLK1
    7     CPL(I)=2*PI*(1-W)*CPLK(I,L)
        ENDIF
CC SCATTERING MEDIA
CC PT, PR
        CALL EQ12(GBUF,G,NAL1,LB,KLGN1)
        CALL PHAS2(M1,NAL1,NDA,NDA,KNDM,KNDM,KNDM,PT,PR
     &     ,GBUF,PLA,PLA)
CC PT0, PR0
        CALL PHAS2(M1,NAL1,NDA,NA0,KNDM,KNDM,KNA0,PT0,PR0
     &     ,GBUF,PLA,PL0)
CC EIGENVALUE PROBLEM
        CALL HOMOG2(M,T1,T2,W,NDA,AMUA,WMM,NA0,AM0
     &      ,PR,PT,PR0,PT0,FSOL,NPLK1,CPL,R,T,ER,ET,ZEIG
     &      ,Q,QI,C11,C22,VP,VM,DP,DM,ERR)
        IF(ERR.NE.' ') RETURN
        CALL EQ32(RE ,R  ,NDA,NDA,LB,KNDM,KNDM,KNLTM,KNDM,KNDM)
        CALL EQ32(TE ,T  ,NDA,NDA,LB,KNDM,KNDM,KNLTM,KNDM,KNDM)
        CALL EQ32(SER,ER ,NDA,NA0,LB,KNDM,KNA0,KNLNM,KNDM,KNA0)
        CALL EQ32(SET,ET ,NDA,NA0,LB,KNDM,KNA0,KNLNM,KNDM,KNA0)
C STORE EXTRA FOR INTERNAL FIELD AT ARBITRARY DEPTH.
        IF(INDT.NE.2 .OR. INDA.NE.2) THEN
          CALL EQ32(QE ,Q  ,NDA,NDA,LB,KNDM,KNDM,KNLN,KNDM,KNDM)
          CALL EQ32(QIE,QI ,NDA,NDA,LB,KNDM,KNDM,KNLN,KNDM,KNDM)
          CALL EQ32(C1E,C11,NDA,NDA,LB,KNDM,KNDM,KNLN,KNDM,KNDM)
          CALL EQ32(C2E,C22,NDA,NDA,LB,KNDM,KNDM,KNLN,KNDM,KNDM)
          CALL EQ32(VPE,VP ,NDA,NA0,LB,KNDM,KNA0,KNLN,KNDM,KNA0)
          CALL EQ32(VME,VM ,NDA,NA0,LB,KNDM,KNA0,KNLN,KNDM,KNA0)
          CALL EQ21(ZEE,ZEIG,NDA,LB,KNDM)
          IF(NPLK1.GT.0) THEN
            CALL EQ32(DPE,DP,NDA,NPLK1,LB,KNDM,KPLK1,KNLN,KNDM,KPLK1)
            CALL EQ32(DME,DM,NDA,NPLK1,LB,KNDM,KPLK1,KNLN,KNDM,KPLK1)
          ENDIF
        ENDIF
    6 CONTINUE
C LAMBERT SURFACE
      NLN1=NLN+1
      IF(INDG.LT.0) THEN
        NLT=NLN
       ELSE
        NLT=NLN1
        NDD(NLT)=NDA
        IUP(NLT)=NLT
        IDN(NLT)=NLT
        T1=DPT(NLT)
        IF(INDG.LE.0)THEN
          CALL GRNDL3(FSOL,GALB,BGND,T1,M,NDA,AMUA,WA,NA0,AM0
     &       ,R,T,ER,ET)
        ELSE
C 2001.04.05 SED Add DDLW for Water Leaving Radiance
          CALL GRNDO2(INDG,FSOL,GALB,BGND,T1,M,NDA,WMP,NA0,AM0
     &     ,SCR,SCI,R,T,ER,ERT,ET,AMUA,WA
     &     ,SBRF,SZA,EZA,AZANG,IUCD,IUCR,IUCSR,DDLW,
     &     R0B,KB,THB,ICANO,INITR,IBRF,XLAND,XOCEAN,NW,ir,RRS,SRS,
     &     IRFS)
cs        write(6,*)'IN FTRN'
cs       do i1=1,20
cs       write(6,*)'R',(R(i1,i2),i2=1,40)
cs       enddo
C 00.03.15 BY SED: GRNDO1->GRNDO2 brdf are added.
C          CALL GRNDO2(INDG,FSOL,GALB,BGND,T1,M,NDA,WMP,NA0,AM0
C     &     ,SCR,SCI,R,T,ER,ERT,ET,AMUA,WA
C     &     ,SBRF,SZA,EZA,AZANG,IUCD,IUCR,IUCSR)
C          CALL GRNDO1(INDG,FSOL,GALB,BGND,T1,M,NDA,WMP,NA0,AM0
C     &     ,SCR,SCI,R,T,ER,ERT,ET,AMUA,WA)
        ENDIF
        CALL EQ32(RE , R,NDA,NDA,NLT,KNDM,KNDM,KNLTM,KNDM,KNDM)
        CALL EQ32(TE , T,NDA,NDA,NLT,KNDM,KNDM,KNLTM,KNDM,KNDM)
        CALL EQ32(SER,ER,NDA,NA0,NLT,KNDM,KNA0,KNLNM,KNDM,KNA0)
        CALL EQ32(SET,ET,NDA,NA0,NLT,KNDM,KNA0,KNLNM,KNDM,KNA0)
      ENDIF
      NLT1=NLT+1
      NDD(NLT1)=NDA
C ADDING OF THE SUBLAYERS.
      CALL TRN1(NLT,NDD,NA0,IUP,IDN,RE,TE,SER,SET,RUP,RDN,ERR)
      IF(ERR.NE.' ') RETURN
      IF(INDT.EQ.2 .AND. INDA.NE.1) THEN
        IF(INDA.EQ.2) THEN
          DO 8 L=1,NLN1
          DO 8 J=1,NA0
          IF(IMTHD.LE.0 .OR. INDG.LE.0) THEN
            DO 9 I=1,NDA
    9       AI(I,J,L)=RUP(I,J,L)/WMP(I)
           else
            DO 59 I=1,NDA
C Don't change RUP itself, because it is used for flux calculation later
C 97.3.17 Debug By Takashi Nakajima
C            RUP1=RUP(I,J,L)-SER(I,J,NLN1)
C     &                     *EXP(-(DPT(NLN1)-DPT(L))/AMUA(I))
            RUP1=RUP(I,J,L)+(ERT(I,J)-SER(I,J,NLN1))
     &                     *EXP(-(DPT(NLN1)-DPT(L))/AMUA(I))
C
   59       AI(I,J,L)=RUP1/WMP(I)
          ENDIF
          DO 10 I=1,NDA
   10     AI(NA1U+1-I,J,L)=RDN(I,J,L)/WMP(I)
    8     CONTINUE
        ENDIF
        IF(M.EQ.0) THEN
          DO 11 L=1,NLN1
          DO 11 J=1,NA0
          FU1=0
          FD1=0
          EX1=-UTAU(L)/AM0(J)
          TRNS0=EXPFN(EX1)*FSOL
CC FOR SCALED INTENSITY
          DO 12 I=1,NDA
          FU1=FU1+WMP(I)*RUP(I,J,L)
   12     FD1=FD1+WMP(I)*RDN(I,J,L)
          FLXU(J,L)=FU1
          FLXD(J,L)=FD1+TRNS0*AM0(J)
   11     CONTINUE
        ENDIF
        RETURN
      ENDIF
C--- INTERPOLATION OF THE FIELD.
      IF(INDA.EQ.1) CALL PLGND(M1,MXLGN1,NA1,KNA1,AM1 ,PL1)
      DO 13 L=1,NLN
        LT=L
        LB=LT+1
        DPTH=DPT(LT)
        TAU=DPT(LB)-DPTH
        T1=DPT(LT)
        T2=DPT(LB)
        W=OMG(L)
C INTEGRAL CONSTANTS:  ALFA, BETA
        CALL CINGR(NDA,NA0,LT,AM0,TAU,RDN,RUP,VPE,VME,C1E,C2E
     &   ,NPLK1,DPE,DME,ALFA,BETA)
C FLUXES OR INTENSITY WHEN INDA=2
        IF(M.EQ.0 .OR. INDA.EQ.2) THEN
          DO 14 IT=1,NTAU
          L1=IABS(IITAU(IT))
          IF(L1.EQ.NLN1) L1=NLN
          IF(L1.NE.L) GOTO 14
          DTAU=UTAU(IT)-DPTH
          IF(IITAU(IT).GT.0) THEN
CC IN THE SUBLAYER
            CALL ADISC(M,L1,NDA,NA0,AM0,WMP,TAU,DTAU,ZEE,QE,QIE
     &      ,VPE,VME,ALFA,BETA,NPLK1,DPE,DME,UDN,UUP)
           ELSE
CC JUST INTERFACE
            LU=IABS(IITAU(IT))
            DO 15 J=1,NA0
            DO 15 I=1,NDA
            UDN(I,J)=RDN(I,J,LU)/WMP(I)
   15       UUP(I,J)=RUP(I,J,LU)/WMP(I)
          ENDIF
CC INTENSITY WHEN INDA=2
          IF(INDA.EQ.2) THEN
            DO 18 J=1,NA0
            IF(IMTHD.LE.0 .OR. INDG.LE.0) THEN
              DO 16 I=1,NDA
   16         AI(I,J,IT)=UUP(I,J)
             ELSE
              DO 56 I=1,NDA
C Don't change UUP itself, because it is used for flux calculation later
C 97.3.17 Debug By Takashi Nakajima
C   56         AI(I,J,IT)=UUP(I,J)-SER(I,J,NLN1)/WMP(I)
C     &                  *EXP(-(DPT(NLN1)-UTAU(IT))/AMUA(I))
   56         AI(I,J,IT)=UUP(I,J)+(ERT(I,J)-SER(I,J,NLN1)/WMP(I))
     &                  *EXP(-(DPT(NLN1)-UTAU(IT))/AMUA(I))
C
            ENDIF
            DO 17 I=1,NDA
   17       AI(NA1U+1-I,J,IT)=UDN(I,J)
   18       CONTINUE
          ENDIF
CC FLUX
          IF(M.EQ.0) THEN
            DO 19 J=1,NA0
            FU1=0
            FD1=0
            EX1=-UTAU(IT)/AM0(J)
            TRNS0=EXPFN(EX1)*FSOL
            DO 20 I=1,NDA
            FU1=FU1+AMUA(I)*WA(I)*UUP(I,J)
   20       FD1=FD1+AMUA(I)*WA(I)*UDN(I,J)
            FLXU(J,IT)=FU1
   19       FLXD(J,IT)=FD1+TRNS0*AM0(J)
          ENDIF
   14     CONTINUE
        ENDIF
C--- INTENSITIES IN THE USER DEFINED DIRECTIONS (WITHOUT CONTRIBUTION
C     FROM THE INTERFACES).
C PHASE FUNCTIONS FOR ANGULAR INTERPOLATION
        IF(INDA.EQ.1) THEN
          NAL1=NLGN1(L)
          CALL EQ12(GBUF,G,NAL1,LT,KLGN1)
C PT10, PR10
          CALL PHAS2(M1,NAL1,NA1,NA0,KNA1,KNA1,KNA0,PT10,PR10
     &     ,GBUF,PL1,PL0)
C PT1, PR1
          CALL PHAS2(M1,NAL1,NA1,NDA,KNA1,KNA1,KNDM,PT1,PR1
     &     ,GBUF,PL1,PLA)

C 2002.05 PT
          INITA=1
C

          DO 21 IU=1,NA1U
C AT INTERFACE.
          I=IIAM1(IU)
          IF(AM1U(IU).GT.0.0) THEN
CC DOWNWARD INCREMENT AT BOTTOM OF SUBLAYER (STAMNES INTEGRATION)
C 2002.05 PT and change function name
            CALL NAINT(M,LT,NDA,NA0,I,FSOL,AM1U(IU),AM0,W,T2,T1,T2
     &       ,ZEE,QE,QIE,VPE,VME,PT10,PR10,PT1,PR1,ALFA,BETA
     &       ,NPLK1,DPE,DME,CPLK,AIB,INITA)
C            CALL AINT(M,LT,NDA,NA0,I,FSOL,AM1U(IU),AM0,W,T2,T1,T2
C     &       ,ZEE,QE,QIE,VPE,VME,PT10,PR10,PT1,PR1,ALFA,BETA
C     &       ,NPLK1,DPE,DME,CPLK,AIB)
C
            DO 22 J=1,NA0
   22       AII(IU,J,LB)=AIB(J)
           ELSE
CC UPWARD INCREMENT AT TOP OF SUBLAYER (STAMNES INTEGRATION)
C 2002.05 PT and change function name
            CALL NAINT(M,LT,NDA,NA0,I,FSOL,AM1U(IU),AM0,W,T1,T1,T2
     &       ,ZEE,QE,QIE,VPE,VME,PT10,PR10,PT1,PR1,ALFA,BETA
     &       ,NPLK1,DPE,DME,CPLK,AIB,INITA)
C            CALL AINT(M,LT,NDA,NA0,I,FSOL,AM1U(IU),AM0,W,T1,T1,T2
C     &       ,ZEE,QE,QIE,VPE,VME,PT10,PR10,PT1,PR1,ALFA,BETA
C     &       ,NPLK1,DPE,DME,CPLK,AIB)
C
            DO 23 J=1,NA0
   23       AII(IU,J,L)=AIB(J)
          ENDIF
C AT USER DEFINED DEPTH
          DO 24 IT=1,NTAU
          L1=IITAU(IT)
CC  WE DO NOT CALCULATE THE FIELD AT THIS STAGE IF THE USER DEFINED
CC     DEPTH IS EXACTLY ON THE INTERFACES (L1.LT.0).
          IF(L1.EQ.L) THEN
C 2002.05 PT and change function name
            CALL NAINT(M,LT,NDA,NA0,I,FSOL,AM1U(IU),AM0,W,UTAU(IT),T1
     &      ,T2,ZEE,QE,QIE,VPE,VME,PT10,PR10,PT1,PR1,ALFA,BETA
     &      ,NPLK1,DPE,DME,CPLK,AIB,INITA)
C            CALL AINT(M,LT,NDA,NA0,I,FSOL,AM1U(IU),AM0,W,UTAU(IT),T1,T2
C     &      ,ZEE,QE,QIE,VPE,VME,PT10,PR10,PT1,PR1,ALFA,BETA
C     &      ,NPLK1,DPE,DME,CPLK,AIB)
C
            DO 25 J=1,NA0
   25       AI(IU,J,IT)=AIB(J)
          ENDIF
   24     CONTINUE
   21     CONTINUE
        ENDIF
   13 CONTINUE
      IF(INDA.LE.0 .OR. INDA.EQ.2) RETURN
C--- ADDING INTENSITIES (INDA=1)
      DO 26 IU=1,NA1U
      I=IIAM1(IU)
      IF(AM1U(IU).GT.0.0) THEN
CC DOWNWARD
        DO 27 J=1,NA0
   27   AIB(J)=0
        DO 28 L=1,NLN1
        IF(L.GT.1) THEN
           EX1=-(DPT(L)-DPT(L-1))/AM1(I)
           EX1=EXPFN(EX1)
          DO 29 J=1,NA0
   29     AIB(J)=AIB(J)*EX1+AII(IU,J,L)
        ENDIF
        DO 30 IT=1,NTAU
        LU=IITAU(IT)
        IF(IABS(LU).EQ.L) THEN
          IF(LU.LT.0) THEN
            DO 31 J=1,NA0
   31       AI(IU,J,IT)=AIB(J)
           ELSE
            DTAU=UTAU(IT)-DPT(L)
            EX1=-DTAU/AM1(I)
            EX1=EXPFN(EX1)
            DO 32 J=1,NA0
   32       AI(IU,J,IT)=AI(IU,J,IT)+AIB(J)*EX1
          ENDIF
        ENDIF
   30   CONTINUE
   28   CONTINUE
CC UPWARD ADDING
       ELSE
        DO 33 J=1,NA0
C 93.5.4
        IF(INDG.LT.0) then
          AIB(J)=0
         ELSE
          IF(INDG.EQ.0) THEN
            AIB(J)=RUP(1,J,NLN1)/WMP(1)
           else
C Interpolation of reflected intensities subtracted single scattering
            IF(IMTHD.LE.0) then
              DO 51 K=1,NDA
              K1=NDA-K+1
C 94.5.7 Bug  YY(K1)=RUP(K,J,NLN1)
C Corrected
              YY(K1)=RUP(K,J,NLN1)/WMP(K)
   51         XX(K1)=AMUA(K)
             else
              DO 50 K=1,NDA
              K1=NDA-K+1
C 94.5.7 Bug  YY(K1)=RUP(K,J,NLN1)-SER(K,J,NLN1)
C Corrected
C 97.3.17 Debug By Takashi Nakajima
C              YY(K1)=(RUP(K,J,NLN1)-SER(K,J,NLN1))/WMP(K)
              YY(K1)=(RUP(K,J,NLN1)-SER(K,J,NLN1))/WMP(K)
     &        + ERT(K,J)/WMP(K)
   50         XX(K1)=AMUA(K)
            endif
            CALL CSPL1(NDA,XX,YY,A,B,C,D)
            X1=AM1(I)
C 94.5.7 Bug  AIB(J)=CSPLI(X1,NDA,XX,A,B,C,D)/WMP(1)
C Corrected
            AIB(J)=CSPLI(X1,NDA,XX,A,B,C,D)
          ENDIF
        ENDIF
C 93.5.4 end
   33   CONTINUE
        DO 34 L=NLN1,1,-1
        IF(L.LE.NLN) THEN
           EX1=-(DPT(L+1)-DPT(L))/AM1(I)
           EX1=EXPFN(EX1)
          DO 35 J=1,NA0
   35     AIB(J)=AIB(J)*EX1+AII(IU,J,L)
        ENDIF
        DO 36 IT=1,NTAU
        LU=IITAU(IT)
        IF(LU.LT.0) THEN
          IF(IABS(LU).EQ.L) THEN
            DO 37 J=1,NA0
   37       AI(IU,J,IT)=AIB(J)
          ENDIF
         ELSE
          IF(LU.EQ.L-1) THEN
            DTAU=DPT(L)-UTAU(IT)
            EX1=-DTAU/AM1(I)
            EX1=EXPFN(EX1)
            DO 38 J=1,NA0
   38       AI(IU,J,IT)=AI(IU,J,IT)+AIB(J)*EX1
          ENDIF
        ENDIF
   36   CONTINUE
   34   CONTINUE
      ENDIF
   26 CONTINUE
      RETURN
      END
C 2002.05 PT
      SUBROUTINE INTCR1(IMTHD,AM0,AM1U,CS1,NTAU,UTAU,UTAUT
     &,NLN,THK,THKT,OMG,OMGT,PHS,PHST,FF,KLGN1,MXLGN1,NLGN1
     &,NLGT1,G,EPSP,NCHK1,COR,SGL2,PHSB,ERR)
C SIGLE AND SECOND SCATTERING CORRECTION
C ASSUME DELTA-M METHOD
C--- HISTORY
C 90. 1.28 CREATED
C 2002.05 Performance Tuned by SED
C--- INPUT
C IMTHD      I         1: MS, 2: TMS, 3: IMS
C AM0        R         COS(SOLAR ZENITH ANGLE)
C AM1U       R         COS(EMERGENT NADIR ANGLE)
C                     .LT.O: UPWARD,  .GT.0: DOWNWARD
C CS1        R         COS(SCATTERING ANGLE)
C NTAU       I         NUMBER OF USER DEFINED ANGLE
C UTAU     R(NTAU)     USER DEFINED OPTICAL DEPTH
C UTAUT    R(NTAU)     TRUNCATED USER DEFINED DEPTH
C NLN        I         NUMBER OF LAYER
C THK      R(NLN)      OPTICAL THICKNESS OF SUBLAYER
C THKT     R(NLN)      TRUNCATED THICKNESS
C OMG      R(NLN)      SINGLE SCATTERING ALEBEDO
C OMGT     R(NLN)      TRUNCATED SINGLE SCATTERING ALBEDO
C PHS      R(NLN)      PHASE FUNCTION  (INTEGRAL = 1 OVER UNIT SPHERE)
C PHST     R(NLN)      TRUNCATED PHASE FUNCTION (1 OVER UNIT SPHERE)
C FF       R(NLN)      TRUNCATION FRACTION
C KLGN1      I         FIRST ARGUMENT SIZE OF G
C MXLGN1     I         MAX(NLGN1) (ONLY FOR IMTHD=3)
C NLGN1    I(NLN)      MAX ORDER OF LEGENDER SERIES +1 IN EACH SUBLAYER
C                        (ONLY FOR IMTHD=3)
C NLGT1    I(NLN)      SAME AS -NLGN1- BUT FOR TRUNCATION
C G      R(KLGN1,NLN)  PHASE FUNCTION MOMENT (ONLY FOR IMTHD=3)
C EPSP       R         CONVERGENCE CRITERION OF LEGENDRE SERIS OF
C                        PHASE FUNCTION ** 2
C NCHK1      I         NUMBER OF CONSECTIVE CONVERGENCES BEFORE
C                        FINAL DECISION
C--- OUTPUT
C COR      R(NTAU)     AI= AI+COR*FSOL IS CORRECTED INTENSITY
C                        AT EACH USER
C ERR      C*64        ERROR INDICATER
C                       DEFINED DEPTH (UTAU)
C--- WORK
C SGL2     R(NTAU)
C PHSB     R(NLN)
C--- AREA FOR THIS ROUTINE
      PARAMETER (PI=3.141592654, RAD=PI/180.0)
C 2002.05 PT
      PARAMETER (KNLN=35)
C
      CHARACTER ERR*(*)
      DIMENSION UTAU(NTAU),UTAUT(NTAU),THK(NLN),THKT(NLN)
     &,OMG(NLN),OMGT(NLN),PHS(NLN),PHST(NLN),FF(NLN),NLGN1(NLN)
     &,NLGT1(NLN),G(KLGN1,NLN),COR(NTAU),SGL2(NTAU),PHSB(NLN)
C 2001.02 PT
      DIMENSION OMPH1(KNLN),OMPH2(KNLN)
C
      ERR=' '
C	WRITE(*,*)'UTAU=',UTAU !XXA
      DO 1 IT=1,NTAU
    1 COR(IT)=0
      IF(IMTHD.LE.0 .OR. IMTHD.GE.4) RETURN
C--- MS-METHOD IN REFERENCE-NT
      IF(IMTHD.EQ.1) THEN
C 2002.05 PT
        DO 2 L=1,NLN
          PHSB(L)=(1-FF(L))*PHST(L)
          OMPH1(L)=OMG(L)*PHS(L)
          OMPH2(L)=OMG(L)*PHSB(L)
    2   CONTINUE
        DO 3 IT=1,NTAU
          CALL SGLR2P(AM1U,AM0,NLN,THK,OMPH1,OMPH2,UTAU(IT),
     &                SGL1,SGL2(IT))
          COR(IT)=-SGL2(IT)+SGL1
    3   CONTINUE
        RETURN
CC SGL2 = U1WAVE OF EQ.(14)
C        DO 2 L=1,NLN
C    2   PHSB(L)=(1-FF(L))*PHST(L)
C        DO 3 IT=1,NTAU
C    3   SGL2(IT)=SGLR(AM1U,AM0,NLN,THK,OMG,PHSB,UTAU(IT))
CC CORRECTION BY MS-METHOD.  SEE EQ.(14)
C        DO 4 IT=1,NTAU
CCC SGL1 = U1 OF EQ.(14)
C        SGL1=SGLR(AM1U,AM0,NLN,THK,OMG,PHS,UTAU(IT))
C    4   COR(IT)=-SGL2(IT)+SGL1
C        RETURN
C
      ENDIF
C--- TMS AND IMS METHODS
C 2002.05 PT
      DO 6 L=1,NLN
        PHSB(L)=PHS(L)/(1-FF(L))
        OMPH1(L)=OMGT(L)*PHSB(L)
        OMPH2(L)=OMGT(L)*PHST(L)
    6 CONTINUE
      DO 5 IT=1,NTAU
        CALL SGLR2P(AM1U,AM0,NLN,THKT,OMPH1,OMPH2,UTAUT(IT),
     &              SGL1,SGL2(IT))
        COR(IT)=-SGL2(IT)+SGL1
    5 CONTINUE
CC  SGL2 = U1* OF EQ.(15)
C      DO 5 IT=1,NTAU
C    5 SGL2(IT)=SGLR(AM1U,AM0,NLN,THKT,OMGT,PHST,UTAUT(IT))
C      DO 6 L=1,NLN
C    6 PHSB(L)=PHS(L)/(1-FF(L))
CC CORRECTION OF INTENSITY BY TMS-METHOD.  SEE EQ.(15)
C      DO 7 IT=1,NTAU
CCC  SGL1 = U1WAVE* OF EQ.(15)
C      SGL1=SGLR(AM1U,AM0,NLN,THKT,OMGT,PHSB,UTAUT(IT))
C    7 COR(IT)=-SGL2(IT)+SGL1
C
      IF(IMTHD.EQ.2 .OR. AM1U.LE.0.0) RETURN
C--- SECONDARY SCATTERING CORRECTION FOR IMS-METHOD
      DO 8 IT=1,NTAU
        UTAUS=UTAU(IT)
C GETTING MEAN OPTICAL CONSTANTS ABOVE THE USER DEFINED LEVEL-UTAUS
        EH=0
        SH=0
        SHH=0
        PHSPK=0
        DPT2=0
        DO 9 L=1,NLN
          DPT1=DPT2
          DPT2=DPT1+THK(L)
          IF(UTAUS.LE.DPT1) GOTO 10
          IF(UTAUS.LT.DPT2) THEN
            TAU=UTAUS-DPT1
           ELSE
            TAU=THK(L)
          ENDIF
          EH= EH     +       TAU
          SH= SH     +OMG(L)*TAU
          SHH=SHH    +OMG(L)*TAU*FF(L)
    9     PHSPK=PHSPK+OMG(L)*TAU*(PHS(L)-(1-FF(L))*PHST(L))
   10   IF(ABS(EH).LE.0) GOTO 8
C WH: MEAN SINGLE SCATTERING ALBEDO
        WH=SH/EH
C FH: MEAN TRUNCATED FRACTION
CXXA==============================================
c	WRITE(*,*)'SH=',SH !XXA
	  if(sh.eq.0)then
		FH=0
		else
	    FH=SHH/SH
	  endif
CXXA==============================================
        IF(FH.LE.0.0) GOTO 8
C PHSPK: MEAN TRUNCETED PEAK OF PHASE FUNCTION
        PHSPK=PHSPK/SHH
C AM3: VARIABLE APPEARING IN EQ.(23)
        AM3=AM0/(1-FH*WH)
C LEGENDRE SUM FOR EQ.(23)
C  WE TRUNCATE THE SERIES WHEN ABS(GPK**2) BECOMES SMALLER THAN -EPSP-
C   FOR SUCCESSIVE THREE TIMES.
        PHSPK2=0
        INIT=1
        ICHK=0
        DO 11 K1=1,MXLGN1
CC PHSPK: 2PHAT-PHAT2 OF EQ.(23)
CC  MEAN VALUE FOR THE LAYER ABOVE THE USER-DEFINED LEVEL
          GPK=0
          DPT2=0
          DO 12 L=1,NLN
          DPT1=DPT2
          DPT2=DPT1+THK(L)
          IF(K1.LE.NLGN1(L) .AND. OMG(L).GT.0) THEN
            IF(UTAUS.LE.DPT1) GOTO 13
            IF(UTAUS.LT.DPT2) THEN
              TAU=UTAUS-DPT1
             ELSE
              TAU=THK(L)
            ENDIF
            IF(K1.LE.NLGT1(L)) THEN
              GP=FF(L)
             ELSE
              GP=G(K1,L)
            ENDIF
            GPK=GPK+GP*OMG(L)*TAU
          ENDIF
   12     CONTINUE
   13     GPK=GPK/SHH
          GPK2=GPK**2
          IF(GPK2.LE.EPSP) THEN
            ICHK=ICHK+1
            IF(ICHK.GE.NCHK1) GOTO 14
          ENDIF
   11     PHSPK2=PHSPK2+(2*K1-1)*GPK2*PLGD(INIT,CS1)
   14   PHSPK2=PHSPK2/4/PI
CC CORRECTION BY UU = UU + UHAT IN IMS-METHOD
CC   SGL3: UHAT OF EQ.(23)
C	write(*,*)FH,WH
        SGL3=(FH*WH)**2/(1-FH*WH)*(2*PHSPK-PHSPK2)
     &    * HF(UTAUS, AM1U, AM3, AM3)
        COR(IT)=COR(IT)-SGL3
C	write(*,*)'sgl3' !xxa
    8 CONTINUE
      RETURN
      END
C 2002.05 PT
      SUBROUTINE RTRN22(INDG,INDA,INDT,INDP,IMTHD,NDA,NA1U,AM1U,NA0,AM0
     &,NFI,FI,NLN,THK,OMG,NLGN1,G,NANG,ANG,PHSF,EPSP,EPSU,GALB,FSOL
     &,NPLK1,CPLK,BGND,NTAU,UTAU,SCR,SCI,FLXD,FLXU,AI,ERR
     &,IUSN,IUCD,IUCO,IUCR,IUCSR,DDLW
     &,IWS,IK,R0B,KB,THB,ICANO,IBRF,ITOFF,XLAND,XOCEAN,NW,IRFS)
C 2001.04.05 SED : ADD DDLW for Water Leaving Radiance
C      SUBROUTINE RTRN22(INDG,INDA,INDT,INDP,IMTHD,NDA,NA1U,AM1U,NA0,AM0
C     &,NFI,FI,NLN,THK,OMG,NLGN1,G,NANG,ANG,PHSF,EPSP,EPSU,GALB,FSOL
C     &,NPLK1,CPLK,BGND,NTAU,UTAU,SCR,SCI,FLXD,FLXU,AI,ERR
C     &,IUSN,IUCD,IUCO,IUCR,IUCSR,DDLW)
C 
C 00.03.14 BY SED : corrected single scattering from canopy&snow BRDF
C                   are added.      
C      SUBROUTINE RTRN22(INDG,INDA,INDT,INDP,IMTHD,NDA,NA1U,AM1U,NA0,AM0
C     &,NFI,FI,NLN,THK,OMG,NLGN1,G,NANG,ANG,PHSF,EPSP,EPSU,GALB,FSOL
C     &,NPLK1,CPLK,BGND,NTAU,UTAU,SCR,SCI,FLXD,FLXU,AI,ERR
C     &,IUSN,IUCD,IUCO,IUCR,IUCSR)
C      SUBROUTINE RTRN21(INDG,INDA,INDT,INDP,IMTHD,NDA,NA1U,AM1U,NA0,AM0
C     &,NFI,FI,NLN,THK,OMG,NLGN1,G,NANG,ANG,PHSF,EPSP,EPSU,GALB,FSOL
C     &,NPLK1,CPLK,BGND,NTAU,UTAU,SCR,SCI,FLXD,FLXU,AI,ERR)
C
C SOLVE THE RADIATIVE TRANSFER IN ATMOSPHERE SYSTEM.
C  -USE FTRN21-
C Complex refractive index for ocean surface
C Useful for microwave region
C BY TERUYUKI NAKAJIMA
C 94.4.4 observation of FTRN21:
C Insufficeient correction of solar single scattering from ocean surface.
C GRND0 should be corrected in the future.
C This version of GRNDO calculates R(I,J) and SR(I,J0) by integrating
C the reflected intensity at ocean surface over the mu-interval
C (mu(I-1/2), mu(I+1/2)).  After solving the transfer with this
C reflection and source matrices of ocean surface, we subtracted
C the singly scattered intensity with SR(I,J0).  After that,
C we add the true single reflected intensity.
C However, this trick will have some error if mu(I)<>mu(J0),
C since the strongly peaked reflected intensity in the direction
C of -mu(J0).  This is distributed among the light along the
C quadrature stream -mu(I).  In more accurate approximation,
C we should treat the peaked reflected intensity as a reflected
C direct radiation not distributed amond the quadrature streams.
C
C 94.9.2  The symmetric operation for R(ocean) in 'OCEANRF'
C can introduce a difference of order about 0.3 % from a version
C without such symmetric operation.
C
C--- HISTORY
C 89.11. 7 CREATED
C 90.11.22 IF(INDP.LE.0 .AND. (INDA.GE.1 .AND. IMTHD.GE.1)) INDPL=1 ->
C                  EQ
C          CHANGE CODES BETWEEN -DO 4 - AND -DO 8 -;
C          IF(ABS(CS1).GT.1.0) CS1=SIGN(1.0,CS1); CHANGE LOOP-13,14
C          ADDING LOOP-47.
C 93. 4. 5 NSF meaning changed, Set NSF=0 after loop-16
C     5. 4 Naming RTRN2 from Higurashi's RTRN with use of FTRN21
C           and corrected single scattering for ocean surface reflection
C    10.19 OK comparing with Hasumi's program.
C 94. 9.27 debug IF(AM1U(IU).LT.0) then A3= ...
C 95. 6. 2 Gerated from RTRN2 with SCR and SCI
C           Useful for microwave region
C 98.1.7
C   u10 > 0.01 for RSTR and PSTR.  u10 < 0.01 will have significant
C   error in radiances due to angular integration errors for surface
C   reflection.  Recommend that u10 is set to be 0.01 for flat surface
C   case
C 00.03.14 corrected single scattering from canopy&snow BRDF are added  BY SED
C 2002.05  Performance tuning by SED
C--- INPUT
C INDG       I      -1: No ground surface
C                    0: Lambert surface
C                    1: Ocean surface initialization
C                    2: Ocean surface with no initialization
C                    When INDG>0 and IMTHD>0 then single scattering correction
C                      for ocean surface reflection
C INDA       I       0: FLUX ONLY.
C                    1: INTENSITY USING -AM1U-.
C                    2: INTENSITY AT GAUSSIAN QUADRATURE POINTS.
C                       -NA1U- AND -AM1U- ARE SET AUTOMATICALLY AS
C                       2*NDA AND (-AMUA, +AMUA).
C INDT       I       1: INTENSITY/FLUX AT USER DEFINED DEPTH -UTAU-.
C                    2: INTENSITY/FLUX AT SUBLAYER INTERFACES.
C                       -NTAU- AND -UTAU- ARE SET AUTOMATICAALY AS
C                       -NLN1- AND -DPT-.
C INDP       I      -1: GIVE -G- AND USE -G- FOR INTENSITY INTERPOLATION
C                       FROM GAUSSIAN POINTS TO USER POINTS.
C                    0: GIVE -G- AND CONSTRUCT PHASE FUNCTION FOR
C                       INTENSITY INTERPOLATION.
C                    1: GIVE PHASE FUNCTION -PHSF-.
C IMTHD      I      -1: NT,  0: DMS-METHOD  FOR INTENSITY/FLUX
C                    1: MS,  2:TMS,  3:IMS-METHOD FOR INTENSITY.
C                    When INDG>0 and IMTHD>0 then single scattering correction
C                      for ocean surface reflection
C NDA        I       NUNBER OF NADIR-QUADRATURE ANGLES IN THE
C                    HEMISPHERE.
C NA1U       I       NUMBER OF EMERGENT NADIR ANGLES IN SPHERE.
C AM1U    R(KNA1U)   CONSINE OF THE EMERGENT NADIR ANGLES.
C                      - FOR UPWARD, + FOR DOWNWARD.
C NA0        I       NUMBER OF SOLAR INCIDENCES.
C AM0     R(NA0)     CONSINE OF SOLAR ZENITH ANGLES .GT. 0.
C NFI        I       NUMBER OF AZIMUTHAL ANGLES.
C FI     R(KNFI)     AZIMUTHAL ANGLES IN DEGREES.
C NLN        I       NUMBER OF ATMOSPHERIC SUBLAYERS.
C THK     R(KNLN)    OPTICAL THICKNESS OF SUBLAYERS FROM TOP TO BOTTOM.
C OMG     R(KNLN)    SINGLE SCATTERING ALBEDO.
C NLGN1   I(KNLN)    MAXIMUM ORDER OF MOMENTS + 1.
C                      GIVE WHEN -INDP- .LE. 0.
C                      GIVE WHEN IMTHD=3 REGARDLESS OF -INDP-.
C                      OTHERWISE, A VALUE .LE. 2*NDA+1 IS GIVEN
C                      AUTOMATICALLY BY THE ROUTINE.
C G   R(KLGN1,KNLN)  LEGENDRE MOMENTS OF PHASE FUNCTION.
C                      GIVE WHEN INDP .LE. 0.
C NANG       I       NUMBER OF SCATTERING ANGLES FOR PHASE FUNCTIONS.
C                      GIVE WHEN INDP=1.
C                      GIVE WHEN INDP=0 .AND. (INDA.GE.1) .AND.
C                                             (IMTHD.GE.1).
C ANG     R(KNANG)   SCATTERING ANGLES IN DEGREES.
C                      GIVE WHEN INDP=1.
C                      GIVE WHEN INDP=0 .AND. (INDA.GE.1) .AND.
C                                             (IMTHD.GE.1).
C                      ANGLES SHOULD HAVE ENOUGH RESOLUTION FOR LEGENDRE
C                      EXPANSION (INDP=1) AND INTERPOLATION BY CUBIC
C                      POLYNOMIAL.
C PHSF    R(KNANG,   PHASE FUNCTION. GIVE WHEN INDP=1.
C EPSP       R       TRUNCATION CRITERION OF PHASE FUNCTION MOMENTS.
C EPSU       R       CONVERGENCE CRITERION OF INTENSITY (INDA.GT.0)
C GALB       R       Ground albedo if INDG=0
C                    U10 (m/sec)   if INDG>0; u10>0.01
C FSOL       R       SOLAR IRRADIANCE AT THE SYSTEM TOP.
C NPLK1      I       NUMBER OF ORDER TO APPROXIMATE PLANK + 1.
C                      IF 0 THEN NO THERMAL.
C CPLK R(KPLK1,KNLN) Plank function
C                      = SUM(K1=1,NPLK1) CPLK(K1,L) * TAU**(K1-1)
C                      TAU IS THE OPTICAL DEPTH MEASURED FROM THE TOP
C                      OF THE SUBLAYER (SAME UNIT AS FSOL).
C BGND       R       (1-GALB)*B when INDG=0
C                    B          when INDG>0
C                    where B is Plank function
C                      (SAME UNIT AS FSOL).
C NTAU       I       NUMBER OF USER DEFINED OPTICAL DEPTHS.
C UTAU    R(KNTAU)   OPTICAL DEPTHS WHERE THE FIELD IS CALCULTED.
C                      TOP TO BOTTOM.
C SCR       R    Relative refractive index of the media
C               About 1.33 for atmosphere to ocean incidence,
C               and 1/1.33 for ocean to atmosphere incidence.
C SCI       R    Relative refractive index for imaginary part
C               M = CR + I*CI

C--- OUTPUT
C INDG               if 1 then 2
C NA1U, AM1U         SAME AS 2*NDA, (-AUMA, +AMUA) WHEN INDA=2.
C NLGN1, G           NORMALIZED BY G(1), AND CUT OFF BY THE CRITERION
C                     -EPSP-.
C NTAU, UTAU         SAME AS NLN+1 AND -DPT- WHEN INDT=1.
C PHSF               NORMALIZED PHASE FUNCTION (1 OVER UNIT SPHERE).
C                    RECONSTRUCTED WHEN (INDA.GE.1) .AND. (IMTHD.GE.1)
C                    .AND. (INDP=0).
C FLXD    R(KNA0,    DOWNWARD FLUX AT -UTAU-.
C           KNTAU)
C FLXU               UPWARD   FLUX AT -UTAU-.
C AI      R(KNA1U,   I(MU1U(I), MU0(J), FI(K), UTAU(IT))
C           KNA0,    INTENSITY AT -UTAU- (WATT/M2/MICRON).
C           KNFI,
C           KNTAU)
C ERR      C*64      ERROR INDICATER. IF " " THEN NORMAL END.
C--- CORRESPONDENCE BETWEEN VARIABLES AND PARAMETERS
C KNA1U       NA1U
C KNA1        NA1
C KNA0        NA0
C KNDM        NDA
C KNFI        NFI
C KNLN        NLN
C KNLN1       KNLN+1
C KNTAU       NTAU
C KLGN1       MXLGN1 = MAX(NLGN1)
C              THIS SHOULD BE VERY LARGE WHEN IMS-METHD IS USED
C              SO AS TO APPROXIMATE P**2, OR WHEN INDP=1 IS ASSIGNED
C              TO RECONSTRUCT THE ORIGINAL PHASE FUNCTION FROM -G-.
C KLGT1       MXLGT1 = SAME AS MXLGN1 BUT FOR THAT OF TRUNCATED
C              VARIABLES
C KPLK1       NPLK1
C$ENDI
C PARAMETERS
      PARAMETER (KNA1U =100)
      PARAMETER (KNA1  =100)
      PARAMETER (KNA0  =2)
      PARAMETER (KNDM  =16)
      PARAMETER (KNFI  =200)
      PARAMETER (KLGN1 =400)
      PARAMETER (KNLN  =35)
      PARAMETER (KNTAU =2)
      PARAMETER (KPLK1 =2)
      PARAMETER (KNANG =250)
      PARAMETER (NCHK1 =3)
      PARAMETER (NCHK2 =3)
C
      PARAMETER (KNLN1=KNLN+1,KLGT1=2*KNDM)
      PARAMETER (PI=3.141592654,RAD=PI/180.0)
C AREAS FOR THIS ROUTINE
      CHARACTER ERR*64
      DIMENSION AM1U(KNA1U),AM0(KNA0),FI(KNFI),THK(KNLN),OMG(KNLN)
     &,NLGN1(KNLN),G(KLGN1,KNLN),ANG(KNANG),PHSF(KNANG,KNLN)
     &,CPLK(KPLK1,KNLN),UTAU(KNTAU),FLXD(KNA0,KNTAU)
     &,FLXU(KNA0,KNTAU),AI(KNA1U,KNA0,KNFI,KNTAU)
C AREAS FOR FTRN21
      DIMENSION AMUA(KNDM),WA(KNDM),GT(KLGT1,KNLN),NLGT1(KNLN)
     &,CPLKT(KPLK1,KNLN),OMGT(KNLN),THKT(KNLN),UTAUT(KNTAU)
     &,AIF(KNA1U,KNA0,KNTAU)
C WORKING AREAS
      DIMENSION CS(KNANG),YY(KNANG),GG(KLGN1),COSM(KNFI)
     &,PHS(KNLN),FF(KNLN),DPT(KNLN1),DPTT(KNLN1)
     &,PHST(KNLN),SGL2(KNTAU),PHSB(KNLN),COR(KNTAU)
       DIMENSION IM1U(KNA1U),JM0(KNA0),BM1U(KNA1U),BM0(KNA0)
     &,EU1(KNA1U),EU0(KNA0),IICHK0(KNA0),IICHK1(KNA1U)
C 00.03.14 Canopy BRDF & Snow BRDF
      DIMENSION CBRF3(KNA0,KNA1U,KNFI)
      DIMENSION SBRF(20,20,40),SZA(20),EZA(20),AZANG(40)
      SAVE CBRF3, SBRF, SZA, EZA, AZANG
C*****FOR CAPONY BRDF******************************************
      real R0B,KB,THB
      integer ICANO
C*****FOR CAPONY BRDF****************************************** 
C 2001.04.05 SED START
      DIMENSION DDLW(KNA0)
C 2001.04.05 END
C 2002.05 PT
C supposition : KLGN1 > KLGT1
      DIMENSION CS11(KNANG),CS12(KNFI,KNA1U,KNA0),ANG12(KNFI,KNA1U,KNA0)
     &, PLGD1(KLGN1,KNANG),PLGD2(KLGN1,KNFI,KNA1U,KNA0)
     &, PHS1(KNLN,KNFI,KNA1U,KNA0)
     &, SFIS(KNFI),SFI(KNFI),SS(KNFI),A4(KNTAU,KNFI,KNA0,KNA1U)
C
       save INIT0
       data INIT0/1/
C 2002.05 PT
      SAVE CS11,CS12,PLGD1,PLGD2,ANG12,SFIS,SFI,PHS1
C
C 00.03.14 Modifeid by SED
C       if(INIT0.GT.0) then
C         INIT0=0
C         IF(INDG.GT.0) INDG=1
C       endif
        INITR=1
        ir=ir+1
CS        write(6,*)
cs          write(6,*)'GALB',GALB
cs          write(6,*)'SCR',SCR
cs          write(6,*)'SCI',SCI
cs          write(6,*)'AM0',AM0
cs          write(6,*)'AM1U',AM1U
cs          write(6,*)'FI',FI*RAD
cs          write(6,*)'INITR',INITR
cs        write(6,*)
       If (INIT0.GT.0) THEN
cs           INIT0=0
C when ocean surface
           If((INDG.GT.0).AND.(INDG.LT.3)) INDG=1
C when canopy brdf surface
           IF(INDG.EQ.3)THEN
           CALL GETCN2(IUCD,IUCO,KNA0,KNA1U,KNFI
     &     ,NA0,NA1U,NFI,AM0,AM1U,FI,CBRF3,
     &      R0B,KB,THB,ICANO,IBRF,IRFS)
           ENDIF
cs       do i1=1,NA1U
cs       write(6,*)'CBRF3',(CBRF3(1,i1,i2),i2=1,NFI)
cs       enddo
C when snow brdf surface
           IF(INDG.EQ.4) CALL GETSN(IUSN,SBRF,SZA,EZA,AZANG)
       ENDIF
C
C CHECK AND SET OF VARIABLES
      CALL CHKRT(INDA,INDT,INDP,IMTHD,NDA,NA1U,AM1U,NA0,AM0
     &,NFI,FI,NLN,THK,OMG,NLGN1,G,NANG,ANG,PHSF,EPSP,EPSU,GALB,FSOL
     &,NPLK1,CPLK,BGND,NTAU,UTAU,AMUA,WA,DPT,MXLGN2,ERR)
      IF(ERR.NE.' ') RETURN
      NLN1=NLN+1
C FOURIER EXPANSION PARAMETERS
      NDA2=2*NDA
      NDA21=NDA2+1
      MXLGT1=NDA2
      IF(MXLGT1.GT.KLGT1) THEN
        ERR='-KLGT1- IS TOO SMALL'
        RETURN
      ENDIF
cs      write(6,*)'check1'
CC FLUX
      IF(INDA.LE.0) THEN
        IF(IMTHD.LT.0) THEN
          MXLGN1=MIN(NDA2,MXLGN2)
         ELSE
          MXLGN1=MIN(NDA21,MXLGN2)
        ENDIF
CC INTENSITY
       ELSE
        IF(INDP.LE.0) THEN
          MXLGN1=MXLGN2
         ELSE
          IF(IMTHD.LE.-1) THEN
            MXLGN1=NDA2
           ELSE
            IF(IMTHD.LE.2) THEN
              MXLGN1=NDA21
             ELSE
              MXLGN1=MXLGN2
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF(MXLGN1.GT.KLGN1) THEN
        ERR='-KLGN1- IS TOO SMALL'
        RETURN
      ENDIF
cs      write(6,*)'check2'
C INDICATER FOR RE-CALCULATING PHASE FUNCTION USING -G-
      INDPL=0
      IF(INDP.EQ.0 .AND. (INDA.GE.1 .AND. IMTHD.GE.1)) INDPL=1
C CLEAR VARIABLES
      IF(INDA.GE.1) THEN
        DO 1 IT=1,NTAU
        DO 1 J=1,NA0
        DO 1 K=1,NFI
        DO 1 I=1,NA1U
   1   AI(I,J,K,IT)=0
      ENDIF
C SET COS(SCATTERING ANGLE)
      IF((INDP.EQ.1) .OR. (INDPL.GE.1)) THEN
        DO 2 I=1,NANG
    2   CS(I)=COS(ANG(I)*RAD)
      ENDIF
C 2002.05 PT
C PLGD1
      IF (IWS .EQ. 1) THEN
        MXIT1=MXLGN1
        DO 50 I=1,NANG
          CS11(I)=CS(I)
          CS1=CS11(I)
          INIT=1
          DO 51 K1=1,MXIT1
            PLGD1(K1,I)=PLGD(INIT,CS1)
   51     CONTINUE
   50   CONTINUE
      ELSE
        IF (MXIT1 .LT. MXLGN1) THEN
          ERR='MXIT1<MXLGN1'
cs          write(6,*)ERR
          RETURN
        ENDIF
      ENDIF
cs      write(6,*)'check3'
C

C LOOP FOR SUBLAYERS
      DO 3 L=1,NLN
CC LEGENDRE EXPANSION OF THE PHASE FUNCTIONS
CC  WE TRUNCATE THE SERIES WHEN ABS(G) BECOMES SMALLER THAN -EPSP-
CC   FOR SUCCESSIVE THREE TIMES.
        IF(INDP.EQ.1) THEN
          DO 4 I=1,NANG
    4     YY(I)=PHSF(I,L)
          CALL LGNDF3(MXLGN1,NANG,CS,YY,GG)
cs          write(6,*)
cs          write(6,*)'GG',GG
cs          write(6,*)
          GG0=GG(1)
          ICHK=0
          DO 5 K1=1,MXLGN1
          G(K1,L)=GG(K1)/GG0
          IF(ABS(G(K1,L)).LE.EPSP) THEN
            ICHK=ICHK+1
            IF(ICHK.GE.NCHK1) GOTO 6
          ENDIF
    5     CONTINUE
          K1=MXLGN1
    6     NLGN1(L)=K1
C CS IS FROM 1 TO -1 FOR LGNDF3, SO THAT GG0<0 (90.11.22)
          GG0=ABS(GG0)
          DO 10 I=1,NANG
   10     PHSF(I,L)=PHSF(I,L)/GG0/4/PI
CC CONSTRUCTION OF PHASE FUNCTION FROM -G- WHEN INDP=0 AND IMTHD.GE.1
         ELSE
          GG0=G(1,L)
          ICHK=0
          K2=NLGN1(L)
          DO 9 K1=1,K2
          G(K1,L)=G(K1,L)/GG0
          IF(ABS(G(K1,L)).LE.EPSP) THEN
            ICHK=ICHK+1
            IF(ICHK.GE.NCHK1) GOTO 46
          ENDIF
    9     CONTINUE
          K1=MXLGN1
   46     NLGN1(L)=K1
          IF(INDPL.GE.1) THEN
C 2002.05 PT
C 1st PLGD
            DO 7 I=1,NANG
              CS1=CS11(I)
              SUM=0
              DO 8 K1=1,NLGN1(L)
                SUM=SUM+(2*K1-1)*G(K1,L)*PLGD1(K1,I)
    8         CONTINUE
              PHSF(I,L)=SUM/4/PI
    7       CONTINUE
C            DO 7 I=1,NANG
C            CS1=CS(I)
C            INIT=1
C            SUM=0
C            DO 8 K1=1,NLGN1(L)
C    8       SUM=SUM+(2*K1-1)*G(K1,L)*PLGD(INIT,CS1)
C    7       PHSF(I,L)=SUM/4/PI
C
          ENDIF
        ENDIF
CC DELTA-M MOMENTS (ICHKT=1 MEANS WE DO TRUNCATION AT LEAST ONE TIME)
        ICHKT=0
        NLGT1(L)=MIN(MXLGT1,NLGN1(L))
        IF(IMTHD.GE.0 .AND. NLGN1(L).GT.MXLGT1) THEN
          FF(L)=G(MXLGT1+1,L)
          ICHKT=1
         ELSE
          FF(L)=0
        ENDIF
        DO 11 K1=1,NLGT1(L)
   11   GT(K1,L)=(G(K1,L)-FF(L))/(1-FF(L))
        OMGT(L)=(1-FF(L))*OMG(L)/(1-FF(L)*OMG(L))
        THKT(L)=(1-FF(L)*OMG(L))*THK(L)
    3 CONTINUE
C RESET -MXLGN1- AND -MXLGT1-
      MXLGN1=1
      MXLGT1=1
      DO 12 L=1,NLN
      MXLGN1=MAX(MXLGN1,NLGN1(L))
   12 MXLGT1=MAX(MXLGT1,NLGT1(L))
C SET TRUNCATED USER DEFINED DEPTHS
      DPTT(1)=DPT(1)
      DO 13 L=1,NLN
   13 DPTT(L+1)=DPTT(L)+THKT(L)
      DO 14 IT=1,NTAU
      DO 47 L=1,NLN
      IF((UTAU(IT)-DPT(L))*(UTAU(IT)-DPT(L+1)).LE.0) THEN
        DTAU=UTAU(IT)-DPT(L)
        DTAUT=(1-FF(L)*OMG(L))*DTAU
        UTAUT(IT)=DPTT(L)+DTAUT
      ENDIF
   47 CONTINUE
      IF(UTAUT(IT).GT.DPTT(NLN+1)) UTAUT(IT)=DPTT(NLN+1)
   14 CONTINUE
C SCALING COEFFICIENTS FOR THERMAL EMISSION
      IF(NPLK1.GT.0) THEN
        DO 15 L=1,NLN
        DO 15 K1=1,NPLK1
   15   CPLKT(K1,L)=CPLK(K1,L)/(1-OMG(L)*FF(L))**(K1-1)
      ENDIF
C SET -MMAX1-
      MMAX1=1
      IF(INDA.GE.1) MMAX1=MXLGT1
C INITIALIZE ANGLE INDEX FOR CHECKING CONVERGENCE
        NB0=NA0
        DO 24 J=1,NA0
        IICHK0(J)=0
        JM0(J)=J
   24   BM0(J)=AM0(J)
      IF(INDA.GT.0) THEN
        NB1U=NA1U
        DO 25 I=1,NA1U
        IICHK1(I)=0
        IM1U(I)=I
   25   BM1U(I)=AM1U(I)
      ENDIF
C FOURIER SUM
      INIT=1
      DO 16 M1=1,MMAX1
        M=M1-1
        FM=PI
        IF(M.EQ.0) FM=2*PI
C 2001.4.5 SED : ADD DDLW for Water Leaving Radiance
        CALL FTRN21(INDG,INIT,M,INDA,INDT,IMTHD,NB1U,BM1U,NB0,BM0
     &  ,NDA,AMUA,WA,NLN,NLGT1,GT,DPTT,OMGT,NPLK1,CPLKT,GALB,BGND
     &  ,FSOL,NTAU,UTAUT,SCR,SCI,FLXD,FLXU,AIF,ERR
     &  ,SBRF,SZA,EZA,AZANG,IUCD,IUCR,IUCSR,DDLW,
     &   R0B,KB,THB,ICANO,INITR,IBRF,XLAND,XOCEAN,NW,ir,IRFS)
C 00.03.14 BY SED: parameters of Snow BRF data table and devise index are added
C        CALL FTRN21(INDG,INIT,M,INDA,INDT,IMTHD,NB1U,BM1U,NB0,BM0
C     &  ,NDA,AMUA,WA,NLN,NLGT1,GT,DPTT,OMGT,NPLK1,CPLKT,GALB,BGND
C     &  ,FSOL,NTAU,UTAUT,SCR,SCI,FLXD,FLXU,AIF,ERR
C     &  ,SBRF,SZA,EZA,AZANG,IUCD,IUCR,IUCSR)
C        CALL FTRN21(INDG,INIT,M,INDA,INDT,IMTHD,NB1U,BM1U,NB0,BM0
C     &  ,NDA,AMUA,WA,NLN,NLGT1,GT,DPTT,OMGT,NPLK1,CPLKT,GALB,BGND
C     &  ,FSOL,NTAU,UTAUT,SCR,SCI,FLXD,FLXU,AIF,ERR)
        IF(ERR.NE.' ') RETURN
cs         write(6,*)'check4'
        IF(INDA.GT.0) THEN
          DO 44 JB=1,NB0
   44     EU0(JB)=0
          DO 45 IB=1,NB1U
   45     EU1(IB)=0
          DO 17 K=1,NFI
          FI1=M*FI(K)*RAD
   17     COSM(K)=COS(FI1)/FM
          DO 18 JB=1,NB0
          J=JM0(JB)
          DO 18 IB=1,NB1U
          I=IM1U(IB)
          DO 18 K=1,NFI
          DO 18 IT=1,NTAU
          DAI=AIF(IB,JB,IT)*COSM(K)
          AI(I,J,K,IT)=AI(I,J,K,IT)+DAI
          IF(ABS(AI(I,J,K,IT)).GT.0) THEN
            EAI=ABS(DAI/AI(I,J,K,IT))
           ELSE
            IF(ABS(DAI).LE.0) THEN
              EAI=0
             ELSE
              EAI=100
            ENDIF
          ENDIF
          EU0(JB)=MAX(EU0(JB),EAI)
          EU1(IB)=MAX(EU1(IB),EAI)
   18     CONTINUE
C CHECK CONVERGENCE FOR AM0
          CALL CONVU(NB0,BM0,JM0,EU0,IICHK0,EPSU,NCHK2)
          IF(NB0.LE.0) GOTO 30
          IF(INDA.NE.2) THEN
            CALL CONVU(NB1U,BM1U,IM1U,EU1,IICHK1,EPSU,NCHK2)
            IF(NB1U.LE.0) GOTO 30
          ENDIF
C
        ENDIF
   16 CONTINUE
C 00.03.14 MODIFIED BY SED
C      IF(INDG.GT.0) INDG=2
      IF((INDG.GT.0).AND.(INDG.LT.3)) INDG=2
      IF (INDG.EQ.3) INDG=30
      IF (INDG.EQ.4) INDG=40
C 2001.4.5  MODIFIED BY SED add 'INDG=5' for LW
      IF (INDG.EQ.5) INDG=2
C
   30 IF(IMTHD.LE.0 .OR. INDA.LE.0 .OR. ICHKT.LE.0) RETURN
cs      write(6,*)'check5'
C 2002.05 PT
C PLGD2
      IF (IWS .EQ. 1) THEN
        DO 40 IZ=1,NFI
          SFIS(IZ)=FI(IZ)*RAD
          SFI(IZ)=COS(SFIS(IZ))
   40   CONTINUE
        MXIT2=MAX(MXLGN1,MXLGT1)
        DO 52 IS=1,NA0
          SMA0=1-AM0(IS)*AM0(IS)
          DO 53 IU=1,NA1U
            SAM1U=1-AM1U(IU)*AM1U(IU)
            SSQRT=SQRT(SMA0*SAM1U)
            SAA=AM0(IS)*AM1U(IU)
            DO 54 IZ=1,NFI
              CS12(IZ,IU,IS)=SAA+SSQRT*SFI(IZ)
C              CS12(IZ,IU,IS)=AM0(IS)*AM1U(IU)
C     &                      +SQRT((1-AM0(IS)**2)*(1-AM1U(IU)**2))
C     &                      *COS(FI(IZ)*RAD)
              IF(ABS(CS12(IZ,IU,IS)).GT.1.0) 
     &          CS12(IZ,IU,IS)=SIGN(1.0,CS12(IZ,IU,IS))
              CS10=CS12(IZ,IU,IS)
              ANG12(IZ,IU,IS)=ACOS(CS10)/RAD
              INIT=1
              DO 55 K1=1,MXIT2
                PLGD2(K1,IZ,IU,IS)=PLGD(INIT,CS10)
   55         CONTINUE
   54       CONTINUE
   53     CONTINUE
   52   CONTINUE
      ELSE
        IF (MXIT2 .LT. MAX(MXLGN1,MXLGT1)) THEN
          ERR='MXIT2<MAX(MXLGN1,MXLGT1)'
          RETURN
        ENDIF
      ENDIF
cs      write(6,*)'check6'
C PHS1
      IF(IK.EQ.1) THEN
      IF(INDP.GE.0) THEN
        DO 60 IS=1,NA0
         DO 61 IU=1,NA1U
          DO 62 IZ=1,NFI
            ANG1=ANG12(IZ,IU,IS)
            DO 63 L=1,NLN
              L9=L
              INIT=1
              PHS1(L,IZ,IU,IS)
     &          =PINT4(INIT,ANG1,KNANG,NANG,ANG,PHSF,L9)
   63       CONTINUE
   62     CONTINUE
   61    CONTINUE
   60   CONTINUE
      ELSE
        DO 65 IS=1,NA0
         DO 66 IU=1,NA1U
          DO 67 IZ=1,NFI
            DO 68 L=1,NLN
              INIT=1
              SUM=0
              DO 69 K1=1,NLGN1(L)
                SUM=SUM+(2*K1-1)*G(K1,L)*PLGD2(K1,IZ,IU,IS)
   69         CONTINUE
              PHS1(L,IZ,IU,IS)=SUM/4/PI
   68       CONTINUE
   67     CONTINUE
   66    CONTINUE
   65   CONTINUE
      ENDIF
      ENDIF
C A4
        IF(INDG.LE.0) THEN
          DO 81 IT=1,NTAU
           DO 82 IZ=1,NFI
            DO 83 IS=1,NA0
             DO 84 IU=1,NA1U
               A4(IT,IZ,IS,IU)=0.0
   84        CONTINUE
   83       CONTINUE
   82      CONTINUE
   81     CONTINUE
C 2002.05 FOR NLW
C        ELSE IF((INDG.GT.0).AND.(INDG.LT.3)) THEN
        ELSE IF(((INDG.GT.0).AND.(INDG.LT.3)).OR.(INDG.EQ.5)) THEN
C    
        DO 92 IU=1,NA1U
            IF(AM1U(IU).LT.0) THEN
              AM11=ABS(AM1U(IU))
              DO 94 IS=1,NA0
                DO 93 IZ=1,NFI
cs                IF (ITOFF.EQ.1)THEN
cs                  write(6,*)'AM11',acos(AM11)*180./PI
cs                  write(6,*)'AM0(IS)',acos(AM0(IS))*180./PI
cs                  write(6,*)'SFIS(IZ)',SFIS(IZ)*180./PI
cs                ENDIF
        SS(IZ)=FSOL*SEARF1(AM11,AM0(IS),SFIS(IZ),GALB,SCR,SCI)
cs        SS(IZ)=FSOL*AM0(IS)*SEARF1(AM11,AM0(IS),SFIS(IZ),GALB,SCR,SCI)
cs        SS(IZ)=FSOL*SEARF1(AM11,AM0(IS),SFIS(IZ),GALB,SCR,SCI)
cs     &   *AM0(IS)/PI
        
cs                IF (ITOFF.EQ.1)write(6,*)'SEARF1',SS(IZ)/FSOL
   93           CONTINUE
cs                write(6,*)'line'
                AA1=-DPTT(NLN1)/AM0(IS)
                DO 95 IT=1,NTAU
                  AA2=AA1-(DPTT(NLN1)-UTAUT(IT))/AM11
                  AA3=EXP(AA2)
                  DO 96 IZ=1,NFI
                    A4(IT,IZ,IS,IU)=SS(IZ)*AA3
   96             CONTINUE
   95           CONTINUE
   94         CONTINUE
            ELSE
              DO 101 IS=1,NA0
               DO 102 IZ=1,NFI
                DO 103 IT=1,NTAU
                  A4(IT,IZ,IS,IU)=0.0
  103           CONTINUE
  102          CONTINUE
  101         CONTINUE
            ENDIF
   92     CONTINUE
       ELSE IF((INDG.EQ.3).OR.(INDG.EQ.30)) THEN
          DO 71 IU=1,NA1U
            IF(AM1U(IU).LT.0) THEN
              AM11=ABS(AM1U(IU))
              DO 72 IS=1,NA0
                AA1=-DPTT(NLN1)/AM0(IS)
                DO 73 IZ =1,NFI
             SSKM=SEARF1(AM11,AM0(IS),SFIS(IZ),GALB,SCR,SCI)
             SSKM=SSKM*PI/AM0(IS)
                  BRF=CBRF3(IS,IU,IZ)
                  SS2=FSOL*(BRF*XLAND+SSKM*XOCEAN)*AM0(IS)/PI
cs                    SS2=FSOL*AM0(IS)*BRF
                  DO 74 IT=1,NTAU
                    AA2=AA1-(DPTT(NLN1)-UTAUT(IT))/AM11
                    AA3=EXP(AA2)
                    A4(IT,IZ,IS,IU)=SS2*AA3
   74             CONTINUE
   73           CONTINUE
   72         CONTINUE
            ELSE
              DO 111 IS=1,NA0
               DO 112 IZ=1,NFI
                DO 113 IT=1,NTAU
                  A4(IT,IZ,IS,IU)=0.0
  113           CONTINUE
  112          CONTINUE
  111         CONTINUE
            ENDIF
   71     CONTINUE
       ELSE IF((INDG.EQ.4).OR.(INDG.EQ.40)) THEN
          RADIV=1./RAD
          DO 76 IU=1,NA1U
            IF(AM1U(IU).LT.0) THEN
              AM11=ABS(AM1U(IU))
              THE=ACOS(AM11)*RADIV
              DO 77 IS=1,NA0
                AA1=-DPTT(NLN1)/AM0(IS)
                THI=ACOS(AM0(IS))*RADIV
                DO 78 IZ =1,NFI
                  CALL SBRFA2(THI,THE,FI(IZ),SBRF,SZA,EZA,AZANG,BRF)
                  SS2=FSOL*BRF*AM0(IS)/PI
                  DO 79 IT=1,NTAU
                    AA2=AA1-(DPTT(NLN1)-UTAUT(IT))/AM11
                    AA3=EXP(AA2)
                    A4(IT,IZ,IS,IU)=SS2*AA3
   79             CONTINUE
   78           CONTINUE
   77         CONTINUE
            ELSE
              DO 121 IS=1,NA0
               DO 122 IZ=1,NFI
                DO 123 IT=1,NTAU
                  A4(IT,IZ,IS,IU)=0.0
  123           CONTINUE
  122          CONTINUE
  121         CONTINUE
            ENDIF
   76     CONTINUE
       ENDIF
C
C--- INTENSITY CORRECTION.
        DO 19 IS=1,NA0
        DO 19 IU=1,NA1U
        DO 19 IZ =1,NFI
C 2002.05 PT
          CS1=CS12(IZ,IU,IS)
        DO 20 L=1,NLN
          SUM=0
          DO 22 K1=1,NLGT1(L)
            SUM=SUM+(2*K1-1)*GT(K1,L)*PLGD2(K1,IZ,IU,IS)
   22     CONTINUE
          PHST(L)=SUM/4/PI
          PHS(L)=PHS1(L,IZ,IU,IS)
   20   CONTINUE
CCC COS(SCATTERING ANGLE)
C        CS1=AM0(IS)*AM1U(IU)+SQRT((1-AM0(IS)**2)*(1-AM1U(IU)**2))
C     &    *COS(FI(IZ)*RAD)
C        IF(ABS(CS1).GT.1.0) CS1=SIGN(1.0,CS1)
C        ANG1=ACOS(CS1)/RAD
C        DO 20 L=1,NLN
C        L9=L
C        INIT=1
CCC INTERPOLATION OF ORIGINAL PHASE FUNCTION FROM GIVEN PHASE FUNCTION.
C        IF(INDP.GE.0) THEN
C          PHS(L)=PINT4(INIT,ANG1,KNANG,NANG,ANG,PHSF,L9)
CCC INTERPOLATION OF ORIGINAL PHASE FUNCTION FROM -G-.
C         ELSE
C          SUM=0
C          DO 21 K1=1,NLGN1(L)
C   21     SUM=SUM+(2*K1-1)*G(K1,L)*PLGD(INIT,CS1)
C          PHS(L)=SUM/4/PI
C        ENDIF
CCC INTERPOLATION OF TRUNCATED PHASE FUNCTION FROM -GT-.
C        INIT=1
C        SUM=0
C        DO 22 K1=1,NLGT1(L)
C   22   SUM=SUM+(2*K1-1)*GT(K1,L)*PLGD(INIT,CS1)
C        PHST(L)=SUM/4/PI
C   20   CONTINUE
C
        CALL INTCR1(IMTHD,AM0(IS),AM1U(IU),CS1,NTAU,UTAU,UTAUT
     &   ,NLN,THK,THKT,OMG,OMGT,PHS,PHST,FF,KLGN1,MXLGN1,NLGN1
     &   ,NLGT1,G,EPSP,NCHK1,COR,SGL2,PHSB,ERR)
        DO 23 IT=1,NTAU
C 2002.05 PT
          AI(IU,IS,IZ,IT)=
     &      AI(IU,IS,IZ,IT)+COR(IT)*FSOL+A4(IT,IZ,IS,IU)
cs          write(6,*)'A4',A4(IT,IZ,IS,IU)
   23   CONTINUE
CC Corrected single scattering from ocean surface
CC 00.03.14 Modified BY SED
CC        IF(INDG.LE.0) then
CC          A3=0
CC         else
CC          AM11=ABS(AM1U(IU))
CC          FIS=FI(IZ)*RAD
CC          IF(AM1U(IU).LT.0) then
CC            A3=FSOL *SEARF1(AM11,AM0(IS),FIS,GALB,SCR,SCI)
CC     &      *EXP(-DPTT(NLN1)/AM0(IS)-(DPTT(NLN1)-UTAUT(IT))/AM11)
CCC    &      *EXP(-(DPTT(NLN1)-UTAUT(IT))*(1/AM0(IS)-1/AM11))
CC          ELSE
CC            A3=0
CC          ENDIF
CC        endif
C       IF(INDG.LE.0) A3=0
CC 2001.04.05 SED for Water LEaving Radiance
C       IF(((INDG.GT.0).AND.(INDG.LT.3)).OR.(INDG.EQ.5)) THEN
CC       IF((INDG.GT.0).AND.(INDG.LT.3)) THEN
CC SED END
C           AM11=ABS(AM1U(IU))
C           FIS=FI(IZ)*RAD
C           IF(AM1U(IU).LT.0) then
C               A3=FSOL *SEARF1(AM11,AM0(IS),FIS,GALB,SCR,SCI)
C     &         *EXP(-DPTT(NLN1)/AM0(IS)-(DPTT(NLN1)-UTAUT(IT))/AM11)
CC    &         *EXP(-(DPTT(NLN1)-UTAUT(IT))*(1/AM0(IS)-1/AM11))
C           ELSE
C                  A3=0
C           ENDIF
C       ENDIF
CC 00.03.14 BY SED CANOPY SURFACE
C       IF((INDG.EQ.3).OR.(INDG.EQ.30)) THEN
C           AM11=ABS(AM1U(IU))
C           IF(AM1U(IU).LT.0) then
C              BRF=CBRF3(IS,IU,IZ)
C              A3=FSOL*BRF*AM0(IS)/PI
C     &        *EXP(-DPTT(NLN1)/AM0(IS)-(DPTT(NLN1)-UTAUT(IT))/AM11)
C           ELSE
C                  A3=0
C           ENDIF
C       ENDIF
CC 00.03.14 BY SED SNOW SURFACE
C       IF((INDG.EQ.4).OR.(INDG.EQ.40)) THEN
C           IF(AM1U(IU).LT.0) then
C              AM11=ABS(AM1U(IU))
CC RAD-->DEGREE
C              THE=ACOS(AM11)/RAD
C              THI=ACOS(AM0(IS))/RAD
C              CALL SBRFA2(THI,THE,FI(IZ),SBRF,SZA,EZA,AZANG,BRF)
C              A3=FSOL*BRF*AM0(IS)/PI
C     &        *EXP(-DPTT(NLN1)/AM0(IS)-(DPTT(NLN1)-UTAUT(IT))/AM11)
C           ELSE
C                  A3=0
C           ENDIF
C       ENDIF
CC
C   23   AI(IU,IS,IZ,IT)=AI(IU,IS,IZ,IT)+COR(IT)*FSOL+A3
C
CC ANGULAR LOOP END
   19 CONTINUE
cs        write(6,*)'IN RTRN FLXD',FLXD
cs        write(6,*)'IN RTRN FLXU',FLXU
C NORMAL END
      ERR=' '
      RETURN
      END
C PT
      SUBROUTINE SGLR2P (AM1, AM0, NL, TC, WP1, WP2, UT, SGL1, SGL2)
C      FUNCTION SGLR (AM1, AM0, NL, TC, W, P, UT)
C SINGLY SCATTERED INTENSITY (MULTI-LAYER) BY EQ. (12) OF NT.
C--- REFERENCE
C NT:  T. NAKAJIMA AND M. TANAKA, 1988, JQSRT, 40, 51-69
C--- HISTORY
C 89. 2.22   CREATED BY T. NAKAJIMA
C              FROM AIDS BY CHANGING THE MEANING OF T, W, P,
C              AND UT.  UT IS A LEVEL INSIDE THE MULTI-LAYER SYSTEM.
C     5. 4   PATCH FOR EXPONENTIAL OVERFLOW.
C     6.20   REFORM STYLE.
C    12.20   WITH CPEX(3)
C 90.12. 1   PUT IF(IALM... OUT OF DO-LOOP (FOR OPTIMIZATION)
C 2002.05    Change FUNTION -> SUBROUTINE for performance tuning
C FOLLOWINGS ARE DEFINITIONS OF SYMBOLS IN THE ROUTINE, WITH THE FORMAT:
C NAME    TYPE       CONTENT
C WHERE TYPE = S (SUBROUTINE), F (FUNCTION), I (INTEGER), R (REAL).
C FOR THE TYPE = I AND R, THE SIZE OF ARRAY IS SHOWN BY ( ).
C--- INPUT
C AM1      R         COS(EMERGENT ZENITH ANGLE) WITH THE SIGN OF
C                    TRANSMITTED( .GT.0),  REFLECTED( .LT.0).
C AM0      R         COS(INCIDENT ZENITH ANGLE) .GT. 0.
C NL       I         NO. OF SUBLAYERS.
C TC     R(NL)       OPTICAL THICKNESS OF SUBLAYER.
C W      R(NL)       SINGLE SCATTERING ALBEDO OF SUBLAYER.
C P      R(NL)       PHASE FUNCTION OF SUBLAYER.
C                    INTEGRATION OVER UNIT SPHERE SHOULD BE 1.
C UT       R         USER DEFINED OPTICAL DEPTH FOR THE INTENSITY
C                    WITHIN THE MULTI-LAYERED SYSTEM.
C--- OUTPUT
C SGLR     F         SINGLY SCATTERED INTENSITY.
      SAVE INIT,EPS
C 2002.05 PT
      DIMENSION TC(NL),CCP(3),WP1(NL),WP2(NL)
C      DIMENSION TC(NL),W(NL),P(NL),CCP(3)
C
      LOGICAL IALM
      DATA INIT/1/
      IF(INIT.GT.0) THEN
        INIT=0
        CALL CPCON(CCP)
        EPS=CCP(1)*30
      ENDIF
C SET EPS: IF ABS(1/AM1 - 1/AM0) .LE. EPS THEN
C                      THE ROUTINE SETS ALMUCANTAR CONDITION-IALM.
C 2002.05 PT
      SGL1=0.
      SGL2=0.
C      SGLR=0
C
      X=1/AM1-1/AM0
      IF(ABS(X).LE.EPS) THEN
        IALM=.TRUE.
      ELSE
        IALM=.FALSE.
      ENDIF
C 2002.05 PT
      IF (IALM) THEN
        E2=0.
      ELSE
        E2=0.
        IF(AM1.LT.0.AND.UT.GT.0.) E2=UT*X
        E2=EXPFN(E2)/X
      ENDIF
C
      T2=0
C 2002.05 PT
C      IST=.TRUE.
C

C X=0 (TRANSMISSION)
      IF(IALM) THEN
        DO 11 L=1,NL
          T1=T2
C 2002.05 PT
          IF(UT.LE.T1) GOTO 13
          T2=T1+TC(L)
C        T2=T1+TC(L)
C        IF(UT.LE.T1) GOTO 13
C
          IF(UT.LT.T2) T2=UT

C 2002.05 PT
C        IF(IST) THEN
C          IST=.FALSE.
C          E2=T1
C        ENDIF
C
          E1=E2
          E2=T2
C 2002.05 PT
          EE=E2-E1
          SGL1=SGL1+WP1(L)*EE
          SGL2=SGL2+WP2(L)*EE
C        SGLR=SGLR+W(L)*P(L)*(E2-E1)
C
   11   CONTINUE
      ELSE
C X<>0
C 2002.05 PT
        IF(AM1.LT.0) THEN
          DO 21 L=1,NL
            T1=T2
            T2=T1+TC(L)
CC REFLECTION
            IF(UT.GE.T2) GOTO 21
            IF(UT.GT.T1) T1=UT
            E1=E2
            E2=T2*X
            E2=EXPFN(E2)/X
            EE=E2-E1
            SGL1=SGL1+WP1(L)*EE
            SGL2=SGL2+WP2(L)*EE
   21     CONTINUE
        ELSE
          DO 31 L=1,NL
            T1=T2
            IF(UT.LE.T1) GOTO 13
            T2=T1+TC(L)
CC TRANSMISSION
            IF(UT.LT.T2) T2=UT
            E1=E2
            E2=T2*X
            E2=EXPFN(E2)/X
            EE=E2-E1
            SGL1=SGL1+WP1(L)*EE
            SGL2=SGL2+WP2(L)*EE
   31     CONTINUE
        ENDIF
      ENDIF
   13 CONTINUE
      E1=-UT/AM1
      EE=EXPFN(E1)
      AAM1=ABS(AM1)
      SGL1=SGL1*EE/AAM1
      SGL2=SGL2*EE/AAM1
C      DO 21 L=1,NL
C        T1=T2
C        T2=T1+TC(L)
CCC REFLECTION
C        IF(AM1.LT.0) THEN
C          IF(UT.GE.T2) GOTO 21
C          IF(UT.GT.T1) T1=UT
CCC TRANSMISSION
C         ELSE
C          IF(UT.LE.T1) GOTO 13
C          IF(UT.LT.T2) T2=UT
C        ENDIF
C
C        IF(IST) THEN
C          IST=.FALSE.
C          E2=T1*X
C          E2=EXPFN(E2)/X
C        ENDIF
C        E1=E2
C        E2=T2*X
C        E2=EXPFN(E2)/X
C        SGLR=SGLR+W(L)*P(L)*(E2-E1)
C 21   CONTINUE
C      ENDIF
C 13   E1=-UT/AM1
C      SGLR=SGLR*EXPFN(E1)/ABS(AM1)
C
      RETURN
      END
C---------------------------------------------------------------------------
C TOOLS
C 00.03.14 SED SUBROUTINE LOCATL()
C              SUBROUTINE LOCATU()
C----------------------------------------------------------------------------
      SUBROUTINE LOCATL( XX, N, X, JL )
C PURPOSE
C     TO FIND THE NEAREST LOWER ELEMENT OF XX FOR X
C     XX(1):SMALLEST VALUE  <  XX(N):LARGESET VALUE
C INPUT 
C     XX R(KXX) ARRAY OF GRID POINTS
C     N  I     TOTAL NUMBER OF ELEMENTS OF GRID POINTS 
C OUTPUT
C     JL  I  THE NEAREST LOWER ELEMENT. XX(JL) < X < XX(JL+1)
C LOCAL 
C     JU,JM,JL I
C PARAMETER
!===================== BY DUAN ==================================
      PARAMETER (KXX = 40 )
!      PARAMETER (KXX = 20 )
!===================== BY DUAN ==================================
C AREA
      DIMENSION  XX(KXX)
C CHECK
      IF( KXX.LT.N )THEN
          WRITE(6,*) 'ERROR IN LOCATL(): DIMENSION SIZE IS SHORT'
          STOP
      ENDIF
C 
      JL = 1
      JU = N
C
      IF( (XX(JL) .LE.X ).AND.(X .LE.XX(JU)) ) THEN
C LOOP START
 10      CONTINUE
         JM = ( JU + JL )/2
         IF ( X .LT. XX(JM) ) THEN
              JU = JM
         ELSE
              JL = JM
         ENDIF
         IF( (JU-JL).GT.1 ) THEN 
              GOTO 10
         ELSE
              GOTO 20
         ENDIF
C LOOP EXIT
 20      CONTINUE
      ELSE
         WRITE(6,*) 'ERROR IN LOCATL(): OVER RANGE'
         WRITE(6,*) 'X=',X
         WRITE(6,*) 'XX(',JL,')=',XX(JL),'<VAL< XX(',JU,')=',XX(JU)
         STOP
      ENDIF
      END
C------------------------------------------------------------------------------
      SUBROUTINE LOCATU( XX, N, X, JL )
C PURPOSE
C     TO FIND THE NEAREST UPPER VALUE OF XX FOR X
C     XX(1):LARGEST VALUE  >  XX(N):SMALLEST VALUE
C INPUT 
C     XX R(20) ARRAY OF GRID POINTS
C     N  I     TOTAL NUMBER OF ELEMENTS OF GRID POINTS 
C     X  R     TARGET
C OUTPUT
C     JL  I  THE NEAREST LOWER LOCATION. XX(JL)> X > XX(JL+1)
C LOCAL 
C     JU,JM,JL I
C PARAMETER
!===================== BY DUAN ==================================
      PARAMETER (KXX = 40 )
!      PARAMETER (KXX = 20 )
!===================== BY DUAN ==================================

C AREA
      DIMENSION XX(KXX)
C CHECK
      IF( KXX.LT.N )THEN
          WRITE(6,*) 'ERROR IN LOCATU(): DIMENSION SIZE IS SHORT'
          STOP
      ENDIF
C 
      JL = 1
      JU = N
C
      IF( (XX(JU) .LE.X ).AND.(X .LE.XX(JL)) ) THEN
C LOOP START
 10      CONTINUE
         JM = ( JU + JL )/2
         IF ( X .GT. XX(JM) ) THEN
              JU = JM
         ELSE
              JL = JM
         ENDIF
         IF( (JU-JL).GT.1 ) THEN 
              GOTO 10
         ELSE
              GOTO 20
         ENDIF
C LOOP EXIT
 20      CONTINUE
      ELSE
         WRITE(6,*) 'ERROR IN LOCATU(): OVER RANGE'
         WRITE(6,*) 'X=',X
         WRITE(6,*) 'XX(',JL,')=',XX(JL),'>VAL> XX(',JU,')=',XX(JU)
         STOP
      ENDIF
      END
C-----------------------------------------------------------------------------
      SUBROUTINE BILIN(X1,X2,Y1,Y2,X,Y)
C BI-LINEAR INTERPOLATION
C 
      Y=((Y2-Y1)/(X2-X1))*(X-X1)+Y1
      RETURN
      END 

      subroutine WVCAL(IND,P,T,RH,PPMV,GM3,E,GM3S,ES)
C Conversion of units for water vapor
C PPMV =N/Na =e/(P-e)*1.0E6, RH =E/ES =GM3/GM3S
C eV = NRT, (P-e)V = Na RT
C--- history
C 95. 9.30  Created
C--- input
C IND   I    1: Give RH  (relative humidity, 0-1)
C            2: Give PPMV (volume mixing ratio in ppmV)
C            3: Give GM3 (mass mixing ratio in g/m3)
C            4: Give E    (water vapor pressure in hPa)
C P     R    Atmospheric pressure (hPa)
C T     R    Temperature (K)
C One of RH, PPMV, GM3, or E
C--- output
C Other than input variable among RH, PPMV and GM3.
C GM3S  R    Saturation water vapor content (g/m3)
C ES    R    Saturation water vapor pressure (hPa)
C--- Gas constant (cgs) and molecular weights of water vapor
C           and air (g/mol)
      parameter (R=8.314e7, W=18.02, AIR=28.964)
C Satuaration water vapor (g/cm3)
      GCM3S=WVSAT(T)*1.0E-6
      ES=GCM3S/W*R*T/1.0E3
      if(IND.eq.1) then
        GCM3=RH*GCM3S
        E=RH*ES
        PPMV=E/(P-E)*1.0E6
       else if(IND.eq.2) then
        E=P*PPMV/(1.0E6+PPMV)
        RH=E/ES
        GCM3=RH*GCM3S
       else if(IND.eq.3) then
        GCM3=GM3*1.0E-6
        RH=GCM3/GCM3S
        E=RH*ES
        PPMV=E/(P-E)*1.0E6
       else if(IND.eq.4) then
        RH=E/ES
        GCM3=RH*GCM3S
        PPMV=E/(P-E)*1.0E6
      endif
      GM3=GCM3*1.0E6
      GM3S=GCM3S*1.0E6
      return
      end
      subroutine INTS5B(IUA,IUS,MATM,NL,ALT,PRS,TMP
     & ,NMOL,CNG,NPTC,CNPRF,ISPCV,RFRAC,ASPHR1,RHO,DRYAER,NAW
     & ,AWCR,NV,NWLV,WLV,RFI,ERR)
C Initilization of star5B
C Assume non-spherical parameter input
C--- history
C 95. 9.20  Created
C 96. 3.11  KPTC=20 for more aerosol models and non-spherical parameters
C           GETPR1 included
C     5. 5  Drop NPOLY and get all the polydispersion information
C--- input
C IUA        I        Device number for atmospheric model file
C IUS        I        Device number for particle model file
C MATM       I        Model number of atmosphere (AFGL)
C                      1: tropical
C                      2: Mid-latitude summer    3: Mid-latitude winter
C                      4: High-lat summer        5: High-lat winter
C                      6: US standard
C--- output
C NL         I        Number of atmospheric layers
C ALT     R(KNL)      Height at interfaces of layers (km), top to bottom
C PRS     R(KNL)      Pressure    at interfaces of layers (mb)
C TMP     R(KNL)      Temperature at interfaces of layers (K)
C NMOL       I        Number of gases
C CNG    R(KNL,KNM0)  Gas concentration (ppmv)
C NPTC      I         Number of particle polydispersions
C CNPRF  R(KNL,KPTC)  Dry volume concentration profile (relative unit)
C ISPCV  I(3,KPTC)    Fundamental materials for 3 comp. internal mixture (1-8)
C RFRAC  R(3,KPTC)    Dry volume fraction of the dry mixture
C ASPHR1 R(3,KPTC)    Non-spherical parameters (x0, G, r)
C RHO    R(KPTC)      Paricle density relative to water
C DRYAER R(6,4,KPTC)  dV/dlnr parameters for the dry mixture
C                     See VLSPC2
C                     C-values (Coefficients of volume spectrum) are ralative
C NAW    I(KPTC)      Number of AW
C AWCR  R(KAW,KPTC,2) 1: AW      Water activity (see Shettle and Fenn.)
C                     2: RMMD    RMMD
C NV        I         Number of fundamental species (1-8)
C NWLV      I         Number of wavelengths for refractive index tables
C WLV    R(KWL)       Wavelengths (micron)
C RFI   R(KWL,KNV,2)  Refractive index of fundamental materials (mr, mi)
C                     =mr - i mi
C                     with log-regular wavelength interval
C ERR      C*64       ERROR INDICATER. IF ' ' THEN NORMAL END.
C---
C for this routine
      SAVE
      CHARACTER ERR*(*)
      PARAMETER (PI=3.141592653,RAD=PI/180.0)
      PARAMETER (KPTC  =20)
      PARAMETER (KNL   =50)
      PARAMETER (KNM0  =28)
      PARAMETER (KAW   =17)
      PARAMETER (KWLV  =210)
C 2001.2.8 SED KNV=9 --> KNV=10
C      PARAMETER (KNV   =9)
      PARAMETER (KNV   =10)
      dimension ALT(KNL),PRS(KNL),TMP(KNL)
     & ,CNG(KNL,KNM0),WLV(KWLV),RFI(KWLV,KNV,2)
C for MLATM
      PARAMETER (KNM1=7, KNM2=21, KATM=6)
      PARAMETER (KNM=KNM1+KNM2)
      CHARACTER IDM(KNM)*8
      DIMENSION WMOL(KNM,10),RAMS(KNM,10),PMATM(KNL,KATM)
     & ,TMATM(KNL,KATM),DNSTY(KNL,KATM),AMOL(KNL,KNM1,KATM)
     & ,TRAC(KNL,KNM2),IDMS(KNM,10)
C for GETPAR
      dimension CNPRF(KNL,KPTC),ISPCV(3,KPTC),RFRAC(3,KPTC),RHO(KPTC)
     & ,DRYAER(6,4,KPTC),NAW(KPTC),AWCR(KAW,KPTC,2),ASPHR1(3,KPTC)
      DATA INIT/1/
C---
      ERR=' '
      IF(INIT.GT.0) then
        INIT=0
C Model atmospheres
        CALL MLATM(IUA,NL,NATM,NM1,NM2,IDM,IDMS,WMOL,RAMS,AIRM
     &   ,ALT,PMATM,TMATM,DNSTY,AMOL,TRAC,ERR)
        IF(ERR.NE.' ') RETURN
C  Get aerosol parameters
        CALL GTPAR1(IUS,NL,NPTC,CNPRF
     & ,ISPCV,RFRAC,ASPHR1,RHO,DRYAER,NAW,AWCR,NV,NWLV,WLV,RFI,ERR)
        IF(ERR.NE.' ') RETURN
      ENDIF
C
      DO 1 L=1,NL
      PRS(L)=PMATM(L,MATM)
      TMP(L)=TMATM(L,MATM)
      DO 2 M=1,7
    2 CNG(L,M)=AMOL(L,M,MATM)
      DO 3 M=8,28
    3 CNG(L,M)=TRAC(L,M-7)
    1 continue
      NMOL=28
      return
      end
      SUBROUTINE GTPAR1(IUS,NL,NPTC,CNPRF
     & ,ISPCV,RFRAC,ASPHR1,RHO,DRYAER,NAW,AWCR,NV,NWLV,WLV,RFI,ERR)
C Get parameters of particle polydispersions
C--- HISTORY
C 95. 1.25  CREATED FROM GETPH3
C 95. 1.24  DINTPM -> AERD (NOW INCLUDS ALSO AEROSOL WATER UPTAKE DATA)
C 95  9.13  with modified AERD file format
C 96. 3.11  Added non-spherical parameters
C     5. 5  AWCR(*,*,2)
C--- Input
C IUS       I         Read unit number of particle parameter file
C NL        I         Number of layers (come from MLATM)
C--- Output
C NPTC      I         Number of particle polydispersions
C CNPRF  R(KNL,KPTC)  Dry volume concentration profile (relative unit)
C ISPCV  I(3,KPTC)    Fundamental materials for 3 comp. internal mixture (1-8)
C RFRAC  R(3,KPTC)    Dry volume fraction of the dry mixture
C ASPHR1 R(3,KPTC)    Non-spherical parameters (x0, G, r)
C RHO     R(KPTC)      Paricle density relative to water
C DRYAER R(6,4,KPTC)  dV/dlnr parameters for the dry mixture
C                     See VLSPC2
C                     C-values (Coefficients of volume spectrum) are ralative
C NAW    I(KPTC)      Number of AW
C AWCR R(KAW,KPTC,2)  1: AW      Water activity (see Shettle and Fenn.)
C                     2: RMMD    RMMD
C NV        I         Number of fundamental species (1-8)
C NWLV      I         Number of wavelengths for refractive index tables
C WLV    R(KWL)       Wavelengths (micron)
C RFI   R(KWL,KNV,2)  Refractive index of fundamental materials (mr, mi)
C                     =mr - i mi
C                     with log-regular wavelength interval
C---
C 2001. 4.11 SED
C     Adding File Read Error Code fo AERDB
C 
      CHARACTER ERR*(*)
      PARAMETER (KNL   =50)
      PARAMETER (KAW   =17)
      PARAMETER (KPTC  =20)
      PARAMETER (KWLV  =210)
C 2001.2.8 SED KNV=9 --> KNV=10
C      PARAMETER (KNV   =9)
      PARAMETER (KNV   =10)
      dimension CNPRF(KNL,KPTC),ISPCV(3,KPTC),RFRAC(3,KPTC),RHO(KPTC)
     & ,DRYAER(6,4,KPTC),NAW(KPTC),AWCR(KAW,KPTC,2)
     & ,WLV(KWLV),RFI(KWLV,KNV,2),ASPHR1(3,KPTC)
C Work
      CHARACTER CH*1
C---
      ERR=' '
      REWIND IUS
C  Number concentration profiles (relative unit)
C  Number is conservative in growing aerosols
C 2001. 4.11 SED
C      READ(IUS,2) CH
C    2 FORMAT(A1)
C      READ(IUS,*) RMIN,RMAX
CC 2001.2.8 SED AERDB_a-c0.1
C      READ(IUS,*) RMIN2,RMAX2
C      READ(IUS,*) NPTC
C      DO 6 M=1,NPTC
C      READ(IUS,2) CH
C    6 read(IUS,*) (CNPRF(L,M),L=1,NL)
CC Loop for aerosol MPTCs
C      DO 1 M=1,NPTC
C      READ(IUS,2) CH
C      READ(IUS,*) (ISPCV (I,M),I=1,3)
C      READ(IUS,*) (RFRAC (I,M),I=1,3)
C      READ(IUS,*) (ASPHR1(I,M),I=1,3)
C      READ(IUS,*) RHO(M)
C      read(IUS,*) NMODE
C      DRYAER(1,2,M)=NMODE
C      DRYAER(1,3,M)=RMIN
C      DRYAER(1,4,M)=RMAX
C      DO 3 J=1,NMODE
C    3 READ(IUS,*) (DRYAER(I,J,M),I=2,6)
C      READ(IUS,*) NAW(M)
C      IF(NAW(M).GT.0) then
C        READ(IUS,2) CH
C        DO 7 I=1 ,NAW(M)
C    7   READ(IUS,*) (AWCR(I,M,K),K=1,2)

      READ(IUS,2,ERR=80) CH
    2 FORMAT(A1)
      READ(IUS,*,ERR=80) RMIN,RMAX
C 2001.2.8 SED AERDB_a-c0.1
      READ(IUS,*,ERR=80) RMIN2,RMAX2
      READ(IUS,*,ERR=80) NPTC
      DO 6 M=1,NPTC
      READ(IUS,2,ERR=80) CH
    6 read(IUS,*,ERR=80) (CNPRF(L,M),L=1,NL)
C Loop for aerosol MPTCs
      DO 1 M=1,NPTC
      READ(IUS,2,ERR=80) CH
      READ(IUS,*,ERR=80) (ISPCV (I,M),I=1,3)
      READ(IUS,*,ERR=80) (RFRAC (I,M),I=1,3)
      READ(IUS,*,ERR=80) (ASPHR1(I,M),I=1,3)
      READ(IUS,*,ERR=80) RHO(M)
      read(IUS,*,ERR=80) NMODE
      DRYAER(1,2,M)=NMODE
      DRYAER(1,3,M)=RMIN
      DRYAER(1,4,M)=RMAX
      DO 3 J=1,NMODE
    3 READ(IUS,*,ERR=80) (DRYAER(I,J,M),I=2,6)
      READ(IUS,*,ERR=80) NAW(M)
      IF(NAW(M).GT.0) then
        READ(IUS,2,ERR=80) CH
        DO 7 I=1 ,NAW(M)
    7   READ(IUS,*,ERR=80) (AWCR(I,M,K),K=1,2)
C SED END
      endif
    1 continue
C
C 2001.2.8 SED AERDB_a-c0.1
      DRYAER(1,3,12)=RMIN2
      DRYAER(1,4,12)=RMAX2
C
C Get refractive index table
C 2001. 4.11 SED
C      READ(IUS,2) CH
C      read(IUS,*) NV,NWLV
C      read(IUS,*) (WLV(I),I=1,NWLV)
C      do 4 IV=1,NV
C      READ(IUS,2) CH
C      read(IUS,*) (RFI(I,IV,1),I=1,NWLV)
C    4 read(IUS,*) (RFI(I,IV,2),I=1,NWLV)

      READ(IUS,2,ERR=80) CH
      read(IUS,*,ERR=80) NV,NWLV
      read(IUS,*,ERR=80) (WLV(I),I=1,NWLV)
      do 4 IV=1,NV
      READ(IUS,2,ERR=80) CH
      read(IUS,*,ERR=80) (RFI(I,IV,1),I=1,NWLV)
    4 read(IUS,*,ERR=80) (RFI(I,IV,2),I=1,NWLV)
C SED END
      RETURN
   80 ERR='Reference File Read Error (AERDB)'
      RETURN

      END
C
C 2002.05.21 Delete RSTR5B for PT BY SED
C
      FUNCTION WVSAT(T)
C Saturation water vapor amount (Lowtran)
C  Ws=A*exp(18.9766-14.9595A-2.4388A**2) g/m3
C  A=T0/T, T0=273.15, T in K (T0-50,T0+50)
C--- history
C 93. 3. 3 Created
C--- Input
C T       R      Absolute temperature (K)
C                T0-50 to T0+50
C--- Output
C WVSAT  RF      Water vapor amount (g/m3)
C---
      A=273.15/T
      WVSAT=A*exp(18.9766-14.9595*A-2.4388*A**2)
      RETURN
      END
      SUBROUTINE MLATM(IU,NL,NATM,NM1,NM2,IDM,IDMS,WMOL,RAMS,AIRM
     & ,ALT,PMATM,TMATM,DNSTY,AMOL,TRAC,ERR)
C SET MODEL PROFILES
C--- HISTORY
C 90. 2.24 MODIFIED BY T. NAKAJIMA
C 92. 3.30 IDMS*8 -> IDMS FOR SUN-SPARC READ-STATEMENT TROUBLE
C 94. 3.29 IDMS(I,J)=' ' -> 0
C 94. 6.20 add return
C    10.30 9 format(A8) for IBM
C 95. 9.15 REWIND IU
C 2001.4.11 Adding File Read Error check BY SED
C--- INPUT
C IU        I         READ UNIT NUMBER
C--- OUTPUT
C NL        I         NUMBER OF LAYERS = 50
C NATM      I         NUMBER OF MODEL ATMOSPHERES = 6
C NM1       I         NUMBER OF MOLECULES OF FIRST KIND = 7
C NM2       I         NUMBER OF MOLECULES OF SECOND KIND = 21
C IDM     C(KNM)*8    MOLECULAR ID
C IDMS   I(KNM,10)    ISOTOPE ID (0 MEANS END)
C WMOL   R(KNM,10)    MOLECULAR WEIGHTS
C RAMS   R(KNM,10)    RELATIVE ABUNDANCE OF ISOTOPES (0 MEANS END)
C AIRM      R         MOLECULAR WEIGHT OF AIR
C ALT     R(KNL)      ALTITUDE (K)
C PMATM  R(KNL,KATM)  MODEL PRESSURE PROFILES (MB)
C                     1: TROPICAL,
C                     2: MID-LATITUDE SUMMER,  3: MID-LATITUDE WINTER
C                     4: HIGH-LAT SUMMER    ,  5: HIGH-LAT WINTER
C                     6: US STANDARD
C TMATM  R(KNL,KATM)  MODEL TEMPERATURE PROFILES
C DNSTY  R(KNL,KATM)  DENSITY (AIR MOLECULES / CM3) = P*NA/R/T
C AMOL   R(KNL,KNM1   MOLECULAR PROFILES (PPMV)
C            ,KATM)   1: H2O    2: CO2     3: O3      4: N2O     5: CO
C                     6: CH4    7: O2
C TRAC   R(KNL,KNM2)  TRACE GASE FROFILES (PPMV)
C                     8: NO     9: SO2    10: NO2    11: NH3    12: HNO3
C                    13: OH    14: HF     15: HCL    16: HBR    17: HI
C                    18: CLO   19: OCS    20: H2CO   21: HOCL   22: N2
C                    23: HCN   24: CH3CL  25: H2O2   26: C2H2   27: C2H6
C                    28: PH3
C
C ERR      C*64       ERROR INDICATER
C
C--- DATA FILE
C MLATMD
C--- AREA FOR THIS ROUTINE
      PARAMETER (KNL=50, KNM1=7, KNM2=21, KATM=6)
      PARAMETER (KNM=KNM1+KNM2)
      CHARACTER ERR*(*),IDM(KNM)*8
      DIMENSION WMOL(KNM,10),RAMS(KNM,10),ALT(KNL),PMATM(KNL,KATM)
     & ,TMATM(KNL,KATM),DNSTY(KNL,KATM),AMOL(KNL,KNM1,KATM)
     & ,TRAC(KNL,KNM2),IDMS(KNM,10)
C--- WORK AREA
      CHARACTER CH*1
      ERR=' '
      REWIND IU
C READ NUMBERS
C 2001.4.11 SED
C      READ(IU,1) CH
C    1 FORMAT(A1)
C      READ(IU,*) NM1,NM2,NATM,NL
      READ(IU,1,ERR=80) CH
    1 FORMAT(A1)
      READ(IU,*,ERR=80) NM1,NM2,NATM,NL
      NM=NM1+NM2
C SED
C READ MOLECULAR INFORMATION
C 2001.4.11 SED
C      READ(IU,1) CH
C      READ(IU,*) AIRM
C      READ(IU,1) CH
C      DO 2 I=1,NM
C      READ(IU,9) IDM(I)
C    9 format(A8)
C      READ(IU,*) NS
C      DO 3 J=1,10
C      IDMS(I,J)=0
C    3 RAMS(I,J)=0
C      READ(IU,*) (IDMS(I,J),J=1,NS)
C      READ(IU,*) (WMOL(I,J),J=1,NS)
C    2 READ(IU,*) (RAMS(I,J),J=1,NS)
      READ(IU,1,ERR=80) CH
      READ(IU,*,ERR=80) AIRM
      READ(IU,1,ERR=80) CH
      DO 2 I=1,NM
      READ(IU,9,ERR=80) IDM(I)
    9 format(A8)
      READ(IU,*,ERR=80) NS
      DO 3 J=1,10
      IDMS(I,J)=0
    3 RAMS(I,J)=0
      READ(IU,*,ERR=80) (IDMS(I,J),J=1,NS)
      READ(IU,*,ERR=80) (WMOL(I,J),J=1,NS)
    2 READ(IU,*,ERR=80) (RAMS(I,J),J=1,NS)
C SED END
C READ ALTITUDE
C 2001. 4.11 SED
C      READ(IU,1) CH
C      READ(IU,*) (ALT(I),I=1,NL)
      READ(IU,1,ERR=80) CH
      READ(IU,*,ERR=80) (ALT(I),I=1,NL)
C SED END
C READ PRESSURE
      DO 4 J=1,NATM
C 2001.4.11 SED
C      READ(IU,1) CH
C    4 READ(IU,*) (PMATM(I,J),I=1,NL)
      READ(IU,1,ERR=80) CH
    4 READ(IU,*,ERR=80) (PMATM(I,J),I=1,NL)
C SED END
C READ TEMPERATURE
      DO 5 J=1,NATM
C 2001.4.11 SED
C      READ(IU,1) CH
C    5 READ(IU,*) (TMATM(I,J),I=1,NL)
      READ(IU,1,ERR=80) CH
    5 READ(IU,*,ERR=80) (TMATM(I,J),I=1,NL)
C SED END
C READ MOLECULAR PROFILES
      DO 6 K=1,NM1
      DO 6 J=1,NATM
C 2001.4.11 SED
C      READ(IU,1) CH
C    6 READ(IU,*) (AMOL(I,K,J),I=1,NL)
      READ(IU,1,ERR=80) CH
    6 READ(IU,*,ERR=80) (AMOL(I,K,J),I=1,NL)
C SED END
      DO 7 J=1,NATM
C 2001.4.11 SED
C      READ(IU,1) CH
C    7 READ(IU,*) (DNSTY(I,J),I=1,NL)
      READ(IU,1,ERR=80) CH
    7 READ(IU,*,ERR=80) (DNSTY(I,J),I=1,NL)
C SED END
C READ TRACE GASE PROFILES
      DO 8 K=1,NM2
C 2001.4.11 SED
C      READ(IU,1) CH
C    8 READ(IU,*) (TRAC(I,K),I=1,NL)
      READ(IU,1,ERR=80) CH
    8 READ(IU,*,ERR=80) (TRAC(I,K),I=1,NL)
C SED END
      return
C 2001.4.11 SED
   80 ERR='Reference File Read Error (MLATMD)'
      RETURN
C SED END
      END

      SUBROUTINE RFIDXB(IUK,WLC,RH,MP,ISPCVP,RFRACP,ASPHR,ROP
     & ,DRYAP,NAWP,AWCRP,NV,NWLV,WLV,RFI,CRW1,CIW1,VOLDP,VOLP
     & ,CEXTP,CABSP,NANG,ANG,PHP,ERR)
C Get parameters for wet particle polydispersions
C--- history
C 95. 2.22 CREATED FROM GETPAR
C 95. 9.16 Modified
C 96. 2.29  2ND ORDER POLYNOMIAL INTERPOLATION (T.Y.NAKAJIMA)
C 96. 3.11 Allow non-spherical parameters
C 96. 7.19 debugged (Takayabu, T. Y. Nakajima)
C 96. 7.19 Log for CI AND CR interpolation.(T.Y.NAKAJIMA)
C     5. 5 AWCRP(*,*,2)
C 97. 3.17 CRW1,CIW1 on SUBROUTINE RFIDXB by T.Y.Nakajima
C 97. 5.6  Same Debug as
C--- input
C IUK      I          Device unit number for reading a kernel file
C WLC      R          Wavelength (cm)
C RH       R          Relative humidity
C MP       I          Aerosol number
C ISPCVP I(3,KPTC)    Fundamental materials for 3 comp. internal mixture (1-8)
C RFRACP R(3,KPTC)    Dry volume fraction of the dry mixture
C ASPHR  R(3,KPTC)    Non-spherical parameters (x0, G, r)
C ROP    R(KPTC)      Paricle density relative to water
C DRYAP  R(6,4,KPTC)  dV/dlnr parameters for the dry mixture
C                     See VLSPC2
C                     C-values (Coefficients of volume spectrum) are ralative
C NAWP   I(KPTC)      Number of AW
C AWCRP R(KAW,KPTC,2) 1: AW      Water activity (see Shettle and Fenn.)
C                     2: RMMD    RMMD
C NV        I         Number of fundamental species (1-8)
C NWLV      I         Number of wavelengths for refractive index tables
C WLV    R(KWL)       Wavelengths (micron)
C RFI   R(KWL,KNV,2)  Refractive index of fundamental materials (mr, mi)
C                     =mr - i mi
C                     with log-regular wavelength interval
C--- output
C VOLDP    R          total dry volume
C VOLP     R          total volume
C CEXTP    R          extinction cross section
C CABSP    R          absorption cross section
C NANG     I          Number of scattering angles
C ANG    R(NANG)      Scattering angle in degrees
C PHP    R(KNANG)     Volume scattering phase function
C ERR      C*64       ERROR INDICATER. IF ' ' THEN NORMAL END.
C---
      SAVE
      CHARACTER ERR*(*)
      PARAMETER (KNM0  =28)
      PARAMETER (KPTC  =20)
      PARAMETER (KAW   =17)
      PARAMETER (KWLV  =210)
C 2001.2.8 SED KNV=9 --> KNV=10
C      PARAMETER (KNV   =9)
      PARAMETER (KNV   =10)
      PARAMETER (KNANG =250)
      PARAMETER (KINTVL=75)
      PARAMETER (KPOL  =1)
      dimension ISPCVP(3,KPTC),RFRACP(3,KPTC),ROP(KPTC)
     & ,DRYAP(6,4,KPTC),NAWP(KPTC),AWCRP(KAW,KPTC,2)
     & ,WLV(KWLV),RFI(KWLV,KNV,2),ANG(KNANG),PHP(*)
     & ,ASPHR(3,KPTC)
C Work (GTPH4B)
      PARAMETER (KNANG2=KNANG+2)
      DIMENSION PR(6,4),PH(KNANG,KPOL),SZP(KINTVL)
CC 96.05.09
      dimension AW(KAW),RMMD(KAW)
CC 96.2.29
      DIMENSION X(3),YR(3),YI(3)
C
      ERR=' '
C loop for polydispersion
      NMODE=DRYAP(1,2,MP)+0.001
      PR(1,2)=1
      PR(1,3)=DRYAP(1,3,MP)
      PR(1,4)=DRYAP(1,4,MP)
      X0=ASPHR(1,MP)
      GG=ASPHR(2,MP)
      RP=ASPHR(3,MP)
C loop for size distribution mode
      CEXTP=0
      CABSP=0
      VOLP =0
      VOLDP=0
      DO 7 I=1,KNANG
    7 PHP(I)=0
C
      DO 5 M=1,NMODE
      DO 2 I=2,6
    2 PR(I,1) =DRYAP(I,M,MP)
      IF(NAWP(MP).LE.0) then
CC Dry aerosols
        RMD=1
        RMW=1
       else
CC Wet aerosols
        ITP=PR(2,1)+0.001
        IF(ITP.NE.2) then
          ERR='Wet aerosols assume only log-normal size distribution'
          return
        endif
        RMD=DRYAP(5,M,MP)
        do 3 J=1,NAWP(MP)
        AW  (J)=AWCRP(J,MP,1)
    3   RMMD(J)=AWCRP(J,MP,2)
        RMW=RAWB(RH,RMD,ROP(MP),NAWP(MP),AW,RMMD)
        PR(5,1)=RMW
C 96.5.5
        PR(3,1)=PR(3,1)*(RMW/RMD)**3
      endif
C Search wavelength in refractive index table
      WL1=WLC*1.0E4
C      DWL=LOG(WL1/WLV(1))/LOG(WLV(NWLV)/WLV(1))*NWLV+1
      DWL=LOG(WL1/WLV(1))/LOG(WLV(NWLV)/WLV(1))*(NWLV-1)+1
      IWL=DWL
CC 96.2.29
CC      IF(IWL.LE.0  ) IWL=1
CC      IF(IWL.GE.NWLV) IWL=NWLV-1
CC      IWL1=IWL+1
C 3 dry aerosol components refractive index:
      CR0=0
      CI0=0
      DO 4 ISP=1,3
      IS=ISPCVP(ISP,MP)
      IF(IS.GT.0) then
C INTERPOLATION TO GET CORRESPONDING CR1 AND CI1 TO WL1
CC 96.2.29 T.Y.Nakajima
      NINTP=3
      IF(NINTP.GT.NWLV)NINTP=NWLV
      IWLT=IWL
      IF(IWL.GE.NWLV-1)IWLT=NWLV-2
      DO 71 IG=1,NINTP
      X (IG)=LOG(WLV(IWLT+IG-1))

C96.7.19 T.Y.Nakajima
C      YR(IG)=RFI(IWLT+IG-1,IS,1)
C      YI(IG)=RFI(IWLT+IG-1,IS,2)
      YR(IG)=LOG( RFI(IWLT+IG-1,IS,1) )
      YI(IG)=LOG( RFI(IWLT+IG-1,IS,2) )
C

   71 CONTINUE
      PINT=LOG(WL1)

C96.7.19 T.Y.Nakajima
C      CR01=BINTP(PINT,NINTP,X,YR)
C      CI01=BINTP(PINT,NINTP,X,YI)
      CR01=EXP( BINTP(PINT,NINTP,X,YR) )
      CI01=EXP( BINTP(PINT,NINTP,X,YI) )
C

CC 96.2.29
CC        CR01=RFI(IWL,IS,1)+(RFI(IWL1,IS,1)-RFI(IWL,IS,1))*(DWL-IWL)
CC        CI01=RFI(IWL,IS,2)+(RFI(IWL1,IS,2)-RFI(IWL,IS,2))*(DWL-IWL)
C dry aerosol refractive index:
        CR0=CR0+CR01*RFRACP(ISP,MP)
        CI0=CI0+CI01*RFRACP(ISP,MP)
      endif
    4 continue
      IF(CR0.LE.0) then
        ERR='Particle species does not exist'
        return
      endif
C water refractive index:
      DO 72 IG=1,NINTP

C96.7.19 T.Y.Nakajima
C      YR(IG)=RFI(IWLT+IG-1,1,1)
C      YI(IG)=RFI(IWLT+IG-1,1,2)
      YR(IG)=LOG( RFI(IWLT+IG-1,1,1) )
      YI(IG)=LOG( RFI(IWLT+IG-1,1,2) )
C

   72 CONTINUE
      PINT=LOG(WL1)

C96.7.19 T.Y.Nakajima
C      CRW1=BINTP(PINT,NINTP,X,YR)
C      CIW1=BINTP(PINT,NINTP,X,YI)
      CRW1=EXP( BINTP(PINT,NINTP,X,YR) )
      CIW1=EXP( BINTP(PINT,NINTP,X,YI) )
C

CC 96.2.29
CC      CRW1=RFI(IWL,1,1)+(RFI(IWL1,1,1)-RFI(IWL,1,1))*(DWL-IWL)
CC      CIW1=RFI(IWL,1,2)+(RFI(IWL1,1,2)-RFI(IWL,1,2))*(DWL-IWL)
C     wet aerosol refractive index:
      CR1=CRW1+(CR0-CRW1)*(RMD/RMW)**3
      CI1=CIW1+(CI0-CIW1)*(RMD/RMW)**3
C Get optical parameters
      CALL GTPH4B(IUK,WLC,CR1,CI1,PR,X0,GG,RP
     & ,NANG,IPOL,ANG,PH,CEXT,CABS,CG,VL,INTVL,SZP,ERR)
      IF(ERR.NE.' ') return
      CEXTP=CEXTP+CEXT
      CABSP=CABSP+CABS
      VOLP =VOLP +VL
      DO 8 I=1,KNANG
    8 PHP(I)=PHP(I)+PH(I,1)
CC total dry volume
      PR(5,1)=DRYAP(5,M,MP)
      PR(3,1)=DRYAP(3,M,MP)
      CALL GETV(WLC,PR,INTVL,SZP,VL)
      VOLDP=VOLDP+VL
    5 continue
      RETURN
      END

      SUBROUTINE GETV(WLC,PR,INTVL,SZP,VL)
C Get total volume
C--- HISTORY
C 95. 9.14   Created from
C--- INPUT
C IUK     I      READ UNIT NUMBER OF THE KERNEL FILE.
C WLC     R      WAVELENGTH IN CM
C PR   R(6,4)    Parameter packet of size distribution (see VLSPC2)
C--- OUTPUT
C VL     R       Volume (cm3/cm3)
C---
      parameter (PI=3.141592653)
      dimension SZP(*),PR(6,4)
      NAV=10
      WVN=2*PI/WLC
	IF(SZP(INTVL)/SZP(1).LT.1.0E-30)THEN
		DEL=LOG(1.0E-30)/(INTVL-1)
	ELSE
		DEL=LOG(SZP(INTVL)/SZP(1))/(INTVL-1)
	ENDIF
      RSZP2=EXP(DEL/2)
      RMIN=PR(1,3)
      RMIN1=SZP(1)/WVN/RSZP2
      IF(RMIN1.GT.RMIN) then
		IF(RMIN1/RMIN.LT.1.0E-30)THEN
			INTVL9=LOG(1.0E-30)/DEL+1
		ELSE
			INTVL9=LOG(RMIN1/RMIN)/DEL+1
		ENDIF
        R2=RMIN1*EXP(-INTVL9*DEL)
       else
        INTVL9=0
      endif
      VL=0
      INTVL8=INTVL+INTVL9
      DO 30 IR8=1,INTVL8
      IR=IR8-INTVL9
      IF(IR.LE.0) THEN
        R1=R2
        R2=R1*RSZP2**2
       else
        R1=SZP(IR)/WVN/RSZP2
        R2=SZP(IR)/WVN*RSZP2
      endif
      DR=(R2-R1)/NAV
      R3=R1-DR/2
      DO 34 J=1,NAV
      R3=R3+DR
	IF(R3/(R3-DR).LT.1.0E-30)THEN
		DLR=LOG(1.0E-30)
	ELSE
		DLR=LOG(R3/(R3-DR))
	ENDIF
      PR(1,1)=R3
      V2=VLSPC2(PR)
   34 VL=VL+V2*DLR
   30 continue
      RETURN
      END
      FUNCTION   SUNIR(V)
C--- HISTORY
C 92. 4.24  REGISTERED FROM LOWTRAN-7
C--- INPUT
C V        R        WAVENUMBER (CM-1) 0 TO 57490 CM-1
C--- OUTPUT
C SUNIR    RF       THE EXTRA-TERRESTRIAL SOLAR IRRADIANCE (WATT/M2/MICRON)
C                   IF 0 THEN V IS OUT OF RANGE
C
C       USES  BLOCK DATA SOLAR   WHICH CONTAINS THE VALUES FOR SOLARA +
C
      COMMON /SUNDAT/ SOLARA(1440), SOLARB(2910)
      DATA  A, B / 3.50187E-13, 3.93281 /
C
C       WM, W0, W1, W2  ARE STATEMENT FUNCTIONS USED BY
C            THE 4 POINT LAGRANGE INTERPOLATION
      WM(P) = P*(P - 1)*(P - 2)
      W0(P) = 3*(P**2 - 1)*(P - 2)
      W1(P) = 3*P*(P + 1)*(P - 2)
      W2(P) = P*(P**2 - 1)
C
C           IF  V  IS TOO SMALL,  WRITE WARNING  +  RETURN SUNIR = 0
      IF(V .LT. 0.0) THEN
        SUNIR = 0.0
        WRITE(6, 900) V
        RETURN
C
      ELSEIF( V .GE. 0.0  .AND.  V .LT. 100.0 ) THEN
C         FOR LOW FREQUENCIES USE A POWER LAW APPROXIMATION
        SUNIR = A*V**B
        RETURN
C
      ELSEIF( V .GE. 100.0  .AND.  V .LT. 28420.0 ) THEN
C         USE  4 POINT INTERPOLATION  ON  ARRAY  SOLARA
C               WHICH IS AT  20 CM-1  SPACING  FROM 0 TO 28720 CM-1
        I = 1 + INT(V/20.0)
        P = MOD(V, 20.0)/20.0
        SUNIR = ( W2(P)*SOLARA(I+2) - W1(P)*SOLARA(I+1) +
     +              W0(P)*SOLARA(I) - WM(P)*SOLARA(I-1) ) / 6
        RETURN
C
      ELSEIF( V .GE. 28420.0  .AND.  V .LE. 57470.0 ) THEN
C         USE  4 POINT INTERPOLATION  ON  ARRAY  SOLARB
C             WHICH IS AT  10 CM-1  SPACING  FROM 28400 TO 57490 CM-1
        I = INT(V/10.0) - 2839
        P = MOD(V, 10.0)/10.0
        SUNIR = ( W2(P)*SOLARB(I+2) - W1(P)*SOLARB(I+1) +
     +              W0(P)*SOLARB(I) - WM(P)*SOLARB(I-1) ) / 6
        RETURN
C
      ELSEIF( V .GT. 57470.0 ) THEN
C           IF  V  IS TOO LARGE,  WRITE WARNING  +  RETURN SUNIR = 0
        SUNIR = 0.0
        WRITE(6, 900) V
        RETURN
C
      ENDIF
C
      RETURN
  900 FORMAT('0 *****  WARNING - INPUT FREQUENCY = ', 1PG12.5, 'CM-1',
     +  /, '   OUTSIDE VALID RANGE OF 0 TO 57470 CM-1    *******', / )
      END
      BLOCK DATA SOLAR
C>    BLOCK DATA
C
C
C     COMMON /SUNDAT/   SOLARA(1440), SOLARB(2910)
      COMMON /SUNDAT/   SUNA01( 41),SUNA02(144),SUNA03(144),SUNA04(144),
     A      SUNA05(144),SUNA06(144),SUNA07(144),SUNA08(144),SUNA09(144),
     B      SUNA10(144),SUNA11(103),SUNB01(144),SUNB02(144),SUNB03(144),
     C      SUNB04(144),SUNB05(144),SUNB06(144),SUNB07(144),SUNB08(144),
     D      SUNB09(144),SUNB10(144),SUNB11(144),SUNB12(144),SUNB13(144),
     E      SUNB14(144),SUNB15(144),SUNB16(144),SUNB17(144),SUNB18(144),
     F      SUNB19(144),SUNB20(144),SUNB21( 30)
C
C         SOLAR SPECTRUM FROM      0 TO    800 CM-1,  IN STEPS OF 20 CM-
      DATA SUNA01  /
     A 0.0000E+00, 4.5756E-08, 7.0100E-07, 3.4580E-06, 1.0728E-05,
     B 2.5700E-05, 5.2496E-05, 9.6003E-05, 1.6193E-04, 2.5766E-04,
     C 3.9100E-04, 5.6923E-04, 8.0203E-04, 1.1006E-03, 1.4768E-03,
     D 1.9460E-03, 2.5213E-03, 3.2155E-03, 4.0438E-03, 5.0229E-03,
     E 6.1700E-03, 7.5145E-03, 9.0684E-03, 1.0853E-02, 1.2889E-02,
     F 1.5213E-02, 1.7762E-02, 2.0636E-02, 2.3888E-02, 2.7524E-02,
     G 3.1539E-02, 3.5963E-02, 4.0852E-02, 4.6236E-02, 5.2126E-02,
     H 5.8537E-02, 6.5490E-02, 7.3017E-02, 8.1169E-02, 9.0001E-02,
     I 9.9540E-02 /
C         SOLAR SPECTRUM FROM    820 TO   3680 CM-1,  IN STEPS OF 20 CM-
      DATA SUNA02  /
     A .10980, .12080, .13260, .14520, .15860, .17310, .18850, .20490,
     B .22240, .24110, .26090, .28200, .30430, .32790, .35270, .37890,
     C .40650, .43550, .46600, .49800, .53160, .56690, .60390, .64260,
     D .68320, .72560, .76990, .81620, .86440, .91470, .96710, 1.0220,
     E 1.0780, 1.1370, 1.1990, 1.2630, 1.3290, 1.3990, 1.4710, 1.5460,
     F 1.6250, 1.7060, 1.7910, 1.8800, 1.9710, 2.0670, 2.1660, 2.2680,
     G 2.3740, 2.4840, 2.5970, 2.7140, 2.8350, 2.9600, 3.0890, 3.2210,
     H 3.3570, 3.4980, 3.6420, 3.7900, 3.9440, 4.1040, 4.2730, 4.4450,
     I 4.6150, 4.7910, 4.9830, 5.1950, 5.4210, 5.6560, 5.8930, 6.1270,
     J 6.3560, 6.5820, 6.8080, 7.0360, 7.2700, 7.5170, 7.7890, 8.0910,
     K 8.4070, 8.7120, 8.9900, 9.2490, 9.5000, 9.7550, 10.010, 10.250,
     L 10.480, 10.700, 10.950, 11.230, 11.550, 11.900, 12.250, 12.600,
     M 12.930, 13.250, 13.530, 13.780, 14.040, 14.320, 14.660, 15.070,
     N 15.530, 16.011, 16.433, 16.771, 17.077, 17.473, 17.964, 18.428,
     O 18.726, 18.906, 19.141, 19.485, 19.837, 20.160, 20.509, 21.024,
     P 21.766, 22.568, 23.190, 23.577, 23.904, 24.335, 24.826, 25.236,
     Q 25.650, 26.312, 27.208, 27.980, 28.418, 28.818, 29.565, 30.533,
     R 31.247, 31.667, 32.221, 33.089, 33.975, 34.597, 35.004, 35.395 /
C         SOLAR SPECTRUM FROM   3700 TO   6560 CM-1,  IN STEPS OF 20 CM-
      DATA SUNA03  /
     A 36.026, 36.985, 37.890, 38.401, 38.894, 39.857, 40.926, 41.570,
     B 42.135, 43.083, 44.352, 45.520, 45.982, 46.281, 48.335, 51.987,
     C 54.367, 54.076, 52.174, 50.708, 52.153, 55.707, 56.549, 54.406,
     D 53.267, 56.084, 61.974, 64.406, 60.648, 55.146, 53.067, 57.476,
     E 64.645, 68.348, 69.055, 69.869, 70.943, 71.662, 72.769, 74.326,
     F 75.257, 74.883, 73.610, 73.210, 74.886, 78.042, 80.204, 80.876,
     G 82.668, 84.978, 86.244, 88.361, 91.998, 95.383, 98.121, 100.29,
     H 100.64, 99.997, 101.82, 105.06, 107.50, 109.99, 112.45, 113.90,
     I 113.79, 119.23, 121.96, 124.58, 127.14, 125.19, 124.37, 125.00,
     J 127.88, 130.67, 131.98, 133.74, 136.69, 136.18, 135.02, 137.44,
     K 138.44, 137.25, 136.35, 142.60, 144.54, 148.37, 151.90, 151.55,
     L 155.35, 157.59, 159.70, 162.28, 168.44, 171.43, 169.82, 170.33,
     M 172.28, 176.68, 181.92, 186.06, 187.85, 186.00, 189.82, 189.35,
     N 192.86, 202.00, 209.63, 205.76, 212.88, 215.63, 216.51, 219.20,
     O 220.29, 221.12, 227.12, 229.97, 233.23, 233.95, 234.52, 234.45,
     P 235.77, 239.80, 243.11, 241.19, 242.34, 243.69, 242.84, 246.19,
     Q 246.11, 246.76, 251.75, 255.38, 258.74, 260.26, 263.40, 268.68,
     R 271.81, 272.95, 273.93, 274.74, 274.43, 279.69, 287.76, 287.72 /
C         SOLAR SPECTRUM FROM   6580 TO   9440 CM-1,  IN STEPS OF 20 CM-
      DATA SUNA04  /
     A 287.96, 290.01, 291.92, 295.28, 296.78, 300.46, 302.19, 299.14,
     B 301.43, 305.68, 309.29, 310.63, 313.24, 314.61, 309.58, 318.81,
     C 320.54, 321.62, 328.58, 331.66, 337.20, 345.62, 345.54, 342.96,
     D 344.38, 346.23, 349.17, 351.79, 354.71, 356.97, 358.29, 362.29,
     E 364.15, 364.97, 367.81, 368.98, 369.07, 372.17, 377.79, 381.25,
     F 384.22, 388.66, 393.58, 396.98, 398.72, 400.61, 404.06, 408.23,
     G 412.47, 415.58, 416.17, 416.53, 419.55, 425.88, 433.30, 437.73,
     H 438.13, 439.79, 441.51, 438.71, 434.25, 437.54, 448.95, 448.86,
     I 439.46, 437.10, 439.34, 444.33, 455.00, 467.05, 473.04, 469.64,
     J 467.53, 473.78, 477.50, 477.50, 480.96, 483.94, 482.19, 479.08,
     K 482.09, 493.43, 498.40, 492.05, 489.53, 493.34, 495.51, 496.52,
     L 499.57, 504.65, 509.68, 512.00, 512.05, 512.31, 515.00, 520.70,
     M 527.30, 531.88, 532.16, 530.48, 532.33, 539.26, 548.57, 553.00,
     N 548.96, 546.05, 551.00, 556.41, 557.21, 557.85, 560.95, 564.02,
     O 565.57, 566.38, 567.88, 571.48, 576.68, 581.54, 586.51, 593.62,
     P 600.70, 602.79, 601.39, 603.00, 606.88, 605.95, 600.97, 600.79,
     Q 607.21, 612.87, 614.13, 614.39, 616.61, 620.53, 625.19, 629.78,
     R 633.79, 637.31, 640.47, 642.53, 642.62, 641.93, 643.11, 646.68 /
C         SOLAR SPECTRUM FROM   9460 TO  12320 CM-1,  IN STEPS OF 20 CM-
       DATA SUNA05  /
     A 650.57, 654.30, 660.95, 672.10, 682.31, 684.89, 682.20, 682.53,
     B 687.79, 691.42, 689.62, 688.14, 693.71, 703.25, 708.07, 706.22,
     C 704.64, 708.97, 717.35, 725.43, 731.08, 734.17, 735.41, 736.60,
     D 739.34, 742.90, 745.04, 744.29, 742.44, 749.53, 755.70, 758.82,
     E 766.31, 761.53, 762.09, 769.68, 764.18, 763.75, 768.88, 762.69,
     F 753.93, 762.38, 765.79, 772.19, 760.67, 762.10, 766.76, 766.98,
     G 769.35, 773.50, 766.84, 763.60, 773.82, 777.18, 779.61, 792.48,
     H 797.54, 787.81, 793.75, 805.96, 804.77, 806.62, 821.72, 830.28,
     I 827.54, 831.06, 830.20, 826.22, 823.28, 822.18, 833.92, 854.58,
     J 859.80, 862.56, 871.16, 875.16, 867.67, 863.87, 883.30, 893.40,
     K 897.74, 905.24, 905.38, 911.07, 930.21, 939.24, 934.74, 935.15,
     L 942.38, 948.13, 947.00, 951.88, 960.12, 951.88, 954.22, 959.07,
     M 963.36, 980.16, 983.66, 978.76, 979.38, 985.24, 977.08, 919.94,
     N 899.68, 962.91, 997.17, 999.93, 995.65, 999.93, 1014.9, 951.57,
     O 893.52, 955.14, 1003.1, 990.13, 978.79, 1011.2, 1034.7, 1031.9,
     P 1029.9, 1039.7, 1045.5, 1044.1, 1049.6, 1056.1, 1049.8, 1038.0,
     Q 1051.9, 1072.2, 1075.5, 1077.0, 1079.3, 1078.0, 1075.7, 1079.7,
     R 1081.0, 1069.8, 1078.4, 1104.3, 1111.4, 1111.7, 1117.6, 1119.6 /
C         SOLAR SPECTRUM FROM  12340 TO  15200 CM-1,  IN STEPS OF 20 CM-
       DATA SUNA06  /
     A 1109.3, 1100.6, 1112.9, 1122.7, 1119.5, 1123.9, 1136.1, 1143.7,
     B 1140.5, 1141.2, 1151.5, 1148.7, 1138.3, 1141.0, 1150.6, 1160.1,
     C 1170.6, 1177.7, 1179.8, 1181.7, 1182.4, 1179.8, 1181.8, 1188.3,
     D 1190.0, 1191.4, 1197.0, 1196.0, 1192.2, 1200.6, 1210.4, 1209.1,
     E 1207.5, 1205.3, 1193.3, 1192.9, 1220.0, 1243.3, 1245.4, 1241.5,
     F 1240.2, 1241.1, 1244.0, 1248.5, 1253.2, 1257.1, 1259.9, 1261.9,
     G 1263.6, 1265.7, 1269.6, 1277.0, 1284.2, 1284.4, 1282.7, 1287.2,
     H 1286.8, 1272.3, 1262.2, 1270.7, 1288.8, 1304.8, 1311.8, 1312.2,
     I 1314.4, 1320.2, 1326.2, 1328.4, 1325.3, 1322.5, 1325.4, 1334.6,
     J 1346.4, 1354.0, 1353.7, 1347.3, 1338.3, 1331.0, 1329.7, 1338.0,
     K 1351.9, 1363.0, 1368.8, 1372.0, 1375.9, 1382.1, 1387.8, 1388.8,
     L 1388.2, 1392.2, 1401.7, 1412.9, 1418.2, 1410.7, 1395.9, 1385.7,
     M 1388.1, 1405.0, 1424.0, 1428.1, 1422.2, 1423.6, 1434.5, 1445.2,
     N 1450.7, 1451.8, 1451.5, 1453.9, 1459.9, 1466.9, 1471.3, 1469.4,
     O 1462.5, 1460.4, 1468.9, 1481.8, 1490.8, 1495.3, 1497.9, 1500.7,
     P 1505.2, 1510.0, 1512.3, 1512.7, 1515.6, 1521.6, 1524.2, 1520.7,
     Q 1520.3, 1531.6, 1545.7, 1548.2, 1541.7, 1542.2, 1553.6, 1563.6,
     R 1563.6, 1559.9, 1561.3, 1569.9, 1581.6, 1577.6, 1529.7, 1447.0 /
C         SOLAR SPECTRUM FROM  15220 TO  18080 CM-1,  IN STEPS OF 20 CM-
      DATA SUNA07  /
     A 1396.9, 1428.7, 1506.4, 1567.1, 1594.0, 1606.1, 1613.5, 1609.0,
     B 1588.6, 1567.8, 1567.3, 1587.2, 1610.2, 1624.4, 1630.2, 1630.9,
     C 1628.1, 1622.3, 1616.9, 1618.9, 1631.6, 1648.1, 1658.2, 1659.7,
     D 1658.1, 1658.0, 1659.4, 1660.4, 1659.2, 1653.7, 1645.3, 1642.1,
     E 1652.7, 1674.2, 1694.1, 1700.6, 1703.4, 1697.6, 1654.5, 1644.4,
     F 1661.6, 1676.3, 1707.7, 1703.1, 1710.8, 1732.3, 1716.5, 1719.6,
     G 1729.6, 1683.1, 1628.5, 1683.5, 1727.0, 1707.8, 1689.4, 1698.4,
     H 1733.1, 1737.8, 1714.1, 1734.6, 1750.1, 1750.1, 1760.3, 1764.3,
     I 1765.3, 1769.4, 1779.9, 1793.0, 1765.1, 1729.4, 1745.9, 1753.4,
     J 1758.1, 1775.0, 1768.4, 1767.9, 1789.5, 1806.6, 1799.3, 1782.6,
     K 1779.3, 1792.1, 1809.7, 1808.0, 1794.4, 1818.6, 1774.2, 1648.5,
     L 1674.3, 1789.3, 1847.2, 1848.3, 1812.9, 1796.4, 1840.3, 1868.3,
     M 1864.6, 1873.2, 1872.2, 1856.0, 1845.0, 1842.4, 1823.9, 1795.1,
     N 1819.6, 1861.5, 1857.7, 1838.6, 1840.5, 1863.5, 1876.8, 1884.4,
     O 1894.9, 1875.2, 1821.2, 1779.4, 1810.2, 1855.3, 1831.8, 1837.3,
     P 1882.3, 1866.4, 1819.6, 1804.8, 1831.4, 1861.6, 1867.1, 1862.9,
     Q 1851.9, 1834.7, 1835.2, 1845.1, 1831.9, 1803.6, 1792.5, 1821.8,
     R 1845.8, 1832.3, 1847.6, 1894.2, 1909.2, 1901.0, 1891.2, 1869.9 /
C         SOLAR SPECTRUM FROM  18100 TO  20960 CM-1,  IN STEPS OF 20 CM-
      DATA SUNA08  /
     A 1854.4, 1865.8, 1873.7, 1868.8, 1881.7, 1897.1, 1884.2, 1856.2,
     B 1840.6, 1855.1, 1885.3, 1903.6, 1900.1, 1887.4, 1887.7, 1879.0,
     C 1844.5, 1844.1, 1877.1, 1847.3, 1785.1, 1792.6, 1848.7, 1894.4,
     D 1908.8, 1892.8, 1867.4, 1885.6, 1959.9, 1971.9, 1895.8, 1883.5,
     E 1917.6, 1853.8, 1793.0, 1875.6, 1974.0, 1975.7, 1943.9, 1926.4,
     F 1914.4, 1902.7, 1882.5, 1813.3, 1710.8, 1717.9, 1859.7, 1965.1,
     G 1970.1, 1941.4, 1902.5, 1852.0, 1836.3, 1879.3, 1901.6, 1862.9,
     H 1839.1, 1840.9, 1780.0, 1684.9, 1677.3, 1718.7, 1697.3, 1684.3,
     I 1784.5, 1898.0, 1910.3, 1877.2, 1866.6, 1862.6, 1860.3, 1899.7,
     J 1971.0, 1999.9, 1970.9, 1936.5, 1922.8, 1922.8, 1924.0, 1917.2,
     K 1912.0, 1926.2, 1959.7, 1995.4, 1995.9, 1938.8, 1883.5, 1894.7,
     L 1933.3, 1935.1, 1899.3, 1852.7, 1820.2, 1821.5, 1865.2, 1935.5,
     M 1966.1, 1919.6, 1881.2, 1931.5, 2015.6, 2050.0, 2021.4, 1960.8,
     N 1938.2, 1997.0, 2051.0, 2003.4, 1912.1, 1880.2, 1895.2, 1898.0,
     O 1898.8, 1938.3, 1994.2, 2010.0, 1982.4, 1948.8, 1927.3, 1911.6,
     P 1877.7, 1791.6, 1679.8, 1645.0, 1727.3, 1845.2, 1926.2, 1973.4,
     Q 2005.2, 2021.6, 2021.8, 2025.7, 2054.3, 2086.5, 2082.6, 2052.9,
     R 2047.1, 2070.2, 2072.4, 2038.1, 2020.2, 2049.9, 2074.0, 2038.1 /
C         SOLAR SPECTRUM FROM  20980 TO  23840 CM-1,  IN STEPS OF 20 CM-
       DATA SUNA09  /
     A 1978.6, 1963.5, 1996.8, 2037.5, 2057.5, 2048.2, 2018.4, 1999.2,
     B 2011.4, 2039.5, 2056.0, 2040.2, 1981.8, 1911.4, 1891.8, 1938.3,
     C 1991.7, 2005.5, 2000.8, 2011.3, 2022.7, 1997.5, 1947.7, 1936.3,
     D 1986.6, 2037.9, 2032.8, 1995.7, 1984.0, 2012.0, 2055.5, 2091.6,
     E 2106.5, 2094.9, 2070.4, 2052.8, 2046.7, 2043.8, 2035.5, 2016.6,
     F 1988.4, 1973.3, 1999.0, 2057.4, 2103.8, 2109.4, 2089.4, 2068.5,
     G 2051.8, 2031.2, 2005.9, 1986.7, 1981.5, 1979.4, 1964.1, 1943.6,
     H 1951.8, 2007.3, 2083.2, 2139.1, 2158.0, 2143.3, 2103.2, 2050.9,
     I 2001.9, 1974.5, 1988.0, 2037.8, 2075.1, 2050.6, 1971.5, 1884.5,
     J 1828.5, 1820.9, 1866.4, 1935.3, 1974.2, 1958.7, 1925.1, 1920.2,
     K 1949.7, 1984.6, 1996.4, 1966.4, 1884.8, 1781.9, 1726.8, 1759.4,
     L 1817.4, 1800.4, 1692.6, 1593.2, 1598.6, 1700.3, 1823.8, 1909.7,
     M 1937.7, 1902.5, 1822.4, 1737.8, 1683.2, 1666.8, 1682.7, 1715.3,
     N 1734.1, 1712.4, 1668.2, 1655.0, 1698.1, 1727.2, 1636.9, 1415.7,
     O 1204.2, 1155.8, 1278.4, 1450.0, 1560.5, 1595.1, 1587.8, 1570.6,
     P 1565.8, 1590.3, 1640.5, 1688.4, 1708.1, 1703.6, 1700.7, 1718.5,
     Q 1749.0, 1772.2, 1772.5, 1745.2, 1690.2, 1624.9, 1589.0, 1618.5,
     R 1701.3, 1783.2, 1816.4, 1800.7, 1765.0, 1734.1, 1714.6, 1705.0 /
C         SOLAR SPECTRUM FROM  23860 TO  26720 CM-1,  IN STEPS OF 20 CM-
       DATA SUNA10  /
     A 1701.6, 1696.6, 1682.0, 1661.4, 1657.2, 1693.0, 1763.2, 1826.5,
     B 1841.6, 1806.1, 1755.6, 1725.8, 1724.2, 1736.8, 1749.0, 1756.1,
     C 1759.5, 1762.1, 1770.2, 1791.7, 1826.8, 1848.9, 1819.6, 1720.7,
     D 1595.5, 1513.9, 1522.5, 1602.0, 1706.2, 1793.4, 1837.9, 1820.3,
     E 1738.3, 1631.1, 1553.1, 1539.2, 1574.3, 1623.9, 1660.6, 1676.8,
     F 1673.1, 1652.9, 1626.4, 1606.7, 1604.2, 1620.9, 1654.5, 1701.2,
     G 1752.2, 1796.2, 1822.8, 1827.4, 1808.5, 1767.0, 1713.9, 1667.3,
     H 1643.7, 1643.5, 1652.5, 1655.3, 1638.7, 1592.2, 1506.4, 1377.3,
     I 1209.5, 1010.5, 807.59, 666.84, 664.53, 835.23, 1099.6, 1330.7,
     J 1423.2, 1363.7, 1194.1, 961.77, 725.04, 551.29, 504.01, 596.30,
     K 775.15, 975.62, 1150.2, 1287.2, 1386.1, 1447.5, 1473.7, 1468.5,
     L 1435.2, 1376.9, 1296.0, 1195.5, 1085.3, 985.40, 917.25, 894.59,
     M 910.86, 951.53, 1001.7, 1046.4, 1070.7, 1061.2, 1021.2, 977.16,
     N 959.15, 982.06, 1020.5, 1032.6, 983.44, 879.83, 762.66, 675.28,
     O 643.33, 662.65, 721.49, 808.35, 913.24, 1027.0, 1139.9, 1236.2,
     P 1293.2, 1287.1, 1210.4, 1102.1, 1021.6, 1022.8, 1109.3, 1232.6,
     Q 1337.0, 1383.1, 1372.8, 1324.7, 1257.7, 1188.8, 1133.5, 1106.5,
     R 1113.7, 1136.8, 1147.9, 1121.4, 1054.1, 968.10, 889.19, 837.87 /
C         SOLAR SPECTRUM FROM  26740 TO  28780 CM-1,  IN STEPS OF 20 CM-
      DATA SUNA11  /
     A 817.64, 823.72, 851.04, 896.53, 959.85, 1041.2, 1137.6, 1231.2,
     B 1294.4, 1299.9, 1241.2, 1155.0, 1092.0, 1097.1, 1170.2, 1263.5,
     C 1322.4, 1307.4, 1233.6, 1146.1, 1090.8, 1092.5, 1134.6, 1188.9,
     D 1228.9, 1245.5, 1248.5, 1250.3, 1260.5, 1274.6, 1279.5, 1261.8,
     E 1214.3, 1145.4, 1069.6, 1001.4, 952.52, 930.48, 941.68, 990.34,
     F 1064.4, 1135.2, 1171.5, 1149.1, 1076.3, 984.35, 906.25, 868.17,
     G 873.75, 915.33, 984.41, 1067.2, 1137.1, 1163.1, 1115.5, 990.55,
     H 830.93, 692.29, 627.44, 654.10, 739.24, 838.88, 911.69, 941.90,
     I 944.42, 939.58, 946.10, 970.23, 1005.2, 1042.4, 1073.8, 1097.0,
     J 1114.3, 1128.8, 1142.9, 1153.4, 1152.4, 1131.5, 1084.2, 1016.7,
     K 945.95, 890.37, 866.15, 876.54, 913.13, 966.10, 1025.4, 1080.2,
     L 1119.0, 1102.7, 1243.5, 1209.9, 1079.2, 852.20, 956.80, 842.31,
     M 897.44, 1081.8, 914.23, 993.09, 1049.8, 844.95, 839.16/
C         SOLAR SPECTRUM FROM  28400 TO  29830 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB01  /
     A 876.54, 892.17, 913.13, 938.18, 966.10, 995.62, 1025.4, 1054.1,
     B 1080.2, 1102.1, 1119.0, 1132.2, 1102.7, 1159.3, 1243.5, 1238.3,
     C 1209.9, 1196.2, 1079.2, 895.60, 852.20, 935.59, 956.80, 897.09,
     D 842.31, 821.15, 897.44, 1042.7, 1081.8, 988.79, 914.23, 929.38,
     E 993.09, 1041.9, 1049.8, 984.33, 844.95, 770.76, 839.16, 939.65,
     F 1026.1, 1121.1, 1162.6, 1142.6, 1077.9, 1027.3, 1078.2, 1094.3,
     G 969.83, 853.72, 849.91, 909.12, 995.68, 1095.0, 1146.9, 1086.3,
     H 1010.4, 1065.4, 1128.9, 1080.6, 987.93, 898.18, 835.20, 771.63,
     I 687.12, 614.52, 606.14, 737.09, 908.13, 997.64, 1080.6, 1126.3,
     J 1056.7, 1028.4, 1141.7, 1252.6, 1225.3, 1103.2, 1038.6, 1043.4,
     K 1002.9, 965.51, 1035.0, 1150.7, 1200.9, 1152.0, 1068.5, 995.84,
     L 889.52, 818.48, 907.01, 1042.2, 1055.6, 1000.6, 972.00, 985.72,
     M 1027.2, 1054.8, 1078.0, 1126.6, 1205.3, 1245.7, 1201.0, 1144.7,
     N 1097.5, 1030.1, 926.85, 836.71, 864.11, 993.50, 1075.3, 1032.6,
     O 1008.9, 1066.1, 1067.4, 1004.8, 971.54, 923.18, 815.71, 799.70,
     P 946.19, 1100.1, 1126.4, 1032.2, 895.14, 784.30, 734.77, 726.53,
     Q 726.88, 765.54, 863.90, 992.24, 1070.9, 1028.1, 858.78, 647.15,
     R 563.18, 679.98, 906.40, 1094.3, 1155.3, 1124.3, 1098.4, 1109.5 /
C         SOLAR SPECTRUM FROM  29840 TO  31270 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB02  /
     A 1076.2, 944.17, 849.20, 928.54, 1062.0, 1118.9, 1119.2, 1074.6,
     B 1005.8, 980.02, 999.11, 1002.4, 939.78, 838.12, 816.13, 908.73,
     C 1014.9, 1058.3, 1043.7, 987.54, 946.35, 981.40, 1055.8, 1094.3,
     D 1028.3, 916.41, 908.99, 991.83, 1049.6, 1076.2, 1093.5, 1076.3,
     E 1014.5, 949.61, 947.26, 1001.2, 1051.5, 1072.8, 1068.0, 1012.5,
     F 907.81, 866.30, 950.89, 1037.5, 1079.5, 1183.9, 1291.3, 1268.6,
     G 1199.3, 1188.6, 1188.0, 1186.6, 1198.2, 1171.3, 1132.6, 1131.6,
     H 1096.0, 971.10, 847.07, 836.62, 922.78, 990.99, 987.51, 969.24,
     I 981.46, 981.36, 971.95, 985.34, 1003.0, 1037.2, 1071.2, 1065.7,
     J 1026.7, 984.84, 1002.7, 1070.3, 1117.5, 1116.0, 1048.9, 965.34,
     K 972.27, 1045.7, 1096.6, 1127.5, 1133.5, 1099.6, 1079.3, 1082.9,
     L 1026.8, 927.50, 879.08, 858.83, 831.01, 807.82, 789.56, 813.75,
     M 893.46, 937.62, 901.56, 864.46, 873.35, 891.03, 862.46, 810.30,
     N 787.36, 752.93, 715.34, 708.07, 728.93, 786.79, 807.73, 736.28,
     O 645.08, 616.90, 649.17, 691.77, 749.18, 820.21, 820.68, 791.26,
     P 854.27, 940.56, 956.38, 909.42, 824.18, 767.17, 722.06, 653.42,
     Q 624.67, 633.73, 655.14, 707.93, 784.94, 880.79, 961.15, 985.60,
     R 986.18, 966.53, 921.47, 888.89, 855.85, 851.66, 886.78, 850.97 /
C         SOLAR SPECTRUM FROM  31280 TO  32710 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB03  /
     A 766.97, 738.95, 724.53, 657.61, 587.77, 616.86, 760.61, 903.23,
     B 917.27, 838.49, 784.80, 759.41, 719.61, 671.48, 624.63, 588.57,
     C 574.70, 596.68, 698.02, 866.39, 974.82, 960.37, 930.10, 962.65,
     D 1007.1, 1001.9, 926.29, 816.64, 763.25, 772.93, 762.66, 729.39,
     E 725.01, 727.16, 672.73, 581.42, 520.97, 488.80, 478.60, 542.08,
     F 663.71, 749.48, 785.87, 811.05, 818.19, 813.80, 824.54, 836.62,
     G 799.66, 728.00, 660.36, 559.28, 473.28, 550.16, 752.04, 885.84,
     H 906.80, 912.21, 929.32, 899.72, 830.20, 774.56, 736.42, 724.09,
     I 740.12, 754.11, 764.96, 780.76, 788.94, 784.87, 758.80, 725.91,
     J 751.84, 804.24, 777.73, 703.36, 665.27, 663.99, 679.36, 706.09,
     K 757.57, 836.09, 880.02, 881.18, 907.91, 929.26, 894.32, 874.01,
     L 918.56, 953.50, 922.32, 866.61, 836.54, 825.28, 752.54, 586.02,
     M 427.46, 374.05, 437.23, 534.32, 556.74, 563.11, 629.31, 631.26,
     N 518.76, 438.31, 460.31, 530.45, 608.50, 657.99, 662.08, 686.17,
     O 775.18, 843.11, 797.46, 685.33, 611.33, 628.74, 711.36, 754.94,
     P 728.80, 722.79, 726.38, 679.68, 665.83, 710.48, 723.10, 724.09,
     Q 760.18, 784.01, 742.78, 634.33, 546.55, 563.54, 611.03, 623.16,
     R 665.36, 743.55, 764.46, 671.14, 513.18, 401.86, 405.77, 515.72 /
C         SOLAR SPECTRUM FROM  32720 TO  34150 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB04  /
     A 639.90, 677.85, 679.55, 759.33, 848.11, 819.89, 751.75, 710.50,
     B 615.33, 525.09, 583.35, 715.23, 767.53, 739.10, 664.05, 580.57,
     C 572.85, 634.13, 648.77, 561.27, 497.72, 591.71, 737.83, 794.19,
     D 802.51, 799.33, 735.79, 658.41, 659.47, 718.18, 761.67, 697.24,
     E 545.14, 474.47, 526.96, 597.65, 584.74, 447.28, 291.35, 261.28,
     F 330.26, 401.96, 466.32, 531.26, 572.34, 584.86, 585.17, 569.46,
     G 558.27, 559.41, 512.02, 426.37, 378.14, 398.26, 473.49, 542.18,
     H 531.76, 437.48, 341.85, 305.82, 299.88, 328.12, 440.04, 586.46,
     I 660.32, 625.22, 510.26, 418.85, 447.36, 534.89, 605.86, 667.07,
     J 687.31, 636.79, 549.63, 472.88, 419.53, 370.06, 327.98, 320.49,
     K 354.00, 399.17, 450.98, 528.34, 608.25, 696.07, 774.28, 760.75,
     L 690.58, 648.20, 580.63, 477.96, 453.91, 488.74, 464.02, 421.59,
     M 444.32, 446.59, 375.95, 342.13, 397.49, 510.97, 646.38, 725.14,
     N 703.06, 639.06, 619.10, 654.66, 665.99, 611.40, 580.22, 607.29,
     O 591.05, 542.30, 583.82, 673.02, 673.21, 582.44, 465.73, 377.25,
     P 377.04, 487.27, 607.93, 617.52, 583.46, 601.68, 615.94, 575.47,
     Q 541.63, 542.06, 522.28, 472.49, 423.29, 438.09, 556.72, 664.34,
     R 669.88, 657.45, 684.71, 705.70, 683.11, 600.81, 509.90, 497.64 /
C         SOLAR SPECTRUM FROM  34160 TO  35590 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB05  /
     A 511.07, 496.07, 500.32, 518.70, 529.91, 563.00, 609.20, 626.49,
     B 622.11, 615.72, 600.44, 591.26, 598.12, 593.07, 590.94, 631.58,
     C 696.48, 718.48, 676.11, 631.56, 619.64, 620.53, 624.10, 636.56,
     D 658.02, 688.78, 724.81, 742.60, 722.31, 675.86, 665.96, 704.73,
     E 703.70, 645.00, 598.26, 587.77, 590.94, 575.93, 528.03, 477.92,
     F 457.52, 456.80, 454.91, 448.65, 445.47, 445.38, 444.43, 446.04,
     G 455.91, 468.02, 454.34, 393.32, 301.22, 211.44, 167.11, 193.99,
     H 254.01, 305.35, 353.03, 385.08, 387.03, 391.60, 406.20, 415.34,
     I 435.34, 469.77, 492.15, 472.73, 409.86, 353.25, 340.68, 355.27,
     J 379.77, 401.81, 409.67, 406.89, 393.16, 378.89, 375.20, 373.52,
     K 360.19, 322.79, 273.55, 237.76, 212.33, 184.80, 156.20, 127.75,
     L 96.269, 68.806, 62.047, 77.143, 100.47, 127.56, 159.88, 194.05,
     M 225.20, 254.64, 285.75, 300.14, 294.40, 308.92, 340.83, 346.26,
     N 336.29, 347.54, 373.81, 388.78, 372.68, 325.29, 294.40, 317.56,
     O 360.30, 378.08, 374.22, 374.03, 383.34, 387.88, 377.55, 356.96,
     P 340.67, 328.71, 314.00, 316.91, 344.51, 355.54, 335.66, 318.68,
     Q 318.65, 322.43, 318.61, 304.92, 284.84, 268.13, 265.80, 273.55,
     R 274.18, 252.38, 215.04, 188.60, 181.31, 181.31, 180.78, 175.24 /
C         SOLAR SPECTRUM FROM  35600 TO  37030 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB06  /
     A 162.06, 145.08, 128.76, 113.76, 98.078, 83.072, 76.222, 78.359,
     B 78.434, 74.235, 75.843, 80.321, 77.859, 70.298, 64.651, 67.049,
     C 77.810, 83.167, 75.286, 71.202, 80.549, 92.008, 100.17, 108.63,
     D 119.44, 130.78, 142.31, 158.94, 177.12, 186.40, 186.60, 181.47,
     E 175.30, 175.54, 179.00, 177.04, 172.60, 172.67, 178.98, 193.77,
     F 215.13, 233.62, 252.05, 277.68, 298.91, 298.40, 280.81, 274.21,
     G 286.52, 285.46, 259.71, 241.39, 246.98, 259.87, 274.27, 298.47,
     H 316.85, 303.19, 263.69, 229.31, 227.90, 256.12, 281.58, 300.19,
     I 310.56, 279.54, 211.93, 152.18, 129.94, 147.47, 181.62, 215.37,
     J 239.50, 233.12, 191.55, 139.41, 110.51, 118.93, 134.79, 129.05,
     K 124.39, 143.53, 158.29, 141.84, 116.32, 111.59, 128.93, 149.17,
     L 153.44, 145.63, 148.52, 159.25, 155.84, 154.17, 177.28, 203.40,
     M 207.35, 205.27, 222.85, 253.18, 271.28, 279.27, 302.17, 321.47,
     N 288.83, 230.14, 206.40, 213.22, 216.49, 207.46, 196.20, 195.21,
     O 202.03, 194.33, 164.86, 136.65, 123.87, 128.14, 161.89, 216.99,
     P 253.68, 249.26, 222.89, 213.11, 243.64, 293.10, 309.42, 286.40,
     Q 269.61, 272.23, 271.67, 265.84, 265.61, 264.77, 266.03, 289.51,
     R 325.67, 337.34, 321.17, 300.30, 282.60, 287.14, 322.06, 335.79 /
C         SOLAR SPECTRUM FROM  37040 TO  38470 CM-1,  IN STEPS OF 10 CM-
       DATA SUNB07  /
     A 297.22, 254.10, 243.47, 239.49, 219.32, 211.94, 239.28, 271.43,
     B 279.37, 272.26, 264.77, 250.52, 229.93, 222.15, 235.30, 256.79,
     C 275.28, 286.92, 284.85, 269.52, 255.05, 253.46, 263.22, 274.78,
     D 279.19, 270.17, 249.41, 229.04, 221.64, 231.38, 252.70, 280.64,
     E 310.06, 328.33, 325.01, 290.26, 238.97, 223.38, 257.24, 282.60,
     F 264.32, 243.34, 253.18, 272.89, 271.32, 256.12, 260.24, 271.35,
     G 257.11, 236.61, 238.72, 248.92, 255.90, 272.04, 291.78, 297.40,
     H 288.09, 283.28, 292.92, 301.74, 309.07, 322.05, 320.42, 295.43,
     I 269.65, 254.41, 240.88, 228.18, 221.23, 213.72, 201.23, 197.17,
     J 212.29, 233.39, 247.65, 261.74, 286.17, 322.49, 349.47, 338.28,
     K 297.06, 261.55, 252.28, 264.65, 286.92, 298.94, 280.45, 244.37,
     L 213.47, 193.03, 182.07, 168.54, 143.12, 114.10, 89.615, 73.589,
     M 73.990, 87.912, 96.265, 94.813, 96.604, 102.30, 102.15, 103.07,
     N 117.81, 137.41, 146.09, 144.28, 137.89, 128.11, 122.82, 128.19,
     O 130.66, 117.31, 98.912, 93.397, 105.63, 122.73, 126.39, 113.05,
     P 92.317, 76.340, 69.032, 66.324, 71.280, 87.431, 105.94, 114.02,
     Q 107.91, 91.872, 75.208, 69.123, 75.930, 90.928, 109.71, 125.70,
     R 135.79, 141.14, 138.14, 121.33, 91.806, 63.497, 52.106, 59.555 /
C         SOLAR SPECTRUM FROM  38480 TO  39910 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB08  /
     A 81.015, 106.67, 118.97, 116.36, 110.82, 100.88, 89.056, 90.431,
     B 104.41, 114.95, 124.85, 148.87, 171.72, 167.22, 142.25, 118.42,
     C 98.653, 78.908, 68.133, 77.286, 100.93, 120.08, 125.49, 131.79,
     D 155.69, 180.75, 181.81, 166.77, 150.06, 133.24, 116.14, 97.728,
     E 81.629, 76.695, 87.607, 110.23, 134.88, 149.13, 147.64, 139.88,
     F 135.19, 135.07, 138.00, 136.73, 128.84, 122.22, 120.48, 121.98,
     G 123.08, 116.30, 101.43, 86.303, 74.719, 68.800, 71.327, 80.626,
     H 90.485, 96.739, 100.69, 100.81, 93.677, 84.740, 81.532, 82.893,
     I 84.564, 87.584, 91.780, 91.272, 87.014, 87.386, 90.149, 84.917,
     J 71.266, 57.873, 51.863, 53.876, 57.909, 58.508, 57.020, 57.432,
     K 60.671, 64.667, 67.362, 67.511, 64.233, 59.035, 55.697, 56.636,
     L 59.400, 59.070, 56.522, 55.834, 55.860, 54.039, 51.976, 52.344,
     M 54.667, 56.450, 56.751, 56.769, 58.002, 60.029, 59.602, 53.134,
     N 42.926, 35.588, 33.447, 35.171, 39.379, 44.371, 47.745, 46.933,
     O 42.441, 37.879, 35.595, 36.458, 41.048, 47.300, 51.098, 50.024,
     P 45.331, 41.282, 40.082, 40.000, 39.104, 37.329, 36.632, 37.792,
     Q 39.189, 41.058, 45.214, 50.737, 54.281, 55.015, 56.138, 60.931,
     R 67.383, 69.534, 65.159, 56.372, 47.326, 44.322, 49.944, 59.696 /
C         SOLAR SPECTRUM FROM  39920 TO  41350 CM-1,  IN STEPS OF 10 CM-
       DATA SUNB09  /
     A 67.929, 71.334, 69.905, 65.620, 59.303, 54.016, 55.880, 65.155,
     B 74.065, 76.217, 73.506, 71.406, 70.849, 69.749, 69.268, 71.380,
     C 72.721, 68.929, 61.665, 54.896, 47.420, 38.325, 32.219, 31.243,
     D 33.310, 35.358, 35.623, 36.840, 41.551, 47.499, 51.176, 50.344,
     E 45.362, 38.341, 33.130, 33.801, 40.140, 49.121, 55.385, 55.174,
     F 50.450, 46.511, 47.495, 51.883, 56.354, 59.603, 61.584, 63.215,
     G 64.603, 64.101, 59.027, 50.956, 47.633, 52.543, 58.883, 59.829,
     H 57.617, 56.727, 57.371, 57.898, 57.177, 55.129, 52.952, 52.018,
     I 52.186, 52.044, 50.269, 46.592, 42.515, 40.755, 41.887, 44.119,
     J 46.536, 48.858, 50.490, 51.919, 54.085, 54.707, 51.927, 49.449,
     K 49.865, 50.933, 50.496, 48.616, 46.717, 46.070, 46.263, 46.733,
     L 48.009, 50.187, 52.420, 53.536, 52.507, 51.380, 53.214, 56.985,
     M 60.614, 63.139, 63.999, 63.869, 65.100, 69.385, 74.743, 78.184,
     N 78.103, 74.113, 67.371, 60.849, 58.924, 62.682, 68.032, 69.117,
     O 64.604, 59.110, 55.998, 56.838, 61.778, 65.874, 65.079, 63.038,
     P 64.809, 69.911, 74.841, 76.439, 73.587, 68.853, 67.497, 72.675,
     Q 80.602, 83.422, 78.957, 72.228, 66.737, 62.842, 61.535, 63.574,
     R 69.248, 76.577, 79.922, 77.755, 73.938, 70.518, 68.003, 66.339 /
C         SOLAR SPECTRUM FROM  41360 TO  42790 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB10  /
     A 63.979, 61.098, 59.421, 58.103, 55.741, 52.549, 48.079, 42.578,
     B 38.373, 37.297, 37.455, 34.861, 30.483, 29.634, 34.734, 42.460,
     C 47.066, 45.848, 40.157, 34.290, 31.584, 30.650, 29.054, 27.788,
     D 30.427, 37.570, 44.196, 46.880, 47.848, 49.166, 49.180, 45.002,
     E 38.135, 35.055, 38.095, 41.750, 40.899, 35.722, 28.884, 24.835,
     F 28.670, 39.646, 50.310, 55.725, 57.401, 58.110, 59.406, 59.360,
     G 53.420, 43.004, 34.787, 33.697, 39.682, 47.554, 52.605, 53.632,
     H 51.001, 45.266, 37.844, 31.030, 25.936, 22.799, 21.882, 23.484,
     I 27.857, 33.447, 37.319, 39.195, 42.826, 50.398, 58.752, 63.301,
     J 61.094, 53.532, 46.046, 41.118, 37.646, 36.304, 40.426, 50.893,
     K 61.553, 65.395, 62.680, 58.087, 54.622, 51.330, 46.874, 42.870,
     L 40.547, 39.760, 40.217, 40.359, 39.559, 40.667, 46.260, 53.413,
     M 56.041, 52.566, 46.674, 41.073, 35.511, 31.231, 31.082, 35.955,
     N 45.199, 55.464, 61.802, 63.505, 61.850, 56.412, 49.388, 46.369,
     O 50.058, 56.694, 60.884, 61.030, 58.107, 54.303, 51.940, 50.508,
     P 46.749, 39.155, 31.535, 28.959, 30.973, 32.670, 31.567, 29.340,
     Q 27.275, 25.184, 24.264, 27.068, 34.296, 42.475, 47.230, 47.425,
     R 44.435, 40.538, 36.868, 33.020, 29.405, 28.753, 34.079, 44.246 /
C         SOLAR SPECTRUM FROM  42800 TO  44230 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB11  /
     A 53.780, 57.974, 56.376, 51.200, 45.308, 40.273, 35.900, 33.344,
     B 34.011, 36.858, 41.283, 47.374, 53.088, 56.201, 55.633, 50.843,
     C 43.997, 38.767, 36.248, 36.380, 40.762, 50.700, 63.371, 73.432,
     D 76.418, 70.373, 58.741, 47.034, 38.598, 34.664, 35.794, 42.084,
     E 49.973, 54.338, 53.956, 52.287, 52.778, 55.571, 59.034, 60.268,
     F 56.247, 47.362, 38.056, 32.889, 31.739, 31.734, 32.476, 35.060,
     G 39.091, 43.398, 48.131, 53.574, 58.749, 63.599, 68.971, 73.421,
     H 73.861, 69.003, 60.557, 51.865, 44.879, 42.060, 44.802, 47.950,
     I 46.882, 42.973, 39.293, 37.711, 37.137, 35.222, 32.243, 30.488,
     J 32.605, 40.429, 51.099, 57.710, 57.150, 52.992, 50.275, 49.986,
     K 49.778, 48.371, 46.421, 44.604, 42.730, 41.244, 41.565, 43.805,
     L 47.013, 48.992, 46.428, 40.595, 37.840, 42.353, 52.248, 60.529,
     M 61.566, 56.800, 52.041, 52.260, 57.077, 61.019, 60.712, 57.048,
     N 51.481, 46.352, 44.366, 44.947, 45.478, 44.944, 43.825, 42.105,
     O 39.466, 36.826, 35.907, 36.357, 35.661, 33.947, 33.690, 34.429,
     P 34.000, 32.645, 31.410, 30.281, 29.409, 29.127, 29.326, 29.869,
     Q 30.601, 31.311, 32.099, 32.779, 32.757, 32.098, 31.975, 33.484,
     R 36.048, 39.169, 43.365, 47.244, 48.214, 45.786, 41.586, 38.775 /
C         SOLAR SPECTRUM FROM  44240 TO  45670 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB12  /
     A 40.753, 46.752, 51.684, 52.597, 51.449, 50.684, 49.450, 46.747,
     B 45.369, 47.685, 50.240, 48.961, 46.693, 48.600, 53.694, 56.465,
     C 54.341, 50.722, 49.877, 51.246, 52.088, 52.765, 56.254, 63.326,
     D 69.744, 71.066, 68.349, 65.123, 62.551, 59.195, 53.705, 48.161,
     E 46.236, 47.710, 49.660, 50.799, 51.836, 54.537, 59.647, 64.707,
     F 65.844, 61.634, 55.570, 54.083, 58.781, 64.888, 69.777, 74.008,
     G 76.492, 76.226, 74.746, 74.941, 77.801, 79.619, 76.190, 67.190,
     H 55.231, 45.813, 43.141, 45.647, 49.466, 52.231, 52.221, 48.886,
     I 44.716, 42.613, 43.385, 45.968, 48.121, 48.998, 49.885, 50.707,
     J 49.893, 48.319, 48.198, 50.280, 53.830, 55.914, 54.822, 52.939,
     K 51.944, 49.438, 42.956, 34.614, 28.100, 24.503, 24.203, 27.839,
     L 34.604, 41.615, 45.324, 45.444, 45.527, 47.179, 45.756, 36.862,
     M 26.037, 20.569, 20.329, 24.263, 30.863, 35.939, 36.711, 35.693,
     N 37.256, 40.862, 44.416, 48.800, 54.182, 57.655, 58.427, 59.965,
     O 63.940, 66.820, 65.465, 59.482, 49.396, 39.422, 34.182, 35.388,
     P 42.875, 52.034, 57.595, 59.093, 57.272, 52.172, 45.493, 39.419,
     Q 35.581, 35.902, 40.354, 46.732, 53.309, 58.781, 61.785, 59.255,
     R 50.030, 41.567, 40.523, 43.584, 44.875, 42.754, 40.077, 39.941 /
C         SOLAR SPECTRUM FROM  45680 TO  47110 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB13  /
     A 40.977, 39.567, 34.955, 30.424, 31.039, 38.687, 47.480, 49.830,
     B 46.790, 44.829, 46.546, 50.415, 54.602, 57.656, 58.463, 57.276,
     C 55.621, 54.514, 53.338, 50.026, 42.817, 33.636, 27.134, 25.516,
     D 27.897, 31.392, 32.125, 29.463, 26.581, 25.956, 27.737, 31.175,
     E 34.959, 37.671, 38.641, 37.958, 36.733, 35.681, 33.877, 30.849,
     F 28.059, 27.615, 29.319, 29.375, 25.390, 20.659, 19.484, 22.297,
     G 27.282, 32.467, 35.906, 37.137, 37.895, 39.130, 39.777, 39.872,
     H 40.778, 42.317, 42.934, 40.430, 34.227, 27.701, 23.880, 22.174,
     I 21.639, 22.589, 25.184, 29.017, 32.981, 36.110, 38.580, 41.239,
     J 44.426, 46.939, 47.010, 44.165, 39.659, 35.556, 32.838, 31.546,
     K 32.676, 36.963, 42.333, 44.931, 43.704, 40.943, 37.973, 35.199,
     L 33.574, 33.339, 34.185, 36.347, 39.963, 43.964, 47.162, 48.987,
     M 48.976, 47.948, 48.004, 49.892, 51.065, 47.834, 40.489, 32.665,
     N 26.795, 24.461, 26.655, 31.928, 37.634, 41.345, 40.956, 36.827,
     O 32.110, 28.612, 26.482, 26.602, 28.831, 30.877, 30.976, 30.063,
     P 29.887, 30.305, 29.974, 28.265, 26.517, 27.066, 30.403, 34.539,
     Q 37.104, 37.598, 37.252, 37.060, 36.498, 34.167, 29.814, 24.192,
     R 18.515, 15.086, 15.040, 17.158, 20.807, 25.682, 30.352, 34.203 /
C         SOLAR SPECTRUM FROM  47120 TO  48550 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB14  /
     A 37.902, 42.531, 47.832, 50.509, 48.019, 42.616, 38.321, 37.370,
     B 40.172, 44.395, 46.132, 43.911, 38.396, 31.379, 26.275, 25.075,
     C 26.652, 28.963, 31.168, 34.168, 38.050, 40.231, 38.347, 32.741,
     D 26.199, 21.863, 20.249, 20.185, 21.726, 25.562, 30.318, 33.431,
     E 34.453, 34.959, 36.374, 37.870, 36.655, 31.966, 25.920, 21.264,
     F 20.663, 24.658, 30.263, 34.021, 34.336, 31.356, 26.926, 23.109,
     G 20.867, 20.684, 22.416, 24.878, 26.779, 27.334, 26.537, 25.210,
     H 24.013, 22.944, 21.800, 20.449, 19.290, 19.528, 21.742, 24.125,
     I 23.994, 21.559, 19.555, 18.915, 18.342, 17.335, 16.549, 16.479,
     J 17.211, 18.445, 19.294, 18.980, 17.912, 17.156, 17.103, 17.256,
     K 16.925, 15.842, 14.485, 13.683, 13.647, 13.914, 14.009, 13.770,
     L 13.456, 13.399, 13.547, 13.760, 14.060, 14.427, 14.644, 14.438,
     M 13.986, 13.749, 13.927, 14.390, 14.759, 14.822, 14.679, 14.448,
     N 14.186, 13.937, 13.754, 13.657, 13.540, 13.308, 13.053, 12.841,
     O 12.704, 12.742, 12.811, 12.662, 12.355, 12.100, 12.003, 12.014,
     P 12.067, 12.223, 12.444, 12.472, 12.164, 11.732, 11.515, 11.619,
     Q 11.873, 12.028, 11.947, 11.722, 11.399, 10.930, 10.473, 10.205,
     R 10.224, 10.694, 11.468, 12.007, 12.083, 11.905, 11.498, 10.891 /
C         SOLAR SPECTRUM FROM  48560 TO  49990 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB15  /
     A 10.575, 10.846, 11.353, 11.612, 11.411, 10.876, 10.383, 10.305,
     B 10.695, 11.245, 11.636, 11.828, 11.918, 11.865, 11.674, 11.510,
     C 11.407, 11.303, 11.216, 11.143, 11.039, 10.983, 11.004, 10.900,
     D 10.653, 10.562, 10.781, 11.186, 11.605, 11.806, 11.582, 11.056,
     E 10.567, 10.335, 10.408, 10.729, 11.165, 11.540, 11.646, 11.372,
     F 10.933, 10.524, 9.9973, 9.3783, 8.9883, 9.0163, 9.4125, 9.9179,
     G 10.278, 10.472, 10.553, 10.575, 10.519, 10.216, 9.6821, 9.1499,
     H 8.7057, 8.3894, 8.3442, 8.6241, 9.1371, 9.7184, 10.191, 10.443,
     I 10.458, 10.289, 9.9772, 9.5829, 9.3097, 9.3195, 9.4694, 9.5182,
     J 9.4326, 9.2478, 8.8197, 7.9809, 6.9996, 6.4856, 6.7462, 7.5406,
     K 8.2813, 8.7258, 9.0682, 9.1665, 8.8637, 8.4638, 8.2393, 8.1656,
     L 8.1880, 8.3578, 8.6488, 8.8980, 9.0117, 9.0659, 9.1955, 9.4207,
     M 9.5526, 9.4237, 9.1290, 8.8441, 8.6138, 8.4237, 8.2979, 8.2598,
     N 8.2859, 8.3475, 8.4533, 8.6285, 8.8310, 8.8866, 8.6750, 8.3312,
     O 8.0091, 7.7296, 7.6239, 7.8692, 8.2725, 8.4086, 8.2515, 8.0914,
     P 8.0003, 7.9367, 7.9266, 7.9580, 8.0492, 8.2376, 8.4263, 8.4811,
     Q 8.3309, 8.0263, 7.7632, 7.6987, 7.8124, 7.9390, 8.0183, 8.0816,
     R 8.0428, 7.8923, 7.6963, 7.4969, 7.4013, 7.4289, 7.4489, 7.4059 /
C         SOLAR SPECTRUM FROM  50000 TO  51430 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB16  /
     A 7.4198, 7.5261, 7.5252, 7.3239, 7.1263, 7.1423, 7.3340, 7.5049,
     B 7.5484, 7.5319, 7.5163, 7.4995, 7.5728, 7.8104, 8.0588, 8.0948,
     C 7.9140, 7.6978, 7.5116, 7.2138, 6.8063, 6.5430, 6.5232, 6.5869,
     D 6.5610, 6.3984, 6.1889, 6.0587, 6.0676, 6.1988, 6.3140, 6.2527,
     E 6.0929, 6.0277, 6.0941, 6.3031, 6.6594, 6.9398, 6.9566, 6.8310,
     F 6.7374, 6.6812, 6.6558, 6.8336, 7.2020, 7.4012, 7.2950, 7.0488,
     G 6.7966, 6.6293, 6.5868, 6.5980, 6.6007, 6.6501, 6.7627, 6.7853,
     H 6.6321, 6.4856, 6.5198, 6.6486, 6.7271, 6.7227, 6.6696, 6.6189,
     I 6.5979, 6.6188, 6.7110, 6.8343, 6.8750, 6.8250, 6.7885, 6.8266,
     J 6.8556, 6.8068, 6.8377, 7.0467, 7.2779, 7.4139, 7.4712, 7.4621,
     K 7.4071, 7.3592, 7.3372, 7.3220, 7.2938, 7.2531, 7.2052, 7.1335,
     L 7.0298, 6.8533, 6.5535, 6.2227, 6.0139, 5.9384, 5.9038, 5.8568,
     M 5.7909, 5.7326, 5.7745, 5.9608, 6.1865, 6.3681, 6.4997, 6.5437,
     N 6.4637, 6.2708, 6.0451, 5.9557, 6.0855, 6.2542, 6.2454, 6.0795,
     O 5.9102, 5.8447, 5.9218, 6.1063, 6.2895, 6.3271, 6.1097, 5.7421,
     P 5.4452, 5.2981, 5.3256, 5.4935, 5.6819, 5.8245, 5.8933, 5.9630,
     Q 6.1703, 6.4525, 6.6325, 6.6965, 6.7185, 6.6238, 6.3107, 5.9241,
     R 5.6987, 5.6651, 5.7428, 5.8790, 5.9715, 5.9618, 5.9674, 6.0754 /
C         SOLAR SPECTRUM FROM  51440 TO  52870 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB17  /
     A 6.2541, 6.4300, 6.4968, 6.4564, 6.4082, 6.3024, 6.0135, 5.6431,
     B 5.3963, 5.2989, 5.2635, 5.2227, 5.1279, 4.9315, 4.6348, 4.3168,
     C 4.0151, 3.6625, 3.2906, 3.1028, 3.1349, 3.1994, 3.2596, 3.4144,
     D 3.5949, 3.6534, 3.6296, 3.6281, 3.5876, 3.4292, 3.2659, 3.2284,
     E 3.2576, 3.3002, 3.4535, 3.7372, 4.0573, 4.3558, 4.5999, 4.7781,
     F 4.8855, 4.8999, 4.8392, 4.7624, 4.7059, 4.6981, 4.7666, 4.8453,
     G 4.8236, 4.7293, 4.6861, 4.7132, 4.7725, 4.8713, 4.9596, 4.9527,
     H 4.8957, 4.9252, 5.0736, 5.2229, 5.2505, 5.1537, 5.0156, 4.8880,
     I 4.7686, 4.6549, 4.5534, 4.4828, 4.4661, 4.5040, 4.5905, 4.7033,
     J 4.7858, 4.8334, 4.9283, 5.0377, 5.0065, 4.8471, 4.6828, 4.5586,
     K 4.4812, 4.4314, 4.3903, 4.3830, 4.4066, 4.3900, 4.2973, 4.1978,
     L 4.1462, 4.1084, 4.1495, 4.3897, 4.6859, 4.8206, 4.7938, 4.6781,
     M 4.5222, 4.3959, 4.3358, 4.2947, 4.2259, 4.1452, 4.1060, 4.1462,
     N 4.2149, 4.2549, 4.3061, 4.3742, 4.3738, 4.2718, 4.1389, 4.0405,
     O 3.9457, 3.8127, 3.7099, 3.7344, 3.8589, 3.9598, 3.9525, 3.8377,
     P 3.6708, 3.5357, 3.4929, 3.5375, 3.6381, 3.7890, 3.9671, 4.0995,
     Q 4.1421, 4.1302, 4.1235, 4.1623, 4.2506, 4.2948, 4.2231, 4.0993,
     R 3.9680, 3.9475, 4.1958, 4.5131, 4.6101, 4.5130, 4.3474, 4.1749 /
C         SOLAR SPECTRUM FROM  52880 TO  54310 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB18  /
     A 4.0467, 3.9956, 4.0078, 4.0374, 4.0255, 3.9379, 3.8192, 3.7529,
     B 3.7675, 3.8260, 3.8654, 3.8518, 3.8148, 3.8028, 3.8098, 3.7934,
     C 3.7660, 3.7944, 3.8689, 3.8978, 3.8856, 3.8923, 3.8570, 3.6940,
     D 3.4693, 3.3222, 3.2824, 3.2887, 3.3039, 3.3222, 3.3313, 3.3326,
     E 3.3482, 3.3807, 3.4188, 3.4602, 3.4972, 3.5151, 3.5155, 3.5165,
     F 3.5258, 3.5406, 3.5478, 3.5345, 3.5339, 3.5820, 3.6396, 3.6448,
     G 3.5872, 3.5112, 3.4804, 3.5257, 3.6238, 3.7290, 3.8023, 3.8024,
     H 3.7268, 3.6578, 3.6439, 3.6422, 3.6373, 3.6397, 3.6410, 3.6494,
     I 3.6608, 3.6251, 3.5212, 3.4020, 3.2845, 3.1230, 2.9483, 2.8515,
     J 2.8432, 2.8638, 2.8967, 2.9505, 3.0025, 3.0552, 3.1106, 3.1178,
     K 3.0596, 2.9854, 2.9316, 2.8903, 2.8590, 2.8500, 2.8450, 2.8121,
     L 2.7626, 2.7424, 2.7667, 2.8024, 2.8165, 2.8111, 2.8128, 2.8569,
     M 2.9659, 3.1062, 3.1990, 3.2128, 3.2088, 3.2391, 3.2661, 3.2364,
     N 3.1173, 2.9094, 2.6952, 2.5324, 2.3959, 2.2953, 2.2510, 2.2245,
     O 2.1811, 2.1301, 2.1482, 2.3257, 2.5856, 2.7226, 2.6495, 2.4508,
     P 2.2444, 2.0850, 1.9891, 1.9843, 2.0816, 2.2233, 2.3248, 2.3551,
     Q 2.3479, 2.3606, 2.4296, 2.5361, 2.6128, 2.6216, 2.6069, 2.6196,
     R 2.6464, 2.6427, 2.5823, 2.4682, 2.3320, 2.2405, 2.2637, 2.3973 /
C         SOLAR SPECTRUM FROM  54320 TO  55750 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB19  /
     A 2.5524, 2.6891, 2.8508, 3.0103, 3.0681, 3.0064, 2.9114, 2.8609,
     B 2.8517, 2.8374, 2.7894, 2.7288, 2.7138, 2.7729, 2.8707, 2.9536,
     C 2.9953, 2.9911, 2.9398, 2.8550, 2.7732, 2.7303, 2.7366, 2.7650,
     D 2.7705, 2.7374, 2.6830, 2.6218, 2.5663, 2.5341, 2.5351, 2.5681,
     E 2.6124, 2.6305, 2.6024, 2.5431, 2.4840, 2.4546, 2.4684, 2.5100,
     F 2.5445, 2.5532, 2.5564, 2.5889, 2.6616, 2.7553, 2.8466, 2.9290,
     G 2.9958, 3.0175, 2.9774, 2.8990, 2.8001, 2.6927, 2.6171, 2.5931,
     H 2.5809, 2.5276, 2.4284, 2.3365, 2.3162, 2.3855, 2.4872, 2.5455,
     I 2.5773, 2.6809, 2.9720, 3.5757, 4.4006, 5.0044, 5.0295, 4.5135,
     J 3.7071, 2.9059, 2.3600, 2.1418, 2.1119, 2.0871, 2.0301, 2.0043,
     K 2.0361, 2.0963, 2.1520, 2.1878, 2.1955, 2.1864, 2.1899, 2.2170,
     L 2.2574, 2.2895, 2.2783, 2.2148, 2.1641, 2.2343, 2.4726, 2.8119,
     M 3.1288, 3.2984, 3.2206, 2.8859, 2.4473, 2.1436, 2.0729, 2.1391,
     N 2.2171, 2.2580, 2.2654, 2.2481, 2.2103, 2.1657, 2.1356, 2.1321,
     O 2.1438, 2.1461, 2.1396, 2.1460, 2.1588, 2.1581, 2.1481, 2.1343,
     P 2.1101, 2.0754, 2.0400, 2.0121, 1.9930, 1.9799, 1.9699, 1.9613,
     Q 1.9537, 1.9454, 1.9312, 1.9058, 1.8726, 1.8470, 1.8465, 1.8693,
     R 1.8844, 1.8635, 1.8143, 1.7618, 1.7188, 1.6853, 1.6656, 1.6708 /
C         SOLAR SPECTRUM FROM  55760 TO  57190 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB20  /
     A 1.7036, 1.7519, 1.8120, 1.9015, 2.0124, 2.0980, 2.1385, 2.1481,
     B 2.1347, 2.1086, 2.0953, 2.1062, 2.1095, 2.0685, 2.0001, 1.9461,
     C 1.9194, 1.9088, 1.9023, 1.8977, 1.9049, 1.9300, 1.9588, 1.9635,
     D 1.9357, 1.9019, 1.8887, 1.8939, 1.9018, 1.9038, 1.8975, 1.8747,
     E 1.8289, 1.7716, 1.7303, 1.7330, 1.7900, 1.8782, 1.9548, 1.9907,
     F 1.9807, 1.9430, 1.9173, 1.9218, 1.9203, 1.8717, 1.7832, 1.6965,
     G 1.6389, 1.6077, 1.5924, 1.5818, 1.5583, 1.5142, 1.4616, 1.4237,
     H 1.4252, 1.4834, 1.5970, 1.7410, 1.8771, 1.9784, 2.0451, 2.0872,
     I 2.0909, 2.0384, 1.9573, 1.9002, 1.8824, 1.8663, 1.8193, 1.7540,
     J 1.6874, 1.6222, 1.5726, 1.5450, 1.5290, 1.5312, 1.5699, 1.6411,
     K 1.7186, 1.7678, 1.7546, 1.6623, 1.5115, 1.3588, 1.2605, 1.2348,
     L 1.2611, 1.3091, 1.3588, 1.3884, 1.3800, 1.3482, 1.3224, 1.3159,
     M 1.3437, 1.4142, 1.4950, 1.5443, 1.5521, 1.5282, 1.4902, 1.4606,
     N 1.4465, 1.4398, 1.4399, 1.4544, 1.4760, 1.4781, 1.4506, 1.4229,
     O 1.4185, 1.4221, 1.4119, 1.3908, 1.3779, 1.3813, 1.3933, 1.4087,
     P 1.4268, 1.4417, 1.4408, 1.4188, 1.3861, 1.3548, 1.3261, 1.2980,
     Q 1.2769, 1.2731, 1.2856, 1.3002, 1.3056, 1.2987, 1.2817, 1.2590,
     R 1.2291, 1.1868, 1.1428, 1.1183, 1.1141, 1.1120, 1.1009, 1.0797 /
C         SOLAR SPECTRUM FROM  57200 TO  57490 CM-1,  IN STEPS OF 10 CM-
      DATA SUNB21  /
     A 1.0523, 1.0284, 1.0251, 1.0577, 1.1195, 1.1791, 1.2061, 1.2013,
     B 1.1936, 1.2000, 1.2040, 1.1824, 1.1489, 1.1400, 1.1539, 1.1629,
     C 1.1617, 1.1586, 1.1564, 1.1572, 1.1565, 1.1399, 1.1037, 1.0627,
     D 1.0341, 1.0223, 1.0199, 1.0188, 1.0174, 1.0163  /
      END
      SUBROUTINE ABGAS1(V,AMTPB,AMTP,TAUM,TWGP,TX,AAA,CPSS)
C EXTINCTION COEFFICIENT OF THE MOLECULAR ABSORPTION/
C        GAS MIXING RATIO (PPMV)
C Associated routines with this routine: SCALD, TAUF, TRKF
C--- HISTORY
C 92. 6.30  CREATED
C    11.13  Make all subscript of arrays more than 0 by Masahito Tsukamoto.
C 93. 6. 8  Add TAUF
C 94. 3.26  ALOG -> LOG at several places of sub-routines.
C--- INPUT
C V        R       WAVENUMBER (CM-1)
C AMTPB   R(53)    EFFECTIVE GAS AMOUNT -DENSTY- INTEGRATED
C                  ALONG OPTICAL PATH, FOR BANDS DEFINED BY -ABCDTA-
C                  UNIT IS in PPMV. SEE -SCALD-
C--- OUTPUT
C V                TRUNCATED TO A MULTIPLE OF 5 CM-1
C AMTP    R(28)    SCALING FACTOR OF ABSORBER AMOUNT FOR LINE ABSORPTION
C                     1: H2O    2: CO2     3: O3      4: N2O     5: CO
C                     6: CH4    7: O2
C                     8: NO     9: SO2    10: NO2    11: NH3
C                     FIRST 11 ARE MEANINGFUL
C TX      R(28)    CONTINUUM ABSORPTION OPTICAL THICKNESS OF EACH GAS
C                     1, 3, 7, 11, 22 HAVE NON-ZERO VALUES
C TAUM   R(3,28)   MOLECULAR LINE ABSORPTION COEFF. (EACH GAS)
C                    PER UNIT OF -AMTPB-
C                    FIRST 11 HAVE NON-ZERO MEANINGFUL
C                    See TRKF and TAUF for how to use it.
C TWGP   R(3,28)   K DISTRIBUTION
C AAA    R(28)     NON-LINEAR PARAMETER FOR ABSORBER AMOUNT DEPENDENCE
C                    IN RANDOM MODEL TRANSMISSION
C CPSS   R(28)     LOG10(CONVERGENCE FACTOR OF ABSORPTION COEFF.
C                    TO REFERENCE PRESSURE AND TEMPERATURE)
C---NOTES
C USE -TRK- TO CALCULATE THE TRANSMITTANCE BY K-DISTRIBUTION
C USE -TRB- TO CALCULATE THE TRANSMITTANCE BY BAND MODEL
C
C USE -DBLTX- TO CALCULATE TRANSMITTANCE BY RANDOM MODEL
C LINK CXDTA      LOCATES COFFICIENT FOR DOUBLE EXPONENTIAL
C      C4DAT      N2 CONTINUUM ABSORPTION COFFICIENT
C      SLF296     SELF-BROADENED WATER VAPOR CONTINUUM AT 296K
C      SLF260     SELF-BROADENED WATER VAPOR CONTINUUM AT 260K
C      FRN296N    FOREIGN-BROADENED WATER VAPOR CONTINUUM AT 296K
C      FUDGE
C      ABCDTA     MOVE DOUBLE EXPONENTIAL COEFFICIENTS INTO NEW ARRAYS
C      C8DTA      RETURN CHAPPUIS VISIBLE ABSORPTION COEFFICIENT
C      O2CONT     O2 CONTINUUM FOR 1395-1760 CM-1
C      SCHRUN     UV O2 SCHUMANN-RUNGE BAND MODEL PARAMETERS
C      O3HHT0     UV O3 HARTLEY BAND TEMPERATURE DEPENDENT COFFICIENT
C                 CONSTANT TERM (24370 TO 40800 CM-1)
C      O3HHT1     UV O3 HARTLEY BAND TEMPERATURE DEPENDENT COFFICIENT
C                 LINEAR TERM
C      O3HHT2     UV O3 HARTLEY BAND TEMPERATURE DEPENDENT COFFICIENT
C                 QUADRATIC TERM
C      O3UV       UV O3 INTERPOLATION FOR 40800-54054 CM-1
C
CC***********************************************************************
C
      SAVE
      PARAMETER (KNM=28)
      DIMENSION AMTPB(53),TX(KNM),TAUM(3,KNM),TWGP(3,KNM),AMTP(KNM)
     & ,AAA(KNM),CPSS(KNM)
      DIMENSION ABB(63),CP1S(11),FAC(3),GKWJ(3,11),DPWJ(3,11)
      COMMON /H2O/    CPH2O(3515)
      COMMON /O3/     CPO3 ( 447)
      COMMON /UFMIX1/ CPCO2(1219)
      COMMON /UFMIX2/ CPCO ( 173),CPCH4( 493),CPN2O( 704),CPO2 ( 382)
      COMMON /TRACEG/ CPNH3( 431), CPNO(  62),CPNO2( 142),CPSO2( 226)
      COMMON /WNLOHI/
     L   IWLH2O(15),IWLO3 ( 6),IWLCO2(11),IWLCO ( 4),IWLCH4( 5),
     L   IWLN2O(12),IWLO2 ( 7),IWLNH3( 3),IWLNO ( 2),IWLNO2( 4),
     L   IWLSO2( 5),
     H   IWHH2O(15),IWHO3 ( 6),IWHCO2(11),IWHCO ( 4),IWHCH4( 5),
     H   IWHN2O(12),IWHO2 ( 7),IWHNH3( 3),IWHNO ( 2),IWHNO2( 4),
     H   IWHSO2( 5)
      COMMON /AABBCC/ AA(11),BB(11),CC(11),IBND(11),A(11),CPS(11)
      DATA   CF1/3.159E-08/,CF2/2.75E-04/
      DATA FAC /1.0,0.09,0.015/
      BIGEXP=30.
C
      DO 2 M=1,KNM
      AAA(M)=0
      CPSS(M)=0
    2 TX(M)=0
      DO 7 K=1, 3
      DO 7 M= 1,KNM
      TAUM(K,M)=0
    7 TWGP(K,M)=0
C
      ALPHA2=200.**2
      FACTOR=0.5
      IV=V/5.
      IV=IV*5
      INDH2O=1
      INDO3 =1
      INDCO2=1
      INDCO =1
      INDCH4=1
      INDN2O=1
      INDO2 =1
      INDNH3=1
      INDNO =1
      INDNO2=1
      INDSO2=1
      V=FLOAT(IV)
      CALL CXDTA(CP,V,IWLH2O,IWHH2O,CPH2O,INDH2O)
      CPS(1) = CP
      CALL CXDTA(CP,V,IWLCO2,IWHCO2,CPCO2,INDCO2)
      CPS(2) = CP
      CALL CXDTA(CP,V,IWLO3, IWHO3, CPO3, INDO3 )
      CPS(3) = CP
      CALL CXDTA(CP,V,IWLN2O,IWHN2O,CPN2O,INDN2O)
      CPS(4) = CP
      CALL CXDTA(CP,V,IWLCO, IWHCO, CPCO, INDCO )
      CPS(5) = CP
      CALL CXDTA(CP,V,IWLCH4,IWHCH4,CPCH4,INDCH4)
      CPS(6) = CP
      CALL CXDTA(CP,V,IWLO2, IWHO2, CPO2, INDO2 )
      CPS(7) = CP
      CALL CXDTA(CP,V,IWLNO, IWHNO, CPNO, INDNO )
      CPS(8) = CP
      CALL CXDTA(CP,V,IWLSO2,IWHSO2,CPSO2,INDSO2)
      CPS(9) = CP
      CALL CXDTA(CP,V,IWLNO2,IWHNO2,CPNO2,INDNO2)
      CPS(10)= CP
      CALL CXDTA(CP,V,IWLNH3,IWHNH3,CPNH3,INDNH3)
      CPS(11)= CP
      CALL ABCDTA(IV)
C K DISTRIBUTION
      DO  1 M = 1,11
      AAA(M)=A(M)
      CPSS(M)=CPS(M)
      IB = IBND(M)
C 92.11.13 Modified by M.T.
C     IB = IBND(M)
      IF(IB.LE.0)THEN
	AMTP(M)=0
       ELSE
        AMTP(M)=AMTPB(IB)
      END IF
C Correction end 92.11.13
  1   CP1S(M)= 10.**CPS(M)
      DO  4  M = 1,11
      DO  5  K = 1,3
      IW = IBND(M)
      GKWJ(K,M) = 0.
      DPWJ(K,M) = 0.
      IF(IW. LE. 0) GO TO 5
      GKWJ(K,M) = FAC(K) * CC(M)
      IF(K .EQ. 1) DPWJ(K,M) = AA(M)
      IF(K .EQ. 2) DPWJ(K,M) = BB(M)
      IF(K .EQ. 3) DPWJ(K,M) = 1- AA(M) - BB(M)
  5   CONTINUE
  4   CONTINUE
C
      CALL C4DTA(ABB(45),V)
C WATER CONTINUUM
       CALL  SLF296(V,SH2OT0)
       CALL  SLF260(V,SH2OT1)
       CALL  FRN296(V,FH2O)
       T0=296.
       T1=260.
       IF(SH2OT0   .GT.0.) THEN
C CORRECTION TO SELF CONTINUUM (1 SEPT 85); FACTOR OF 0.78 @ 1000
          XH2O=      (1. - 0.2333*( ALPHA2/((V -1050.)**2+ALPHA2)) )
          SH2OT0  =   SH2OT0 *XH2O
          SH2OT1  =   SH2OT1 *XH2O
      END IF
C PROTECT AGAINST EXPONENTIAL UNDERFLOW AT HIGH FREQUENCY
      VTEMP=V/.6952
      IF(VTEMP/T1.LE.BIGEXP)THEN
           XD=EXP(-V/(T0*0.6952))
           RADFN0=V*(1.-XD)/(1.+XD)
           XD=EXP(-V/(T1*0.6952))
           RADFN1=V*(1.-XD)/(1.+XD)
      ELSE
           RADFN0 = V
           RADFN1 = V
      ENDIF
      CALL FUDGE(V,FDG)
      ABB(42)=SH2OT0*RADFN0
      CALL C8DTA(ABB(46),V)
      ABB(43)=(SH2OT1*RADFN1)-(SH2OT0*RADFN0)
      ABB(44)=(FH2O+FDG)*RADFN0
C HNO3 ABSORPTION CALCULATION
C      CALL HNO3 (V,ABB(47))
      CALL HERTDA(ABB(48),V)
      CALL O2CONT(V,SIGO20,SIGO2A,SIGO2B)
      IF(V. GT. 49600 )CALL SCHRUN(V,CPS(7))
C DIFFUSE OZONE
           ABBUV = 0.
           IF(V .GT.24370. .AND. V .LT.40800.) THEN
              ABB(46) = 0.
              CALL O3HHT0                (V,C0)
              CALL O3HHT1                (V  ,CT1)
              CALL O3HHT2                (V,  CT2)
C
      END IF
      IF(V .GE.40800)   THEN
           CALL O3UV(V,C0)
           ABB(46) = 0.
      ENDIF
      WO3  = AMTPB(46) * .269
      IF(V .GT.24370. .AND. V .LT.40800.) THEN
           W1O3 = AMTPB(52)
           W2O3 = AMTPB(53)
C   COZ   = C0  *(1.+ CT1  *TC+CT2   *TC*TC)
           ABBUV = C0 * (WO3+CT1*W1O3+CT2   *W2O3)
      ENDIF
      IF(V .GE.40800) THEN
           ABBUV = C0 *  WO3
      ENDIF
C WATER VAPOR CONTINUUM
      TXH2O1 = ABB(42) *(AMTPB(42) *1.0E-20)
      TXH2O2 = ABB(43) *(AMTPB(43) *1.0E-20)
      TXH2O3 = ABB(44) *(AMTPB(44) *1.0E-20)
      TX(1) = TXH2O1 + TXH2O2 + TXH2O3
C N2 CONTINUUM
      TX(22)=ABB(45)*AMTPB(45)
C     OZONE
      TX(3) = ABB(46)*AMTPB(46) + ABBUV
C OXYGEN
      WT2 =   AMTPB(48) - AMTPB(50)*220.
      TX(7) =SIGO20*(AMTPB(50)+SIGO2A*WT2+SIGO2B*AMTPB(49))
C UV O2 HERZBERG CONTINUUM ABSORPTION CALCULATION
      TX(7) =AMTPB(51)*ABB(48)+ TX(7)
C HNO3
      TX(11)=AMTPB(47)*ABB(47)
C MOLECULAR LINE ABSORPTION
C   EVALUATE THE WEIGHTED K DISTRIBUTION QUANTITIES FOR
C   WATER VAPOR AND THE UNIFORMLY MIXED GASES
      DO 8 K=1, 3
      DO 8 M= 1,11
      TAUM(K,M)=GKWJ(K,M)*CP1S(M)
    8 TWGP(K,M)=DPWJ(K,M)*TAUM(K,M)
      RETURN
      END
      FUNCTION TAUBF(AMTP,TX,AAA,CPSS)
C TOTAL EFFECTIVE OPTICAL THICKNESS FOR THE EFFECTIVE ABSORBER AMOUNT
C -W*AMTP-, BY RANDOM MODEL
      DIMENSION AMTP(28),TX(28),AAA(28),CPSS(28)
C ADD N2 CONTINUUM
      TAUBF=TX(22)
      DO 1 M=1,11
      W1=AMTP(M)
      TAU=0
      IF (W1 .LT. 1.0E-20) GOTO 1
      IF (CPSS(M) .LE. -20) GOTO 1
C ALOG10 -> LOG10 (94.3.26)
      WS=LOG10(W1)+CPSS(M)
      QAWS=AAA(M)*WS
      IF(QAWS .GE. 2.0) THEN
        TAU=30
       ELSE
        IF (QAWS .GT. -6.0)  TAU=10.0**QAWS
      ENDIF
    1 TAUBF=TAUBF+TAU+TX(M)
      RETURN
      END
      SUBROUTINE TAUF(AMTP,TAUM,TWGP,TX,TAU,WGT)
C Synthesize optical thickness and k-distribution for the airmass.
C--- HISTORY
C 93. 6. 8 CREATED
C--- INPUT
C AMTP     R(11)    SCALED ABSORBER AMOUNT FOR EACH BAND
C TX       R(11)    ABSORPTION OPTICAL THICKNESS FOR
C                    CONTINUUM ABSORPTION
C TAUM   R(3,11)    K-VALUES IN THREE TERM K-DISTRIBUTION MODEL
C TWGP   R(3,11)    K-DISTRIBUTIONS
C--- OUTPUT
C TAU      R(3)     Optical thickness in the three term k-approximation
C WGT      R(3)     Weights

      DIMENSION AMTP(28),TAUM(3,28),TWGP(3,28),TX(28),TAU(3),WGT(3)
      DO 1 K=1,3
      TAUM1=0
      TWGP1=0
      DO 2 M=1,11
        TAUM1= TAUM1 + AMTP(M)*TAUM(K,M)
    2   TWGP1= TWGP1 + AMTP(M)*TWGP(K,M)
      IF(TAUM1 .NE. 0) THEN
        TWGP1 = TWGP1 / TAUM1
       ELSE
        TWGP1=1/3.0
      ENDIF
      TAU(K)=TAUM1
    1 WGT(K)=TWGP1
C ADD N2 CONTINUUM
      EX=TX(22)
      DO 3 M=1,11
    3 EX=EX+TX(M)
      DO 4 K=1,3
    4 TAU(K)=TAU(K)+EX
      RETURN
      END
      FUNCTION TRKF(AMTP,TAUM,TWGP,TX)
C K-DISTRIBUTION TRANSMITTANCE FOR EFFECTIVE ABSORBER AMOUNT
C   - W*AMTP-
C--- HISTORY
C 92. 6.30 CREATED
C--- INPUT
C AMTP     R(11)    SCALED ABSORBER AMOUNT FOR EACH BAND
C TX       R(11)    ABSORPTION OPTICAL THICKNESS FOR
C                    CONTINUUM ABSORPTION
C TAUM   R(3,11)    K-VALUES IN THREE TERM K-DISTRIBUTION MODEL
C TWGP   R(3,11)    K-DISTRIBUTIONS
C--- OUTPUT
C TR         R      TRANSMITTANCE
      DIMENSION AMTP(28),TAUM(3,28),TWGP(3,28),TX(28)
      TRKF=0
      DO 1 K=1,3
      TAUM1=0
      TWGP1=0
      DO 2 M=1,11
        TAUM1= TAUM1 + AMTP(M)*TAUM(K,M)
    2   TWGP1= TWGP1 + AMTP(M)*TWGP(K,M)
      IF(TAUM1 .NE. 0) THEN
        TWGP1 = TWGP1 / TAUM1
       ELSE
        TWGP1=1/3.0
      ENDIF
    1 TRKF=TRKF+TWGP1 * EXP(-TAUM1)
C ADD N2 CONTINUUM
      EX=TX(22)
      DO 3 M=1,11
    3 EX=EX+TX(M)
      TRKF=TRKF*EXP(-EX)
      RETURN
      END
      SUBROUTINE SCALD(PP,TT,W,IBMOL,DENSTY)
C EFFECTIVE GAS ABSORPTION AMOUNT
C--- HISTORY
C 92. 6.27 CREATED
C--- INPUT
C PP       R        PRESSURE IN MB
C TT       R        TEMPERATURE IN K
C W        R(28)    GAS AMOUNT IN PPMV (FIRST 11 ARE MEANINGFUL)
C---
C IBMOL    I(53)    BAND NUMBER TO MOLECULAR ID
C                   ABSORBER AMOUNT (PPMV) FOR ID: 1-11 SHOULD BE GIVEN.
C                   ABSORBER AMOUNT = 1    FOR ID: 22   SHOULD BE GIVEN.
C                   OTHERWISE ZERO.
C DENSTY   R(53)    EFFECTIVE GAS AMOUNT FOR THE BANDS DEFINED BY IBMOL
C                   UNIT IS DIFFERENT EACH BY EACH
C MOLECULAR CODE
C  1: H2O,  2: CO2,   3: O3,   4: N2O,  5: CO,  6: CH4,  7: O2
C  8: NO,   9: SO2,  10: NO2, 11: NH3
C     K
C     1-41    MOLECULAR LINE ABSORPTION
C     42-44   WATER VAPOR CONTINUUM ABSORPTION
C     45      N2 CONTINUUM
C     46      ULTRAVIOLET OZONE
C     47      HNO3 (NITRIC ACID)
C     48-50   OXYGEN
C     51      OXYGEN HERZBERG CONTINUUM
C     52-53   ULTRAVIOLET OZONE
C
      SAVE
      DIMENSION  W(28),IBMOL(53),DENSTY(53)
      DIMENSION IBMOL1(53)
C
      DATA PZERO /1013.25/,TZERO/273.15/
C  XLOSCH: LOSCHMIDT'S NUMBER (MOLECULES CM-2,KM-1)
C AXLOSCH:                    (MOLECULES CM-3)
      DATA XLOSCH/2.6868E24/,AXLOSCH/2.6868E19/
C CON CONVERTS WATER VAPOR FROM GM M-3 TO MOLECULES CM-2 KM-1
      DATA CON/3.3429E21/
C CONJOE=(1/XLOSCH)*1.E5*1.E-6 WITH
C        1.E5 ARISING FROM CM TO KM CONVERSION AND
C        1.E-6  "       "  PPMV
C CONJOE IS USED TO CHANGE PPMV TO ATM CM/KM
      DATA CONJOE/3.7194E-21/
C H20 CONTINUUM IS STORED AT 296 K RHZERO IS AIR DENSITY AT 296 K
C IN UNITS OF LOSCHMIDT'S. RHZERO=(273.15/296.0)
      DATA RHZERO/0.922804/
      DATA IBMOL1/ 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     &             1,  1,  1,  1,  3,  3,  3,  3,  3,  2,
     &             2,  2,  2,  2,  2,  2,  2,  5,  5,  6,
     &             4,  4,  4,  7,  7, 11, 11,  8, 10,  9,
     &             9,  1,  1,  1, 22,  3, 12,  7,  7,  7,
     &             7,  3,  3/
      DATA INIT/1/
      IF(INIT.GT.0) THEN
        INIT=0
        DO 1 K=1,53
    1   IBMOL(K)=IBMOL1(K)
      ENDIF
      PSS=PP/PZERO
      TSS=TZERO/TT
C AIR MOLECULES/CM3
      WAIR = AXLOSCH * PSS * TSS
C AAPV TO G/M3 (2.989E-23 = 18 G/AVOGADRO NUMBER)
      WH1    = 2.989E-23 *WAIR*W(1)
      WTEMP=WH1
C     UV OZONE (PPMV TO ATM CM/KM)
      DENSTY(46)= CONJOE   *WAIR    *W(3)
C     N2 CONTINUUM
      F1=PSS * TSS
      F2=PSS * SQRT(TSS)
      DENSTY(45)=0.781*F1*F2
C     SELF BROADENED WATER
      RHOAIR = F1
      RHOH2O = CON *WTEMP/XLOSCH
      RHOFRN = RHOAIR - RHOH2O
C PPMV TO MOLECULES/(CM2 KM)
      DENSTY(42)= XLOSCH*RHOH2O**2/RHZERO
C     FOREIGN BROADENED
      DENSTY(44)= XLOSCH*RHOH2O*RHOFRN/RHZERO
      DENSTY(43)=0.
C****************************************************
C   HNO3 IS SUBTRACTED FROM LOWTRAN
      DENSTY(47)=0.
C****************************************************
C      O2 IN ATM CM/KM
C      O2 TEMP DEP
C
      DT = TT  - 220.
      WO2D       = CONJOE   *WAIR    *W(7)  * PSS
C
C     DT CAN BE NEGIVATIVE
C     EFFECTIVE DT CALCULATED IN TRANS
C
      DENSTY(48)  = WO2D * TT
      DENSTY(49)  = WO2D * DT * DT
      DENSTY(50)  = WO2D
C
C  --- FOR H2O  G/M3
      CONH2O=WH1  *.1
      DENSTY(1)=CONH2O*PSS**0.9810*TSS**( 0.3324)
      DENSTY(2)=CONH2O*PSS**1.1406*TSS**(-2.6343)
      DENSTY(3)=CONH2O*PSS**0.9834*TSS**(-2.5294)
      DENSTY(4)=CONH2O*PSS**1.0443*TSS**(-2.4359)
      DENSTY(5)=CONH2O*PSS**0.9681*TSS**(-1.9537)
      DENSTY(6)=CONH2O*PSS**0.9555*TSS**(-1.5378)
      DENSTY(7)=CONH2O*PSS**0.9362*TSS**(-1.6338)
      DENSTY(8)=CONH2O*PSS**0.9233*TSS**(-0.9398)
      DENSTY(9)=CONH2O*PSS**0.8658*TSS**(-0.1034)
      DENSTY(10)=CONH2O*PSS**0.8874*TSS**(-0.2576)
      DENSTY(11)=CONH2O*PSS**0.7982*TSS**( 0.0588)
      DENSTY(12)=CONH2O*PSS**0.8088*TSS**( 0.2816)
      DENSTY(13)=CONH2O*PSS**0.6642*TSS**( 0.2764)
      DENSTY(14)=CONH2O*PSS**0.6656*TSS**( 0.5061)
C  --- FOR O3
      CONO3 = CONJOE   *WAIR    *W(3)
      DENSTY(15)=CONO3 *PSS**0.4200*TSS**( 1.3909)
      DENSTY(16)=CONO3 *PSS**0.4221*TSS**( 0.7678)
      DENSTY(17)=CONO3 *PSS**0.3739*TSS**( 0.1225)
      DENSTY(18)=CONO3 *PSS**0.1770*TSS**( 0.9827)
      DENSTY(19)=CONO3 *PSS**0.3921*TSS**( 0.1942)
C  --- FOR CO2
      CONCO2= CONJOE   *WAIR  *W(2)
      DENSTY(20)=CONCO2*PSS**0.6705*TSS**(-2.2560)
      DENSTY(21)=CONCO2*PSS**0.7038*TSS**(-5.0768)
      DENSTY(22)=CONCO2*PSS**0.7258*TSS**(-1.6740)
      DENSTY(23)=CONCO2*PSS**0.6982*TSS**(-1.8107)
      DENSTY(24)=CONCO2*PSS**0.8867*TSS**(-0.5327)
      DENSTY(25)=CONCO2*PSS**0.7883*TSS**(-1.3244)
      DENSTY(26)=CONCO2*PSS**0.6899*TSS**(-0.8152)
      DENSTY(27)=CONCO2*PSS**0.6035*TSS**( 0.6026)
C  --- FOR CO
      CONCO = CONJOE   *WAIR  *W(5)
      DENSTY(28)=CONCO *PSS**0.7589*TSS**( 0.6911)
      DENSTY(29)=CONCO *PSS**0.9267*TSS**( 0.1716)
C  --- FOR CH4
      CONCH4= CONJOE   *WAIR  *W(6)
      DENSTY(30)=CONCH4*PSS**0.7139*TSS**(-0.4185)
C  --- FOR N2O
      CONN2O= CONJOE   *WAIR  *W(4)
      DENSTY(31)=CONN2O*PSS**0.3783*TSS**( 0.9399)
      DENSTY(32)=CONN2O*PSS**0.7203*TSS**(-0.1836)
      DENSTY(33)=CONN2O*PSS**0.7764*TSS**( 1.1931)
C  --- FOR O2
      CONO2 = CONJOE   *WAIR  *W(7)
      DENSTY(34)=CONO2 *PSS**1.1879*TSS**( 2.9738)
      DENSTY(35)=CONO2 *PSS**0.9353*TSS**( 0.1936)
C  --- FOR NH3
      CONNH3= CONJOE   *WAIR  *W(11)
      DENSTY(36)=CONNH3*PSS**0.8023*TSS**(-0.9111)
      DENSTY(37)=CONNH3*PSS**0.6968*TSS**( 0.3377)
C  --- FOR NO
      CONNO = CONJOE   *WAIR  *W(8)
      DENSTY(38)=CONNO *PSS**0.5265*TSS**(-0.4702)
C  --- FOR NO2
      CONNO2= CONJOE   *WAIR  *W(10)
      DENSTY(39)=CONNO2*PSS**0.3956*TSS**(-0.0545)
C  --- FOR SO2
      CONSO2= CONJOE   *WAIR  *W(9)
      DENSTY(40)=CONSO2*PSS**0.2943*TSS**( 1.2316)
      DENSTY(41)=CONSO2*PSS**0.2135*TSS**( 0.0733)
C***********************************************************************
C   HERZBERG CONTINUUM PRESSURE DEPENDENCE CALCULATION, SHARDANAND 1977
C      AND   YOSHINO ET AL 1988
C
C     OXYGEN
C
      DENSTY(51)=(1.+.83*F1)*CONO2
C ADDED FROM -GEO-
      DENSTY(52)=DENSTY(46)*0.269*(TT-273.15)
      DENSTY(53)=DENSTY(46)*0.269*(TT-273.15)**2
      WTEM = (296.0-TT)/(296.0-260.0)
      IF(WTEM.LT.0.0) WTEM = 0.
      IF(WTEM.GT.1.0) WTEM = 1.0
      DENSTY(43) =WTEM*DENSTY(42)
      RETURN
      END
      SUBROUTINE ABCDTA(IV)
C
      COMMON /ABC/ FACTOR(3),ANH3(2),ACO2(10),ACO(3),
     X             ACH4(4),ANO2(3),AN2O(11),AO2(6),AO3(5),
     X             ASO2(4),AH2O(14),ANO,
     X             AANH3(2),BBNH3(2),CCNH3(2),
     X             AACO2(10),BBCO2(10),CCCO2(10),
     X             AACO(3),BBCO(3),CCCO(3),
     X             AACH4(4),BBCH4(4),CCCH4(4),
     X             AANO2(3),BBNO2(3),CCNO2(3),
     X             AAN2O(11),BBN2O(11),CCN2O(11),
     X             AAO2(6),BBO2(6),CCO2(6),
     X             AAO3(5),BBO3(5),CCO3(5),
     X             AASO2(4),BBSO2(4),CCSO2(4),
     X             AAH2O(14),BBH2O(14),CCH2O(14),
     X             AANO     ,BBNO     ,CCNO
C
      COMMON /AABBCC/ AA(11),BB(11),CC(11),IBND(11),A(11),CPS(11)
      DIMENSION TJO(10)
      DATA TJO /.9,.8,.7,.6,.5,.4,.3,.2,.1,.02/
C
C    MOL
C     1    H2O (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C     2    CO2 (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C     3    O3  (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C     4    N2O (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C     5    CO  (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C     6    CH4 (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C     7    O2  (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C     8    NO  (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C     9    SO2 (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C    10    NO2 (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C    11    NH3 (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)
C
C  ---H2O
      IMOL = 1
      IW = -1
      IF(IV.GE.     0.AND.IV.LE.   345) IW = 1
      IF(IV.GE.   350.AND.IV.LE.  1000) IW = 2
      IF(IV.GE.  1005.AND.IV.LE.  1640) IW = 3
      IF(IV.GE.  1645.AND.IV.LE.  2530) IW = 4
      IF(IV.GE.  2535.AND.IV.LE.  3420) IW = 5
      IF(IV.GE.  3425.AND.IV.LE.  4310) IW = 6
      IF(IV.GE.  4315.AND.IV.LE.  6150) IW = 7
      IF(IV.GE.  6155.AND.IV.LE.  8000) IW = 8
      IF(IV.GE.  8005.AND.IV.LE.  9615) IW = 9
      IF(IV.GE.  9620.AND.IV.LE. 11540) IW = 10
      IF(IV.GE. 11545.AND.IV.LE. 13070) IW = 11
      IF(IV.GE. 13075.AND.IV.LE. 14860) IW = 12
      IF(IV.GE. 14865.AND.IV.LE. 16045) IW = 13
      IF(IV.GE. 16340.AND.IV.LE. 17860) IW = 14
      IBAND = IW
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =   AH2O(IBAND)
           AA(IMOL)  = AAH2O(IBAND)
           BB(IMOL)  = BBH2O(IBAND)
           CC(IMOL)  = CCH2O(IBAND)
      ENDIF
C  ---O3
      IMOL = 3
      IW = -1
      IF (IV .GE.     0 .AND. IV .LE.   200)  IW = 15
      IF (IV .GE.   515 .AND. IV .LE.  1275)  IW = 16
      IF (IV .GE.  1630 .AND. IV .LE.  2295)  IW = 17
      IF (IV .GE.  2670 .AND. IV .LE.  2845)  IW = 18
      IF (IV .GE.  2850 .AND. IV .LE.  3260)  IW = 19
      IBAND      = IW-14
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =  AO3(IBAND)
           AA(IMOL) = AAO3(IBAND)
           BB(IMOL) = BBO3(IBAND)
           CC(IMOL) = CCO3(IBAND)
      ENDIF
C  ---CO2
      IMOL = 2
      IW = -1
      IF (IV .GE.   425 .AND. IV .LE.   835)  IW = 20
      IF (IV .GE.   840 .AND. IV .LE.  1440)  IW = 21
      IF (IV .GE.  1805 .AND. IV .LE.  2855)  IW = 22
      IF (IV .GE.  3070 .AND. IV .LE.  3755)  IW = 23
      IF (IV .GE.  3760 .AND. IV .LE.  4065)  IW = 24
      IF (IV .GE.  4530 .AND. IV .LE.  5380)  IW = 25
      IF (IV .GE.  5905 .AND. IV .LE.  7025)  IW = 26
      IF((IV .GE.  7395 .AND. IV .LE.  7785) .OR.
     *   (IV .GE.  8030 .AND. IV .LE.  8335) .OR.
     *   (IV .GE.  9340 .AND. IV .LE.  9670)) IW = 27
      IBAND = IW-19
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =  ACO2(IBAND)
           AA(IMOL) = AACO2(IBAND)
           BB(IMOL) = BBCO2(IBAND)
           CC(IMOL) = CCCO2(IBAND)
      ENDIF
C  ---CO
      IMOL = 5
      IW = -1
      IF (IV .GE.     0 .AND. IV .LE.   175) IW = 28
      IF((IV .GE.  1940 .AND. IV .LE.  2285) .OR.
     *   (IV .GE.  4040 .AND. IV .LE.  4370)) IW = 29
      IBAND = IW-27
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =  ACO(IBAND)
           AA(IMOL) = AACO(IBAND)
           BB(IMOL) = BBCO(IBAND)
           CC(IMOL) = CCCO(IBAND)
      ENDIF
C  ---CH4
      IMOL = 6
      IW = -1
      IF((IV .GE.  1065 .AND. IV .LE.  1775) .OR.
     *   (IV .GE.  2345 .AND. IV .LE.  3230) .OR.
     *   (IV .GE.  4110 .AND. IV .LE.  4690) .OR.
     *   (IV .GE.  5865 .AND. IV .LE.  6135))IW = 30
      IBAND = IW-29
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =  ACH4(IBAND)
           AA(IMOL) = AACH4(IBAND)
           BB(IMOL) = BBCH4(IBAND)
           CC(IMOL) = CCCH4(IBAND)
      ENDIF
C  ---N2O
      IMOL = 4
      IW = -1
      IF (IV .GE.     0 .AND. IV .LE.   120)  IW = 31
      IF((IV .GE.   490 .AND. IV .LE.   775) .OR.
     *   (IV .GE.   865 .AND. IV .LE.   995) .OR.
     *   (IV .GE.  1065 .AND. IV .LE.  1385) .OR.
     *   (IV .GE.  1545 .AND. IV .LE.  2040) .OR.
     *   (IV .GE.  2090 .AND. IV .LE.  2655)) IW = 32
      IF((IV .GE.  2705 .AND. IV .LE.  2865) .OR.
     *   (IV .GE.  3245 .AND. IV .LE.  3925) .OR.
     *   (IV .GE.  4260 .AND. IV .LE.  4470) .OR.
     *   (IV .GE.  4540 .AND. IV .LE.  4785) .OR.
     *   (IV .GE.  4910 .AND. IV .LE.  5165)) IW = 33
      IBAND = IW-30
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =  AN2O(IBAND)
           AA(IMOL) = AAN2O(IBAND)
           BB(IMOL) = BBN2O(IBAND)
           CC(IMOL) = CCN2O(IBAND)
      ENDIF
C  ---O2
      IMOL = 7
      IW = -1
      IF (IV .GE.     0 .AND. IV .LE.   265)  IW = 34
      IF((IV .GE.  7650 .AND. IV .LE.  8080) .OR.
     *   (IV .GE.  9235 .AND. IV .LE.  9490) .OR.
     *   (IV .GE. 12850 .AND. IV .LE. 13220) .OR.
     *   (IV .GE. 14300 .AND. IV .LE. 14600) .OR.
     *   (IV .GE. 15695 .AND. IV .LE. 15955)) IW = 35
       IF(IV .GE. 49600 .AND. IV. LE. 52710)  IW = 35
      IBAND = IW-33
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =  AO2(IBAND)
           IF(IV .GE. 49600 .AND. IV. LE. 52710)  A(IMOL)  = .4704
           AA(IMOL) = AAO2(IBAND)
           BB(IMOL) = BBO2(IBAND)
           CC(IMOL) = CCO2(IBAND)
      ENDIF
C  ---NH3
      IMOL = 11
      IW = -1
      IF (IV .GE.     0 .AND. IV .LE.   385)  IW = 36
      IF (IV .GE.   390 .AND. IV .LE.  2150)  IW = 37
      IBAND = IW-35
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =  ANH3(IBAND)
           AA(IMOL) = AANH3(IBAND)
           BB(IMOL) = BBNH3(IBAND)
           CC(IMOL) = CCNH3(IBAND)
      ENDIF
C  ---NO
      IMOL = 8
      IW = -1
      IF (IV .GE.  1700 .AND. IV .LE.  2005) IW  = 38
      IBAND = IW-37
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =  ANO
           AA(IMOL) = AANO
           BB(IMOL) = BBNO
           CC(IMOL) = CCNO
      ENDIF
C  ---NO2
      IW = -1
      IMOL = 10
      IF((IV .GE.   580 .AND. IV .LE.   925) .OR.
     *   (IV .GE.  1515 .AND. IV .LE.  1695) .OR.
     *   (IV .GE.  2800 .AND. IV .LE.  2970)) IW = 39
      IBAND = IW-38
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =  ANO2(IBAND)
           AA(IMOL) = AANO2(IBAND)
           BB(IMOL) = BBNO2(IBAND)
           CC(IMOL) = CCNO2(IBAND)
      ENDIF
C  ---SO2
      IMOL = 9
      IW = -1
      IF (IV .GE.     0 .AND. IV .LE.   185)  IW = 40
      IF((IV .GE.   400 .AND. IV .LE.   650) .OR.
     *   (IV .GE.   950 .AND. IV .LE.  1460) .OR.
     *   (IV .GE.  2415 .AND. IV .LE.  2580)) IW = 41
      IBAND = IW-39
      IBND(IMOL) = IW
      IF(IW .GT.  0) THEN
           A(IMOL)  =  ASO2(IBAND)
           AA(IMOL) = AASO2(IBAND)
           BB(IMOL) = BBSO2(IBAND)
           CC(IMOL) = CCSO2(IBAND)
      ENDIF
      RETURN
      END
      BLOCK DATA ABCD
C>    BLOCK DATA
      COMMON /ABC/ FACTOR(3),ANH3(2),ACO2(10),ACO(3),
     X             ACH4(4),ANO2(3),AN2O(11),AO2(6),AO3(5),
     X             ASO2(4),AH2O(14),ANO,
     X             AANH3(2),BBNH3(2),CCNH3(2),
     X             AACO2(10),BBCO2(10),CCCO2(10),
     X             AACO(3),BBCO(3),CCCO(3),
     X             AACH4(4),BBCH4(4),CCCH4(4),
     X             AANO2(3),BBNO2(3),CCNO2(3),
     X             AAN2O(11),BBN2O(11),CCN2O(11),
     X             AAO2(6),BBO2(6),CCO2(6),
     X             AAO3(5),BBO3(5),CCO3(5),
     X             AASO2(4),BBSO2(4),CCSO2(4),
     X             AAH2O(14),BBH2O(14),CCH2O(14),
     X             AANO     ,BBNO     ,CCNO
      DATA FACTOR/1.0,0.09,0.015/
      DATA ANH3/.4704,.6035/
      DATA ACO2/.6176,.6810,.6033,.6146,.6513,.6050,
     1 .6160,.7070,.7070,.7070/
      DATA ACO/.6397,.6133,.6133/
      DATA ACH4/.5844,.5844,.5844,.5844/
      DATA ANO/.6613/
      DATA ANO2/.7249,.7249,.7249/
      DATA AN2O/.8997,.7201,.7201,.7201,.7201,.7201,
     1 .6933,.6933,.6933,.6933,.6933/
      DATA AO2/.6011,.5641,.5641,.5641,.5641,.5641/
      DATA AO3/.8559,.7593,.7819,.9175,.7703/
      DATA ASO2/.8907,.8466,.8466,.8466/
      DATA AH2O/.5274,.5299,.5416,.5479,.5495,.5464,.5454,
     1 .5474,.5579,.5621,.5847,.6076,.6508,.6570/
      DATA AANH3/.285772,.134244/
      DATA BBNH3/.269839,.353937/
      DATA CCNH3/19.9507,27.8458/
      DATA AACO2/.120300,.069728,.134448,.123189,.090948,
     1 .132717,.121835,.054348,.054348,.054348/
      DATA BBCO2/.348172,.303510,.354002,.349583,.327160,
     1 .353435,.348936,.280674,.280674,.280674/
      DATA CCCO2/29.4277,37.0842,27.8241,29.0834,33.4608,
     1 28.0093,29.2436,40.1951,40.1951,40.1951/
      DATA AACO/.100401,.124454,.124454/
      DATA BBCO/.335296,.350165,.350165/
      DATA CCCO/32.0496,28.9354,28.9354/
      DATA AACH4/.154447,.154447,.154447,.154447/
      DATA BBCH4/.357657,.357657,.357657,.357657/
      DATA CCCH4/25.8920,25.8920,25.8920,25.8920/
      DATA AANO/.083336/
      DATA BBNO/.319585/
      DATA CCNO/34.6834/
      DATA AANO2/.045281,.045281,.045281/
      DATA BBNO2/.264248,.264248,.264248/
      DATA CCNO2/42.2784,42.2784,42.2784/
      DATA AAN2O/.001679,.047599,.047599,.047599,.047599,
     1 .047599,.062106,.062106,.062106,.062106,.062106/
      DATA BBN2O/.095621,.268696,.268696,.268696,.268696,
     1 .268696,.292891,.292891,.292891,.292891,.292891/
      DATA CCN2O/59.3660,41.7251,41.7251,41.7251,41.7251,
     1 41.7251,38.5667,38.5667,38.5667,38.5667,38.5667/
      DATA AAO2/.136706,.177087,.177087,.177087,.177087,.177087/
      DATA BBO2/.354683,.355447,.355447,.355447,.355447,.355447/
      DATA CCO2/27.5869,24.1314,24.1314,24.1314,24.1314,24.1314/
      DATA AAO3/.006712,.030870,.023278,.000458,.027004/
      DATA BBO3/.138026,.231722,.209952,.078492,.221153/
      DATA CCO3/55.6442,46.1189,48.5155,60.7802,47.2982/
      DATA AASO2/.002468,.008192,.008192,.008192/
      DATA BBSO2/.104307,.147065,.147065,.147065/
      DATA CCSO2/58.6298,54.8078,54.8078,54.8078/
      DATA AAH2O/.219312,.216415,.206349,.196196,.194540,.198500,
     1 .198500,.196196,.184148,.179360,.154120,.130095,.091341,.086549/
      DATA BBH2O/.334884,.336904,.343272,.348610,.349810,.347498,
     1 .347498,.348610,.353429,.354864,.357640,.352497,.327526,.322898/
      DATA CCH2O/21.8352,21.9588,22.4234,22.9517,23.0750,22.8262,
     1 22.8262,22.9517,23.6654,23.9774,25.9207,28.2957,33.3998,34.1575/
      END
      SUBROUTINE CXDTA (CPRIME,V,IWL,IWH,CP,IND)
C     THIS SUBROUTINE FINDS THE CPRIME FOR THE WAVENUMBER V.
C     INPUT:         V --- WAVENUMBER
C            (IWL,IWH) --- WAVENUMBER PAIR SPECIFIES THE ABSORPTION
C                          REGION. BOTH ARE ARRAYS AND TERMINATED
C                          WITH THE VALUE -999
C                   CP --- ARRAY CONTAINS THE CPRIMES
C     OUTPUT:   CPRIME --- THE CPRIME CORRESPONDING TO V
C     I/O:         IND --- INDICATOR INDICATES THE ABSORPTION REGION
C                          WHERE THE WAVENUMBER IS EXPECTED TO BE IN
C                          OR NEARBY (IT SERVES FOR THE PURPOSE
C                          TO SPEED UP THE SEARCHING PROCESS)
      DIMENSION IWL(*),IWH(*),CP(*)
      IV=V
      CPRIME=-20.0
      IF (IWL(IND+1) .EQ. -999 .AND. IV .GT. IWH(IND)) RETURN
      IF (IV .LT. IWL(1)) RETURN
      IC=0
  100 IF (IV .GE. IWL(IND) .AND. IV .LE. IWH(IND)) GO TO 200
      IF (IV .GT. IWH(IND) .AND. IV .LT. IWL(IND+1)) RETURN
      IND=IND+1
      IF (IWL(IND) .NE. -999) GO TO 100
      IND=IND-1
      IF (IV .GT. IWH(IND)) RETURN
      IND=1
      GO TO 100
  200 IF (IND .EQ. 1) GO TO 400
      INDM1=IND-1
      DO 300 I=1,INDM1
        IC=IC+(IWH(I)-IWL(I))/5+1
  300 CONTINUE
  400 IC=IC+(IV-IWL(IND))/5+1
      CPRIME=CP(IC)
      RETURN
      END
      SUBROUTINE C4DTA (C4L,V)
C **  N2 CONTINUUM
C **  C4,C8 IN BLOCK DATA C4D
      COMMON /C4C8/ C4(133),C8(102)
      C4L=0.
      IF(V.LT.2080.) RETURN
      IF(V.GT.2740.) RETURN
      IV=V
      L=(IV-2080)/5+1
      C4L=C4(L)
      RETURN
      END
      BLOCK DATA C4D
C>    BLOCK DATA
      COMMON /C4C8/ C401(114),C4115(19),C8(102)
C        N2 CONTINUUM ABSORPTION COEFFICIENTS
C     C4 LOCATION  1    V =  2080 CM-1
C     C4 LOCATION  133  V =  2740 CM-1
      DATA C401 /
     1 2.93E-04, 3.86E-04, 5.09E-04, 6.56E-04, 8.85E-04, 1.06E-03,
     2 1.31E-03, 1.73E-03, 2.27E-03, 2.73E-03, 3.36E-03, 3.95E-03,
     3 5.46E-03, 7.19E-03, 9.00E-03, 1.13E-02, 1.36E-02, 1.66E-02,
     4 1.96E-02, 2.16E-02, 2.36E-02, 2.63E-02, 2.90E-02, 3.15E-02,
     5 3.40E-02, 3.66E-02, 3.92E-02, 4.26E-02, 4.60E-02, 4.95E-02,
     6 5.30E-02, 5.65E-02, 6.00E-02, 6.30E-02, 6.60E-02, 6.89E-02,
     7 7.18E-02, 7.39E-02, 7.60E-02, 7.84E-02, 8.08E-02, 8.39E-02,
     8 8.70E-02, 9.13E-02, 9.56E-02, 1.08E-01, 1.20E-01, 1.36E-01,
     9 1.52E-01, 1.60E-01, 1.69E-01, 1.60E-01, 1.51E-01, 1.37E-01,
     $ 1.23E-01, 1.19E-01, 1.16E-01, 1.14E-01, 1.12E-01, 1.12E-01,
     $ 1.11E-01, 1.11E-01, 1.12E-01, 1.14E-01, 1.13E-01, 1.12E-01,
     $ 1.09E-01, 1.07E-01, 1.02E-01, 9.90E-02, 9.50E-02, 9.00E-02,
     $ 8.65E-02, 8.20E-02, 7.65E-02, 7.05E-02, 6.50E-02, 6.10E-02,
     $ 5.50E-02, 4.95E-02, 4.50E-02, 4.00E-02, 3.75E-02, 3.50E-02,
     $ 3.10E-02, 2.65E-02, 2.50E-02, 2.20E-02, 1.95E-02, 1.75E-02,
     $ 1.60E-02, 1.40E-02, 1.20E-02, 1.05E-02, 9.50E-03, 9.00E-03,
     $ 8.00E-03, 7.00E-03, 6.50E-03, 6.00E-03, 5.50E-03, 4.75E-03,
     $ 4.00E-03, 3.75E-03, 3.50E-03, 3.00E-03, 2.50E-03, 2.25E-03,
     $ 2.00E-03, 1.85E-03, 1.70E-03, 1.60E-03, 1.50E-03, 1.50E-03/
      DATA C4115 /
     1 1.54E-03, 1.50E-03, 1.47E-03, 1.34E-03, 1.25E-03, 1.06E-03,
     2 9.06E-04, 7.53E-04, 6.41E-04, 5.09E-04, 4.04E-04, 3.36E-04,
     3 2.86E-04, 2.32E-04, 1.94E-04, 1.57E-04, 1.31E-04, 1.02E-04,
     4 8.07E-05/
C        4M  H2O CONTINUUM
C        OZONE U.V. + VISIBLE BAND MODEL ABSORPTION COEFF
C     C8 LOCATION  1    V =  13000  CM-1
C     C8 LOCATION  56   V =  24200  CM-1
C        DV = 200  CM-1
C     C8 LOCATION  57   V =  27500  CM-1
C     C8 LOCATION  102  V =  50000  CM-1
C        DV = 500  CM-1
      DATA C8 /
     1 4.50E-03, 8.00E-03, 1.07E-02, 1.10E-02, 1.27E-02, 1.71E-02,
     2 2.00E-02, 2.45E-02, 3.07E-02, 3.84E-02, 4.78E-02, 5.67E-02,
     3 6.54E-02, 7.62E-02, 9.15E-02, 1.00E-01, 1.09E-01, 1.20E-01,
     4 1.28E-01, 1.12E-01, 1.11E-01, 1.16E-01, 1.19E-01, 1.13E-01,
     5 1.03E-01, 9.24E-02, 8.28E-02, 7.57E-02, 7.07E-02, 6.58E-02,
     6 5.56E-02, 4.77E-02, 4.06E-02, 3.87E-02, 3.82E-02, 2.94E-02,
     7 2.09E-02, 1.80E-02, 1.91E-02, 1.66E-02, 1.17E-02, 7.70E-03,
     8 6.10E-03, 8.50E-03, 6.10E-03, 3.70E-03, 3.20E-03, 3.10E-03,
     9 2.55E-03, 1.98E-03, 1.40E-03, 8.25E-04, 2.50E-04, 0.      ,
     $ 0.      , 0.      , 5.65E-04, 2.04E-03, 7.35E-03, 2.03E-02,
     $ 4.98E-02, 1.18E-01, 2.46E-01, 5.18E-01, 1.02E+00, 1.95E+00,
     $ 3.79E+00, 6.65E+00, 1.24E+01, 2.20E+01, 3.67E+01, 5.95E+01,
     $ 8.50E+01, 1.26E+02, 1.68E+02, 2.06E+02, 2.42E+02, 2.71E+02,
     $ 2.91E+02, 3.02E+02, 3.03E+02, 2.94E+02, 2.77E+02, 2.54E+02,
     $ 2.26E+02, 1.96E+02, 1.68E+02, 1.44E+02, 1.17E+02, 9.75E+01,
     $ 7.65E+01, 6.04E+01, 4.62E+01, 3.46E+01, 2.52E+01, 2.00E+01,
     $ 1.57E+01, 1.20E+01, 1.00E+01, 8.80E+00, 8.30E+00, 8.60E+00/
      END
      SUBROUTINE SLF296(V1C,SH2OT0)
C     LOADS SELF CONTINUUM  296K
      COMMON /SH2O/ V1,V2,DV,NPT,S296(2003)
      CALL SINT(V1,V1C,DV,NPT,S296,SH2OT0)
      RETURN
      END
      SUBROUTINE SLF260(V1C,SH2OT1)
C     LOADS SELF CONTINUUM  260K
      COMMON /S260/ V1,V2,DV,NPT,S260(2003)
      CALL SINT(V1,V1C,DV,NPT,S260,SH2OT1)
      RETURN
      END
      SUBROUTINE FRN296(V1C,FH2O)
C     LOADS FOREIGN CONTINUUM  296K
      COMMON /FH2O/ V1,V2,DV,NPT,F296(2003)
      CALL SINT(V1,V1C,DV,NPT,F296,FH2O)
      RETURN
      END
      SUBROUTINE SINT(V1,V1C,DV,NPT,CONTI,CONTO)
C
C     INTERPOLATION  FOR CONTINUUM WITH LOWTRAN
C
      DIMENSION CONTI(2003)
      CONTO=0.
      I=(V1C-V1)/DV+1.00001
      IF(I.GE.NPT)GO TO 10
      CONTO=CONTI(I)
      IMOD=DMOD(dble(V1C),10.)
      IF(IMOD.GT.0) CONTO=(CONTI(I)+CONTI(I+1))/2.
10    CONTINUE
      RETURN
      END
      SUBROUTINE O3INT(V1C,V1,DV,NPT,CONTI,CONTO)
C
C     INTERPOLATION  FOR  O3 CONTINUUM WITH LOWTRAN
C
      DIMENSION CONTI(2687)
      CONTO=0.
      I=(V1C-V1)/DV+1.00001
      IF(I.LT.1  )GO TO 10
      IF(I.GT.NPT)GO TO 10
      CONTO=CONTI(I)
10    CONTINUE
      RETURN
      END
      SUBROUTINE O3HHT0(V,C)
      COMMON /O3HH0/ V1S,V2S,DVS,NPTS,S(2687)
C
      CALL O3INT(V ,V1S,DVS,NPTS,S,C)
      RETURN
      END
      BLOCK DATA BO3HH0
C>    BLOCK DATA
C
C
C     O3HH0 CONTAINS O3 HARTLEY HUGGINS CROSS SECTIONS FOR 273K
C               UNITS OF (CM**2/MOL)*1.E-20
C
C     NOW INCLUDES MOLINA & MOLINA AT 273K WITH THE TEMPERATURE
C     DEPENDENCE DETERMINED FROM THE 195K HARVARD MEASUREMENTS,
C     EMPLOYING THE BASS ALGORITHM (CO(1+C1*T+C2*T2); THIS IS
C     ONLY FOR THE WAVELENGTH RANGE FROM .34 TO .35 MICRONS;
C     OTHERWISE, THE BASS DATA ALONE HAVE BEEN EMPLOYED BETWEEN
C     .34 AND .245 MICRONS.
C
C     NEW T-DEPENDENT X-SECTIONS BETWEEN .345 AND .36 MICRONS
C     HAVE NOW BEEN ADDED, BASED ON WORK BY CACCIANI, DISARRA
C     AND FIOCCO, UNIVERSITY OF ROME, 1987.  QUADRATIC TEMP
C     HAS BEEN DERIVED, AS ABOVE.
C
C     MOLINA & MOLINA HAVE AGAIN BEEN USED BETWEEN .245 AND .185
C     MICRONS (NO TEMPERATURE DEPENDENCE)
C
C     AGREEMENT AMONGST THE FOUR DATA SETS IS REASONABLE (<10%)
C     AND OFTEN EXCELLENT (0-3%)
C
C
      COMMON /O3HH0/  V1C,V2C,DVC,NC,
     X           O30001(80),O30081(80),O30161(80),O30241(80),O30321(80),
     X           O30401( 7),
     X           C00001(80),C00081(80),C00161(80),C00241(80),C00321(80),
     X           C00401(80),C00481(80),C00561(80),C00641(80),C00721(80),
     X           C00801(80),C00881(80),C00961(80),C01041(80),C01121(80),
     X           C01201(80),C01281(80),C01361(80),C01441(80),C01521(80),
     X           C01601(80),C01681(80),C01761(80),C01841(80),C01921(80),
     X           C02001(80),C02081(80),C02161(80),C02241(40)
C
C     DATA V1C  /27370./,V2C  /29400./,DVC  /5./,NC  /407/ INN & TANAKA
C         DATA FROM INN & TANAKA, HANDBOOK OF GEOPHYSICS, 1957, P 16-24
C                LINEARLY INTERPOLATED BY SAC, JUNE 1985
C                CONVERSION: (I&T)/(LOSCHMIDT 1 1987*1.2)
C
C     DATA V1C /29405./, V2C /40800./ ,DVC /5./, NC /2280/  BASS
C         DATA FROM BASS, JUNE 1985
C
      DATA V1C /27370./, V2C /40800./ ,DVC /5./, NC /2687/
C
      DATA O30001/
C    X 2.08858E-03, 1.98947E-03, 1.89037E-03, 1.79126E-03, 1.69215E-03,
C     THIS LINE OF DATA HAS BEEN REPLACED BY MONTONICALLY DECREASING
C     VALUES
     X 1.00000E-03, 1.15000E-03, 1.25000E-03, 1.40000E-03, 1.50000E-03,
     X 1.59304E-03, 1.62396E-03, 1.76216E-03, 1.90036E-03, 2.03856E-03,
     X 2.16538E-03, 2.02324E-03, 1.88110E-03, 1.73896E-03, 1.59682E-03,
     X 1.45468E-03, 1.31253E-03, 1.17039E-03, 1.02825E-03, 8.86108E-04,
     X 7.43963E-04, 6.01821E-04, 4.59679E-04, 5.14820E-04, 5.73044E-04,
     X 6.31269E-04, 6.89493E-04, 7.47718E-04, 8.05942E-04, 8.64167E-04,
     X 9.22392E-04, 9.80617E-04, 1.03884E-03, 1.09707E-03, 1.15528E-03,
     X 1.21351E-03, 1.27173E-03, 1.32996E-03, 1.38818E-03, 1.44641E-03,
     X 1.50463E-03, 1.56286E-03, 1.62108E-03, 1.67931E-03, 1.73753E-03,
     X 1.79575E-03, 1.85398E-03, 1.91220E-03, 1.97043E-03, 2.02865E-03,
     X 2.08688E-03, 2.14510E-03, 2.20333E-03, 2.26155E-03, 2.31978E-03,
     X 2.37800E-03, 2.43623E-03, 2.49444E-03, 2.55267E-03, 2.61089E-03,
     X 2.66912E-03, 2.72734E-03, 2.78557E-03, 2.84379E-03, 2.90202E-03,
     X 2.96024E-03, 3.01847E-03, 3.07669E-03, 3.13491E-03, 3.19313E-03,
     X 3.25136E-03, 3.30958E-03, 3.36781E-03, 3.31660E-03, 3.21583E-03,
     X 3.11505E-03, 3.22165E-03, 3.46058E-03, 3.69953E-03, 3.93846E-03/
      DATA O30081/
     X 4.17739E-03, 4.41633E-03, 4.42256E-03, 4.13791E-03, 4.17894E-03,
     X 4.25583E-03, 4.33273E-03, 4.40963E-03, 4.49259E-03, 4.44532E-03,
     X 4.17540E-03, 3.84814E-03, 3.41823E-03, 3.11003E-03, 2.86548E-03,
     X 2.73912E-03, 2.70800E-03, 2.70882E-03, 2.70866E-03, 2.70816E-03,
     X 2.71228E-03, 2.78044E-03, 2.86135E-03, 3.00163E-03, 3.15222E-03,
     X 3.33394E-03, 3.48231E-03, 3.64966E-03, 3.83242E-03, 3.97733E-03,
     X 4.10299E-03, 4.26332E-03, 4.41165E-03, 4.54040E-03, 4.65544E-03,
     X 4.91897E-03, 5.23429E-03, 5.45390E-03, 5.74420E-03, 5.96314E-03,
     X 6.07198E-03, 6.07338E-03, 5.99162E-03, 5.95079E-03, 6.04655E-03,
     X 6.18239E-03, 6.56998E-03, 6.93885E-03, 7.38561E-03, 7.73029E-03,
     X 7.90493E-03, 7.72072E-03, 7.40226E-03, 6.53860E-03, 5.30328E-03,
     X 4.23000E-03, 3.45735E-03, 3.21167E-03, 3.16694E-03, 3.30966E-03,
     X 3.47431E-03, 3.68089E-03, 3.92006E-03, 4.05246E-03, 4.16408E-03,
     X 4.08710E-03, 3.98224E-03, 4.07316E-03, 4.19498E-03, 4.44990E-03,
     X 4.77881E-03, 5.08270E-03, 5.37384E-03, 5.70240E-03, 5.91906E-03,
     X 5.96745E-03, 5.92363E-03, 5.80363E-03, 5.60812E-03, 5.37450E-03/
      DATA O30161/
     X 5.16202E-03, 4.98389E-03, 4.95294E-03, 5.04930E-03, 5.17576E-03,
     X 5.26042E-03, 5.22957E-03, 5.32404E-03, 5.39630E-03, 5.53353E-03,
     X 5.68057E-03, 5.78679E-03, 5.83795E-03, 5.93810E-03, 6.09330E-03,
     X 6.40001E-03, 6.69056E-03, 7.04863E-03, 7.41339E-03, 7.87421E-03,
     X 8.35570E-03, 8.97672E-03, 9.58486E-03, 1.01972E-02, 1.08463E-02,
     X 1.14105E-02, 1.18935E-02, 1.22404E-02, 1.25053E-02, 1.28759E-02,
     X 1.32169E-02, 1.37796E-02, 1.46488E-02, 1.57324E-02, 1.68897E-02,
     X 1.78560E-02, 1.87101E-02, 1.92197E-02, 1.94106E-02, 1.90711E-02,
     X 1.86585E-02, 1.82149E-02, 1.82219E-02, 1.85639E-02, 1.91924E-02,
     X 2.01342E-02, 2.12312E-02, 2.26362E-02, 2.39610E-02, 2.55156E-02,
     X 2.71338E-02, 2.87904E-02, 3.04268E-02, 3.17055E-02, 3.28248E-02,
     X 3.36026E-02, 3.36867E-02, 3.26393E-02, 2.99356E-02, 2.56607E-02,
     X 2.11545E-02, 1.79508E-02, 1.59757E-02, 1.49569E-02, 1.46214E-02,
     X 1.46214E-02, 1.48217E-02, 1.51379E-02, 1.53816E-02, 1.58087E-02,
     X 1.62186E-02, 1.66627E-02, 1.70961E-02, 1.76101E-02, 1.81759E-02,
     X 1.86154E-02, 1.88889E-02, 1.89577E-02, 1.89316E-02, 1.88826E-02/
      DATA O30241/
     X 1.90915E-02, 1.95550E-02, 2.02707E-02, 2.11620E-02, 2.21844E-02,
     X 2.30920E-02, 2.37270E-02, 2.37422E-02, 2.33578E-02, 2.20358E-02,
     X 1.96239E-02, 1.73329E-02, 1.57013E-02, 1.50566E-02, 1.49248E-02,
     X 1.52044E-02, 1.57658E-02, 1.63436E-02, 1.68986E-02, 1.74180E-02,
     X 1.78192E-02, 1.80677E-02, 1.79927E-02, 1.77900E-02, 1.75599E-02,
     X 1.74982E-02, 1.76674E-02, 1.81633E-02, 1.87826E-02, 1.96898E-02,
     X 2.06898E-02, 2.17167E-02, 2.28231E-02, 2.40702E-02, 2.55084E-02,
     X 2.69701E-02, 2.86915E-02, 3.05796E-02, 3.22328E-02, 3.42637E-02,
     X 3.61708E-02, 3.79118E-02, 3.94418E-02, 4.07333E-02, 4.17158E-02,
     X 4.17081E-02, 4.01127E-02, 3.65411E-02, 3.25123E-02, 2.98737E-02,
     X 2.83616E-02, 2.79907E-02, 2.80571E-02, 2.84778E-02, 2.91698E-02,
     X 2.99500E-02, 3.07468E-02, 3.13903E-02, 3.19811E-02, 3.24616E-02,
     X 3.26503E-02, 3.26829E-02, 3.27688E-02, 3.36446E-02, 3.55133E-02,
     X 3.88447E-02, 4.28854E-02, 4.55381E-02, 4.77161E-02, 4.93567E-02,
     X 4.95127E-02, 5.00492E-02, 5.06233E-02, 5.12739E-02, 5.20327E-02,
     X 5.29001E-02, 5.38677E-02, 5.49272E-02, 5.60703E-02, 5.72886E-02/
      DATA O30321/
     X 5.85739E-02, 5.99178E-02, 6.13170E-02, 6.28474E-02, 6.46499E-02,
     X 6.68672E-02, 6.96421E-02, 7.31174E-02, 7.74361E-02, 8.27413E-02,
     X 8.91756E-02, 9.67018E-02, 1.04844E-01, 1.13063E-01, 1.20818E-01,
     X 1.27567E-01, 1.32771E-01, 1.35888E-01, 1.36377E-01, 1.33780E-01,
     X 1.28385E-01, 1.20887E-01, 1.11978E-01, 1.02354E-01, 9.27108E-02,
     X 8.37418E-02, 7.61423E-02, 7.06032E-02, 6.74255E-02, 6.62092E-02,
     X 6.64813E-02, 6.77689E-02, 6.95995E-02, 7.15004E-02, 7.29991E-02,
     X 7.36229E-02, 7.29641E-02, 7.11015E-02, 6.83345E-02, 6.49638E-02,
     X 6.12897E-02, 5.76125E-02, 5.42326E-02, 5.14504E-02, 4.95645E-02,
     X 4.87078E-02, 4.87234E-02, 4.94254E-02, 5.06280E-02, 5.21454E-02,
     X 5.37919E-02, 5.53818E-02, 5.67293E-02, 5.76709E-02, 5.82319E-02,
     X 5.85334E-02, 5.86968E-02, 5.88439E-02, 5.90963E-02, 5.95756E-02,
     X 6.04035E-02, 6.17016E-02, 6.35548E-02, 6.59664E-02, 6.89282E-02,
     X 7.24326E-02, 7.64718E-02, 8.10380E-02, 8.61236E-02, 9.17211E-02,
     X 9.78192E-02, 1.04353E-01, 1.11218E-01, 1.18308E-01, 1.25519E-01,
     X 1.32745E-01, 1.39881E-01, 1.46821E-01, 1.53461E-01, 1.59687E-01/
      DATA O30401/
C    X 1.64187E-01, 1.69368E-01, 1.74549E-01, 1.79731E-01, 1.84912E-01,
C      1.90094E-01, 1.95275E-01/
C   THE VALUE AT 29400. HAS BEEN CHANGED TO PROVIDE A SMOOTH TRANSITION
C    X 1.90094E-01, 1.85275E-01/
     X 1.65365E-01, 1.70353E-01, 1.74507E-01, 1.77686E-01, 1.79748E-01,
     X 1.80549E-01, 1.79948E-01/
C
C
C    FOLLOWING DATA ARE FROM BASS JUNE 1985
C
      DATA C00001 /
     X 1.81094E-01, 1.57760E-01, 1.37336E-01, 1.19475E-01, 1.17191E-01,
     X 1.14331E-01, 1.15984E-01, 1.10412E-01, 1.12660E-01, 1.16014E-01,
     X 1.15060E-01, 1.12041E-01, 1.11611E-01, 1.00378E-01, 9.54850E-02,
     X 9.87528E-02, 9.46153E-02, 9.53093E-02, 9.72653E-02, 9.66468E-02,
     X 9.39750E-02, 1.03552E-01, 1.01361E-01, 1.04315E-01, 1.12842E-01,
     X 1.02800E-01, 1.09576E-01, 1.05577E-01, 1.17334E-01, 1.25763E-01,
     X 1.27597E-01, 1.34267E-01, 1.44799E-01, 1.57366E-01, 1.67369E-01,
     X 1.81778E-01, 1.89207E-01, 2.01376E-01, 2.10310E-01, 2.21721E-01,
     X 2.43162E-01, 2.55542E-01, 2.75312E-01, 2.88576E-01, 3.02505E-01,
     X 3.15141E-01, 3.28908E-01, 3.49000E-01, 3.56620E-01, 3.59852E-01,
     X 3.57517E-01, 3.12924E-01, 2.63610E-01, 2.50854E-01, 2.25642E-01,
     X 2.15954E-01, 2.12099E-01, 2.13039E-01, 2.12286E-01, 2.17214E-01,
     X 2.28784E-01, 2.28276E-01, 2.34677E-01, 2.30730E-01, 2.16107E-01,
     X 1.99471E-01, 1.85629E-01, 1.72730E-01, 1.56229E-01, 1.38156E-01,
     X 1.37641E-01, 1.33169E-01, 1.32759E-01, 1.30102E-01, 1.35396E-01,
     X 1.37976E-01, 1.41571E-01, 1.46448E-01, 1.44508E-01, 1.47612E-01/
      DATA C00081 /
     X 1.47424E-01, 1.48173E-01, 1.52936E-01, 1.58908E-01, 1.58808E-01,
     X 1.59860E-01, 1.73936E-01, 1.84109E-01, 1.95143E-01, 2.08267E-01,
     X 2.19256E-01, 2.31653E-01, 2.46400E-01, 2.60437E-01, 2.70792E-01,
     X 2.79749E-01, 2.91068E-01, 2.98080E-01, 3.10421E-01, 3.24540E-01,
     X 3.39003E-01, 3.58322E-01, 3.81520E-01, 4.02798E-01, 4.35972E-01,
     X 4.56220E-01, 4.79037E-01, 5.02597E-01, 5.24648E-01, 5.33964E-01,
     X 5.39211E-01, 5.43613E-01, 5.28793E-01, 4.94103E-01, 4.34481E-01,
     X 3.76792E-01, 3.37161E-01, 3.15750E-01, 3.11042E-01, 3.08745E-01,
     X 3.09195E-01, 3.05859E-01, 3.01443E-01, 2.88111E-01, 2.81303E-01,
     X 2.75329E-01, 2.60812E-01, 2.59337E-01, 2.45576E-01, 2.40470E-01,
     X 2.39705E-01, 2.45389E-01, 2.49801E-01, 2.53235E-01, 2.54387E-01,
     X 2.64311E-01, 2.74146E-01, 2.89737E-01, 2.96673E-01, 3.07337E-01,
     X 3.24380E-01, 3.42266E-01, 3.59522E-01, 3.78005E-01, 3.97178E-01,
     X 4.23351E-01, 4.45925E-01, 4.63029E-01, 4.94843E-01, 5.19418E-01,
     X 5.49928E-01, 5.69115E-01, 6.02396E-01, 6.43471E-01, 6.76401E-01,
     X 7.14024E-01, 7.42425E-01, 7.60916E-01, 7.83319E-01, 7.98299E-01/
      DATA C00161 /
     X 7.76672E-01, 7.22769E-01, 6.45967E-01, 5.80850E-01, 5.76514E-01,
     X 5.79380E-01, 5.90359E-01, 6.21721E-01, 6.37540E-01, 6.52572E-01,
     X 6.63442E-01, 6.69026E-01, 6.69038E-01, 6.53319E-01, 6.21950E-01,
     X 5.47619E-01, 4.58994E-01, 4.14888E-01, 3.97736E-01, 3.88775E-01,
     X 3.87424E-01, 3.93567E-01, 4.03442E-01, 4.05217E-01, 4.12848E-01,
     X 4.12246E-01, 4.16620E-01, 4.13195E-01, 4.08467E-01, 4.13104E-01,
     X 4.24498E-01, 4.32002E-01, 4.46361E-01, 4.61131E-01, 4.77228E-01,
     X 4.96519E-01, 5.16764E-01, 5.38966E-01, 5.54187E-01, 5.73748E-01,
     X 6.07260E-01, 6.34358E-01, 6.60286E-01, 6.95533E-01, 7.37090E-01,
     X 7.83894E-01, 8.19557E-01, 8.49244E-01, 8.91832E-01, 9.44885E-01,
     X 9.86271E-01, 1.02262E+00, 1.07242E+00, 1.12162E+00, 1.18287E+00,
     X 1.22402E+00, 1.24978E+00, 1.24392E+00, 1.19668E+00, 1.11562E+00,
     X 1.03983E+00, 9.31884E-01, 8.35307E-01, 7.92620E-01, 7.81980E-01,
     X 7.89623E-01, 8.05987E-01, 8.27344E-01, 8.57514E-01, 8.66302E-01,
     X 8.72092E-01, 8.66840E-01, 8.40536E-01, 7.87360E-01, 7.35743E-01,
     X 6.92039E-01, 6.64032E-01, 6.48360E-01, 6.46288E-01, 6.49505E-01/
      DATA C00241 /
     X 6.69937E-01, 6.81006E-01, 7.00969E-01, 7.19834E-01, 7.26964E-01,
     X 7.50591E-01, 7.73600E-01, 8.00673E-01, 8.20347E-01, 8.37855E-01,
     X 8.66780E-01, 9.04297E-01, 9.46300E-01, 9.69134E-01, 9.97928E-01,
     X 1.06388E+00, 1.11032E+00, 1.15221E+00, 1.21324E+00, 1.24462E+00,
     X 1.31978E+00, 1.35617E+00, 1.38792E+00, 1.39196E+00, 1.35161E+00,
     X 1.29381E+00, 1.30295E+00, 1.32965E+00, 1.37024E+00, 1.44064E+00,
     X 1.50484E+00, 1.57200E+00, 1.62097E+00, 1.67874E+00, 1.72676E+00,
     X 1.73383E+00, 1.66091E+00, 1.54936E+00, 1.35454E+00, 1.20070E+00,
     X 1.14609E+00, 1.13642E+00, 1.13784E+00, 1.14609E+00, 1.14531E+00,
     X 1.16024E+00, 1.16891E+00, 1.16111E+00, 1.14192E+00, 1.09903E+00,
     X 1.05745E+00, 1.02341E+00, 1.00121E+00, 1.00036E+00, 1.00576E+00,
     X 1.02405E+00, 1.04379E+00, 1.07623E+00, 1.11347E+00, 1.17305E+00,
     X 1.20016E+00, 1.22697E+00, 1.27479E+00, 1.32572E+00, 1.38690E+00,
     X 1.43768E+00, 1.48379E+00, 1.55317E+00, 1.64020E+00, 1.71268E+00,
     X 1.77183E+00, 1.85824E+00, 1.95131E+00, 2.04609E+00, 2.13151E+00,
     X 2.17777E+00, 2.22832E+00, 2.26886E+00, 2.19775E+00, 2.05087E+00/
      DATA C00321 /
     X 1.96103E+00, 1.95554E+00, 1.98037E+00, 2.05440E+00, 2.11629E+00,
     X 2.17893E+00, 2.24384E+00, 2.30464E+00, 2.32525E+00, 2.29945E+00,
     X 2.21712E+00, 2.03430E+00, 1.82139E+00, 1.70354E+00, 1.64631E+00,
     X 1.62164E+00, 1.61356E+00, 1.63900E+00, 1.66313E+00, 1.67409E+00,
     X 1.69143E+00, 1.70181E+00, 1.69165E+00, 1.67699E+00, 1.67879E+00,
     X 1.67312E+00, 1.68133E+00, 1.70002E+00, 1.72500E+00, 1.76308E+00,
     X 1.80634E+00, 1.87548E+00, 1.94924E+00, 1.99812E+00, 2.05333E+00,
     X 2.14035E+00, 2.21847E+00, 2.27412E+00, 2.29752E+00, 2.30750E+00,
     X 2.36165E+00, 2.44394E+00, 2.52782E+00, 2.61343E+00, 2.71640E+00,
     X 2.81613E+00, 2.93679E+00, 3.01577E+00, 3.15995E+00, 3.15931E+00,
     X 2.96658E+00, 2.73295E+00, 2.67480E+00, 2.66652E+00, 2.69393E+00,
     X 2.75102E+00, 2.86503E+00, 2.99163E+00, 2.99576E+00, 3.02603E+00,
     X 2.98415E+00, 2.79309E+00, 2.65337E+00, 2.50962E+00, 2.43207E+00,
     X 2.34812E+00, 2.34872E+00, 2.35186E+00, 2.39477E+00, 2.42629E+00,
     X 2.48068E+00, 2.55087E+00, 2.55952E+00, 2.56497E+00, 2.64323E+00,
     X 2.67961E+00, 2.66263E+00, 2.70243E+00, 2.74911E+00, 2.81786E+00/
      DATA C00401 /
     X 2.88684E+00, 2.97790E+00, 3.04305E+00, 3.13053E+00, 3.23857E+00,
     X 3.35582E+00, 3.40654E+00, 3.38117E+00, 3.36296E+00, 3.39480E+00,
     X 3.49066E+00, 3.60832E+00, 3.71817E+00, 3.83924E+00, 3.96355E+00,
     X 4.03656E+00, 4.00518E+00, 3.90389E+00, 3.74790E+00, 3.61385E+00,
     X 3.57066E+00, 3.59438E+00, 3.66182E+00, 3.71176E+00, 3.75255E+00,
     X 3.79101E+00, 3.85278E+00, 3.85027E+00, 3.81112E+00, 3.72553E+00,
     X 3.61017E+00, 3.54384E+00, 3.52406E+00, 3.54097E+00, 3.59375E+00,
     X 3.66312E+00, 3.72632E+00, 3.76825E+00, 3.86798E+00, 3.92916E+00,
     X 3.95610E+00, 4.00120E+00, 4.05865E+00, 4.11981E+00, 4.14634E+00,
     X 4.19109E+00, 4.20317E+00, 4.25754E+00, 4.35131E+00, 4.48573E+00,
     X 4.58716E+00, 4.67462E+00, 4.78228E+00, 4.91196E+00, 5.01871E+00,
     X 5.10663E+00, 5.17780E+00, 5.21393E+00, 5.18144E+00, 5.04379E+00,
     X 4.86504E+00, 4.78569E+00, 4.72717E+00, 4.69132E+00, 4.65797E+00,
     X 4.60305E+00, 4.59798E+00, 4.65300E+00, 4.69707E+00, 4.74790E+00,
     X 4.82581E+00, 4.80953E+00, 4.80517E+00, 4.82685E+00, 4.82321E+00,
     X 4.84806E+00, 4.88591E+00, 4.91759E+00, 4.98074E+00, 5.07071E+00/
      DATA C00481 /
     X 5.18733E+00, 5.30567E+00, 5.38670E+00, 5.43942E+00, 5.51797E+00,
     X 5.62652E+00, 5.71228E+00, 5.82347E+00, 5.91434E+00, 6.00171E+00,
     X 6.06977E+00, 6.13040E+00, 6.21990E+00, 6.29980E+00, 6.37206E+00,
     X 6.48233E+00, 6.53068E+00, 6.53275E+00, 6.56858E+00, 6.54577E+00,
     X 6.50472E+00, 6.41504E+00, 6.33853E+00, 6.31184E+00, 6.21253E+00,
     X 6.22034E+00, 6.26918E+00, 6.28982E+00, 6.29461E+00, 6.35418E+00,
     X 6.40956E+00, 6.38020E+00, 6.39784E+00, 6.45383E+00, 6.50134E+00,
     X 6.56808E+00, 6.58850E+00, 6.58882E+00, 6.65097E+00, 6.75259E+00,
     X 6.83256E+00, 6.92593E+00, 6.98083E+00, 7.03632E+00, 7.11147E+00,
     X 7.15622E+00, 7.21106E+00, 7.27319E+00, 7.33382E+00, 7.38601E+00,
     X 7.48971E+00, 7.61459E+00, 7.70134E+00, 7.76194E+00, 7.85534E+00,
     X 7.99519E+00, 8.12227E+00, 8.25461E+00, 8.34670E+00, 8.42733E+00,
     X 8.51806E+00, 8.57638E+00, 8.56481E+00, 8.55461E+00, 8.55593E+00,
     X 8.58756E+00, 8.50070E+00, 8.54400E+00, 8.57575E+00, 8.62083E+00,
     X 8.60684E+00, 8.67824E+00, 8.72069E+00, 8.79127E+00, 8.85479E+00,
     X 8.86770E+00, 8.90574E+00, 8.91531E+00, 8.94800E+00, 9.00167E+00/
      DATA C00561 /
     X 9.14051E+00, 9.25421E+00, 9.39694E+00, 9.50896E+00, 9.53190E+00,
     X 9.55977E+00, 9.53482E+00, 9.49662E+00, 9.53359E+00, 9.54007E+00,
     X 9.49809E+00, 9.49373E+00, 9.53282E+00, 9.63757E+00, 9.67855E+00,
     X 9.67633E+00, 9.67045E+00, 9.79481E+00, 9.93420E+00, 1.00234E+01,
     X 1.01372E+01, 1.02577E+01, 1.05056E+01, 1.07873E+01, 1.09967E+01,
     X 1.10873E+01, 1.11624E+01, 1.13006E+01, 1.14875E+01, 1.16106E+01,
     X 1.16744E+01, 1.17582E+01, 1.17709E+01, 1.18537E+01, 1.19623E+01,
     X 1.19763E+01, 1.19879E+01, 1.20384E+01, 1.20763E+01, 1.20826E+01,
     X 1.20449E+01, 1.19747E+01, 1.20227E+01, 1.21805E+01, 1.23134E+01,
     X 1.24042E+01, 1.25614E+01, 1.26828E+01, 1.26645E+01, 1.26963E+01,
     X 1.28226E+01, 1.28720E+01, 1.28981E+01, 1.29462E+01, 1.29363E+01,
     X 1.29199E+01, 1.29797E+01, 1.28860E+01, 1.29126E+01, 1.30205E+01,
     X 1.31327E+01, 1.31722E+01, 1.31901E+01, 1.33189E+01, 1.34833E+01,
     X 1.36228E+01, 1.37474E+01, 1.38548E+01, 1.39450E+01, 1.40926E+01,
     X 1.43099E+01, 1.44836E+01, 1.46257E+01, 1.47755E+01, 1.49163E+01,
     X 1.51038E+01, 1.53308E+01, 1.54194E+01, 1.54852E+01, 1.55968E+01/
      DATA C00641 /
     X 1.57025E+01, 1.58667E+01, 1.60365E+01, 1.61427E+01, 1.62967E+01,
     X 1.64735E+01, 1.66123E+01, 1.67268E+01, 1.67673E+01, 1.67825E+01,
     X 1.68898E+01, 1.68178E+01, 1.68216E+01, 1.68574E+01, 1.68799E+01,
     X 1.70317E+01, 1.70767E+01, 1.71508E+01, 1.72965E+01, 1.73421E+01,
     X 1.73937E+01, 1.74420E+01, 1.74535E+01, 1.75110E+01, 1.75497E+01,
     X 1.75149E+01, 1.75955E+01, 1.78260E+01, 1.78271E+01, 1.79750E+01,
     X 1.80600E+01, 1.81597E+01, 1.83454E+01, 1.85243E+01, 1.87382E+01,
     X 1.88904E+01, 1.90395E+01, 1.92759E+01, 1.95398E+01, 1.97712E+01,
     X 1.98487E+01, 1.99522E+01, 2.02363E+01, 2.03271E+01, 2.07090E+01,
     X 2.09195E+01, 2.10974E+01, 2.11702E+01, 2.12964E+01, 2.14339E+01,
     X 2.15764E+01, 2.17351E+01, 2.18486E+01, 2.19700E+01, 2.21663E+01,
     X 2.24244E+01, 2.24813E+01, 2.25248E+01, 2.26357E+01, 2.26457E+01,
     X 2.27249E+01, 2.27172E+01, 2.27123E+01, 2.26859E+01, 2.27216E+01,
     X 2.29306E+01, 2.30711E+01, 2.31374E+01, 2.31815E+01, 2.33423E+01,
     X 2.33810E+01, 2.36430E+01, 2.36807E+01, 2.36676E+01, 2.38607E+01,
     X 2.41559E+01, 2.43413E+01, 2.44401E+01, 2.45968E+01, 2.47927E+01/
      DATA C00721 /
     X 2.50743E+01, 2.53667E+01, 2.55749E+01, 2.57357E+01, 2.58927E+01,
     X 2.61523E+01, 2.64110E+01, 2.66650E+01, 2.68829E+01, 2.70635E+01,
     X 2.72797E+01, 2.75064E+01, 2.77229E+01, 2.80341E+01, 2.82003E+01,
     X 2.83346E+01, 2.83909E+01, 2.86212E+01, 2.88006E+01, 2.89577E+01,
     X 2.90965E+01, 2.91834E+01, 2.93224E+01, 2.94094E+01, 2.94848E+01,
     X 2.96584E+01, 2.96749E+01, 2.97760E+01, 2.99163E+01, 3.00238E+01,
     X 3.01290E+01, 3.02307E+01, 3.03663E+01, 3.05897E+01, 3.07937E+01,
     X 3.10403E+01, 3.11778E+01, 3.13271E+01, 3.15799E+01, 3.18435E+01,
     X 3.21614E+01, 3.25097E+01, 3.27701E+01, 3.29600E+01, 3.32583E+01,
     X 3.36348E+01, 3.40282E+01, 3.41751E+01, 3.44128E+01, 3.46199E+01,
     X 3.49363E+01, 3.52087E+01, 3.54056E+01, 3.55596E+01, 3.56694E+01,
     X 3.58104E+01, 3.60276E+01, 3.62818E+01, 3.63505E+01, 3.66069E+01,
     X 3.67544E+01, 3.70664E+01, 3.72525E+01, 3.73491E+01, 3.76006E+01,
     X 3.77102E+01, 3.78970E+01, 3.81254E+01, 3.82728E+01, 3.81720E+01,
     X 3.82781E+01, 3.84982E+01, 3.87202E+01, 3.89958E+01, 3.94148E+01,
     X 3.98434E+01, 3.98952E+01, 4.01573E+01, 4.06014E+01, 4.09651E+01/
      DATA C00801 /
     X 4.12821E+01, 4.16849E+01, 4.19899E+01, 4.22719E+01, 4.27736E+01,
     X 4.32254E+01, 4.33883E+01, 4.39831E+01, 4.39414E+01, 4.42613E+01,
     X 4.46503E+01, 4.49027E+01, 4.50384E+01, 4.52929E+01, 4.57269E+01,
     X 4.56433E+01, 4.57350E+01, 4.60128E+01, 4.60487E+01, 4.61183E+01,
     X 4.64397E+01, 4.68211E+01, 4.70706E+01, 4.72821E+01, 4.74972E+01,
     X 4.78253E+01, 4.81615E+01, 4.84480E+01, 4.85703E+01, 4.87397E+01,
     X 4.90015E+01, 4.93673E+01, 4.97291E+01, 4.99836E+01, 5.02975E+01,
     X 5.05572E+01, 5.08226E+01, 5.13433E+01, 5.17112E+01, 5.19703E+01,
     X 5.23128E+01, 5.27305E+01, 5.30599E+01, 5.34555E+01, 5.39625E+01,
     X 5.43627E+01, 5.45446E+01, 5.49263E+01, 5.53511E+01, 5.57270E+01,
     X 5.60904E+01, 5.63875E+01, 5.68475E+01, 5.73172E+01, 5.81134E+01,
     X 5.86399E+01, 5.90384E+01, 5.91417E+01, 5.90883E+01, 5.93610E+01,
     X 5.95794E+01, 5.99600E+01, 5.98493E+01, 5.99441E+01, 6.02748E+01,
     X 6.04778E+01, 6.05233E+01, 6.07194E+01, 6.11589E+01, 6.13324E+01,
     X 6.17685E+01, 6.23166E+01, 6.31055E+01, 6.38211E+01, 6.42320E+01,
     X 6.45195E+01, 6.51125E+01, 6.56765E+01, 6.59286E+01, 6.62716E+01/
      DATA C00881 /
     X 6.65693E+01, 6.68906E+01, 6.72246E+01, 6.75177E+01, 6.78476E+01,
     X 6.82599E+01, 6.84400E+01, 6.89072E+01, 6.95720E+01, 7.01410E+01,
     X 7.05519E+01, 7.09367E+01, 7.13975E+01, 7.22128E+01, 7.28222E+01,
     X 7.33808E+01, 7.38828E+01, 7.44496E+01, 7.49983E+01, 7.54178E+01,
     X 7.60554E+01, 7.62484E+01, 7.67892E+01, 7.71262E+01, 7.76235E+01,
     X 7.81413E+01, 7.85694E+01, 7.91248E+01, 7.94715E+01, 7.96200E+01,
     X 8.00270E+01, 8.03783E+01, 8.07100E+01, 8.11929E+01, 8.17375E+01,
     X 8.18410E+01, 8.23341E+01, 8.26754E+01, 8.30893E+01, 8.34232E+01,
     X 8.35533E+01, 8.36017E+01, 8.38589E+01, 8.43366E+01, 8.47593E+01,
     X 8.51614E+01, 8.55271E+01, 8.58979E+01, 8.64892E+01, 8.74367E+01,
     X 8.82440E+01, 8.89105E+01, 8.90980E+01, 8.97266E+01, 9.04886E+01,
     X 9.12709E+01, 9.21243E+01, 9.26673E+01, 9.31331E+01, 9.38190E+01,
     X 9.44877E+01, 9.50636E+01, 9.57445E+01, 9.65211E+01, 9.68623E+01,
     X 9.75356E+01, 9.81991E+01, 9.88881E+01, 9.94554E+01, 9.99292E+01,
     X 1.00357E+02, 1.00670E+02, 1.01227E+02, 1.01529E+02, 1.01889E+02,
     X 1.02033E+02, 1.02254E+02, 1.02731E+02, 1.02914E+02, 1.03120E+02/
      DATA C00961 /
     X 1.03674E+02, 1.03768E+02, 1.04146E+02, 1.04850E+02, 1.05525E+02,
     X 1.06263E+02, 1.06653E+02, 1.07084E+02, 1.07461E+02, 1.08052E+02,
     X 1.08793E+02, 1.09395E+02, 1.09811E+02, 1.10079E+02, 1.10656E+02,
     X 1.11575E+02, 1.12544E+02, 1.13453E+02, 1.14440E+02, 1.15292E+02,
     X 1.15869E+02, 1.16925E+02, 1.17854E+02, 1.18723E+02, 1.19574E+02,
     X 1.19940E+02, 1.21108E+02, 1.21807E+02, 1.22490E+02, 1.23278E+02,
     X 1.24094E+02, 1.24816E+02, 1.25469E+02, 1.26217E+02, 1.26878E+02,
     X 1.27536E+02, 1.28168E+02, 1.28682E+02, 1.29076E+02, 1.30171E+02,
     X 1.30667E+02, 1.31242E+02, 1.31665E+02, 1.31961E+02, 1.32347E+02,
     X 1.32805E+02, 1.33152E+02, 1.33869E+02, 1.34261E+02, 1.34498E+02,
     X 1.35028E+02, 1.36049E+02, 1.36577E+02, 1.37491E+02, 1.38078E+02,
     X 1.38389E+02, 1.38819E+02, 1.39653E+02, 1.39770E+02, 1.40812E+02,
     X 1.40926E+02, 1.41267E+02, 1.41872E+02, 1.42233E+02, 1.43447E+02,
     X 1.44641E+02, 1.45500E+02, 1.45996E+02, 1.47040E+02, 1.48767E+02,
     X 1.48785E+02, 1.49525E+02, 1.50266E+02, 1.50814E+02, 1.51443E+02,
     X 1.52272E+02, 1.52846E+02, 1.54000E+02, 1.54629E+02, 1.54907E+02/
      DATA C01041 /
     X 1.55527E+02, 1.56642E+02, 1.57436E+02, 1.59036E+02, 1.59336E+02,
     X 1.59661E+02, 1.60287E+02, 1.61202E+02, 1.62410E+02, 1.63040E+02,
     X 1.62872E+02, 1.63248E+02, 1.63776E+02, 1.64313E+02, 1.65782E+02,
     X 1.65692E+02, 1.66049E+02, 1.66701E+02, 1.67786E+02, 1.69150E+02,
     X 1.69996E+02, 1.71634E+02, 1.71137E+02, 1.71372E+02, 1.72525E+02,
     X 1.73816E+02, 1.75219E+02, 1.76091E+02, 1.78260E+02, 1.79299E+02,
     X 1.79904E+02, 1.81718E+02, 1.83807E+02, 1.85488E+02, 1.85929E+02,
     X 1.86787E+02, 1.88282E+02, 1.89546E+02, 1.91489E+02, 1.92646E+02,
     X 1.93399E+02, 1.93838E+02, 1.94406E+02, 1.95829E+02, 1.96745E+02,
     X 1.96978E+02, 1.97243E+02, 1.97636E+02, 1.98025E+02, 1.98227E+02,
     X 1.99552E+02, 2.00304E+02, 2.01031E+02, 2.01788E+02, 2.02432E+02,
     X 2.03817E+02, 2.04866E+02, 2.05561E+02, 2.06180E+02, 2.07024E+02,
     X 2.08303E+02, 2.09426E+02, 2.10575E+02, 2.11637E+02, 2.12559E+02,
     X 2.13361E+02, 2.14191E+02, 2.15264E+02, 2.16366E+02, 2.17316E+02,
     X 2.17717E+02, 2.17154E+02, 2.19172E+02, 2.20346E+02, 2.20849E+02,
     X 2.21539E+02, 2.22810E+02, 2.22740E+02, 2.22824E+02, 2.23285E+02/
      DATA C01121 /
     X 2.23696E+02, 2.23864E+02, 2.23968E+02, 2.23544E+02, 2.24804E+02,
     X 2.25953E+02, 2.26753E+02, 2.27732E+02, 2.29505E+02, 2.30108E+02,
     X 2.31232E+02, 2.32552E+02, 2.33979E+02, 2.36677E+02, 2.38481E+02,
     X 2.41797E+02, 2.44025E+02, 2.45113E+02, 2.47373E+02, 2.47258E+02,
     X 2.48617E+02, 2.49790E+02, 2.50562E+02, 2.51198E+02, 2.51289E+02,
     X 2.52509E+02, 2.54136E+02, 2.55335E+02, 2.55808E+02, 2.56567E+02,
     X 2.57977E+02, 2.58987E+02, 2.59622E+02, 2.60170E+02, 2.61127E+02,
     X 2.60655E+02, 2.62129E+02, 2.64020E+02, 2.65659E+02, 2.67086E+02,
     X 2.67615E+02, 2.69800E+02, 2.71452E+02, 2.73314E+02, 2.76972E+02,
     X 2.78005E+02, 2.79815E+02, 2.81709E+02, 2.84043E+02, 2.87070E+02,
     X 2.88842E+02, 2.90555E+02, 2.92401E+02, 2.94314E+02, 2.96074E+02,
     X 2.97103E+02, 2.98037E+02, 2.98113E+02, 2.97705E+02, 2.97350E+02,
     X 2.97329E+02, 2.97016E+02, 2.96752E+02, 2.96599E+02, 2.96637E+02,
     X 2.97057E+02, 2.97585E+02, 2.98179E+02, 2.98997E+02, 3.00012E+02,
     X 3.00806E+02, 3.00908E+02, 3.02369E+02, 3.04063E+02, 3.05325E+02,
     X 3.06737E+02, 3.08066E+02, 3.09694E+02, 3.11530E+02, 3.13132E+02/
      DATA C01201 /
     X 3.13296E+02, 3.15513E+02, 3.16887E+02, 3.17682E+02, 3.18296E+02,
     X 3.18654E+02, 3.18912E+02, 3.19236E+02, 3.19626E+02, 3.20020E+02,
     X 3.20186E+02, 3.20709E+02, 3.21628E+02, 3.22625E+02, 3.23504E+02,
     X 3.25479E+02, 3.26825E+02, 3.28146E+02, 3.29404E+02, 3.30512E+02,
     X 3.32634E+02, 3.34422E+02, 3.35602E+02, 3.36833E+02, 3.39372E+02,
     X 3.43446E+02, 3.46374E+02, 3.48719E+02, 3.50881E+02, 3.53160E+02,
     X 3.54890E+02, 3.57162E+02, 3.59284E+02, 3.60876E+02, 3.62295E+02,
     X 3.63987E+02, 3.64835E+02, 3.65257E+02, 3.65738E+02, 3.65904E+02,
     X 3.65976E+02, 3.66460E+02, 3.67087E+02, 3.67377E+02, 3.69079E+02,
     X 3.70694E+02, 3.70940E+02, 3.70557E+02, 3.72693E+02, 3.73852E+02,
     X 3.75679E+02, 3.77863E+02, 3.79964E+02, 3.81368E+02, 3.82716E+02,
     X 3.85556E+02, 3.89072E+02, 3.91796E+02, 3.92766E+02, 3.96551E+02,
     X 3.97833E+02, 3.97285E+02, 4.01929E+02, 4.02158E+02, 4.04553E+02,
     X 4.06451E+02, 4.06236E+02, 4.08135E+02, 4.07797E+02, 4.08415E+02,
     X 4.10111E+02, 4.11781E+02, 4.12735E+02, 4.11547E+02, 4.11606E+02,
     X 4.13548E+02, 4.12557E+02, 4.12923E+02, 4.12866E+02, 4.13009E+02/
      DATA C01281 /
     X 4.14447E+02, 4.16032E+02, 4.17032E+02, 4.19064E+02, 4.22458E+02,
     X 4.26021E+02, 4.25192E+02, 4.25684E+02, 4.27536E+02, 4.29972E+02,
     X 4.31994E+02, 4.36037E+02, 4.39132E+02, 4.40363E+02, 4.40716E+02,
     X 4.40342E+02, 4.42063E+02, 4.44408E+02, 4.45454E+02, 4.47835E+02,
     X 4.48256E+02, 4.48831E+02, 4.50257E+02, 4.51427E+02, 4.52373E+02,
     X 4.53899E+02, 4.55496E+02, 4.56311E+02, 4.57314E+02, 4.59922E+02,
     X 4.61048E+02, 4.59840E+02, 4.62144E+02, 4.63152E+02, 4.64565E+02,
     X 4.66715E+02, 4.69380E+02, 4.70751E+02, 4.72012E+02, 4.73482E+02,
     X 4.75524E+02, 4.79307E+02, 4.82035E+02, 4.84423E+02, 4.86712E+02,
     X 4.88754E+02, 4.90102E+02, 4.92047E+02, 4.94150E+02, 4.95375E+02,
     X 4.95828E+02, 4.97555E+02, 4.98559E+02, 4.97618E+02, 4.99265E+02,
     X 4.99979E+02, 5.00681E+02, 5.01386E+02, 5.00868E+02, 5.01935E+02,
     X 5.03151E+02, 5.04329E+02, 5.05546E+02, 5.08259E+02, 5.09222E+02,
     X 5.09818E+02, 5.11397E+02, 5.12391E+02, 5.13326E+02, 5.14329E+02,
     X 5.15443E+02, 5.16533E+02, 5.21417E+02, 5.25071E+02, 5.26581E+02,
     X 5.27762E+02, 5.29274E+02, 5.31704E+02, 5.34310E+02, 5.35727E+02/
      DATA C01361 /
     X 5.36838E+02, 5.37082E+02, 5.36733E+02, 5.36170E+02, 5.36063E+02,
     X 5.36451E+02, 5.37870E+02, 5.40475E+02, 5.42268E+02, 5.41972E+02,
     X 5.42532E+02, 5.44764E+02, 5.46844E+02, 5.47525E+02, 5.49150E+02,
     X 5.52049E+02, 5.55423E+02, 5.56259E+02, 5.57424E+02, 5.59189E+02,
     X 5.61167E+02, 5.64512E+02, 5.66753E+02, 5.68183E+02, 5.69628E+02,
     X 5.73474E+02, 5.76192E+02, 5.78058E+02, 5.79588E+02, 5.81619E+02,
     X 5.83530E+02, 5.84852E+02, 5.85326E+02, 5.88130E+02, 5.90570E+02,
     X 5.91785E+02, 5.91371E+02, 5.90931E+02, 5.90942E+02, 5.91168E+02,
     X 5.91291E+02, 5.89791E+02, 5.91146E+02, 5.90804E+02, 5.87847E+02,
     X 5.89067E+02, 5.91027E+02, 5.90951E+02, 5.89227E+02, 5.93389E+02,
     X 5.92921E+02, 5.92739E+02, 5.94544E+02, 5.98941E+02, 6.02302E+02,
     X 6.03908E+02, 6.04265E+02, 6.06737E+02, 6.08560E+02, 6.11272E+02,
     X 6.14992E+02, 6.18595E+02, 6.20930E+02, 6.22107E+02, 6.22957E+02,
     X 6.26710E+02, 6.28657E+02, 6.30132E+02, 6.31543E+02, 6.33043E+02,
     X 6.36932E+02, 6.38248E+02, 6.37126E+02, 6.41648E+02, 6.48274E+02,
     X 6.52638E+02, 6.53922E+02, 6.56647E+02, 6.59351E+02, 6.60525E+02/
      DATA C01441 /
     X 6.60130E+02, 6.61375E+02, 6.62660E+02, 6.63976E+02, 6.65181E+02,
     X 6.64820E+02, 6.64458E+02, 6.64927E+02, 6.66555E+02, 6.66759E+02,
     X 6.68218E+02, 6.70323E+02, 6.72703E+02, 6.76085E+02, 6.79180E+02,
     X 6.80850E+02, 6.80017E+02, 6.79928E+02, 6.80886E+02, 6.82038E+02,
     X 6.82271E+02, 6.84057E+02, 6.85309E+02, 6.86816E+02, 6.90180E+02,
     X 6.93205E+02, 6.95870E+02, 6.98794E+02, 7.03776E+02, 7.04010E+02,
     X 7.05041E+02, 7.07254E+02, 7.07432E+02, 7.10736E+02, 7.13791E+02,
     X 7.15542E+02, 7.16468E+02, 7.17412E+02, 7.17783E+02, 7.17340E+02,
     X 7.18184E+02, 7.18716E+02, 7.18809E+02, 7.18282E+02, 7.20317E+02,
     X 7.18568E+02, 7.16274E+02, 7.19119E+02, 7.20852E+02, 7.21727E+02,
     X 7.22607E+02, 7.26369E+02, 7.26412E+02, 7.27101E+02, 7.29404E+02,
     X 7.30786E+02, 7.30910E+02, 7.30656E+02, 7.30566E+02, 7.33408E+02,
     X 7.37064E+02, 7.39178E+02, 7.36713E+02, 7.37365E+02, 7.40861E+02,
     X 7.45281E+02, 7.46178E+02, 7.46991E+02, 7.48035E+02, 7.49777E+02,
     X 7.54665E+02, 7.56585E+02, 7.57408E+02, 7.58131E+02, 7.58155E+02,
     X 7.60838E+02, 7.64792E+02, 7.68161E+02, 7.69263E+02, 7.73166E+02/
      DATA C01521 /
     X 7.79006E+02, 7.82037E+02, 7.83109E+02, 7.84674E+02, 7.87444E+02,
     X 7.89510E+02, 7.90130E+02, 7.91364E+02, 7.95225E+02, 8.03599E+02,
     X 8.06340E+02, 8.05105E+02, 8.05120E+02, 8.08515E+02, 8.10907E+02,
     X 8.11388E+02, 8.13432E+02, 8.12579E+02, 8.10564E+02, 8.08719E+02,
     X 8.07682E+02, 8.05009E+02, 8.01754E+02, 8.01013E+02, 7.99926E+02,
     X 7.99067E+02, 7.98369E+02, 7.94090E+02, 7.92883E+02, 7.94244E+02,
     X 7.98220E+02, 7.98201E+02, 7.98332E+02, 7.99289E+02, 8.02355E+02,
     X 8.03621E+02, 8.05302E+02, 8.08368E+02, 8.09983E+02, 8.11529E+02,
     X 8.13068E+02, 8.14717E+02, 8.16441E+02, 8.19241E+02, 8.22944E+02,
     X 8.23768E+02, 8.25030E+02, 8.26103E+02, 8.26374E+02, 8.28331E+02,
     X 8.32620E+02, 8.38618E+02, 8.43666E+02, 8.45212E+02, 8.46324E+02,
     X 8.48536E+02, 8.50192E+02, 8.53083E+02, 8.56653E+02, 8.59614E+02,
     X 8.62000E+02, 8.64593E+02, 8.67678E+02, 8.70908E+02, 8.73408E+02,
     X 8.74779E+02, 8.74005E+02, 8.76718E+02, 8.80445E+02, 8.84365E+02,
     X 8.83806E+02, 8.84292E+02, 8.85539E+02, 8.87474E+02, 8.84905E+02,
     X 8.84039E+02, 8.85105E+02, 8.83733E+02, 8.82224E+02, 8.79865E+02/
      DATA C01601 /
     X 8.75663E+02, 8.75575E+02, 8.73144E+02, 8.68602E+02, 8.70278E+02,
     X 8.69659E+02, 8.68701E+02, 8.69250E+02, 8.71057E+02, 8.72860E+02,
     X 8.74361E+02, 8.74458E+02, 8.77576E+02, 8.81613E+02, 8.84358E+02,
     X 8.87440E+02, 8.91549E+02, 8.96568E+02, 8.99836E+02, 9.02880E+02,
     X 9.05428E+02, 9.06891E+02, 9.07349E+02, 9.10151E+02, 9.15917E+02,
     X 9.16197E+02, 9.18571E+02, 9.21219E+02, 9.20292E+02, 9.21949E+02,
     X 9.24509E+02, 9.27454E+02, 9.29474E+02, 9.31348E+02, 9.32818E+02,
     X 9.32658E+02, 9.36280E+02, 9.39512E+02, 9.39667E+02, 9.44078E+02,
     X 9.47196E+02, 9.48291E+02, 9.46150E+02, 9.46918E+02, 9.49093E+02,
     X 9.51372E+02, 9.53109E+02, 9.56308E+02, 9.61335E+02, 9.58214E+02,
     X 9.56188E+02, 9.55660E+02, 9.58633E+02, 9.57541E+02, 9.54879E+02,
     X 9.51663E+02, 9.52839E+02, 9.52055E+02, 9.49253E+02, 9.50187E+02,
     X 9.50323E+02, 9.50937E+02, 9.54362E+02, 9.55855E+02, 9.56350E+02,
     X 9.55908E+02, 9.57963E+02, 9.61866E+02, 9.66948E+02, 9.69786E+02,
     X 9.74302E+02, 9.79061E+02, 9.82465E+02, 9.86019E+02, 9.89930E+02,
     X 9.94294E+02, 9.97011E+02, 9.98207E+02, 9.98607E+02, 1.00175E+03/
      DATA C01681 /
     X 1.00275E+03, 1.00284E+03, 1.00294E+03, 1.00485E+03, 1.00593E+03,
     X 1.00524E+03, 1.00415E+03, 1.00335E+03, 1.00278E+03, 1.00185E+03,
     X 9.99982E+02, 9.98177E+02, 9.97959E+02, 9.99161E+02, 9.98810E+02,
     X 9.95415E+02, 9.94342E+02, 9.92998E+02, 9.91340E+02, 9.90900E+02,
     X 9.90407E+02, 9.89232E+02, 9.85447E+02, 9.86312E+02, 9.87461E+02,
     X 9.86090E+02, 9.86670E+02, 9.85534E+02, 9.81877E+02, 9.84946E+02,
     X 9.86392E+02, 9.86709E+02, 9.88086E+02, 9.90269E+02, 9.92566E+02,
     X 9.94029E+02, 9.95795E+02, 9.97788E+02, 1.00005E+03, 1.00287E+03,
     X 1.00566E+03, 1.00833E+03, 1.00982E+03, 1.01348E+03, 1.01862E+03,
     X 1.02322E+03, 1.02786E+03, 1.03179E+03, 1.03339E+03, 1.03833E+03,
     X 1.04317E+03, 1.04598E+03, 1.04753E+03, 1.04981E+03, 1.05321E+03,
     X 1.05492E+03, 1.05721E+03, 1.05978E+03, 1.06033E+03, 1.06107E+03,
     X 1.06155E+03, 1.06035E+03, 1.05838E+03, 1.05649E+03, 1.05553E+03,
     X 1.05498E+03, 1.05387E+03, 1.05171E+03, 1.04877E+03, 1.04725E+03,
     X 1.04748E+03, 1.04733E+03, 1.04704E+03, 1.04643E+03, 1.04411E+03,
     X 1.04435E+03, 1.04520E+03, 1.04233E+03, 1.04047E+03, 1.03992E+03/
      DATA C01761 /
     X 1.04192E+03, 1.04171E+03, 1.04140E+03, 1.04197E+03, 1.04415E+03,
     X 1.04548E+03, 1.04533E+03, 1.04616E+03, 1.04705E+03, 1.04800E+03,
     X 1.05025E+03, 1.05219E+03, 1.05412E+03, 1.05808E+03, 1.06062E+03,
     X 1.06292E+03, 1.06780E+03, 1.07219E+03, 1.07610E+03, 1.07913E+03,
     X 1.08405E+03, 1.08798E+03, 1.08835E+03, 1.09140E+03, 1.09447E+03,
     X 1.09676E+03, 1.10015E+03, 1.10272E+03, 1.10410E+03, 1.10749E+03,
     X 1.10991E+03, 1.11121E+03, 1.10981E+03, 1.10981E+03, 1.11063E+03,
     X 1.10714E+03, 1.10500E+03, 1.10357E+03, 1.10093E+03, 1.09898E+03,
     X 1.09679E+03, 1.09188E+03, 1.09088E+03, 1.09040E+03, 1.08586E+03,
     X 1.08178E+03, 1.07752E+03, 1.07243E+03, 1.07178E+03, 1.07084E+03,
     X 1.06693E+03, 1.06527E+03, 1.06405E+03, 1.06285E+03, 1.06287E+03,
     X 1.06276E+03, 1.06221E+03, 1.06464E+03, 1.06579E+03, 1.06498E+03,
     X 1.06596E+03, 1.06812E+03, 1.07159E+03, 1.07361E+03, 1.07556E+03,
     X 1.07751E+03, 1.08128E+03, 1.08523E+03, 1.08927E+03, 1.09193E+03,
     X 1.09612E+03, 1.10133E+03, 1.10435E+03, 1.10781E+03, 1.11168E+03,
     X 1.11641E+03, 1.12217E+03, 1.12839E+03, 1.13298E+03, 1.13575E+03/
      DATA C01841 /
     X 1.13742E+03, 1.13929E+03, 1.14132E+03, 1.14340E+03, 1.14518E+03,
     X 1.14742E+03, 1.14943E+03, 1.14935E+03, 1.14975E+03, 1.15086E+03,
     X 1.15420E+03, 1.15267E+03, 1.15007E+03, 1.15155E+03, 1.14982E+03,
     X 1.14663E+03, 1.14301E+03, 1.13986E+03, 1.13676E+03, 1.13307E+03,
     X 1.12898E+03, 1.12516E+03, 1.12284E+03, 1.12068E+03, 1.11855E+03,
     X 1.11632E+03, 1.11464E+03, 1.11318E+03, 1.11180E+03, 1.11163E+03,
     X 1.11160E+03, 1.11035E+03, 1.11178E+03, 1.11395E+03, 1.11447E+03,
     X 1.11439E+03, 1.11440E+03, 1.11582E+03, 1.11560E+03, 1.11478E+03,
     X 1.11448E+03, 1.11454E+03, 1.11494E+03, 1.11607E+03, 1.11736E+03,
     X 1.11854E+03, 1.11875E+03, 1.11989E+03, 1.12165E+03, 1.12427E+03,
     X 1.12620E+03, 1.12758E+03, 1.12774E+03, 1.12870E+03, 1.13001E+03,
     X 1.13006E+03, 1.13078E+03, 1.13172E+03, 1.12971E+03, 1.12857E+03,
     X 1.12810E+03, 1.12740E+03, 1.12659E+03, 1.12564E+03, 1.12338E+03,
     X 1.12117E+03, 1.11902E+03, 1.11878E+03, 1.11855E+03, 1.11828E+03,
     X 1.11791E+03, 1.11784E+03, 1.11815E+03, 1.11957E+03, 1.12046E+03,
     X 1.12042E+03, 1.11929E+03, 1.12074E+03, 1.12708E+03, 1.12600E+03/
      DATA C01921 /
     X 1.12538E+03, 1.12871E+03, 1.13167E+03, 1.13388E+03, 1.13444E+03,
     X 1.13595E+03, 1.13801E+03, 1.14096E+03, 1.14230E+03, 1.14304E+03,
     X 1.14421E+03, 1.14580E+03, 1.14767E+03, 1.15000E+03, 1.15126E+03,
     X 1.15181E+03, 1.15197E+03, 1.15364E+03, 1.15626E+03, 1.15538E+03,
     X 1.15636E+03, 1.15908E+03, 1.16024E+03, 1.16188E+03, 1.16411E+03,
     X 1.16310E+03, 1.16430E+03, 1.16927E+03, 1.17035E+03, 1.17052E+03,
     X 1.17013E+03, 1.16968E+03, 1.16969E+03, 1.17106E+03, 1.17123E+03,
     X 1.17006E+03, 1.16536E+03, 1.16087E+03, 1.15691E+03, 1.15608E+03,
     X 1.15388E+03, 1.15077E+03, 1.14967E+03, 1.14793E+03, 1.14554E+03,
     X 1.14212E+03, 1.13908E+03, 1.13654E+03, 1.13499E+03, 1.13308E+03,
     X 1.13033E+03, 1.13051E+03, 1.13073E+03, 1.12898E+03, 1.12941E+03,
     X 1.13051E+03, 1.13086E+03, 1.13189E+03, 1.13304E+03, 1.13192E+03,
     X 1.13131E+03, 1.13110E+03, 1.13499E+03, 1.13914E+03, 1.14359E+03,
     X 1.14383E+03, 1.14390E+03, 1.14435E+03, 1.14540E+03, 1.14646E+03,
     X 1.14716E+03, 1.14880E+03, 1.15062E+03, 1.15170E+03, 1.15093E+03,
     X 1.14926E+03, 1.15133E+03, 1.15167E+03, 1.15043E+03, 1.15134E+03/
      DATA C02001 /
     X 1.15135E+03, 1.15000E+03, 1.15087E+03, 1.15118E+03, 1.14935E+03,
     X 1.14780E+03, 1.14647E+03, 1.14560E+03, 1.14404E+03, 1.14238E+03,
     X 1.14406E+03, 1.14245E+03, 1.13781E+03, 1.13664E+03, 1.13653E+03,
     X 1.13778E+03, 1.13813E+03, 1.13794E+03, 1.13681E+03, 1.13515E+03,
     X 1.13328E+03, 1.13132E+03, 1.13080E+03, 1.13130E+03, 1.13400E+03,
     X 1.13526E+03, 1.13494E+03, 1.13193E+03, 1.12898E+03, 1.12654E+03,
     X 1.12739E+03, 1.12849E+03, 1.12774E+03, 1.12733E+03, 1.12733E+03,
     X 1.12943E+03, 1.13014E+03, 1.12967E+03, 1.12731E+03, 1.12671E+03,
     X 1.12885E+03, 1.13050E+03, 1.13201E+03, 1.13345E+03, 1.13488E+03,
     X 1.13605E+03, 1.13530E+03, 1.13737E+03, 1.14186E+03, 1.14250E+03,
     X 1.14305E+03, 1.14383E+03, 1.14510E+03, 1.14659E+03, 1.14848E+03,
     X 1.14949E+03, 1.14995E+03, 1.14934E+03, 1.15058E+03, 1.15368E+03,
     X 1.15435E+03, 1.15422E+03, 1.15296E+03, 1.15228E+03, 1.15189E+03,
     X 1.15198E+03, 1.15081E+03, 1.14881E+03, 1.14562E+03, 1.14276E+03,
     X 1.14030E+03, 1.13637E+03, 1.13254E+03, 1.12942E+03, 1.12653E+03,
     X 1.12362E+03, 1.11987E+03, 1.11712E+03, 1.11522E+03, 1.11403E+03/
      DATA C02081 /
     X 1.11226E+03, 1.10947E+03, 1.10956E+03, 1.10976E+03, 1.10748E+03,
     X 1.10673E+03, 1.10688E+03, 1.10675E+03, 1.10533E+03, 1.10230E+03,
     X 1.10384E+03, 1.10496E+03, 1.10274E+03, 1.10197E+03, 1.10196E+03,
     X 1.10278E+03, 1.10257E+03, 1.10147E+03, 1.10205E+03, 1.10308E+03,
     X 1.10478E+03, 1.10358E+03, 1.10197E+03, 1.10305E+03, 1.10390E+03,
     X 1.10456E+03, 1.10526E+03, 1.10588E+03, 1.10640E+03, 1.10747E+03,
     X 1.10904E+03, 1.11214E+03, 1.11350E+03, 1.11359E+03, 1.11604E+03,
     X 1.11706E+03, 1.11594E+03, 1.11600E+03, 1.11616E+03, 1.11561E+03,
     X 1.11556E+03, 1.11547E+03, 1.11370E+03, 1.11289E+03, 1.11276E+03,
     X 1.11338E+03, 1.11437E+03, 1.11595E+03, 1.11309E+03, 1.10958E+03,
     X 1.10887E+03, 1.10573E+03, 1.10068E+03, 1.10194E+03, 1.10165E+03,
     X 1.09813E+03, 1.09973E+03, 1.10233E+03, 1.10121E+03, 1.10097E+03,
     X 1.10149E+03, 1.10162E+03, 1.10222E+03, 1.10389E+03, 1.10315E+03,
     X 1.10158E+03, 1.10193E+03, 1.10186E+03, 1.10135E+03, 1.10336E+03,
     X 1.10500E+03, 1.10459E+03, 1.10592E+03, 1.10784E+03, 1.10076E+03,
     X 1.09615E+03, 1.09496E+03, 1.09422E+03, 1.09350E+03, 1.09244E+03/
      DATA C02161 /
     X 1.08955E+03, 1.08535E+03, 1.08379E+03, 1.08184E+03, 1.07889E+03,
     X 1.07563E+03, 1.07238E+03, 1.07042E+03, 1.06882E+03, 1.06761E+03,
     X 1.06816E+03, 1.06772E+03, 1.06327E+03, 1.06313E+03, 1.06563E+03,
     X 1.06254E+03, 1.06072E+03, 1.06095E+03, 1.06173E+03, 1.06269E+03,
     X 1.06361E+03, 1.06438E+03, 1.06501E+03, 1.06465E+03, 1.06481E+03,
     X 1.06685E+03, 1.06642E+03, 1.06447E+03, 1.06701E+03, 1.06791E+03,
     X 1.06612E+03, 1.06471E+03, 1.06403E+03, 1.06774E+03, 1.06823E+03,
     X 1.06524E+03, 1.06479E+03, 1.06453E+03, 1.06346E+03, 1.06175E+03,
     X 1.05958E+03, 1.05941E+03, 1.05936E+03, 1.05938E+03, 1.05736E+03,
     X 1.05449E+03, 1.05307E+03, 1.05180E+03, 1.05074E+03, 1.04810E+03,
     X 1.04536E+03, 1.04477E+03, 1.04389E+03, 1.04272E+03, 1.04006E+03,
     X 1.03739E+03, 1.03533E+03, 1.03476E+03, 1.03516E+03, 1.03275E+03,
     X 1.03093E+03, 1.03062E+03, 1.02997E+03, 1.02919E+03, 1.02993E+03,
     X 1.02983E+03, 1.02837E+03, 1.02611E+03, 1.02386E+03, 1.02426E+03,
     X 1.02542E+03, 1.02750E+03, 1.02638E+03, 1.02496E+03, 1.02608E+03,
     X 1.02568E+03, 1.02388E+03, 1.02522E+03, 1.02692E+03, 1.02834E+03/
      DATA C02241 /
     X 1.02828E+03, 1.02716E+03, 1.02667E+03, 1.02607E+03, 1.02503E+03,
     X 1.02723E+03, 1.03143E+03, 1.02881E+03, 1.02646E+03, 1.02500E+03,
     X 1.02569E+03, 1.02743E+03, 1.02608E+03, 1.02548E+03, 1.02620E+03,
     X 1.02733E+03, 1.02839E+03, 1.02575E+03, 1.02432E+03, 1.02471E+03,
     X 1.02392E+03, 1.02267E+03, 1.02077E+03, 1.01964E+03, 1.01957E+03,
     X 1.01848E+03, 1.01704E+03, 1.01524E+03, 1.01352E+03, 1.01191E+03,
     X 1.01066E+03, 1.00952E+03, 1.00849E+03, 1.00660E+03, 1.00368E+03,
     X 9.99713E+02, 9.95921E+02, 9.94845E+02, 9.93286E+02, 9.91204E+02/
C
      END
      SUBROUTINE O3HHT1(V,C)
C     SUBROUTINE O3HHT1(V1C,V2C,DVC,NPTC,C)
      COMMON /O3HH1/ V1S,V2S,DVS,NPTS,S(2690)
C
      CALL O3INT(V ,V1S,DVS,NPTS,S,C)
C
      RETURN
      END
      BLOCK DATA BO3HH1
C>    BLOCK DATA
C
C     RATIO (C1/C0)
C     DATA FROM BASS 1985
C
C     NOW INCLUDES MOLINA & MOLINA AT 273K WITH THE TEMPERATURE
C     DEPENDENCE DETERMINED FROM THE 195K HARVARD MEASUREMENTS,
C     EMPLOYING THE BASS ALGORITHM (CO(1+C1*T+C2*T2); THIS IS
C     ONLY FOR THE WAVELENGTH RANGE FROM .34 TO .35 MICRONS;
C     OTHERWISE, THE BASS DATA ALONE HAVE BEEN EMPLOYED BETWEEN
C     .34 AND .245 MICRONS.
C
C     NEW T-DEPENDENT X-SECTIONS BETWEEN .345 AND .36 MICRONS
C     HAVE NOW BEEN ADDED, BASED ON WORK BY CACCIANI, DISARRA
C     AND FIOCCO, UNIVERSITY OF ROME, 1987.  QUADRATIC TEMP
C     HAS BEEN DERIVED, AS ABOVE.
C
C     AGREEMENT AMONGST THE FOUR DATA SETS IS REASONABLE (<10%)
C     AND OFTEN EXCELLENT (0-3%)
C
C
      COMMON /O3HH1/  V1C,V2C,DVC,NC,
     X           O31001(88),C10086(80),C10166(80),C10246(65),C10311(16),
     X           C10327(80),C10407(1),
     X           C10001(80),C10081(80),C10161(80),C10241(80),C10321(80),
     X           C10401(80),C10481(80),C10561(80),C10641(80),C10721(80),
     X           C10801(80),C10881(80),C10961(80),C11041(80),C11121(80),
     X           C11201(80),C11281(80),C11361(80),C11441(80),C11521(80),
     X           C11601(80),C11681(80),C11761(80),C11841(80),C11921(80),
     X           C12001(80),C12081(80),C12161(80),C12241(40)
C
C     DATA V1C /29405./, V2C /40800./ ,DVC /5./, NC /2280/   BASS
      DATA V1C /27370./, V2C /40800./ ,DVC /5./, NC /2690/
C
      DATA O31001/88*1.3E-3/

      DATA C10086/
     X 1.37330E-03, 1.62821E-03, 2.01703E-03, 2.54574E-03, 3.20275E-03,
     X 3.89777E-03, 4.62165E-03, 5.26292E-03, 5.86986E-03, 6.41494E-03,
     X 6.96761E-03, 7.48539E-03, 7.89600E-03, 7.87305E-03, 7.81981E-03,
     X 7.63864E-03, 7.67455E-03, 7.72586E-03, 7.69784E-03, 7.57367E-03,
     X 7.27336E-03, 7.14064E-03, 7.24207E-03, 7.09851E-03, 6.93654E-03,
     X 6.89385E-03, 7.05768E-03, 6.85578E-03, 6.58301E-03, 6.50848E-03,
     X 6.52083E-03, 6.46590E-03, 6.70692E-03, 6.92053E-03, 7.17734E-03,
     X 7.05364E-03, 6.63440E-03, 6.54702E-03, 6.27173E-03, 5.98150E-03,
     X 5.66579E-03, 5.51549E-03, 5.50291E-03, 5.93271E-03, 6.36950E-03,
     X 7.18562E-03, 7.51767E-03, 6.53815E-03, 7.22341E-03, 8.63056E-03,
     X 9.11740E-03, 8.80903E-03, 8.59902E-03, 7.74287E-03, 7.33509E-03,
     X 7.50180E-03, 7.81686E-03, 7.85635E-03, 8.08554E-03, 7.21968E-03,
     X 7.99028E-03, 9.90724E-03, 1.29121E-02, 1.54686E-02, 1.60876E-02,
     X 1.59530E-02, 1.57040E-02, 1.59499E-02, 1.63961E-02, 1.72670E-02,
     X 1.81634E-02, 1.95519E-02, 2.14181E-02, 2.28670E-02, 2.33506E-02,
     X 2.22736E-02, 2.14296E-02, 2.15271E-02, 2.30730E-02, 2.36220E-02/
      DATA C10166/
     X 2.44466E-02, 2.44476E-02, 2.39223E-02, 2.41386E-02, 2.53687E-02,
     X 2.67491E-02, 2.80425E-02, 2.77558E-02, 2.82626E-02, 2.86776E-02,
     X 2.88781E-02, 2.89248E-02, 2.89983E-02, 2.85534E-02, 2.87102E-02,
     X 2.83695E-02, 2.76719E-02, 2.76091E-02, 2.90733E-02, 2.80388E-02,
     X 2.73706E-02, 2.65055E-02, 2.61268E-02, 2.45892E-02, 2.37213E-02,
     X 2.22542E-02, 2.10116E-02, 2.02852E-02, 1.97635E-02, 1.94079E-02,
     X 1.90997E-02, 1.85598E-02, 1.79221E-02, 1.77887E-02, 1.73709E-02,
     X 1.67263E-02, 1.60932E-02, 1.50775E-02, 1.39563E-02, 1.23691E-02,
     X 1.07402E-02, 9.35859E-03, 8.43786E-03, 7.92075E-03, 7.33239E-03,
     X 6.73638E-03, 6.28740E-03, 5.85640E-03, 5.85384E-03, 6.10577E-03,
     X 7.26050E-03, 9.66384E-03, 1.29629E-02, 1.69596E-02, 2.03465E-02,
     X 2.26429E-02, 2.39653E-02, 2.47970E-02, 2.51993E-02, 2.51383E-02,
     X 2.52014E-02, 2.47766E-02, 2.47171E-02, 2.47478E-02, 2.43986E-02,
     X 2.43498E-02, 2.40537E-02, 2.40574E-02, 2.40446E-02, 2.40847E-02,
     X 2.39400E-02, 2.42127E-02, 2.47123E-02, 2.52914E-02, 2.52103E-02,
     X 2.51421E-02, 2.43229E-02, 2.37902E-02, 2.30865E-02, 2.28174E-02/
      DATA C10246/
     X 2.28830E-02, 2.33671E-02, 2.38274E-02, 2.46699E-02, 2.56739E-02,
     X 2.61408E-02, 2.62898E-02, 2.64228E-02, 2.55561E-02, 2.47095E-02,
     X 2.39071E-02, 2.34319E-02, 2.28738E-02, 2.23434E-02, 2.18888E-02,
     X 2.13639E-02, 2.11937E-02, 2.10110E-02, 2.07672E-02, 2.00697E-02,
     X 1.97605E-02, 1.91208E-02, 1.82056E-02, 1.73945E-02, 1.64542E-02,
     X 1.53969E-02, 1.41816E-02, 1.35665E-02, 1.27109E-02, 1.18254E-02,
     X 1.11489E-02, 1.03984E-02, 1.00760E-02, 9.86649E-03, 9.76766E-03,
     X 9.41662E-03, 9.19082E-03, 9.44272E-03, 1.04547E-02, 1.24713E-02,
     X 1.49310E-02, 1.70272E-02, 1.86057E-02, 1.93555E-02, 1.98350E-02,
     X 2.00041E-02, 2.01233E-02, 2.01917E-02, 1.98918E-02, 1.96649E-02,
     X 1.95162E-02, 2.01044E-02, 2.06711E-02, 2.08881E-02, 2.04812E-02,
     X 1.92249E-02, 1.80188E-02, 1.69496E-02, 1.60488E-02, 1.52865E-02,
     X 1.46940E-02, 1.41067E-02, 1.35675E-02, 1.31094E-02, 1.27542E-02/
      DATA C10311/
     X                                                     1.3073E-02,
     X 1.2795E-02,  1.2753E-02,  1.2868E-02,  1.2885E-02,  1.2554E-02,
     X 1.2106E-02,  1.1616E-02,  1.1394E-02,  1.1092E-02,  1.0682E-02,
     X 1.0519E-02,  9.7219E-03,  9.3434E-03,  8.5260E-03,  8.3333E-03/
      DATA C10327/
     X 7.8582E-03,  6.8295E-03,  6.7963E-03,  6.7516E-03,  6.2930E-03,
     X 6.1615E-03,  6.1250E-03,  5.9011E-03,  5.7823E-03,  5.4688E-03,
     X 5.0978E-03,  4.4526E-03,  3.8090E-03,  3.2310E-03,  3.0128E-03,
     X 3.9063E-03,  6.7911E-03,  9.3161E-03,  1.0256E-02,  1.0183E-02,
     X 9.8289E-03,  9.5683E-03,  9.0406E-03,  8.7148E-03,  8.5284E-03,
     X 8.6149E-03,  8.7238E-03,  9.3679E-03,  1.0683E-02,  1.2016E-02,
     X 1.3097E-02,  1.3610E-02,  1.3588E-02,  1.3805E-02,  1.3928E-02,
     X 1.3903E-02,  1.3446E-02,  1.3258E-02,  1.3194E-02,  1.2703E-02,
     X 1.2393E-02,  1.2487E-02,  1.2341E-02,  1.2388E-02,  1.2061E-02,
     X 1.2122E-02,  1.1850E-02,  1.2032E-02,  1.1806E-02,  1.1810E-02,
     X 1.1572E-02,  1.1397E-02,  1.0980E-02,  1.1012E-02,  1.0524E-02,
     X 1.0518E-02,  1.0227E-02,  9.6837E-03,  9.6425E-03,  8.9938E-03,
     X 9.1488E-03,  8.8595E-03,  8.5976E-03,  8.4447E-03,  8.0731E-03,
     X 8.0283E-03,  7.7827E-03,  7.7638E-03,  7.2438E-03,  6.8246E-03,
     X 6.3457E-03,  5.6632E-03,  5.2500E-03,  4.3593E-03,  3.9431E-03,
     X 3.1580E-03,  2.2298E-03,  1.7818E-03,  1.4513E-03,  1.3188E-03/
      DATA C10407/
     X 2.1034E-03/
      DATA C10001 /
     X 6.45621E-03, 7.11308E-03, 1.06130E-02, 1.36338E-02, 1.27746E-02,
     X 1.42152E-02, 1.41144E-02, 1.64830E-02, 1.67110E-02, 1.57368E-02,
     X 1.54644E-02, 1.45248E-02, 1.43206E-02, 1.56946E-02, 1.54268E-02,
     X 1.37500E-02, 1.50224E-02, 1.60919E-02, 1.49099E-02, 1.53960E-02,
     X 1.61871E-02, 1.46539E-02, 1.38258E-02, 1.32571E-02, 1.21580E-02,
     X 1.39596E-02, 1.16029E-02, 1.47042E-02, 1.07441E-02, 1.08999E-02,
     X 1.05562E-02, 1.00589E-02, 9.60711E-03, 9.36950E-03, 7.65303E-03,
     X 6.86216E-03, 7.05344E-03, 6.90728E-03, 6.78627E-03, 6.97435E-03,
     X 5.75456E-03, 5.81685E-03, 5.00915E-03, 4.90259E-03, 4.42545E-03,
     X 4.14633E-03, 3.61657E-03, 3.08178E-03, 2.91680E-03, 2.94554E-03,
     X 3.35794E-03, 5.49025E-03, 7.09867E-03, 6.82592E-03, 8.84835E-03,
     X 9.15718E-03, 9.17935E-03, 8.31848E-03, 7.79481E-03, 7.75125E-03,
     X 6.95844E-03, 7.34506E-03, 7.53823E-03, 7.03272E-03, 7.57051E-03,
     X 9.20239E-03, 1.10864E-02, 1.16188E-02, 1.30029E-02, 1.44364E-02,
     X 1.29292E-02, 1.36031E-02, 1.35967E-02, 1.30412E-02, 1.29874E-02,
     X 1.14829E-02, 1.18009E-02, 1.20829E-02, 1.17831E-02, 1.21489E-02/
      DATA C10081 /
     X 1.27019E-02, 1.25557E-02, 1.23812E-02, 1.20158E-02, 1.26749E-02,
     X 1.17139E-02, 1.14552E-02, 1.11268E-02, 9.79143E-03, 8.79741E-03,
     X 8.85709E-03, 8.57653E-03, 8.93908E-03, 8.46205E-03, 8.56506E-03,
     X 8.14319E-03, 8.14415E-03, 7.74205E-03, 7.80727E-03, 7.49886E-03,
     X 7.71114E-03, 6.55963E-03, 6.87550E-03, 6.39162E-03, 5.55359E-03,
     X 5.43275E-03, 4.90649E-03, 4.41165E-03, 4.21875E-03, 3.62592E-03,
     X 3.40700E-03, 2.40267E-03, 2.61479E-03, 2.75677E-03, 4.10842E-03,
     X 5.79601E-03, 7.10708E-03, 8.07826E-03, 8.16166E-03, 8.72620E-03,
     X 8.85878E-03, 8.72755E-03, 8.25811E-03, 8.12100E-03, 7.78534E-03,
     X 7.39762E-03, 8.43880E-03, 8.53789E-03, 9.90072E-03, 1.01668E-02,
     X 1.00827E-02, 9.73556E-03, 9.57462E-03, 1.01289E-02, 1.10670E-02,
     X 1.03508E-02, 1.00929E-02, 9.10236E-03, 9.39459E-03, 8.79601E-03,
     X 8.67936E-03, 8.53862E-03, 7.95459E-03, 8.04037E-03, 7.95361E-03,
     X 7.87432E-03, 6.99165E-03, 7.37107E-03, 6.09187E-03, 6.21030E-03,
     X 5.33277E-03, 5.04633E-03, 4.45811E-03, 4.34153E-03, 3.98596E-03,
     X 3.84225E-03, 3.41943E-03, 3.60535E-03, 2.81691E-03, 2.49771E-03/
      DATA C10161 /
     X 2.35046E-03, 2.50947E-03, 3.75462E-03, 4.92349E-03, 5.09294E-03,
     X 4.98312E-03, 5.19325E-03, 4.41827E-03, 4.25192E-03, 4.46745E-03,
     X 4.08731E-03, 3.84776E-03, 3.67507E-03, 3.76845E-03, 3.69210E-03,
     X 4.59864E-03, 6.42677E-03, 7.83255E-03, 7.89247E-03, 8.10883E-03,
     X 8.00825E-03, 8.40322E-03, 7.97108E-03, 8.24714E-03, 8.39006E-03,
     X 8.68787E-03, 8.61108E-03, 8.81552E-03, 9.36996E-03, 9.08243E-03,
     X 9.69116E-03, 9.66185E-03, 9.22856E-03, 9.65086E-03, 9.35398E-03,
     X 9.06358E-03, 8.76851E-03, 8.43072E-03, 7.85659E-03, 7.93936E-03,
     X 7.49712E-03, 7.20199E-03, 6.94581E-03, 6.64086E-03, 6.12627E-03,
     X 6.13967E-03, 5.67310E-03, 5.09928E-03, 4.59112E-03, 3.95257E-03,
     X 3.67652E-03, 3.28781E-03, 2.77471E-03, 2.74494E-03, 2.15529E-03,
     X 1.95283E-03, 1.75043E-03, 1.60419E-03, 1.82688E-03, 2.34667E-03,
     X 2.92502E-03, 3.88322E-03, 4.39984E-03, 4.67814E-03, 4.80395E-03,
     X 4.69130E-03, 4.54564E-03, 4.46773E-03, 4.59178E-03, 4.37498E-03,
     X 4.12706E-03, 4.18299E-03, 4.57267E-03, 5.60127E-03, 6.51936E-03,
     X 7.10498E-03, 7.49870E-03, 7.89554E-03, 7.97428E-03, 8.21044E-03/
      DATA C10241 /
     X 8.06324E-03, 7.76648E-03, 7.62238E-03, 7.77675E-03, 7.46905E-03,
     X 7.61115E-03, 7.42715E-03, 7.28461E-03, 7.51514E-03, 7.38782E-03,
     X 6.97206E-03, 6.52738E-03, 6.10147E-03, 5.87553E-03, 5.49218E-03,
     X 4.94873E-03, 4.47920E-03, 4.25005E-03, 3.98094E-03, 3.92084E-03,
     X 3.41707E-03, 3.30501E-03, 3.09208E-03, 3.19686E-03, 3.55283E-03,
     X 4.20775E-03, 4.11155E-03, 3.72193E-03, 3.52000E-03, 3.13572E-03,
     X 2.87629E-03, 2.64251E-03, 2.33451E-03, 2.22426E-03, 2.05800E-03,
     X 1.75214E-03, 2.32530E-03, 2.68651E-03, 3.66315E-03, 4.93904E-03,
     X 5.32850E-03, 5.43978E-03, 5.32656E-03, 5.15649E-03, 5.42096E-03,
     X 5.37193E-03, 5.23454E-03, 5.34557E-03, 5.50533E-03, 6.13216E-03,
     X 6.65129E-03, 7.09357E-03, 7.46042E-03, 7.68605E-03, 7.91866E-03,
     X 7.52953E-03, 7.48272E-03, 7.17800E-03, 6.80060E-03, 6.60427E-03,
     X 6.43049E-03, 6.45975E-03, 6.20534E-03, 5.93094E-03, 5.67360E-03,
     X 5.38584E-03, 5.19364E-03, 4.92599E-03, 4.60655E-03, 4.24669E-03,
     X 3.94253E-03, 3.55894E-03, 3.24256E-03, 2.92974E-03, 2.62760E-03,
     X 2.52238E-03, 2.24714E-03, 2.26350E-03, 2.44380E-03, 3.03798E-03/
      DATA C10321 /
     X 3.50000E-03, 3.55416E-03, 3.43661E-03, 3.19814E-03, 3.02155E-03,
     X 2.73890E-03, 2.50078E-03, 2.34595E-03, 2.18282E-03, 2.19285E-03,
     X 2.49482E-03, 3.13434E-03, 4.18947E-03, 4.72069E-03, 5.29712E-03,
     X 5.39004E-03, 5.44846E-03, 5.37952E-03, 5.09935E-03, 5.08741E-03,
     X 5.05257E-03, 5.10339E-03, 5.17968E-03, 5.31841E-03, 5.58106E-03,
     X 5.65031E-03, 5.65680E-03, 5.76184E-03, 5.71213E-03, 5.48515E-03,
     X 5.32168E-03, 5.18505E-03, 4.99640E-03, 4.78746E-03, 4.57244E-03,
     X 4.32728E-03, 4.14464E-03, 3.97659E-03, 4.01874E-03, 4.10588E-03,
     X 3.99644E-03, 3.84584E-03, 3.64222E-03, 3.39590E-03, 3.00386E-03,
     X 2.73790E-03, 2.45095E-03, 2.29068E-03, 1.64530E-03, 1.68602E-03,
     X 2.32934E-03, 3.14851E-03, 3.65706E-03, 3.70878E-03, 3.75103E-03,
     X 3.79183E-03, 3.32032E-03, 2.42604E-03, 2.48775E-03, 2.34603E-03,
     X 2.36349E-03, 3.33744E-03, 3.44617E-03, 4.27280E-03, 4.61076E-03,
     X 5.20165E-03, 5.14851E-03, 5.22909E-03, 5.08278E-03, 5.16125E-03,
     X 5.01572E-03, 4.51685E-03, 4.67541E-03, 4.83421E-03, 4.57546E-03,
     X 4.55111E-03, 5.03093E-03, 4.67838E-03, 4.44282E-03, 4.40774E-03/
      DATA C10401 /
     X 4.48123E-03, 4.24410E-03, 4.03559E-03, 3.73969E-03, 3.45458E-03,
     X 3.18217E-03, 3.16115E-03, 3.36877E-03, 3.62026E-03, 3.69898E-03,
     X 3.49845E-03, 3.13839E-03, 2.77731E-03, 2.40106E-03, 2.03935E-03,
     X 1.84377E-03, 2.07757E-03, 2.39550E-03, 2.86272E-03, 3.27900E-03,
     X 3.42304E-03, 3.50211E-03, 3.29197E-03, 3.24784E-03, 3.20864E-03,
     X 3.28063E-03, 3.01328E-03, 3.00379E-03, 3.19562E-03, 3.45113E-03,
     X 3.75149E-03, 3.98520E-03, 4.19181E-03, 4.15773E-03, 4.02490E-03,
     X 3.95936E-03, 3.79001E-03, 3.77647E-03, 3.48528E-03, 3.55768E-03,
     X 3.62812E-03, 3.48650E-03, 3.35434E-03, 3.20088E-03, 3.25316E-03,
     X 3.04467E-03, 3.12633E-03, 3.23602E-03, 3.07723E-03, 2.80070E-03,
     X 2.72498E-03, 2.74752E-03, 2.58943E-03, 2.32482E-03, 2.20218E-03,
     X 2.10846E-03, 2.05991E-03, 2.01844E-03, 2.16224E-03, 2.48456E-03,
     X 2.88022E-03, 2.93939E-03, 3.01176E-03, 2.98886E-03, 2.96947E-03,
     X 3.38082E-03, 3.61657E-03, 3.42654E-03, 3.41274E-03, 3.22475E-03,
     X 2.97658E-03, 3.21944E-03, 3.32032E-03, 3.33273E-03, 3.58854E-03,
     X 3.67023E-03, 3.64069E-03, 3.74557E-03, 3.77703E-03, 3.64042E-03/
      DATA C10481 /
     X 3.39468E-03, 3.22657E-03, 3.16466E-03, 3.24224E-03, 3.24801E-03,
     X 3.19487E-03, 3.40155E-03, 3.16940E-03, 2.92293E-03, 3.00998E-03,
     X 2.82851E-03, 2.60381E-03, 2.59242E-03, 2.48530E-03, 2.76677E-03,
     X 2.45506E-03, 2.21845E-03, 2.30407E-03, 2.28136E-03, 2.37278E-03,
     X 2.25313E-03, 2.47836E-03, 2.77858E-03, 2.89803E-03, 2.86131E-03,
     X 3.14118E-03, 3.14119E-03, 2.88881E-03, 3.19502E-03, 2.99538E-03,
     X 2.91212E-03, 3.22739E-03, 3.05960E-03, 3.18901E-03, 3.05805E-03,
     X 3.12205E-03, 2.95636E-03, 3.24111E-03, 3.29433E-03, 3.09206E-03,
     X 3.06696E-03, 2.97735E-03, 2.90897E-03, 2.88979E-03, 2.75105E-03,
     X 2.92156E-03, 3.03445E-03, 2.91664E-03, 2.85559E-03, 2.98405E-03,
     X 2.95376E-03, 2.80234E-03, 2.78349E-03, 2.73421E-03, 2.70035E-03,
     X 2.60074E-03, 2.34840E-03, 2.37626E-03, 2.32927E-03, 2.20842E-03,
     X 2.31080E-03, 2.42771E-03, 2.43339E-03, 2.53280E-03, 2.37093E-03,
     X 2.37377E-03, 2.73453E-03, 2.60836E-03, 2.55568E-03, 2.44062E-03,
     X 2.71093E-03, 2.64421E-03, 2.66969E-03, 2.55560E-03, 2.71800E-03,
     X 2.79534E-03, 2.59070E-03, 2.55373E-03, 2.45272E-03, 2.55571E-03/
      DATA C10561 /
     X 2.54606E-03, 2.57349E-03, 2.46807E-03, 2.35634E-03, 2.44470E-03,
     X 2.47050E-03, 2.57131E-03, 2.71649E-03, 2.58800E-03, 2.54524E-03,
     X 2.69505E-03, 2.89122E-03, 2.77399E-03, 2.63306E-03, 2.82269E-03,
     X 2.95684E-03, 3.07415E-03, 2.70594E-03, 2.65650E-03, 2.90613E-03,
     X 2.96666E-03, 2.94767E-03, 2.81765E-03, 2.64829E-03, 2.43062E-03,
     X 2.33816E-03, 2.38210E-03, 2.45701E-03, 2.38508E-03, 2.40746E-03,
     X 2.49779E-03, 2.28209E-03, 2.26185E-03, 2.26604E-03, 2.19232E-03,
     X 2.19160E-03, 2.32246E-03, 2.11108E-03, 2.26220E-03, 2.26849E-03,
     X 2.34787E-03, 2.49323E-03, 2.46872E-03, 2.52974E-03, 2.35858E-03,
     X 2.36865E-03, 2.33533E-03, 2.21338E-03, 2.24610E-03, 2.24776E-03,
     X 2.24423E-03, 2.29276E-03, 2.18487E-03, 2.27621E-03, 2.31141E-03,
     X 2.44095E-03, 2.45198E-03, 2.56919E-03, 2.56823E-03, 2.41982E-03,
     X 2.39968E-03, 2.62447E-03, 2.55339E-03, 2.51556E-03, 2.47477E-03,
     X 2.50276E-03, 2.48381E-03, 2.48484E-03, 2.48316E-03, 2.38541E-03,
     X 2.41183E-03, 2.55888E-03, 2.42810E-03, 2.43356E-03, 2.25996E-03,
     X 2.34736E-03, 2.10305E-03, 2.13870E-03, 2.17472E-03, 2.05354E-03/
      DATA C10641 /
     X 2.11572E-03, 2.19557E-03, 2.09545E-03, 2.07831E-03, 1.94425E-03,
     X 1.89333E-03, 1.98025E-03, 1.98328E-03, 2.01702E-03, 1.98333E-03,
     X 2.01150E-03, 2.02484E-03, 2.10759E-03, 2.11892E-03, 2.10175E-03,
     X 2.05314E-03, 2.13338E-03, 2.25764E-03, 2.19055E-03, 2.10818E-03,
     X 2.05100E-03, 2.05685E-03, 2.10843E-03, 2.10228E-03, 2.10646E-03,
     X 2.22640E-03, 2.31253E-03, 2.31230E-03, 2.21885E-03, 2.19568E-03,
     X 2.23583E-03, 2.34754E-03, 2.28622E-03, 2.21876E-03, 2.26679E-03,
     X 2.30828E-03, 2.24944E-03, 2.13851E-03, 2.02938E-03, 1.96770E-03,
     X 2.05953E-03, 2.13814E-03, 2.03158E-03, 2.24655E-03, 1.95119E-03,
     X 2.12979E-03, 2.08581E-03, 2.02434E-03, 1.98926E-03, 1.98792E-03,
     X 1.97237E-03, 1.93397E-03, 1.92360E-03, 1.90805E-03, 1.89300E-03,
     X 1.83548E-03, 1.87215E-03, 1.85589E-03, 1.85718E-03, 1.79361E-03,
     X 1.77984E-03, 1.91506E-03, 2.04256E-03, 2.04095E-03, 1.94031E-03,
     X 1.90447E-03, 2.02049E-03, 1.98360E-03, 2.04364E-03, 2.02519E-03,
     X 2.20802E-03, 1.96964E-03, 1.94559E-03, 2.09922E-03, 2.11184E-03,
     X 2.05706E-03, 2.02257E-03, 2.01781E-03, 2.01055E-03, 1.86538E-03/
      DATA C10721 /
     X 1.86899E-03, 1.76798E-03, 1.85871E-03, 1.95363E-03, 1.96404E-03,
     X 1.84169E-03, 1.82851E-03, 1.84582E-03, 1.81997E-03, 1.76461E-03,
     X 1.68384E-03, 1.65530E-03, 1.73550E-03, 1.62463E-03, 1.68793E-03,
     X 1.60472E-03, 1.67560E-03, 1.67431E-03, 1.61779E-03, 1.66446E-03,
     X 1.66403E-03, 1.55724E-03, 1.62351E-03, 1.71545E-03, 1.69645E-03,
     X 1.59540E-03, 1.62948E-03, 1.66784E-03, 1.66416E-03, 1.66131E-03,
     X 1.71502E-03, 1.76555E-03, 1.75182E-03, 1.72327E-03, 1.72338E-03,
     X 1.69993E-03, 1.78819E-03, 1.73517E-03, 1.74802E-03, 1.81751E-03,
     X 1.70973E-03, 1.65075E-03, 1.70784E-03, 1.73655E-03, 1.71670E-03,
     X 1.67367E-03, 1.69338E-03, 1.61772E-03, 1.54914E-03, 1.56009E-03,
     X 1.59467E-03, 1.60761E-03, 1.57117E-03, 1.54045E-03, 1.53102E-03,
     X 1.44516E-03, 1.49898E-03, 1.56048E-03, 1.60087E-03, 1.62636E-03,
     X 1.62472E-03, 1.53931E-03, 1.55536E-03, 1.61649E-03, 1.66493E-03,
     X 1.86915E-03, 1.59984E-03, 1.60483E-03, 1.66549E-03, 1.73449E-03,
     X 1.73673E-03, 1.68393E-03, 1.67434E-03, 1.77880E-03, 1.76154E-03,
     X 1.43028E-03, 1.69651E-03, 1.60934E-03, 1.69413E-03, 1.70514E-03/
      DATA C10801 /
     X 1.62471E-03, 1.74854E-03, 1.76480E-03, 1.63495E-03, 1.59364E-03,
     X 1.39603E-03, 1.47897E-03, 1.49509E-03, 1.70002E-03, 1.63048E-03,
     X 1.44807E-03, 1.45071E-03, 1.53998E-03, 1.45276E-03, 1.29129E-03,
     X 1.52900E-03, 1.64444E-03, 1.37450E-03, 1.42574E-03, 1.47355E-03,
     X 1.51202E-03, 1.54376E-03, 1.51421E-03, 1.43989E-03, 1.45732E-03,
     X 1.42912E-03, 1.59906E-03, 1.56748E-03, 1.52383E-03, 1.47665E-03,
     X 1.51465E-03, 1.55582E-03, 1.54521E-03, 1.55189E-03, 1.56772E-03,
     X 1.45401E-03, 1.55775E-03, 1.43120E-03, 1.39659E-03, 1.41451E-03,
     X 1.45157E-03, 1.48303E-03, 1.42540E-03, 1.26387E-03, 1.37479E-03,
     X 1.46381E-03, 1.38134E-03, 1.32733E-03, 1.38030E-03, 1.44619E-03,
     X 1.41344E-03, 1.31982E-03, 1.24944E-03, 1.20096E-03, 1.21107E-03,
     X 1.27999E-03, 1.22523E-03, 1.22193E-03, 1.35957E-03, 1.41427E-03,
     X 1.35679E-03, 1.15438E-03, 1.41184E-03, 1.49093E-03, 1.32193E-03,
     X 1.25009E-03, 1.37625E-03, 1.49022E-03, 1.44180E-03, 1.27628E-03,
     X 1.29670E-03, 1.31636E-03, 1.28874E-03, 1.31177E-03, 1.35732E-03,
     X 1.33854E-03, 1.30253E-03, 1.31374E-03, 1.27379E-03, 1.18339E-03/
      DATA C10881 /
     X 1.22016E-03, 1.26551E-03, 1.26371E-03, 1.28180E-03, 1.36024E-03,
     X 1.45759E-03, 1.29413E-03, 1.35858E-03, 1.26528E-03, 1.18623E-03,
     X 1.21812E-03, 1.28799E-03, 1.37028E-03, 1.29268E-03, 1.27639E-03,
     X 1.19487E-03, 1.23542E-03, 1.25010E-03, 1.17418E-03, 1.13914E-03,
     X 1.21951E-03, 1.13780E-03, 1.16443E-03, 1.17883E-03, 1.11982E-03,
     X 1.05708E-03, 1.04865E-03, 1.05884E-03, 1.06599E-03, 1.13828E-03,
     X 1.10373E-03, 1.07739E-03, 1.04632E-03, 1.06118E-03, 1.15445E-03,
     X 1.17300E-03, 1.00675E-03, 1.04235E-03, 1.08398E-03, 1.06587E-03,
     X 1.05536E-03, 1.08614E-03, 1.09026E-03, 1.09141E-03, 1.13051E-03,
     X 1.08667E-03, 1.04016E-03, 1.04897E-03, 1.08894E-03, 1.09682E-03,
     X 1.09638E-03, 9.79254E-04, 1.00668E-03, 1.02569E-03, 1.00581E-03,
     X 9.74433E-04, 9.66321E-04, 9.78440E-04, 9.01587E-04, 1.02149E-03,
     X 9.87464E-04, 9.41872E-04, 9.05021E-04, 8.59547E-04, 9.03963E-04,
     X 8.66415E-04, 8.84726E-04, 8.77087E-04, 8.70584E-04, 8.81338E-04,
     X 8.97658E-04, 8.97586E-04, 9.19028E-04, 8.82438E-04, 9.00710E-04,
     X 9.54329E-04, 9.54490E-04, 9.10940E-04, 9.95472E-04, 9.50134E-04/
      DATA C10961 /
     X 9.17127E-04, 9.70916E-04, 9.87575E-04, 9.65026E-04, 9.71779E-04,
     X 1.00967E-03, 1.00053E-03, 9.26063E-04, 9.34721E-04, 9.76354E-04,
     X 9.78436E-04, 9.36012E-04, 9.64448E-04, 9.95903E-04, 9.89960E-04,
     X 9.41143E-04, 9.04393E-04, 8.84719E-04, 8.41396E-04, 8.67234E-04,
     X 8.55864E-04, 8.63314E-04, 8.72317E-04, 8.40899E-04, 7.79593E-04,
     X 7.88481E-04, 8.21075E-04, 7.38342E-04, 7.56537E-04, 7.57278E-04,
     X 7.35854E-04, 7.32765E-04, 6.67398E-04, 7.45338E-04, 7.33094E-04,
     X 7.01840E-04, 6.85595E-04, 6.95740E-04, 7.24015E-04, 7.00907E-04,
     X 7.28498E-04, 6.89410E-04, 6.91728E-04, 7.40601E-04, 7.62775E-04,
     X 7.40912E-04, 7.35021E-04, 7.07799E-04, 7.54113E-04, 8.44845E-04,
     X 8.53956E-04, 6.42186E-04, 7.40557E-04, 7.54340E-04, 7.55544E-04,
     X 7.88986E-04, 7.97902E-04, 6.98460E-04, 7.74873E-04, 6.81178E-04,
     X 7.15567E-04, 7.56723E-04, 7.98438E-04, 8.83150E-04, 8.45671E-04,
     X 7.40924E-04, 7.35498E-04, 7.77829E-04, 6.93566E-04, 5.10188E-04,
     X 7.52717E-04, 6.94185E-04, 6.71928E-04, 6.73286E-04, 6.89415E-04,
     X 7.22917E-04, 7.89448E-04, 8.53812E-04, 7.45132E-04, 7.68732E-04/
      DATA C11041 /
     X 8.10104E-04, 7.55615E-04, 7.09145E-04, 6.80676E-04, 7.54594E-04,
     X 7.89416E-04, 7.88579E-04, 7.49805E-04, 6.13534E-04, 7.22491E-04,
     X 7.95410E-04, 7.80604E-04, 7.74283E-04, 7.93224E-04, 6.86522E-04,
     X 8.06038E-04, 8.30285E-04, 8.37763E-04, 8.03863E-04, 7.33526E-04,
     X 7.42588E-04, 6.31046E-04, 8.16153E-04, 8.95391E-04, 8.61330E-04,
     X 8.38726E-04, 8.16761E-04, 8.16118E-04, 6.37058E-04, 6.30868E-04,
     X 7.26410E-04, 7.03464E-04, 5.93454E-04, 6.01985E-04, 6.51157E-04,
     X 6.68569E-04, 6.56297E-04, 6.58732E-04, 5.99721E-04, 5.34301E-04,
     X 5.33271E-04, 5.57992E-04, 5.70096E-04, 5.59932E-04, 5.32110E-04,
     X 5.64713E-04, 6.25026E-04, 6.38973E-04, 6.05323E-04, 7.17460E-04,
     X 6.19407E-04, 5.90228E-04, 5.43682E-04, 5.38446E-04, 6.56146E-04,
     X 6.09081E-04, 6.04737E-04, 6.45526E-04, 6.46978E-04, 5.89738E-04,
     X 5.63852E-04, 6.18018E-04, 5.71768E-04, 5.75433E-04, 6.05766E-04,
     X 5.93065E-04, 5.31708E-04, 5.41187E-04, 5.76985E-04, 5.78176E-04,
     X 5.75339E-04, 6.85426E-04, 5.51038E-04, 6.02049E-04, 6.20406E-04,
     X 5.80169E-04, 5.36399E-04, 5.59608E-04, 5.46575E-04, 5.66979E-04/
      DATA C11121 /
     X 5.94982E-04, 6.18469E-04, 6.56281E-04, 8.22124E-04, 7.81716E-04,
     X 7.29616E-04, 7.14460E-04, 7.08969E-04, 6.53794E-04, 7.33138E-04,
     X 8.29513E-04, 8.99395E-04, 9.05526E-04, 7.98257E-04, 7.86935E-04,
     X 6.10797E-04, 4.63912E-04, 4.05675E-04, 3.66230E-04, 4.86472E-04,
     X 5.31818E-04, 5.15865E-04, 4.87344E-04, 4.99857E-04, 5.35479E-04,
     X 5.27561E-04, 4.99000E-04, 4.77056E-04, 4.74242E-04, 4.66595E-04,
     X 4.66325E-04, 4.94704E-04, 5.12842E-04, 5.01795E-04, 4.80789E-04,
     X 5.73709E-04, 5.65214E-04, 5.11321E-04, 4.55242E-04, 4.29330E-04,
     X 5.09792E-04, 4.70489E-04, 4.82859E-04, 4.99195E-04, 4.07724E-04,
     X 4.99951E-04, 4.55755E-04, 4.42528E-04, 4.19433E-04, 3.31325E-04,
     X 3.70517E-04, 3.77708E-04, 2.97923E-04, 2.27470E-04, 2.47389E-04,
     X 2.38324E-04, 2.56706E-04, 2.45046E-04, 2.62539E-04, 3.37054E-04,
     X 3.33930E-04, 3.01390E-04, 3.08028E-04, 3.41464E-04, 3.70574E-04,
     X 3.47893E-04, 3.28433E-04, 3.46976E-04, 3.60351E-04, 3.50559E-04,
     X 3.56070E-04, 3.62782E-04, 3.37330E-04, 3.33763E-04, 3.57046E-04,
     X 3.08784E-04, 2.93898E-04, 2.80842E-04, 2.54114E-04, 2.38198E-04/
      DATA C11201 /
     X 3.48753E-04, 2.97334E-04, 2.82929E-04, 2.94150E-04, 3.07875E-04,
     X 3.21129E-04, 3.38335E-04, 3.49826E-04, 3.47647E-04, 3.35438E-04,
     X 3.58145E-04, 3.72391E-04, 3.59372E-04, 3.64755E-04, 4.16867E-04,
     X 3.43614E-04, 3.34932E-04, 3.12782E-04, 3.28220E-04, 4.32595E-04,
     X 3.49513E-04, 3.51861E-04, 3.81166E-04, 3.91194E-04, 3.38944E-04,
     X 2.63445E-04, 2.49520E-04, 2.46184E-04, 2.33203E-04, 2.16315E-04,
     X 1.89536E-04, 1.95730E-04, 1.99664E-04, 1.77139E-04, 1.27969E-04,
     X 5.17216E-05, 7.60445E-05, 1.24418E-04, 1.30989E-04, 2.31539E-04,
     X 2.21334E-04, 2.08757E-04, 2.18351E-04, 2.46202E-04, 2.29824E-04,
     X 2.28909E-04, 2.88826E-04, 3.58039E-04, 2.60800E-04, 2.33025E-04,
     X 2.52667E-04, 2.61394E-04, 2.31384E-04, 2.29388E-04, 2.54701E-04,
     X 2.21158E-04, 1.61506E-04, 1.36752E-04, 1.69481E-04, 8.64539E-05,
     X 1.64407E-04, 3.65674E-04, 3.18233E-04, 4.00755E-04, 3.33375E-04,
     X 2.62930E-04, 2.87052E-04, 2.51395E-04, 2.85274E-04, 2.66915E-04,
     X 2.10866E-04, 1.89517E-04, 1.67378E-04, 2.79951E-04, 2.97224E-04,
     X 1.89222E-04, 3.33825E-04, 3.56386E-04, 3.89727E-04, 4.30407E-04/
      DATA C11281 /
     X 4.45922E-04, 4.23446E-04, 4.41347E-04, 4.06723E-04, 3.00181E-04,
     X 1.85243E-04, 3.13176E-04, 4.08991E-04, 4.24776E-04, 3.56412E-04,
     X 3.84760E-04, 2.30602E-04, 1.77702E-04, 2.62329E-04, 2.49442E-04,
     X 3.76212E-04, 3.69176E-04, 2.97681E-04, 2.71662E-04, 2.05694E-04,
     X 2.11418E-04, 2.25439E-04, 2.27013E-04, 2.47845E-04, 3.14603E-04,
     X 2.68802E-04, 2.04334E-04, 2.77399E-04, 2.68273E-04, 2.04991E-04,
     X 2.24441E-04, 3.55074E-04, 2.90135E-04, 3.35680E-04, 3.59358E-04,
     X 3.44716E-04, 3.24496E-04, 3.48146E-04, 3.49042E-04, 3.54848E-04,
     X 3.86418E-04, 3.59198E-04, 3.47608E-04, 3.20522E-04, 2.78401E-04,
     X 2.64579E-04, 2.23694E-04, 2.34370E-04, 2.52559E-04, 1.88475E-04,
     X 2.01258E-04, 1.63979E-04, 1.45384E-04, 1.91215E-04, 1.76958E-04,
     X 1.69167E-04, 1.71767E-04, 1.86595E-04, 2.14969E-04, 2.48345E-04,
     X 2.46691E-04, 2.25234E-04, 2.26755E-04, 1.64112E-04, 1.87750E-04,
     X 2.22984E-04, 2.00443E-04, 2.38863E-04, 2.77590E-04, 2.91953E-04,
     X 2.80611E-04, 3.08215E-04, 1.79095E-04, 1.46920E-04, 2.29177E-04,
     X 2.54685E-04, 2.68866E-04, 2.13346E-04, 1.20122E-04, 5.55240E-05/
      DATA C11361 /
     X 5.99017E-05, 1.07768E-04, 1.67810E-04, 2.06886E-04, 2.36232E-04,
     X 2.24598E-04, 2.30792E-04, 2.71274E-04, 1.29062E-04, 1.92624E-04,
     X 2.38438E-04, 1.98994E-04, 1.81687E-04, 2.55733E-04, 2.84379E-04,
     X 2.54459E-04, 2.30884E-04, 2.68873E-04, 3.07231E-04, 3.15063E-04,
     X 2.46725E-04, 2.60370E-04, 2.66391E-04, 2.50708E-04, 2.04296E-04,
     X 1.66011E-04, 1.19164E-04, 1.06700E-04, 1.77576E-04, 1.91741E-04,
     X 1.66618E-04, 1.49824E-04, 1.80699E-04, 2.20905E-04, 1.38754E-04,
     X 6.27971E-05, 7.52567E-05, 1.89995E-04, 1.72489E-04, 1.40424E-04,
     X 1.52384E-04, 1.63942E-04, 1.19901E-04, 1.49234E-04, 2.68313E-04,
     X 2.08815E-04, 1.17218E-04, 1.42235E-04, 2.71237E-04, 1.38192E-04,
     X 2.15643E-04, 2.84476E-04, 2.78117E-04, 2.19234E-04, 1.59128E-04,
     X 1.78819E-04, 2.67785E-04, 2.66786E-04, 2.58545E-04, 2.68476E-04,
     X 2.88542E-04, 2.59726E-04, 3.00936E-04, 3.11237E-04, 2.61275E-04,
     X 1.37136E-04, 2.76566E-04, 3.82888E-04, 3.97564E-04, 4.43655E-04,
     X 3.15415E-04, 2.60869E-04, 3.19171E-04, 3.34205E-04, 2.02914E-04,
     X 1.16223E-04, 1.14737E-04, 6.10978E-05,-8.03695E-06,-1.07062E-05/
      DATA C11441 /
     X 6.50664E-05, 1.12586E-04, 1.56727E-04, 1.57927E-04, 1.05762E-04,
     X 1.03646E-04, 1.72520E-04, 2.23668E-04, 2.12775E-04, 2.33525E-04,
     X 2.75558E-04, 2.34256E-04, 5.10062E-05, 1.76007E-04, 1.70850E-04,
     X 1.43266E-04, 1.89626E-04, 2.97283E-04, 3.02773E-04, 2.74401E-04,
     X 3.00754E-04, 3.66813E-04, 3.54383E-04, 2.90580E-04, 2.32206E-04,
     X 1.58405E-04, 1.54663E-04, 1.84598E-04, 1.26408E-04, 2.14481E-04,
     X 2.00791E-04, 1.05796E-04, 2.39794E-04, 1.66105E-04, 7.88615E-05,
     X 4.30615E-05, 7.37518E-05, 1.24926E-04, 1.38295E-04, 8.54356E-05,
     X 6.12641E-05, 6.54466E-05, 6.17727E-05, 1.30688E-05, 6.00462E-05,
     X 1.52612E-04, 2.11656E-04, 9.67692E-05, 8.67858E-05, 1.34888E-04,
     X 1.90899E-04, 1.03234E-04, 1.03837E-04, 1.49767E-04, 2.19058E-04,
     X 2.26549E-04, 2.11506E-04, 1.85238E-04, 1.53774E-04, 1.32313E-04,
     X 6.10658E-05, 2.37782E-05, 1.24450E-04, 1.87610E-04, 1.44775E-04,
     X 5.60937E-05, 6.64032E-05, 1.28073E-04, 1.77512E-04, 1.84684E-04,
     X 5.73677E-05, 5.29679E-05, 9.95510E-05, 1.61423E-04, 3.19036E-04,
     X 3.17383E-04, 2.36505E-04, 1.80844E-04, 1.63722E-04, 1.21478E-04/
      DATA C11521 /
     X 6.85823E-05, 7.42058E-05, 1.14838E-04, 1.21131E-04, 8.01009E-05,
     X 1.52058E-04, 2.18368E-04, 2.53416E-04, 2.27116E-04, 1.25336E-04,
     X 6.26421E-05, 5.32471E-05, 1.34705E-04, 2.07005E-05,-5.18630E-05,
     X-3.25696E-05,-8.06171E-05,-1.09430E-04,-1.05637E-04,-4.96066E-05,
     X-7.76138E-05,-4.85930E-05, 3.65111E-06,-2.86933E-05,-4.61366E-05,
     X-4.88820E-05,-3.08816E-05, 8.43778E-05, 1.40484E-04, 1.31125E-04,
     X 3.55198E-05, 8.47412E-05, 1.23408E-04, 1.36799E-04, 1.21147E-04,
     X 1.25585E-04, 1.32337E-04, 1.34092E-04, 1.26652E-04, 1.12131E-04,
     X 1.00927E-04, 1.13828E-04, 1.06053E-04, 9.43643E-05, 8.33628E-05,
     X 8.65842E-05, 7.59315E-05, 8.28623E-05, 1.39681E-04, 1.80492E-04,
     X 1.65779E-04, 1.03843E-04, 3.10284E-05, 1.94408E-05, 4.57525E-05,
     X 1.02436E-04, 1.39750E-04, 1.43342E-04, 1.11999E-04, 2.94197E-05,
     X 2.76980E-05, 5.51685E-05, 9.39909E-05, 1.16108E-04, 7.72703E-05,
     X 4.37409E-05, 1.13925E-04, 8.18872E-05, 2.87657E-05,-2.41413E-05,
     X 1.24699E-05, 2.19589E-05,-5.88247E-06,-9.66151E-05,-2.06255E-05,
     X-1.83148E-06,-5.63625E-05,-8.65590E-05,-8.26020E-05,-5.06239E-05/
      DATA C11601 /
     X 1.28065E-05,-1.34669E-05, 1.59701E-05, 9.44755E-05, 1.63032E-05,
     X 2.51304E-05, 7.38226E-05, 1.28405E-04, 1.17413E-04, 9.92387E-05,
     X 9.51533E-05, 2.17008E-04, 2.25854E-04, 1.90448E-04, 1.77207E-04,
     X 1.80844E-04, 1.53501E-04, 9.80430E-05, 1.27404E-04, 1.16465E-04,
     X 9.98611E-05, 1.25556E-04, 1.73627E-04, 1.12347E-04,-7.73523E-05,
     X 5.66599E-05, 5.36347E-05, 1.20227E-06, 6.96325E-05, 4.79010E-05,
     X-1.09886E-05,-9.16457E-05,-7.09170E-05,-5.31410E-05,-2.68376E-05,
     X 6.32641E-05, 8.06052E-06,-4.99262E-05,-2.56644E-05,-8.76854E-05,
     X-8.21360E-05,-5.02403E-06, 4.66629E-05, 6.93127E-05, 5.53828E-05,
     X-2.32399E-05,-2.07514E-05,-7.33240E-05,-2.10483E-04,-1.53757E-04,
     X-7.13861E-05,-1.07356E-05,-1.26578E-04,-7.48854E-05, 3.25418E-06,
     X 2.97068E-05, 3.35685E-05, 3.15022E-05, 2.68904E-05, 3.87401E-05,
     X 5.12522E-05, 5.12172E-05, 1.05053E-05, 1.65321E-05, 3.47537E-05,
     X 5.62503E-05, 4.18666E-05, 3.13970E-05, 3.11750E-05, 7.21547E-05,
     X 2.55262E-05,-2.76061E-05, 5.43449E-06,-5.20575E-05,-1.08627E-04,
     X-1.40475E-04,-1.59926E-04,-1.32237E-04,-8.15458E-05,-1.31738E-04/
      DATA C11681 /
     X-1.64036E-04,-1.69351E-04,-1.24797E-04,-1.61950E-04,-2.01904E-04,
     X-2.22995E-04,-1.87647E-04,-1.70817E-04,-1.64583E-04,-1.12811E-04,
     X-8.38306E-05,-8.62707E-05,-1.54362E-04,-1.98090E-04,-2.12920E-04,
     X-1.89358E-04,-2.02988E-04,-1.72791E-04,-1.02863E-04,-1.09877E-04,
     X-1.04257E-04,-8.20734E-05,-2.18346E-05,-2.94593E-05,-4.18226E-05,
     X-1.86891E-05,-6.14620E-05,-3.21912E-05, 1.00844E-04, 6.92419E-05,
     X 3.16713E-05, 5.62042E-07, 5.18900E-05, 7.48835E-05, 8.03381E-05,
     X 7.24685E-05, 9.55588E-05, 9.22801E-05, 2.87159E-05, 2.26234E-05,
     X 2.62790E-05, 3.58332E-05, 6.23297E-05, 5.01998E-05, 1.81446E-05,
     X 3.33564E-05, 3.97765E-06,-2.60624E-05, 7.01802E-06,-4.16797E-05,
     X-8.70108E-05,-8.22182E-05,-6.64886E-05,-7.88704E-05,-1.28305E-04,
     X-1.29990E-04,-1.12646E-04,-8.68394E-05,-1.29584E-04,-1.44352E-04,
     X-1.42082E-04,-1.33790E-04,-1.27963E-04,-1.21233E-04,-1.09965E-04,
     X-1.02233E-04,-1.03804E-04,-1.19503E-04,-7.74707E-05,-4.66805E-05,
     X-3.52201E-05,-4.07406E-05,-4.66887E-05,-5.05962E-05,-3.30333E-05,
     X-3.47981E-05,-3.60962E-05, 1.44242E-05, 4.10478E-05, 3.68984E-05/
      DATA C11761 /
     X-2.81300E-05, 2.83171E-05, 7.48062E-05, 4.29333E-05, 8.50076E-06,
     X 4.98135E-06, 4.44854E-05, 2.51860E-05, 3.12189E-05, 6.39424E-05,
     X 7.20715E-05, 9.89688E-05, 1.33768E-04, 1.07781E-04, 9.76731E-05,
     X 9.21479E-05, 6.72624E-05, 5.41295E-05, 4.89022E-05, 5.28039E-05,
     X-4.48737E-06,-5.15409E-05,-3.57396E-05,-1.94752E-05,-2.09521E-05,
     X-5.13096E-05,-2.62781E-05,-2.75451E-05,-6.98423E-05,-1.25462E-04,
     X-1.68362E-04,-1.97456E-04,-1.90669E-04,-2.06890E-04,-2.36699E-04,
     X-1.97732E-04,-1.76504E-04,-1.67505E-04,-1.60694E-04,-1.85851E-04,
     X-2.01567E-04,-9.82507E-05,-1.33338E-04,-1.95199E-04,-1.40781E-04,
     X-8.90988E-05,-3.63239E-05, 2.16510E-05,-1.56807E-05,-4.21285E-05,
     X 5.50505E-06, 6.78937E-07, 3.12346E-06, 3.64202E-05, 3.50651E-05,
     X 6.20423E-05, 1.38667E-04, 7.74738E-05, 6.77036E-05, 1.38367E-04,
     X 1.17359E-04, 1.06637E-04, 1.12404E-04, 9.78586E-05, 1.03178E-04,
     X 1.28717E-04, 1.56642E-04, 1.62544E-04, 1.50109E-04, 1.43214E-04,
     X 1.33651E-04, 1.24352E-04, 1.41420E-04, 1.36340E-04, 1.18769E-04,
     X 1.31656E-04, 8.81533E-05, 1.55214E-05,-3.68736E-07,-1.76213E-05/
      DATA C11841 /
     X-2.85341E-05, 4.65155E-06, 5.41350E-06,-7.01247E-06, 6.57918E-06,
     X-2.45784E-05,-6.89104E-05,-6.90953E-05,-6.23937E-05,-6.72978E-05,
     X-1.39547E-04,-1.44228E-04,-1.42543E-04,-2.31080E-04,-2.12756E-04,
     X-1.62089E-04,-1.66063E-04,-1.61872E-04,-1.59764E-04,-1.80217E-04,
     X-1.38355E-04,-8.45661E-05,-7.58308E-05,-4.65144E-05,-2.76855E-05,
     X-7.48714E-05,-8.28561E-05,-6.45277E-05,-7.08509E-06,-1.05566E-05,
     X-1.96352E-05, 3.55561E-05, 2.24676E-05,-1.25648E-05,-1.87661E-05,
     X 6.99061E-06, 2.33676E-05,-5.25111E-05,-3.86758E-05, 1.03585E-06,
     X-1.65901E-05,-1.04855E-05, 5.03694E-06, 1.25937E-05,-8.31340E-06,
     X-4.37906E-05,-7.91444E-05,-4.62167E-05, 5.14238E-06,-4.52863E-05,
     X-5.86455E-05,-4.98093E-05,-3.03495E-05,-5.09377E-05,-8.88116E-05,
     X-6.21360E-05,-7.38148E-05,-1.07502E-04,-7.55276E-05,-6.39257E-05,
     X-6.86921E-05,-8.05504E-05,-9.24178E-05,-1.03991E-04,-1.00468E-04,
     X-6.71447E-05,-3.84897E-06,-5.99067E-06,-2.21894E-05,-5.21766E-05,
     X-3.93796E-05,-4.06712E-05,-6.21649E-05,-1.13073E-04,-1.20560E-04,
     X-5.92397E-05, 5.24432E-05, 9.41628E-05,-3.47458E-07, 5.33267E-05/
      DATA C11921 /
     X 8.92961E-05, 2.75694E-05,-7.48460E-06,-2.15504E-05, 1.05501E-06,
     X 6.30910E-06, 5.94620E-07,-2.45194E-05,-1.59657E-05, 7.93610E-07,
     X-1.05319E-05,-2.36584E-05,-3.95700E-05,-6.57225E-05,-5.23797E-05,
     X-1.82588E-05,-1.43240E-05,-3.29989E-05,-6.48909E-05,-2.41326E-05,
     X-1.89195E-05,-4.64607E-05,-1.00739E-05,-1.35033E-05,-6.49945E-05,
     X-5.19986E-05,-6.68505E-05,-1.31530E-04,-1.45464E-04,-1.46815E-04,
     X-1.39684E-04,-1.23205E-04,-1.26738E-04,-1.93822E-04,-2.37508E-04,
     X-2.52917E-04,-1.91110E-04,-1.36217E-04,-9.41093E-05,-1.20601E-04,
     X-1.17295E-04,-9.57420E-05,-1.57227E-04,-1.62795E-04,-1.12201E-04,
     X-1.20419E-04,-1.10597E-04,-7.61223E-05,-6.27167E-05,-5.54733E-05,
     X-5.50437E-05,-5.14148E-05,-3.59591E-05, 1.09906E-05, 5.94396E-06,
     X-1.38597E-05,-8.80857E-06,-3.13101E-05,-6.31715E-05,-4.04264E-05,
     X-1.66405E-05, 7.94396E-06,-3.41772E-06,-4.03175E-05,-1.06888E-04,
     X-9.50526E-05,-7.46111E-05,-5.09617E-05,-6.70981E-05,-7.93529E-05,
     X-5.58423E-05,-1.01523E-04,-1.62269E-04,-1.69958E-04,-1.37786E-04,
     X-8.79862E-05,-1.46838E-04,-1.66938E-04,-1.51380E-04,-1.62184E-04/
      DATA C12001 /
     X-1.61105E-04,-1.42088E-04,-1.57033E-04,-1.65294E-04,-1.45079E-04,
     X-9.76982E-05,-6.09891E-05,-1.01719E-04,-1.03049E-04,-8.85546E-05,
     X-1.47754E-04,-1.44542E-04,-8.34620E-05,-8.99440E-05,-7.11901E-05,
     X-1.57480E-05,-8.81797E-05,-1.56314E-04,-1.65952E-04,-1.80986E-04,
     X-2.04610E-04,-2.58669E-04,-2.16016E-04,-1.21582E-04,-1.44929E-04,
     X-1.72886E-04,-2.05950E-04,-1.93829E-04,-1.67518E-04,-1.22969E-04,
     X-1.13060E-04,-1.14854E-04,-1.26198E-04,-1.24288E-04,-1.19519E-04,
     X-1.50456E-04,-1.53286E-04,-1.32231E-04,-7.42672E-05,-2.23129E-05,
     X 1.79115E-05, 1.42073E-05,-1.21676E-05,-7.56567E-05,-1.03423E-04,
     X-1.10373E-04,-8.77244E-05,-6.43485E-05,-4.05156E-05,-6.24405E-05,
     X-5.70375E-05,-2.36695E-06,-3.75929E-05,-7.97119E-05,-6.70419E-05,
     X-6.99475E-05,-8.19748E-05,-1.06895E-04,-1.31422E-04,-1.55438E-04,
     X-1.61937E-04,-1.62626E-04,-1.54977E-04,-1.77814E-04,-2.00386E-04,
     X-1.87407E-04,-2.07243E-04,-2.44672E-04,-2.19014E-04,-2.13695E-04,
     X-2.32440E-04,-1.85194E-04,-1.51172E-04,-1.69834E-04,-1.73780E-04,
     X-1.75232E-04,-2.00698E-04,-1.82826E-04,-1.27786E-04,-1.33633E-04/
      DATA C12081 /
     X-1.21317E-04,-7.50390E-05,-1.06743E-04,-1.40805E-04,-1.06336E-04,
     X-9.46654E-05,-9.78182E-05,-1.19906E-04,-1.14160E-04,-7.28186E-05,
     X-1.07652E-04,-1.20978E-04,-3.79658E-05,-3.16113E-05,-6.02417E-05,
     X-7.51148E-05,-5.56145E-05,-6.77421E-06,-1.74321E-05,-4.67952E-05,
     X-1.05000E-04,-6.29932E-05,-4.74356E-06,-2.83397E-05,-4.65192E-05,
     X-6.04574E-05,-4.33970E-05,-3.18311E-05,-3.02321E-05,-4.49667E-05,
     X-6.85347E-05,-1.11375E-04,-1.16293E-04,-9.38757E-05,-1.38594E-04,
     X-1.60483E-04,-1.48344E-04,-1.33436E-04,-1.27387E-04,-1.59508E-04,
     X-1.74026E-04,-1.72170E-04,-1.49196E-04,-1.33233E-04,-1.22382E-04,
     X-1.78156E-04,-2.21349E-04,-2.41846E-04,-2.06549E-04,-1.68283E-04,
     X-1.89512E-04,-1.44523E-04,-4.67953E-05,-1.00334E-04,-1.23478E-04,
     X-8.14024E-05,-9.18016E-05,-1.17536E-04,-1.36160E-04,-1.38780E-04,
     X-1.27749E-04,-1.45598E-04,-1.55964E-04,-1.45120E-04,-1.25544E-04,
     X-1.05692E-04,-1.17639E-04,-1.24142E-04,-1.24749E-04,-1.63878E-04,
     X-1.97021E-04,-1.98617E-04,-2.69136E-04,-3.68357E-04,-2.33702E-04,
     X-1.61830E-04,-1.78578E-04,-2.01839E-04,-2.28731E-04,-2.63606E-04/
      DATA C12161 /
     X-2.44698E-04,-1.86451E-04,-2.20546E-04,-2.22752E-04,-1.55169E-04,
     X-1.25100E-04,-1.09794E-04,-9.59016E-05,-1.03857E-04,-1.35573E-04,
     X-1.73780E-04,-1.82457E-04,-9.39821E-05,-1.18245E-04,-2.11563E-04,
     X-1.37392E-04,-9.28173E-05,-9.71073E-05,-9.72535E-05,-9.39557E-05,
     X-7.50117E-05,-6.70754E-05,-7.01186E-05,-5.76151E-05,-5.18785E-05,
     X-7.14209E-05,-7.01682E-05,-5.61614E-05,-8.92769E-05,-1.06238E-04,
     X-9.70294E-05,-6.70229E-05,-4.69214E-05,-1.53105E-04,-2.02326E-04,
     X-1.90395E-04,-2.04367E-04,-2.16787E-04,-2.08725E-04,-1.78119E-04,
     X-1.31043E-04,-1.32204E-04,-1.51522E-04,-2.05143E-04,-1.77144E-04,
     X-1.16130E-04,-1.44440E-04,-1.66010E-04,-1.78206E-04,-1.61163E-04,
     X-1.46351E-04,-1.96722E-04,-2.27027E-04,-2.37243E-04,-2.25235E-04,
     X-1.99552E-04,-1.40238E-04,-1.26311E-04,-1.42746E-04,-1.19028E-04,
     X-1.18750E-04,-1.72076E-04,-1.72120E-04,-1.48285E-04,-1.85116E-04,
     X-1.98602E-04,-1.74016E-04,-1.37913E-04,-1.01221E-04,-9.69581E-05,
     X-1.08794E-04,-1.39433E-04,-1.38575E-04,-1.32088E-04,-1.37431E-04,
     X-1.30033E-04,-1.10829E-04,-1.35604E-04,-1.66515E-04,-1.98167E-04/
      DATA C12241 /
     X-1.97716E-04,-1.74019E-04,-1.64719E-04,-1.64779E-04,-1.85725E-04,
     X-2.28526E-04,-2.84329E-04,-1.82449E-04,-1.30747E-04,-1.93620E-04,
     X-2.28529E-04,-2.47361E-04,-1.90001E-04,-1.66278E-04,-2.02540E-04,
     X-2.31811E-04,-2.53772E-04,-2.08629E-04,-1.85021E-04,-1.93989E-04,
     X-2.16568E-04,-2.38288E-04,-1.94453E-04,-1.87154E-04,-2.30493E-04,
     X-2.34696E-04,-2.30351E-04,-2.60562E-04,-2.86427E-04,-3.06699E-04,
     X-2.79131E-04,-2.49392E-04,-3.03389E-04,-3.10346E-04,-2.61782E-04,
     X-2.30905E-04,-2.11669E-04,-2.37680E-04,-2.38194E-04,-2.10955E-04/
      END
      SUBROUTINE O3HHT2(V,C)
      COMMON /O3HH2/ V1S,V2S,DVS,NPTS,S(2690)
C
      CALL O3INT(V ,V1S,DVS,NPTS,S,C)
C
      RETURN
      END
      BLOCK DATA BO3HH2
C>    BLOCK DATA
C
C     RATIO (C2/C0)
C     DATA FROM BASS 1985
C
C     NOW INCLUDES MOLINA & MOLINA AT 273K WITH THE TEMPERATURE
C     DEPENDENCE DETERMINED FROM THE 195K HARVARD MEASUREMENTS,
C     EMPLOYING THE BASS ALGORITHM (CO(1+C1*T+C2*T2); THIS IS
C     ONLY FOR THE WAVELENGTH RANGE FROM .34 TO .35 MICRONS;
C     OTHERWISE, THE BASS DATA ALONE HAVE BEEN EMPLOYED BETWEEN
C     .34 AND .245 MICRONS.
C
C     NEW T-DEPENDENT X-SECTIONS BETWEEN .345 AND .36 MICRONS
C     HAVE NOW BEEN ADDED, BASED ON WORK BY CACCIANI, DISARRA
C     AND FIOCCO, UNIVERSITY OF ROME, 1987.  QUADRATIC TEMP
C     HAS BEEN DERIVED, AS ABOVE.
C
C     AGREEMENT AMONGST THE FOUR DATA SETS IS REASONABLE (<10%)
C     AND OFTEN EXCELLENT (0-3%)
C
C
      COMMON /O3HH2/  V1C,V2C,DVC,NC,
     X           O32001(88),C20086(80),C20166(80),C20246(65),C20311(16),
     X           C20327(80),C20407(1),
     X           C20001(80),C20081(80),C20161(80),C20241(80),C20321(80),
     X           C20401(80),C20481(80),C20561(80),C20641(80),C20721(80),
     X           C20801(80),C20881(80),C20961(80),C21041(80),C21121(80),
     X           C21201(80),C21281(80),C21361(80),C21441(80),C21521(80),
     X           C21601(80),C21681(80),C21761(80),C21841(80),C21921(80),
     X           C22001(80),C22081(80),C22161(80),C22241(40)
C
C     DATA V1C /29405./, V2C /40800./ ,DVC /5./, NC /2280/   BASS
      DATA V1C /27370./, V2C /40800./ ,DVC /5./, NC /2690/
C
      DATA O32001/88*1.0E-5/

      DATA C20086/
     X 1.29359E-05, 1.55806E-05, 2.00719E-05, 2.64912E-05, 3.48207E-05,
     X 4.36986E-05, 5.31318E-05, 6.13173E-05, 6.89465E-05, 7.56793E-05,
     X 8.26345E-05, 8.90916E-05, 9.38759E-05, 9.22998E-05, 9.03184E-05,
     X 8.65369E-05, 8.58531E-05, 8.55635E-05, 8.40418E-05, 8.11983E-05,
     X 7.58246E-05, 7.29282E-05, 7.32629E-05, 7.04060E-05, 6.71451E-05,
     X 6.56515E-05, 6.68943E-05, 6.32785E-05, 5.88386E-05, 5.70860E-05,
     X 5.64435E-05, 5.49441E-05, 5.70845E-05, 5.89357E-05, 6.14433E-05,
     X 5.91790E-05, 5.31727E-05, 5.14007E-05, 4.74318E-05, 4.35356E-05,
     X 3.93903E-05, 3.70963E-05, 3.63867E-05, 4.05296E-05, 4.48891E-05,
     X 5.37190E-05, 5.70440E-05, 4.60408E-05, 5.25778E-05, 6.81728E-05,
     X 7.27275E-05, 6.81353E-05, 6.48386E-05, 5.46521E-05, 4.93098E-05,
     X 5.04438E-05, 5.30309E-05, 5.28788E-05, 5.47387E-05, 4.52523E-05,
     X 5.29451E-05, 7.42215E-05, 1.08971E-04, 1.40085E-04, 1.46553E-04,
     X 1.43526E-04, 1.39051E-04, 1.40983E-04, 1.45564E-04, 1.55589E-04,
     X 1.66142E-04, 1.82840E-04, 2.06486E-04, 2.24339E-04, 2.29268E-04,
     X 2.13109E-04, 2.00305E-04, 1.99955E-04, 2.18566E-04, 2.24182E-04/
      DATA C20166/
     X 2.33505E-04, 2.31824E-04, 2.22666E-04, 2.23905E-04, 2.38131E-04,
     X 2.54322E-04, 2.69548E-04, 2.62953E-04, 2.67609E-04, 2.70567E-04,
     X 2.70689E-04, 2.68251E-04, 2.66029E-04, 2.60053E-04, 2.61689E-04,
     X 2.56582E-04, 2.43655E-04, 2.38792E-04, 2.45309E-04, 2.31061E-04,
     X 2.22837E-04, 2.16440E-04, 2.19032E-04, 1.85634E-04, 1.74638E-04,
     X 1.51767E-04, 1.38480E-04, 1.32506E-04, 1.28317E-04, 1.26855E-04,
     X 1.27123E-04, 1.24040E-04, 1.19202E-04, 1.28649E-04, 1.36271E-04,
     X 1.42080E-04, 1.47804E-04, 1.39534E-04, 1.27284E-04, 1.09554E-04,
     X 8.69470E-05, 6.72096E-05, 5.23407E-05, 5.12433E-05, 5.15794E-05,
     X 4.94683E-05, 4.95809E-05, 4.07499E-05, 3.14984E-05, 1.46457E-05,
     X 6.98660E-06, 1.85313E-05, 5.48879E-05, 1.09447E-04, 1.52536E-04,
     X 1.78778E-04, 1.91128E-04, 1.99161E-04, 2.02937E-04, 1.95527E-04,
     X 1.92214E-04, 1.83376E-04, 1.81710E-04, 1.82283E-04, 1.75182E-04,
     X 1.72406E-04, 1.68170E-04, 1.67400E-04, 1.69469E-04, 1.69092E-04,
     X 1.65985E-04, 1.66912E-04, 1.74226E-04, 1.85036E-04, 1.85517E-04,
     X 1.85805E-04, 1.73809E-04, 1.67628E-04, 1.57690E-04, 1.54952E-04/
      DATA C20246/
     X 1.53707E-04, 1.57710E-04, 1.58175E-04, 1.67253E-04, 1.82079E-04,
     X 1.91285E-04, 1.96564E-04, 2.03822E-04, 1.93736E-04, 1.82924E-04,
     X 1.73610E-04, 1.69904E-04, 1.66725E-04, 1.63747E-04, 1.63129E-04,
     X 1.62435E-04, 1.67218E-04, 1.69507E-04, 1.70744E-04, 1.65839E-04,
     X 1.72077E-04, 1.67734E-04, 1.51487E-04, 1.43770E-04, 1.37435E-04,
     X 1.25172E-04, 1.12395E-04, 1.07991E-04, 1.00345E-04, 9.36611E-05,
     X 9.59763E-05, 9.26600E-05, 1.00120E-04, 1.04746E-04, 1.10222E-04,
     X 1.03308E-04, 8.97457E-05, 7.91634E-05, 7.50275E-05, 8.30832E-05,
     X 1.01191E-04, 1.21560E-04, 1.34840E-04, 1.38712E-04, 1.41746E-04,
     X 1.39578E-04, 1.37052E-04, 1.33850E-04, 1.26641E-04, 1.21342E-04,
     X 1.17669E-04, 1.25973E-04, 1.33623E-04, 1.33839E-04, 1.24427E-04,
     X 1.02462E-04, 8.76101E-05, 8.27912E-05, 8.29040E-05, 7.78590E-05,
     X 7.39042E-05, 6.45765E-05, 5.70151E-05, 5.11846E-05, 4.83163E-05/
      DATA C20311/
     X                                                     5.4470E-05,
     X 5.3312E-05,  5.3135E-05,  5.3619E-05,  5.3686E-05,  5.2308E-05,
     X 5.0441E-05,  4.8402E-05,  4.7476E-05,  4.6215E-05,  4.4507E-05,
     X 4.3830E-05,  4.0508E-05,  3.8931E-05,  3.5525E-05,  3.4722E-05/
      DATA C20327/
     X 3.2743E-05,  2.8456E-05,  2.8318E-05,  2.8132E-05,  2.6221E-05,
     X 2.5673E-05,  2.5521E-05,  2.4588E-05,  2.4093E-05,  2.2787E-05,
     X 2.1241E-05,  1.8553E-05,  1.5871E-05,  1.3462E-05,  1.2553E-05,
     X 1.6276E-05,  2.8296E-05,  3.8817E-05,  4.2733E-05,  4.2429E-05,
     X 4.0954E-05,  3.9868E-05,  3.7669E-05,  3.6312E-05,  3.5535E-05,
     X 3.5895E-05,  3.6349E-05,  3.9033E-05,  4.4512E-05,  5.0066E-05,
     X 5.4572E-05,  5.6710E-05,  5.6615E-05,  5.7520E-05,  5.8034E-05,
     X 5.7927E-05,  5.6027E-05,  5.5242E-05,  5.4974E-05,  5.2927E-05,
     X 5.1638E-05,  5.2027E-05,  5.1420E-05,  5.1618E-05,  5.0253E-05,
     X 5.0509E-05,  4.9376E-05,  5.0135E-05,  4.9191E-05,  4.9210E-05,
     X 4.8216E-05,  4.7487E-05,  4.5749E-05,  4.5884E-05,  4.3852E-05,
     X 4.3824E-05,  4.2612E-05,  4.0349E-05,  4.0177E-05,  3.7474E-05,
     X 3.8120E-05,  3.6915E-05,  3.5823E-05,  3.5186E-05,  3.3638E-05,
     X 3.3451E-05,  3.2428E-05,  3.2349E-05,  3.0183E-05,  2.8436E-05,
     X 2.6440E-05,  2.3597E-05,  2.1875E-05,  1.8164E-05,  1.6430E-05,
     X 1.3159E-05,  9.2907E-06,  7.4243E-06,  6.0469E-06,  5.4951E-06/
      DATA C20407/
     X 8.7642E-06/
      DATA C20001 /
     X 2.16295E-05, 1.69111E-05, 5.39633E-05, 1.01866E-04, 8.28657E-05,
     X 9.16593E-05, 8.88666E-05, 1.37764E-04, 1.44322E-04, 1.20659E-04,
     X 1.10332E-04, 1.01317E-04, 9.09964E-05, 1.17148E-04, 1.18000E-04,
     X 7.21801E-05, 1.10550E-04, 1.32672E-04, 1.02474E-04, 1.10434E-04,
     X 1.38759E-04, 8.92135E-05, 9.18239E-05, 9.08256E-05, 7.02969E-05,
     X 1.12827E-04, 8.25561E-05, 1.39555E-04, 6.72239E-05, 7.82804E-05,
     X 8.56258E-05, 8.61068E-05, 7.16732E-05, 6.25720E-05, 5.23957E-05,
     X 3.78801E-05, 4.37281E-05, 4.99821E-05, 5.96976E-05, 7.19070E-05,
     X 3.89579E-05, 5.30171E-05, 3.92507E-05, 4.93901E-05, 4.53047E-05,
     X 4.89955E-05, 4.61649E-05, 3.75742E-05, 3.14124E-05, 2.37893E-05,
     X 3.34899E-06, 3.08080E-05, 5.35883E-05, 3.39838E-05, 7.02334E-05,
     X 7.24784E-05, 7.46533E-05, 6.22257E-05, 6.38945E-05, 6.73423E-05,
     X 4.51321E-05, 5.91854E-05, 5.51601E-05, 4.41923E-05, 3.59217E-05,
     X 4.08520E-05, 6.15981E-05, 6.66549E-05, 8.26031E-05, 1.13556E-04,
     X 8.72988E-05, 9.71052E-05, 9.31839E-05, 8.73745E-05, 8.61717E-05,
     X 6.05645E-05, 6.51131E-05, 6.93393E-05, 7.01096E-05, 6.43565E-05/
      DATA C20081 /
     X 7.36929E-05, 7.66881E-05, 7.60815E-05, 7.13570E-05, 8.40487E-05,
     X 8.51489E-05, 7.54168E-05, 6.72694E-05, 4.75508E-05, 3.59379E-05,
     X 4.24698E-05, 4.17850E-05, 4.56047E-05, 4.12779E-05, 4.55933E-05,
     X 4.27941E-05, 4.42230E-05, 3.68525E-05, 3.83392E-05, 3.83722E-05,
     X 4.64904E-05, 3.33878E-05, 3.53027E-05, 3.54694E-05, 2.36233E-05,
     X 2.99641E-05, 2.56097E-05, 2.14134E-05, 2.74403E-05, 2.83896E-05,
     X 3.17082E-05, 1.75526E-05, 2.80382E-05, 3.18009E-05, 4.08715E-05,
     X 4.77807E-05, 5.00609E-05, 5.12459E-05, 4.44062E-05, 4.74942E-05,
     X 4.99882E-05, 5.18837E-05, 5.03246E-05, 5.55168E-05, 5.35853E-05,
     X 4.81834E-05, 6.66231E-05, 5.26670E-05, 6.84700E-05, 6.53412E-05,
     X 5.71740E-05, 4.61076E-05, 3.90239E-05, 4.72924E-05, 6.32194E-05,
     X 5.20868E-05, 4.81039E-05, 3.71748E-05, 4.37492E-05, 3.63959E-05,
     X 3.79823E-05, 3.72225E-05, 3.02360E-05, 3.22961E-05, 3.43398E-05,
     X 3.57176E-05, 2.65446E-05, 3.29388E-05, 1.65455E-05, 2.66173E-05,
     X 1.74277E-05, 1.74324E-05, 1.27879E-05, 1.46247E-05, 1.92378E-05,
     X 2.20049E-05, 1.44790E-05, 2.49244E-05, 2.29209E-05, 1.76192E-05/
      DATA C20161 /
     X 1.84528E-05, 2.54350E-05, 3.33972E-05, 3.69190E-05, 2.92139E-05,
     X 2.47666E-05, 2.86764E-05, 1.48163E-05, 1.80461E-05, 2.84545E-05,
     X 2.41064E-05, 2.85721E-05, 3.31996E-05, 3.75973E-05, 3.73874E-05,
     X 4.69293E-05, 5.12665E-05, 5.35607E-05, 4.64577E-05, 3.59887E-05,
     X 3.39168E-05, 3.89746E-05, 3.12196E-05, 3.70907E-05, 3.95172E-05,
     X 4.61642E-05, 4.26029E-05, 4.17856E-05, 4.51437E-05, 4.04189E-05,
     X 4.19251E-05, 4.53977E-05, 3.69860E-05, 4.20904E-05, 3.69735E-05,
     X 3.57898E-05, 3.47729E-05, 3.14280E-05, 2.71197E-05, 3.34380E-05,
     X 2.69843E-05, 2.88036E-05, 2.51912E-05, 2.45699E-05, 2.23184E-05,
     X 2.50563E-05, 2.24493E-05, 1.77101E-05, 1.64763E-05, 1.34978E-05,
     X 1.57081E-05, 1.45966E-05, 1.02722E-05, 2.07177E-05, 1.47662E-05,
     X 1.50721E-05, 1.24431E-05, 1.51572E-05, 1.92210E-05, 2.06047E-05,
     X 2.02921E-05, 3.22062E-05, 2.37112E-05, 1.94803E-05, 2.40726E-05,
     X 2.11531E-05, 1.89158E-05, 2.46957E-05, 2.63175E-05, 2.57747E-05,
     X 2.22047E-05, 2.52755E-05, 2.80848E-05, 3.75157E-05, 4.09915E-05,
     X 4.04853E-05, 3.21661E-05, 3.15652E-05, 3.21576E-05, 3.67060E-05/
      DATA C20241 /
     X 3.13071E-05, 2.84939E-05, 2.71169E-05, 2.99559E-05, 2.94631E-05,
     X 3.26716E-05, 2.99028E-05, 2.60045E-05, 3.15375E-05, 3.12895E-05,
     X 2.77767E-05, 2.43976E-05, 2.10764E-05, 2.22725E-05, 2.04581E-05,
     X 1.63509E-05, 1.60028E-05, 1.60294E-05, 1.62366E-05, 1.89293E-05,
     X 1.79675E-05, 1.89259E-05, 1.68300E-05, 1.99460E-05, 2.42370E-05,
     X 2.64738E-05, 1.93137E-05, 1.39460E-05, 1.32222E-05, 1.38752E-05,
     X 1.62071E-05, 1.79652E-05, 1.63772E-05, 1.56251E-05, 1.81918E-05,
     X 1.46111E-05, 2.92174E-05, 2.94263E-05, 2.46180E-05, 2.93333E-05,
     X 3.13657E-05, 2.97686E-05, 2.78387E-05, 2.40924E-05, 2.93369E-05,
     X 2.93747E-05, 2.77665E-05, 3.00814E-05, 3.01068E-05, 3.62275E-05,
     X 3.56613E-05, 3.66913E-05, 3.56280E-05, 3.52856E-05, 3.63928E-05,
     X 2.96738E-05, 2.90314E-05, 2.62972E-05, 2.15250E-05, 1.97910E-05,
     X 2.02314E-05, 2.20209E-05, 2.05131E-05, 2.12017E-05, 1.96689E-05,
     X 1.61907E-05, 1.57662E-05, 1.58239E-05, 1.54650E-05, 1.46376E-05,
     X 1.32891E-05, 1.30511E-05, 1.17635E-05, 1.28585E-05, 1.12887E-05,
     X 1.32627E-05, 1.31833E-05, 1.68679E-05, 1.98092E-05, 2.70744E-05/
      DATA C20321 /
     X 2.22033E-05, 1.63430E-05, 1.61104E-05, 1.50865E-05, 1.54382E-05,
     X 1.55654E-05, 1.67924E-05, 1.89185E-05, 1.96791E-05, 2.14894E-05,
     X 2.76137E-05, 2.67339E-05, 2.79423E-05, 2.54664E-05, 3.10707E-05,
     X 2.72745E-05, 2.60940E-05, 2.47736E-05, 2.21105E-05, 2.20357E-05,
     X 2.26499E-05, 2.34137E-05, 2.29537E-05, 2.36157E-05, 2.48244E-05,
     X 2.26667E-05, 2.07781E-05, 2.11702E-05, 1.91214E-05, 1.62172E-05,
     X 1.61285E-05, 1.63952E-05, 1.68156E-05, 1.61236E-05, 1.56611E-05,
     X 1.47697E-05, 1.50856E-05, 1.44169E-05, 1.63816E-05, 1.74283E-05,
     X 1.49853E-05, 1.62444E-05, 1.70007E-05, 1.60371E-05, 1.22713E-05,
     X 1.45518E-05, 1.35051E-05, 1.40787E-05,-1.54925E-05,-2.15204E-05,
     X-4.04516E-06, 2.22439E-05, 3.21262E-05, 3.83792E-05, 4.44462E-05,
     X 4.44192E-05, 2.77328E-05, 4.10549E-06, 4.48758E-06,-1.27771E-05,
     X-2.17204E-05,-1.23979E-05,-1.04928E-05, 7.43085E-06, 1.55350E-05,
     X 3.15204E-05, 3.17601E-05, 2.93677E-05, 3.42485E-05, 3.87087E-05,
     X 3.61242E-05, 2.62406E-05, 3.31686E-05, 3.54314E-05, 2.50625E-05,
     X 2.60444E-05, 4.10729E-05, 3.47247E-05, 3.31716E-05, 3.34778E-05/
      DATA C20401 /
     X 4.03029E-05, 4.09241E-05, 3.96717E-05, 3.53410E-05, 2.81048E-05,
     X 1.98891E-05, 1.92314E-05, 2.82525E-05, 3.76641E-05, 4.34135E-05,
     X 4.24570E-05, 3.98429E-05, 3.29417E-05, 2.16679E-05, 8.88085E-06,
     X-5.05319E-06,-8.14815E-06,-5.01930E-06, 7.13565E-06, 2.00949E-05,
     X 2.65988E-05, 2.77656E-05, 2.09299E-05, 1.98968E-05, 2.04835E-05,
     X 1.75254E-05, 6.48674E-06, 3.14323E-06, 1.93242E-06, 3.86745E-06,
     X 1.39727E-05, 2.10731E-05, 2.66432E-05, 2.69551E-05, 2.57453E-05,
     X 2.72834E-05, 2.58860E-05, 2.51266E-05, 1.76048E-05, 2.03072E-05,
     X 2.61960E-05, 2.36230E-05, 1.81172E-05, 1.33972E-05, 1.60959E-05,
     X 1.61081E-05, 2.34099E-05, 2.64979E-05, 2.36894E-05, 2.13665E-05,
     X 2.16774E-05, 2.52566E-05, 1.99785E-05, 1.40414E-05, 1.39948E-05,
     X 1.32637E-05, 7.24742E-06, 1.11395E-06,-1.27323E-06, 4.56637E-07,
     X 6.93250E-06, 5.07198E-06, 7.90632E-06, 9.08149E-06, 1.03602E-05,
     X 2.17425E-05, 2.71741E-05, 2.16875E-05, 1.95088E-05, 1.56568E-05,
     X 8.41152E-06, 1.26749E-05, 1.17673E-05, 9.96037E-06, 1.21982E-05,
     X 1.31854E-05, 1.50216E-05, 1.72214E-05, 2.02773E-05, 2.09625E-05/
      DATA C20481 /
     X 1.66656E-05, 1.45666E-05, 1.66608E-05, 2.04989E-05, 2.21395E-05,
     X 2.35993E-05, 2.69390E-05, 2.13921E-05, 1.72643E-05, 1.70995E-05,
     X 1.78241E-05, 1.85308E-05, 1.80360E-05, 1.48619E-05, 1.90369E-05,
     X 1.51089E-05, 1.22705E-05, 1.62608E-05, 1.41637E-05, 1.23786E-05,
     X 7.02677E-06, 8.89811E-06, 1.07379E-05, 1.23677E-05, 1.48196E-05,
     X 2.05770E-05, 1.70994E-05, 1.00072E-05, 1.76119E-05, 1.41779E-05,
     X 1.34358E-05, 1.58674E-05, 1.65837E-05, 1.69569E-05, 1.40381E-05,
     X 1.46118E-05, 1.30556E-05, 1.97204E-05, 1.97488E-05, 1.64524E-05,
     X 1.73764E-05, 1.66355E-05, 1.64419E-05, 1.65486E-05, 1.21523E-05,
     X 1.51513E-05, 1.60354E-05, 1.38528E-05, 1.45538E-05, 1.71702E-05,
     X 1.56336E-05, 1.31279E-05, 1.47346E-05, 1.70719E-05, 1.75588E-05,
     X 1.55187E-05, 1.29598E-05, 1.38463E-05, 1.35382E-05, 1.16062E-05,
     X 1.37014E-05, 1.34487E-05, 1.15536E-05, 1.33597E-05, 9.24478E-06,
     X 7.28477E-06, 1.40321E-05, 1.31518E-05, 1.03118E-05, 8.59764E-06,
     X 1.57138E-05, 1.20792E-05, 1.49440E-05, 1.34375E-05, 1.54686E-05,
     X 1.65346E-05, 1.33823E-05, 1.37238E-05, 1.36128E-05, 1.46206E-05/
      DATA C20561 /
     X 1.40777E-05, 1.59980E-05, 1.30180E-05, 1.01390E-05, 1.12366E-05,
     X 9.86099E-06, 1.10702E-05, 1.26783E-05, 9.51072E-06, 8.07299E-06,
     X 1.22955E-05, 1.53506E-05, 1.29711E-05, 9.78759E-06, 1.28800E-05,
     X 1.39702E-05, 1.64832E-05, 1.06473E-05, 1.15419E-05, 1.63795E-05,
     X 1.69837E-05, 1.72726E-05, 1.77231E-05, 1.62337E-05, 1.20881E-05,
     X 1.13210E-05, 1.20531E-05, 1.31374E-05, 1.22259E-05, 1.27802E-05,
     X 1.38962E-05, 8.87355E-06, 9.42264E-06, 1.02075E-05, 7.91816E-06,
     X 9.66835E-06, 1.24921E-05, 8.43227E-06, 1.10637E-05, 1.03958E-05,
     X 9.40996E-06, 1.22922E-05, 1.21088E-05, 1.30116E-05, 1.18776E-05,
     X 1.42245E-05, 1.34745E-05, 1.11165E-05, 1.29914E-05, 1.29801E-05,
     X 1.10895E-05, 1.12331E-05, 9.03490E-06, 9.33726E-06, 9.63923E-06,
     X 1.11299E-05, 9.53481E-06, 1.21708E-05, 1.11951E-05, 7.22558E-06,
     X 6.66928E-06, 1.08926E-05, 1.07870E-05, 9.23485E-06, 8.50452E-06,
     X 9.41914E-06, 8.74027E-06, 8.93322E-06, 9.79061E-06, 8.26490E-06,
     X 8.37630E-06, 1.17064E-05, 1.10176E-05, 1.11587E-05, 9.45563E-06,
     X 1.18352E-05, 7.79327E-06, 9.22766E-06, 1.01868E-05, 8.23925E-06/
      DATA C20641 /
     X 9.23706E-06, 1.04428E-05, 8.80392E-06, 9.37098E-06, 7.43126E-06,
     X 7.01424E-06, 9.29360E-06, 8.97171E-06, 9.31718E-06, 9.87118E-06,
     X 8.11419E-06, 8.77416E-06, 9.96927E-06, 8.87533E-06, 9.33163E-06,
     X 7.41505E-06, 9.39988E-06, 1.17932E-05, 1.03287E-05, 9.17415E-06,
     X 8.43035E-06, 8.00040E-06, 8.33346E-06, 7.66787E-06, 7.18411E-06,
     X 1.06236E-05, 1.05559E-05, 8.49187E-06, 9.22472E-06, 8.16512E-06,
     X 8.35687E-06, 1.06325E-05, 9.80273E-06, 9.01599E-06, 9.20499E-06,
     X 9.98417E-06, 9.23191E-06, 6.98769E-06, 5.17748E-06, 4.57130E-06,
     X 8.18492E-06, 9.98095E-06, 7.52148E-06, 1.33038E-05, 8.17630E-06,
     X 1.02454E-05, 9.62706E-06, 9.44304E-06, 8.86704E-06, 8.88116E-06,
     X 8.79062E-06, 8.20042E-06, 8.55789E-06, 9.26249E-06, 1.00467E-05,
     X 7.96012E-06, 9.08773E-06, 1.01481E-05, 8.84360E-06, 7.94928E-06,
     X 6.68425E-06, 8.56576E-06, 1.05282E-05, 1.10647E-05, 9.91625E-06,
     X 7.95356E-06, 8.66443E-06, 9.13551E-06, 1.04870E-05, 9.79244E-06,
     X 1.26214E-05, 8.42148E-06, 8.13468E-06, 1.11338E-05, 1.06780E-05,
     X 8.54578E-06, 7.82119E-06, 8.33258E-06, 8.23644E-06, 5.95583E-06/
      DATA C20721 /
     X 5.85592E-06, 4.05898E-06, 6.39260E-06, 8.43280E-06, 8.76251E-06,
     X 6.70423E-06, 6.81368E-06, 7.43506E-06, 7.14376E-06, 6.51065E-06,
     X 5.65633E-06, 5.42394E-06, 7.10817E-06, 4.78831E-06, 6.29380E-06,
     X 4.87344E-06, 6.81764E-06, 6.51611E-06, 5.70526E-06, 6.50590E-06,
     X 6.61568E-06, 5.39248E-06, 6.32002E-06, 7.98976E-06, 7.73795E-06,
     X 4.85788E-06, 5.83443E-06, 6.11694E-06, 5.40408E-06, 5.00946E-06,
     X 5.62153E-06, 6.30263E-06, 6.05764E-06, 5.53274E-06, 5.80664E-06,
     X 5.18684E-06, 6.85555E-06, 6.22889E-06, 6.06959E-06, 6.49228E-06,
     X 5.64064E-06, 4.92690E-06, 5.77661E-06, 7.18450E-06, 7.38658E-06,
     X 6.77379E-06, 5.74668E-06, 6.68355E-06, 6.13655E-06, 6.43266E-06,
     X 7.08896E-06, 7.71187E-06, 7.37273E-06, 6.75882E-06, 6.39307E-06,
     X 4.59520E-06, 5.10323E-06, 5.80178E-06, 6.88172E-06, 6.68825E-06,
     X 7.50416E-06, 6.14975E-06, 6.51422E-06, 7.74942E-06, 8.11492E-06,
     X 1.19607E-05, 7.92722E-06, 4.47848E-06, 6.02524E-06, 9.74067E-06,
     X 1.02429E-05, 8.60819E-06, 8.57044E-06, 1.09196E-05, 1.02048E-05,
     X 3.86222E-06, 9.26104E-06, 7.33341E-06, 9.08181E-06, 1.05569E-05/
      DATA C20801 /
     X 1.06776E-05, 1.10247E-05, 1.04520E-05, 8.78328E-06, 7.60679E-06,
     X 7.27896E-06, 9.72776E-06, 5.16039E-06, 1.03134E-05, 1.09088E-05,
     X 8.12575E-06, 7.61685E-06, 8.16346E-06, 5.91269E-06, 3.61448E-06,
     X 8.74336E-06, 1.03990E-05, 6.25691E-06, 7.04541E-06, 7.94348E-06,
     X 8.39807E-06, 8.67342E-06, 8.32173E-06, 7.56015E-06, 8.31782E-06,
     X 6.36556E-06, 6.99328E-06, 6.24490E-06, 6.73080E-06, 6.95852E-06,
     X 7.55508E-06, 7.74168E-06, 7.90414E-06, 8.94934E-06, 7.99809E-06,
     X 6.12528E-06, 9.04115E-06, 7.14535E-06, 5.88625E-06, 6.43941E-06,
     X 7.11566E-06, 7.47425E-06, 8.23805E-06, 6.19919E-06, 7.31614E-06,
     X 8.24852E-06, 6.82172E-06, 5.45362E-06, 6.66115E-06, 8.44300E-06,
     X 8.07530E-06, 7.22735E-06, 5.85614E-06, 5.13900E-06, 6.03215E-06,
     X 6.59491E-06, 4.81592E-06, 4.48587E-06, 7.11525E-06, 8.36201E-06,
     X 7.11669E-06, 2.80033E-06, 6.50756E-06, 9.43974E-06, 5.22402E-06,
     X 3.82334E-06, 7.29963E-06, 8.62313E-06, 7.42018E-06, 4.56506E-06,
     X 5.29972E-06, 5.62787E-06, 4.63852E-06, 5.18329E-06, 7.01884E-06,
     X 7.24888E-06, 5.18157E-06, 5.40219E-06, 5.92412E-06, 4.97977E-06/
      DATA C20881 /
     X 5.29040E-06, 5.33812E-06, 4.76620E-06, 4.65759E-06, 5.10546E-06,
     X 6.49525E-06, 4.43416E-06, 5.30223E-06, 3.27044E-06, 2.55324E-06,
     X 4.85017E-06, 7.46556E-06, 8.04448E-06, 5.14009E-06, 6.09755E-06,
     X 5.38381E-06, 6.41959E-06, 6.59233E-06, 4.83160E-06, 3.81289E-06,
     X 5.37013E-06, 5.69212E-06, 5.54983E-06, 5.73495E-06, 4.00639E-06,
     X 2.33817E-06, 2.55751E-06, 3.29627E-06, 3.59845E-06, 6.20623E-06,
     X 4.47088E-06, 3.49267E-06, 3.09273E-06, 3.32506E-06, 4.83353E-06,
     X 6.39001E-06, 3.78074E-06, 4.07848E-06, 4.01811E-06, 3.19767E-06,
     X 3.34053E-06, 4.34246E-06, 3.68003E-06, 3.01090E-06, 3.98545E-06,
     X 2.72338E-06, 1.90024E-06, 2.77553E-06, 3.73381E-06, 2.58685E-06,
     X 1.70987E-06,-5.48480E-07, 1.64591E-06, 2.43481E-06, 2.52116E-06,
     X 2.19316E-06, 1.32392E-06, 1.75370E-06, 2.65409E-07, 2.22278E-06,
     X 2.53079E-06, 2.87260E-06, 1.87600E-06,-3.84453E-07, 1.80836E-06,
     X 9.28123E-07, 1.94986E-06, 2.40483E-06, 2.79865E-06, 2.86361E-06,
     X 2.63868E-06, 3.34704E-06, 3.32132E-06, 2.58463E-06, 2.45684E-06,
     X 3.35043E-06, 3.19848E-06, 1.73037E-06, 2.98206E-06, 2.77491E-06/
      DATA C20961 /
     X 6.51674E-07, 2.52219E-06, 2.97136E-06, 1.96700E-06, 2.29350E-06,
     X 3.01956E-06, 3.20811E-06, 1.30467E-06, 1.68172E-06, 2.56264E-06,
     X 2.46167E-06, 1.78221E-06, 2.31647E-06, 2.69480E-06, 2.63619E-06,
     X 1.81319E-06, 1.83448E-06, 2.23432E-06, 8.14045E-07, 8.75863E-07,
     X 1.61350E-06, 1.59796E-06, 2.08419E-06, 1.89665E-06, 6.93584E-07,
     X 1.09880E-06, 3.79031E-07,-3.36470E-07, 1.04326E-06, 1.06497E-06,
     X 2.15108E-07, 3.28774E-07,-5.17613E-07, 1.27762E-06, 8.22924E-07,
     X 4.92835E-07, 2.24698E-08,-1.99111E-07, 1.30262E-06,-3.81299E-07,
     X 9.55084E-07, 2.17641E-07,-6.03874E-08, 8.44121E-07, 1.72391E-06,
     X 1.66921E-06, 2.19855E-06, 1.17655E-06, 1.79637E-06, 3.31670E-06,
     X 3.40206E-06, 6.05670E-07, 2.08299E-06, 2.10121E-06, 1.68598E-06,
     X 2.21155E-06, 2.43221E-06, 5.81282E-08, 1.62613E-06,-5.49850E-07,
     X 2.14143E-07, 1.21751E-06, 2.30470E-06, 4.27911E-06, 2.96622E-06,
     X 8.67534E-07, 9.12041E-07, 2.48797E-06, 9.43519E-07,-3.60949E-06,
     X 2.01928E-06, 1.88873E-06, 8.06749E-07, 7.33519E-07, 1.17440E-06,
     X 1.69744E-06, 3.64492E-06, 3.11556E-06, 8.89471E-07, 1.93064E-06/
      DATA C21041 /
     X 3.02787E-06, 1.92575E-06, 1.73720E-06,-1.32700E-07, 1.41743E-06,
     X 2.24632E-06, 2.47945E-06, 2.05151E-06,-9.56031E-07, 2.57317E-07,
     X 3.00980E-06, 3.07981E-06, 2.78202E-06, 3.02555E-06, 5.48784E-09,
     X 2.37693E-06, 2.90011E-06, 2.93608E-06, 2.14837E-06, 6.55832E-07,
     X 3.41155E-07,-2.13884E-06, 2.52553E-06, 4.27109E-06, 3.33766E-06,
     X 3.07708E-06, 2.66405E-06, 3.22850E-06,-5.78879E-07,-6.06194E-07,
     X 1.72864E-06, 1.57072E-06,-3.39701E-07, 7.21540E-08, 1.67012E-06,
     X 2.48568E-06, 2.70214E-06, 3.62383E-06, 2.20408E-06, 1.19395E-06,
     X 1.53825E-06, 2.37511E-06, 2.66754E-06, 1.77020E-06, 5.40420E-07,
     X 2.01156E-06, 3.27498E-06, 3.04720E-06, 1.96213E-06, 3.71633E-06,
     X 2.07886E-06, 1.60069E-06, 5.33370E-07, 1.33966E-07, 2.16073E-06,
     X 8.81457E-07, 1.12880E-06, 2.40509E-06, 2.94252E-06, 2.22899E-06,
     X 1.80941E-06, 2.68577E-06, 2.44584E-06, 2.51720E-06, 2.64857E-06,
     X 2.24182E-06, 1.62007E-06, 2.60421E-06, 3.09782E-06, 3.11099E-06,
     X 3.81513E-06, 6.91606E-06, 3.28767E-06, 3.44175E-06, 4.16771E-06,
     X 3.75452E-06, 2.21050E-06, 2.99939E-06, 2.86993E-06, 2.47080E-06/
      DATA C21121 /
     X 2.33607E-06, 2.68568E-06, 3.39344E-06, 6.09518E-06, 5.10422E-06,
     X 4.04027E-06, 4.01363E-06, 4.53142E-06, 2.94424E-06, 4.76694E-06,
     X 6.44206E-06, 7.86435E-06, 8.55564E-06, 6.00857E-06, 5.48073E-06,
     X 1.56287E-06,-1.16619E-06,-1.85215E-06,-3.04762E-06,-3.45420E-07,
     X 2.48111E-07,-1.39302E-07,-6.27593E-07,-5.26792E-07, 4.81454E-08,
     X-3.08631E-08,-1.02976E-06,-1.54919E-06,-9.34044E-07,-1.02507E-06,
     X-1.39794E-06,-1.15709E-06,-1.04875E-06,-1.64379E-06,-2.97514E-06,
     X-3.22236E-07,-1.18978E-06,-2.85325E-06,-3.93143E-06,-4.15349E-06,
     X-2.33228E-06,-3.27125E-06,-2.44987E-06,-1.44460E-06,-3.59727E-06,
     X-7.18516E-07,-1.53237E-06,-1.53526E-06,-1.56450E-06,-2.91088E-06,
     X-8.52134E-07,-1.44575E-07,-1.50350E-06,-2.92806E-06,-2.47710E-06,
     X-9.71202E-07,-9.82754E-07,-1.09924E-06,-6.08199E-07, 3.62885E-07,
     X-6.67372E-07,-1.00033E-06,-1.12001E-06,-1.06624E-06,-9.23789E-07,
     X-9.83788E-07,-2.11656E-06,-2.45001E-06,-2.75874E-06,-3.36003E-06,
     X-3.38364E-06,-2.63747E-06,-3.11047E-06,-3.75258E-06,-3.83211E-06,
     X-3.52833E-06,-3.48464E-06,-3.77021E-06,-4.26887E-06,-4.23917E-06/
      DATA C21201 /
     X-1.42438E-06,-2.48477E-06,-2.84719E-06,-2.70247E-06,-2.50588E-06,
     X-2.22900E-06,-1.78393E-06,-1.76826E-06,-2.16396E-06,-2.67543E-06,
     X-2.23706E-06,-2.31793E-06,-2.87590E-06,-3.07803E-06,-2.50493E-06,
     X-4.54223E-06,-5.15511E-06,-5.39690E-06,-4.89633E-06,-3.33710E-06,
     X-4.56583E-06,-4.78877E-06,-3.93508E-06,-3.29027E-06,-4.95668E-06,
     X-6.01801E-06,-5.76016E-06,-5.34657E-06,-5.29080E-06,-5.57133E-06,
     X-5.73135E-06,-5.39374E-06,-5.09808E-06,-5.12874E-06,-5.20269E-06,
     X-7.30702E-06,-7.04220E-06,-5.96514E-06,-5.74802E-06,-4.53961E-06,
     X-4.42127E-06,-4.63922E-06,-4.80622E-06,-4.69659E-06,-5.96786E-06,
     X-6.29800E-06,-4.75452E-06,-2.85907E-06,-5.33662E-06,-5.31681E-06,
     X-5.04646E-06,-5.21729E-06,-5.93409E-06,-5.73462E-06,-5.44926E-06,
     X-6.43325E-06,-7.74451E-06,-7.83147E-06,-5.51568E-06,-7.37048E-06,
     X-4.25726E-06, 2.32917E-06,-5.61131E-07, 2.05234E-06, 3.74631E-07,
     X-7.66493E-07, 1.42689E-06,-7.79683E-07, 9.06809E-07, 5.13642E-07,
     X-1.52504E-06,-2.12058E-06,-2.50316E-06, 1.03637E-08, 5.60002E-07,
     X-1.48075E-06, 1.94155E-06, 1.91846E-06, 2.78507E-06, 3.90146E-06/
      DATA C21281 /
     X 3.61409E-06, 3.23677E-06, 4.00022E-06, 3.19157E-06, 4.03034E-07,
     X-2.03929E-06, 1.23366E-06, 3.28589E-06, 3.94168E-06, 3.94672E-06,
     X 3.84619E-06, 2.30400E-07,-2.07799E-06,-1.75115E-06,-5.71958E-07,
     X 2.33425E-06, 2.01664E-06, 6.05673E-07, 9.57363E-07,-8.89924E-07,
     X-4.71331E-07, 2.82826E-07, 5.10859E-07, 3.63512E-07, 9.86288E-07,
     X-4.86309E-07,-2.23163E-06,-1.23370E-06,-2.43131E-07,-2.11498E-06,
     X-1.56756E-06, 2.70905E-06, 1.87606E-08, 7.83721E-08, 1.58444E-06,
     X 2.88574E-06, 1.40306E-06, 2.40883E-06, 2.84063E-06, 3.13820E-06,
     X 3.71016E-06, 3.12975E-06, 3.21981E-06, 2.56191E-06, 1.04624E-06,
     X 1.87464E-07, 7.25329E-07, 1.03650E-06, 7.23663E-07,-4.18739E-07,
     X 9.95744E-07,-1.80878E-07,-1.04044E-06, 3.86965E-07,-9.36186E-07,
     X-4.02271E-07,-2.00231E-07,-5.94965E-07, 4.94038E-07, 3.34585E-07,
     X 4.82255E-07, 1.12599E-06, 2.11763E-06, 2.66807E-07, 2.29324E-07,
     X 7.07005E-07, 3.41907E-07,-1.17115E-07, 9.03089E-07, 1.76844E-06,
     X 1.87134E-06, 2.64057E-06, 4.00395E-07,-4.19679E-07, 6.30769E-07,
     X 1.02725E-06, 1.05876E-06,-4.08660E-07,-2.32668E-06,-2.73468E-06/
      DATA C21361 /
     X-2.40600E-06,-1.81203E-06,-7.96431E-07, 7.40789E-07, 2.73188E-07,
     X 1.68367E-07,-1.27227E-07,-1.05041E-06,-3.51726E-06,-1.64956E-06,
     X-5.63840E-07,-1.61242E-06,-1.33264E-06, 1.56604E-06, 2.35083E-06,
     X 9.26708E-07, 5.41983E-07, 3.54277E-07, 8.53743E-07, 1.54196E-06,
     X 1.19902E-06, 1.10552E-06, 1.63179E-06, 1.96366E-06, 7.82848E-07,
     X-3.34741E-08,-7.90842E-07,-6.45131E-07, 1.36158E-06, 1.62453E-06,
     X 6.68965E-07,-4.86203E-08, 6.83561E-07, 1.89652E-06,-2.80988E-07,
     X-2.30536E-06,-1.90777E-06, 1.31617E-06, 1.27309E-06, 5.90825E-07,
     X 5.65686E-07, 1.23631E-07,-1.70279E-06,-1.60768E-06, 9.69543E-07,
     X 1.01108E-07,-2.02473E-06,-1.75146E-06, 6.33201E-07,-3.59110E-06,
     X-9.71706E-07, 9.16822E-07, 1.40681E-07,-7.16745E-07,-2.11376E-06,
     X-1.00951E-06, 2.12465E-06, 1.06982E-06, 1.44032E-06, 1.49692E-06,
     X 1.07277E-06, 1.37006E-06, 1.66932E-06, 1.75820E-06, 1.41859E-06,
     X-5.84947E-08, 2.17349E-06, 4.27053E-06, 5.27286E-06, 5.87085E-06,
     X 2.42692E-06, 2.39305E-06, 6.19573E-06, 5.12518E-06, 1.27171E-06,
     X-6.81963E-07, 4.16199E-08,-1.36608E-06,-2.53272E-06,-2.37700E-06/
      DATA C21441 /
     X-7.96719E-07, 3.85367E-07,-1.08393E-07,-9.04587E-07,-1.54917E-06,
     X-3.11945E-06,-5.58484E-07, 1.61347E-06, 1.11736E-06, 2.11889E-06,
     X 2.43534E-06, 1.46709E-06,-1.05429E-06, 1.09978E-06, 7.22493E-07,
     X 8.53307E-08, 1.22733E-06, 2.99380E-06, 3.62416E-06, 3.81404E-06,
     X 4.46735E-06, 4.70753E-06, 4.54494E-06, 3.83002E-06, 2.28067E-06,
     X 2.03102E-06, 2.43844E-06, 2.93132E-06, 2.17555E-06, 3.92919E-06,
     X 3.53089E-06, 1.61388E-06, 5.09498E-06, 3.40067E-06, 1.58876E-06,
     X 1.17367E-06, 1.13344E-06, 1.17798E-06, 1.10976E-06, 7.90635E-07,
     X-4.15989E-07,-1.00581E-06,-9.60236E-07,-1.79111E-07,-5.70733E-07,
     X 1.49766E-06, 3.44374E-06, 6.45914E-07, 1.00532E-06, 2.01068E-06,
     X 2.59092E-06, 9.35770E-08, 6.00121E-07, 1.54409E-06, 2.03537E-06,
     X 8.10358E-07, 1.34126E-06, 1.88873E-06, 1.43283E-06,-2.05029E-07,
     X-1.09782E-06,-6.56149E-07, 2.01650E-06, 1.84770E-06, 4.39586E-08,
     X-2.03588E-06,-1.46366E-06,-3.45189E-07, 4.02577E-07, 3.10362E-07,
     X-2.16073E-06,-1.91861E-06,-2.90520E-07, 2.03692E-06, 3.47996E-06,
     X 4.21761E-06, 3.89000E-06, 1.86138E-06, 1.56143E-06, 4.88964E-07/
      DATA C21521 /
     X-9.28184E-07,-4.34315E-07, 8.74954E-07, 1.58417E-06, 1.36880E-06,
     X 2.65016E-06, 4.62623E-06, 5.81990E-06, 4.72139E-06, 1.95905E-06,
     X 1.54151E-06, 2.95768E-06, 4.71536E-06, 2.62359E-06, 9.11513E-07,
     X 4.75677E-07,-1.53801E-06,-2.32382E-06,-2.25220E-06,-1.46641E-06,
     X-2.23014E-06,-2.12604E-06,-1.66259E-06,-2.48856E-06,-2.38895E-06,
     X-2.18158E-06,-1.95841E-06, 4.43899E-07, 1.08517E-06, 1.66370E-07,
     X-2.42342E-06,-7.19331E-07, 3.19532E-07, 3.58690E-07,-2.01979E-07,
     X 5.07242E-07, 1.10562E-06, 1.00419E-06, 1.22379E-06, 7.05180E-07,
     X 1.42283E-07, 8.61092E-07, 8.95236E-07, 1.18043E-07,-1.23589E-06,
     X-6.16316E-07,-1.18947E-06,-1.45838E-06,-1.47522E-09, 1.33867E-06,
     X 9.18310E-07,-8.98949E-07,-2.27314E-06,-1.71510E-06,-7.16704E-07,
     X 8.60666E-09, 5.68015E-07, 1.31219E-06, 1.75478E-06, 5.11790E-07,
     X 3.35270E-07, 5.39243E-07, 9.08467E-07, 1.39382E-06, 1.08806E-06,
     X 1.18589E-06, 3.58461E-06, 2.78668E-06, 1.25964E-06,-2.72255E-07,
     X 1.72305E-06, 1.82937E-06, 7.46252E-07,-1.10555E-06, 2.24967E-07,
     X 6.45674E-07,-1.87591E-07,-8.84068E-07,-1.75433E-06,-2.17670E-06/
      DATA C21601 /
     X-1.37112E-06,-2.31722E-06,-2.23860E-06,-1.16796E-06,-2.23765E-06,
     X-1.86406E-06,-1.03517E-06,-5.90824E-07,-6.57710E-07,-7.00941E-07,
     X-4.46064E-07, 1.77205E-06, 2.45066E-06, 2.39371E-06, 2.30736E-06,
     X 2.35355E-06, 1.85070E-06, 9.62711E-07, 2.59644E-06, 2.05304E-06,
     X 9.70090E-07, 1.50942E-06, 3.79439E-06, 2.94597E-06,-1.91789E-06,
     X 6.44324E-08,-3.92094E-07,-1.55398E-06, 4.46701E-08,-4.78760E-07,
     X-1.70061E-06,-3.17252E-06,-2.93173E-06,-2.01455E-06,-7.76298E-07,
     X-2.74577E-07,-1.39907E-06,-2.16470E-06,-1.26010E-06,-2.76845E-06,
     X-2.38226E-06,-5.49068E-08, 9.65258E-07, 1.08650E-06, 5.64738E-07,
     X-5.78379E-07,-5.68918E-07,-1.90177E-06,-5.08874E-06,-3.03648E-06,
     X-1.30527E-06,-4.87669E-07,-2.83326E-06,-1.97823E-06,-5.94313E-07,
     X-1.50961E-07,-1.15908E-06,-1.43260E-06,-9.29331E-07,-1.39459E-06,
     X-1.27237E-06,-1.50189E-06,-3.79292E-06,-3.92038E-06,-3.58490E-06,
     X-3.26439E-06,-2.42138E-06,-2.70516E-06,-3.58080E-06,-1.71822E-06,
     X-2.41567E-06,-3.50193E-06,-2.62394E-06,-3.08424E-06,-3.89604E-06,
     X-4.84127E-06,-4.41385E-06,-3.22673E-06,-1.80987E-06,-2.93027E-06/
      DATA C21681 /
     X-3.17366E-06,-2.79721E-06,-1.78848E-06,-2.80254E-06,-3.55572E-06,
     X-3.34632E-06,-2.83979E-06,-2.48022E-06,-2.15090E-06,-1.08311E-06,
     X-6.15216E-07,-7.13008E-07,-1.70841E-06,-2.96098E-06,-3.57134E-06,
     X-3.04405E-06,-3.35280E-06,-2.97780E-06,-1.97966E-06,-2.33197E-06,
     X-2.76708E-06,-2.70409E-06,-4.51094E-07,-1.43068E-06,-2.83719E-06,
     X-2.98921E-06,-4.14949E-06,-3.63780E-06,-8.10138E-07,-1.61597E-06,
     X-2.25394E-06,-2.58110E-06,-1.57781E-06,-1.71520E-06,-2.30016E-06,
     X-2.61268E-06,-1.96696E-06,-1.86744E-06,-3.15645E-06,-3.59354E-06,
     X-3.61015E-06,-3.21793E-06,-2.57436E-06,-2.74347E-06,-3.33319E-06,
     X-2.93400E-06,-3.25986E-06,-3.46384E-06,-2.22114E-06,-2.92650E-06,
     X-3.73666E-06,-3.70485E-06,-2.75963E-06,-2.40652E-06,-2.93107E-06,
     X-1.77517E-06,-1.57096E-06,-2.17533E-06,-2.80190E-06,-2.27942E-06,
     X-1.37371E-06,-1.65974E-06,-1.26079E-06,-8.08050E-07,-8.41278E-07,
     X-1.53860E-06,-1.66687E-06,-6.56592E-07,-3.05110E-08, 1.08623E-07,
     X-2.87222E-07,-2.63555E-07,-7.89575E-07,-1.56059E-06,-6.42174E-07,
     X-9.43333E-07,-1.38671E-06, 6.50443E-07, 1.35301E-06, 9.27981E-07/
      DATA C21761 /
     X-1.21705E-06,-9.63848E-08, 8.73593E-07,-3.47278E-08,-1.79042E-06,
     X-2.15544E-06,-4.48668E-07,-1.17414E-06,-1.35437E-06,-8.90688E-07,
     X-4.54757E-07, 2.41484E-09, 3.88010E-07,-1.85349E-08, 1.58011E-07,
     X 3.70566E-07,-7.30268E-07,-8.42354E-07,-4.13738E-07, 3.96796E-07,
     X-5.55763E-07,-1.26877E-06,-2.89854E-07, 5.78676E-07, 9.51356E-07,
     X 5.56912E-07, 1.05014E-06, 9.75896E-07, 5.91573E-08,-6.15073E-07,
     X-1.48803E-06,-2.53397E-06,-1.77027E-06,-2.08546E-06,-3.10452E-06,
     X-1.65227E-06,-1.15981E-06,-1.25849E-06,-9.65711E-07,-1.90319E-06,
     X-2.71831E-06,-5.71559E-08,-1.20368E-06,-3.16820E-06,-2.22766E-06,
     X-1.19828E-06,-2.82573E-07, 2.53850E-07,-9.10547E-07,-1.65529E-06,
     X-6.00138E-07,-4.98898E-07,-3.45799E-07, 2.25160E-07, 1.14332E-07,
     X 3.16082E-07, 1.12681E-06,-6.04876E-07,-7.24616E-07, 1.48177E-06,
     X 1.05680E-06, 5.91076E-07, 2.07187E-07, 3.82385E-07, 5.91560E-07,
     X 8.26519E-07, 1.22139E-06, 1.63501E-06, 2.06423E-06, 2.50038E-06,
     X 2.38037E-06, 1.91688E-06, 2.46702E-06, 2.45066E-06, 2.16732E-06,
     X 3.13517E-06, 2.68221E-06, 1.39877E-06, 8.58945E-07, 6.83181E-07/
      DATA C21841 /
     X 8.46816E-07, 1.73491E-06, 1.98732E-06, 1.94059E-06, 2.19284E-06,
     X 1.73215E-06, 1.06865E-06, 1.14117E-06, 1.43213E-06, 1.42275E-06,
     X-4.15449E-07,-2.39911E-07, 3.46498E-08,-2.75022E-06,-2.43736E-06,
     X-1.06489E-06,-7.81941E-07,-8.04801E-07,-1.04984E-06,-1.65734E-06,
     X-1.03167E-06,-3.18255E-08, 5.70283E-07, 6.19050E-07, 2.92257E-07,
     X-6.01436E-07,-7.04005E-07,-3.70875E-07, 4.12830E-07, 1.31319E-07,
     X-1.61570E-07, 9.76170E-07, 7.99907E-07, 1.41860E-07,-1.98022E-07,
     X 3.13766E-07, 7.43982E-07,-6.11287E-07,-5.21146E-07, 1.11156E-07,
     X 3.91719E-07, 5.45566E-07, 6.39059E-07, 7.29515E-07, 4.59167E-07,
     X 6.13179E-08,-3.48146E-08, 5.32046E-07, 1.19736E-06, 3.83982E-07,
     X 1.73267E-07, 3.54304E-07, 9.34657E-07, 5.53819E-07,-2.86678E-07,
     X 2.01853E-08,-1.56159E-07,-6.08130E-07,-2.14929E-07, 1.66317E-08,
     X 9.32462E-08,-4.83623E-07,-9.16323E-07,-1.22772E-06,-1.61586E-06,
     X-1.27409E-06,-1.98119E-07,-3.69182E-08,-1.41061E-07,-5.12562E-07,
     X-4.55495E-07,-8.12132E-07,-1.71772E-06,-2.70741E-06,-2.98751E-06,
     X-2.19520E-06, 3.01900E-07, 1.17806E-06,-1.23067E-06, 4.17086E-07/
      DATA C21921 /
     X 1.68113E-06, 4.81677E-07,-1.55187E-07,-3.35287E-07, 2.94916E-07,
     X 4.57124E-07, 3.38692E-07,-2.49203E-07,-3.62585E-07,-2.39653E-07,
     X 3.72675E-08,-7.79964E-09,-2.83285E-07,-9.74713E-07,-6.91171E-07,
     X 1.21925E-07, 3.39940E-07, 3.68441E-08,-5.82188E-07, 2.12605E-07,
     X 4.65144E-07, 2.17190E-07, 7.50119E-07, 8.62008E-07, 4.63016E-07,
     X 1.25620E-06, 1.04567E-06,-8.17037E-07,-1.20023E-06,-1.06224E-06,
     X-3.77100E-07,-1.28057E-07,-2.76183E-07,-1.24304E-06,-2.56776E-06,
     X-3.36699E-06,-1.49408E-06,-1.01189E-07, 7.41870E-07,-6.45425E-07,
     X-7.47111E-07, 4.79055E-10,-1.32339E-06,-1.86135E-06,-1.61074E-06,
     X-1.82039E-06,-1.68040E-06,-1.08025E-06,-8.61965E-07,-7.00131E-07,
     X-5.63105E-07,-8.09843E-07,-8.09221E-07, 1.69474E-07,-1.33941E-07,
     X-7.49558E-07,-5.19013E-07,-8.53534E-07,-1.33703E-06,-3.11161E-07,
     X 8.99037E-07, 2.25330E-06, 1.44822E-06, 3.07437E-07,-1.22366E-06,
     X-7.64217E-07, 2.13156E-08, 1.07909E-06, 6.10755E-07, 1.81483E-07,
     X 8.12405E-07,-9.13283E-08,-1.35885E-06,-1.58366E-06,-7.88594E-07,
     X 4.48283E-07,-1.23754E-06,-1.65105E-06,-8.93014E-07,-1.48622E-06/
      DATA C22001 /
     X-1.67948E-06,-1.24310E-06,-1.54411E-06,-1.65677E-06,-1.04998E-06,
     X-1.46985E-07, 4.61778E-07,-4.87832E-07,-4.89452E-07,-1.24840E-07,
     X-1.70101E-06,-1.66976E-06,-1.48528E-07,-1.12621E-07,-2.30607E-08,
     X 1.82301E-07,-8.58152E-07,-1.89794E-06,-2.46464E-06,-2.32745E-06,
     X-2.02112E-06,-2.07656E-06,-1.43824E-06,-5.16583E-07,-1.80702E-06,
     X-2.93490E-06,-3.89216E-06,-3.36211E-06,-2.41393E-06,-9.53406E-07,
     X-1.16269E-06,-1.66431E-06,-1.77150E-06,-1.82496E-06,-1.93095E-06,
     X-2.75759E-06,-2.83618E-06,-2.27908E-06,-6.33348E-07, 5.61257E-07,
     X 1.00142E-06, 7.73337E-07, 3.17721E-07,-3.69804E-07,-8.82058E-07,
     X-1.17364E-06,-4.53480E-07,-2.47824E-07,-4.79624E-07,-5.17032E-07,
     X-3.46498E-07, 1.42669E-07,-1.59168E-07,-5.06580E-07,-3.18573E-07,
     X-2.74092E-07,-2.68860E-07, 1.32811E-07,-2.35567E-09,-6.71971E-07,
     X-9.75302E-07,-8.70978E-07,-3.59071E-08,-3.01726E-07,-8.27641E-07,
     X-1.14899E-06,-1.50160E-06,-1.83660E-06,-1.26290E-06,-1.07659E-06,
     X-1.34878E-06,-5.24626E-07,-7.85094E-08,-8.79473E-07,-1.19291E-06,
     X-1.33298E-06,-1.59750E-06,-1.31836E-06,-5.73079E-07,-1.10349E-06/
      DATA C22081 /
     X-1.11807E-06,-1.99530E-07,-8.10496E-07,-1.42679E-06,-5.34617E-07,
     X-2.05001E-07,-2.51690E-07,-1.01740E-06,-1.02841E-06,-7.48750E-08,
     X-1.01770E-06,-1.50413E-06, 1.80898E-07, 3.63788E-07,-1.97900E-07,
     X-1.16721E-06,-1.05497E-06,-2.07218E-08,-1.90590E-07,-8.25501E-07,
     X-2.21142E-06,-1.19905E-06, 2.16271E-07,-2.52574E-07,-4.35837E-07,
     X-3.95272E-07, 5.97065E-08, 2.76639E-07, 9.22569E-08, 1.20142E-07,
     X-2.95030E-09,-1.08216E-06,-1.32386E-06,-9.62248E-07,-1.99430E-06,
     X-2.13890E-06,-9.56082E-07,-6.94022E-07,-7.75721E-07,-1.31048E-06,
     X-1.50080E-06,-1.35873E-06,-7.48378E-07,-4.83436E-07,-4.69624E-07,
     X-1.51156E-06,-2.48221E-06,-3.30134E-06,-2.79114E-06,-2.08976E-06,
     X-2.24768E-06,-1.06947E-06, 1.17462E-06,-2.51423E-07,-7.85729E-07,
     X 5.37467E-07,-9.39876E-08,-1.11303E-06,-7.46860E-07,-9.36220E-07,
     X-1.59880E-06,-1.61420E-06,-1.54368E-06,-1.41036E-06,-7.20350E-07,
     X 1.35544E-07, 3.14481E-07, 6.29265E-07, 1.09161E-06,-1.36044E-07,
     X-1.22932E-06,-1.29847E-06,-3.26429E-06,-6.01062E-06,-2.09945E-06,
     X 1.26878E-07,-2.88050E-08,-6.82802E-07,-1.39340E-06,-1.82986E-06/
      DATA C22161 /
     X-1.67208E-06,-1.07994E-06,-1.89195E-06,-2.10782E-06,-1.04519E-06,
     X-3.27672E-07, 1.95516E-07, 1.63838E-07,-2.29575E-07,-1.01609E-06,
     X-2.19286E-06,-2.71850E-06,-9.77485E-07,-1.48830E-06,-3.37826E-06,
     X-1.59130E-06,-5.74498E-07,-8.27962E-07,-9.92211E-07,-1.14422E-06,
     X-1.41420E-06,-1.11629E-06,-2.51575E-07, 1.60805E-07, 1.82934E-07,
     X-7.28868E-07,-2.57062E-07, 1.06520E-06, 4.16488E-07, 2.97049E-08,
     X 6.62797E-08, 8.29435E-07, 1.29657E-06,-2.27961E-06,-3.40386E-06,
     X-1.88594E-06,-2.29732E-06,-2.72594E-06,-2.09847E-06,-1.31771E-06,
     X-4.23693E-07,-4.96348E-07,-9.40209E-07,-2.08707E-06,-1.21368E-06,
     X 4.79409E-07,-1.12548E-08,-4.57316E-07,-8.40885E-07,-5.03210E-07,
     X-1.61036E-07,-1.05835E-06,-1.66417E-06,-1.97827E-06,-1.63737E-06,
     X-1.11711E-06,-3.16081E-07,-6.81746E-07,-1.82599E-06,-1.12895E-06,
     X-9.19712E-07,-1.91707E-06,-2.14767E-06,-2.03629E-06,-2.86441E-06,
     X-3.07735E-06,-2.28656E-06,-1.40256E-06,-5.50649E-07,-3.11627E-07,
     X-7.90261E-07,-2.10728E-06,-1.89739E-06,-1.53762E-06,-2.39947E-06,
     X-2.28765E-06,-1.27564E-06,-2.15154E-06,-3.17932E-06,-3.84234E-06/
      DATA C22241 /
     X-3.65102E-06,-2.84055E-06,-2.48744E-06,-2.27683E-06,-2.33087E-06,
     X-3.44460E-06,-5.19613E-06,-2.85882E-06,-1.39921E-06,-2.00579E-06,
     X-2.80593E-06,-3.65940E-06,-2.39526E-06,-1.70389E-06,-2.03532E-06,
     X-2.71522E-06,-3.42227E-06,-2.23606E-06,-1.77845E-06,-2.42071E-06,
     X-2.61515E-06,-2.56413E-06,-1.49601E-06,-1.23245E-06,-2.08440E-06,
     X-2.11121E-06,-1.93424E-06,-2.27439E-06,-2.58183E-06,-2.84705E-06,
     X-2.32183E-06,-1.80966E-06,-3.04089E-06,-3.14334E-06,-1.91331E-06,
     X-1.51037E-06,-1.43610E-06,-2.11316E-06,-2.45184E-06,-2.42262E-06/
      END
      SUBROUTINE O3UV(V,C)
      COMMON /O3UVF/ V1 ,V2 ,DV ,NPT ,S(133)
C
C     INTERPOLATION  FOR  O3 CONTINUUM WITH LOWTRAN
C
      C    =0.
      I=(V  -V1)/DV+1.00001
      IF(I.LT.1   )GO TO 10
      IF(I.GT.NPT )GO TO 10
      VR = I*DV + V1
      IF(VR. LE. (V+.1) .AND .VR.GE. (V-.1)) GO TO 5
      IF(I .EQ. NPT ) I=NPT-1
      AM = (S(I+1) -S(I))/DV
      C0 = S(I) - AM * VR
      C  = AM * V + C0
      GO TO 10
5     C    =    S(I)
10    CONTINUE
C
      RETURN
      END
      BLOCK DATA O3UVFD
C>    BLOCK DATA
      COMMON /O3UVF / V1O1,V2O1,DVO1,NPT1,C02281(80),C02361(53)
C
C        OZONE UV  VISIBLE ABSORPTION COEFFICIENTS
C                     (CM-ATM)-1
C     DATA DERIVED FROM MOLINA & MOLINA, JGR,91,14501-14508,1986.
C     VALUES BETWEEN 245 AND 185NM (40800 AND 54054CM-1) USED AS
C     DIRECT AVERAGE WITH NO TEMPERATURE DEPENDENCE.
C
C     O3 LOCATION  1    V =  40800  CM-1
C     O3 LOCATION  133  V =  54054  CM-1
C        DV = 100  CM-1
C
      DATA V1O1,V2O1,DVO1,NPT1/ 40800.,54000.,100.,133/
      DATA C02281/
     C 9.91204E+02, 9.76325E+02, 9.72050E+02, 9.51049E+02, 9.23530E+02,
     C 9.02306E+02, 8.90510E+02, 8.60115E+02, 8.39094E+02, 8.27926E+02,
     C 7.95525E+02, 7.73583E+02, 7.55018E+02, 7.31076E+02, 7.10415E+02,
     C 6.87747E+02, 6.66639E+02, 6.39484E+02, 6.27101E+02, 6.01019E+02,
     C 5.77594E+02, 5.60403E+02, 5.40837E+02, 5.21289E+02, 4.99329E+02,
     C 4.81742E+02, 4.61608E+02, 4.45707E+02, 4.28261E+02, 4.09672E+02,
     C 3.93701E+02, 3.77835E+02, 3.61440E+02, 3.45194E+02, 3.30219E+02,
     C 3.15347E+02, 3.01164E+02, 2.87788E+02, 2.74224E+02, 2.61339E+02,
     C 2.48868E+02, 2.36872E+02, 2.25747E+02, 2.14782E+02, 2.03997E+02,
     C 1.94281E+02, 1.84525E+02, 1.75275E+02, 1.67151E+02, 1.58813E+02,
     C 1.50725E+02, 1.43019E+02, 1.35825E+02, 1.28878E+02, 1.22084E+02,
     C 1.15515E+02, 1.09465E+02, 1.03841E+02, 9.83780E+01, 9.31932E+01,
     C 8.83466E+01, 8.38631E+01, 7.96631E+01, 7.54331E+01, 7.13805E+01,
     C 6.78474E+01, 6.44340E+01, 6.13104E+01, 5.81777E+01, 5.53766E+01,
     C 5.27036E+01, 5.03555E+01, 4.82633E+01, 4.61483E+01, 4.42014E+01,
     C 4.23517E+01, 4.07774E+01, 3.93060E+01, 3.80135E+01, 3.66348E+01/
      DATA C02361/
     C 3.53665E+01, 3.47884E+01, 3.39690E+01, 3.34288E+01, 3.29135E+01,
     C 3.23104E+01, 3.18875E+01, 3.16800E+01, 3.15925E+01, 3.12932E+01,
     C 3.12956E+01, 3.15522E+01, 3.14950E+01, 3.15924E+01, 3.19059E+01,
     C 3.23109E+01, 3.27873E+01, 3.33788E+01, 3.39804E+01, 3.44925E+01,
     C 3.50502E+01, 3.55853E+01, 3.59416E+01, 3.68933E+01, 3.78284E+01,
     C 3.86413E+01, 3.98049E+01, 4.04700E+01, 4.12958E+01, 4.23482E+01,
     C 4.31203E+01, 4.41885E+01, 4.52651E+01, 4.61492E+01, 4.70493E+01,
     C 4.80497E+01, 4.90242E+01, 4.99652E+01, 5.10316E+01, 5.21510E+01,
     C 5.32130E+01, 5.43073E+01, 5.56207E+01, 5.61756E+01, 5.66799E+01,
     C 5.85545E+01, 5.92409E+01, 5.96168E+01, 6.12497E+01, 6.20231E+01,
     C 6.24621E+01, 6.34160E+01, 6.43622E+01/
      END
      SUBROUTINE O2CONT(V,SIGMA,ALPHA,BETA)
C
C     THIS ROUTINE IS DRIVEN BY FREQUENCY, RETURNING ONLY THE
C     O2 COEFFICIENTS, INDEPENDENT OF TEMPERATURE.
C
C  *******************************************************************
C  *  THESE COMMENTS APPLY TO THE COLUME ARRAYS FOR:                 *
C  *       PBAR*UBAR(O2)                                             *
C  *       PBAR*UBAR(O2)*DT                                          *
C  *   AND PBAR*UBAR(O2)*DT*DT    WHERE:  DT=TBAR-220.               *
C  *  THAT HAVE BEEN COMPILED IN OTHER PARTS OF THE LOWTRAN CODE     *
C  *                                                                 *
C  *  LOWTRAN7 COMPATIBLE:                                           *
C  *  O2 CONTINUUM SUBROUTINE FOR 1395-1760CM-1                      *
C  *  MODIFIED BY G.P. ANDERSON, APRIL '88                           *
C  *                                                                 *
C  *  THE EXPONENTIAL TEMPERATURE EMPLOYED IN THE FASCOD2 ALGORITHM  *
C  *  (SEE BELOW) IS NOT READILY SUITABLE FOR LOWTRAN.  THEREFORE    *
C  *  THE EXPONENTIALS HAVE BEEN LINEARLY EXPANDED, KEEPING ONLY THE *
C  *  LINEAR AND QUADRATIC TERMS:                                    *
C  *                                                                 *
C  *  EXP(A*DT)=1.+ A*DT + (A*DT)**2/2. + ....                       *
C  *                                                                 *
C  *     EXP(B*DT*DT)=1.+ B*DT*DT + (B*DT*DT)**2/2. + ....           *
C  *                                                                 *
C  *  THE PRODUCT OF THE TWO TERMS IS:                               *
C  *                                                                 *
C  *     (1. + A*DT + (A*A/2. + B)*DT*DT )                           *
C  *                                                                 *
C  *  THIS EXPANSION ONLY WORKS WELL FOR SMALL VALUES OF X IN EXP(X) *
C  *                                                                 *
C  *  SINCE DT = T-220., THE APPROXIMATION IS VERY GOOD UNTIL        *
C  *  T.GT.260. OR DT.GT.40.   AT T=280, THE MAXIMUM ERRORS ARE STILL*
C  *  LESS THAN 10% BUT AT T=300, THOSE ERRORS ARE AS LARGE AS 20%   *
C  *******************************************************************
C
C     THE FOLLOWING COMMENTS ARE EXCERPTED DIRECTLY FROM FASCOD2
C
C      THIS SUBROUTINE CONTAINS THE ROGERS AND WALSHAW
C      EQUIVALENT COEFFICIENTS DERIVED FROM THE THEORETICAL
C      VALUES SUPPLIED BY ROLAND DRAYSON. THESE VALUES USE
C      THE SAME DATA AS TIMOFEYEV AND AGREE WITH TIMOFEYEV'S RESULTS.
C      THE DATA ARE IN THE FORM OF STRENGTHS(O2SO) AND TWO
C      COEFFICIENTS (O2A & O2B),  WHICH ARE USED TO CORRECT FOR
C      TEMPERATURE. THE DEPENDENCY ON PRESSURE SQUARED
C      IS CONTAINED IN THE P*WO2 PART OF THE CONSTANT.
C      NOTE THAT SINCE THE COEFFICIENTS ARE FOR AIR, THE
C      THE STRENGTHS ARE DIVIDED BY THE O2 MIXING RATIO FOR
C      DRY AIR OF 0.20946 (THIS IS ASSUMED CONSTANT).
C      ORIGINAL FORMULATION OF THE COEFFICIENTS WAS BY LARRY GORDLEY.
C      THIS VERSION WRITTEN BY EARL THOMPSON, JULY 1984.
C
C
      COMMON/O2C/ O2DRAY(74),O2C001(74),O2S0(74),O2A(74),O2B(74),
     X V1O2,V2O2,DVO2,NPTO2
      SIGMA =0
      ALPHA =0
      BETA  =0
      IF(V .LT. 1395) GO TO 30
      IF(V .GT. 1760) GO TO 30
C
C
      CALL O2INT(V,V1O2,DVO2,NPTO2,C,O2S0,A,O2A,B,O2B)
C
C
C
C     OLD 'FASCOD2' TEMPERATURE DEPENDENCE USING BLOCK DATA ARRAYS
C
C     C(J)=O2S0(I)* EXP(O2A(I)*TD+O2B(I)*TD*TD) /(0.20946*VJ)
C
C     NEW COEFFICIENT DEFINITIONS FOR LOWTRAN FORMULATION
C
      ALPHA= A
      BETA=A**2/2.+B
      SIGMA=C/0.20946
C
C     NEW 'LOWTRAN7' TEMPERATURE DEPENDENCE
C
C     THIS WOULD BE THE CODING FOR THE LOWTRAN7 FORMULATION, BUT
C       BECAUSE THE T-DEPENDENCE IS INCLUDED IN THE AMOUNTS, ONLY
C       THE COEFFICIENTS (SIGMA, ALPHA & BETA) ARE BEING RETURNED
C
C     C(J)=SIGMA*(1.+ALPHA*TD+BETA*TD*TD)
C
30    RETURN
      END
      SUBROUTINE O2INT(V1C,V1,DV,NPT,C,CARRAY,A,AARRAY,B,BARRAY)
C
C     INTERPOLATION FOR O2 PRESSURE INDUCED CONTINUUM, NECESSARY FOR
C          LOWTRAN7 FORMULATION  (MODELED AFTER THE LOWTRAN UV-O3 BANDS)
C
      DIMENSION CARRAY(74),AARRAY(74),BARRAY(74)
      C=0.
      A=0.
      B=0.
      I=(V1C-V1)/DV+1.00001
      IF(I.LT.1  )GO TO 10
      IF(I.GT.NPT)GO TO 10
      C=CARRAY(I)
      A=AARRAY(I)
      B=BARRAY(I)
10    CONTINUE
      RETURN
      END
      BLOCK DATA BO2C
C>    BLOCK DATA
C
C     BLOCK DATA   (IDENTICAL TO BLOCK DATA IN FASCOD2)
C
      COMMON/O2C/ O2DRAY(74),O2C001(74),O2S0(74),O2A(74),O2B(74),
     X V1O2,V2O2,DVO2,NPTO2
      DATA V1O2,V2O2,DVO2,NPTO2 /1395.0,1760.0,5.0,74/
      DATA O2S0/
     A0.       ,
     +  .110E-8, .220E-8, .440E-8, .881E-8, .176E-7, .353E-7, .705E-7,
     B .141E-06, .158E-06, .174E-06, .190E-06, .207E-06, .253E-06,
     B .307E-06, .357E-06, .401E-06, .445E-06, .508E-06, .570E-06,
     B .599E-06, .627E-06, .650E-06, .672E-06, .763E-06, .873E-06,
     B .101E-05, .109E-05, .121E-05, .133E-05, .139E-05, .145E-05,
     B .148E-05, .140E-05, .134E-05, .126E-05, .118E-05, .114E-05,
     B .109E-05, .105E-05, .105E-05, .105E-05, .104E-05, .103E-05,
     B .992E-06, .945E-06, .876E-06, .806E-06, .766E-06, .726E-06,
     B .640E-06, .555E-06, .469E-06, .416E-06, .364E-06, .311E-06,
     B .266E-06, .222E-06, .177E-06, .170E-06, .162E-06, .155E-06,
     B .143E-06, .130E-06, .118E-06, .905E-07, .629E-07,
     + .316E-7, .157E-7, .786E-8, .393E-8, .196E-8, .982E-9,
     + 0./
      DATA O2A /
     A 0.       ,
     +   .147E-3, .147E-3, .147E-3,  .147E-3, .147E-3, .147E-3, .147E-3,
     B  .147E-03,  .122E-02,  .204E-02,  .217E-02,  .226E-02,  .126E-02,
     B  .362E-03, -.198E-02, -.545E-02, -.786E-02, -.624E-02, -.475E-02,
     B -.506E-02, -.533E-02, -.586E-02, -.635E-02, -.644E-02, -.679E-02,
     B -.741E-02, -.769E-02, -.780E-02, -.788E-02, -.844E-02, -.894E-02,
     B -.899E-02, -.922E-02, -.892E-02, -.857E-02, -.839E-02, -.854E-02,
     B -.871E-02, -.889E-02, -.856E-02, -.823E-02, -.796E-02, -.768E-02,
     B -.715E-02, -.638E-02, -.570E-02, -.491E-02, -.468E-02, -.443E-02,
     B -.333E-02, -.184E-02,  .313E-03, -.164E-04, -.417E-03, -.916E-03,
     B -.206E-02, -.343E-02, -.515E-02, -.365E-02, -.172E-02,  .926E-03,
     B  .168E-02,  .262E-02,  .380E-02,  .551E-02,  .889E-02,
     + .889E-2,  .889E-2, .889E-2, .889E-2, .889E-2, .889E-2,
     +  0./
      DATA O2B  /
     A 0.       ,
     + .306E-4,-.306E-4,-.306E-4,-.306E-4,-.306E-4,-.306E-4,-.306E-4,
     B -.306E-04, -.218E-04, -.159E-04, -.346E-05,  .642E-05,  .360E-05,
     B -.140E-05,  .157E-04,  .471E-04,  .656E-04,  .303E-04, -.192E-05,
     B  .705E-05,  .149E-04,  .200E-04,  .245E-04,  .158E-04,  .841E-05,
     B  .201E-05,  .555E-05,  .108E-04,  .150E-04,  .193E-04,  .230E-04,
     B  .243E-04,  .226E-04,  .184E-04,  .157E-04,  .169E-04,  .197E-04,
     B  .226E-04,  .258E-04,  .235E-04,  .212E-04,  .185E-04,  .156E-04,
     B  .125E-04,  .872E-05,  .760E-05,  .577E-05,  .334E-07, -.652E-05,
     B -.977E-05, -.157E-04, -.273E-04, -.180E-04, -.641E-05,  .817E-05,
     B  .326E-04,  .626E-04,  .101E-03,  .755E-04,  .430E-04, -.113E-05,
     B -.578E-05, -.120E-04, -.208E-04, -.235E-04, -.364E-04,
     + .364E-4, -.364E-4,-.364E-4,-.364E-4,-.364E-4,-.364E-4,
     + 0./
C
      END
      BLOCK DATA SF296
C>    BLOCK DATA
C               06/28/82
C               UNITS OF (CM**3/MOL) * 1.E-20
      COMMON /SH2O/ V1,V2,DV,NPT,S0000(2),
     1      S0001(50),S0051(50),S0101(50),S0151(50),S0201(50),S0251(50),
     2      S0301(50),S0351(50),S0401(50),S0451(50),S0501(50),S0551(50),
     3      S0601(50),S0651(50),S0701(50),S0751(50),S0801(50),S0851(50),
     4      S0901(50),S0951(50),S1001(50),S1051(50),S1101(50),S1151(50),
     5      S1201(50),S1251(50),S1301(50),S1351(50),S1401(50),S1451(50),
     6      S1501(50),S1551(50),S1601(50),S1651(50),S1701(50),S1751(50),
     7      S1801(50),S1851(50),S1901(50),S1951(50),S2001(1)
C
C
       DATA V1,V2,DV,NPT /
     1      -20.0,     20000.0,       10.0,  2003/
C
C
      DATA S0000/ 1.1109E-01 ,1.0573E-01/
      DATA S0001/
     C 1.0162E-01, 1.0573E-01, 1.1109E-01, 1.2574E-01, 1.3499E-01,
     C 1.4327E-01, 1.5065E-01, 1.5164E-01, 1.5022E-01, 1.3677E-01,
     C 1.3115E-01, 1.2253E-01, 1.1271E-01, 1.0070E-01, 8.7495E-02,
     C 8.0118E-02, 6.9940E-02, 6.2034E-02, 5.6051E-02, 4.7663E-02,
     C 4.2450E-02, 3.6690E-02, 3.3441E-02, 3.0711E-02, 2.5205E-02,
     C 2.2113E-02, 1.8880E-02, 1.6653E-02, 1.4626E-02, 1.2065E-02,
     C 1.0709E-02, 9.1783E-03, 7.7274E-03, 6.7302E-03, 5.6164E-03,
     C 4.9089E-03, 4.1497E-03, 3.5823E-03, 3.1124E-03, 2.6414E-03,
     C 2.3167E-03, 2.0156E-03, 1.7829E-03, 1.5666E-03, 1.3928E-03,
     C 1.2338E-03, 1.0932E-03, 9.7939E-04, 8.8241E-04, 7.9173E-04/
      DATA S0051/
     C 7.1296E-04, 6.4179E-04, 5.8031E-04, 5.2647E-04, 4.7762E-04,
     C 4.3349E-04, 3.9355E-04, 3.5887E-04, 3.2723E-04, 2.9919E-04,
     C 2.7363E-04, 2.5013E-04, 2.2876E-04, 2.0924E-04, 1.9193E-04,
     C 1.7618E-04, 1.6188E-04, 1.4891E-04, 1.3717E-04, 1.2647E-04,
     C 1.1671E-04, 1.0786E-04, 9.9785E-05, 9.2350E-05, 8.5539E-05,
     C 7.9377E-05, 7.3781E-05, 6.8677E-05, 6.3993E-05, 5.9705E-05,
     C 5.5788E-05, 5.2196E-05, 4.8899E-05, 4.5865E-05, 4.3079E-05,
     C 4.0526E-05, 3.8182E-05, 3.6025E-05, 3.4038E-05, 3.2203E-05,
     C 3.0511E-05, 2.8949E-05, 2.7505E-05, 2.6170E-05, 2.4933E-05,
     C 2.3786E-05, 2.2722E-05, 2.1736E-05, 2.0819E-05, 1.9968E-05/
      DATA S0101/
     C 1.9178E-05, 1.8442E-05, 1.7760E-05, 1.7127E-05, 1.6541E-05,
     C 1.5997E-05, 1.5495E-05, 1.5034E-05, 1.4614E-05, 1.4230E-05,
     C 1.3883E-05, 1.3578E-05, 1.3304E-05, 1.3069E-05, 1.2876E-05,
     C 1.2732E-05, 1.2626E-05, 1.2556E-05, 1.2544E-05, 1.2604E-05,
     C 1.2719E-05, 1.2883E-05, 1.3164E-05, 1.3581E-05, 1.4187E-05,
     C 1.4866E-05, 1.5669E-05, 1.6717E-05, 1.8148E-05, 2.0268E-05,
     C 2.2456E-05, 2.5582E-05, 2.9183E-05, 3.3612E-05, 3.9996E-05,
     C 4.6829E-05, 5.5055E-05, 6.5897E-05, 7.5360E-05, 8.7213E-05,
     C 1.0046E-04, 1.1496E-04, 1.2943E-04, 1.5049E-04, 1.6973E-04,
     C 1.8711E-04, 2.0286E-04, 2.2823E-04, 2.6780E-04, 2.8766E-04/
      DATA S0151/
     C 3.1164E-04, 3.3640E-04, 3.6884E-04, 3.9159E-04, 3.8712E-04,
     C 3.7433E-04, 3.4503E-04, 3.1003E-04, 2.8027E-04, 2.5253E-04,
     C 2.3408E-04, 2.2836E-04, 2.4442E-04, 2.7521E-04, 2.9048E-04,
     C 3.0489E-04, 3.2646E-04, 3.3880E-04, 3.3492E-04, 3.0987E-04,
     C 2.9482E-04, 2.8711E-04, 2.6068E-04, 2.2683E-04, 1.9996E-04,
     C 1.7788E-04, 1.6101E-04, 1.3911E-04, 1.2013E-04, 1.0544E-04,
     C 9.4224E-05, 8.1256E-05, 7.3667E-05, 6.2233E-05, 5.5906E-05,
     C 5.1619E-05, 4.5140E-05, 4.0273E-05, 3.3268E-05, 3.0258E-05,
     C 2.6440E-05, 2.3103E-05, 2.0749E-05, 1.8258E-05, 1.6459E-05,
     C 1.4097E-05, 1.2052E-05, 1.0759E-05, 9.1400E-06, 8.1432E-06/
      DATA S0201/
     C 7.1460E-06, 6.4006E-06, 5.6995E-06, 4.9372E-06, 4.4455E-06,
     C 3.9033E-06, 3.4740E-06, 3.1269E-06, 2.8059E-06, 2.5558E-06,
     C 2.2919E-06, 2.0846E-06, 1.8983E-06, 1.7329E-06, 1.5929E-06,
     C 1.4631E-06, 1.3513E-06, 1.2461E-06, 1.1519E-06, 1.0682E-06,
     C 9.9256E-07, 9.2505E-07, 8.6367E-07, 8.0857E-07, 7.5674E-07,
     C 7.0934E-07, 6.6580E-07, 6.2580E-07, 5.8853E-07, 5.5333E-07,
     C 5.2143E-07, 4.9169E-07, 4.6431E-07, 4.3898E-07, 4.1564E-07,
     C 3.9405E-07, 3.7403E-07, 3.5544E-07, 3.3819E-07, 3.2212E-07,
     C 3.0714E-07, 2.9313E-07, 2.8003E-07, 2.6777E-07, 2.5628E-07,
     C 2.4551E-07, 2.3540E-07, 2.2591E-07, 2.1701E-07, 2.0866E-07/
      DATA S0251/
     C 2.0082E-07, 1.9349E-07, 1.8665E-07, 1.8027E-07, 1.7439E-07,
     C 1.6894E-07, 1.6400E-07, 1.5953E-07, 1.5557E-07, 1.5195E-07,
     C 1.4888E-07, 1.4603E-07, 1.4337E-07, 1.4093E-07, 1.3828E-07,
     C 1.3569E-07, 1.3270E-07, 1.2984E-07, 1.2714E-07, 1.2541E-07,
     C 1.2399E-07, 1.2102E-07, 1.1878E-07, 1.1728E-07, 1.1644E-07,
     C 1.1491E-07, 1.1305E-07, 1.1235E-07, 1.1228E-07, 1.1224E-07,
     C 1.1191E-07, 1.1151E-07, 1.1098E-07, 1.1068E-07, 1.1109E-07,
     C 1.1213E-07, 1.1431E-07, 1.1826E-07, 1.2322E-07, 1.3025E-07,
     C 1.4066E-07, 1.5657E-07, 1.7214E-07, 1.9449E-07, 2.2662E-07,
     C 2.6953E-07, 3.1723E-07, 3.7028E-07, 4.4482E-07, 5.3852E-07/
      DATA S0301/
     C 6.2639E-07, 7.2175E-07, 7.7626E-07, 8.7248E-07, 9.6759E-07,
     C 1.0102E-06, 1.0620E-06, 1.1201E-06, 1.2107E-06, 1.2998E-06,
     C 1.3130E-06, 1.2856E-06, 1.2350E-06, 1.1489E-06, 1.0819E-06,
     C 1.0120E-06, 9.4795E-07, 9.2858E-07, 9.8060E-07, 1.0999E-06,
     C 1.1967E-06, 1.2672E-06, 1.3418E-06, 1.3864E-06, 1.4330E-06,
     C 1.4592E-06, 1.4598E-06, 1.4774E-06, 1.4726E-06, 1.4820E-06,
     C 1.5050E-06, 1.4984E-06, 1.5181E-06, 1.5888E-06, 1.6850E-06,
     C 1.7690E-06, 1.9277E-06, 2.1107E-06, 2.3068E-06, 2.5347E-06,
     C 2.8069E-06, 3.1345E-06, 3.5822E-06, 3.9051E-06, 4.3422E-06,
     C 4.8704E-06, 5.5351E-06, 6.3454E-06, 7.2690E-06, 8.2974E-06/
      DATA S0351/
     C 9.7609E-06, 1.1237E-05, 1.3187E-05, 1.5548E-05, 1.8784E-05,
     C 2.1694E-05, 2.5487E-05, 3.0092E-05, 3.5385E-05, 4.2764E-05,
     C 4.9313E-05, 5.5800E-05, 6.2968E-05, 7.1060E-05, 7.7699E-05,
     C 8.7216E-05, 8.9335E-05, 9.2151E-05, 9.2779E-05, 9.4643E-05,
     C 9.7978E-05, 1.0008E-04, 1.0702E-04, 1.1026E-04, 1.0828E-04,
     C 1.0550E-04, 1.0432E-04, 1.0428E-04, 9.8980E-05, 9.4992E-05,
     C 9.5159E-05, 1.0058E-04, 1.0738E-04, 1.1550E-04, 1.1229E-04,
     C 1.0596E-04, 1.0062E-04, 9.1742E-05, 8.4492E-05, 6.8099E-05,
     C 5.6295E-05, 4.6502E-05, 3.8071E-05, 3.0721E-05, 2.3297E-05,
     C 1.8688E-05, 1.4830E-05, 1.2049E-05, 9.6754E-06, 7.9192E-06/
      DATA S0401/
     C 6.6673E-06, 5.6468E-06, 4.8904E-06, 4.2289E-06, 3.6880E-06,
     C 3.2396E-06, 2.8525E-06, 2.5363E-06, 2.2431E-06, 1.9949E-06,
     C 1.7931E-06, 1.6164E-06, 1.4431E-06, 1.2997E-06, 1.1559E-06,
     C 1.0404E-06, 9.4300E-07, 8.4597E-07, 7.6133E-07, 6.8623E-07,
     C 6.2137E-07, 5.6345E-07, 5.1076E-07, 4.6246E-07, 4.1906E-07,
     C 3.8063E-07, 3.4610E-07, 3.1554E-07, 2.8795E-07, 2.6252E-07,
     C 2.3967E-07, 2.1901E-07, 2.0052E-07, 1.8384E-07, 1.6847E-07,
     C 1.5459E-07, 1.4204E-07, 1.3068E-07, 1.2036E-07, 1.1095E-07,
     C 1.0237E-07, 9.4592E-08, 8.7530E-08, 8.1121E-08, 7.5282E-08,
     C 6.9985E-08, 6.5189E-08, 6.0874E-08, 5.6989E-08, 5.3530E-08/
      DATA S0451/
     C 5.0418E-08, 4.7745E-08, 4.5367E-08, 4.3253E-08, 4.1309E-08,
     C 3.9695E-08, 3.8094E-08, 3.6482E-08, 3.4897E-08, 3.3500E-08,
     C 3.2302E-08, 3.0854E-08, 2.9698E-08, 2.8567E-08, 2.7600E-08,
     C 2.6746E-08, 2.5982E-08, 2.5510E-08, 2.5121E-08, 2.4922E-08,
     C 2.4909E-08, 2.5013E-08, 2.5216E-08, 2.5589E-08, 2.6049E-08,
     C 2.6451E-08, 2.6978E-08, 2.7687E-08, 2.8600E-08, 2.9643E-08,
     C 3.0701E-08, 3.2058E-08, 3.3695E-08, 3.5558E-08, 3.7634E-08,
     C 3.9875E-08, 4.2458E-08, 4.5480E-08, 4.8858E-08, 5.2599E-08,
     C 5.7030E-08, 6.2067E-08, 6.7911E-08, 7.4579E-08, 8.1902E-08,
     C 8.9978E-08, 9.9870E-08, 1.1102E-07, 1.2343E-07, 1.3732E-07/
      DATA S0501/
     C 1.5394E-07, 1.7318E-07, 1.9383E-07, 2.1819E-07, 2.4666E-07,
     C 2.8109E-07, 3.2236E-07, 3.7760E-07, 4.4417E-07, 5.2422E-07,
     C 6.1941E-07, 7.4897E-07, 9.2041E-07, 1.1574E-06, 1.4126E-06,
     C 1.7197E-06, 2.1399E-06, 2.6266E-06, 3.3424E-06, 3.8418E-06,
     C 4.5140E-06, 5.0653E-06, 5.8485E-06, 6.5856E-06, 6.8937E-06,
     C 6.9121E-06, 6.9005E-06, 6.9861E-06, 6.8200E-06, 6.6089E-06,
     C 6.5809E-06, 7.3496E-06, 8.0311E-06, 8.3186E-06, 8.4260E-06,
     C 9.0644E-06, 9.4965E-06, 9.4909E-06, 9.0160E-06, 9.1494E-06,
     C 9.3629E-06, 9.5944E-06, 9.5459E-06, 8.9919E-06, 8.6040E-06,
     C 7.8613E-06, 7.1567E-06, 6.2677E-06, 5.1899E-06, 4.4188E-06/
      DATA S0551/
     C 3.7167E-06, 3.0636E-06, 2.5573E-06, 2.0317E-06, 1.6371E-06,
     C 1.3257E-06, 1.0928E-06, 8.9986E-07, 7.4653E-07, 6.1111E-07,
     C 5.1395E-07, 4.3500E-07, 3.7584E-07, 3.2633E-07, 2.8413E-07,
     C 2.4723E-07, 2.1709E-07, 1.9294E-07, 1.7258E-07, 1.5492E-07,
     C 1.3820E-07, 1.2389E-07, 1.1189E-07, 1.0046E-07, 9.0832E-08,
     C 8.2764E-08, 7.4191E-08, 6.7085E-08, 6.0708E-08, 5.4963E-08,
     C 4.9851E-08, 4.5044E-08, 4.0916E-08, 3.7220E-08, 3.3678E-08,
     C 3.0663E-08, 2.7979E-08, 2.5495E-08, 2.3286E-08, 2.1233E-08,
     C 1.9409E-08, 1.7770E-08, 1.6260E-08, 1.4885E-08, 1.3674E-08,
     C 1.2543E-08, 1.1551E-08, 1.0655E-08, 9.8585E-09, 9.1398E-09/
      DATA S0601/
     C 8.4806E-09, 7.8899E-09, 7.3547E-09, 6.8670E-09, 6.4131E-09,
     C 5.9930E-09, 5.6096E-09, 5.2592E-09, 4.9352E-09, 4.6354E-09,
     C 4.3722E-09, 4.1250E-09, 3.9081E-09, 3.7118E-09, 3.5372E-09,
     C 3.3862E-09, 3.2499E-09, 3.1324E-09, 3.0313E-09, 2.9438E-09,
     C 2.8686E-09, 2.8050E-09, 2.7545E-09, 2.7149E-09, 2.6907E-09,
     C 2.6724E-09, 2.6649E-09, 2.6642E-09, 2.6725E-09, 2.6871E-09,
     C 2.7056E-09, 2.7357E-09, 2.7781E-09, 2.8358E-09, 2.9067E-09,
     C 2.9952E-09, 3.1020E-09, 3.2253E-09, 3.3647E-09, 3.5232E-09,
     C 3.7037E-09, 3.9076E-09, 4.1385E-09, 4.3927E-09, 4.6861E-09,
     C 5.0238E-09, 5.4027E-09, 5.8303E-09, 6.3208E-09, 6.8878E-09/
      DATA S0651/
     C 7.5419E-09, 8.3130E-09, 9.1952E-09, 1.0228E-08, 1.1386E-08,
     C 1.2792E-08, 1.4521E-08, 1.6437E-08, 1.8674E-08, 2.1160E-08,
     C 2.4506E-08, 2.8113E-08, 3.2636E-08, 3.7355E-08, 4.2234E-08,
     C 4.9282E-08, 5.7358E-08, 6.6743E-08, 7.8821E-08, 9.4264E-08,
     C 1.1542E-07, 1.3684E-07, 1.6337E-07, 2.0056E-07, 2.3252E-07,
     C 2.6127E-07, 2.9211E-07, 3.3804E-07, 3.7397E-07, 3.8205E-07,
     C 3.8810E-07, 3.9499E-07, 3.9508E-07, 3.7652E-07, 3.5859E-07,
     C 3.6198E-07, 3.7871E-07, 4.0925E-07, 4.2717E-07, 4.8241E-07,
     C 5.2008E-07, 5.6530E-07, 5.9531E-07, 6.1994E-07, 6.5080E-07,
     C 6.6355E-07, 6.9193E-07, 6.9930E-07, 7.3058E-07, 7.4678E-07/
      DATA S0701/
     C 7.9193E-07, 8.3627E-07, 9.1267E-07, 1.0021E-06, 1.1218E-06,
     C 1.2899E-06, 1.4447E-06, 1.7268E-06, 2.0025E-06, 2.3139E-06,
     C 2.5599E-06, 2.8920E-06, 3.3059E-06, 3.5425E-06, 3.9522E-06,
     C 4.0551E-06, 4.2818E-06, 4.2892E-06, 4.4210E-06, 4.5614E-06,
     C 4.6739E-06, 4.9482E-06, 5.1118E-06, 5.0986E-06, 4.9417E-06,
     C 4.9022E-06, 4.8449E-06, 4.8694E-06, 4.8111E-06, 4.9378E-06,
     C 5.3231E-06, 5.7362E-06, 6.2350E-06, 6.0951E-06, 5.7281E-06,
     C 5.4585E-06, 4.9032E-06, 4.3009E-06, 3.4776E-06, 2.8108E-06,
     C 2.2993E-06, 1.7999E-06, 1.3870E-06, 1.0750E-06, 8.5191E-07,
     C 6.7951E-07, 5.5336E-07, 4.6439E-07, 4.0243E-07, 3.5368E-07/
      DATA S0751/
     C 3.1427E-07, 2.7775E-07, 2.4486E-07, 2.1788E-07, 1.9249E-07,
     C 1.7162E-07, 1.5115E-07, 1.3478E-07, 1.2236E-07, 1.1139E-07,
     C 1.0092E-07, 9.0795E-08, 8.2214E-08, 7.4691E-08, 6.7486E-08,
     C 6.0414E-08, 5.4584E-08, 4.8754E-08, 4.3501E-08, 3.8767E-08,
     C 3.4363E-08, 3.0703E-08, 2.7562E-08, 2.4831E-08, 2.2241E-08,
     C 1.9939E-08, 1.8049E-08, 1.6368E-08, 1.4863E-08, 1.3460E-08,
     C 1.2212E-08, 1.1155E-08, 1.0185E-08, 9.3417E-09, 8.5671E-09,
     C 7.8292E-09, 7.1749E-09, 6.5856E-09, 6.0588E-09, 5.5835E-09,
     C 5.1350E-09, 4.7395E-09, 4.3771E-09, 4.0476E-09, 3.7560E-09,
     C 3.4861E-09, 3.2427E-09, 3.0240E-09, 2.8278E-09, 2.6531E-09/
      DATA S0801/
     C 2.4937E-09, 2.3511E-09, 2.2245E-09, 2.1133E-09, 2.0159E-09,
     C 1.9330E-09, 1.8669E-09, 1.8152E-09, 1.7852E-09, 1.7752E-09,
     C 1.7823E-09, 1.8194E-09, 1.8866E-09, 1.9759E-09, 2.0736E-09,
     C 2.2083E-09, 2.3587E-09, 2.4984E-09, 2.6333E-09, 2.8160E-09,
     C 3.0759E-09, 3.3720E-09, 3.6457E-09, 4.0668E-09, 4.4541E-09,
     C 4.7976E-09, 5.0908E-09, 5.4811E-09, 6.1394E-09, 6.3669E-09,
     C 6.5714E-09, 6.8384E-09, 7.1918E-09, 7.3741E-09, 7.2079E-09,
     C 7.2172E-09, 7.2572E-09, 7.3912E-09, 7.6188E-09, 8.3291E-09,
     C 8.7885E-09, 9.2412E-09, 1.0021E-08, 1.0752E-08, 1.1546E-08,
     C 1.1607E-08, 1.1949E-08, 1.2346E-08, 1.2516E-08, 1.2826E-08/
      DATA S0851/
     C 1.3053E-08, 1.3556E-08, 1.4221E-08, 1.5201E-08, 1.6661E-08,
     C 1.8385E-08, 2.0585E-08, 2.3674E-08, 2.7928E-08, 3.3901E-08,
     C 4.1017E-08, 4.9595E-08, 6.0432E-08, 7.6304E-08, 9.0764E-08,
     C 1.0798E-07, 1.2442E-07, 1.4404E-07, 1.6331E-07, 1.8339E-07,
     C 2.0445E-07, 2.2288E-07, 2.3083E-07, 2.3196E-07, 2.3919E-07,
     C 2.3339E-07, 2.3502E-07, 2.3444E-07, 2.6395E-07, 2.9928E-07,
     C 3.0025E-07, 3.0496E-07, 3.1777E-07, 3.4198E-07, 3.4739E-07,
     C 3.2696E-07, 3.4100E-07, 3.5405E-07, 3.7774E-07, 3.8285E-07,
     C 3.6797E-07, 3.5800E-07, 3.2283E-07, 2.9361E-07, 2.4881E-07,
     C 2.0599E-07, 1.7121E-07, 1.3641E-07, 1.1111E-07, 8.9413E-08/
      DATA S0901/
     C 7.3455E-08, 6.2078E-08, 5.2538E-08, 4.5325E-08, 3.9005E-08,
     C 3.4772E-08, 3.1203E-08, 2.8132E-08, 2.5250E-08, 2.2371E-08,
     C 2.0131E-08, 1.7992E-08, 1.6076E-08, 1.4222E-08, 1.2490E-08,
     C 1.1401E-08, 1.0249E-08, 9.2279E-09, 8.5654E-09, 7.6227E-09,
     C 6.9648E-09, 6.2466E-09, 5.7252E-09, 5.3800E-09, 4.6960E-09,
     C 4.2194E-09, 3.7746E-09, 3.3813E-09, 3.0656E-09, 2.6885E-09,
     C 2.4311E-09, 2.1572E-09, 1.8892E-09, 1.7038E-09, 1.4914E-09,
     C 1.3277E-09, 1.1694E-09, 1.0391E-09, 9.2779E-10, 8.3123E-10,
     C 7.4968E-10, 6.8385E-10, 6.2915E-10, 5.7784E-10, 5.2838E-10,
     C 4.8382E-10, 4.4543E-10, 4.1155E-10, 3.7158E-10, 3.3731E-10/
      DATA S0951/
     C 3.0969E-10, 2.8535E-10, 2.6416E-10, 2.4583E-10, 2.2878E-10,
     C 2.1379E-10, 2.0073E-10, 1.8907E-10, 1.7866E-10, 1.6936E-10,
     C 1.6119E-10, 1.5424E-10, 1.4847E-10, 1.4401E-10, 1.4068E-10,
     C 1.3937E-10, 1.3943E-10, 1.4281E-10, 1.4766E-10, 1.5701E-10,
     C 1.7079E-10, 1.8691E-10, 2.0081E-10, 2.1740E-10, 2.4847E-10,
     C 2.6463E-10, 2.7087E-10, 2.7313E-10, 2.8352E-10, 2.9511E-10,
     C 2.8058E-10, 2.7227E-10, 2.7356E-10, 2.8012E-10, 2.8034E-10,
     C 2.9031E-10, 3.1030E-10, 3.3745E-10, 3.8152E-10, 4.0622E-10,
     C 4.2673E-10, 4.3879E-10, 4.5488E-10, 4.7179E-10, 4.6140E-10,
     C 4.6339E-10, 4.6716E-10, 4.7024E-10, 4.7931E-10, 4.8503E-10/
      DATA S1001/
     C 4.9589E-10, 4.9499E-10, 5.0363E-10, 5.3184E-10, 5.6451E-10,
     C 6.0932E-10, 6.6469E-10, 7.4076E-10, 8.3605E-10, 9.4898E-10,
     C 1.0935E-09, 1.2593E-09, 1.4913E-09, 1.8099E-09, 2.1842E-09,
     C 2.7284E-09, 3.2159E-09, 3.7426E-09, 4.5226E-09, 5.3512E-09,
     C 6.1787E-09, 6.8237E-09, 7.9421E-09, 9.0002E-09, 9.6841E-09,
     C 9.9558E-09, 1.0232E-08, 1.0591E-08, 1.0657E-08, 1.0441E-08,
     C 1.0719E-08, 1.1526E-08, 1.2962E-08, 1.4336E-08, 1.6150E-08,
     C 1.8417E-08, 2.0725E-08, 2.3426E-08, 2.5619E-08, 2.7828E-08,
     C 3.0563E-08, 3.3438E-08, 3.6317E-08, 4.0400E-08, 4.4556E-08,
     C 5.0397E-08, 5.3315E-08, 5.9185E-08, 6.5311E-08, 6.9188E-08/
      DATA S1051/
     C 7.7728E-08, 7.9789E-08, 8.6598E-08, 8.7768E-08, 9.1773E-08,
     C 9.7533E-08, 1.0007E-07, 1.0650E-07, 1.0992E-07, 1.0864E-07,
     C 1.0494E-07, 1.0303E-07, 1.0031E-07, 1.0436E-07, 1.0537E-07,
     C 1.1184E-07, 1.2364E-07, 1.3651E-07, 1.4881E-07, 1.4723E-07,
     C 1.4118E-07, 1.3371E-07, 1.1902E-07, 1.0007E-07, 7.9628E-08,
     C 6.4362E-08, 5.0243E-08, 3.8133E-08, 2.9400E-08, 2.3443E-08,
     C 1.9319E-08, 1.6196E-08, 1.4221E-08, 1.2817E-08, 1.1863E-08,
     C 1.1383E-08, 1.1221E-08, 1.1574E-08, 1.1661E-08, 1.2157E-08,
     C 1.2883E-08, 1.3295E-08, 1.4243E-08, 1.4240E-08, 1.4614E-08,
     C 1.4529E-08, 1.4685E-08, 1.4974E-08, 1.4790E-08, 1.4890E-08/
      DATA S1101/
     C 1.4704E-08, 1.4142E-08, 1.3374E-08, 1.2746E-08, 1.2172E-08,
     C 1.2336E-08, 1.2546E-08, 1.3065E-08, 1.4090E-08, 1.5215E-08,
     C 1.6540E-08, 1.6144E-08, 1.5282E-08, 1.4358E-08, 1.2849E-08,
     C 1.0998E-08, 8.6956E-09, 7.0881E-09, 5.5767E-09, 4.2792E-09,
     C 3.2233E-09, 2.5020E-09, 1.9985E-09, 1.5834E-09, 1.3015E-09,
     C 1.0948E-09, 9.4141E-10, 8.1465E-10, 7.1517E-10, 6.2906E-10,
     C 5.5756E-10, 4.9805E-10, 4.3961E-10, 3.9181E-10, 3.5227E-10,
     C 3.1670E-10, 2.8667E-10, 2.5745E-10, 2.3212E-10, 2.0948E-10,
     C 1.8970E-10, 1.7239E-10, 1.5659E-10, 1.4301E-10, 1.3104E-10,
     C 1.2031E-10, 1.1095E-10, 1.0262E-10, 9.5130E-11, 8.8595E-11/
      DATA S1151/
     C 8.2842E-11, 7.7727E-11, 7.3199E-11, 6.9286E-11, 6.5994E-11,
     C 6.3316E-11, 6.1244E-11, 5.9669E-11, 5.8843E-11, 5.8832E-11,
     C 5.9547E-11, 6.1635E-11, 6.4926E-11, 7.0745E-11, 7.8802E-11,
     C 8.6724E-11, 1.0052E-10, 1.1575E-10, 1.3626E-10, 1.5126E-10,
     C 1.6751E-10, 1.9239E-10, 2.1748E-10, 2.2654E-10, 2.2902E-10,
     C 2.3240E-10, 2.4081E-10, 2.3930E-10, 2.2378E-10, 2.2476E-10,
     C 2.2791E-10, 2.4047E-10, 2.5305E-10, 2.8073E-10, 3.1741E-10,
     C 3.6592E-10, 4.1495E-10, 4.6565E-10, 5.0990E-10, 5.5607E-10,
     C 6.1928E-10, 6.6779E-10, 7.3350E-10, 8.1434E-10, 8.9635E-10,
     C 9.9678E-10, 1.1256E-09, 1.2999E-09, 1.4888E-09, 1.7642E-09/
      DATA S1201/
     C 1.9606E-09, 2.2066E-09, 2.4601E-09, 2.7218E-09, 3.0375E-09,
     C 3.1591E-09, 3.2852E-09, 3.2464E-09, 3.3046E-09, 3.2710E-09,
     C 3.2601E-09, 3.3398E-09, 3.7446E-09, 4.0795E-09, 4.0284E-09,
     C 4.0584E-09, 4.1677E-09, 4.5358E-09, 4.4097E-09, 4.2744E-09,
     C 4.5449E-09, 4.8147E-09, 5.2656E-09, 5.2476E-09, 5.0275E-09,
     C 4.7968E-09, 4.3654E-09, 3.9530E-09, 3.2447E-09, 2.6489E-09,
     C 2.1795E-09, 1.7880E-09, 1.4309E-09, 1.1256E-09, 9.1903E-10,
     C 7.6533E-10, 6.3989E-10, 5.5496E-10, 4.9581E-10, 4.5722E-10,
     C 4.3898E-10, 4.3505E-10, 4.3671E-10, 4.5329E-10, 4.6827E-10,
     C 4.9394E-10, 5.1122E-10, 5.1649E-10, 5.0965E-10, 4.9551E-10/
      DATA S1251/
     C 4.8928E-10, 4.7947E-10, 4.7989E-10, 4.9071E-10, 4.8867E-10,
     C 4.7260E-10, 4.5756E-10, 4.5400E-10, 4.5993E-10, 4.4042E-10,
     C 4.3309E-10, 4.4182E-10, 4.6735E-10, 5.0378E-10, 5.2204E-10,
     C 5.0166E-10, 4.6799E-10, 4.3119E-10, 3.8803E-10, 3.3291E-10,
     C 2.6289E-10, 2.1029E-10, 1.7011E-10, 1.3345E-10, 1.0224E-10,
     C 7.8207E-11, 6.2451E-11, 5.0481E-11, 4.1507E-11, 3.5419E-11,
     C 3.0582E-11, 2.6900E-11, 2.3778E-11, 2.1343E-11, 1.9182E-11,
     C 1.7162E-11, 1.5391E-11, 1.3877E-11, 1.2619E-11, 1.1450E-11,
     C 1.0461E-11, 9.6578E-12, 8.9579E-12, 8.3463E-12, 7.8127E-12,
     C 7.3322E-12, 6.9414E-12, 6.6037E-12, 6.3285E-12, 6.1095E-12/
      DATA S1301/
     C 5.9387E-12, 5.8118E-12, 5.7260E-12, 5.6794E-12, 5.6711E-12,
     C 5.7003E-12, 5.7670E-12, 5.8717E-12, 6.0151E-12, 6.1984E-12,
     C 6.4232E-12, 6.6918E-12, 7.0065E-12, 7.3705E-12, 7.7873E-12,
     C 8.2612E-12, 8.7972E-12, 9.4009E-12, 1.0079E-11, 1.0840E-11,
     C 1.1692E-11, 1.2648E-11, 1.3723E-11, 1.4935E-11, 1.6313E-11,
     C 1.7905E-11, 1.9740E-11, 2.1898E-11, 2.4419E-11, 2.7426E-11,
     C 3.0869E-11, 3.4235E-11, 3.7841E-11, 4.1929E-11, 4.6776E-11,
     C 5.2123E-11, 5.8497E-11, 6.5294E-11, 7.4038E-11, 8.4793E-11,
     C 9.6453E-11, 1.1223E-10, 1.2786E-10, 1.4882E-10, 1.7799E-10,
     C 2.0766E-10, 2.4523E-10, 2.8591E-10, 3.3386E-10, 4.0531E-10/
      DATA S1351/
     C 4.7663E-10, 5.4858E-10, 6.3377E-10, 7.1688E-10, 8.4184E-10,
     C 9.5144E-10, 1.0481E-09, 1.1356E-09, 1.2339E-09, 1.3396E-09,
     C 1.4375E-09, 1.5831E-09, 1.7323E-09, 1.9671E-09, 2.2976E-09,
     C 2.6679E-09, 3.0777E-09, 3.4321E-09, 3.8192E-09, 4.2711E-09,
     C 4.4903E-09, 4.8931E-09, 5.2253E-09, 5.4040E-09, 5.6387E-09,
     C 5.6704E-09, 6.0345E-09, 6.1079E-09, 6.2576E-09, 6.4039E-09,
     C 6.3776E-09, 6.1878E-09, 5.8616E-09, 5.7036E-09, 5.5840E-09,
     C 5.6905E-09, 5.8931E-09, 6.2478E-09, 6.8291E-09, 7.4528E-09,
     C 7.6078E-09, 7.3898E-09, 6.7573E-09, 5.9827E-09, 5.0927E-09,
     C 4.0099E-09, 3.1933E-09, 2.4296E-09, 1.8485E-09, 1.4595E-09/
      DATA S1401/
     C 1.2017E-09, 1.0164E-09, 8.7433E-10, 7.7108E-10, 7.0049E-10,
     C 6.5291E-10, 6.1477E-10, 5.9254E-10, 5.8150E-10, 5.7591E-10,
     C 5.8490E-10, 5.8587E-10, 5.9636E-10, 6.2408E-10, 6.5479E-10,
     C 7.0480E-10, 7.2313E-10, 7.5524E-10, 8.0863E-10, 8.3386E-10,
     C 9.2342E-10, 9.6754E-10, 1.0293E-09, 1.0895E-09, 1.1330E-09,
     C 1.2210E-09, 1.2413E-09, 1.2613E-09, 1.2671E-09, 1.2225E-09,
     C 1.1609E-09, 1.0991E-09, 1.0600E-09, 1.0570E-09, 1.0818E-09,
     C 1.1421E-09, 1.2270E-09, 1.3370E-09, 1.4742E-09, 1.4946E-09,
     C 1.4322E-09, 1.3210E-09, 1.1749E-09, 1.0051E-09, 7.8387E-10,
     C 6.1844E-10, 4.6288E-10, 3.4164E-10, 2.5412E-10, 1.9857E-10/
      DATA S1451/
     C 1.5876E-10, 1.2966E-10, 1.0920E-10, 9.4811E-11, 8.3733E-11,
     C 7.3906E-11, 6.7259E-11, 6.1146E-11, 5.7119E-11, 5.3546E-11,
     C 4.8625E-11, 4.4749E-11, 4.1089E-11, 3.7825E-11, 3.4465E-11,
     C 3.1018E-11, 2.8109E-11, 2.5610E-11, 2.2859E-11, 2.0490E-11,
     C 1.8133E-11, 1.5835E-11, 1.3949E-11, 1.2295E-11, 1.0799E-11,
     C 9.6544E-12, 8.7597E-12, 7.9990E-12, 7.3973E-12, 6.9035E-12,
     C 6.4935E-12, 6.1195E-12, 5.8235E-12, 5.5928E-12, 5.4191E-12,
     C 5.2993E-12, 5.2338E-12, 5.2272E-12, 5.2923E-12, 5.4252E-12,
     C 5.6523E-12, 5.9433E-12, 6.3197E-12, 6.9016E-12, 7.5016E-12,
     C 8.2885E-12, 9.4050E-12, 1.0605E-11, 1.2257E-11, 1.3622E-11/
      DATA S1501/
     C 1.5353E-11, 1.7543E-11, 1.9809E-11, 2.2197E-11, 2.4065E-11,
     C 2.6777E-11, 2.9751E-11, 3.2543E-11, 3.5536E-11, 3.9942E-11,
     C 4.6283E-11, 5.4556E-11, 6.5490E-11, 7.6803E-11, 9.0053E-11,
     C 1.0852E-10, 1.2946E-10, 1.4916E-10, 1.7748E-10, 2.0073E-10,
     C 2.2485E-10, 2.5114E-10, 2.7715E-10, 3.1319E-10, 3.3305E-10,
     C 3.5059E-10, 3.5746E-10, 3.6311E-10, 3.7344E-10, 3.6574E-10,
     C 3.7539E-10, 3.9434E-10, 4.3510E-10, 4.3340E-10, 4.2588E-10,
     C 4.3977E-10, 4.6062E-10, 4.7687E-10, 4.6457E-10, 4.8578E-10,
     C 5.2344E-10, 5.6752E-10, 5.8702E-10, 5.6603E-10, 5.3784E-10,
     C 4.9181E-10, 4.3272E-10, 3.5681E-10, 2.8814E-10, 2.3320E-10/
      DATA S1551/
     C 1.8631E-10, 1.4587E-10, 1.1782E-10, 9.8132E-11, 8.2528E-11,
     C 6.9174E-11, 6.1056E-11, 5.3459E-11, 4.7116E-11, 4.1878E-11,
     C 3.8125E-11, 3.6347E-11, 3.5071E-11, 3.3897E-11, 3.3541E-11,
     C 3.3563E-11, 3.5469E-11, 3.8111E-11, 3.8675E-11, 4.1333E-11,
     C 4.3475E-11, 4.6476E-11, 4.9761E-11, 5.1380E-11, 5.4135E-11,
     C 5.3802E-11, 5.5158E-11, 5.6864E-11, 5.9311E-11, 6.3827E-11,
     C 6.7893E-11, 6.8230E-11, 6.6694E-11, 6.6018E-11, 6.4863E-11,
     C 6.5893E-11, 6.3813E-11, 6.4741E-11, 6.8630E-11, 7.0255E-11,
     C 7.0667E-11, 6.8810E-11, 6.4104E-11, 5.8136E-11, 4.7242E-11,
     C 3.7625E-11, 3.1742E-11, 2.5581E-11, 1.8824E-11, 1.3303E-11/
      DATA S1601/
     C 9.6919E-12, 7.5353E-12, 6.0986E-12, 5.0742E-12, 4.3094E-12,
     C 3.7190E-12, 3.2520E-12, 2.8756E-12, 2.5680E-12, 2.3139E-12,
     C 2.1025E-12, 1.9257E-12, 1.7777E-12, 1.6539E-12, 1.5508E-12,
     C 1.4657E-12, 1.3966E-12, 1.3417E-12, 1.2998E-12, 1.2700E-12,
     C 1.2514E-12, 1.2437E-12, 1.2463E-12, 1.2592E-12, 1.2823E-12,
     C 1.3157E-12, 1.3596E-12, 1.4144E-12, 1.4806E-12, 1.5588E-12,
     C 1.6497E-12, 1.7544E-12, 1.8738E-12, 2.0094E-12, 2.1626E-12,
     C 2.3354E-12, 2.5297E-12, 2.7483E-12, 2.9941E-12, 3.2708E-12,
     C 3.5833E-12, 3.9374E-12, 4.3415E-12, 4.8079E-12, 5.3602E-12,
     C 5.9816E-12, 6.7436E-12, 7.6368E-12, 8.6812E-12, 9.8747E-12/
      DATA S1651/
     C 1.1350E-11, 1.3181E-11, 1.5406E-11, 1.7868E-11, 2.0651E-11,
     C 2.4504E-11, 2.9184E-11, 3.4159E-11, 3.9979E-11, 4.8704E-11,
     C 5.7856E-11, 6.7576E-11, 7.9103E-11, 9.4370E-11, 1.1224E-10,
     C 1.3112E-10, 1.5674E-10, 1.8206E-10, 2.0576E-10, 2.3187E-10,
     C 2.7005E-10, 3.0055E-10, 3.3423E-10, 3.6956E-10, 3.8737E-10,
     C 4.2630E-10, 4.5154E-10, 4.8383E-10, 5.3582E-10, 5.8109E-10,
     C 6.3741E-10, 6.3874E-10, 6.3870E-10, 6.5818E-10, 6.5056E-10,
     C 6.5291E-10, 6.3159E-10, 6.3984E-10, 6.4549E-10, 6.5444E-10,
     C 6.7035E-10, 6.7665E-10, 6.9124E-10, 6.8451E-10, 6.9255E-10,
     C 6.9923E-10, 7.0396E-10, 6.7715E-10, 6.0371E-10, 5.3774E-10/
      DATA S1701/
     C 4.6043E-10, 3.7635E-10, 2.9484E-10, 2.2968E-10, 1.8185E-10,
     C 1.4191E-10, 1.1471E-10, 9.4790E-11, 7.9613E-11, 6.7989E-11,
     C 5.9391E-11, 5.2810E-11, 4.7136E-11, 4.2618E-11, 3.8313E-11,
     C 3.4686E-11, 3.1669E-11, 2.9110E-11, 2.6871E-11, 2.5074E-11,
     C 2.4368E-11, 2.3925E-11, 2.4067E-11, 2.4336E-11, 2.4704E-11,
     C 2.5823E-11, 2.7177E-11, 2.9227E-11, 3.1593E-11, 3.5730E-11,
     C 4.0221E-11, 4.3994E-11, 4.8448E-11, 5.3191E-11, 5.8552E-11,
     C 6.3458E-11, 6.6335E-11, 7.2457E-11, 7.9091E-11, 8.2234E-11,
     C 8.7668E-11, 8.7951E-11, 9.2952E-11, 9.6157E-11, 9.5926E-11,
     C 1.0120E-10, 1.0115E-10, 9.9577E-11, 9.6633E-11, 9.2891E-11/
      DATA S1751/
     C 9.3315E-11, 9.5584E-11, 1.0064E-10, 1.0509E-10, 1.1455E-10,
     C 1.2443E-10, 1.2963E-10, 1.2632E-10, 1.1308E-10, 1.0186E-10,
     C 8.5880E-11, 6.7863E-11, 5.1521E-11, 3.7780E-11, 2.8842E-11,
     C 2.2052E-11, 1.7402E-11, 1.4406E-11, 1.1934E-11, 1.0223E-11,
     C 8.9544E-12, 7.9088E-12, 7.0675E-12, 6.2222E-12, 5.6051E-12,
     C 5.0502E-12, 4.5578E-12, 4.2636E-12, 3.9461E-12, 3.7599E-12,
     C 3.5215E-12, 3.2467E-12, 3.0018E-12, 2.6558E-12, 2.3928E-12,
     C 2.0707E-12, 1.7575E-12, 1.5114E-12, 1.2941E-12, 1.1004E-12,
     C 9.5175E-13, 8.2894E-13, 7.3253E-13, 6.5551E-13, 5.9098E-13,
     C 5.3548E-13, 4.8697E-13, 4.4413E-13, 4.0600E-13, 3.7188E-13/
      DATA S1801/
     C 3.4121E-13, 3.1356E-13, 2.8856E-13, 2.6590E-13, 2.4533E-13,
     C 2.2663E-13, 2.0960E-13, 1.9407E-13, 1.7990E-13, 1.6695E-13,
     C 1.5512E-13, 1.4429E-13, 1.3437E-13, 1.2527E-13, 1.1693E-13,
     C 1.0927E-13, 1.0224E-13, 9.5767E-14, 8.9816E-14, 8.4335E-14,
     C 7.9285E-14, 7.4626E-14, 7.0325E-14, 6.6352E-14, 6.2676E-14,
     C 5.9274E-14, 5.6121E-14, 5.3195E-14, 5.0479E-14, 4.7953E-14,
     C 4.5602E-14, 4.3411E-14, 4.1367E-14, 3.9456E-14, 3.7670E-14,
     C 3.5996E-14, 3.4427E-14, 3.2952E-14, 3.1566E-14, 3.0261E-14,
     C 2.9030E-14, 2.7868E-14, 2.6770E-14, 2.5730E-14, 2.4745E-14,
     C 2.3809E-14, 2.2921E-14, 2.2076E-14, 2.1271E-14, 2.0504E-14/
      DATA S1851/
     C 1.9772E-14, 1.9073E-14, 1.8404E-14, 1.7764E-14, 1.7151E-14,
     C 1.6564E-14, 1.6000E-14, 1.5459E-14, 1.4939E-14, 1.4439E-14,
     C 1.3958E-14, 1.3495E-14, 1.3049E-14, 1.2620E-14, 1.2206E-14,
     C 1.1807E-14, 1.1422E-14, 1.1050E-14, 1.0691E-14, 1.0345E-14,
     C 1.0010E-14, 9.6870E-15, 9.3747E-15, 9.0727E-15, 8.7808E-15,
     C 8.4986E-15, 8.2257E-15, 7.9617E-15, 7.7064E-15, 7.4594E-15,
     C 7.2204E-15, 6.9891E-15, 6.7653E-15, 6.5488E-15, 6.3392E-15,
     C 6.1363E-15, 5.9399E-15, 5.7499E-15, 5.5659E-15, 5.3878E-15,
     C 5.2153E-15, 5.0484E-15, 4.8868E-15, 4.7303E-15, 4.5788E-15,
     C 4.4322E-15, 4.2902E-15, 4.1527E-15, 4.0196E-15, 3.8907E-15/
      DATA S1901/
     C 3.7659E-15, 3.6451E-15, 3.5281E-15, 3.4149E-15, 3.3052E-15,
     C 3.1991E-15, 3.0963E-15, 2.9967E-15, 2.9004E-15, 2.8071E-15,
     C 2.7167E-15, 2.6293E-15, 2.5446E-15, 2.4626E-15, 2.3833E-15,
     C 2.3064E-15, 2.2320E-15, 2.1600E-15, 2.0903E-15, 2.0228E-15,
     C 1.9574E-15, 1.8942E-15, 1.8329E-15, 1.7736E-15, 1.7163E-15,
     C 1.6607E-15, 1.6069E-15, 1.5548E-15, 1.5044E-15, 1.4557E-15,
     C 1.4084E-15, 1.3627E-15, 1.3185E-15, 1.2757E-15, 1.2342E-15,
     C 1.1941E-15, 1.1552E-15, 1.1177E-15, 1.0813E-15, 1.0461E-15,
     C 1.0120E-15, 9.7900E-16, 9.4707E-16, 9.1618E-16, 8.8628E-16,
     C 8.5734E-16, 8.2933E-16, 8.0223E-16, 7.7600E-16, 7.5062E-16/
      DATA S1951/
     C 7.2606E-16, 7.0229E-16, 6.7929E-16, 6.5703E-16, 6.3550E-16,
     C 6.1466E-16, 5.9449E-16, 5.7498E-16, 5.5610E-16, 5.3783E-16,
     C 5.2015E-16, 5.0305E-16, 4.8650E-16, 4.7049E-16, 4.5500E-16,
     C 4.4002E-16, 4.2552E-16, 4.1149E-16, 3.9792E-16, 3.8479E-16,
     C 3.7209E-16, 3.5981E-16, 3.4792E-16, 3.3642E-16, 3.2530E-16,
     C 3.1454E-16, 3.0413E-16, 2.9406E-16, 2.8432E-16, 2.7490E-16,
     C 2.6579E-16, 2.5697E-16, 2.4845E-16, 2.4020E-16, 2.3223E-16,
     C 2.2451E-16, 2.1705E-16, 2.0984E-16, 2.0286E-16, 1.9611E-16,
     C 1.8958E-16, 1.8327E-16, 1.7716E-16, 1.7126E-16, 1.6555E-16,
     C 1.6003E-16, 1.5469E-16, 1.4952E-16, 1.4453E-16, 1.3970E-16/
      DATA S2001/
     C 1.3503E-16/
C
      END
      BLOCK DATA SF260
C>    BLOCK DATA
C               06/28/82
C               UNITS OF (CM**3/MOL) * 1.E-20
      COMMON /S260/ V1,V2,DV,NPT,S0000(2),
     1      S0001(50),S0051(50),S0101(50),S0151(50),S0201(50),S0251(50),
     2      S0301(50),S0351(50),S0401(50),S0451(50),S0501(50),S0551(50),
     3      S0601(50),S0651(50),S0701(50),S0751(50),S0801(50),S0851(50),
     4      S0901(50),S0951(50),S1001(50),S1051(50),S1101(50),S1151(50),
     5      S1201(50),S1251(50),S1301(50),S1351(50),S1401(50),S1451(50),
     6      S1501(50),S1551(50),S1601(50),S1651(50),S1701(50),S1751(50),
     7      S1801(50),S1851(50),S1901(50),S1951(50),S2001(1)
C
C
       DATA V1,V2,DV,NPT /
     1      -20.0,     20000.0,       10.0,  2003/
C
C
      DATA S0000/ 1.7750E-01, 1.7045E-01/
      DATA S0001/
     C 1.6457E-01, 1.7045E-01, 1.7750E-01, 2.0036E-01, 2.1347E-01,
     C 2.2454E-01, 2.3428E-01, 2.3399E-01, 2.3022E-01, 2.0724E-01,
     C 1.9712E-01, 1.8317E-01, 1.6724E-01, 1.4780E-01, 1.2757E-01,
     C 1.1626E-01, 1.0098E-01, 8.9033E-02, 7.9770E-02, 6.7416E-02,
     C 5.9588E-02, 5.1117E-02, 4.6218E-02, 4.2179E-02, 3.4372E-02,
     C 2.9863E-02, 2.5252E-02, 2.2075E-02, 1.9209E-02, 1.5816E-02,
     C 1.3932E-02, 1.1943E-02, 1.0079E-02, 8.7667E-03, 7.4094E-03,
     C 6.4967E-03, 5.5711E-03, 4.8444E-03, 4.2552E-03, 3.6953E-03,
     C 3.2824E-03, 2.9124E-03, 2.6102E-03, 2.3370E-03, 2.1100E-03,
     C 1.9008E-03, 1.7145E-03, 1.5573E-03, 1.4206E-03, 1.2931E-03/
      DATA S0051/
     C 1.1803E-03, 1.0774E-03, 9.8616E-04, 9.0496E-04, 8.3071E-04,
     C 7.6319E-04, 7.0149E-04, 6.4637E-04, 5.9566E-04, 5.4987E-04,
     C 5.0768E-04, 4.6880E-04, 4.3317E-04, 4.0037E-04, 3.7064E-04,
     C 3.4325E-04, 3.1809E-04, 2.9501E-04, 2.7382E-04, 2.5430E-04,
     C 2.3630E-04, 2.1977E-04, 2.0452E-04, 1.9042E-04, 1.7740E-04,
     C 1.6544E-04, 1.5442E-04, 1.4425E-04, 1.3486E-04, 1.2618E-04,
     C 1.1817E-04, 1.1076E-04, 1.0391E-04, 9.7563E-05, 9.1696E-05,
     C 8.6272E-05, 8.1253E-05, 7.6607E-05, 7.2302E-05, 6.8311E-05,
     C 6.4613E-05, 6.1183E-05, 5.8001E-05, 5.5048E-05, 5.2307E-05,
     C 4.9761E-05, 4.7395E-05, 4.5197E-05, 4.3155E-05, 4.1256E-05/
      DATA S0101/
     C 3.9491E-05, 3.7849E-05, 3.6324E-05, 3.4908E-05, 3.3594E-05,
     C 3.2374E-05, 3.1244E-05, 3.0201E-05, 2.9240E-05, 2.8356E-05,
     C 2.7547E-05, 2.6814E-05, 2.6147E-05, 2.5551E-05, 2.5029E-05,
     C 2.4582E-05, 2.4203E-05, 2.3891E-05, 2.3663E-05, 2.3531E-05,
     C 2.3483E-05, 2.3516E-05, 2.3694E-05, 2.4032E-05, 2.4579E-05,
     C 2.5234E-05, 2.6032E-05, 2.7119E-05, 2.8631E-05, 3.0848E-05,
     C 3.3262E-05, 3.6635E-05, 4.0732E-05, 4.5923E-05, 5.3373E-05,
     C 6.1875E-05, 7.2031E-05, 8.5980E-05, 9.8642E-05, 1.1469E-04,
     C 1.3327E-04, 1.5390E-04, 1.7513E-04, 2.0665E-04, 2.3609E-04,
     C 2.6220E-04, 2.8677E-04, 3.2590E-04, 3.8624E-04, 4.1570E-04/
      DATA S0151/
     C 4.5207E-04, 4.9336E-04, 5.4500E-04, 5.8258E-04, 5.8086E-04,
     C 5.6977E-04, 5.3085E-04, 4.8020E-04, 4.3915E-04, 4.0343E-04,
     C 3.7853E-04, 3.7025E-04, 3.9637E-04, 4.4675E-04, 4.7072E-04,
     C 4.9022E-04, 5.2076E-04, 5.3676E-04, 5.2755E-04, 4.8244E-04,
     C 4.5473E-04, 4.3952E-04, 3.9614E-04, 3.4086E-04, 2.9733E-04,
     C 2.6367E-04, 2.3767E-04, 2.0427E-04, 1.7595E-04, 1.5493E-04,
     C 1.3851E-04, 1.1874E-04, 1.0735E-04, 9.0490E-05, 8.1149E-05,
     C 7.4788E-05, 6.5438E-05, 5.8248E-05, 4.8076E-05, 4.3488E-05,
     C 3.7856E-05, 3.3034E-05, 2.9592E-05, 2.6088E-05, 2.3497E-05,
     C 2.0279E-05, 1.7526E-05, 1.5714E-05, 1.3553E-05, 1.2145E-05/
      DATA S0201/
     C 1.0802E-05, 9.7681E-06, 8.8196E-06, 7.8291E-06, 7.1335E-06,
     C 6.4234E-06, 5.8391E-06, 5.3532E-06, 4.9079E-06, 4.5378E-06,
     C 4.1716E-06, 3.8649E-06, 3.5893E-06, 3.3406E-06, 3.1199E-06,
     C 2.9172E-06, 2.7348E-06, 2.5644E-06, 2.4086E-06, 2.2664E-06,
     C 2.1359E-06, 2.0159E-06, 1.9051E-06, 1.8031E-06, 1.7074E-06,
     C 1.6185E-06, 1.5356E-06, 1.4584E-06, 1.3861E-06, 1.3179E-06,
     C 1.2545E-06, 1.1951E-06, 1.1395E-06, 1.0873E-06, 1.0384E-06,
     C 9.9250E-07, 9.4935E-07, 9.0873E-07, 8.7050E-07, 8.3446E-07,
     C 8.0046E-07, 7.6834E-07, 7.3800E-07, 7.0931E-07, 6.8217E-07,
     C 6.5648E-07, 6.3214E-07, 6.0909E-07, 5.8725E-07, 5.6655E-07/
      DATA S0251/
     C 5.4693E-07, 5.2835E-07, 5.1077E-07, 4.9416E-07, 4.7853E-07,
     C 4.6381E-07, 4.5007E-07, 4.3728E-07, 4.2550E-07, 4.1450E-07,
     C 4.0459E-07, 3.9532E-07, 3.8662E-07, 3.7855E-07, 3.7041E-07,
     C 3.6254E-07, 3.5420E-07, 3.4617E-07, 3.3838E-07, 3.3212E-07,
     C 3.2655E-07, 3.1865E-07, 3.1203E-07, 3.0670E-07, 3.0252E-07,
     C 2.9749E-07, 2.9184E-07, 2.8795E-07, 2.8501E-07, 2.8202E-07,
     C 2.7856E-07, 2.7509E-07, 2.7152E-07, 2.6844E-07, 2.6642E-07,
     C 2.6548E-07, 2.6617E-07, 2.6916E-07, 2.7372E-07, 2.8094E-07,
     C 2.9236E-07, 3.1035E-07, 3.2854E-07, 3.5481E-07, 3.9377E-07,
     C 4.4692E-07, 5.0761E-07, 5.7715E-07, 6.7725E-07, 8.0668E-07/
      DATA S0301/
     C 9.3716E-07, 1.0797E-06, 1.1689E-06, 1.3217E-06, 1.4814E-06,
     C 1.5627E-06, 1.6519E-06, 1.7601E-06, 1.9060E-06, 2.0474E-06,
     C 2.0716E-06, 2.0433E-06, 1.9752E-06, 1.8466E-06, 1.7526E-06,
     C 1.6657E-06, 1.5870E-06, 1.5633E-06, 1.6520E-06, 1.8471E-06,
     C 1.9953E-06, 2.0975E-06, 2.2016E-06, 2.2542E-06, 2.3081E-06,
     C 2.3209E-06, 2.2998E-06, 2.3056E-06, 2.2757E-06, 2.2685E-06,
     C 2.2779E-06, 2.2348E-06, 2.2445E-06, 2.3174E-06, 2.4284E-06,
     C 2.5290E-06, 2.7340E-06, 2.9720E-06, 3.2332E-06, 3.5392E-06,
     C 3.9013E-06, 4.3334E-06, 4.9088E-06, 5.3428E-06, 5.9142E-06,
     C 6.6106E-06, 7.4709E-06, 8.5019E-06, 9.6835E-06, 1.0984E-05/
      DATA S0351/
     C 1.2831E-05, 1.4664E-05, 1.7080E-05, 2.0103E-05, 2.4148E-05,
     C 2.7948E-05, 3.2855E-05, 3.9046E-05, 4.6429E-05, 5.6633E-05,
     C 6.6305E-05, 7.6048E-05, 8.7398E-05, 1.0034E-04, 1.1169E-04,
     C 1.2813E-04, 1.3354E-04, 1.3952E-04, 1.4204E-04, 1.4615E-04,
     C 1.5144E-04, 1.5475E-04, 1.6561E-04, 1.7135E-04, 1.6831E-04,
     C 1.6429E-04, 1.6353E-04, 1.6543E-04, 1.5944E-04, 1.5404E-04,
     C 1.5458E-04, 1.6287E-04, 1.7277E-04, 1.8387E-04, 1.7622E-04,
     C 1.6360E-04, 1.5273E-04, 1.3667E-04, 1.2364E-04, 9.7576E-05,
     C 7.9140E-05, 6.4241E-05, 5.1826E-05, 4.1415E-05, 3.1347E-05,
     C 2.5125E-05, 2.0027E-05, 1.6362E-05, 1.3364E-05, 1.1117E-05/
      DATA S0401/
     C 9.4992E-06, 8.1581E-06, 7.1512E-06, 6.2692E-06, 5.5285E-06,
     C 4.9000E-06, 4.3447E-06, 3.8906E-06, 3.4679E-06, 3.1089E-06,
     C 2.8115E-06, 2.5496E-06, 2.2982E-06, 2.0861E-06, 1.8763E-06,
     C 1.7035E-06, 1.5548E-06, 1.4107E-06, 1.2839E-06, 1.1706E-06,
     C 1.0709E-06, 9.8099E-07, 8.9901E-07, 8.2394E-07, 7.5567E-07,
     C 6.9434E-07, 6.3867E-07, 5.8845E-07, 5.4263E-07, 5.0033E-07,
     C 4.6181E-07, 4.2652E-07, 3.9437E-07, 3.6497E-07, 3.3781E-07,
     C 3.1292E-07, 2.9011E-07, 2.6915E-07, 2.4989E-07, 2.3215E-07,
     C 2.1582E-07, 2.0081E-07, 1.8700E-07, 1.7432E-07, 1.6264E-07,
     C 1.5191E-07, 1.4207E-07, 1.3306E-07, 1.2484E-07, 1.1737E-07/
      DATA S0451/
     C 1.1056E-07, 1.0451E-07, 9.9060E-08, 9.4135E-08, 8.9608E-08,
     C 8.5697E-08, 8.1945E-08, 7.8308E-08, 7.4808E-08, 7.1686E-08,
     C 6.8923E-08, 6.5869E-08, 6.3308E-08, 6.0840E-08, 5.8676E-08,
     C 5.6744E-08, 5.5016E-08, 5.3813E-08, 5.2792E-08, 5.2097E-08,
     C 5.1737E-08, 5.1603E-08, 5.1656E-08, 5.1989E-08, 5.2467E-08,
     C 5.2918E-08, 5.3589E-08, 5.4560E-08, 5.5869E-08, 5.7403E-08,
     C 5.8968E-08, 6.0973E-08, 6.3432E-08, 6.6245E-08, 6.9353E-08,
     C 7.2686E-08, 7.6541E-08, 8.0991E-08, 8.5950E-08, 9.1429E-08,
     C 9.7851E-08, 1.0516E-07, 1.1349E-07, 1.2295E-07, 1.3335E-07,
     C 1.4488E-07, 1.5864E-07, 1.7412E-07, 1.9140E-07, 2.1078E-07/
      DATA S0501/
     C 2.3369E-07, 2.5996E-07, 2.8848E-07, 3.2169E-07, 3.5991E-07,
     C 4.0566E-07, 4.5969E-07, 5.3094E-07, 6.1458E-07, 7.1155E-07,
     C 8.3045E-07, 9.9021E-07, 1.2042E-06, 1.4914E-06, 1.8145E-06,
     C 2.2210E-06, 2.7831E-06, 3.4533E-06, 4.4446E-06, 5.1989E-06,
     C 6.2289E-06, 7.1167E-06, 8.3949E-06, 9.6417E-06, 1.0313E-05,
     C 1.0485E-05, 1.0641E-05, 1.0898E-05, 1.0763E-05, 1.0506E-05,
     C 1.0497E-05, 1.1696E-05, 1.2654E-05, 1.3029E-05, 1.3175E-05,
     C 1.4264E-05, 1.4985E-05, 1.4999E-05, 1.4317E-05, 1.4616E-05,
     C 1.4963E-05, 1.5208E-05, 1.4942E-05, 1.3879E-05, 1.3087E-05,
     C 1.1727E-05, 1.0515E-05, 9.0073E-06, 7.3133E-06, 6.1181E-06/
      DATA S0551/
     C 5.0623E-06, 4.1105E-06, 3.3915E-06, 2.6711E-06, 2.1464E-06,
     C 1.7335E-06, 1.4302E-06, 1.1847E-06, 9.9434E-07, 8.2689E-07,
     C 7.0589E-07, 6.0750E-07, 5.3176E-07, 4.6936E-07, 4.1541E-07,
     C 3.6625E-07, 3.2509E-07, 2.9156E-07, 2.6308E-07, 2.3819E-07,
     C 2.1421E-07, 1.9366E-07, 1.7626E-07, 1.5982E-07, 1.4567E-07,
     C 1.3354E-07, 1.2097E-07, 1.1029E-07, 1.0063E-07, 9.2003E-08,
     C 8.4245E-08, 7.7004E-08, 7.0636E-08, 6.4923E-08, 5.9503E-08,
     C 5.4742E-08, 5.0450E-08, 4.6470E-08, 4.2881E-08, 3.9550E-08,
     C 3.6541E-08, 3.3803E-08, 3.1279E-08, 2.8955E-08, 2.6858E-08,
     C 2.4905E-08, 2.3146E-08, 2.1539E-08, 2.0079E-08, 1.8746E-08/
      DATA S0601/
     C 1.7517E-08, 1.6396E-08, 1.5369E-08, 1.4426E-08, 1.3543E-08,
     C 1.2724E-08, 1.1965E-08, 1.1267E-08, 1.0617E-08, 1.0010E-08,
     C 9.4662E-09, 8.9553E-09, 8.4988E-09, 8.0807E-09, 7.7043E-09,
     C 7.3721E-09, 7.0707E-09, 6.8047E-09, 6.5702E-09, 6.3634E-09,
     C 6.1817E-09, 6.0239E-09, 5.8922E-09, 5.7824E-09, 5.7019E-09,
     C 5.6368E-09, 5.5940E-09, 5.5669E-09, 5.5583E-09, 5.5653E-09,
     C 5.5837E-09, 5.6243E-09, 5.6883E-09, 5.7800E-09, 5.8964E-09,
     C 6.0429E-09, 6.2211E-09, 6.4282E-09, 6.6634E-09, 6.9306E-09,
     C 7.2336E-09, 7.5739E-09, 7.9562E-09, 8.3779E-09, 8.8575E-09,
     C 9.3992E-09, 1.0004E-08, 1.0684E-08, 1.1450E-08, 1.2320E-08/
      DATA S0651/
     C 1.3311E-08, 1.4455E-08, 1.5758E-08, 1.7254E-08, 1.8927E-08,
     C 2.0930E-08, 2.3348E-08, 2.6074E-08, 2.9221E-08, 3.2770E-08,
     C 3.7485E-08, 4.2569E-08, 4.8981E-08, 5.5606E-08, 6.2393E-08,
     C 7.1901E-08, 8.2921E-08, 9.5513E-08, 1.1111E-07, 1.3143E-07,
     C 1.5971E-07, 1.8927E-07, 2.2643E-07, 2.7860E-07, 3.2591E-07,
     C 3.7024E-07, 4.2059E-07, 4.9432E-07, 5.5543E-07, 5.7498E-07,
     C 5.9210E-07, 6.1005E-07, 6.1577E-07, 5.9193E-07, 5.6602E-07,
     C 5.7403E-07, 6.0050E-07, 6.4723E-07, 6.7073E-07, 7.5415E-07,
     C 8.0982E-07, 8.7658E-07, 9.1430E-07, 9.4459E-07, 9.8347E-07,
     C 9.8768E-07, 1.0153E-06, 1.0066E-06, 1.0353E-06, 1.0353E-06/
      DATA S0701/
     C 1.0722E-06, 1.1138E-06, 1.1923E-06, 1.2947E-06, 1.4431E-06,
     C 1.6537E-06, 1.8662E-06, 2.2473E-06, 2.6464E-06, 3.1041E-06,
     C 3.4858E-06, 4.0167E-06, 4.6675E-06, 5.0983E-06, 5.7997E-06,
     C 6.0503E-06, 6.4687E-06, 6.5396E-06, 6.7986E-06, 7.0244E-06,
     C 7.2305E-06, 7.6732E-06, 7.9783E-06, 7.9846E-06, 7.7617E-06,
     C 7.7657E-06, 7.7411E-06, 7.8816E-06, 7.8136E-06, 8.0051E-06,
     C 8.5799E-06, 9.1659E-06, 9.8646E-06, 9.4920E-06, 8.7670E-06,
     C 8.2034E-06, 7.2297E-06, 6.2324E-06, 4.9315E-06, 3.9128E-06,
     C 3.1517E-06, 2.4469E-06, 1.8815E-06, 1.4627E-06, 1.1698E-06,
     C 9.4686E-07, 7.8486E-07, 6.6970E-07, 5.8811E-07, 5.2198E-07/
      DATA S0751/
     C 4.6809E-07, 4.1671E-07, 3.7006E-07, 3.3066E-07, 2.9387E-07,
     C 2.6415E-07, 2.3409E-07, 2.0991E-07, 1.9132E-07, 1.7519E-07,
     C 1.5939E-07, 1.4368E-07, 1.3050E-07, 1.1883E-07, 1.0772E-07,
     C 9.6884E-08, 8.7888E-08, 7.8956E-08, 7.1024E-08, 6.3824E-08,
     C 5.7256E-08, 5.1769E-08, 4.7037E-08, 4.2901E-08, 3.8970E-08,
     C 3.5467E-08, 3.2502E-08, 2.9827E-08, 2.7389E-08, 2.5111E-08,
     C 2.3056E-08, 2.1267E-08, 1.9610E-08, 1.8133E-08, 1.6775E-08,
     C 1.5491E-08, 1.4329E-08, 1.3265E-08, 1.2300E-08, 1.1420E-08,
     C 1.0593E-08, 9.8475E-09, 9.1585E-09, 8.5256E-09, 7.9525E-09,
     C 7.4226E-09, 6.9379E-09, 6.4950E-09, 6.0911E-09, 5.7242E-09/
      DATA S0801/
     C 5.3877E-09, 5.0821E-09, 4.8051E-09, 4.5554E-09, 4.3315E-09,
     C 4.1336E-09, 3.9632E-09, 3.8185E-09, 3.7080E-09, 3.6296E-09,
     C 3.5804E-09, 3.5776E-09, 3.6253E-09, 3.7115E-09, 3.8151E-09,
     C 3.9804E-09, 4.1742E-09, 4.3581E-09, 4.5306E-09, 4.7736E-09,
     C 5.1297E-09, 5.5291E-09, 5.9125E-09, 6.4956E-09, 7.0362E-09,
     C 7.5318E-09, 7.9947E-09, 8.6438E-09, 9.7227E-09, 1.0130E-08,
     C 1.0549E-08, 1.1064E-08, 1.1702E-08, 1.2043E-08, 1.1781E-08,
     C 1.1838E-08, 1.1917E-08, 1.2131E-08, 1.2476E-08, 1.3611E-08,
     C 1.4360E-08, 1.5057E-08, 1.6247E-08, 1.7284E-08, 1.8420E-08,
     C 1.8352E-08, 1.8722E-08, 1.9112E-08, 1.9092E-08, 1.9311E-08/
      DATA S0851/
     C 1.9411E-08, 1.9884E-08, 2.0508E-08, 2.1510E-08, 2.3143E-08,
     C 2.5050E-08, 2.7596E-08, 3.1231E-08, 3.6260E-08, 4.3410E-08,
     C 5.2240E-08, 6.3236E-08, 7.7522E-08, 9.8688E-08, 1.1859E-07,
     C 1.4341E-07, 1.6798E-07, 1.9825E-07, 2.2898E-07, 2.6257E-07,
     C 2.9884E-07, 3.3247E-07, 3.4936E-07, 3.5583E-07, 3.7150E-07,
     C 3.6580E-07, 3.7124E-07, 3.7030E-07, 4.1536E-07, 4.6656E-07,
     C 4.6677E-07, 4.7507E-07, 4.9653E-07, 5.3795E-07, 5.4957E-07,
     C 5.2238E-07, 5.4690E-07, 5.6569E-07, 5.9844E-07, 5.9835E-07,
     C 5.6522E-07, 5.4123E-07, 4.7904E-07, 4.2851E-07, 3.5603E-07,
     C 2.8932E-07, 2.3655E-07, 1.8592E-07, 1.4943E-07, 1.1971E-07/
      DATA S0901/
     C 9.8482E-08, 8.3675E-08, 7.1270E-08, 6.2496E-08, 5.4999E-08,
     C 4.9821E-08, 4.5387E-08, 4.1340E-08, 3.7453E-08, 3.3298E-08,
     C 3.0120E-08, 2.7032E-08, 2.4236E-08, 2.1500E-08, 1.8988E-08,
     C 1.7414E-08, 1.5706E-08, 1.4192E-08, 1.3204E-08, 1.1759E-08,
     C 1.0737E-08, 9.6309E-09, 8.8179E-09, 8.2619E-09, 7.2264E-09,
     C 6.4856E-09, 5.8037E-09, 5.2093E-09, 4.7205E-09, 4.1749E-09,
     C 3.7852E-09, 3.3915E-09, 3.0089E-09, 2.7335E-09, 2.4398E-09,
     C 2.2031E-09, 1.9786E-09, 1.7890E-09, 1.6266E-09, 1.4830E-09,
     C 1.3576E-09, 1.2518E-09, 1.1587E-09, 1.0726E-09, 9.9106E-10,
     C 9.1673E-10, 8.5084E-10, 7.9147E-10, 7.2882E-10, 6.7342E-10/
      DATA S0951/
     C 6.2593E-10, 5.8294E-10, 5.4435E-10, 5.0997E-10, 4.7806E-10,
     C 4.4931E-10, 4.2357E-10, 4.0023E-10, 3.7909E-10, 3.5999E-10,
     C 3.4285E-10, 3.2776E-10, 3.1468E-10, 3.0377E-10, 2.9479E-10,
     C 2.8877E-10, 2.8512E-10, 2.8617E-10, 2.8976E-10, 3.0001E-10,
     C 3.1718E-10, 3.3898E-10, 3.5857E-10, 3.8358E-10, 4.3131E-10,
     C 4.5741E-10, 4.6948E-10, 4.7594E-10, 4.9529E-10, 5.1563E-10,
     C 4.9475E-10, 4.8369E-10, 4.8829E-10, 5.0047E-10, 5.0203E-10,
     C 5.1954E-10, 5.5352E-10, 5.9928E-10, 6.7148E-10, 7.1121E-10,
     C 7.4317E-10, 7.6039E-10, 7.8313E-10, 8.0684E-10, 7.8553E-10,
     C 7.8312E-10, 7.8537E-10, 7.8872E-10, 8.0185E-10, 8.1004E-10/
      DATA S1001/
     C 8.2608E-10, 8.2525E-10, 8.3857E-10, 8.7920E-10, 9.2451E-10,
     C 9.8661E-10, 1.0629E-09, 1.1659E-09, 1.2922E-09, 1.4387E-09,
     C 1.6254E-09, 1.8425E-09, 2.1428E-09, 2.5477E-09, 3.0379E-09,
     C 3.7570E-09, 4.4354E-09, 5.1802E-09, 6.2769E-09, 7.4894E-09,
     C 8.7474E-09, 9.8037E-09, 1.1582E-08, 1.3293E-08, 1.4471E-08,
     C 1.5025E-08, 1.5580E-08, 1.6228E-08, 1.6413E-08, 1.6020E-08,
     C 1.6393E-08, 1.7545E-08, 1.9590E-08, 2.1449E-08, 2.3856E-08,
     C 2.7050E-08, 3.0214E-08, 3.3733E-08, 3.6487E-08, 3.9353E-08,
     C 4.2660E-08, 4.6385E-08, 4.9955E-08, 5.5313E-08, 6.0923E-08,
     C 6.8948E-08, 7.3649E-08, 8.2602E-08, 9.2212E-08, 9.9080E-08/
      DATA S1051/
     C 1.1319E-07, 1.1790E-07, 1.2941E-07, 1.3199E-07, 1.3914E-07,
     C 1.4843E-07, 1.5300E-07, 1.6419E-07, 1.7095E-07, 1.6988E-07,
     C 1.6494E-07, 1.6327E-07, 1.6067E-07, 1.6909E-07, 1.7118E-07,
     C 1.8106E-07, 1.9857E-07, 2.1696E-07, 2.3385E-07, 2.2776E-07,
     C 2.1402E-07, 1.9882E-07, 1.7362E-07, 1.4308E-07, 1.1158E-07,
     C 8.8781E-08, 6.8689E-08, 5.2062E-08, 4.0427E-08, 3.2669E-08,
     C 2.7354E-08, 2.3200E-08, 2.0580E-08, 1.8676E-08, 1.7329E-08,
     C 1.6621E-08, 1.6433E-08, 1.6953E-08, 1.7134E-08, 1.7948E-08,
     C 1.9107E-08, 1.9875E-08, 2.1416E-08, 2.1556E-08, 2.2265E-08,
     C 2.2171E-08, 2.2534E-08, 2.3029E-08, 2.2828E-08, 2.3143E-08/
      DATA S1101/
     C 2.2965E-08, 2.2223E-08, 2.1108E-08, 2.0265E-08, 1.9516E-08,
     C 1.9941E-08, 2.0312E-08, 2.1080E-08, 2.2611E-08, 2.4210E-08,
     C 2.6069E-08, 2.5097E-08, 2.3318E-08, 2.1543E-08, 1.8942E-08,
     C 1.5960E-08, 1.2386E-08, 9.9340E-09, 7.7502E-09, 5.9462E-09,
     C 4.5113E-09, 3.5523E-09, 2.8844E-09, 2.3394E-09, 1.9584E-09,
     C 1.6749E-09, 1.4624E-09, 1.2809E-09, 1.1359E-09, 1.0087E-09,
     C 9.0166E-10, 8.1079E-10, 7.2219E-10, 6.4922E-10, 5.8803E-10,
     C 5.3290E-10, 4.8590E-10, 4.4111E-10, 4.0184E-10, 3.6644E-10,
     C 3.3529E-10, 3.0789E-10, 2.8286E-10, 2.6089E-10, 2.4125E-10,
     C 2.2355E-10, 2.0783E-10, 1.9370E-10, 1.8088E-10, 1.6948E-10/
      DATA S1151/
     C 1.5929E-10, 1.5013E-10, 1.4193E-10, 1.3470E-10, 1.2841E-10,
     C 1.2307E-10, 1.1865E-10, 1.1502E-10, 1.1243E-10, 1.1099E-10,
     C 1.1066E-10, 1.1216E-10, 1.1529E-10, 1.2171E-10, 1.3128E-10,
     C 1.4153E-10, 1.5962E-10, 1.8048E-10, 2.0936E-10, 2.3165E-10,
     C 2.5746E-10, 2.9600E-10, 3.3707E-10, 3.5267E-10, 3.5953E-10,
     C 3.6822E-10, 3.8363E-10, 3.8286E-10, 3.5883E-10, 3.6154E-10,
     C 3.6653E-10, 3.8507E-10, 4.0250E-10, 4.4435E-10, 4.9889E-10,
     C 5.6932E-10, 6.3599E-10, 7.0281E-10, 7.5777E-10, 8.1279E-10,
     C 8.8910E-10, 9.3400E-10, 1.0076E-09, 1.0945E-09, 1.1898E-09,
     C 1.3108E-09, 1.4725E-09, 1.7028E-09, 1.9619E-09, 2.3527E-09/
      DATA S1201/
     C 2.6488E-09, 3.0327E-09, 3.4396E-09, 3.8797E-09, 4.4115E-09,
     C 4.6853E-09, 4.9553E-09, 4.9551E-09, 5.1062E-09, 5.0996E-09,
     C 5.1119E-09, 5.2283E-09, 5.8297E-09, 6.3439E-09, 6.2675E-09,
     C 6.3296E-09, 6.5173E-09, 7.1685E-09, 7.0528E-09, 6.8856E-09,
     C 7.3182E-09, 7.6990E-09, 8.3461E-09, 8.1946E-09, 7.7153E-09,
     C 7.2411E-09, 6.4511E-09, 5.7336E-09, 4.6105E-09, 3.6962E-09,
     C 2.9944E-09, 2.4317E-09, 1.9399E-09, 1.5331E-09, 1.2633E-09,
     C 1.0613E-09, 9.0136E-10, 7.9313E-10, 7.1543E-10, 6.6485E-10,
     C 6.4225E-10, 6.3980E-10, 6.4598E-10, 6.7428E-10, 7.0270E-10,
     C 7.4694E-10, 7.7946E-10, 7.9395E-10, 7.8716E-10, 7.6933E-10/
      DATA S1251/
     C 7.6220E-10, 7.4825E-10, 7.4805E-10, 7.6511E-10, 7.6492E-10,
     C 7.4103E-10, 7.1979E-10, 7.1686E-10, 7.3403E-10, 7.1142E-10,
     C 7.0212E-10, 7.1548E-10, 7.5253E-10, 8.0444E-10, 8.2378E-10,
     C 7.8004E-10, 7.1712E-10, 6.4978E-10, 5.7573E-10, 4.8675E-10,
     C 3.7945E-10, 3.0118E-10, 2.4241E-10, 1.9100E-10, 1.4816E-10,
     C 1.1567E-10, 9.4183E-11, 7.7660E-11, 6.5270E-11, 5.6616E-11,
     C 4.9576E-11, 4.4137E-11, 3.9459E-11, 3.5759E-11, 3.2478E-11,
     C 2.9419E-11, 2.6703E-11, 2.4365E-11, 2.2412E-11, 2.0606E-11,
     C 1.9067E-11, 1.7800E-11, 1.6695E-11, 1.5729E-11, 1.4887E-11,
     C 1.4135E-11, 1.3519E-11, 1.2992E-11, 1.2563E-11, 1.2223E-11/
      DATA S1301/
     C 1.1962E-11, 1.1775E-11, 1.1657E-11, 1.1605E-11, 1.1619E-11,
     C 1.1697E-11, 1.1839E-11, 1.2046E-11, 1.2319E-11, 1.2659E-11,
     C 1.3070E-11, 1.3553E-11, 1.4113E-11, 1.4754E-11, 1.5480E-11,
     C 1.6298E-11, 1.7214E-11, 1.8236E-11, 1.9372E-11, 2.0635E-11,
     C 2.2036E-11, 2.3590E-11, 2.5317E-11, 2.7242E-11, 2.9400E-11,
     C 3.1849E-11, 3.4654E-11, 3.7923E-11, 4.1695E-11, 4.6055E-11,
     C 5.0940E-11, 5.5624E-11, 6.0667E-11, 6.6261E-11, 7.2692E-11,
     C 7.9711E-11, 8.7976E-11, 9.6884E-11, 1.0775E-10, 1.2093E-10,
     C 1.3531E-10, 1.5404E-10, 1.7315E-10, 1.9862E-10, 2.3341E-10,
     C 2.7014E-10, 3.1716E-10, 3.6957E-10, 4.3233E-10, 5.2566E-10/
      DATA S1351/
     C 6.2251E-10, 7.2149E-10, 8.3958E-10, 9.5931E-10, 1.1388E-09,
     C 1.2973E-09, 1.4442E-09, 1.5638E-09, 1.6974E-09, 1.8489E-09,
     C 1.9830E-09, 2.1720E-09, 2.3662E-09, 2.6987E-09, 3.1697E-09,
     C 3.6907E-09, 4.2625E-09, 4.7946E-09, 5.3848E-09, 6.0897E-09,
     C 6.4730E-09, 7.1483E-09, 7.7432E-09, 8.0851E-09, 8.5013E-09,
     C 8.5909E-09, 9.1890E-09, 9.3124E-09, 9.5936E-09, 9.8787E-09,
     C 9.9036E-09, 9.6712E-09, 9.2036E-09, 9.0466E-09, 8.9380E-09,
     C 9.1815E-09, 9.5092E-09, 1.0027E-08, 1.0876E-08, 1.1744E-08,
     C 1.1853E-08, 1.1296E-08, 1.0134E-08, 8.8245E-09, 7.3930E-09,
     C 5.7150E-09, 4.4884E-09, 3.4027E-09, 2.6054E-09, 2.0790E-09/
      DATA S1401/
     C 1.7267E-09, 1.4724E-09, 1.2722E-09, 1.1234E-09, 1.0186E-09,
     C 9.4680E-10, 8.8854E-10, 8.5127E-10, 8.3157E-10, 8.2226E-10,
     C 8.3395E-10, 8.3294E-10, 8.4725E-10, 8.8814E-10, 9.3697E-10,
     C 1.0112E-09, 1.0412E-09, 1.0948E-09, 1.1810E-09, 1.2267E-09,
     C 1.3690E-09, 1.4512E-09, 1.5568E-09, 1.6552E-09, 1.7321E-09,
     C 1.8797E-09, 1.9210E-09, 1.9686E-09, 1.9917E-09, 1.9357E-09,
     C 1.8486E-09, 1.7575E-09, 1.7113E-09, 1.7163E-09, 1.7623E-09,
     C 1.8536E-09, 1.9765E-09, 2.1334E-09, 2.3237E-09, 2.3259E-09,
     C 2.1833E-09, 1.9785E-09, 1.7308E-09, 1.4596E-09, 1.1198E-09,
     C 8.7375E-10, 6.5381E-10, 4.8677E-10, 3.6756E-10, 2.9155E-10/
      DATA S1451/
     C 2.3735E-10, 1.9590E-10, 1.6638E-10, 1.4549E-10, 1.2947E-10,
     C 1.1511E-10, 1.0548E-10, 9.6511E-11, 9.0469E-11, 8.5170E-11,
     C 7.7804E-11, 7.1971E-11, 6.6213E-11, 6.1063E-11, 5.5881E-11,
     C 5.0508E-11, 4.5932E-11, 4.1997E-11, 3.7672E-11, 3.3972E-11,
     C 3.0318E-11, 2.6769E-11, 2.3874E-11, 2.1336E-11, 1.9073E-11,
     C 1.7313E-11, 1.5904E-11, 1.4684E-11, 1.3698E-11, 1.2873E-11,
     C 1.2175E-11, 1.1542E-11, 1.1024E-11, 1.0602E-11, 1.0267E-11,
     C 1.0012E-11, 9.8379E-12, 9.7482E-12, 9.7564E-12, 9.8613E-12,
     C 1.0092E-11, 1.0418E-11, 1.0868E-11, 1.1585E-11, 1.2351E-11,
     C 1.3372E-11, 1.4841E-11, 1.6457E-11, 1.8681E-11, 2.0550E-11/
      DATA S1501/
     C 2.2912E-11, 2.5958E-11, 2.9137E-11, 3.2368E-11, 3.4848E-11,
     C 3.8462E-11, 4.2190E-11, 4.5629E-11, 4.9022E-11, 5.4232E-11,
     C 6.1900E-11, 7.1953E-11, 8.5368E-11, 9.9699E-11, 1.1734E-10,
     C 1.4185E-10, 1.7017E-10, 1.9813E-10, 2.3859E-10, 2.7304E-10,
     C 3.0971E-10, 3.5129E-10, 3.9405E-10, 4.5194E-10, 4.8932E-10,
     C 5.2436E-10, 5.4098E-10, 5.5542E-10, 5.7794E-10, 5.6992E-10,
     C 5.8790E-10, 6.1526E-10, 6.8034E-10, 6.7956E-10, 6.6864E-10,
     C 6.9329E-10, 7.2971E-10, 7.6546E-10, 7.5078E-10, 7.8406E-10,
     C 8.3896E-10, 9.0111E-10, 9.1994E-10, 8.7189E-10, 8.1426E-10,
     C 7.3097E-10, 6.3357E-10, 5.1371E-10, 4.0936E-10, 3.2918E-10/
      DATA S1551/
     C 2.6255E-10, 2.0724E-10, 1.6879E-10, 1.4165E-10, 1.1989E-10,
     C 1.0125E-10, 8.9629E-11, 7.8458E-11, 6.8826E-11, 6.0935E-11,
     C 5.5208E-11, 5.2262E-11, 5.0260E-11, 4.8457E-11, 4.7888E-11,
     C 4.8032E-11, 5.0838E-11, 5.4668E-11, 5.5790E-11, 6.0056E-11,
     C 6.3811E-11, 6.8848E-11, 7.4590E-11, 7.8249E-11, 8.3371E-11,
     C 8.3641E-11, 8.6591E-11, 8.9599E-11, 9.3487E-11, 1.0066E-10,
     C 1.0765E-10, 1.0851E-10, 1.0619E-10, 1.0557E-10, 1.0460E-10,
     C 1.0796E-10, 1.0523E-10, 1.0674E-10, 1.1261E-10, 1.1431E-10,
     C 1.1408E-10, 1.0901E-10, 9.9105E-11, 8.8077E-11, 6.9928E-11,
     C 5.4595E-11, 4.5401E-11, 3.6313E-11, 2.6986E-11, 1.9463E-11/
      DATA S1601/
     C 1.4577E-11, 1.1583E-11, 9.5492E-12, 8.0770E-12, 6.9642E-12,
     C 6.0966E-12, 5.4046E-12, 4.8431E-12, 4.3815E-12, 3.9987E-12,
     C 3.6790E-12, 3.4113E-12, 3.1868E-12, 2.9992E-12, 2.8434E-12,
     C 2.7153E-12, 2.6120E-12, 2.5311E-12, 2.4705E-12, 2.4290E-12,
     C 2.4053E-12, 2.3988E-12, 2.4087E-12, 2.4349E-12, 2.4771E-12,
     C 2.5355E-12, 2.6103E-12, 2.7019E-12, 2.8110E-12, 2.9383E-12,
     C 3.0848E-12, 3.2518E-12, 3.4405E-12, 3.6527E-12, 3.8902E-12,
     C 4.1555E-12, 4.4510E-12, 4.7801E-12, 5.1462E-12, 5.5539E-12,
     C 6.0086E-12, 6.5171E-12, 7.0884E-12, 7.7357E-12, 8.4831E-12,
     C 9.3096E-12, 1.0282E-11, 1.1407E-11, 1.2690E-11, 1.4148E-11/
      DATA S1651/
     C 1.5888E-11, 1.7992E-11, 2.0523E-11, 2.3342E-11, 2.6578E-11,
     C 3.0909E-11, 3.6228E-11, 4.2053E-11, 4.9059E-11, 5.9273E-11,
     C 7.0166E-11, 8.2298E-11, 9.7071E-11, 1.1673E-10, 1.4010E-10,
     C 1.6621E-10, 2.0127E-10, 2.3586E-10, 2.7050E-10, 3.0950E-10,
     C 3.6584E-10, 4.1278E-10, 4.6591E-10, 5.2220E-10, 5.5246E-10,
     C 6.1500E-10, 6.5878E-10, 7.1167E-10, 7.9372E-10, 8.6975E-10,
     C 9.6459E-10, 9.7368E-10, 9.8142E-10, 1.0202E-09, 1.0200E-09,
     C 1.0356E-09, 1.0092E-09, 1.0269E-09, 1.0366E-09, 1.0490E-09,
     C 1.0717E-09, 1.0792E-09, 1.1016E-09, 1.0849E-09, 1.0929E-09,
     C 1.0971E-09, 1.0969E-09, 1.0460E-09, 9.2026E-10, 8.1113E-10/
      DATA S1701/
     C 6.8635E-10, 5.5369E-10, 4.2908E-10, 3.3384E-10, 2.6480E-10,
     C 2.0810E-10, 1.6915E-10, 1.4051E-10, 1.1867E-10, 1.0158E-10,
     C 8.8990E-11, 7.9175E-11, 7.0440E-11, 6.3453E-11, 5.7009E-11,
     C 5.1662E-11, 4.7219E-11, 4.3454E-11, 4.0229E-11, 3.7689E-11,
     C 3.6567E-11, 3.5865E-11, 3.5955E-11, 3.5928E-11, 3.6298E-11,
     C 3.7629E-11, 3.9300E-11, 4.1829E-11, 4.4806E-11, 5.0534E-11,
     C 5.6672E-11, 6.2138E-11, 6.8678E-11, 7.6111E-11, 8.4591E-11,
     C 9.2634E-11, 9.8085E-11, 1.0830E-10, 1.1949E-10, 1.2511E-10,
     C 1.3394E-10, 1.3505E-10, 1.4342E-10, 1.4874E-10, 1.4920E-10,
     C 1.5872E-10, 1.5972E-10, 1.5821E-10, 1.5425E-10, 1.4937E-10/
      DATA S1751/
     C 1.5089E-10, 1.5521E-10, 1.6325E-10, 1.6924E-10, 1.8265E-10,
     C 1.9612E-10, 2.0176E-10, 1.9359E-10, 1.7085E-10, 1.5197E-10,
     C 1.2646E-10, 9.8552E-11, 7.4530E-11, 5.5052E-11, 4.2315E-11,
     C 3.2736E-11, 2.6171E-11, 2.1909E-11, 1.8286E-11, 1.5752E-11,
     C 1.3859E-11, 1.2288E-11, 1.1002E-11, 9.7534E-12, 8.8412E-12,
     C 8.0169E-12, 7.2855E-12, 6.8734E-12, 6.4121E-12, 6.1471E-12,
     C 5.7780E-12, 5.3478E-12, 4.9652E-12, 4.4043E-12, 3.9862E-12,
     C 3.4684E-12, 2.9681E-12, 2.5791E-12, 2.2339E-12, 1.9247E-12,
     C 1.6849E-12, 1.4863E-12, 1.3291E-12, 1.2021E-12, 1.0947E-12,
     C 1.0015E-12, 9.1935E-13, 8.4612E-13, 7.8036E-13, 7.2100E-13/
      DATA S1801/
     C 6.6718E-13, 6.1821E-13, 5.7353E-13, 5.3269E-13, 4.9526E-13,
     C 4.6093E-13, 4.2937E-13, 4.0034E-13, 3.7361E-13, 3.4895E-13,
     C 3.2621E-13, 3.0520E-13, 2.8578E-13, 2.6782E-13, 2.5120E-13,
     C 2.3581E-13, 2.2154E-13, 2.0832E-13, 1.9605E-13, 1.8466E-13,
     C 1.7408E-13, 1.6425E-13, 1.5511E-13, 1.4661E-13, 1.3869E-13,
     C 1.3131E-13, 1.2444E-13, 1.1803E-13, 1.1205E-13, 1.0646E-13,
     C 1.0124E-13, 9.6358E-14, 9.1789E-14, 8.7509E-14, 8.3498E-14,
     C 7.9735E-14, 7.6202E-14, 7.2882E-14, 6.9760E-14, 6.6822E-14,
     C 6.4053E-14, 6.1442E-14, 5.8978E-14, 5.6650E-14, 5.4448E-14,
     C 5.2364E-14, 5.0389E-14, 4.8516E-14, 4.6738E-14, 4.5048E-14/
      DATA S1851/
     C 4.3441E-14, 4.1911E-14, 4.0453E-14, 3.9063E-14, 3.7735E-14,
     C 3.6467E-14, 3.5254E-14, 3.4093E-14, 3.2980E-14, 3.1914E-14,
     C 3.0891E-14, 2.9909E-14, 2.8965E-14, 2.8058E-14, 2.7185E-14,
     C 2.6344E-14, 2.5535E-14, 2.4755E-14, 2.4002E-14, 2.3276E-14,
     C 2.2576E-14, 2.1899E-14, 2.1245E-14, 2.0613E-14, 2.0002E-14,
     C 1.9411E-14, 1.8839E-14, 1.8285E-14, 1.7749E-14, 1.7230E-14,
     C 1.6727E-14, 1.6240E-14, 1.5768E-14, 1.5310E-14, 1.4867E-14,
     C 1.4436E-14, 1.4019E-14, 1.3614E-14, 1.3221E-14, 1.2840E-14,
     C 1.2471E-14, 1.2112E-14, 1.1764E-14, 1.1425E-14, 1.1097E-14,
     C 1.0779E-14, 1.0469E-14, 1.0169E-14, 9.8775E-15, 9.5943E-15/
      DATA S1901/
     C 9.3193E-15, 9.0522E-15, 8.7928E-15, 8.5409E-15, 8.2962E-15,
     C 8.0586E-15, 7.8278E-15, 7.6036E-15, 7.3858E-15, 7.1742E-15,
     C 6.9687E-15, 6.7691E-15, 6.5752E-15, 6.3868E-15, 6.2038E-15,
     C 6.0260E-15, 5.8533E-15, 5.6856E-15, 5.5226E-15, 5.3642E-15,
     C 5.2104E-15, 5.0610E-15, 4.9158E-15, 4.7748E-15, 4.6378E-15,
     C 4.5047E-15, 4.3753E-15, 4.2497E-15, 4.1277E-15, 4.0091E-15,
     C 3.8939E-15, 3.7820E-15, 3.6733E-15, 3.5677E-15, 3.4651E-15,
     C 3.3655E-15, 3.2686E-15, 3.1746E-15, 3.0832E-15, 2.9944E-15,
     C 2.9082E-15, 2.8244E-15, 2.7431E-15, 2.6640E-15, 2.5872E-15,
     C 2.5126E-15, 2.4401E-15, 2.3697E-15, 2.3014E-15, 2.2349E-15/
      DATA S1951/
     C 2.1704E-15, 2.1077E-15, 2.0468E-15, 1.9877E-15, 1.9302E-15,
     C 1.8744E-15, 1.8202E-15, 1.7675E-15, 1.7164E-15, 1.6667E-15,
     C 1.6184E-15, 1.5716E-15, 1.5260E-15, 1.4818E-15, 1.4389E-15,
     C 1.3971E-15, 1.3566E-15, 1.3172E-15, 1.2790E-15, 1.2419E-15,
     C 1.2058E-15, 1.1708E-15, 1.1368E-15, 1.1037E-15, 1.0716E-15,
     C 1.0405E-15, 1.0102E-15, 9.8079E-16, 9.5224E-16, 9.2451E-16,
     C 8.9758E-16, 8.7142E-16, 8.4602E-16, 8.2136E-16, 7.9740E-16,
     C 7.7414E-16, 7.5154E-16, 7.2961E-16, 7.0830E-16, 6.8761E-16,
     C 6.6752E-16, 6.4801E-16, 6.2906E-16, 6.1066E-16, 5.9280E-16,
     C 5.7545E-16, 5.5860E-16, 5.4224E-16, 5.2636E-16, 5.1094E-16/
      DATA S2001/
     C 4.9596E-16/
C
      END
      BLOCK DATA BFH2O
C>    BLOCK DATA
C               06/28/82
C               UNITS OF (CM**3/MOL)*1.E-20
      COMMON /FH2O/ V1,V2,DV,NPT,F0000(2),
     1      F0001(50),F0051(50),F0101(50),F0151(50),F0201(50),F0251(50),
     2      F0301(50),F0351(50),F0401(50),F0451(50),F0501(50),F0551(50),
     3      F0601(50),F0651(50),F0701(50),F0751(50),F0801(50),F0851(50),
     4      F0901(50),F0951(50),F1001(50),F1051(50),F1101(50),F1151(50),
     5      F1201(50),F1251(50),F1301(50),F1351(50),F1401(50),F1451(50),
     6      F1501(50),F1551(50),F1601(50),F1651(50),F1701(50),F1751(50),
     7      F1801(50),F1851(50),F1901(50),F1951(50),F2001(1)
C
C
       DATA V1,V2,DV,NPT /
     1      -20.0,     20000.0,       10.0,  2003/
C
C
      DATA F0000/ 1.2859E-02, 1.1715E-02/
      DATA F0001/
     X 1.1038E-02, 1.1715E-02, 1.2859E-02, 1.5326E-02, 1.6999E-02,
     X 1.8321E-02, 1.9402E-02, 1.9570E-02, 1.9432E-02, 1.7572E-02,
     X 1.6760E-02, 1.5480E-02, 1.3984E-02, 1.2266E-02, 1.0467E-02,
     X 9.4526E-03, 8.0485E-03, 6.9484E-03, 6.1416E-03, 5.0941E-03,
     X 4.4836E-03, 3.8133E-03, 3.4608E-03, 3.1487E-03, 2.4555E-03,
     X 2.0977E-03, 1.7266E-03, 1.4920E-03, 1.2709E-03, 9.8081E-04,
     X 8.5063E-04, 6.8822E-04, 5.3809E-04, 4.4679E-04, 3.3774E-04,
     X 2.7979E-04, 2.1047E-04, 1.6511E-04, 1.2993E-04, 9.3033E-05,
     X 7.4360E-05, 5.6428E-05, 4.5442E-05, 3.4575E-05, 2.7903E-05,
     X 2.1374E-05, 1.6075E-05, 1.3022E-05, 1.0962E-05, 8.5959E-06/
      DATA F0051/
     X 6.9125E-06, 5.3808E-06, 4.3586E-06, 3.6394E-06, 2.9552E-06,
     X 2.3547E-06, 1.8463E-06, 1.6036E-06, 1.3483E-06, 1.1968E-06,
     X 1.0333E-06, 8.4484E-07, 6.7195E-07, 5.0947E-07, 4.2343E-07,
     X 3.4453E-07, 2.7830E-07, 2.3063E-07, 1.9951E-07, 1.7087E-07,
     X 1.4393E-07, 1.2575E-07, 1.0750E-07, 8.2325E-08, 5.7524E-08,
     X 4.4482E-08, 3.8106E-08, 3.4315E-08, 2.9422E-08, 2.5069E-08,
     X 2.2402E-08, 1.9349E-08, 1.6152E-08, 1.2208E-08, 8.9660E-09,
     X 7.1322E-09, 6.1028E-09, 5.2938E-09, 4.5350E-09, 3.4977E-09,
     X 2.9511E-09, 2.4734E-09, 2.0508E-09, 1.8507E-09, 1.6373E-09,
     X 1.5171E-09, 1.3071E-09, 1.2462E-09, 1.2148E-09, 1.2590E-09/
      DATA F0101/
     X 1.3153E-09, 1.3301E-09, 1.4483E-09, 1.6944E-09, 2.0559E-09,
     X 2.2954E-09, 2.6221E-09, 3.2606E-09, 4.2392E-09, 5.2171E-09,
     X 6.2553E-09, 8.2548E-09, 9.5842E-09, 1.1280E-08, 1.3628E-08,
     X 1.7635E-08, 2.1576E-08, 2.4835E-08, 3.0014E-08, 3.8485E-08,
     X 4.7440E-08, 5.5202E-08, 7.0897E-08, 9.6578E-08, 1.3976E-07,
     X 1.8391E-07, 2.3207E-07, 2.9960E-07, 4.0408E-07, 5.9260E-07,
     X 7.8487E-07, 1.0947E-06, 1.4676E-06, 1.9325E-06, 2.6587E-06,
     X 3.4534E-06, 4.4376E-06, 5.8061E-06, 7.0141E-06, 8.4937E-06,
     X 1.0186E-05, 1.2034E-05, 1.3837E-05, 1.6595E-05, 1.9259E-05,
     X 2.1620E-05, 2.3681E-05, 2.7064E-05, 3.2510E-05, 3.5460E-05/
      DATA F0151/
     X 3.9109E-05, 4.2891E-05, 4.7757E-05, 5.0981E-05, 5.0527E-05,
     X 4.8618E-05, 4.4001E-05, 3.7982E-05, 3.2667E-05, 2.7794E-05,
     X 2.4910E-05, 2.4375E-05, 2.7316E-05, 3.2579E-05, 3.5499E-05,
     X 3.8010E-05, 4.1353E-05, 4.3323E-05, 4.3004E-05, 3.9790E-05,
     X 3.7718E-05, 3.6360E-05, 3.2386E-05, 2.7409E-05, 2.3626E-05,
     X 2.0631E-05, 1.8371E-05, 1.5445E-05, 1.2989E-05, 1.1098E-05,
     X 9.6552E-06, 8.0649E-06, 7.2365E-06, 5.9137E-06, 5.2759E-06,
     X 4.8860E-06, 4.1321E-06, 3.5918E-06, 2.7640E-06, 2.4892E-06,
     X 2.1018E-06, 1.7848E-06, 1.5855E-06, 1.3569E-06, 1.1986E-06,
     X 9.4693E-07, 7.4097E-07, 6.3443E-07, 4.8131E-07, 4.0942E-07/
      DATA F0201/
     X 3.3316E-07, 2.8488E-07, 2.3461E-07, 1.7397E-07, 1.4684E-07,
     X 1.0953E-07, 8.5396E-08, 6.9261E-08, 5.4001E-08, 4.5430E-08,
     X 3.2791E-08, 2.5995E-08, 2.0225E-08, 1.5710E-08, 1.3027E-08,
     X 1.0229E-08, 8.5277E-09, 6.5249E-09, 5.0117E-09, 3.9906E-09,
     X 3.2332E-09, 2.7847E-09, 2.4570E-09, 2.3359E-09, 2.0599E-09,
     X 1.8436E-09, 1.6559E-09, 1.4910E-09, 1.2794E-09, 9.8229E-10,
     X 8.0054E-10, 6.0769E-10, 4.5646E-10, 3.3111E-10, 2.4428E-10,
     X 1.8007E-10, 1.3291E-10, 9.7974E-11, 7.8271E-11, 6.3833E-11,
     X 5.4425E-11, 4.6471E-11, 4.0209E-11, 3.5227E-11, 3.1212E-11,
     X 2.8840E-11, 2.7762E-11, 2.7935E-11, 3.2012E-11, 3.9525E-11/
      DATA F0251/
     X 5.0303E-11, 6.8027E-11, 9.3954E-11, 1.2986E-10, 1.8478E-10,
     X 2.5331E-10, 3.4827E-10, 4.6968E-10, 6.2380E-10, 7.9106E-10,
     X 1.0026E-09, 1.2102E-09, 1.4146E-09, 1.6154E-09, 1.7510E-09,
     X 1.8575E-09, 1.8742E-09, 1.8700E-09, 1.8582E-09, 1.9657E-09,
     X 2.1204E-09, 2.0381E-09, 2.0122E-09, 2.0436E-09, 2.1213E-09,
     X 2.0742E-09, 1.9870E-09, 2.0465E-09, 2.1556E-09, 2.2222E-09,
     X 2.1977E-09, 2.1047E-09, 1.9334E-09, 1.7357E-09, 1.5754E-09,
     X 1.4398E-09, 1.4018E-09, 1.5459E-09, 1.7576E-09, 2.1645E-09,
     X 2.9480E-09, 4.4439E-09, 5.8341E-09, 8.0757E-09, 1.1658E-08,
     X 1.6793E-08, 2.2694E-08, 2.9468E-08, 3.9278E-08, 5.2145E-08/
      DATA F0301/
     X 6.4378E-08, 7.7947E-08, 8.5321E-08, 9.7848E-08, 1.0999E-07,
     X 1.1489E-07, 1.2082E-07, 1.2822E-07, 1.4053E-07, 1.5238E-07,
     X 1.5454E-07, 1.5018E-07, 1.4048E-07, 1.2359E-07, 1.0858E-07,
     X 9.3486E-08, 8.1638E-08, 7.7690E-08, 8.4625E-08, 1.0114E-07,
     X 1.1430E-07, 1.2263E-07, 1.3084E-07, 1.3380E-07, 1.3573E-07,
     X 1.3441E-07, 1.2962E-07, 1.2638E-07, 1.1934E-07, 1.1371E-07,
     X 1.0871E-07, 9.8843E-08, 9.1877E-08, 9.1050E-08, 9.3213E-08,
     X 9.2929E-08, 1.0155E-07, 1.1263E-07, 1.2370E-07, 1.3636E-07,
     X 1.5400E-07, 1.7656E-07, 2.1329E-07, 2.3045E-07, 2.5811E-07,
     X 2.9261E-07, 3.4259E-07, 4.0770E-07, 4.8771E-07, 5.8081E-07/
      DATA F0351/
     X 7.2895E-07, 8.7482E-07, 1.0795E-06, 1.3384E-06, 1.7208E-06,
     X 2.0677E-06, 2.5294E-06, 3.1123E-06, 3.7900E-06, 4.7752E-06,
     X 5.6891E-06, 6.6261E-06, 7.6246E-06, 8.7730E-06, 9.6672E-06,
     X 1.0980E-05, 1.1287E-05, 1.1670E-05, 1.1635E-05, 1.1768E-05,
     X 1.2039E-05, 1.2253E-05, 1.3294E-05, 1.4005E-05, 1.3854E-05,
     X 1.3420E-05, 1.3003E-05, 1.2645E-05, 1.1715E-05, 1.1258E-05,
     X 1.1516E-05, 1.2494E-05, 1.3655E-05, 1.4931E-05, 1.4649E-05,
     X 1.3857E-05, 1.3120E-05, 1.1791E-05, 1.0637E-05, 8.2760E-06,
     X 6.5821E-06, 5.1959E-06, 4.0158E-06, 3.0131E-06, 2.0462E-06,
     X 1.4853E-06, 1.0365E-06, 7.3938E-07, 4.9752E-07, 3.4148E-07/
      DATA F0401/
     X 2.4992E-07, 1.8363E-07, 1.4591E-07, 1.1380E-07, 9.0588E-08,
     X 7.3697E-08, 6.0252E-08, 5.1868E-08, 4.2660E-08, 3.6163E-08,
     X 3.2512E-08, 2.9258E-08, 2.4238E-08, 2.1209E-08, 1.6362E-08,
     X 1.3871E-08, 1.2355E-08, 9.6940E-09, 7.7735E-09, 6.2278E-09,
     X 5.2282E-09, 4.3799E-09, 3.5545E-09, 2.7527E-09, 2.0950E-09,
     X 1.6344E-09, 1.2689E-09, 1.0403E-09, 8.4880E-10, 6.3461E-10,
     X 4.7657E-10, 3.5220E-10, 2.7879E-10, 2.3021E-10, 1.6167E-10,
     X 1.1732E-10, 8.9206E-11, 7.0596E-11, 5.8310E-11, 4.4084E-11,
     X 3.1534E-11, 2.5068E-11, 2.2088E-11, 2.2579E-11, 2.2637E-11,
     X 2.5705E-11, 3.2415E-11, 4.6116E-11, 6.5346E-11, 9.4842E-11/
      DATA F0451/
     X 1.2809E-10, 1.8211E-10, 2.4052E-10, 3.0270E-10, 3.5531E-10,
     X 4.2402E-10, 4.6730E-10, 4.7942E-10, 4.6813E-10, 4.5997E-10,
     X 4.5788E-10, 4.0311E-10, 3.7367E-10, 3.3149E-10, 2.9281E-10,
     X 2.5231E-10, 2.1152E-10, 1.9799E-10, 1.8636E-10, 1.9085E-10,
     X 2.0786E-10, 2.2464E-10, 2.3785E-10, 2.5684E-10, 2.7499E-10,
     X 2.6962E-10, 2.6378E-10, 2.6297E-10, 2.6903E-10, 2.7035E-10,
     X 2.5394E-10, 2.5655E-10, 2.7184E-10, 2.9013E-10, 3.0585E-10,
     X 3.0791E-10, 3.1667E-10, 3.4343E-10, 3.7365E-10, 4.0269E-10,
     X 4.7260E-10, 5.6584E-10, 6.9791E-10, 8.6569E-10, 1.0393E-09,
     X 1.2067E-09, 1.5047E-09, 1.8583E-09, 2.2357E-09, 2.6498E-09/
      DATA F0501/
     X 3.2483E-09, 3.9927E-09, 4.6618E-09, 5.5555E-09, 6.6609E-09,
     X 8.2139E-09, 1.0285E-08, 1.3919E-08, 1.8786E-08, 2.5150E-08,
     X 3.3130E-08, 4.5442E-08, 6.3370E-08, 9.0628E-08, 1.2118E-07,
     X 1.5927E-07, 2.1358E-07, 2.7825E-07, 3.7671E-07, 4.4894E-07,
     X 5.4442E-07, 6.2240E-07, 7.3004E-07, 8.3384E-07, 8.7933E-07,
     X 8.8080E-07, 8.6939E-07, 8.6541E-07, 8.2055E-07, 7.7278E-07,
     X 7.5989E-07, 8.6909E-07, 9.7945E-07, 1.0394E-06, 1.0646E-06,
     X 1.1509E-06, 1.2017E-06, 1.1915E-06, 1.1259E-06, 1.1549E-06,
     X 1.1938E-06, 1.2356E-06, 1.2404E-06, 1.1716E-06, 1.1149E-06,
     X 1.0073E-06, 8.9845E-07, 7.6639E-07, 6.1517E-07, 5.0887E-07/
      DATA F0551/
     X 4.1269E-07, 3.2474E-07, 2.5698E-07, 1.8893E-07, 1.4009E-07,
     X 1.0340E-07, 7.7724E-08, 5.7302E-08, 4.2178E-08, 2.9603E-08,
     X 2.1945E-08, 1.6301E-08, 1.2806E-08, 1.0048E-08, 7.8970E-09,
     X 6.1133E-09, 4.9054E-09, 4.1985E-09, 3.6944E-09, 3.2586E-09,
     X 2.7362E-09, 2.3647E-09, 2.1249E-09, 1.8172E-09, 1.6224E-09,
     X 1.5158E-09, 1.2361E-09, 1.0682E-09, 9.2312E-10, 7.9220E-10,
     X 6.8174E-10, 5.6147E-10, 4.8268E-10, 4.1534E-10, 3.3106E-10,
     X 2.8275E-10, 2.4584E-10, 2.0742E-10, 1.7840E-10, 1.4664E-10,
     X 1.2390E-10, 1.0497E-10, 8.5038E-11, 6.7008E-11, 5.6355E-11,
     X 4.3323E-11, 3.6914E-11, 3.2262E-11, 3.0749E-11, 3.0318E-11/
      DATA F0601/
     X 2.9447E-11, 2.9918E-11, 3.0668E-11, 3.1315E-11, 3.0329E-11,
     X 2.8259E-11, 2.6065E-11, 2.3578E-11, 2.0469E-11, 1.6908E-11,
     X 1.4912E-11, 1.1867E-11, 9.9730E-12, 8.1014E-12, 6.7528E-12,
     X 6.3133E-12, 5.8599E-12, 6.0145E-12, 6.5105E-12, 7.0537E-12,
     X 7.4973E-12, 7.8519E-12, 8.5039E-12, 9.1995E-12, 1.0694E-11,
     X 1.1659E-11, 1.2685E-11, 1.3087E-11, 1.3222E-11, 1.2634E-11,
     X 1.1077E-11, 9.6259E-12, 8.3202E-12, 7.4857E-12, 6.8069E-12,
     X 6.7496E-12, 7.3116E-12, 8.0171E-12, 8.6394E-12, 9.2659E-12,
     X 1.0048E-11, 1.0941E-11, 1.2226E-11, 1.3058E-11, 1.5193E-11,
     X 1.8923E-11, 2.3334E-11, 2.8787E-11, 3.6693E-11, 4.8295E-11/
      DATA F0651/
     X 6.4260E-11, 8.8269E-11, 1.1865E-10, 1.5961E-10, 2.0605E-10,
     X 2.7349E-10, 3.7193E-10, 4.8216E-10, 6.1966E-10, 7.7150E-10,
     X 1.0195E-09, 1.2859E-09, 1.6535E-09, 2.0316E-09, 2.3913E-09,
     X 3.0114E-09, 3.7495E-09, 4.6504E-09, 5.9145E-09, 7.6840E-09,
     X 1.0304E-08, 1.3010E-08, 1.6441E-08, 2.1475E-08, 2.5892E-08,
     X 2.9788E-08, 3.3820E-08, 4.0007E-08, 4.4888E-08, 4.5765E-08,
     X 4.6131E-08, 4.6239E-08, 4.4849E-08, 4.0729E-08, 3.6856E-08,
     X 3.6164E-08, 3.7606E-08, 4.1457E-08, 4.3750E-08, 5.1150E-08,
     X 5.6054E-08, 6.1586E-08, 6.4521E-08, 6.6494E-08, 6.9024E-08,
     X 6.8893E-08, 7.0901E-08, 6.9760E-08, 7.1485E-08, 7.0740E-08/
      DATA F0701/
     X 7.3764E-08, 7.6618E-08, 8.4182E-08, 9.3838E-08, 1.0761E-07,
     X 1.2851E-07, 1.4748E-07, 1.8407E-07, 2.2109E-07, 2.6392E-07,
     X 2.9887E-07, 3.4493E-07, 4.0336E-07, 4.3551E-07, 4.9231E-07,
     X 5.0728E-07, 5.3781E-07, 5.3285E-07, 5.4496E-07, 5.5707E-07,
     X 5.6944E-07, 6.1123E-07, 6.4317E-07, 6.4581E-07, 6.1999E-07,
     X 6.0191E-07, 5.7762E-07, 5.7241E-07, 5.7013E-07, 6.0160E-07,
     X 6.6905E-07, 7.4095E-07, 8.2121E-07, 8.0947E-07, 7.6145E-07,
     X 7.2193E-07, 6.3722E-07, 5.4316E-07, 4.2186E-07, 3.2528E-07,
     X 2.5207E-07, 1.8213E-07, 1.2658E-07, 8.6746E-08, 6.0216E-08,
     X 4.1122E-08, 2.8899E-08, 2.1740E-08, 1.7990E-08, 1.5593E-08/
      DATA F0751/
     X 1.3970E-08, 1.2238E-08, 1.0539E-08, 9.2386E-09, 7.8481E-09,
     X 6.8704E-09, 5.7615E-09, 5.0434E-09, 4.6886E-09, 4.3770E-09,
     X 3.9768E-09, 3.5202E-09, 3.1854E-09, 2.9009E-09, 2.5763E-09,
     X 2.2135E-09, 1.9455E-09, 1.6248E-09, 1.3368E-09, 1.0842E-09,
     X 8.4254E-10, 6.7414E-10, 5.4667E-10, 4.5005E-10, 3.4932E-10,
     X 2.6745E-10, 2.2053E-10, 1.8162E-10, 1.4935E-10, 1.1618E-10,
     X 9.1888E-11, 8.0672E-11, 6.8746E-11, 6.2668E-11, 5.5715E-11,
     X 4.5074E-11, 3.7669E-11, 3.2082E-11, 2.8085E-11, 2.4838E-11,
     X 1.9791E-11, 1.6964E-11, 1.3887E-11, 1.1179E-11, 9.7499E-12,
     X 7.8255E-12, 6.3698E-12, 5.3265E-12, 4.6588E-12, 4.4498E-12/
      DATA F0801/
     X 3.9984E-12, 3.7513E-12, 3.7176E-12, 3.9148E-12, 4.2702E-12,
     X 5.0090E-12, 6.5801E-12, 8.7787E-12, 1.2718E-11, 1.8375E-11,
     X 2.5304E-11, 3.5403E-11, 4.8842E-11, 6.4840E-11, 8.0911E-11,
     X 1.0136E-10, 1.2311E-10, 1.4203E-10, 1.5869E-10, 1.8093E-10,
     X 2.1370E-10, 2.5228E-10, 2.8816E-10, 3.4556E-10, 3.9860E-10,
     X 4.4350E-10, 4.7760E-10, 5.2357E-10, 6.0827E-10, 6.3635E-10,
     X 6.5886E-10, 6.8753E-10, 7.2349E-10, 7.2789E-10, 6.8232E-10,
     X 6.6081E-10, 6.4232E-10, 6.3485E-10, 6.4311E-10, 7.2235E-10,
     X 7.7263E-10, 8.1668E-10, 9.0324E-10, 9.7643E-10, 1.0535E-09,
     X 1.0195E-09, 1.0194E-09, 1.0156E-09, 9.6792E-10, 9.2725E-10/
      DATA F0851/
     X 8.7347E-10, 8.4484E-10, 8.2647E-10, 8.4363E-10, 9.1261E-10,
     X 1.0051E-09, 1.1511E-09, 1.4037E-09, 1.8066E-09, 2.4483E-09,
     X 3.2739E-09, 4.3194E-09, 5.6902E-09, 7.7924E-09, 9.7376E-09,
     X 1.2055E-08, 1.4303E-08, 1.6956E-08, 1.9542E-08, 2.2233E-08,
     X 2.5186E-08, 2.7777E-08, 2.8943E-08, 2.8873E-08, 2.9417E-08,
     X 2.7954E-08, 2.7524E-08, 2.7040E-08, 3.1254E-08, 3.6843E-08,
     X 3.7797E-08, 3.8713E-08, 4.0135E-08, 4.2824E-08, 4.3004E-08,
     X 4.0279E-08, 4.2781E-08, 4.5220E-08, 4.8948E-08, 5.0172E-08,
     X 4.8499E-08, 4.7182E-08, 4.2204E-08, 3.7701E-08, 3.0972E-08,
     X 2.4654E-08, 1.9543E-08, 1.4609E-08, 1.1171E-08, 8.3367E-09/
      DATA F0901/
     X 6.3791E-09, 5.0790E-09, 4.0655E-09, 3.3658E-09, 2.7882E-09,
     X 2.4749E-09, 2.2287E-09, 2.0217E-09, 1.8191E-09, 1.5897E-09,
     X 1.4191E-09, 1.2448E-09, 1.0884E-09, 9.3585E-10, 7.9429E-10,
     X 7.3214E-10, 6.5008E-10, 5.7549E-10, 5.4300E-10, 4.7251E-10,
     X 4.3451E-10, 3.8446E-10, 3.5589E-10, 3.4432E-10, 2.8209E-10,
     X 2.4620E-10, 2.1278E-10, 1.8406E-10, 1.6314E-10, 1.3261E-10,
     X 1.1696E-10, 9.6865E-11, 7.6814E-11, 6.6411E-11, 5.0903E-11,
     X 4.0827E-11, 3.0476E-11, 2.3230E-11, 1.7707E-11, 1.3548E-11,
     X 1.0719E-11, 9.3026E-12, 8.7967E-12, 8.3136E-12, 7.3918E-12,
     X 6.5293E-12, 5.9243E-12, 5.3595E-12, 3.5266E-12, 2.2571E-12/
      DATA F0951/
     X 1.6150E-12, 1.1413E-12, 8.4998E-13, 7.0803E-13, 5.1747E-13,
     X 4.0694E-13, 3.6528E-13, 3.3670E-13, 3.1341E-13, 2.9390E-13,
     X 2.8680E-13, 3.1283E-13, 3.7294E-13, 5.0194E-13, 6.7919E-13,
     X 1.0455E-12, 1.5230E-12, 2.3932E-12, 3.4231E-12, 5.0515E-12,
     X 7.3193E-12, 9.9406E-12, 1.2193E-11, 1.4742E-11, 1.9269E-11,
     X 2.1816E-11, 2.2750E-11, 2.2902E-11, 2.3888E-11, 2.4902E-11,
     X 2.2160E-11, 2.0381E-11, 1.9903E-11, 2.0086E-11, 1.9304E-11,
     X 2.0023E-11, 2.2244E-11, 2.5450E-11, 3.1228E-11, 3.4560E-11,
     X 3.6923E-11, 3.7486E-11, 3.8124E-11, 3.8317E-11, 3.4737E-11,
     X 3.3037E-11, 3.1724E-11, 2.9840E-11, 2.8301E-11, 2.5857E-11/
      DATA F1001/
     X 2.3708E-11, 1.9452E-11, 1.6232E-11, 1.5174E-11, 1.4206E-11,
     X 1.4408E-11, 1.5483E-11, 1.8642E-11, 2.3664E-11, 3.0181E-11,
     X 4.0160E-11, 5.2287E-11, 7.2754E-11, 1.0511E-10, 1.4531E-10,
     X 2.0998E-10, 2.6883E-10, 3.3082E-10, 4.2638E-10, 5.3132E-10,
     X 6.3617E-10, 7.1413E-10, 8.5953E-10, 9.9715E-10, 1.0796E-09,
     X 1.0978E-09, 1.1052E-09, 1.1095E-09, 1.0641E-09, 9.7881E-10,
     X 9.6590E-10, 1.0332E-09, 1.1974E-09, 1.3612E-09, 1.5829E-09,
     X 1.8655E-09, 2.1465E-09, 2.4779E-09, 2.7370E-09, 2.9915E-09,
     X 3.3037E-09, 3.6347E-09, 3.9587E-09, 4.4701E-09, 5.0122E-09,
     X 5.8044E-09, 6.1916E-09, 6.9613E-09, 7.7863E-09, 8.2820E-09/
      DATA F1051/
     X 9.4359E-09, 9.7387E-09, 1.0656E-08, 1.0746E-08, 1.1210E-08,
     X 1.1905E-08, 1.2194E-08, 1.3145E-08, 1.3738E-08, 1.3634E-08,
     X 1.3011E-08, 1.2511E-08, 1.1805E-08, 1.2159E-08, 1.2390E-08,
     X 1.3625E-08, 1.5678E-08, 1.7886E-08, 1.9933E-08, 1.9865E-08,
     X 1.9000E-08, 1.7812E-08, 1.5521E-08, 1.2593E-08, 9.5635E-09,
     X 7.2987E-09, 5.2489E-09, 3.5673E-09, 2.4206E-09, 1.6977E-09,
     X 1.2456E-09, 9.3744E-10, 7.8379E-10, 6.9960E-10, 6.6451E-10,
     X 6.8521E-10, 7.4234E-10, 8.6658E-10, 9.4972E-10, 1.0791E-09,
     X 1.2359E-09, 1.3363E-09, 1.5025E-09, 1.5368E-09, 1.6152E-09,
     X 1.6184E-09, 1.6557E-09, 1.7035E-09, 1.6916E-09, 1.7237E-09/
      DATA F1101/
     X 1.7175E-09, 1.6475E-09, 1.5335E-09, 1.4272E-09, 1.3282E-09,
     X 1.3459E-09, 1.4028E-09, 1.5192E-09, 1.7068E-09, 1.9085E-09,
     X 2.1318E-09, 2.1020E-09, 1.9942E-09, 1.8654E-09, 1.6391E-09,
     X 1.3552E-09, 1.0186E-09, 7.8540E-10, 5.7022E-10, 3.9247E-10,
     X 2.5441E-10, 1.6699E-10, 1.1132E-10, 6.8989E-11, 4.5255E-11,
     X 3.1106E-11, 2.3161E-11, 1.7618E-11, 1.4380E-11, 1.1601E-11,
     X 9.7148E-12, 8.4519E-12, 6.5392E-12, 5.4113E-12, 4.7624E-12,
     X 4.0617E-12, 3.6173E-12, 2.8608E-12, 2.2724E-12, 1.7436E-12,
     X 1.3424E-12, 1.0358E-12, 7.3064E-13, 5.4500E-13, 4.0551E-13,
     X 2.8642E-13, 2.1831E-13, 1.6860E-13, 1.2086E-13, 1.0150E-13/
      DATA F1151/
     X 9.3550E-14, 8.4105E-14, 7.3051E-14, 6.9796E-14, 7.9949E-14,
     X 1.0742E-13, 1.5639E-13, 2.1308E-13, 3.1226E-13, 4.6853E-13,
     X 6.6917E-13, 1.0088E-12, 1.4824E-12, 2.2763E-12, 3.3917E-12,
     X 4.4585E-12, 6.3187E-12, 8.4189E-12, 1.1302E-11, 1.3431E-11,
     X 1.5679E-11, 1.9044E-11, 2.2463E-11, 2.3605E-11, 2.3619E-11,
     X 2.3505E-11, 2.3805E-11, 2.2549E-11, 1.9304E-11, 1.8382E-11,
     X 1.7795E-11, 1.8439E-11, 1.9146E-11, 2.1966E-11, 2.6109E-11,
     X 3.1883E-11, 3.7872E-11, 4.3966E-11, 4.8789E-11, 5.3264E-11,
     X 5.9705E-11, 6.3744E-11, 7.0163E-11, 7.9114E-11, 8.8287E-11,
     X 9.9726E-11, 1.1498E-10, 1.3700E-10, 1.6145E-10, 1.9913E-10/
      DATA F1201/
     X 2.2778E-10, 2.6216E-10, 2.9770E-10, 3.3405E-10, 3.7821E-10,
     X 3.9552E-10, 4.1322E-10, 4.0293E-10, 4.0259E-10, 3.8853E-10,
     X 3.7842E-10, 3.8551E-10, 4.4618E-10, 5.0527E-10, 5.0695E-10,
     X 5.1216E-10, 5.1930E-10, 5.5794E-10, 5.3320E-10, 5.2008E-10,
     X 5.6888E-10, 6.1883E-10, 6.9006E-10, 6.9505E-10, 6.6768E-10,
     X 6.3290E-10, 5.6753E-10, 5.0327E-10, 3.9830E-10, 3.1147E-10,
     X 2.4416E-10, 1.8860E-10, 1.3908E-10, 9.9156E-11, 7.3779E-11,
     X 5.6048E-11, 4.2457E-11, 3.4505E-11, 2.9881E-11, 2.7865E-11,
     X 2.8471E-11, 3.1065E-11, 3.4204E-11, 3.9140E-11, 4.3606E-11,
     X 4.9075E-11, 5.3069E-11, 5.5236E-11, 5.5309E-11, 5.3832E-11/
      DATA F1251/
     X 5.3183E-11, 5.1783E-11, 5.2042E-11, 5.4422E-11, 5.5656E-11,
     X 5.4409E-11, 5.2659E-11, 5.1696E-11, 5.1726E-11, 4.9003E-11,
     X 4.9050E-11, 5.1700E-11, 5.6818E-11, 6.3129E-11, 6.6542E-11,
     X 6.4367E-11, 5.9908E-11, 5.4470E-11, 4.7903E-11, 3.9669E-11,
     X 2.9651E-11, 2.2286E-11, 1.6742E-11, 1.1827E-11, 7.7739E-12,
     X 4.8805E-12, 3.1747E-12, 2.0057E-12, 1.2550E-12, 8.7434E-13,
     X 6.2755E-13, 4.9752E-13, 4.0047E-13, 3.5602E-13, 3.0930E-13,
     X 2.4903E-13, 1.9316E-13, 1.4995E-13, 1.2059E-13, 8.7242E-14,
     X 6.4511E-14, 5.3300E-14, 4.3741E-14, 3.4916E-14, 2.6560E-14,
     X 1.6923E-14, 1.1816E-14, 6.7071E-15, 3.6474E-15, 2.0686E-15/
      DATA F1301/
     X 1.1925E-15, 6.8948E-16, 3.9661E-16, 2.2576E-16, 1.2669E-16,
     X 6.9908E-17, 3.7896E-17, 2.0280E-17, 1.1016E-17, 6.7816E-18,
     X 6.0958E-18, 8.9913E-18, 1.7201E-17, 3.4964E-17, 7.0722E-17,
     X 1.4020E-16, 2.7167E-16, 5.1478E-16, 9.5500E-16, 1.7376E-15,
     X 3.1074E-15, 5.4789E-15, 9.5640E-15, 1.6635E-14, 2.9145E-14,
     X 5.2179E-14, 8.8554E-14, 1.4764E-13, 2.3331E-13, 3.5996E-13,
     X 5.2132E-13, 6.3519E-13, 7.3174E-13, 8.3752E-13, 9.8916E-13,
     X 1.1515E-12, 1.4034E-12, 1.6594E-12, 2.1021E-12, 2.7416E-12,
     X 3.4135E-12, 4.5517E-12, 5.5832E-12, 7.2303E-12, 9.9484E-12,
     X 1.2724E-11, 1.6478E-11, 2.0588E-11, 2.5543E-11, 3.3625E-11/
      DATA F1351/
     X 4.1788E-11, 5.0081E-11, 6.0144E-11, 6.9599E-11, 8.4408E-11,
     X 9.7143E-11, 1.0805E-10, 1.1713E-10, 1.2711E-10, 1.3727E-10,
     X 1.4539E-10, 1.6049E-10, 1.7680E-10, 2.0557E-10, 2.4967E-10,
     X 3.0096E-10, 3.5816E-10, 4.0851E-10, 4.6111E-10, 5.2197E-10,
     X 5.5043E-10, 6.0324E-10, 6.4983E-10, 6.7498E-10, 7.0545E-10,
     X 7.0680E-10, 7.5218E-10, 7.5723E-10, 7.7840E-10, 8.0081E-10,
     X 8.0223E-10, 7.7271E-10, 7.1676E-10, 6.7819E-10, 6.4753E-10,
     X 6.5844E-10, 7.0163E-10, 7.7503E-10, 8.8152E-10, 9.9022E-10,
     X 1.0229E-09, 9.9296E-10, 8.9911E-10, 7.7813E-10, 6.3785E-10,
     X 4.7491E-10, 3.5280E-10, 2.4349E-10, 1.6502E-10, 1.1622E-10/
      DATA F1401/
     X 8.6715E-11, 6.7360E-11, 5.3910E-11, 4.5554E-11, 4.1300E-11,
     X 3.9728E-11, 3.9000E-11, 3.9803E-11, 4.1514E-11, 4.3374E-11,
     X 4.6831E-11, 4.8921E-11, 5.1995E-11, 5.7242E-11, 6.2759E-11,
     X 7.0801E-11, 7.4555E-11, 7.9754E-11, 8.7616E-11, 9.1171E-11,
     X 1.0349E-10, 1.1047E-10, 1.2024E-10, 1.2990E-10, 1.3725E-10,
     X 1.5005E-10, 1.5268E-10, 1.5535E-10, 1.5623E-10, 1.5009E-10,
     X 1.4034E-10, 1.3002E-10, 1.2225E-10, 1.1989E-10, 1.2411E-10,
     X 1.3612E-10, 1.5225E-10, 1.7202E-10, 1.9471E-10, 1.9931E-10,
     X 1.9079E-10, 1.7478E-10, 1.5259E-10, 1.2625E-10, 9.3332E-11,
     X 6.8796E-11, 4.6466E-11, 2.9723E-11, 1.8508E-11, 1.2106E-11/
      DATA F1451/
     X 8.0142E-12, 5.4066E-12, 3.9329E-12, 3.1665E-12, 2.7420E-12,
     X 2.3996E-12, 2.3804E-12, 2.3242E-12, 2.4476E-12, 2.5331E-12,
     X 2.3595E-12, 2.2575E-12, 2.1298E-12, 2.0088E-12, 1.8263E-12,
     X 1.6114E-12, 1.4422E-12, 1.2946E-12, 1.0837E-12, 9.1282E-13,
     X 7.2359E-13, 5.3307E-13, 3.8837E-13, 2.6678E-13, 1.6769E-13,
     X 1.0826E-13, 7.2364E-14, 4.5201E-14, 3.0808E-14, 2.2377E-14,
     X 1.7040E-14, 9.2181E-15, 5.2934E-15, 3.5774E-15, 3.1431E-15,
     X 3.7647E-15, 5.6428E-15, 9.5139E-15, 1.7322E-14, 2.8829E-14,
     X 4.7708E-14, 6.9789E-14, 9.7267E-14, 1.4662E-13, 1.9429E-13,
     X 2.5998E-13, 3.6636E-13, 4.7960E-13, 6.5129E-13, 7.7638E-13/
      DATA F1501/
     X 9.3774E-13, 1.1467E-12, 1.3547E-12, 1.5686E-12, 1.6893E-12,
     X 1.9069E-12, 2.1352E-12, 2.3071E-12, 2.4759E-12, 2.8247E-12,
     X 3.4365E-12, 4.3181E-12, 5.6107E-12, 7.0017E-12, 8.6408E-12,
     X 1.0974E-11, 1.3742E-11, 1.6337E-11, 2.0157E-11, 2.3441E-11,
     X 2.6733E-11, 3.0247E-11, 3.3737E-11, 3.8618E-11, 4.1343E-11,
     X 4.3870E-11, 4.4685E-11, 4.4881E-11, 4.5526E-11, 4.3628E-11,
     X 4.4268E-11, 4.6865E-11, 5.3426E-11, 5.4020E-11, 5.3218E-11,
     X 5.4587E-11, 5.6360E-11, 5.7740E-11, 5.6426E-11, 6.0399E-11,
     X 6.6981E-11, 7.4319E-11, 7.7977E-11, 7.5539E-11, 7.1610E-11,
     X 6.4606E-11, 5.5498E-11, 4.3944E-11, 3.3769E-11, 2.5771E-11/
      DATA F1551/
     X 1.9162E-11, 1.3698E-11, 1.0173E-11, 7.8925E-12, 6.1938E-12,
     X 4.7962E-12, 4.0811E-12, 3.3912E-12, 2.8625E-12, 2.4504E-12,
     X 2.2188E-12, 2.2139E-12, 2.2499E-12, 2.2766E-12, 2.3985E-12,
     X 2.5459E-12, 2.9295E-12, 3.4196E-12, 3.6155E-12, 4.0733E-12,
     X 4.4610E-12, 4.9372E-12, 5.4372E-12, 5.7304E-12, 6.1640E-12,
     X 6.1278E-12, 6.2940E-12, 6.4947E-12, 6.8174E-12, 7.5190E-12,
     X 8.2608E-12, 8.4971E-12, 8.3484E-12, 8.1888E-12, 7.8552E-12,
     X 7.8468E-12, 7.5943E-12, 7.9096E-12, 8.6869E-12, 9.1303E-12,
     X 9.2547E-12, 8.9322E-12, 8.2177E-12, 7.3408E-12, 5.7956E-12,
     X 4.4470E-12, 3.5881E-12, 2.6748E-12, 1.7074E-12, 9.6700E-13/
      DATA F1601/
     X 5.2645E-13, 2.9943E-13, 1.7316E-13, 1.0039E-13, 5.7859E-14,
     X 3.2968E-14, 1.8499E-14, 1.0192E-14, 5.5015E-15, 2.9040E-15,
     X 1.4968E-15, 7.5244E-16, 3.6852E-16, 1.7568E-16, 8.1464E-17,
     X 3.6717E-17, 1.6076E-17, 6.8341E-18, 2.8195E-18, 1.1286E-18,
     X  .0000E+00,  .0000E+00,  .0000E+00,  .0000E+00,  .0000E+00,
     X  .0000E+00,  .0000E+00,  .0000E+00,  .0000E+00, 1.4070E-18,
     X 3.0405E-18, 6.4059E-18, 1.3169E-17, 2.6443E-17, 5.1917E-17,
     X 9.9785E-17, 1.8802E-16, 3.4788E-16, 6.3328E-16, 1.1370E-15,
     X 2.0198E-15, 3.5665E-15, 6.3053E-15, 1.1309E-14, 2.1206E-14,
     X 3.2858E-14, 5.5165E-14, 8.6231E-14, 1.2776E-13, 1.7780E-13/
      DATA F1651/
     X 2.5266E-13, 3.6254E-13, 5.1398E-13, 6.8289E-13, 8.7481E-13,
     X 1.1914E-12, 1.6086E-12, 2.0469E-12, 2.5761E-12, 3.4964E-12,
     X 4.4980E-12, 5.5356E-12, 6.7963E-12, 8.5720E-12, 1.0700E-11,
     X 1.2983E-11, 1.6270E-11, 1.9609E-11, 2.2668E-11, 2.5963E-11,
     X 3.0918E-11, 3.4930E-11, 3.9330E-11, 4.4208E-11, 4.6431E-11,
     X 5.1141E-11, 5.4108E-11, 5.8077E-11, 6.5050E-11, 7.2126E-11,
     X 8.1064E-11, 8.1973E-11, 8.1694E-11, 8.3081E-11, 8.0240E-11,
     X 7.9225E-11, 7.6256E-11, 7.8468E-11, 8.0041E-11, 8.1585E-11,
     X 8.3485E-11, 8.3774E-11, 8.5870E-11, 8.6104E-11, 8.8516E-11,
     X 9.0814E-11, 9.2522E-11, 8.8913E-11, 7.8381E-11, 6.8568E-11/
      DATA F1701/
     X 5.6797E-11, 4.4163E-11, 3.2369E-11, 2.3259E-11, 1.6835E-11,
     X 1.1733E-11, 8.5273E-12, 6.3805E-12, 4.8983E-12, 3.8831E-12,
     X 3.2610E-12, 2.8577E-12, 2.5210E-12, 2.2913E-12, 2.0341E-12,
     X 1.8167E-12, 1.6395E-12, 1.4890E-12, 1.3516E-12, 1.2542E-12,
     X 1.2910E-12, 1.3471E-12, 1.4689E-12, 1.5889E-12, 1.6989E-12,
     X 1.8843E-12, 2.0902E-12, 2.3874E-12, 2.7294E-12, 3.3353E-12,
     X 4.0186E-12, 4.5868E-12, 5.2212E-12, 5.8856E-12, 6.5991E-12,
     X 7.2505E-12, 7.6637E-12, 8.5113E-12, 9.4832E-12, 9.9678E-12,
     X 1.0723E-11, 1.0749E-11, 1.1380E-11, 1.1774E-11, 1.1743E-11,
     X 1.2493E-11, 1.2559E-11, 1.2332E-11, 1.1782E-11, 1.1086E-11/
      DATA F1751/
     X 1.0945E-11, 1.1178E-11, 1.2083E-11, 1.3037E-11, 1.4730E-11,
     X 1.6450E-11, 1.7403E-11, 1.7004E-11, 1.5117E-11, 1.3339E-11,
     X 1.0844E-11, 8.0915E-12, 5.6615E-12, 3.7196E-12, 2.5194E-12,
     X 1.6569E-12, 1.1201E-12, 8.2335E-13, 6.0270E-13, 4.8205E-13,
     X 4.1313E-13, 3.6243E-13, 3.2575E-13, 2.7730E-13, 2.5292E-13,
     X 2.3062E-13, 2.1126E-13, 2.1556E-13, 2.1213E-13, 2.2103E-13,
     X 2.1927E-13, 2.0794E-13, 1.9533E-13, 1.6592E-13, 1.4521E-13,
     X 1.1393E-13, 8.3772E-14, 6.2077E-14, 4.3337E-14, 2.7165E-14,
     X 1.6821E-14, 9.5407E-15, 5.3093E-15, 3.0320E-15, 1.7429E-15,
     X 9.9828E-16, 5.6622E-16, 3.1672E-16, 1.7419E-16, 9.3985E-17/
      DATA F1801/
     X 4.9656E-17, 2.5652E-17, 1.2942E-17, 6.3695E-18, 3.0554E-18,
     C 1.4273E-18, -0.       , -0.       , -0.       , -0.       ,
     C -0.       , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        /
      DATA F1851/
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        /
      DATA F1901/
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        /
      DATA F1951/
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        ,
     C 0.        , 0.        , 0.        , 0.        , 0.        /
      DATA F2001/
     C 0.        /
C
      END
      SUBROUTINE C8DTA (C8L,V)
C **  OZONE U.V + VISIBLE
      COMMON /C4C8/ C4(133),C8(102)
      C8L=0.
      IF(V.LT.13000.) RETURN
      IF(V.GT.50000.) RETURN
      IV=V
      IF(IV.GT.24200.AND.IV.LT.27500) RETURN
      XI=(V-13000.0)/200.0+1.
      IF(IV.GE.27500) XI=(V-27500.0)/500.+57.
      N=XI+1.001
      XD=XI-FLOAT(N)
      C8L=C8(N)+XD*(C8(N)-C8(N-1))
      RETURN
      END
      SUBROUTINE HERTDA(HERZ,V)
C
C     HERZBERG O2 ABSORPTION
C     HALL,1987 PRIVATE COMMUNICATION, BASED ON:
C
C     REF. JOHNSTON ET.AL, JGR,89,11661-11665,1984
C         NICOLET, 1987 (RECENT STUDIES IN ATOMIC & MOLECULAR PROCESSES,
C                        PLEMUN PUBLISHING CORP, NY 1987)
C     AND YOSHINO, ET.AL., 1988 (PREPRINT OF "IMPROVED ABSORPTION
C         CROSS SECTIONS OF OXYGEN IN THE WAVELENGTH REGION 205-240NM
C         OF THE HERZBERG CONTINUUM")
C
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP
C
      HERZ=0.0
      IF(V.LE.36000.00) RETURN
C
C     EXTRAPOLATE SMOOTHLY THROUGH THE HERZBERG BAND REGION
C     NOTE: HERZBERG BANDS ARE NOT CORRECTLY INCLUDED
C
      CORR=0.
      IF(V.LE.40000.)CORR=((40000.-V)/4000.)*7.917E-27
C
C     CONVERSION TO ATM-CM /KM
C
      RLOSCH = 2.6868 E24 * 1.0E-5
C
C     HALL'S NEW HERZBERG  (LEAST SQRS FIT, LN(P))
C
C     YRATIO=2048.7/WL(I)  ****IN ANGSTOMS****
C           =.20487/WN(I)     IN MICRONS
C           =WCM(I)/48811.0   IN CM-1
C
      YRATIO= V    /48811.0
C ALOG -> LOG (94.3.26)
	IF(YRATIO.LT.1.0E-30)YRATIO=1.0E-30
      HERZ=6.884E-24*(YRATIO)*EXP(-69.738*(LOG(YRATIO))**2)-CORR
      HERZ = HERZ * RLOSCH
      RETURN
      END
      SUBROUTINE SCHRUN(V,CPRUN)
      COMMON /SHUR/ SHN(430)
      DATA V1,V2,DV,INUM /49600.,51710.,5.,425/
      CPRUN = -20.
      IF(V .LT. V1) GO TO 20
      IF(V .GT. V2) GO TO 20
      IND = (V - V1)/DV + 1.0001
      IF(IND . GT. INUM) THEN
            PRINT*,'  IND GT INUM  V IND ',V,IND
            GO TO 20
      ENDIF
      CPRUN = SHN(IND)
20    RETURN
      END
      BLOCK DATA SHUMG
C>    BLOCK DATA
C
C     SCHUMANN-RUNGE O2 BAND MODEL - SAMPLE CODING
C
       COMMON /SHUR/ SHN001(70),SHN076(75),SHN151(75),
     X  SHN226(75),SHN301(75),SHN376(54),SHDUM(6)
       DATA SHN001/
     X  -8.00000,  -8.00000,  -8.00000,  -6.30103,  -6.00000,
     X  -5.94896,  -5.94896,  -5.55139,  -5.17613,  -4.90612,
     X  -4.56059,  -4.30010,  -4.30739,  -4.34455,  -4.35231,
     X  -4.50777,  -4.41705,  -3.93569,  -3.70298,  -3.84254,
     X  -4.01007,  -4.13821,  -3.92122,  -3.55006,  -3.22681,
     X  -3.17363,  -3.55224,  -3.66208,  -3.32331,  -3.00804,
     X  -2.99732,  -3.30951,  -3.20867,  -2.69141,  -2.49670,
     X  -2.74355,  -2.69834,  -2.67293,  -2.67371,  -2.89603,
     X  -3.13808,  -3.45473,  -3.54812,  -3.00636,  -2.72446,
     X  -2.90208,  -2.93847,  -3.03693,  -3.15944,  -3.43764,
     X  -3.67262,  -3.53970,  -2.84182,  -2.51909,  -2.50557,
     X  -2.44459,  -2.72040,  -2.95979,  -3.02842,  -2.92391,
     X  -2.61329,  -2.24839,  -2.03988,  -1.98147,  -1.97078,
     X  -2.14548,  -2.51734,  -2.47024,  -2.02579,  -1.70360/
       DATA SHN076/
     X  -1.64178,  -2.05789,  -2.41111,  -2.30034,  -1.91818,
     X  -1.50450,  -1.32084,  -1.80380,  -2.13878,  -1.94658,
     X  -1.61627,  -1.55771,  -1.88813,  -1.64415,  -1.43970,
     X  -1.72633,  -1.50064,  -1.29499,  -1.47224,  -1.42286,
     X  -1.84903,  -2.42249,  -2.95877,  -3.43342,  -3.85023,
     X  -4.92183,  -4.92959,  -4.80852,  -4.67030,  -4.72573,
     X  -4.84445,  -4.86951,  -4.90354,  -4.80891,  -4.61211,
     X  -4.48205,  -4.51391,  -4.66502,  -4.84670,  -4.88606,
     X  -4.82391,  -4.69897,  -4.51203,  -4.13960,  -3.87805,
     X  -3.80311,  -3.77114,  -3.88260,  -4.14615,  -4.39649,
     X  -4.62899,  -4.78494,  -4.69514,  -4.27200,  -3.92731,
     X  -3.72681,  -3.60335,  -3.49142,  -3.38223,  -3.52349,
     X  -3.64037,  -3.58526,  -3.48978,  -3.36320,  -3.37270,
     X  -3.58359,  -3.83908,  -4.06157,  -3.96920,  -3.24875,
     X  -2.78627,  -2.54861,  -2.56192,  -2.79838,  -2.89008/
       DATA SHN151/
     X  -2.97200,  -2.91496,  -2.85783,  -3.00554,  -3.22285,
     X  -3.17575,  -2.82405,  -2.44375,  -2.24512,  -2.13519,
     X  -2.17638,  -2.12548,  -2.24833,  -2.42286,  -2.48889,
     X  -2.57284,  -2.67481,  -2.84576,  -2.57849,  -2.23621,
     X  -1.97914,  -2.01655,  -2.08918,  -2.25852,  -2.60669,
     X  -2.91101,  -3.24343,  -3.54870,  -3.05507,  -2.41260,
     X  -1.97192,  -1.74591,  -1.70757,  -1.86170,  -2.21955,
     X  -2.52520,  -2.86220,  -2.96082,  -2.42138,  -1.96791,
     X  -1.71099,  -1.68871,  -1.86617,  -2.21148,  -2.51694,
     X  -2.77760,  -2.37949,  -1.89083,  -1.58900,  -1.52710,
     X  -1.68850,  -2.03635,  -2.31319,  -2.17366,  -1.60655,
     X  -1.27097,  -1.14262,  -1.34089,  -1.68119,  -1.78236,
     X  -1.45853,  -1.19063,  -1.11210,  -1.38628,  -1.48342,
     X  -1.12039,   -.85543,   -.77060,  -1.05684,  -1.05423,
     X   -.93689,   -.86922,   -.94306,   -.76850,   -.59062/
       DATA SHN226/
     X   -.50208,   -.53499,   -.88884,  -1.18360,  -1.52243,
     X  -1.84564,  -2.17740,  -2.50559,  -2.83351,  -3.15308,
     X  -3.41587,  -3.41025,  -3.23752,  -3.13656,  -3.30149,
     X  -3.55280,  -3.77885,  -3.71929,  -3.36467,  -3.06275,
     X  -2.83782,  -2.68294,  -2.55793,  -2.63001,  -2.90714,
     X  -3.18561,  -3.46714,  -3.70067,  -3.62895,  -3.02605,
     X  -2.65584,  -2.46195,  -2.48991,  -2.44044,  -2.29494,
     X  -2.28839,  -2.29827,  -2.22063,  -2.12801,  -2.18940,
     X  -2.48029,  -2.74669,  -2.83833,  -2.45937,  -2.16507,
     X  -2.02067,  -2.03314,  -1.80888,  -1.51479,  -1.38580,
     X  -1.37993,  -1.63534,  -1.83905,  -1.87999,  -1.82492,
     X  -1.89398,  -1.90149,  -1.78545,  -1.65285,  -1.40144,
     X  -1.17488,  -1.07228,  -1.15343,  -1.37759,  -1.70025,
     X  -2.01075,  -2.33004,  -2.62771,  -2.87105,  -2.84082,
     X  -2.00293,  -1.31932,   -.92860,   -.76253,   -.84790/
       DATA SHN301/
     X  -1.16306,  -1.46677,  -1.79051,  -2.09491,  -2.34556,
     X  -2.13867,  -1.37321,   -.82048,   -.53990,   -.47636,
     X   -.72816,  -1.03484,  -1.33688,  -1.61955,  -1.78843,
     X  -1.43388,   -.81369,   -.44878,   -.28512,   -.40431,
     X   -.72200,  -1.00945,  -1.28895,  -1.31856,   -.85686,
     X   -.42072,   -.19421,   -.18317,   -.46858,   -.73309,
     X   -.93390,   -.77552,   -.37922,   -.12965,   -.05480,
     X   -.26659,   -.48423,   -.50987,   -.24666,   -.01742,
     X    .07660,   -.06367,   -.20185,   -.11253,    .06726,
     X    .17955,    .14879,    .15975,    .28769,    .41632,
     X    .49995,    .61664,    .76706,    .82624,    .76615,
     X    .43165,    .13821,   -.18926,   -.48784,   -.77913,
     X  -1.08972,  -1.39948,  -1.70006,  -1.94700,  -1.96249,
     X  -1.67500,  -1.41241,  -1.29981,  -1.40100,  -1.69529,
     X  -1.96904,  -2.25253,  -2.44942,  -2.13985,  -1.80460/
       DATA SHN376/
     X  -1.60216,  -1.72517,  -1.98472,  -2.08115,  -1.62632,
     X  -1.12971,   -.86160,   -.81141,  -1.07504,  -1.34407,
     X  -1.50074,  -1.47345,  -1.41077,  -1.59810,  -1.67103,
     X  -1.53208,  -1.36215,  -1.26724,   -.91307,   -.50826,
     X   -.27840,   -.24468,   -.46373,   -.76619,  -1.07304,
     X  -1.37968,  -1.66148,  -1.89046,  -2.02811,  -1.97679,
     X  -1.55840,   -.94089,   -.46463,   -.21757,   -.16985,
     X   -.41642,   -.69469,   -.98624,  -1.26028,  -1.48661,
     X  -1.58100,  -1.42675,  -1.01563,   -.52312,   -.13686,
     X    .06300,    .07682,   -.16825,   -.42809,   -.69506,
     X   -.91898,  -1.03253,   -.90609,   -.42809           /
      END
      BLOCK DATA CPH2O
C>    BLOCK DATA
C
C     C' FOR WATER VAPOR (H2O)
      COMMON /H2O/
     +        C11H2O( 70),
     +        C21H2O(126),C22H2O(  5),
     +        C31H2O(126),C32H2O(  2),
     +        C41H2O(126),C42H2O( 52),
     +        C51H2O(126),C52H2O( 52),
     +        C61H2O(126),C62H2O( 52),
     +        C71H2O(126),C72H2O(126),C73H2O(116),
     +        C81H2O(126),C82H2O(126),C83H2O(118),
     +        C91H2O(126),C92H2O(126),C93H2O( 71),
     +        CA1H2O(126),CA2H2O(126),CA3H2O(126),CA4H2O(7),
     +        CB1H2O(126),CB2H2O(126),CB3H2O( 54),
     +        CC1H2O(126),CC2H2O(126),CC3H2O(106),
     +        CD1H2O(126),CD2H2O(111),
     +        CE1H2O(126),CE2H2O(126),CE3H2O( 53)
C=H2O ====C' FOR   14 BAND MODELS
C=H2O ====    0-  345
      DATA C11H2O/
     X -.59366, -.16679,  .42846,  .87819, 1.26357, 1.59247, 1.86372,
     X 2.11483, 2.31810, 2.44040, 2.55998, 2.69879, 2.79810, 2.89747,
     X 2.98118, 3.04863, 3.09568, 3.15381, 3.22984, 3.23785, 3.20991,
     X 3.14246, 3.03461, 2.98864, 3.03520, 3.08981, 3.10027, 3.11302,
     X 3.10266, 3.05765, 3.06166, 3.01593, 2.95500, 2.95328, 2.95297,
     X 2.91497, 2.83753, 2.74642, 2.70474, 2.75606, 2.84097, 2.89052,
     X 2.89886, 2.86150, 2.78032, 2.67212, 2.52752, 2.39301, 2.38109,
     8 2.43965, 2.46195, 2.39329, 2.22943, 2.15815, 2.16157, 2.29683,
     9 2.40335, 2.35569, 2.29239, 2.12968, 2.03781, 1.94313, 1.86282,
     X 1.87312, 1.88177, 1.95321, 1.94145, 1.92602, 1.92812, 1.90587/
C=H2O ====  350- 1000
      DATA C21H2O/
     X 2.04943, 1.95396, 1.78078, 1.60325, 1.55071, 1.49473, 1.46485,
     X 1.50231, 1.39831, 1.30664, 1.14704,  .96109,  .93139, 1.00613,
     X 1.11827, 1.13529, 1.07767,  .96652,  .90777,  .91973,  .90622,
     X  .93883,  .90861,  .81968,  .79852,  .69385,  .56997,  .49693,
     X  .40867,  .37846,  .44490,  .53554,  .59020,  .59196,  .50771,
     X  .34361,  .20796,  .15417,  .13600,  .14235,  .12700,  .08853,
     X  .06715,  .11430,  .15016,  .15016,  .13964,  .04897, -.04476,
     8 -.16953, -.30196, -.39901, -.42462, -.39340, -.35671, -.30771,
     9 -.31570, -.35021, -.47016, -.62308, -.77946, -.85086, -.82482,
     X -.83468, -.83991, -.89726, -.90918, -.84484, -.71025, -.62777,
     1 -.66324, -.76848,-1.03341,-1.27044,-1.49576,-1.61769,-1.53549,
     X-1.47958,-1.33160,-1.29625,-1.40768,-1.52411,-1.72765,-1.82510,
     X-1.76468,-1.70983,-1.59977,-1.50730,-1.46683,-1.39464,-1.43093,
     X-1.58947,-1.78778,-2.06146,-2.33634,-2.40749,-2.49065,-2.44182,
     X-2.25150,-2.19801,-2.08624,-2.10309,-2.27174,-2.36492,-2.45781,
     X-2.44508,-2.36196,-2.38101,-2.48058,-2.61957,-2.74895,-2.74245,
     X-2.63961,-2.61588,-2.61569,-2.71770,-2.92220,-3.01021,-2.99432,
     X-2.89456,-2.79847,-2.73359,-2.69055,-2.65898,-2.60837,-2.63170/
      DATA C22H2O/
     X-2.79096,-2.97394,-3.15934,-3.17057,-2.95258/
C=H2O ==== 1005- 1640
      DATA C31H2O/
     C-2.78308,-2.69196,-2.60867,-2.62239,-2.62637,-2.62950,-2.71010,
     C-2.72574,-2.71317,-2.61321,-2.51967,-2.42437,-2.38734,-2.45056,
     C-2.47843,-2.58702,-2.56472,-2.44706,-2.30814,-2.12582,-2.02697,
     C-1.99880,-2.05659,-2.05701,-2.06643,-2.04721,-1.90723,-1.90946,
     C-1.92812,-1.86522,-1.88820,-1.77270,-1.60669,-1.51740,-1.40182,
     C-1.38758,-1.38799,-1.41620,-1.43182,-1.37124,-1.28249,-1.09992,
     C -.99724, -.97950, -.99952,-1.09066,-1.09980,-1.00750, -.87259,
     8 -.70131, -.48309, -.30502, -.20407, -.13886, -.19661, -.24505,
     9 -.28415, -.34466, -.34496, -.28657, -.09485,  .16770,  .38311,
     C  .48553,  .49475,  .49074,  .52493,  .57439,  .60303,  .66919,
     1  .75656,  .90385, 1.04976, 1.13836, 1.20132, 1.21963, 1.30344,
     C 1.41212, 1.46770, 1.47630, 1.45559, 1.43315, 1.49679, 1.62749,
     C 1.68517, 1.70120, 1.66090, 1.59891, 1.64107, 1.76792, 1.93419,
     C 2.09362, 2.13280, 2.07959, 2.01987, 1.96835, 2.03073, 2.17591,
     C 2.32257, 2.49261, 2.60881, 2.66112, 2.68139, 2.70360, 2.70568,
     C 2.67997, 2.66478, 2.63655, 2.59716, 2.57555, 2.58781, 2.58940,
     C 2.50826, 2.28771, 1.95070, 1.59144, 1.31269, 1.21786, 1.22507,
     8 1.31945, 1.53875, 1.78543, 2.02655, 2.22881, 2.32061, 2.34163/
      DATA C32H2O/
     C 2.39432, 2.43073/
C=H2O ==== 1645- 2530
      DATA C41H2O/
     C 2.53438, 2.55861, 2.51156, 2.46499, 2.46254, 2.51561, 2.56373,
     C 2.62430, 2.67999, 2.68386, 2.68780, 2.68227, 2.59536, 2.42505,
     C 2.29307, 2.17816, 2.11945, 2.20521, 2.32197, 2.38083, 2.38052,
     C 2.25417, 2.11473, 2.06142, 2.02788, 2.01508, 1.97680, 1.91586,
     C 1.87253, 1.83706, 1.80766, 1.67367, 1.45528, 1.29956, 1.18809,
     C 1.20246, 1.33650, 1.45778, 1.48886, 1.40546, 1.22716, 1.01444,
     C  .91282,  .87247,  .83576,  .80170,  .71481,  .66927,  .65846,
     C  .66839,  .68503,  .66215,  .72413,  .78703,  .77831,  .71136,
     C  .51200,  .35931,  .30680,  .33365,  .36267,  .32095,  .25710,
     C  .12363, -.02266, -.18001, -.28048, -.27808, -.19047, -.08151,
     C -.09169, -.16662, -.24404, -.27238, -.27345, -.32244, -.42037,
     C -.54071, -.63500, -.69930, -.77174, -.83521, -.86639, -.82329,
     C -.78820, -.82340, -.83838, -.91387, -.96524, -.96364,-1.05757,
     C-1.12747,-1.19973,-1.27071,-1.30173,-1.34436,-1.35556,-1.35990,
     C-1.30386,-1.26726,-1.28022,-1.32843,-1.43599,-1.55929,-1.69416,
     C-1.79362,-1.86416,-1.90037,-1.91305,-1.94866,-1.95483,-1.92284,
     C-1.87535,-1.83065,-1.86043,-1.93470,-2.01410,-2.07677,-2.07980,
     C-2.01822,-1.96078,-1.95185,-1.96638,-2.05704,-2.17667,-2.24120/
      DATA C42H2O/
     C-2.27833,-2.33268,-2.37375,-2.43075,-2.54346,-2.60789,-2.68442,
     C-2.78402,-2.83736,-2.89622,-2.95598,-3.03170,-3.13338,-3.26736,
     C-3.41725,-3.51456,-3.61586,-3.67210,-3.67841,-3.72135,-3.74941,
     C-3.78822,-3.85868,-3.90419,-3.91592,-3.97897,-4.00562,-4.08675,
     C-4.18795,-4.15833,-4.18094,-4.18872,-4.25849,-4.42026,-4.57444,
     C-4.64021,-4.58636,-4.51788,-4.46274,-4.44165,-4.45450,-4.42101,
     C-4.35067,-4.30493,-4.23157,-4.11952,-4.01918,-3.93341,-3.81424,
     C-3.70572,-3.62484,-3.48143/
C=H2O ==== 2535- 3420
      DATA C51H2O/
     C-3.35886,-3.26514,-3.15517,-3.02814,-2.95147,-2.83444,-2.68908,
     C-2.62390,-2.50458,-2.39841,-2.35516,-2.24360,-2.18204,-2.16652,
     C-2.08381,-2.02597,-1.99880,-1.90122,-1.84045,-1.82575,-1.74889,
     C-1.70489,-1.66792,-1.60475,-1.59789,-1.59221,-1.60854,-1.66569,
     C-1.68527,-1.72998,-1.79886,-1.81356,-1.82715,-1.79425,-1.61106,
     C-1.40549,-1.24369,-1.15433,-1.23589,-1.44178,-1.64717,-1.78560,
     C-1.84622,-1.77824,-1.69071,-1.66066,-1.58765,-1.54222,-1.51960,
     8-1.45477,-1.39881,-1.38659,-1.37586,-1.36025,-1.39179,-1.36927,
     C-1.35455,-1.38734,-1.40292,-1.45598,-1.51545,-1.56173,-1.62478,
     C-1.69200,-1.75192,-1.81120,-1.83354,-1.87063,-1.89006,-1.88485,
     C-1.90298,-1.85403,-1.82001,-1.82495,-1.82901,-1.90076,-1.93649,
     C-1.83304,-1.70268,-1.52380,-1.41443,-1.41301,-1.39373,-1.34561,
     C-1.20932,-1.03186, -.85296, -.71145, -.59825, -.51884, -.51690,
     C -.51723, -.52224, -.50043, -.40989, -.32204, -.24881, -.18653,
     C -.17548, -.22729, -.32885, -.46183, -.47994, -.36042, -.23072,
     6 -.12160, -.06422, -.14924, -.21674, -.17913, -.15803, -.04515,
     C  .14450,  .28118,  .39718,  .49818,  .51040,  .44761,  .29666,
     8  .01147, -.32421, -.66518, -.96090,-1.13017,-1.18009,-1.08032/
      DATA C52H2O/
     C -.80133, -.52001, -.33748, -.22519, -.20871, -.26962, -.22592,
     C -.15919, -.07358,  .09367,  .20019,  .25965,  .27816,  .28577,
     C  .22305,  .17722,  .14469,  .06694,  .07268,  .10103,  .14554,
     C  .20352,  .25681,  .25790,  .21316,  .15965,  .08703,  .01638,
     C -.03529, -.03274, -.08812, -.12524, -.13536, -.23808, -.28262,
     C -.30082, -.29252, -.13320,  .05226,  .17657,  .21670,  .12268,
     C  .00438, -.03051, -.00359,  .02967,  .04460, -.01109, -.06041,
     C -.07485, -.02511,  .07116/
C=H2O ==== 3425- 4310
      DATA C61H2O/
     C  .18506,  .27668,  .32130,  .35452,  .39867,  .36470,  .34978,
     C  .36519,  .38993,  .47009,  .54349,  .60193,  .67101,  .73253,
     C  .84100,  .92974, 1.00406, 1.06301, 1.07261, 1.09629, 1.10790,
     C 1.10959, 1.11710, 1.15716, 1.24152, 1.34834, 1.45152, 1.53939,
     C 1.59331, 1.60894, 1.63833, 1.67031, 1.74144, 1.82069, 1.90463,
     C 1.98593, 2.02996, 2.10254, 2.16357, 2.16140, 2.11190, 2.06655,
     C 2.02241, 2.02978, 2.06771, 2.04985, 2.02048, 1.99566, 2.01593,
     8 2.11269, 2.22805, 2.27037, 2.23480, 2.16907, 2.09990, 2.08096,
     C 2.10710, 2.15298, 2.19061, 2.25811, 2.34221, 2.43200, 2.59765,
     C 2.72007, 2.77243, 2.71671, 2.56246, 2.33896, 2.14412, 1.97864,
     C 1.79640, 1.73371, 1.71380, 1.74950, 1.91932, 2.10063, 2.26262,
     C 2.36884, 2.42988, 2.47605, 2.51875, 2.53371, 2.51476, 2.47425,
     C 2.40051, 2.39254, 2.39540, 2.35342, 2.33460, 2.26830, 2.17169,
     C 2.09605, 2.04747, 2.01127, 1.89721, 1.74928, 1.55948, 1.38069,
     C 1.34831, 1.35751, 1.35809, 1.34286, 1.25929, 1.16743, 1.09595,
     6 1.00365,  .87965,  .76257,  .64206,  .56343,  .49943,  .40691,
     C  .29104,  .18437,  .12690,  .09157,  .13377,  .18899,  .20257,
     8  .19155,  .09384, -.01238, -.14283, -.26122, -.31851, -.45610/
      DATA C62H2O/
     C -.58273, -.65867, -.73100, -.66169, -.52264, -.46798, -.50258,
     C -.59104, -.72925, -.81067, -.80914, -.86943, -.92975, -.92524,
     C -.88289, -.79203, -.69250, -.68167, -.75444, -.86193, -.97556,
     C-1.10473,-1.20018,-1.24824,-1.27702,-1.22693,-1.18773,-1.13552,
     C-1.14015,-1.21589,-1.26394,-1.39464,-1.46192,-1.52629,-1.64635,
     C-1.71511,-1.78752,-1.79358,-1.77801,-1.75599,-1.77196,-1.83224,
     C-1.89985,-1.98528,-2.09408,-2.24126,-2.37607,-2.43218,-2.43830,
     C-2.38400,-2.33538,-2.43573/
C=H2O ==== 4315- 6150
      DATA C71H2O/
     X-2.52275,-2.67290,-2.83451,-2.93019,-3.01749,-3.02463,-2.99666,
     X-2.95414,-2.91300,-2.96493,-3.07471,-3.25693,-3.47657,-3.67222,
     X-3.88925,-3.97727,-3.94079,-3.81920,-3.66194,-3.59739,-3.64351,
     X-3.74016,-3.90037,-4.04679,-4.07663,-4.03256,-3.91836,-3.80990,
     X-3.76032,-3.77951,-3.84240,-3.90305,-3.92223,-3.82628,-3.65450,
     X-3.44339,-3.25756,-3.09919,-3.00901,-2.95747,-2.88271,-2.82108,
     X-2.72633,-2.59367,-2.46775,-2.36235,-2.28438,-2.27343,-2.30886,
     8-2.33620,-2.27813,-2.20677,-2.16170,-2.14594,-2.24245,-2.36299,
     X-2.42996,-2.50866,-2.55678,-2.50968,-2.47465,-2.42796,-2.37981,
     X-2.34092,-2.30518,-2.26753,-2.27390,-2.44156,-2.72384,-3.06108,
     X-3.38056,-3.48970,-3.41674,-3.36528,-3.27790,-3.15495,-3.01945,
     X-2.81869,-2.66003,-2.56096,-2.49017,-2.46335,-2.51454,-2.59743,
     X-2.67025,-2.78841,-2.77863,-2.63881,-2.54169,-2.40240,-2.37146,
     X-2.46253,-2.54291,-2.65346,-2.69467,-2.69130,-2.65025,-2.59152,
     X-2.56343,-2.50785,-2.44665,-2.41418,-2.34553,-2.28223,-2.25278,
     6-2.20694,-2.16892,-2.14295,-2.14341,-2.16443,-2.24853,-2.38594,
     X-2.49449,-2.58047,-2.55462,-2.41673,-2.35641,-2.32619,-2.34603,
     8-2.40102,-2.30576,-2.20532,-2.09307,-2.00782,-2.00039,-1.91252/
      DATA C72H2O/
     X-1.80383,-1.65749,-1.55728,-1.59262,-1.70939,-1.83569,-1.84895,
     X-1.71457,-1.53813,-1.41904,-1.37588,-1.39458,-1.39135,-1.35232,
     X-1.30470,-1.24821,-1.20394,-1.19607,-1.15995,-1.13948,-1.11024,
     X-1.03785, -.99804, -.95430, -.92707, -.93592, -.93528, -.86881,
     X -.75121, -.55836, -.35056, -.22085, -.13412, -.12673, -.13867,
     X -.11656, -.07357,  .01888,  .11050,  .20428,  .29291,  .35923,
     X  .43608,  .47266,  .49792,  .54978,  .60489,  .67778,  .71787,
     8  .73606,  .74796,  .75193,  .81728,  .87972,  .95990, 1.07451,
     X 1.13098, 1.17565, 1.19031, 1.20334, 1.27687, 1.35910, 1.41924,
     X 1.37988, 1.28213, 1.16286, 1.08658, 1.06554, 1.03702, 1.01290,
     X  .95519,  .94231,  .94216,  .95764, 1.03405, 1.11309, 1.27076,
     X 1.48131, 1.66125, 1.76502, 1.68299, 1.50126, 1.28195, 1.13724,
     X 1.09863, 1.12031, 1.23502, 1.34328, 1.39556, 1.40851, 1.40939,
     X 1.40259, 1.39505, 1.38427, 1.33724, 1.29860, 1.34354, 1.43194,
     X 1.50874, 1.54493, 1.48740, 1.37260, 1.26973, 1.21297, 1.11026,
     6  .97625,  .87238,  .76100,  .71825,  .73936,  .69604,  .64138,
     X  .59585,  .51097,  .44903,  .40524,  .29892,  .21583,  .19145,
     8  .15378,  .13759,  .09412, -.04455, -.18870, -.28538, -.37204/
      DATA C73H2O/
     X -.46390, -.57884, -.70647, -.78911, -.79511, -.76645, -.76146,
     X -.80163, -.83155, -.86672, -.92994, -.99971,-1.10990,-1.25701,
     X-1.32841,-1.33350,-1.35269,-1.31799,-1.35095,-1.48830,-1.57874,
     X-1.67539,-1.72874,-1.68087,-1.67518,-1.73066,-1.77654,-1.79238,
     X-1.81386,-1.77187,-1.73774,-1.78673,-1.82129,-1.86174,-1.87867,
     X-1.92986,-1.95895,-1.98042,-2.10738,-2.14350,-2.22883,-2.35165,
     X-2.30593,-2.31343,-2.23607,-2.17791,-2.29047,-2.40740,-2.60466,
     8-2.70413,-2.67647,-2.64479,-2.62274,-2.66727,-2.67591,-2.66531,
     X-2.64576,-2.69566,-2.79611,-2.90809,-2.99381,-2.94495,-2.94833,
     X-2.97002,-3.01283,-3.07907,-3.08348,-3.06412,-3.08084,-3.20105,
     X-3.32453,-3.49652,-3.63219,-3.65897,-3.69476,-3.63741,-3.54369,
     X-3.44992,-3.41310,-3.43168,-3.48306,-3.57513,-3.59385,-3.59684,
     X-3.60814,-3.50612,-3.41284,-3.34107,-3.27248,-3.26950,-3.31027,
     X-3.32205,-3.29589,-3.29768,-3.28777,-3.29950,-3.39843,-3.43784,
     X-3.47042,-3.54250,-3.55457,-3.69278,-3.82390,-3.91709,-4.02428,
     6-3.97802,-4.04945,-3.99837,-3.96096,-4.01515,-4.01286,-4.27890,
     7-4.64526,-4.92520,-5.20714,-5.02961/
C=H2O ==== 6155- 8000
      DATA C81H2O/
     X-4.88315,-4.85584,-4.76921,-4.54440,-4.33075,-4.16671,-4.04406,
     X-4.09564,-4.11792,-4.14522,-4.19109,-4.14906,-4.22221,-4.35301,
     X-4.47867,-4.50537,-4.41913,-4.24856,-4.05892,-3.91396,-3.73977,
     X-3.60042,-3.52610,-3.50040,-3.55218,-3.66025,-3.77097,-3.87835,
     X-3.96454,-3.93046,-3.92926,-3.96805,-3.99038,-4.10179,-4.21981,
     X-4.24013,-4.26190,-4.27753,-4.25594,-4.28500,-4.29071,-4.26155,
     X-4.16114,-4.04160,-3.91756,-3.82524,-3.76258,-3.74207,-3.77017,
     8-3.80666,-3.92858,-4.01356,-4.10145,-4.16708,-4.09123,-4.00345,
     X-3.88032,-3.81171,-3.80771,-3.83212,-3.88507,-3.81399,-3.70048,
     X-3.58376,-3.46350,-3.42785,-3.41629,-3.40329,-3.36172,-3.26599,
     X-3.16908,-3.10954,-3.03394,-2.95828,-2.85536,-2.71469,-2.60076,
     X-2.48946,-2.38513,-2.32220,-2.30051,-2.34186,-2.37590,-2.33267,
     X-2.21087,-2.03216,-1.91013,-1.82328,-1.77996,-1.76714,-1.72488,
     X-1.71325,-1.67669,-1.62963,-1.60411,-1.54027,-1.47681,-1.37155,
     X-1.25978,-1.23494,-1.26986,-1.33751,-1.37220,-1.28322,-1.14853,
     6-1.03021, -.89832, -.84340, -.83317, -.78856, -.76905, -.69209,
     X -.53147, -.37401, -.25508, -.21755, -.22627, -.23936, -.22223,
     8 -.17345, -.11880, -.10331, -.15444, -.20353, -.25350, -.26628/
      DATA C82H2O/
     X -.13441,  .02358,  .13657,  .22032,  .19637,  .12621,  .07999,
     X  .04393, -.01900, -.06543, -.08129, -.14847, -.17765, -.23113,
     X -.29309, -.28723, -.27521, -.20013, -.11575, -.00428,  .10976,
     X  .16530,  .18309,  .13200,  .10610,  .10394,  .13621,  .17117,
     X  .17251,  .18671,  .16161,  .16640,  .18417,  .18573,  .24876,
     X  .26103,  .28476,  .33612,  .30642,  .30150,  .27173,  .21976,
     X  .23130,  .27376,  .30887,  .34334,  .34765,  .31180,  .30774,
     8  .31256,  .35423,  .42454,  .44493,  .43846,  .44507,  .43684,
     X  .49327,  .53868,  .51933,  .54592,  .54951,  .63201,  .74737,
     X  .80266,  .88719,  .87874,  .84412,  .84352,  .81737,  .86380,
     X  .94765,  .95553,  .93965,  .90241,  .91481, 1.00917, 1.11552,
     X 1.15202, 1.06885,  .96737,  .85164,  .80701,  .82571,  .87391,
     X  .98520, 1.07042, 1.18051, 1.29004, 1.37755, 1.48278, 1.47663,
     X 1.40851, 1.27508, 1.11986,  .98454,  .88260,  .82338,  .79509,
     X  .83355,  .91046, 1.04503, 1.21868, 1.36672, 1.46155, 1.47085,
     6 1.46520, 1.42619, 1.37940, 1.41333, 1.43128, 1.45974, 1.54526,
     X 1.53032, 1.48103, 1.39624, 1.26267, 1.17261, 1.09232, 1.05888,
     8 1.01929,  .94626,  .87615,  .73334,  .61962,  .52576,  .40124/
      DATA C83H2O/
     X  .32424,  .20042,  .05769, -.09325, -.27407, -.40779, -.52559,
     X -.58490, -.57916, -.54457, -.50743, -.45937, -.41861, -.41520,
     X -.39164, -.36510, -.30857, -.23157, -.18280, -.15878, -.21295,
     X -.29332, -.39457, -.54826, -.71006, -.87700, -.96819, -.98703,
     X -.93748, -.83916, -.78698, -.76209, -.80754, -.93347,-1.06076,
     X-1.15801,-1.16256,-1.09618,-1.03195,-1.05522,-1.13586,-1.23387,
     X-1.33214,-1.32682,-1.33648,-1.38038,-1.42553,-1.49769,-1.52950,
     8-1.54445,-1.56745,-1.61707,-1.69148,-1.76787,-1.82556,-1.84347,
     X-1.86221,-1.87097,-1.84614,-1.88659,-1.98535,-2.12108,-2.27740,
     X-2.39335,-2.39886,-2.33846,-2.30442,-2.27409,-2.29854,-2.39124,
     X-2.56427,-2.73609,-2.88840,-3.00443,-3.02685,-3.09379,-3.16003,
     X-3.13090,-3.06189,-3.00807,-2.95169,-3.01568,-3.11918,-3.18931,
     X-3.35446,-3.46712,-3.51002,-3.48618,-3.36603,-3.29278,-3.32935,
     X-3.47177,-3.61763,-3.68930,-3.67420,-3.62078,-3.67644,-3.76717,
     X-3.78944,-3.79818,-3.75336,-3.74321,-3.86778,-3.96899,-4.05004,
     6-4.15451,-4.17979,-4.22704,-4.28851,-4.25560,-4.21920,-4.27564,
     7-4.42921,-4.58506,-4.70967,-4.80136,-4.64650,-4.65341/
C=H2O ==== 8005- 9615
      DATA C91H2O/
     X-4.51995,-4.42433,-4.42137,-4.44853,-4.44819,-4.49132,-4.49176,
     X-4.52929,-4.58468,-4.60533,-4.62362,-4.60168,-4.59803,-4.45292,
     X-4.26920,-4.09891,-3.92615,-3.86016,-3.69436,-3.53699,-3.38584,
     X-3.23356,-3.19281,-3.14232,-3.11326,-3.04386,-2.90514,-2.80270,
     X-2.68808,-2.62726,-2.61349,-2.57111,-2.54465,-2.47142,-2.42795,
     X-2.40936,-2.37936,-2.41255,-2.40417,-2.41017,-2.39774,-2.33861,
     X-2.23985,-2.08388,-2.00350,-1.93767,-1.91020,-1.92815,-1.89802,
     8-1.85648,-1.84229,-1.86062,-1.89799,-1.95863,-2.01858,-2.05596,
     X-2.06508,-2.02824,-1.93392,-1.83965,-1.74890,-1.71252,-1.72275,
     X-1.71193,-1.68781,-1.66945,-1.64316,-1.63675,-1.69286,-1.70297,
     X-1.72751,-1.75100,-1.73714,-1.79804,-1.84371,-1.86235,-1.88812,
     X-1.83704,-1.77649,-1.70661,-1.60188,-1.50341,-1.43505,-1.46076,
     X-1.51651,-1.57911,-1.61619,-1.55812,-1.49706,-1.45230,-1.42832,
     X-1.44314,-1.52138,-1.60752,-1.62106,-1.64265,-1.64250,-1.64573,
     X-1.74951,-1.80667,-1.76036,-1.68790,-1.57515,-1.53228,-1.57292,
     6-1.61350,-1.65583,-1.63563,-1.58694,-1.56417,-1.53128,-1.54079,
     X-1.55014,-1.53022,-1.53190,-1.50230,-1.50260,-1.49991,-1.45992,
     8-1.41944,-1.31703,-1.21850,-1.14990,-1.08809,-1.04748,-1.01748/
      DATA C92H2O/
     X -.95109, -.84680, -.74538, -.60472, -.50362, -.46372, -.42447,
     X -.44838, -.44419, -.40683, -.38084, -.33053, -.32846, -.33572,
     X -.31158, -.29906, -.20305, -.13083, -.09973, -.06963, -.12740,
     X -.20199, -.29978, -.35896, -.38843, -.41730, -.45017, -.51507,
     X -.56213, -.57297, -.50844, -.42276, -.29372, -.08843,  .09240,
     X  .25840,  .28311,  .13891, -.06768, -.28207, -.39760, -.40444,
     X -.31138, -.14305, -.02128,  .04782,  .08894,  .10200,  .09648,
     8  .10814,  .09787,  .04275,  .07559,  .12150,  .14186,  .19034,
     X  .13856,  .07934,  .05903, -.00117, -.04140, -.11747, -.21938,
     X -.28241, -.37335, -.49225, -.58631, -.68229, -.75086, -.77623,
     X -.84652, -.93691,-1.00829,-1.07836,-1.10936,-1.10990,-1.10672,
     X-1.07623,-1.03447,-1.01613,-1.00369, -.99511,-1.06778,-1.12221,
     X-1.14258,-1.19379,-1.17257,-1.15262,-1.17033,-1.16389,-1.14503,
     X-1.13643,-1.12323,-1.19203,-1.33003,-1.47540,-1.65339,-1.68424,
     X-1.66968,-1.67118,-1.61782,-1.65910,-1.73337,-1.81449,-1.93135,
     6-2.03554,-2.03827,-1.99609,-2.00710,-2.03895,-2.19678,-2.30931,
     X-2.30301,-2.23226,-2.07787,-2.03277,-2.03851,-2.10514,-2.23452,
     8-2.33474,-2.44465,-2.43944,-2.37675,-2.35973,-2.37611,-2.48915/
      DATA C93H2O/
     X-2.59681,-2.62562,-2.61907,-2.61274,-2.73225,-2.84636,-2.91882,
     X-2.95084,-2.84617,-2.83687,-2.84531,-2.82928,-2.88406,-2.93621,
     X-3.00526,-3.09956,-3.16051,-3.18338,-3.25056,-3.38003,-3.56102,
     X-3.72396,-3.80811,-3.82369,-3.79760,-3.90921,-4.04910,-4.14132,
     X-4.22416,-4.16634,-4.21193,-4.37375,-4.54004,-4.54848,-4.34009,
     X-4.10097,-3.93945,-3.99014,-4.18155,-4.46321,-4.84035,-4.95672,
     X-4.88529,-4.92967,-5.09480,-5.27981,-5.39165,-5.32774,-5.16805,
     8-5.26308,-5.53619,-5.93153,-6.48485,-6.38350,-6.02883,-5.76237,
     X-5.65535,-5.58220,-5.58090,-5.69939,-5.87562,-6.23761,-6.45380,
     X-6.50710,-6.40861,-6.18069,-6.15034,-6.12957,-6.08168,-6.05912,
     1-6.20029/
C=H2O ==== 9620-11540
      DATA CA1H2O/
     X-6.35916,-6.63834,-7.22799,-6.87579,-6.38557,-6.05701,-5.77145,
     X-5.71889,-5.54063,-5.34887,-5.20440,-5.01687,-4.88229,-4.75732,
     X-4.61829,-4.47540,-4.40382,-4.22901,-4.07893,-3.91067,-3.71540,
     X-3.66982,-3.60413,-3.59635,-3.66139,-3.67630,-3.61574,-3.49060,
     X-3.33033,-3.18950,-3.19004,-3.27293,-3.43811,-3.58539,-3.69658,
     X-3.64411,-3.52966,-3.51758,-3.45900,-3.56858,-3.67516,-3.75396,
     X-3.80574,-3.77074,-3.74231,-3.63809,-3.64323,-3.59911,-3.62673,
     8-3.64385,-3.54801,-3.49160,-3.38461,-3.33358,-3.21719,-3.04173,
     X-2.89493,-2.77334,-2.79171,-2.91085,-3.04844,-3.23627,-3.31742,
     X-3.35484,-3.39756,-3.34285,-3.36017,-3.34117,-3.26031,-3.20256,
     X-3.07615,-2.98533,-3.01199,-3.13943,-3.33780,-3.54162,-3.64413,
     X-3.59251,-3.59490,-3.60162,-3.66139,-3.81236,-3.87304,-4.04749,
     X-4.11623,-4.09447,-4.12708,-3.91916,-3.77960,-3.62012,-3.44890,
     X-3.42739,-3.42156,-3.36932,-3.34675,-3.22941,-3.12258,-3.12447,
     X-3.07216,-3.06608,-3.04637,-2.99581,-3.00597,-2.94524,-2.83430,
     6-2.69244,-2.53460,-2.44553,-2.36211,-2.25128,-2.12504,-1.99329,
     X-1.94694,-1.96858,-2.02552,-2.02890,-1.95458,-1.83064,-1.68469,
     8-1.63148,-1.63055,-1.64868,-1.68433,-1.65098,-1.54445,-1.45543/
      DATA CA2H2O/
     X-1.39405,-1.35500,-1.38974,-1.43708,-1.49729,-1.58141,-1.63709,
     X-1.71988,-1.74834,-1.78729,-1.81439,-1.79445,-1.80727,-1.78446,
     X-1.77116,-1.69515,-1.57106,-1.41358,-1.22505,-1.11749,-1.06719,
     X-1.05722,-1.05923,-1.08022,-1.08249,-1.05940,-1.05527, -.97884,
     X -.90009, -.86984, -.84202, -.84891, -.86571, -.87771, -.86436,
     X -.89675, -.95811, -.95681, -.98685, -.91920, -.79481, -.73405,
     X -.63486, -.61580, -.66083, -.69059, -.75323, -.74477, -.65052,
     8 -.58475, -.56151, -.61494, -.70313, -.70147, -.64776, -.57626,
     X -.52669, -.56405, -.57813, -.57452, -.57656, -.52371, -.48121,
     X -.47066, -.44204, -.42321, -.43939, -.40019, -.34592, -.36666,
     X -.36117, -.41494, -.53334, -.63311, -.73668, -.83196, -.91543,
     X -.92801, -.91893, -.82619, -.64369, -.45814, -.28838, -.20295,
     X -.12845, -.12789, -.14668, -.10804, -.12206, -.08664, -.05495,
     X -.09929, -.16477, -.24481, -.32305, -.39276, -.44000, -.52873,
     X -.60139, -.69141, -.79857, -.89923,-1.00968,-1.08832,-1.14958,
     6-1.21303,-1.28067,-1.38492,-1.47822,-1.51729,-1.55518,-1.53633,
     X-1.51062,-1.50327,-1.51801,-1.57645,-1.65941,-1.73134,-1.75165,
     8-1.72655,-1.71606,-1.73263,-1.74728,-1.79286,-1.73848,-1.66180/
      DATA CA3H2O/
     X-1.56283,-1.40366,-1.32738,-1.25309,-1.25065,-1.26987,-1.24009,
     X-1.22822,-1.19404,-1.20867,-1.23645,-1.19332,-1.13591,-1.08205,
     X-1.04976,-1.14128,-1.23489,-1.27858,-1.33065,-1.28360,-1.22682,
     X-1.18706,-1.15823,-1.14067,-1.16633,-1.17506,-1.15970,-1.19126,
     X-1.19843,-1.30385,-1.42862,-1.58004,-1.72327,-1.78743,-1.86895,
     X-1.85190,-1.80529,-1.69422,-1.49103,-1.32529,-1.20009,-1.13762,
     X-1.11678,-1.13199,-1.16550,-1.16402,-1.17932,-1.17405,-1.15184,
     8-1.20924,-1.29157,-1.34831,-1.38571,-1.42632,-1.43812,-1.50800,
     X-1.62119,-1.70590,-1.86161,-2.00714,-2.11745,-2.25960,-2.34777,
     X-2.44254,-2.64264,-2.83979,-3.04320,-3.29364,-3.41153,-3.49359,
     X-3.60572,-3.67873,-3.78090,-3.85398,-3.88200,-3.83753,-3.77740,
     X-3.85401,-3.79646,-3.72746,-3.70451,-3.59083,-3.70223,-3.88363,
     X-4.03077,-4.20725,-4.19594,-4.16725,-4.13410,-4.16791,-4.12138,
     X-4.08875,-4.14355,-4.10163,-4.19018,-4.25695,-4.31184,-4.47906,
     X-4.51148,-4.57929,-4.59458,-4.62081,-4.83031,-5.02522,-5.15710,
     6-5.27403,-5.22837,-5.32058,-5.55260,-5.72630,-5.86735,-5.86402,
     X-5.84419,-5.89720,-6.15533,-6.51283,-6.98011,-7.28495,-7.08784,
     8-6.77605,-6.49215,-6.43947,-6.42083,-6.59354,-6.78419,-6.98883/
      DATA CA4H2O/
     X-7.11018,-6.93420,-6.83581,-6.87136,-6.96133,-7.28561,-8.27079/
C=H2O ====11545-13070
      DATA CB1H2O/
     X-8.59451,-9.45197,-8.33631,-8.21424,-6.89777,-6.27923,-5.89945,
     X-5.66364,-5.69459,-5.87082,-5.81185,-5.70141,-5.45890,-5.24048,
     X-5.30703,-5.32430,-5.18694,-5.03410,-4.82279,-4.72208,-4.55097,
     X-4.36284,-4.20326,-4.04534,-4.05883,-4.01183,-3.93857,-3.83212,
     X-3.66113,-3.56021,-3.45969,-3.38518,-3.33373,-3.32721,-3.34771,
     X-3.35412,-3.34584,-3.22701,-3.14293,-3.09481,-3.05706,-3.13587,
     X-3.18156,-3.26336,-3.34559,-3.38498,-3.39054,-3.33695,-3.34959,
     8-3.36191,-3.53258,-3.66238,-3.68946,-3.69155,-3.52990,-3.48606,
     X-3.41160,-3.34144,-3.31933,-3.26341,-3.22609,-3.18298,-3.12800,
     X-3.02166,-2.93903,-2.84135,-2.69864,-2.63582,-2.60853,-2.59699,
     X-2.64799,-2.71846,-2.70856,-2.67797,-2.67978,-2.58432,-2.57052,
     X-2.57883,-2.48977,-2.47541,-2.43446,-2.39253,-2.42823,-2.44830,
     X-2.49704,-2.54904,-2.54865,-2.51223,-2.39407,-2.28582,-2.22341,
     X-2.18280,-2.17308,-2.15234,-2.10486,-2.08564,-2.08578,-2.09615,
     X-2.11136,-2.10200,-2.06832,-2.04134,-2.00194,-1.95185,-1.92025,
     6-1.85931,-1.85988,-1.91696,-2.01129,-2.15097,-2.20539,-2.21302,
     X-2.22309,-2.24412,-2.30471,-2.33847,-2.25445,-2.08096,-1.85098,
     8-1.61538,-1.45841,-1.42089,-1.54484,-1.74234,-1.96839,-2.18038/
      DATA CB2H2O/
     X-2.28394,-2.31701,-2.24125,-2.05966,-1.88694,-1.78105,-1.69960,
     X-1.64107,-1.62909,-1.58168,-1.56599,-1.59412,-1.56739,-1.56346,
     X-1.54456,-1.55619,-1.61000,-1.67072,-1.75312,-1.82511,-1.87588,
     X-1.89436,-1.94377,-1.96038,-2.02291,-2.14131,-2.19637,-2.27114,
     X-2.33418,-2.36152,-2.44688,-2.53819,-2.61011,-2.69105,-2.73774,
     X-2.76700,-2.82031,-2.85910,-2.88525,-2.95422,-2.99210,-3.06247,
     X-3.12280,-3.12274,-3.13972,-3.09998,-3.11771,-3.10670,-3.00116,
     8-2.91302,-2.75838,-2.66379,-2.65726,-2.62212,-2.59431,-2.55351,
     X-2.49874,-2.47486,-2.52563,-2.54608,-2.54311,-2.54938,-2.49038,
     X-2.49664,-2.52688,-2.58688,-2.67000,-2.71830,-2.77113,-2.80244,
     X-2.84845,-2.87062,-2.83368,-2.69338,-2.52225,-2.40864,-2.34429,
     X-2.40612,-2.55941,-2.73915,-2.94409,-3.12344,-3.27308,-3.32104,
     X-3.27077,-3.13129,-2.92504,-2.78515,-2.71007,-2.66733,-2.62518,
     X-2.62279,-2.59906,-2.56745,-2.59548,-2.53657,-2.50849,-2.47640,
     X-2.46021,-2.53747,-2.62220,-2.76449,-2.88041,-2.96357,-3.02153,
     6-3.06178,-3.14581,-3.25318,-3.44687,-3.69634,-3.90497,-4.09399,
     X-4.22346,-4.29749,-4.51957,-4.79572,-5.03708,-5.27140,-5.34657,
     8-5.44757,-5.52207,-5.57087,-5.64385,-5.80294,-5.90763,-5.94797/
      DATA CB3H2O/
     X-5.85439,-5.62721,-5.45121,-5.40324,-5.38540,-5.39409,-5.59404,
     X-5.69955,-5.76877,-5.86764,-5.78129,-5.88887,-6.12206,-6.37505,
     X-6.85575,-7.13884,-6.98622,-6.96112,-6.84830,-6.72456,-6.67530,
     X-6.65251,-6.66033,-6.88151,-7.11199,-7.33922,-7.61766,-7.66585,
     X-7.87464,-8.59519,-9.04047,-9.30602,-9.51273,-8.93688,-9.43151,
     X-8.84005,-10.0000,-9.65151,-8.98420,-10.0000,-9.43151,-9.68331,
     X-10.0000,-9.43067,-9.90527,-10.0000,-9.98810,-9.65151,-9.74384,
     8-9.20004,-10.0000,-9.20004,-10.0000,-10.0000/
C=H2O ====13075-14860
      DATA CC1H2O/
     X-9.85239,-8.09585,-7.66916,-7.89183,-8.46587,-8.79342,-8.93440,
     X-8.68356,-8.83423,-8.01626,-7.91911,-8.27604,-9.44864,-9.69462,
     X-10.0000,-9.69462,-8.31857,-7.91867,-7.86404,-8.32240,-8.68705,
     X-9.61515,-9.25284,-8.68705,-8.28789,-7.63730,-8.25919,-10.0000,
     X-9.51758,-10.0000,-10.0000,-9.51758,-8.05261,-7.76848,-8.24255,
     X-9.34171,-9.19941,-8.56505,-7.78955,-7.23750,-6.64136,-6.41854,
     X-6.14335,-5.86704,-5.70840,-5.48179,-5.23814,-5.00650,-4.80407,
     8-4.69986,-4.70404,-4.80846,-4.99355,-5.19947,-5.33300,-5.30550,
     X-5.17017,-5.05309,-4.95685,-4.79959,-4.65496,-4.54077,-4.44407,
     X-4.43768,-4.47167,-4.40429,-4.30236,-4.22549,-4.15283,-4.06900,
     X-3.99244,-3.87562,-3.76949,-3.78198,-3.77003,-3.67364,-3.52061,
     X-3.34459,-3.20426,-3.15993,-3.13851,-3.09692,-3.07747,-3.02936,
     X-3.02192,-2.96720,-2.90584,-2.79069,-2.65042,-2.62072,-2.53133,
     X-2.50554,-2.48745,-2.41611,-2.43532,-2.40913,-2.38723,-2.33337,
     X-2.21812,-2.15072,-2.08313,-2.06151,-2.10585,-2.13670,-2.18757,
     6-2.23995,-2.26992,-2.34229,-2.38059,-2.38087,-2.33246,-2.21618,
     X-2.14795,-2.12707,-2.09130,-2.05043,-1.95550,-1.77077,-1.66044,
     8-1.58224,-1.51702,-1.54004,-1.54074,-1.53712,-1.54938,-1.52352/
      DATA CC2H2O/
     X-1.49404,-1.51985,-1.57774,-1.64393,-1.65332,-1.56238,-1.45105,
     X-1.39264,-1.40635,-1.46167,-1.50365,-1.47751,-1.47268,-1.45992,
     X-1.46654,-1.50223,-1.47576,-1.47730,-1.46977,-1.45630,-1.44490,
     X-1.43097,-1.43664,-1.49313,-1.63372,-1.81848,-1.97938,-2.06009,
     X-2.10124,-2.02376,-1.95095,-1.86835,-1.70161,-1.54835,-1.37614,
     X-1.25130,-1.17408,-1.17670,-1.19790,-1.21344,-1.27252,-1.28030,
     X-1.31031,-1.34321,-1.34056,-1.35264,-1.38137,-1.44648,-1.56735,
     8-1.72096,-1.88960,-2.06793,-2.19583,-2.29229,-2.34400,-2.34120,
     X-2.36407,-2.39688,-2.45450,-2.53132,-2.58327,-2.60585,-2.60773,
     X-2.60606,-2.64243,-2.71853,-2.78182,-2.84459,-2.83665,-2.78522,
     X-2.71157,-2.61628,-2.53314,-2.45692,-2.41679,-2.41535,-2.45500,
     X-2.51120,-2.57918,-2.62854,-2.63617,-2.61204,-2.53833,-2.43992,
     X-2.37490,-2.34880,-2.34476,-2.36650,-2.36553,-2.34094,-2.33633,
     X-2.30483,-2.26933,-2.25141,-2.22841,-2.27610,-2.33673,-2.37913,
     X-2.44271,-2.48571,-2.57146,-2.64200,-2.61103,-2.47198,-2.27897,
     6-2.15874,-2.06557,-2.05371,-2.06333,-2.04641,-2.04429,-2.01040,
     X-2.00804,-1.99416,-2.05499,-2.09948,-2.09706,-2.10517,-2.01267,
     8-1.99934,-2.03719,-2.12114,-2.29537,-2.44295,-2.55926,-2.66007/
      DATA CC3H2O/
     X-2.73808,-2.75972,-2.78032,-2.67031,-2.44995,-2.27133,-2.11654,
     X-2.02598,-2.01402,-2.04264,-2.04511,-2.02975,-2.00687,-1.94840,
     X-1.93975,-1.97104,-2.01554,-2.09336,-2.15829,-2.26705,-2.40356,
     X-2.55216,-2.78858,-3.00697,-3.22778,-3.44413,-3.55439,-3.66412,
     X-3.73884,-3.92224,-4.18922,-4.41150,-4.55122,-4.48637,-4.29339,
     X-4.19248,-4.28419,-4.41178,-4.60369,-4.81645,-4.83716,-4.93818,
     X-4.87772,-4.65255,-4.40133,-4.14378,-4.05431,-4.02425,-4.04257,
     8-4.11794,-4.12153,-4.16374,-4.17459,-4.10274,-4.04733,-4.00176,
     X-4.01760,-4.13393,-4.29085,-4.38409,-4.39975,-4.33916,-4.31515,
     X-4.35426,-4.44628,-4.51842,-4.52596,-4.53399,-4.54590,-4.63864,
     X-4.75657,-4.86504,-4.95580,-5.03365,-5.14879,-5.33868,-5.62972,
     X-5.92584,-6.30299,-6.62922,-6.70773,-6.97810,-7.35919,-7.64909,
     X-8.62765,-8.55378,-7.76305,-7.47054,-7.07789,-7.11538,-7.34052,
     X-7.75694,-9.17126,-10.0000,-9.86547,-8.71424,-8.66552,-8.31602,
     X-8.41339,-7.92192,-8.66385,-8.99856,-9.65437,-9.36822,-9.46517,
     6-9.43986/
C=H2O ====14865-16045
      DATA CD1H2O/
     X-8.65941,-10.0000,-10.0000,-8.82641,-8.56244,-7.93689,-7.68823,
     X-7.55818,-7.05113,-6.76446,-6.49313,-6.24749,-6.12617,-6.05220,
     X-6.13798,-6.07909,-5.86845,-5.69141,-5.50496,-5.48376,-5.56108,
     X-5.42768,-5.29615,-5.10664,-4.88111,-4.78669,-4.62385,-4.52174,
     X-4.49073,-4.45792,-4.54129,-4.54480,-4.51341,-4.47258,-4.27643,
     X-4.18091,-4.09557,-4.04222,-4.11247,-4.14851,-4.16970,-4.11065,
     X-4.04809,-4.00745,-3.99879,-4.07978,-4.12451,-4.19723,-4.17393,
     8-4.09022,-4.02101,-3.87998,-3.79109,-3.66411,-3.50066,-3.40580,
     X-3.32713,-3.30194,-3.35131,-3.35137,-3.29933,-3.20658,-3.06263,
     X-2.97995,-2.98759,-2.99176,-3.00756,-2.97359,-2.85849,-2.81640,
     X-2.77094,-2.75469,-2.77297,-2.71165,-2.69187,-2.64524,-2.60542,
     X-2.60059,-2.57842,-2.59991,-2.58577,-2.60792,-2.66006,-2.70803,
     X-2.79094,-2.81048,-2.79532,-2.79499,-2.84578,-2.90638,-2.96270,
     X-2.90633,-2.71535,-2.54313,-2.37822,-2.31125,-2.35246,-2.49011,
     X-2.68215,-2.83136,-2.96357,-2.95873,-2.90544,-2.84387,-2.70352,
     6-2.58329,-2.49207,-2.41735,-2.35522,-2.30279,-2.25786,-2.22067,
     X-2.20741,-2.19735,-2.20181,-2.22358,-2.27247,-2.33737,-2.39631,
     8-2.45029,-2.49867,-2.56939,-2.64313,-2.77129,-2.92580,-3.05513/
      DATA CD2H2O/
     X-3.23728,-3.31415,-3.33588,-3.39544,-3.43947,-3.57455,-3.69955,
     X-3.77227,-3.76260,-3.70753,-3.70942,-3.73899,-3.82827,-3.93052,
     X-4.10437,-4.24931,-4.35000,-4.42069,-4.25644,-4.21454,-4.17061,
     X-4.11168,-4.16038,-4.16686,-4.19465,-4.23251,-4.27305,-4.21672,
     X-4.13963,-4.07622,-3.97332,-3.96263,-3.95541,-3.97392,-4.03776,
     X-4.07778,-4.01771,-3.87070,-3.70710,-3.59495,-3.62243,-3.69528,
     X-3.76171,-3.76582,-3.65793,-3.61555,-3.59708,-3.63113,-3.63691,
     8-3.57465,-3.55435,-3.47507,-3.49075,-3.53253,-3.57495,-3.68837,
     X-3.68628,-3.68771,-3.64979,-3.60831,-3.56633,-3.48250,-3.37856,
     X-3.22908,-3.14286,-3.11346,-3.13691,-3.26625,-3.44333,-3.64611,
     X-3.86925,-4.08285,-4.22794,-4.25115,-4.14282,-3.85944,-3.59027,
     X-3.43514,-3.31856,-3.24442,-3.22555,-3.18795,-3.20363,-3.30589,
     X-3.41248,-3.60718,-3.70563,-3.65431,-3.57332,-3.47347,-3.47521,
     X-3.53388,-3.72003,-3.97569,-4.31048,-4.87330,-5.39648,-6.27322,
     X-8.18185,-8.07588,-8.20933,-8.60643,-8.83713,-9.01727,-9.15690,
     6-9.41970,-9.51520,-9.63843,-9.87539,-9.94314,-10.0000/
C=H2O ====16340-17860
      DATA CE1H2O/
     X-10.0000,-9.99542,-9.97748,-9.94374,-9.93287,-9.90450,-9.85082,
     X-9.82140,-9.73549,-9.64536,-9.59412,-9.54635,-9.26735,-9.23243,
     X-9.05763,-8.93240,-8.74549,-8.52992,-8.25637,-8.13836,-7.66071,
     X-7.35897,-7.37375,-7.09925,-6.98326,-6.89298,-6.79545,-6.97172,
     X-6.67558,-6.37369,-6.21189,-5.94606,-5.84975,-5.83536,-5.82878,
     X-5.78456,-5.68334,-5.44809,-5.28421,-5.06970,-4.89514,-4.80192,
     X-4.73588,-4.78558,-4.78127,-4.73462,-4.54889,-4.33093,-4.18543,
     8-4.09190,-4.11204,-4.13402,-4.13401,-3.97210,-3.79621,-3.65860,
     X-3.55511,-3.57549,-3.57633,-3.53833,-3.46143,-3.34082,-3.23729,
     X-3.17300,-3.14437,-3.10547,-3.05061,-2.96941,-2.86694,-2.79500,
     X-2.75350,-2.75307,-2.77146,-2.79530,-2.76451,-2.68758,-2.63931,
     X-2.57797,-2.58894,-2.59717,-2.52817,-2.47282,-2.42360,-2.45382,
     X-2.56145,-2.61304,-2.59963,-2.52689,-2.46472,-2.46461,-2.45407,
     X-2.39432,-2.25523,-2.14408,-2.05525,-2.01888,-2.07413,-2.12889,
     X-2.25990,-2.39692,-2.48925,-2.54855,-2.53415,-2.54460,-2.50455,
     6-2.46921,-2.42259,-2.28066,-2.22625,-2.17393,-2.13289,-2.19687,
     X-2.21326,-2.23949,-2.27620,-2.26819,-2.29009,-2.29281,-2.25201,
     8-2.17355,-2.07947,-2.03121,-2.01967,-2.04954,-2.08143,-2.06833/
      DATA CE2H2O/
     X-2.05240,-2.05599,-2.06967,-2.12334,-2.21510,-2.29897,-2.40035,
     X-2.52428,-2.62702,-2.73003,-2.87671,-2.99894,-3.10548,-3.25316,
     X-3.32982,-3.39709,-3.53992,-3.63406,-3.74020,-3.92706,-3.96893,
     X-3.93910,-3.93559,-3.82934,-3.82006,-3.87551,-3.89939,-3.94509,
     X-3.95617,-3.96332,-3.96114,-3.99122,-4.01273,-4.01717,-4.02888,
     X-4.04697,-4.10112,-4.14864,-4.27169,-4.32135,-4.33175,-4.41165,
     X-4.36331,-4.39914,-4.42505,-4.40381,-4.48901,-4.44885,-4.38473,
     8-4.32458,-4.19760,-4.16511,-4.15683,-4.14102,-4.11365,-4.10673,
     X-4.13026,-4.13652,-4.19636,-4.19684,-4.14832,-4.05676,-3.96205,
     X-3.90165,-3.84404,-3.86524,-3.83773,-3.69609,-3.55481,-3.42043,
     X-3.33841,-3.37637,-3.44611,-3.49193,-3.52932,-3.44601,-3.36757,
     X-3.31227,-3.23777,-3.21254,-3.19842,-3.22310,-3.28352,-3.27914,
     X-3.23481,-3.12437,-3.04729,-3.06777,-3.09818,-3.19530,-3.24569,
     X-3.24974,-3.30729,-3.27728,-3.25317,-3.22055,-3.15996,-3.17334,
     X-3.17694,-3.12288,-3.04593,-2.99049,-2.98361,-3.06492,-3.19818,
     6-3.31628,-3.42190,-3.47775,-3.55095,-3.56669,-3.53409,-3.38883,
     X-3.17115,-3.00955,-2.89158,-2.83770,-2.86055,-2.86096,-2.83436,
     8-2.82886,-2.78602,-2.80289,-2.85454,-2.89629,-2.99573,-3.11206/
      DATA CE3H2O/
     X-3.27394,-3.47183,-3.64849,-3.79741,-3.91130,-4.08705,-4.24317,
     X-4.41275,-4.55729,-4.55082,-4.66958,-4.82149,-4.94204,-5.13772,
     X-5.22105,-5.20710,-5.18691,-5.09729,-5.03217,-4.96344,-4.97810,
     X-5.03506,-5.05380,-5.08007,-5.10835,-5.13285,-5.24491,-5.44530,
     X-5.65236,-5.80563,-5.75192,-5.58691,-5.38023,-5.31721,-5.30923,
     X-5.34087,-5.39044,-5.38089,-5.43438,-5.52124,-5.79590,-6.25048,
     X-6.78272,-8.29899,-8.50913,-8.77871,-8.91512,-9.13453,-9.37455,
     8-9.56578,-9.71290,-9.89385,-10.0000/
      END
      BLOCK DATA CPO3
C>    BLOCK DATA
C
C     C' FOR O3
      COMMON /O3/  C11O3(  41),
     +             C21O3( 126),C22O3(  27),
     +             C31O3( 126),C32O3(   8),
     +             C41O3(  36),
     +             C51O3(  83)
C=O3  ====C' FOR    5 BAND MODELS
C=O3  ====    0-  200
      DATA C11O3/
     1 -2.0427, -1.8966, -1.6263, -1.3896, -1.2170, -1.0996, -1.0214,
     2 -0.9673, -0.9249, -0.8896, -0.8612, -0.8417, -0.8360, -0.8483,
     3 -0.8785, -0.9273, -0.9932, -1.0720, -1.1639, -1.2662, -1.3771,
     4 -1.4976, -1.6274, -1.7712, -1.9289, -2.1027, -2.2948, -2.4987,
     5 -2.7321, -2.9992, -3.3045, -3.6994, -4.1022, -4.6467, -5.1328,
     6 -5.6481, -6.1634, -6.6787, -7.1940, -7.7093, -8.0000/
C=O3  ====  515- 1275
      DATA C21O3/
     1 -7.9274, -7.6418, -7.3562, -7.0706, -6.7850, -6.4994, -6.2138,
     2 -5.9282, -5.6426, -5.3570, -5.0714, -4.7858, -4.5002, -4.2146,
     3 -3.9290, -3.6213, -3.3407, -3.0722, -2.8226, -2.5914, -2.3778,
     4 -2.1823, -2.0057, -1.8456, -1.6991, -1.5659, -1.4436, -1.3323,
     5 -1.2319, -1.1407, -1.0550, -0.9733, -0.9033, -0.8584, -0.8527,
     6 -0.8838, -0.9219, -0.9360, -0.9025, -0.8402, -0.7913, -0.7794,
     7 -0.8123, -0.8750, -0.9484, -1.0206, -1.0864, -1.1520, -1.2202,
     8 -1.2928, -1.3745, -1.4641, -1.5611, -1.6669, -1.7816, -1.9051,
     9 -2.0383, -2.1796, -2.3312, -2.4906, -2.6569, -2.8354, -3.0179,
     $ -3.2121, -3.4106, -3.6208, -3.8332, -4.0584, -4.2854, -4.4979,
     1 -4.7175, -4.9109, -5.1246, -5.3344, -5.5442, -5.7540, -5.9638,
     2 -6.1736, -6.3834, -6.5932, -6.8030, -7.0128, -6.9011, -6.2590,
     3 -5.8119, -5.1603, -4.3327, -3.6849, -3.1253, -2.6304, -2.1903,
     4 -1.8019, -1.4585, -1.1533, -0.8770, -0.6166, -0.3630, -0.1102,
     5  0.1336,  0.3525,  0.5326,  0.6678,  0.7510,  0.7752,  0.7826,
     6  0.7874,  0.8006,  0.8241,  0.7614,  0.5662,  0.1949, -0.2770,
     7 -0.6199, -0.8347, -0.9586, -1.0168, -1.0501, -1.0816, -1.0980,
     8 -1.0833, -1.0424, -0.9972, -0.9724, -0.9855, -1.0365, -1.1187/
      DATA C22O3/
     1 -1.2150, -1.3142, -1.4103, -1.4998, -1.5933, -1.6938, -1.8061,
     2 -1.9332, -2.0737, -2.2279, -2.3966, -2.5787, -2.7755, -2.9855,
     3 -3.2090, -3.4465, -3.6967, -3.9633, -4.2461, -4.5502, -4.8912,
     4 -5.2845, -5.7654, -6.4194, -6.9288, -7.4382, -7.9476/
C=O3  ==== 1630- 2295
      DATA C31O3/
     1 -8.0000, -7.5432, -6.9273, -6.3115, -5.5431, -4.9563, -4.4640,
     2 -4.0371, -3.6533, -3.3069, -2.9877, -2.7042, -2.4507, -2.2355,
     3 -2.0651, -1.9477, -1.8705, -1.8422, -1.8235, -1.7782, -1.7367,
     4 -1.7012, -1.7208, -1.8353, -2.0331, -2.3077, -2.5996, -2.7517,
     5 -2.7263, -2.6671, -2.6415, -2.6449, -2.6613, -2.6589, -2.6083,
     6 -2.5250, -2.4529, -2.4157, -2.4298, -2.4906, -2.5823, -2.6873,
     7 -2.7808, -2.8612, -2.9303, -3.0022, -3.0873, -3.1844, -3.2929,
     8 -3.4158, -3.5361, -3.6710, -3.8062, -3.9520, -4.1140, -4.2635,
     9 -4.4395, -4.6138, -4.8372, -5.0837, -5.3302, -5.3665, -5.4358,
     $ -5.0651, -4.8416, -4.5293, -4.2547, -4.0039, -3.7818, -3.5850,
     1 -3.4091, -3.2509, -3.0934, -2.9485, -2.8055, -2.6705, -2.5482,
     2 -2.4362, -2.3380, -2.2486, -2.1645, -2.0834, -2.0035, -1.9081,
     3 -1.7681, -1.5768, -1.3615, -1.1463, -0.9482, -0.7800, -0.6336,
     4 -0.5092, -0.4105, -0.3495, -0.3274, -0.3133, -0.3023, -0.2859,
     5 -0.3055, -0.4374, -0.6972, -1.1064, -1.4904, -1.9687, -2.4498,
     7 -2.5971, -2.5220, -2.4301, -2.3467, -2.2901, -2.2746, -2.3021,
     7 -2.3635, -2.4420, -2.5088, -2.5485, -2.5617, -2.5656, -2.5771,
     8 -2.6134, -2.6822, -2.7885, -2.9379, -3.1200, -3.3260, -3.5464/
      DATA C32O3/
     1 -3.7736, -4.0311, -4.3651, -4.7794, -5.5152, -6.1240, -7.2193,
     2 -8.0000/
C=O3  ==== 2670- 2845
      DATA C41O3/
     1 -7.9721, -7.6118, -7.2515, -6.8913, -6.5310, -6.1707, -5.8105,
     2 -5.4502, -5.0899, -4.7297, -4.3694, -3.9462, -3.6022, -3.2886,
     3 -3.0234, -2.7863, -2.5797, -2.4073, -2.2760, -2.1894, -2.1359,
     4 -2.1160, -2.0808, -2.0151, -1.9666, -1.9409, -1.9868, -2.1450,
     5 -2.3965, -2.8042, -3.5500, -4.8275, -5.6378, -6.4482, -7.2585,
     6 -8.0000/
C=O3  ==== 2850- 3260
      DATA C51O3/
     1 -8.0000, -7.6278, -7.2556, -6.8834, -6.5111, -6.1389, -5.7667,
     2 -5.3945, -5.0223, -4.6501, -4.2779, -3.9056, -3.5334, -3.3828,
     3 -3.2452, -3.1411, -3.0403, -2.9428, -2.8436, -2.7573, -2.6853,
     4 -2.6040, -2.5218, -2.4121, -2.3547, -2.1970, -2.0668, -1.9121,
     5 -1.7617, -1.6153, -1.4688, -1.4022, -1.3447, -1.2669, -1.1902,
     6 -1.1805, -1.1707, -1.1609, -1.1609, -1.1805, -1.1999, -1.4214,
     7 -1.6348, -1.7519, -1.9730, -2.2078, -2.4608, -2.5337, -2.5923,
     8 -2.6616, -2.6384, -2.6271, -2.6154, -2.5570, -2.4983, -2.4480,
     9 -2.3890, -2.3663, -2.3431, -2.3314, -2.3200, -2.3200, -2.3314,
     $ -2.3431, -2.3547, -2.3777, -2.4004, -2.5218, -2.6499, -2.7694,
     1 -2.9057, -3.0286, -3.1543, -3.3696, -3.6053, -4.1977, -4.7811,
     2 -5.2933, -5.7554, -6.4542, -7.0239, -7.5937, -8.0000/
      END
      BLOCK DATA WVBNRG
C>    BLOCK DATA
C     WAVENUMBER-LOW AND WAVENUMBER-HIGH SPECIFY A BAND REGION
C     FOR A MOLECULAR ABSORBER.
C     THE UNIT FOR WAVENUMBER IS 1/CM.
C     -999 IS AN INDICATOR TO INDICATE THE END OF ABSORPTION BANDS
C     FOR ANY SPECIFIC ABSORBER.
      COMMON /WNLOHI/
     L   IWLH2O(15),IWLO3 ( 6),IWLCO2(11),IWLCO ( 4),IWLCH4( 5),
     L   IWLN2O(12),IWLO2 ( 7),IWLNH3( 3),IWLNO ( 2),IWLNO2( 4),
     L   IWLSO2( 5),
     H   IWHH2O(15),IWHO3 ( 6),IWHCO2(11),IWHCO ( 4),IWHCH4( 5),
     H   IWHN2O(12),IWHO2 ( 7),IWHNH3( 3),IWHNO ( 2),IWHNO2( 4),
     H   IWHSO2( 5)
C
      DATA IWLH2O/   0,    350,   1005,   1645,   2535,   3425,   4315,
     L    6155,   8005,   9620,  11545,  13075,  14865,  16340,   -999/
      DATA IWHH2O/ 345,   1000,   1640,   2530,   3420,   4310,   6150,
     H    8000,   9615,  11540,  13070,  14860,  16045,  17860,   -999/
C
      DATA IWLO3 /   0,    515,   1630,   2670,   2850,   -999/
      DATA IWHO3 / 200,   1275,   2295,   2845,   3260,   -999/
C
      DATA IWLCO2/ 425,    840,   1805,   3070,   3760,   4530,   5905,
     L    7395,   8030,   9340,   -999/
      DATA IWHCO2/ 835,   1440,   2855,   3755,   4065,   5380,   7025,
     H    7785,   8335,   9670,   -999/
C
      DATA IWLCO /   0,   1940,   4040,   -999/
      DATA IWHCO / 175,   2285,   4370,   -999/
C
      DATA IWLCH4/1065,   2345,   4110,   5865,   -999/
      DATA IWHCH4/1775,   3230,   4690,   6135,   -999/
C
      DATA IWLN2O/   0,    490,    865,   1065,   1545,   2090,   2705,
     L    3245,   4260,   4540,   4910,   -999/
      DATA IWHN2O/ 120,    775,    995,   1385,   2040,   2655,   2865,
     H    3925,   4470,   4785,   5165,   -999/
C
      DATA IWLO2 /   0,   7650,   9235,  12850,  14300,  15695,   -999/
      DATA IWHO2 / 265,   8080,   9490,  13220,  14600,  15955,   -999/
C
      DATA IWLNH3/   0,    390,   -999/
      DATA IWHNH3/ 385,   2150,   -999/
C
      DATA IWLNO /1700,   -999/
      DATA IWHNO /2005,   -999/
C
      DATA IWLNO2/ 580,   1515,   2800,   -999/
      DATA IWHNO2/ 925,   1695,   2970,   -999/
C
      DATA IWLSO2/   0,    400,    950,   2415,   -999/
      DATA IWHSO2/ 185,    650,   1460,   2580,   -999/
      END
      BLOCK DATA CPUMIX
C>    BLOCK DATA
C
C     C' FOR UNIFORMLY MIXED GASES (CO2, CO, CH4, N2O, O2)
      COMMON /UFMIX1/
     +        C11CO2( 83),
     +        C21CO2(121),
     +        C31CO2(126),C32CO2( 85),
     +        C41CO2(126),C42CO2( 12),
     +        C51CO2( 62),
     +        C61CO2(126),C62CO2( 45),
     +        C71CO2(126),C72CO2( 99),
     +        C81CO2(126),C82CO2( 82)
      COMMON /UFMIX2/
     +        C11CO(  36),
     +        C21CO( 126),C22CO(  11),
     +        C11CH4(126),C12CH4(126),C13CH4(126),C14CH4(115),
     +        C11N2O( 25),
     +        C21N2O(126),C22N2O(126),C23N2O(112),
     +        C31N2O(126),C32N2O(126),C33N2O( 63),
     +        C11O2(  54),
     +        C21O2( 126),C22O2( 126),C23O2(  76)
C=CO2 ====C' FOR    8 BAND MODELS
C=CO2 ====  425-  835
      DATA C11CO2/
     1 -9.8495, -9.6484, -9.4472, -9.2461, -9.0449, -8.9544, -8.6127,
     2 -8.4076, -8.2710, -8.0391, -7.9485, -7.9638, -7.7849, -7.6278,
     3 -7.1418, -6.7823, -6.3826, -6.0323, -5.7501, -5.5249, -5.3304,
     4 -5.0105, -4.7703, -4.5714, -4.3919, -4.2974, -4.1370, -3.8761,
     5 -3.5936, -3.2852, -3.0016, -2.7303, -2.4868, -2.2741, -2.0936,
     6 -1.9424, -1.8092, -1.6843, -1.5372, -1.3803, -1.2043, -0.9930,
     7 -0.7724, -0.5509, -0.3465, -0.1785, -0.0470,  0.0449,  0.1114,
     8  0.1367,  0.0910,  0.0066, -0.1269, -0.2994, -0.4934, -0.7101,
     9 -0.9087, -1.1004, -1.2694, -1.4064, -1.5622, -1.6810, -1.7841,
     $ -1.8973, -2.0274, -2.2079, -2.4264, -2.6763, -2.9312, -3.1896,
     1 -3.4262, -3.5979, -3.7051, -3.7372, -3.7983, -3.9154, -4.0520,
     2 -4.2567, -4.4661, -4.6670, -4.9226, -5.2203, -5.5597/
C=CO2 ====  840- 1440
      DATA C21CO2/
     1 -5.6403, -5.7039, -5.7674, -5.8310, -5.8948, -5.9503, -6.0217,
     2 -6.0392, -5.9855, -5.8620, -5.6834, -5.5083, -5.3473, -5.2028,
     3 -5.0799, -4.9628, -4.8379, -4.7032, -4.5584, -4.4213, -4.3198,
     4 -4.2786, -4.2843, -4.3099, -4.3210, -4.2769, -4.2229, -4.2179,
     5 -4.2950, -4.4789, -4.7550, -5.0902, -5.4329, -5.6689, -5.6608,
     6 -5.4582, -5.1969, -4.9419, -4.7106, -4.5084, -4.3409, -4.2211,
     7 -4.1563, -4.1259, -4.1108, -4.0803, -4.0211, -3.9824, -4.0053,
     8 -4.1221, -4.3504, -4.6741, -5.0826, -5.5857, -6.2301, -7.0829,
     9 -8.1344, -8.8601, -9.0457, -9.1231, -9.0728, -9.1413, -9.1221,
     $ -9.1882, -9.2752, -9.2237, -9.3604, -9.3058, -9.5455, -9.5567,
     1 -9.3754, -8.7756, -8.0904, -7.4827, -6.9585, -6.5095, -6.1194,
     2 -5.7824, -5.4910, -5.2532, -5.0840, -4.9920, -4.9577, -4.9638,
     3 -4.9741, -4.9555, -4.9466, -4.9774, -5.0719, -5.2558, -5.5213,
     4 -5.8633, -6.2877, -6.7878, -7.2602, -7.2940, -6.8524, -6.3372,
     5 -5.8854, -5.5065, -5.2011, -4.9776, -4.8471, -4.7885, -4.7783,
     6 -4.7815, -4.7538, -4.7228, -4.7259, -4.7860, -4.9231, -5.1270,
     7 -5.3831, -5.6849, -6.0351, -6.4437, -6.9160, -7.4815, -8.1437,
     8 -8.9449, -9.8564/
C=CO2 ==== 1805- 2855
      DATA C31CO2/
     1 -9.8903, -9.4365, -8.9826, -8.5288, -8.1184, -7.6555, -7.1673,
     2 -6.7226, -6.3423, -6.0410, -5.8154, -5.6519, -5.5186, -5.3859,
     3 -5.2279, -5.0238, -4.7865, -4.5343, -4.2846, -4.0560, -3.8717,
     4 -3.7624, -3.7231, -3.7335, -3.8312, -3.9854, -4.1930, -4.4895,
     5 -4.7394, -4.8892, -4.9499, -4.9392, -4.9787, -5.1129, -5.3330,
     6 -5.6093, -5.8862, -6.0581, -6.0274, -5.8356, -5.5989, -5.3738,
     7 -5.1661, -4.9472, -4.7020, -4.4354, -4.1439, -3.8561, -3.5944,
     8 -3.3694, -3.2100, -3.1041, -3.0411, -3.0471, -3.1077, -3.2305,
     9 -3.4274, -3.6115, -3.7542, -3.8666, -3.9338, -4.0079, -4.0962,
     $ -4.2142, -4.1433, -4.2870, -4.4796, -4.6618, -4.8204, -4.9499,
     1 -4.9862, -5.0171, -5.0282, -5.0580, -5.0398, -4.9465, -4.7816,
     2 -4.5538, -4.2975, -4.0286, -3.7528, -3.4715, -3.1899, -2.9041,
     3 -2.6127, -2.3212, -2.0435, -1.7894, -1.5531, -1.3382, -1.1515,
     4 -0.9990, -0.8833, -0.8006, -0.7227, -0.6288, -0.4977, -0.3249,
     5 -0.1349,  0.0576,  0.2487,  0.4386,  0.6260,  0.8081,  0.9681,
     6  1.0859,  1.1522,  1.1861,  1.2039,  1.2255,  1.2587,  1.2473,
     7  1.1457,  0.9139,  0.5250,  0.0173, -0.5796, -1.3944, -2.3841,
     8 -2.7244, -2.9264, -3.0689, -3.2120, -3.3353, -3.4510, -3.5566/
      DATA C32CO2/
     1 -3.6518, -3.7460, -3.8500, -3.9680, -4.0981, -4.2259, -4.3369,
     2 -4.4329, -4.5305, -4.6264, -4.7438, -4.8842, -5.0248, -5.1448,
     3 -5.2371, -5.2781, -5.3299, -5.3766, -5.4233, -5.4699, -5.5166,
     4 -5.5633, -5.6646, -5.7593, -5.8461, -5.9229, -5.9818, -6.0065,
     5 -5.9747, -5.8741, -5.7230, -5.5620, -5.4389, -5.3788, -5.3679,
     6 -5.3827, -5.3837, -5.3460, -5.3186, -5.3394, -5.4320, -5.6095,
     7 -5.8446, -6.0992, -6.3399, -6.5499, -6.7434, -6.9359, -7.1219,
     8 -7.2818, -7.3984, -7.4881, -7.5452, -7.5994, -7.6445, -7.6734,
     9 -7.6422, -7.5057, -7.2650, -6.9975, -6.7749, -6.6398, -6.5875,
     $ -6.5912, -6.6192, -6.6155, -6.5866, -6.5851, -6.6382, -6.7736,
     1 -7.0009, -7.2896, -7.6327, -7.9767, -8.2633, -8.4744, -8.5455,
     2 -8.5813, -8.6025, -8.6459, -8.8948, -9.1436, -9.3925, -9.6413,
     3 -9.8902/
C=CO2 ==== 3070- 3755
      DATA C41CO2/
     1 -9.8006, -9.5049, -9.1947, -8.7254, -8.4410, -8.1781, -8.0182,
     2 -7.9381, -7.8793, -7.7636, -7.5549, -7.2962, -7.0244, -6.7556,
     3 -6.4888, -6.2443, -6.0422, -5.9088, -5.8590, -5.8890, -5.9850,
     4 -6.0949, -6.1164, -6.0207, -5.8592, -5.7110, -5.6328, -5.6369,
     5 -5.7274, -5.9069, -6.1720, -6.5203, -6.9586, -7.4776, -8.0607,
     6 -8.5514, -8.7011, -8.4232, -7.9274, -7.6159, -7.3836, -7.1969,
     7 -7.0523, -6.7685, -6.4022, -6.0354, -5.7125, -5.4659, -5.3088,
     8 -5.2546, -5.2991, -5.3819, -5.4615, -5.4117, -5.2107, -5.0103,
     9 -4.8232, -4.7071, -4.6850, -4.7385, -4.8797, -5.1024, -5.4015,
     $ -5.7758, -6.2225, -6.6681, -6.9127, -6.8919, -6.6972, -6.5012,
     1 -6.3123, -6.1091, -5.8641, -5.5889, -5.3057, -5.0340, -4.7826,
     2 -4.5476, -4.3277, -4.1224, -3.9333, -3.7675, -3.6324, -3.5163,
     3 -3.4043, -3.2744, -3.1180, -2.9557, -2.8254, -2.7359, -2.6721,
     4 -2.6084, -2.5105, -2.3772, -2.2317, -2.0866, -1.9521, -1.8292,
     5 -1.7110, -1.5992, -1.4873, -1.3646, -1.2260, -1.0721, -0.9281,
     6 -0.8379, -0.8123, -0.8261, -0.8483, -0.8305, -0.7792, -0.7626,
     7 -0.8228, -0.9908, -1.2503, -1.5347, -1.7934, -1.9837, -2.0715,
     8 -2.0375, -1.8975, -1.6906, -1.4497, -1.2048, -0.9831, -0.8125/
      DATA C42CO2/
     1 -0.7157, -0.6707, -0.6532, -0.6297, -0.5706, -0.5263, -0.5489,
     2 -0.6857, -0.9793, -1.3962, -1.8673, -2.3655/
C=CO2 ==== 3760- 4065
      DATA C51CO2/
     1 -3.5436, -4.0424, -4.4084, -4.6848, -4.8663, -4.9516, -4.9790,
     2 -4.9923, -5.0207, -5.0596, -5.0958, -5.1018, -5.0636, -5.0354,
     3 -5.0546, -5.1454, -5.3274, -5.5863, -5.8889, -6.1770, -6.3555,
     4 -6.4096, -6.4371, -6.5112, -6.6680, -6.9183, -7.2418, -7.5827,
     5 -7.8704, -8.0551, -8.1705, -8.2500, -8.3554, -8.3961, -8.4354,
     6 -8.3920, -8.2785, -8.0499, -7.7437, -7.4130, -7.1153, -6.8861,
     7 -6.7422, -6.6786, -6.6774, -6.7053, -6.7090, -6.6794, -6.6055,
     8 -6.4827, -6.3454, -6.2401, -6.1992, -6.2676, -6.4833, -6.8490,
     9 -7.4310, -8.4606, -9.7364, -9.8771, -9.8840, -9.9559/
C=CO2 ==== 4530- 5380
      DATA C61CO2/
     1 -9.9489, -9.6003, -9.0910, -8.5793, -8.2059, -7.9099, -7.7157,
     2 -7.6145, -7.5964, -7.5942, -7.5256, -7.3190, -6.9986, -6.6884,
     3 -6.4102, -6.1769, -5.9882, -5.8421, -5.7499, -5.7201, -5.7189,
     4 -5.7108, -5.6669, -5.5955, -5.5686, -5.6287, -5.8000, -6.0855,
     5 -6.4398, -6.7793, -6.9427, -6.9205, -6.8363, -6.7059, -6.5272,
     6 -6.2903, -6.0085, -5.7224, -5.4722, -5.2772, -5.1501, -5.0768,
     7 -5.0219, -4.9579, -4.8555, -4.7213, -4.5868, -4.4594, -4.3387,
     8 -4.2219, -4.1002, -3.9812, -3.8876, -3.8207, -3.7673, -3.7120,
     9 -3.6223, -3.4912, -3.3444, -3.1983, -3.0732, -3.0262, -3.0078,
     $ -3.0123, -3.0213, -2.9957, -2.9261, -2.8770, -2.8887, -2.9853,
     1 -3.1609, -3.3643, -3.5468, -3.6759, -3.7488, -3.7704, -3.7535,
     2 -3.7113, -3.6368, -3.5277, -3.3812, -3.2020, -3.0043, -2.8020,
     3 -2.6122, -2.4524, -2.3405, -2.2838, -2.2521, -2.2319, -2.1960,
     4 -2.1562, -2.1732, -2.2913, -2.5476, -2.9382, -3.3966, -3.8525,
     5 -4.2541, -4.5682, -4.7376, -4.7524, -4.6733, -4.5170, -4.3123,
     6 -4.0891, -3.8565, -3.6218, -3.3909, -3.1785, -3.0100, -2.9105,
     7 -2.8588, -2.8286, -2.7912, -2.7207, -2.6729, -2.6858, -2.7745,
     8 -2.9414, -3.1445, -3.3617, -3.5954, -3.8508, -4.1739, -4.5122/
      DATA C62CO2/
     1 -4.8985, -5.3426, -5.8737, -6.4734, -7.0715, -7.5042, -7.6034,
     2 -7.5143, -7.4358, -7.4089, -7.3969, -7.3813, -7.3018, -7.1858,
     3 -7.0633, -6.9962, -6.9905, -7.0319, -7.1331, -7.2054, -7.1856,
     4 -7.0561, -6.7966, -6.4771, -6.1996, -5.9593, -5.7560, -5.5370,
     5 -5.2836, -5.0966, -4.9583, -4.9126, -5.0022, -5.1370, -5.3465,
     6 -5.6279, -5.9364, -6.3695, -6.9602, -7.6823, -8.2701, -8.6427,
     7 -9.0728, -9.5366, -9.9588/
C=CO2 ==== 5905- 7025
      DATA C71CO2/
     1 -9.9871, -9.6762, -9.3358, -8.9954, -8.5140, -8.2066, -7.9742,
     2 -7.8579, -7.8073, -7.7894, -7.7466, -7.7009, -7.6393, -7.5889,
     3 -7.5697, -7.5200, -7.3908, -7.1796, -6.9610, -6.7869, -6.6972,
     4 -6.6735, -6.6775, -6.6495, -6.5292, -6.3435, -6.1371, -5.9268,
     5 -5.7254, -5.5433, -5.4023, -5.3292, -5.3090, -5.3171, -5.3193,
     6 -5.2705, -5.2085, -5.1835, -5.2186, -5.3367, -5.5305, -5.7725,
     7 -6.0228, -6.2150, -6.2857, -6.2634, -6.2250, -6.2234, -6.2616,
     8 -6.2931, -6.2508, -6.0971, -5.8679, -5.6195, -5.3906, -5.1944,
     9 -5.0216, -4.8566, -4.6919, -4.5255, -4.3785, -4.2879, -4.2583,
     $ -4.2636, -4.2768, -4.2484, -4.1853, -4.1586, -4.2079, -4.3651,
     1 -4.6407, -5.0141, -5.4719, -6.0015, -6.5173, -6.7829, -6.6805,
     2 -6.4180, -6.0793, -5.7404, -5.4204, -5.1265, -4.8634, -4.6378,
     3 -4.4559, -4.3360, -4.2752, -4.2461, -4.2257, -4.1768, -4.1068,
     4 -4.0743, -4.1193, -4.2732, -4.5464, -4.9256, -5.4090, -6.0184,
     5 -6.7985, -7.7078, -8.3457, -8.5160, -8.6106, -8.8175, -9.1922,
     6 -9.6775, -9.7423, -9.1980, -8.4120, -7.7499, -7.1685, -6.6817,
     7 -6.2701, -5.9301, -5.6567, -5.4521, -5.3289, -5.2776, -5.2630,
     8 -5.2547, -5.2083, -5.1296, -5.0823, -5.0914, -5.1806, -5.3503/
      DATA C72CO2/
     1 -5.5600, -5.7877, -5.9936, -6.1720, -6.3801, -6.6371, -6.9964,
     2 -7.5010, -8.1628, -8.9951, -9.8931,-10.0000,-10.0000,-10.0000,
     3-10.0000,-10.0000,-10.0000,-10.0000,-10.0000, -9.4967, -8.9198,
     4 -8.5081, -8.1255, -7.8286, -7.5478, -7.1487, -6.7853, -6.5537,
     5 -6.3931, -6.4107, -6.5087, -6.6607, -6.9026, -7.2104, -7.4445,
     6 -7.6303, -7.6346, -7.4521, -7.2211, -7.0043, -6.7903, -6.5666,
     7 -6.3499, -6.1534, -5.9988, -5.9033, -5.8760, -5.8693, -5.8277,
     8 -5.7282, -5.6262, -5.5865, -5.6665, -5.9228, -6.3399, -7.0180,
     9 -8.4230,-10.0000,-10.0000,-10.0000, -9.4090, -8.8272, -8.3057,
     $ -7.8885, -7.5044, -7.1560, -6.8292, -6.5250, -6.2461, -5.9904,
     1 -5.7533, -5.5295, -5.3135, -5.1058, -4.9152, -4.7463, -4.6054,
     2 -4.4937, -4.3928, -4.2838, -4.1626, -4.0387, -3.9295, -3.8612,
     3 -3.8501, -3.8647, -3.8625, -3.8099, -3.7351, -3.7179, -3.8549,
     4 -4.2312, -4.7632, -5.4270, -6.4200, -8.1414, -9.0451, -9.5326,
     5 -9.8301/
C=CO2 ==== 7395- 7785, 8030- 8335, 9340- 9670
      DATA C81CO2/
     1 -9.9472, -9.8274, -8.9797, -8.4298, -7.8906, -7.4477, -7.0750,
     2 -6.7698, -6.5338, -6.3739, -6.2980, -6.2739, -6.2726, -6.2555,
     3 -6.1989, -6.1529, -6.1654, -6.2584, -6.4610, -6.7805, -7.2235,
     4 -7.8191, -8.5850, -9.6084,-10.0000,-10.0000, -9.9199, -9.1093,
     5 -8.4490, -7.9158, -7.4364, -7.0400, -6.6958, -6.4131, -6.1855,
     6 -6.0158, -5.9123, -5.8700, -5.8530, -5.8340, -5.7866, -5.7224,
     7 -5.7048, -5.7653, -5.9281, -6.2234, -6.6646, -7.2957, -8.2799,
     8 -9.9457,-10.0000,-10.0000,-10.0000,-10.0000,-10.0000,-10.0000,
     9-10.0000, -9.2766, -8.6201, -8.0764, -7.6374, -7.2752, -6.9802,
     $ -6.7578, -6.6163, -6.5546, -6.5392, -6.5397, -6.5132, -6.4531,
     1 -6.4161, -6.4482, -6.5683, -6.8086, -7.1762, -7.6772, -8.3574,
     2 -9.2188,-10.0000,-10.0000, -9.5350, -8.9686, -8.5329, -8.1920,
     3 -7.9237, -7.6797, -7.5039, -7.3667, -7.2856, -7.1969, -7.0745,
     4 -6.9330, -6.7926, -6.6818, -6.6144, -6.5643, -6.5183, -6.4910,
     5 -6.4481, -6.3567, -6.2177, -6.0566, -5.9096, -5.7975, -5.7093,
     6 -5.6165, -5.5127, -5.4124, -5.3426, -5.3061, -5.2648, -5.1864,
     7 -5.0876, -5.0226, -5.0397, -5.1905, -5.4858, -5.9101, -6.4851,
     8 -6.7862, -6.5368, -6.2765, -6.0398, -5.8260, -5.6397, -5.4799/
      DATA C82CO2/
     1 -5.3438, -5.2274, -5.1411, -5.0917, -5.0473, -4.9820, -4.9114,
     2 -4.8634, -4.8844, -5.0363, -5.3351, -5.7802, -6.5387, -8.3735,
     3 -9.9977, -9.7506, -9.1887, -8.6824, -8.3488, -8.0533, -7.8664,
     4 -7.7346, -7.6934, -7.6674, -7.6268, -7.5451, -7.4677, -7.4520,
     5 -7.5471, -7.7913, -8.1917, -8.8835,-10.0000,-10.0000,-10.0000,
     6-10.0000,-10.0000, -9.7234, -8.9969, -8.5776, -8.1737, -7.8640,
     7 -7.5729, -7.3186, -7.0973, -6.9131, -6.7782, -6.7073, -6.6768,
     8 -6.6303, -6.5406, -6.4509, -6.3950, -6.4345, -6.6270, -6.9507,
     9 -7.5028, -8.6428,-10.0000,-10.0000,-10.0000,-10.0000, -9.5303,
     $ -8.9369, -8.4952, -8.1465, -7.8567, -7.6177, -7.4249, -7.2876,
     1 -7.2206, -7.1948, -7.1552, -7.0773, -6.9884, -6.9402, -6.9839,
     2 -7.1773, -7.4999, -8.0643, -9.1480,-10.0000/
C=CO  ====C' FOR    2 BAND MODEL
C=CO  ====    0-  175
      DATA C11CO/
     1 -4.6868, -4.4127, -3.9461, -3.5662, -3.2921, -3.1081, -2.9807,
     2 -2.8977, -2.8580, -2.8461, -2.8587, -2.9029, -2.9646, -3.0480,
     3 -3.1589, -3.2836, -3.4277, -3.5993, -3.7963, -4.0164, -4.2799,
     4 -4.5750, -4.8722, -5.2741, -5.6819, -6.0799, -6.4828, -6.8857,
     5 -7.2886, -7.6915, -8.0944, -8.4973, -8.9002, -9.3031, -9.7060,
     6-10.0000/
C=CO  ==== 1940- 2285, 4040- 4370
      DATA C21CO/
     1-10.0000, -9.5312, -8.8977, -8.2642, -7.5767, -6.9972, -6.5408,
     2 -6.1219, -5.6734, -5.2658, -4.8686, -4.4918, -4.1423, -3.8133,
     3 -3.4998, -3.2104, -2.9443, -2.7138, -2.5084, -2.3109, -2.1245,
     4 -1.9387, -1.7608, -1.6054, -1.4733, -1.3594, -1.2540, -1.1480,
     5 -1.0341, -0.9216, -0.8189, -0.7235, -0.6362, -0.5549, -0.4856,
     6 -0.4401, -0.4268, -0.4657, -0.5571, -0.6573, -0.7404, -0.7523,
     7 -0.6601, -0.5380, -0.4211, -0.3367, -0.3167, -0.3320, -0.3753,
     8 -0.4489, -0.5438, -0.6653, -0.8052, -0.9690, -1.1506, -1.3522,
     9 -1.5791, -1.8248, -2.1073, -2.4246, -2.7877, -3.2152, -3.7089,
     $ -4.2832, -4.9518, -5.7251, -6.5319, -7.4879, -9.0885,-10.0000,
     1-10.0000, -9.5611, -9.0875, -8.6139, -7.9747, -7.5250, -7.1931,
     2 -6.8596, -6.5741, -6.2922, -6.0098, -5.7669, -5.5345, -5.3229,
     3 -5.1461, -4.9882, -4.8493, -4.7239, -4.6064, -4.5009, -4.4071,
     4 -4.3322, -4.2661, -4.1926, -4.0956, -3.9611, -3.7984, -3.6314,
     5 -3.4757, -3.3408, -3.2237, -3.1219, -3.0325, -2.9494, -2.8765,
     6 -2.8117, -2.7531, -2.7023, -2.6635, -2.6440, -2.6550, -2.7225,
     7 -2.8161, -2.9015, -2.9241, -2.8228, -2.6726, -2.5320, -2.4291,
     8 -2.3772, -2.3732, -2.3995, -2.4574, -2.5486, -2.6664, -2.8209/
      DATA C22CO/
     1 -3.0129, -3.2516, -3.5482, -3.9165, -4.3714, -4.9326, -5.6394,
     2 -6.5163, -7.6063, -9.3575,-10.0000/
C=CH4 ====C' FOR    1 BAND MODEL
C=CH4 ==== 1065- 1775, 2345- 3230, 4110- 4690, 5865- 6135
      DATA C11CH4/
     1-10.0000, -9.4577, -8.8866, -8.2246, -7.7940, -7.1734, -6.7965,
     2 -6.5695, -6.1929, -5.9169, -5.7452, -5.4731, -5.3001, -5.1872,
     3 -4.9672, -4.8474, -4.6939, -4.5210, -4.3377, -4.1346, -3.9322,
     4 -3.7339, -3.5077, -3.2719, -3.0296, -2.8124, -2.6199, -2.4479,
     5 -2.2502, -2.0541, -1.8800, -1.7092, -1.5791, -1.4379, -1.2992,
     6 -1.1735, -1.0510, -0.9646, -0.8779, -0.8002, -0.7574, -0.7356,
     7 -0.7478, -0.7512, -0.6906, -0.5594, -0.4417, -0.4019, -0.5027,
     8 -0.7628, -0.9625, -1.0431, -1.0068, -0.8781, -0.7559, -0.6628,
     9 -0.6128, -0.6118, -0.6575, -0.7620, -0.9217, -1.1264, -1.3660,
     $ -1.6352, -1.9264, -2.2266, -2.5123, -2.7472, -2.8820, -2.9129,
     1 -2.9145, -2.8854, -2.8508, -2.8512, -2.8202, -2.8023, -2.8004,
     2 -2.7800, -2.8175, -2.8413, -2.8943, -2.9876, -3.0688, -3.2424,
     3 -3.4064, -3.5759, -3.7630, -3.8925, -4.0774, -4.3243, -4.5964,
     4 -3.8654, -3.0974, -2.5967, -2.2482, -2.1016, -2.1488, -2.3261,
     5 -2.6448, -3.0446, -3.3958, -3.6510, -3.7049, -3.7240, -3.5992,
     6 -3.4937, -3.3676, -3.2230, -3.1630, -3.0691, -3.0776, -3.0872,
     7 -3.0974, -3.1223, -3.1285, -3.1212, -3.1333, -3.1674, -3.1668,
     8 -3.2433, -3.2398, -3.3135, -3.3975, -3.4427, -3.6434, -3.7528/
      DATA C12CH4/
     1 -3.9466, -4.1940, -4.3362, -4.5539, -4.7410, -4.9155, -5.1345,
     2 -5.3908, -5.5592, -5.8270, -6.0289, -6.2365, -6.6730, -7.0538,
     3 -7.6216, -8.5697, -9.8483,-10.0000, -9.3577, -8.5950, -7.8323,
     4 -7.0696, -6.3069, -5.5442, -5.1501, -4.8853, -4.6900, -4.5262,
     5 -4.3957, -4.2823, -4.2736, -4.2054, -4.1168, -3.9986, -3.8712,
     6 -3.8692, -3.8777, -3.8965, -3.9092, -3.8788, -3.7661, -3.6900,
     7 -3.6239, -3.5597, -3.5193, -3.4906, -3.4415, -3.3730, -3.3579,
     8 -3.3427, -3.3208, -3.3048, -3.3136, -3.2904, -3.2545, -3.2241,
     9 -3.1453, -3.0187, -2.9427, -2.8630, -2.8146, -2.8604, -2.8922,
     $ -2.9650, -2.9959, -2.8920, -2.7989, -2.7028, -2.6506, -2.7285,
     1 -2.8420, -2.9304, -2.9622, -2.8726, -2.7566, -2.6745, -2.6337,
     2 -2.6533, -2.6800, -2.7098, -2.7479, -2.6859, -2.6216, -2.5701,
     3 -2.4683, -2.4426, -2.4463, -2.4194, -2.4578, -2.4894, -2.4639,
     4 -2.4825, -2.4998, -2.4381, -2.4123, -2.3654, -2.2698, -2.2387,
     5 -2.2364, -2.2029, -2.1780, -2.1433, -2.0355, -1.9458, -1.8723,
     6 -1.7936, -1.7639, -1.7782, -1.8022, -1.8115, -1.7818, -1.6986,
     7 -1.6169, -1.5975, -1.6545, -1.7742, -1.8937, -1.9544, -1.8942,
     8 -1.7761, -1.6392, -1.5236, -1.4551, -1.4221, -1.4245, -1.4174/
      DATA C13CH4/
     1 -1.4177, -1.3776, -1.3349, -1.2909, -1.2470, -1.2162, -1.1850,
     2 -1.1677, -1.1449, -1.1229, -1.1031, -1.0795, -1.0687, -1.0692,
     3 -1.0904, -1.1166, -1.1511, -1.1951, -1.2321, -1.2831, -1.2716,
     4 -1.1902, -0.9715, -0.6654, -0.4103, -0.3011, -0.5049, -0.8659,
     5 -1.1777, -1.3847, -1.4359, -1.3908, -1.2992, -1.1923, -1.0951,
     6 -1.0213, -0.9578, -0.9299, -0.9207, -0.9292, -0.9725, -1.0126,
     7 -1.0750, -1.1149, -1.1636, -1.2059, -1.2638, -1.3327, -1.4079,
     8 -1.4983, -1.5711, -1.6872, -1.7870, -1.9266, -2.0774, -2.2119,
     9 -2.3875, -2.5155, -2.6822, -2.8372, -3.0032, -3.2413, -3.5058,
     $ -3.9508, -4.5133, -5.3536, -8.0815, -8.9081, -9.8155,-10.0000,
     1 -7.4757, -5.1602, -4.2454, -3.7640, -3.3256, -3.0103, -2.7726,
     2 -2.5510, -2.3849, -2.2318, -2.1080, -2.0086, -1.9290, -1.8902,
     3 -1.8750, -1.8700, -1.8476, -1.7390, -1.5724, -1.4284, -1.3425,
     4 -1.3791, -1.5132, -1.6508, -1.7283, -1.6684, -1.5432, -1.4447,
     5 -1.3773, -1.3490, -1.3642, -1.4016, -1.4713, -1.5836, -1.6984,
     6 -1.8085, -1.8486, -1.7464, -1.6338, -1.5555, -1.5552, -1.6935,
     7 -1.8165, -1.8417, -1.7697, -1.6346, -1.5589, -1.5466, -1.5604,
     8 -1.6307, -1.6867, -1.7593, -1.8051, -1.8167, -1.8518, -1.8559/
      DATA C14CH4/
     1 -1.8547, -1.8907, -1.8851, -1.8933, -1.9081, -1.9025, -1.9451,
     2 -1.9924, -2.0321, -2.0816, -2.1026, -2.1137, -2.1351, -2.1629,
     3 -2.1876, -2.2340, -2.2960, -2.3747, -2.4970, -2.6244, -2.7641,
     4 -2.8912, -3.0328, -3.1944, -3.3877, -3.4566, -3.1662, -2.7253,
     5 -2.3992, -2.2214, -2.2022, -2.3978, -2.7449, -3.2639, -3.9311,
     6 -4.1470, -3.9351, -3.7471, -3.6245, -3.4791, -3.4710, -3.4210,
     7 -3.4125, -3.4475, -3.4140, -3.4908, -3.5164, -3.5944, -3.7403,
     8 -3.8192, -4.0177, -4.1833, -4.3518, -4.6486, -4.8778, -5.2542,
     9 -5.7834, -6.3451, -7.7212,-10.0000, -9.9134, -7.9181, -6.0815,
     $ -5.4397, -4.9875, -4.6154, -4.4846, -4.3541, -4.3037, -4.3073,
     1 -4.2471, -4.2593, -4.1984, -4.1895, -4.1697, -4.1578, -4.1950,
     2 -4.1878, -4.2299, -4.2209, -4.2646, -4.3123, -4.3911, -4.4588,
     3 -4.1873, -3.8353, -3.5282, -3.3055, -3.3351, -3.5671, -3.8750,
     4 -4.2645, -4.4786, -4.4293, -4.3183, -4.1996, -4.0879, -4.0169,
     5 -3.9787, -3.9536, -3.9454, -3.9283, -3.9166, -3.9152, -3.9336,
     6 -3.9561, -3.9932, -4.0934, -4.2317, -4.5084, -4.9460, -5.4958,
     7 -6.5492, -8.5604, -9.6202/
C=N2O ====C' FOR    3 BAND MODEL
C=N2O ====    0-  120
      DATA C11N2O/
     1 -2.8003, -2.6628, -2.4313, -2.2579, -2.1700, -2.1702, -2.2490,
     2 -2.4003, -2.6264, -2.9219, -3.2954, -3.7684, -4.2621, -4.7558,
     3 -5.2495, -5.7432, -6.2369, -6.7306, -7.2243, -7.7180, -8.2117,
     4 -8.7054, -9.1991, -9.6928,-10.0000/
C=N2O ====  490-  775,  865-  995, 1065- 1385, 1545- 2040, 2090- 2655
      DATA C21N2O/
     1 -9.7185, -8.8926, -8.0667, -7.2307, -6.4149, -5.4872, -4.7083,
     2 -4.0319, -3.4752, -3.0155, -2.6046, -2.2057, -1.8137, -1.4741,
     3 -1.1914, -0.9603, -0.7923, -0.6629, -0.5849, -0.5402, -0.4975,
     4 -0.5148, -0.5592, -0.6521, -0.8148, -1.0186, -1.2764, -1.5873,
     5 -1.9638, -2.3881, -2.8083, -3.2392, -3.6934, -4.0682, -4.1366,
     6 -3.9423, -3.7143, -3.4975, -3.2602, -3.0976, -2.9815, -2.9153,
     7 -2.9596, -3.0281, -3.1264, -3.2650, -3.3906, -3.5717, -3.8312,
     8 -4.1706, -4.6077, -5.1839, -5.9224, -6.9862, -7.6901, -8.3940,
     9 -9.0979, -9.8018, -9.9154, -9.2271, -8.5388, -7.8504, -7.1621,
     $ -6.2428, -5.6051, -5.0971, -4.7237, -4.4104, -4.2050, -4.0681,
     1 -4.0278, -4.0307, -4.0492, -4.0333, -3.9710, -3.9249, -3.9360,
     2 -4.0316, -4.2317, -4.5414, -4.9787, -5.5623, -6.3335, -7.9968,
     3 -9.6601, -9.5486, -8.8517, -8.1548, -7.4579, -6.7610, -6.0641,
     4 -5.3672, -4.6703, -3.6918, -3.0656, -2.5796, -2.1876, -1.8646,
     5 -1.5919, -1.3587, -1.1684, -1.0286, -0.9470, -0.9271, -0.9442,
     6 -0.9695, -0.9753, -0.9573, -0.9550, -1.0000, -1.1070, -1.2791,
     7 -1.4976, -1.7281, -1.9277, -2.0227, -1.9577, -1.7625, -1.5020,
     8 -1.2186, -0.9270, -0.6326, -0.3429, -0.0768,  0.1500,  0.3215/
      DATA C22N2O/
     1  0.4104,  0.4385,  0.4288,  0.4185,  0.4570,  0.4972,  0.4987,
     2  0.4216,  0.2360, -0.0319, -0.3714, -0.7539, -1.1534, -1.5855,
     3 -2.0610, -2.6068, -3.2635, -4.1038, -5.2761, -6.1437, -7.0079,
     4 -7.9440, -8.8801, -9.8162,-10.0000, -9.5951, -9.1305, -8.6659,
     5 -8.2013, -7.7367, -7.2721, -6.8075, -6.1598, -5.8695, -5.3510,
     6 -4.9491, -4.6310, -4.3846, -4.0784, -3.7763, -3.5901, -3.4607,
     7 -3.4386, -3.5481, -3.7014, -3.9310, -4.2251, -4.4593, -4.8210,
     8 -5.3494, -6.1286, -7.5981,-10.0000,-10.0000,-10.0000,-10.0000,
     9 -6.3743, -5.5592, -5.0129, -4.6075, -4.3171, -4.0928, -3.7537,
     $ -3.5406, -3.3869, -3.2913, -3.3633, -3.4932, -3.6924, -4.0074,
     1 -4.2504, -4.5389, -4.9425, -5.4741, -6.2069, -7.5981,-10.0000,
     2-10.0000,-10.0000, -6.9215, -6.0798, -5.1934, -4.6288, -4.1316,
     3 -3.7322, -3.4089, -3.1573, -2.9573, -2.7298, -2.5615, -2.4382,
     4 -2.3523, -2.3774, -2.4508, -2.5755, -2.7757, -2.9904, -3.2733,
     5 -3.6524, -4.1599, -4.7952, -5.7004, -6.8762, -6.9822, -6.2484,
     6 -5.7613, -5.2586, -4.8674, -4.6633, -4.5332, -4.5158, -4.6593,
     7 -4.8427, -5.0917, -5.5781, -6.0645, -6.5509, -7.0373, -7.5237,
     8 -8.0101, -8.4965, -8.9829, -9.4693, -9.9557, -9.7130, -8.6609/
      DATA C23N2O/
     1 -7.6089, -6.5568, -5.0880, -4.4527, -3.9302, -3.4438, -2.9701,
     2 -2.5423, -2.1616, -1.8076, -1.4763, -1.1580, -0.8445, -0.5455,
     3 -0.2506,  0.0234,  0.2775,  0.5113,  0.7154,  0.8929,  1.0359,
     4  1.1306,  1.1697,  1.1807,  1.1803,  1.1974,  1.2466,  1.2629,
     5  1.2068,  1.0472,  0.7695,  0.4083, -0.0244, -0.5477, -1.2202,
     6 -2.1067, -2.9508, -3.2107, -3.1587, -2.9600, -2.7641, -2.6324,
     7 -2.5671, -2.5664, -2.6088, -2.6425, -2.6606, -2.6895, -2.7551,
     8 -2.8837, -3.0884, -3.3746, -3.7078, -4.0975, -4.6272, -5.2484,
     9-10.0000,-10.0000,-10.0000, -7.3571, -5.0287, -4.3047, -3.6431,
     $ -3.1026, -2.6122, -2.1941, -1.8454, -1.5726, -1.3829, -1.2818,
     1 -1.2505, -1.2579, -1.2731, -1.2502, -1.2092, -1.2044, -1.2577,
     2 -1.3942, -1.6262, -1.9347, -2.2830, -2.5386, -2.4801, -2.1671,
     3 -1.8061, -1.4726, -1.1797, -0.9377, -0.7542, -0.6392, -0.5899,
     4 -0.5743, -0.5669, -0.5339, -0.4745, -0.4471, -0.4779, -0.5877,
     5 -0.7964, -1.0942, -1.4812, -1.9593, -2.5140, -3.1350, -3.8102,
     6 -4.5825, -5.5982, -6.4193, -7.2403, -8.0614, -8.8825, -9.7035/
C=N2O ==== 2705- 2865, 3245- 3925, 4260- 4470, 4540- 4785, 4910- 5165
      DATA C31N2O/
     1 -9.8910, -8.9876, -8.0843, -7.1809, -6.1501, -5.3742, -4.7352,
     2 -4.2051, -3.7525, -3.3562, -2.9916, -2.6649, -2.3872, -2.1499,
     3 -1.9747, -1.7982, -1.6518, -1.5582, -1.4838, -1.5004, -1.5821,
     4 -1.6912, -1.8673, -2.0756, -2.3351, -2.7020, -3.1921, -3.8409,
     5 -4.7085, -5.9588, -6.5829, -8.5585, -9.8584, -9.9723, -9.4215,
     6 -8.8707, -8.3199, -7.7691, -7.2183, -6.5567, -6.4345, -5.6448,
     7 -5.0529, -4.4643, -3.9624, -3.5231, -3.1395, -2.8067, -2.5232,
     8 -2.2858, -2.0820, -1.9049, -1.7554, -1.6485, -1.5959, -1.5838,
     9 -1.5961, -1.5997, -1.5734, -1.5615, -1.5974, -1.7059, -1.9034,
     $ -2.1631, -2.4181, -2.5427, -2.4592, -2.2513, -2.0187, -1.7879,
     1 -1.5612, -1.3399, -1.1265, -0.9226, -0.7379, -0.5790, -0.4573,
     2 -0.3952, -0.3683, -0.3511, -0.3216, -0.2556, -0.2126, -0.2593,
     3 -0.4361, -0.7702, -1.2089, -1.7060, -2.2937, -3.1133, -4.4419,
     4 -6.0119, -6.9457,-10.0000,-10.0000,-10.0000,-10.0000, -7.0394,
     5 -5.9637, -5.2317, -4.6419, -4.1663, -3.7874, -3.5000, -3.3086,
     6 -3.2143, -3.1926, -3.2105, -3.2308, -3.1971, -3.1510, -3.1402,
     7 -3.1969, -3.3477, -3.6005, -3.9534, -4.4117, -4.9729, -5.6009,
     8 -6.2179, -5.9845, -5.5502, -4.9010, -4.3401, -3.8232, -3.3802/
      DATA C32N2O/
     1 -2.9972, -2.6747, -2.4143, -2.2209, -2.1080, -2.0682, -2.0687,
     2 -2.0775, -2.0485, -1.9847, -1.9531, -1.9870, -2.1110, -2.3366,
     3 -2.6293, -2.8922, -2.9474, -2.7627, -2.4999, -2.2554, -2.0537,
     4 -1.9062, -1.8268, -1.7941, -1.7766, -1.7468, -1.6767, -1.6130,
     5 -1.6085, -1.6849, -1.8599, -2.1258, -2.4538, -2.8205, -3.2028,
     6 -3.5988, -4.0691, -4.7117, -5.6320, -6.4806, -7.3731, -8.2602,
     7 -9.1474,-10.0000,-10.0000, -9.5340, -9.0282, -8.5224, -8.0166,
     8 -7.5109, -7.0051, -6.4117, -6.0148, -5.4878, -5.1742, -4.8859,
     9 -4.4873, -4.2249, -4.0285, -3.8669, -3.8247, -3.7652, -3.6521,
     $ -3.4906, -3.2613, -3.0307, -2.8156, -2.6172, -2.4264, -2.2442,
     1 -2.0775, -1.9432, -1.8703, -1.8523, -1.8552, -1.8443, -1.7814,
     2 -1.7104, -1.7043, -1.7952, -2.0205, -2.3968, -2.9374, -3.7689,
     3 -5.3159, -7.4139, -9.5119, -9.7965, -9.1511, -8.5057, -7.8603,
     4 -7.2149, -6.5695, -6.2415, -5.5829, -5.0296, -4.5660, -4.1722,
     5 -3.8364, -3.5551, -3.3398, -3.1970, -3.1363, -3.1232, -3.1257,
     6 -3.0999, -3.0288, -2.9746, -2.9875, -3.0925, -3.3137, -3.6496,
     7 -4.0276, -4.1958, -3.9760, -3.6179, -3.2725, -2.9653, -2.6962,
     8 -2.4677, -2.2828, -2.1547, -2.0949, -2.0763, -2.0606, -2.0142/
      DATA C33N2O/
     1 -1.9239, -1.8618, -1.8813, -2.0099, -2.2825, -2.7071, -3.3277,
     2 -4.3300, -6.2151, -8.3543,-10.0000, -9.7275, -9.1257, -8.5239,
     3 -7.9221, -7.3203, -6.7185, -6.6089, -5.8877, -5.4527, -5.0879,
     4 -4.6598, -4.3806, -4.1830, -4.0426, -4.0175, -4.0178, -3.9811,
     5 -3.9244, -3.8056, -3.6968, -3.6435, -3.6326, -3.6339, -3.6157,
     6 -3.5478, -3.4826, -3.4807, -3.5665, -3.7650, -4.0718, -4.3980,
     7 -4.5075, -4.3358, -4.0765, -3.8674, -3.7221, -3.6588, -3.6429,
     8 -3.6371, -3.6014, -3.5209, -3.4616, -3.4774, -3.5957, -3.8481,
     9 -4.2598, -4.8784, -5.8266, -6.7468, -8.1352, -9.2208,-10.0000/
C=O2  ====C' FOR    2 BAND MODEL
C=O2  ====    0-  265
      DATA C11O2/
     1 -6.1363, -6.1794, -6.2538, -6.3705, -6.5110, -6.6162, -6.7505,
     2 -6.7896, -6.8305, -6.8471, -6.8282, -6.8772, -6.8680, -6.9332,
     3 -6.9511, -7.0048, -7.0662, -7.1043, -7.2055, -7.2443, -7.3520,
     4 -7.4079, -7.4998, -7.5924, -7.6682, -7.7993, -7.8712, -8.0161,
     5 -8.1102, -8.2485, -8.3758, -8.4942, -8.6532, -8.7554, -8.9453,
     6 -9.0665, -9.2631, -9.4387, -9.6325, -9.8757,-10.0628,-10.3761,
     7-10.5478,-10.9147,-11.2052,-11.5129,-11.8206,-12.1283,-12.4360,
     8-12.7437,-13.0514,-13.3591,-13.6668,-13.9745/
C=O2  ==== 7650- 8080, 9235- 9490,12850-13220,14300-14600,15695-15955
      DATA C21O2/
     1-13.9458,-13.7692,-13.5048,-13.1422,-13.0242,-12.6684,-12.3571,
     2-12.2428,-11.8492,-11.6427,-11.5173,-11.2108,-11.1584,-11.0196,
     3-10.8040,-10.8059,-10.5828,-10.4580,-10.4170,-10.1823,-10.1435,
     4-10.0030, -9.8136, -9.7772, -9.5680, -9.4595, -9.3502, -9.1411,
     5 -9.0476, -8.8628, -8.7051, -8.5838, -8.4282, -8.3271, -8.1958,
     6 -8.0838, -7.9652, -7.8371, -7.7476, -7.6431, -7.5736, -7.5149,
     7 -7.4194, -7.2688, -7.0722, -6.8815, -6.7627, -6.8055, -6.9114,
     8 -6.9936, -7.0519, -7.0597, -7.0680, -7.1242, -7.2088, -7.3265,
     9 -7.4673, -7.6326, -7.8110, -8.0096, -8.2104, -8.4036, -8.5853,
     $ -8.7252, -8.8511, -8.9427, -9.0375, -9.1228, -9.2246, -9.3291,
     1 -9.4436, -9.5716, -9.6951, -9.8408, -9.9759,-10.1489,-10.3027,
     2-10.5178,-10.7265,-10.9787,-11.2939,-11.5552,-11.9595,-12.2436,
     3-12.6942,-13.2011,-13.8191,-13.9216,-13.7293,-13.5370,-13.3447,
     4-13.1523,-12.9600,-12.7677,-12.5754,-12.3830,-12.1907,-11.9948,
     5-11.7759,-11.5926,-11.4214,-11.2493,-11.1094,-10.9477,-10.8332,
     6-10.7323,-10.6380,-10.5725,-10.4409,-10.2013, -9.8839, -9.6546,
     7 -9.5053, -9.4638, -9.5526, -9.6558, -9.7430, -9.7958, -9.7896,
     8 -9.8320, -9.9447,-10.1221,-10.3707,-10.6623,-10.9761,-11.2271/
      DATA C22O2/
     1-11.4091,-11.4921,-11.6015,-11.6945,-11.8333,-11.9985,-12.1788,
     2-12.3822,-12.6605,-13.0796,-13.3528,-13.6463,-13.9398,-13.7034,
     3-13.3150,-13.1177,-12.6462,-12.4868,-12.2205,-11.9650,-11.6941,
     4-11.4377,-11.2136,-10.9567,-10.7980,-10.5546,-10.3952,-10.2403,
     5-10.0491, -9.9226, -9.7871, -9.6557, -9.6106, -9.5142, -9.4763,
     6 -9.4163, -9.2348, -9.1088, -8.7946, -8.5876, -8.3128, -8.0945,
     7 -7.9127, -7.7229, -7.5860, -7.4215, -7.2726, -7.1179, -6.9516,
     8 -6.8075, -6.6413, -6.5043, -6.3519, -6.2112, -6.0839, -5.9337,
     9 -5.8321, -5.6969, -5.5923, -5.5076, -5.4002, -5.3413, -5.2826,
     $ -5.2458, -5.2877, -5.3743, -5.4654, -5.5262, -5.4429, -5.2430,
     1 -5.0284, -4.8464, -4.7534, -4.7825, -4.9462, -5.2290, -5.6440,
     2 -6.1889, -6.8427, -7.7731, -9.1688, -9.6893,-10.1853,-10.7670,
     3-11.4611,-12.3081,-13.1476,-13.8192,-13.5871,-13.2189,-12.9705,
     4-12.4825,-12.1301,-11.9430,-11.6636,-11.3197,-11.1678,-10.8967,
     5-10.6002,-10.4857,-10.1986, -9.9731, -9.8547, -9.5817, -9.4382,
     6 -9.3042, -9.0755, -8.9944, -8.8060, -8.6543, -8.5441, -8.3556,
     7 -8.2557, -8.0959, -7.9717, -7.8453, -7.7076, -7.5910, -7.4567,
     8 -7.3439, -7.2248, -7.1236, -7.0209, -6.9345, -6.8404, -6.7560/
      DATA C23O2/
     1 -6.6744, -6.5870, -6.5278, -6.4809, -6.5042, -6.5797, -6.6564,
     2 -6.6939, -6.5912, -6.3776, -6.1438, -6.0062, -6.0469, -6.3081,
     3 -6.8199, -7.4307, -8.1345, -9.1190,-10.4203,-11.4698,-12.5942,
     4-13.5316,-13.8693,-13.9392,-13.6885,-13.4377,-13.1869,-12.9362,
     5-12.6854,-12.3720,-12.2852,-11.9331,-11.7575,-11.6297,-11.3290,
     6-11.1205,-11.0084,-10.7243,-10.5543,-10.4485,-10.1764,-10.0759,
     7 -9.9304, -9.7196, -9.6630, -9.4774, -9.3638, -9.2675, -9.1121,
     8 -9.0368, -8.9025, -8.8028, -8.7012, -8.5909, -8.5121, -8.4141,
     9 -8.3444, -8.2687, -8.2003, -8.1571, -8.1141, -8.1261, -8.1848,
     $ -8.2395, -8.2478, -8.0877, -7.7880, -7.5611, -7.4487, -7.4880,
     1 -7.7644, -8.2142, -8.8765,-10.1091,-12.4483,-13.7228/
      END
      BLOCK DATA CPTRCG
C>    BLOCK DATA
C
C     C' FOR TRACE GASES (NH3, NO, NO2, SO2)
      COMMON /TRACEG/  C11NH3( 78),
     +                 C21NH3(126),C22NH3(126),C23NH3(101),
     +                 C11NO ( 62),
     +                 C11NO2(126),C12NO2( 16),
     +                 C11SO2( 38),
     +                 C21SO2(126),C22SO2( 62)
C=NH3 ====C' FOR    2 BAND MODEL
C=NH3 ====    0-  385
      DATA C11NH3/
     1 -5.7142, -5.2854, -4.5163, -3.9795, -3.4393, -2.8735, -2.4947,
     2 -2.2290, -2.0624, -1.9616, -1.8707, -1.7712, -1.6473, -1.5376,
     3 -1.4315, -1.3328, -1.2391, -1.1768, -1.1302, -1.0755, -1.0272,
     4 -0.9884, -0.9501, -0.9287, -0.9101, -0.8982, -0.8888, -0.8709,
     5 -0.8620, -0.8645, -0.8676, -0.8910, -0.9084, -0.9328, -0.9546,
     6 -0.9743, -0.9983, -1.0202, -1.0569, -1.0824, -1.1086, -1.1475,
     7 -1.1790, -1.2059, -1.2668, -1.3237, -1.3801, -1.4271, -1.4920,
     8 -1.5403, -1.5848, -1.6498, -1.7382, -1.8294, -1.9203, -2.0694,
     9 -2.2134, -2.3622, -2.5516, -2.7633, -2.9344, -3.1172, -3.3543,
     $ -3.5671, -3.7504, -3.9884, -4.2633, -4.5505, -4.7837, -5.0350,
     1 -5.3733, -5.6478, -5.8856, -6.1041, -6.3375, -6.5709, -6.8043,
     2 -7.0377/
C=NH3 ====  390- 2150
      DATA C21NH3/
     1 -7.2620, -7.0950, -6.9279, -6.7608, -6.5938, -6.4267, -6.2597,
     2 -6.0926, -5.8842, -5.7560, -5.5844, -5.4248, -5.2573, -5.0771,
     3 -4.9244, -4.7903, -4.6512, -4.5169, -4.3961, -4.2607, -4.1705,
     4 -4.1294, -4.0611, -3.9538, -3.8821, -3.7592, -3.6754, -3.6830,
     5 -3.6977, -3.6925, -3.6632, -3.5899, -3.5218, -3.5265, -3.6535,
     6 -3.8068, -3.9818, -4.0574, -3.9789, -3.8858, -3.8120, -3.8927,
     7 -3.8799, -3.8623, -3.3984, -2.8857, -2.5814, -2.4066, -2.3850,
     8 -2.5415, -2.8161, -3.2265, -3.7177, -3.9932, -4.0683, -4.0785,
     9 -3.9912, -3.7418, -3.4742, -3.2651, -3.0715, -2.9500, -2.8669,
     $ -2.7723, -2.6614, -2.5613, -2.4372, -2.3085, -2.1696, -2.0302,
     1 -1.9166, -1.8071, -1.7221, -1.6370, -1.5453, -1.4487, -1.3539,
     2 -1.2570, -1.1618, -1.1131, -1.0824, -1.0559, -1.0190, -0.9721,
     3 -0.9218, -0.8680, -0.8556, -0.8568, -0.8713, -0.8984, -0.9076,
     4 -0.9024, -0.8882, -0.8968, -0.9492, -1.0089, -1.0846, -1.1556,
     5 -1.1792, -1.1946, -1.1964, -1.2173, -1.2424, -1.1744, -0.9743,
     6 -0.6350, -0.2975, -0.0705,  0.0144, -0.0978, -0.3536, -0.5630,
     7 -0.5479, -0.3784, -0.1797, -0.1151, -0.3085, -0.6180, -0.9718,
     8 -1.2926, -1.2748, -1.1217, -1.0197, -0.9300, -0.8817, -0.8723/
      DATA C22NH3/
     1 -0.8309, -0.7804, -0.7075, -0.6431, -0.6176, -0.6012, -0.6079,
     2 -0.6272, -0.6304, -0.6193, -0.6026, -0.5882, -0.6029, -0.6317,
     3 -0.6862, -0.7447, -0.7921, -0.8275, -0.8595, -0.8856, -0.9236,
     4 -0.9934, -1.0693, -1.1460, -1.2100, -1.2863, -1.3593, -1.4292,
     5 -1.5029, -1.6054, -1.7067, -1.8110, -1.9350, -2.0346, -2.1305,
     6 -2.2294, -2.3724, -2.4917, -2.6218, -2.8056, -2.9693, -3.1101,
     7 -3.2790, -3.5315, -3.7011, -3.8952, -4.1527, -4.4121, -4.5244,
     8 -4.8599, -5.1940, -5.5589, -5.8170, -6.1402, -6.4633, -6.7865,
     9 -7.1096, -7.4328, -7.7559, -8.0000, -7.8199, -7.5988, -7.3778,
     $ -7.1567, -6.9357, -6.7146, -6.4936, -6.2725, -6.0515, -5.8304,
     1 -5.5963, -5.3883, -5.2319, -5.0536, -4.9029, -4.7789, -4.5867,
     2 -4.3414, -4.1399, -3.9784, -3.7553, -3.5773, -3.4123, -3.2254,
     3 -3.0384, -2.9243, -2.7755, -2.5809, -2.4726, -2.3206, -2.1209,
     4 -2.0331, -1.9016, -1.7458, -1.6927, -1.5958, -1.4863, -1.4492,
     5 -1.3730, -1.2859, -1.2554, -1.2129, -1.1689, -1.1802, -1.1948,
     6 -1.1882, -1.2185, -1.2464, -1.2522, -1.2946, -1.3587, -1.3971,
     7 -1.4488, -1.5261, -1.5495, -1.5478, -1.4926, -1.3115, -1.0455,
     8 -0.7987, -0.5972, -0.4664, -0.4244, -0.4426, -0.4952, -0.5772/
      DATA C23NH3/
     1 -0.6845, -0.8097, -0.9443, -1.0904, -1.2232, -1.2853, -1.2949,
     2 -1.2708, -1.1896, -1.1467, -1.1187, -1.0700, -1.0392, -1.0227,
     3 -1.0178, -1.0089, -1.0021, -0.9706, -0.9569, -0.9928, -1.0310,
     4 -1.0767, -1.1053, -1.1241, -1.1717, -1.2203, -1.2772, -1.3356,
     5 -1.3855, -1.4734, -1.5701, -1.6572, -1.7638, -1.8652, -1.9918,
     6 -2.1449, -2.2388, -2.3251, -2.3936, -2.4525, -2.5998, -2.7147,
     7 -2.7704, -2.7852, -2.7524, -2.7646, -2.8507, -3.0422, -3.2642,
     8 -3.5201, -3.6328, -3.7624, -3.9505, -4.1399, -4.3087, -4.3859,
     9 -4.4295, -4.4493, -4.3317, -4.1892, -4.0545, -3.9356, -3.9117,
     $ -4.0001, -4.0627, -4.0833, -4.0997, -4.0659, -4.0264, -4.0893,
     1 -4.1832, -4.2522, -4.3182, -4.3949, -4.4191, -4.4580, -4.5997,
     2 -4.7282, -4.8370, -5.0041, -5.1644, -5.2101, -5.4145, -5.5114,
     3 -5.6986, -5.8057, -5.9529, -6.1000, -6.2472, -6.3943, -6.5415,
     4 -6.6886, -6.8358, -6.9829, -7.1301, -7.2772, -7.4244, -7.5715,
     5 -7.7187, -7.8658, -8.0000/
C=NO  ====C' FOR    1 BAND MODEL
C=NO  ==== 1700- 2005
      DATA C11NO/
     1 -7.9265, -7.5649, -7.2033, -6.8418, -6.4802, -6.0647, -5.7193,
     2 -5.3955, -5.1475, -4.8233, -4.5194, -4.3184, -3.9664, -3.7045,
     3 -3.3398, -3.0368, -2.7282, -2.4448, -2.1791, -1.9315, -1.7046,
     4 -1.4984, -1.3133, -1.1486, -1.0036, -0.8776, -0.7699, -0.6811,
     5 -0.6124, -0.5663, -0.5488, -0.5673, -0.6076, -0.6791, -0.7553,
     6 -0.7811, -0.7711, -0.6840, -0.5704, -0.4791, -0.4138, -0.3950,
     7 -0.4189, -0.4794, -0.5751, -0.7062, -0.8751, -1.0852, -1.3406,
     8 -1.6473, -2.0068, -2.4335, -2.9068, -3.4595, -4.0370, -4.6795,
     9 -5.2704, -5.8613, -6.4522, -7.0431, -7.6340, -8.0000/
C=NO2 ====C' FOR    1 BAND MODEL
C=NO2 ====  580-  925, 1515- 1695, 2800- 2970
      DATA C11NO2/
     1 -6.0000, -5.8419, -5.5313, -5.1048, -4.9512, -4.5830, -4.2676,
     2 -3.9783, -3.7150, -3.4782, -3.2541, -3.0597, -2.8625, -2.6989,
     3 -2.5323, -2.3904, -2.2561, -2.1346, -2.0320, -1.9284, -1.8584,
     4 -1.7778, -1.7222, -1.6776, -1.6024, -1.5658, -1.4917, -1.4117,
     5 -1.3706, -1.3045, -1.2914, -1.3292, -1.3666, -1.4268, -1.4564,
     6 -1.4076, -1.3284, -1.2804, -1.2497, -1.2519, -1.3123, -1.3704,
     7 -1.4192, -1.4878, -1.5301, -1.5575, -1.5912, -1.6250, -1.6544,
     8 -1.6849, -1.7340, -1.7748, -1.8171, -1.8679, -1.9256, -1.9809,
     9 -2.0386, -2.1112, -2.1769, -2.2462, -2.3199, -2.4129, -2.5156,
     $ -2.6575, -2.8825, -3.1831, -3.6209, -4.2271, -5.5290, -6.0000,
     1 -6.0000, -5.5415, -4.8964, -4.2513, -3.6063, -2.9612, -2.1733,
     2 -1.5514, -1.0260, -0.5817, -0.2030,  0.1231,  0.4098,  0.6653,
     3  0.8885,  1.0716,  1.2025,  1.2697,  1.2926,  1.3006,  1.3128,
     4  1.3449,  1.3656,  1.3245,  1.1868,  0.9310,  0.5907,  0.2056,
     5 -0.2337, -0.7633, -1.4541, -2.4451, -3.1822, -3.9193, -4.6565,
     6 -5.3936, -6.0000, -6.0000, -5.7606, -5.3422, -4.9238, -4.5055,
     7 -4.0871, -3.6687, -3.2504, -2.8320, -2.3736, -1.9565, -1.5769,
     8 -1.2400, -0.9384, -0.6781, -0.4630, -0.2944, -0.1783, -0.1213/
      DATA C12NO2/
     1 -0.1033, -0.0934, -0.0723, -0.0267,  0.0016, -0.0394, -0.1700,
     2 -0.4141, -0.7861, -1.2951, -2.0379, -3.0984, -3.8692, -4.6399,
     3 -5.4107, -6.0000/
C=SO2 ====C' FOR    2 BAND MODEL
C=SO2 ====    0-  185
      DATA C11SO2/
     1 -0.9312, -0.8101, -0.5729, -0.3590, -0.2016, -0.0971, -0.0333,
     2  0.0048,  0.0228,  0.0214, -0.0044, -0.0567, -0.1334, -0.2315,
     3 -0.3451, -0.4741, -0.6198, -0.7854, -0.9764, -1.1922, -1.4326,
     4 -1.6951, -1.9687, -2.2788, -2.6034, -2.9398, -3.3551, -3.7704,
     5 -4.1857, -4.6010, -5.0163, -5.4316, -5.8469, -6.2622, -6.6775,
     6 -7.0928, -7.5081, -7.9234/
C=SO2 ====  400-  650,  950- 1460, 2415- 2580
      DATA C21SO2/
     1 -8.0000, -7.4209, -6.6994, -5.9778, -5.2563, -4.4248, -3.7369,
     2 -3.0917, -2.5200, -2.0303, -1.6307, -1.3056, -1.0373, -0.8189,
     3 -0.6395, -0.4880, -0.3574, -0.2369, -0.1237, -0.0261,  0.0250,
     4  0.0186, -0.0194, -0.0659, -0.0638, -0.0065,  0.0468,  0.0682,
     5  0.0355, -0.0431, -0.1334, -0.2175, -0.2954, -0.3738, -0.4588,
     6 -0.5571, -0.6729, -0.8131, -0.9805, -1.1831, -1.4334, -1.7354,
     7 -2.1065, -2.5705, -3.1238, -3.7691, -4.5793, -5.7012, -6.5603,
     8 -7.4195, -8.0000, -7.9302, -7.6563, -7.3824, -7.1085, -6.8346,
     9 -6.5607, -6.2868, -6.0129, -5.7390, -5.4651, -5.1912, -4.9173,
     $ -4.6434, -4.3695, -4.0956, -3.8217, -3.5478, -3.2739, -3.0000,
     1 -2.7261, -2.4522, -2.1783, -1.9317, -1.7073, -1.5004, -1.3136,
     2 -1.1444, -0.9901, -0.8505, -0.7238, -0.6083, -0.5025, -0.4016,
     3 -0.3047, -0.2112, -0.1263, -0.0656, -0.0414, -0.0509, -0.0731,
     4 -0.0802, -0.0483,  0.0032,  0.0339,  0.0249, -0.0296, -0.1170,
     5 -0.2141, -0.3069, -0.3968, -0.4881, -0.5881, -0.7019, -0.8299,
     6 -0.9729, -1.1305, -1.3036, -1.4924, -1.7000, -1.9306, -2.1906,
     7 -2.4959, -2.8613, -3.3176, -3.9236, -4.6847, -5.2561, -4.7082,
     8 -4.1110, -3.6582, -3.1963, -2.7063, -1.9643, -1.3089, -0.6856/
      DATA C22SO2/
     1 -0.0412,  0.3678,  0.6712,  0.9031,  1.0577,  1.1145,  1.1272,
     2  1.1300,  1.1237,  1.1459,  1.1047,  0.9617,  0.7107,  0.3254,
     3 -0.2322, -1.0612, -1.7715, -2.6089, -3.0225, -3.3542, -3.7339,
     4 -4.1986, -4.7852, -5.6390, -6.2740, -6.9091, -7.5441, -8.0000,
     5 -8.0000, -7.5698, -6.8815, -6.1933, -5.3530, -4.8602, -4.1286,
     6 -2.9922, -2.3525, -1.8905, -1.5178, -1.2295, -1.0082, -0.8484,
     7 -0.7634, -0.7340, -0.7203, -0.7167, -0.7097, -0.7297, -0.8391,
     8 -1.0472, -1.3607, -1.7720, -2.2957, -3.0566, -4.1073, -4.5337,
     9 -4.9481, -5.4542, -6.2445, -6.8148, -7.3850, -7.9553/
      END
      SUBROUTINE FUDGE(V,SUMY)
C
C     TO CALCULATE H2O FAR WING CONTINUUM USING THE SUMS OF EXPONENTIALS
C
C     THIS FUNCTION IS WITHIN 5% OF THE ORIGINAL "FUDGE" BETWEEN 0 AND
C     3000CM-1, PRESERVING THAT VALIDATION.
C     THE NEW FUNCTION IS 0.01 OF THE ORIGINAL NEAR 10000CM-1 (1.06NM),
C     IN ACCORDANCE WITH THE MEASUREMENTS OF JAYCOR, FUNDED BY SDIO.
C
C ALOG -> LOG (94.3.26)
C     Y0(V)=EXP(LOG(3.159E-8)-(2.75E-4)*V)
      Y1(V)=EXP(LOG(1.025*3.159E-8)-(2.75E-4)*V)
      Y2(V)=EXP(LOG(8.97E-6)-(1.300E-3)*V)
C
C     YO=Y0(V)
      YA=Y1(V)
      YB=Y2(V)
      YAINV=1/YA
      YBINV=1/YB
      SUMY=1./(1.*YAINV+1.*YBINV)
      RETURN
      END
      FUNCTION BPLNK(W,T)
C PLANK FUNCTION WITH RESPECT TO WAVELENGTH
C--- HISTORY
C 89. 8. 1  CREATED
C--- INPUT
C W      R      WAVELENGTH (MICRON)
C T      R      ABSOLUTE TEMPERATURE (K)
C--- OUTPUT
C BPLNK  RF     PLANK FUNCTION (W/M2/STR/MICRON)
      IF(W*T.LE.0.0) THEN
        BPLNK=0
       ELSE
        X=1.438786E4/W/T
        BPLNK=1.1911E8/W**5/(EXPFN(X)-1)
      ENDIF
      RETURN
      END
C
C 2002.5.21 Delete RTRN22 for PT by SED
C
      SUBROUTINE CHKRT(INDA,INDT,INDP,IMTHD,NDA,NA1U,AM1U,NA0,AM0
     &,NFI,FI,NLN,THK,OMG,NLGN1,G,NANG,ANG,PHSF,EPSP,EPSU,GALB,FSOL
     &,NPLK1,CPLK,BGND,NTAU,UTAU,AMUA,WA,DPT,MXLGN2,ERR)
C CHECK-IN VARIABLES FOR -RTRN1- AND SET SOME VARIABLES.
C--- HISTORY
C 90. 1.20   CREATED
C    11.22   ADD: ELSE MXLGN2=2*NDA+1
C    12. 1   ADD LOOP-19.
C 93. 5. 4   Delete the condition GALB<=1 because GALB is used as wind
C            velocity when INDG>0.
C INPUT/OUTPUT SEE MAIN ROUTINE.
      PARAMETER (KNA1U =100)
      PARAMETER (KNA0  =2)
      PARAMETER (KNDM  =16)
      PARAMETER (KNFI  =200)
      PARAMETER (KLGN1 =400)
      PARAMETER (KNLN  =35)
      PARAMETER (KNTAU =2)
      PARAMETER (KPLK1 =2)
      PARAMETER (KNANG =250)
C
      PARAMETER (KNLN1=KNLN+1,KLGT1=2*KNDM)
      PARAMETER (PI=3.141592654)
C AREAS FOR THIS ROUTINE
      CHARACTER ERR*64
      DIMENSION AM1U(KNA1U),AM0(KNA0),FI(KNFI),THK(KNLN),OMG(KNLN)
     &,NLGN1(KNLN),G(KLGN1,KNLN),ANG(KNANG),PHSF(KNANG,KNLN)
     &,CPLK(KPLK1,KNLN),UTAU(KNTAU)
      DIMENSION AMUA(KNDM),WA(KNDM),DPT(KNLN1)
      DIMENSION CCP(3)
      CALL CPCON(CCP)
      EPS=CCP(1)*10
      ERR=' '
C EPSP, EPSU, FSOL
      IF(EPSP.LT.0) THEN
        ERR='ILLEGAL VALUE OF EPSP'
        RETURN
      ENDIF
      IF(EPSU.LT.0) THEN
        ERR='ILLEGAL VALUE OF EPSU'
        RETURN
      ENDIF
      IF(FSOL.LT.0) THEN
        ERR='ILLEGAL VALUE OF FSOL'
        RETURN
      ENDIF
C INDA
      IF(INDA.LT.0 .OR. INDA.GT.2) THEN
        ERR='ILLEGAL VALUE OF INDA'
        RETURN
      ENDIF
C INDT
      IF(INDT.LT.0 .OR. INDT.GT.2) THEN
        ERR='ILLEGAL VALUE OF INDT'
        RETURN
      ENDIF
C INDP
      IF(INDP.LT.-1 .OR. INDP.GT.1) THEN
        ERR='ILLEGAL VALUE OF INDP'
        RETURN
       ENDIF
C IMTHD
      IF(IMTHD.GT.3) THEN
        ERR='ILLEGAL VALUE OF IMTHD'
        RETURN
      ENDIF
C NDA
      IF(NDA.LE.0 .OR. NDA.GT.KNDM) THEN
        ERR='ILLEGAL VALUE OF NDA'
        RETURN
      ENDIF
C SET QUADRATURE
      CALL QUADA(NDA,AMUA,WA)
C NA0, AM0
      IF(NA0.LE.0 .OR. NA0.GT.KNA0) THEN
        ERR='ILLEGAL VALUE OF NA0'
        RETURN
      ENDIF
      DO 20 I=1,NA0
      IF(AM0(I).LE.0.0 .OR. AM0(I).GT.1.0) THEN
        ERR='ILLEGAL VALUE OF AM0'
        RETURN
      ENDIF
   20 CONTINUE
C INDA, AM1U, FI
      IF(INDA.GT.0) THEN
        IF(NFI.LE.0 .OR. NFI.GT.KNFI) THEN
          ERR='ILLEGAL VALUE OF NFI'
          RETURN
        ENDIF
        IF(INDA.EQ.2) NA1U=2*NDA
        IF(NA1U.LE.0 .OR. NA1U.GT.KNA1U) THEN
          ERR='ILLEGAL VALUE OF NA1U'
          RETURN
        ENDIF
        IF(INDA.EQ.2) THEN
          DO 1 I=1,NDA
          AM1U(I)        = -AMUA(I)
    1     AM1U(NA1U+1-I) =  AMUA(I)
        ENDIF
      ENDIF
C NLN
      IF(NLN.LE.0 .OR. NLN.GT.KNLN) THEN
        ERR='ILLEGAL VALUE OF NLN'
        RETURN
      ENDIF
      DPT(1)=0
      DO 17 L=1,NLN
   17 DPT(L+1)=DPT(L)+THK(L)
C NTAU
      IF(INDT.EQ.2) NTAU=NLN+1
      IF(NTAU.LE.0 .OR. NTAU.GT.KNTAU) THEN
        ERR='ILLEGAL VALUE OF NTAU'
        RETURN
      ENDIF
C UTAU
      IF(INDT.EQ.2) THEN
        DO 3 IT=1,NTAU
    3   UTAU(IT)=DPT(IT)
      ENDIF
C NLGN1,  MXLGN2=MAX(NLGN1)
      IF(INDP.LE.0 .OR. (INDA.GT.0 .AND. IMTHD.EQ.3)) THEN
        MXLGN2=1
        DO 4 L=1,NLN
        N1=NLGN1(L)
        IF(N1.LE.0 .OR. N1.GT.KLGN1) THEN
          ERR='ILLEGAL VALUE OF NLGN1'
          RETURN
        ENDIF
        MXLGN2=MAX(MXLGN2,N1)
    4   CONTINUE
       ELSE
        MXLGN2=2*NDA+1
      ENDIF
C NANG
      IF(INDP.GT.0 .OR. (IMTHD.GE.1 .AND. INDA.GE.1)) THEN
        IF(NANG.LE.3 .OR. NANG.GT.KNANG) THEN
          ERR='YOU SHOULD SET AT LEAST FOUR ANGLES FOR THIS CONDITION'
          RETURN
        ENDIF
        DO 12 I=2,NANG
          IF(ANG(I).LE.ANG(I-1)) THEN
            ERR='YOUR SHOULD SET SCATTERING ANGLE FROM 0 TO 180 DEGREES'
            RETURN
          ENDIF
   12   CONTINUE
      ENDIF
C CHECK ORDER OF DPT AND UTAU.
      DO 5 L=1,NLN
      IF(DPT(L+1).LT.DPT(L)) THEN
        ERR='DPT SHOULD BE SET FROM TOP TO BOTTOM'
        RETURN
      ENDIF
    5 CONTINUE
      IF(NTAU.GE.2) THEN
        DO 6 IT=2,NTAU
        IF(UTAU(IT).LT.UTAU(IT-1)) THEN
          ERR='UTAU SHOULD BE SET FROM TOP TO BOTTOM'
          RETURN
        ENDIF
    6   CONTINUE
      ENDIF
      DO 19 IT=1,NTAU
      IF(UTAU(IT).GT.DPT(NLN+1)) THEN
        IF(ABS(UTAU(IT)-DPT(NLN+1)).LE.EPS) THEN
          UTAU(IT)=DPT(NLN+1)
         ELSE
          ERR='UTAU IS OUT OF BOUNDS'
          RETURN
        ENDIF
      ENDIF
   19 CONTINUE
C RESET CPLK AND BGND, GALB
      IF(GALB.LT.0.0) THEN
        ERR='ILLEGAL VALUE OF GALB'
        RETURN
      ENDIF
      IF(NPLK1.GT.KPLK1) THEN
        ERR='TOO LARGE -NPLK1-, CHANGE -KPLK1-'
        RETURN
       ELSE
        IF(NPLK1.LE.0) THEN
          BGND=0
          DO 18 L=1,NLN
          DO 18 K1=1,NPLK1
   18     CPLK(K1,L)=0
        ENDIF
      ENDIF
      RETURN
      END
      SUBROUTINE CONVU(N,AM,JJ,ER,IC,EPSU,NCHK)
C CHECK CONVERGENCE OF INTENSITY IN THE DIRECTION OF MU
C--- HISTORY
C 90. 1.20 CREATED
C--- INPUT
C N      I        NUMBER OF STREAMS TO BE CHECKED
C AM    R(N)      DIRECTION OF STREAM
C JJ    I(N)      STREAM NUMBER IN THE ORIGINAL ORDER
C ER    R(N)      MAXIMUM ERROR FOR THE STREAM REGARDLESS OF OTHER
C                  ANGLES AND LAYERS
C IC    I(N)      NUMBER OF CONSECTIVE SERIES WITH -ER- LESS THAN -EPSU-
C EPSU    R       CONVERGENCE CRITERION
C NCHK    I       MAX. NUMBER FOR THE VALUE OF -IC- BY WHICH THE
C                  ROUTINE CONFIRMS CONVERGENCE
C--- OUTPUT
C N, AM, JJ, IC   UPDATED AFTER DROPPING CONVERGENT STREAMS
C$ENDI
      DIMENSION AM(*),JJ(*),ER(*),IC(*)
      J1=0
      DO 1 J=1,N
      IF(ABS(ABS(AM(J))-1).LE.0) THEN
        IC(J)=999
        ER(J)=0
      ENDIF
      IF(ER(J).LT.EPSU) THEN
        IC(J)=IC(J)+1
        IF(IC(J).GE.NCHK) GOTO 1
       ELSE
        IC(J)=0
      ENDIF
      J1=J1+1
      IC(J1)=IC(J)
      JJ(J1)=JJ(J)
      AM(J1)=AM(J)
      ER(J1)=ER(J)
    1 CONTINUE
      N=J1
      RETURN
      END
C
C 2002.05.21 Delete INTCR1 for PT BY SED
C
      FUNCTION PINT4(INIT,ANG1,KNA,NA,ANG,P,L)
C INTERPOLATION OF THE PHASE FUNCTION.
C--- HISTORY
C 89.11. 8 CREATED FROM PINT3. CHANGE INTERPOLATION X-> ANG
C 90. 1.23 DEBUG
C--- INPUT
C INIT       I      1 THEN SEARCH ANG1-INTERVAL ELSE NOT SEARCH.
C ANG1       R      SCATTERING ANGLE IN DEGREE FOR INTERPOLATION.
C NA         I      NO. OF SCATTERING ANGLES.
C ANG      R(NA)    SCATTERING ANGLES IN DEGREE.
C P     R(KNA,L)    PHASE FUNCTION
C L          I      LAYER NUMBER
C--- OUTPUT VARIABLES
C INIT       I      0
C PINT4      R      INTERPOLATED VALUE OF P AT ANG1.
C$ENDI
C--- VARIABLES FOR THE ROUTINE.
      SAVE I1,I2,I3
      DIMENSION ANG(NA),P(KNA,L)
C--- WORKING AREAS.
      PARAMETER (PI=3.141592654, RAD=PI/180.0)
C
      IF(INIT.GE.1) THEN
        INIT=0
        DO 1 I=1,NA-1
        IF((ANG1-ANG(I))*(ANG1-ANG(I+1)).LE.0.0) GOTO 2
    1   CONTINUE
        I=NA
    2   IF(I-1) 3,3,4
    3   I1=1
        I3=3
        GO TO 5
C *
    4   IF(I-NA+1) 6,7,7
    6   I1=I-1
        I3=I+1
        GO TO 5
C *
    7   I1=NA-2
        I3=NA
    5   I2=I1+1
      ENDIF
      XX=ANG1
      X1=ANG(I1)
      X2=ANG(I2)
      X3=ANG(I3)
      ALP1=P(I1,L)
      ALP2=P(I2,L)
      ALP3=P(I3,L)
      ISIGN=-1
      IF(ALP1.GT.0.0 .AND. ALP2.GT.0.0 .AND. ALP3.GT.0.0) THEN
        ISIGN=1
        ALP1=LOG(ALP1)
        ALP2=LOG(ALP2)
        ALP3=LOG(ALP3)
      ENDIF
      PP=(XX-X2)*(XX-X3)/(X1-X2)/(X1-X3)*ALP1
     &  +(XX-X1)*(XX-X3)/(X2-X1)/(X2-X3)*ALP2
     &  +(XX-X1)*(XX-X2)/(X3-X1)/(X3-X2)*ALP3
      IF(ISIGN.GE.1) PP=EXPFN(PP)
      PINT4=PP
      RETURN
      END
      FUNCTION HF(TAU,AM1,AM2,AM3)
C GEOMETRICAL FACTOR FOR THE SCONDERY SCATTERING EQ.(24) OF NT.
C    HF=INTEG(0,TAU)DT*INTEG(0,T)DT1*EXP(T*(1/MU1-1/MU2)
C             + T1*(1/MU2-1/MU3))*EXP(-TAU/AM1)/AM1/AM2
C--- REFERENCE
C NT:  T. NAKAJIMA AND M. TANAKA, 1988, JQSRT, 40, 51-69
C--- HISTORY
C 88. 9.22  CREATED BY T. NAKAJIMA
C 89. 5. 4  USE EXPFN
C--- INPUT
C TAU      R         OPTICAL THICKNESS OF THE LAYER.
C AM1      R         COS(ZENITH ANGLE-1).
C AM2      R         COS(ZENITH ANGLE-2).
C AM3      R         COS(ZENITH ANGLE-3).
C--- OUTPUT
C HF       F         GEOMETRICAL FACTOR.
C
      SAVE INIT,EPS
      DIMENSION CCP(3)
      DATA INIT/1/
C
C SET EPS: IF ABS(1/AM1 - 1/AM0)*TAU .LE. EPS THEN
C                      THE ROUTINE SETS ALMUCANTAR CONDITION-IALM.
      IF(INIT.GT.0) THEN
        INIT=0
        CALL CPCON(CCP)
        EPS=CCP(1)*30
      ENDIF
C
      X1=1/AM1-1/AM2
      X2=1/AM2-1/AM3
      X3=1/AM1-1/AM3
      EX1=-TAU/AM1
      EX2=-TAU/AM2
      EX3=-TAU/AM3
      EX1=EXPFN(EX1)
      EX2=EXPFN(EX2)
      EX3=EXPFN(EX3)
C
      IF(ABS(X2*TAU).LE.EPS) GOTO 1
C X2 <> 0
CC I1
      IF(ABS(X3*TAU).LE.EPS) THEN
        AI1=EX1*(TAU+X3*TAU*TAU/2)
       ELSE
        AI1=(EX3-EX1)/X3
      ENDIF
CC I2
      IF(ABS(X1*TAU).LE.EPS) THEN
        AI2=EX1*(TAU+X1*TAU*TAU/2)
       ELSE
        AI2=(EX2-EX1)/X1
      ENDIF
      HF=(AI1-AI2)/AM1/AM2/X2
      RETURN
C X2 =  0
    1 IF(ABS(X1*TAU).LE.EPS) THEN
        HF=TAU**2*(0.5-X1*TAU/3)*EX1/AM1/AM2
       ELSE
        HF=((TAU-1/X1)*EX2+EX1/X1)/AM1/AM2/X1
      ENDIF
      RETURN
      END
C
C 2002.5.21 Delete FTRN21 for PT BY SED
C
      SUBROUTINE HOMOG2(M,T1,T2,OMG,N1,AM1,WMM,N2,AM2
     &    ,PR,PT,PR0,PT0,FSOL,NPLK1,CPLK,R,T,ER,ET,ZEIG
     &    ,Q,QI,C11,C22,VP,VM,DP,DM,ERR)
C SOLVE THE TRANSFER IN A HOMOGENEOUS SCATTERING AND EMITTING
C  MEDIUM BY THE DISCRETE ORDINATE METHOD.
C--- HISTORY
C 89.10.31 CREATED FROM HOMOG1 INCLUDING THERMAL RADIATION.
C 95.11.22 EPS=CCP(1)*10 -> EPS=EXP(LOG(CCP(1))*0.8)
C--- INPUT
C T1         R       OPTICAL DEPTH AT THE LAYER TOP.
C T2         R       OPTICAL DEPTH AT THE LAYER BOTTOM.
C OMG        R       SINGLE SCATTERING ALBEDO.
C N1         I       NO. OF THE QUADRATURE STREAMS.
C AM1     R(KNDM)    MU (I), I=1,N1.
C WMM     R(KNDM)    SQRT(W1/M1)
C N2         I       NO. OF THE SOLAR DIRECTIONS.
C AM2     R(KNA0)    MU0(I), I=1,N2.
C PR      R(KNDM,    SCALED P+-(I,J) I,J=1,N1
C           KNDM)
C PT                 SCALED P++(I,J)
C PR0     R(KNDM,    SCALED P0+-(I,J)  I=1,N1; J=1,N2.
C PT0       KNDM)    SCALED P0++(I,J)  I=1,N1; J=1,N2.
C FSOL       R       SOLAR IRRADIANCE AT THE TOP OF THE SYSTEM.
C NPLK1      I       MAX ORDER OF PLANK FUNCTION EXPANSION BY TAU + 1.
C                      IF NPLK1=0 THEN NO THERMAL.
C CPLK    R(NPLK1)   2*PI*(1-W)*B(N)
C--- OUTPUT
C R       R(KNDM,    REFLECTION   MATRIX   RIJ, I,J=1,N1.
C T         KNDM)    TRANSMISSION MATRIX   TIJ.
C ER      R(KNDM,    UPGOING   SOURCE MATRIX EU(I,J), I=1,N1; J=1,N2.
C ET        KNA0)    DOWNGOING SOURCE MATRIX ED(I,J).
C ZEIG    R(KNDM)    ROOT OF THE EIGENVALUES OF Z.
C Q                  Q-MATRIX
C QI                 INVERSE OF Q
C C11                C1 = INVERSE OF 2A-
C C22                C2 = INVERSE OF 2B-
C VP                 VS+
C VM                 VS-
C DP   R(KNDM,KPLK1) THERMAL EMISSION INTENSITY EXAPNSION (DNWARD)
C DM   R(KNDM,KPLK1) THERMAL EMISSION INTENSITY EXAPNSION (UPWARD)
C ERR      C*64      ERROR INDEX
C--- PARAMETER
C KNDM       I       DECLARED SIZE FOR NDA
C KNA0       I       DECLARED SIZE FOR NA0
C KPLK1      I       DECLARED SIZE FOR NPLK1
      PARAMETER (KNDM  =16)
      PARAMETER (KNA0  =2)
      PARAMETER (KPLK1 =2)
      PARAMETER (PI=3.141592653)
C--- AREAS FOR THE ROUTINE.
      CHARACTER ERR*(*)
      DIMENSION AM1(KNDM),WMM(KNDM),AM2(KNA0)
     &,PR(KNDM,KNDM),PT(KNDM,KNDM),PR0(KNDM,KNA0),PT0(KNDM,KNA0)
     &,CPLK(KPLK1),ZEIG(KNDM),R(KNDM,KNDM),T(KNDM,KNDM)
     &,ER(KNDM,KNA0),ET(KNDM,KNA0),Q(KNDM,KNDM),QI(KNDM,KNDM)
     &,C11(KNDM,KNDM),C22(KNDM,KNDM),VP(KNDM,KNA0),VM(KNDM,KNA0)
     &,DP(KNDM,KPLK1),DM(KNDM,KPLK1)
C--- WORKING AREAS
      PARAMETER (KROWIJ=KNDM*(KNDM+1)/2,KNDM2=2*KNDM)
      DIMENSION CCP(3),X(KNDM,KNDM),Y(KNDM,KNDM),XI(KNDM,KNDM)
     &,SP(KNDM,KNA0),SM(KNDM,KNA0),G1(KNDM,KNA0),GAM(KNDM,KNA0)
     &,E0(KNA0),C(KNDM),SL(KNDM),SL1(KNDM)
     &,AP(KNDM,KNDM),AM(KNDM,KNDM),BP(KNDM,KNDM),BM(KNDM,KNDM)
     &,IW(KNDM2),DP0(KNDM),DP1(KNDM),DM0(KNDM),DM1(KNDM)
C PRECISION
      CALL CPCON(CCP)
C      EPS=CCP(1)*10
	IF(CCP(1).LT.1.0E-30)CCP(1)=1.0E-30
      EPS=EXP(LOG(CCP(1))*0.8)
C
      ERR=' '
      TAU=T2-T1
C X, Y MATRICES
      DO 1 I=1,N1
        DO 2 J=1,N1
        X(I,J)=-OMG*(PT(I,J)-PR(I,J))
    2   Y(I,J)=-OMG*(PT(I,J)+PR(I,J))
        X(I,I)=1.0/AM1(I)+X(I,I)
    1   Y(I,I)=1.0/AM1(I)+Y(I,I)
C DECOMPOSITION OF XY
      IF(M.EQ.0 .AND. 1.0-OMG.LE.EPS) THEN
        IW0=1
       ELSE
        IW0=0
      ENDIF
      IF(M.EQ.0 .AND. IW0.EQ.0 .AND. NPLK1.GT.0) THEN
        IPK=1
       ELSE
        IPK=0
      ENDIF
      CALL GETQM(IW0,N1,X,Y,ZEIG,Q,QI,XI,IMN,ERR)
      IF(ERR.NE.' ') RETURN
C THERMAL SOURCE
CC C
      IF(IPK.EQ.0) THEN
        IF(NPLK1.GT.0) THEN
          DO 25 J=1,NPLK1
          DO 25 I=1,N1
          DP(I,J)=0
   25     DM(I,J)=0
        ENDIF
       ELSE
        DO 3 I=1,N1
          SUM1=0
          DO 4 J=1,N1
    4     SUM1=SUM1+Q(J,I)*WMM(J)
          DO 5 J=NPLK1,1,-1
          IF(J+2.GT.NPLK1) THEN
            CPLK1=0
            ELSE
            CPLK1=DP(I,J+2)
          ENDIF
    5     DP(I,J)=((J+1)*J*CPLK1+SUM1*CPLK(J))/ZEIG(I)**2
    3   CONTINUE
CC D
        CALL AXB(DM,Q,DP,N1,N1,NPLK1,KNDM,KNDM,KNDM)
        DO 6 J=1,NPLK1
          DO 7 I=1,N1
          SUM=0
          IF(J+1.LE.NPLK1) THEN
            DO 8 K=1,N1
    8       SUM=SUM+QI(K,I)*DP(K,J+1)
          ENDIF
          DP(I,J)=DM(I,J)-J*SUM
    7     DM(I,J)=DM(I,J)+J*SUM
    6   CONTINUE
      ENDIF
C SIGMA+ - (FOR SINGLE SCATTERING)
      DO 9 I=1,N1
      DO 9 J=1,N2
      SP(I,J)=OMG*(PT0(I,J)+PR0(I,J))
    9 SM(I,J)=OMG*(PT0(I,J)-PR0(I,J))
C LOWER G
      DO 10 I=1,N1
      DO 10 J=1,N2
      SUM=0
      DO 11 K=1,N1
   11 SUM=SUM+X(I,K)*SP(K,J)
   10 G1(I,J)=-SUM-SM(I,J)/AM2(J)
C GAMMA
      CALL AXB(GAM,QI,G1,N1,N1,N2,KNDM,KNDM,KNDM)
      DO 12 I=1,N1
      DO 12 J=1,N2
   12 GAM(I,J)=GAM(I,J)/(1.0/AM2(J)**2-ZEIG(I)**2)
C VS+ AND -
      DO 14 J=1,N2
      TRNS0=-T1/AM2(J)
      TRNS0=EXPFN(TRNS0)*FSOL
      DO 14 I=1,N1
      SUM1=0.0
      SUM2=0.0
      DO 13 K=1,N1
      SUM1=SUM1+Q (I,K)*GAM(K,J)
   13 SUM2=SUM2+QI(K,I)*GAM(K,J)/AM2(J)+XI(I,K)*SM(K,J)
      VP(I,J)=(SUM1+SUM2)/2.0*TRNS0
   14 VM(I,J)=(SUM1-SUM2)/2.0*TRNS0
C E0
      DO 15 I=1,N2
      EX1=-TAU/AM2(I)
   15 E0(I)=EXPFN(EX1)
C BASE FUNCTION  C(TAU) AND S(TAU).
      DO 16 I=1,N1
   16 CALL CSFN(TAU,TAU,ZEIG(I),C(I),SL(I),SL1(I))
C A+-, B+-
      DO 17 I=1,N1
      DO 17 J=1,N1
      SUM1=Q (I,J)*C  (J)
      SUM2=QI(J,I)*SL (J)
      SUM3=Q (I,J)*SL1(J)
      SUM4=QI(J,I)*C  (J)
      AP(I,J)=SUM1-SUM2
      AM(I,J)=SUM1+SUM2
      BP(I,J)=SUM3-SUM4
   17 BM(I,J)=SUM3+SUM4
C C11 AND C22 -> THEIR INVERSION.
      DO 18 I=1,N1
      DO 18 J=1,N1
      C11(I,J)=2.0*AM(I,J)
   18 C22(I,J)=2.0*BM(I,J)
      CALL TNVSS2(N1,C11,DT,0.0,KNDM,IW,ERR)
      IF(ERR.NE.' ') THEN
        ERR='ERROR TO GET -C11- (HOMOG2)'
        RETURN
      ENDIF
      CALL TNVSS2(N1,C22,DT,0.0,KNDM,IW,ERR)
      IF(ERR.NE.' ') THEN
        ERR='ERROR TO GET -C22- (HOMOG2)'
        RETURN
      ENDIF
C R, T MATRICES
      DO 19 I=1,N1
      DO 19 J=1,N1
      SUM1=0
      SUM2=0
      DO 20 K=1,N1
      SUM1=SUM1+AP(I,K)*C11(K,J)
   20 SUM2=SUM2+BP(I,K)*C22(K,J)
      R(I,J)=SUM1+SUM2
   19 T(I,J)=SUM1-SUM2
C ER, ET MATRICES
      DO 22 I=1,N1
      DP1(I)=0
      DM1(I)=0
      IF(IPK.EQ.1) THEN
        DO 21 J=1,NPLK1
        DP1(I)=DP1(I)+DP(I,J)*TAU**(J-1)
   21   DM1(I)=DM1(I)+DM(I,J)*TAU**(J-1)
        DP0(I)=DP(I,1)
        DM0(I)=DM(I,1)
       ELSE
        DP0(I)=0
        DM0(I)=0
      ENDIF
   22 CONTINUE
      DO 23 J=1,N2
      DO 23 I=1,N1
      SUM1=0
      SUM2=0
      DO 24 K=1,N1
      VP0=VP(K,J)      +DP0(K)
      VM1=VM(K,J)*E0(J)+DM1(K)
      SUM1=SUM1+R(I,K)*VP0+T(I,K)*VM1
   24 SUM2=SUM2+T(I,K)*VP0+R(I,K)*VM1
      ER(I,J)=VM(I,J)      +DM0(I)-SUM1
   23 ET(I,J)=VP(I,J)*E0(J)+DP1(I)-SUM2
      RETURN
      END
      SUBROUTINE GETQM(IW0,N,X,Y,ZEIG,Q,QI,XI,IMN,ERR)
C SOLVE    XY = Q ZEIG**2 INVERSE(Q)
C ROOT DECOMPOSITION METHOD
C--- HISTORY
C 89. 8. 4 CREATED
C--- INPUT
C IW0       I        IF 1 THEN RENOMALIZATION (FOR M=0 AND W0=1)
C N         I        ORDER OF MATRICES
C X     R(KNDM,KNDM) SYMMETRIC MATRIX
C Y     R(KNDM,KNDM) SYMMETRIC MATRIX
C--- OUTPUT
C ZEIG    R(KNDM)    SQRT(EIGENVALUE)
C Q     R(KNDM,KNDM) ROTATION MATIX
C QI    R(KNDM,KNDM) INVERSE OF Q
C XI    R(KNDM,KNDM) INVERSE OF X
C IMN       I        LOCATION OF MINIMUM EIGENVALUE
C ERR     C*64       ERROR INDICATER
C--- PRPC-PARAMETER
C KNDM      I        DECLARED SIZE OF MATRICES
C
      PARAMETER (KNDM  =16)
      CHARACTER ERR*(*)
      DIMENSION X(KNDM,KNDM),Y(KNDM,KNDM),ZEIG(KNDM),Q(KNDM,KNDM)
     & ,QI(KNDM,KNDM),XI(KNDM,KNDM)
C WORKING AREA
      PARAMETER (KROWIJ=(KNDM*(KNDM+1))/2)
      DIMENSION V(KNDM,KNDM),SQX(KNDM,KNDM),SQXI(KNDM,KNDM)
     & ,ROWIJ(KROWIJ),WK(KNDM)
C ROOT DECOMPOSITION OF X (USE V AND ZEING FOR U AND XEIG).
      ERR=' '
      K=0
      DO 1 I=1,N
      DO 1 J=1,I
      K=K+1
    1 ROWIJ(K)=X(I,J)
      CALL SYMTRX(ROWIJ,N,ZEIG,V,KNDM,WK,IERR)
      IF(IERR.GT.128) THEN
        ERR='ERROR IN DECOMPOSITION OF X IN GETQM'
        RETURN
      ENDIF
      DO 2 I=1,N
      IF(ZEIG(I).LE.0.0) THEN
        ERR='NON-POSITIVE EIGENVALUE OF X'
        RETURN
      ENDIF
    2 ZEIG(I)=SQRT(ZEIG(I))
      DO 5 I=1,N
      DO 5 J=1,N
      SUM1=0
      SUM2=0
      SUM3=0
      DO 6 K=1,N
      SUM1=SUM1+V(I,K)*ZEIG(K)*   V(J,K)
      SUM2=SUM2+V(I,K)/ZEIG(K)*   V(J,K)
    6 SUM3=SUM3+V(I,K)/ZEIG(K)**2*V(J,K)
      SQX (I,J)=SUM1
      SQXI(I,J)=SUM2
    5 XI  (I,J)=SUM3
      CALL AXB(V,SQX,Y,N,N,N,KNDM,KNDM,KNDM)
      CALL AXB(Q,V,SQX,N,N,N,KNDM,KNDM,KNDM)
C ROOT DECOMPOSITION OF Z (USE Q FOR Z).
      K=0
      DO 7 I=1,N
      DO 7 J=1,I
      K=K+1
    7 ROWIJ(K)=Q(I,J)
      CALL SYMTRX(ROWIJ,N,ZEIG,V,KNDM,WK,IERR)
      IF(IERR.GT.128) THEN
        ERR='ERROR IN DECOMPOSITION OF Z IN GETQM'
        RETURN
      ENDIF
C CHECK MINIMUM EIGENVALUE
      IMN=1
      ZMN=ZEIG(1)
      IF(N.GE.2) THEN
        DO 8 J=2,N
        IF(ZEIG(J).LT.ZMN) THEN
          IMN=J
          ZMN=ZEIG(J)
        ENDIF
    8   CONTINUE
      ENDIF
C RENORMALIZATION
      IF(IW0.EQ.1) ZEIG(IMN)=0
C
      DO 9 I=1,N
      IF(ZEIG(I).LT.0.0) THEN
        ERR='NON-POSITIVE EIGENVALUE OF Z'
        RETURN
      ENDIF
    9 ZEIG(I)=SQRT(ZEIG(I))
C Q-MATRICES
      DO 10 I=1,N
      DO 10 J=1,N
      SUM1=0
      SUM2=0
      DO 11 K=1,N
      SUM1=SUM1+SQX(I,K)*V(K,J)
   11 SUM2=SUM2+V(K,I)*SQXI(K,J)
      Q (I,J)=SUM1
   10 QI(I,J)=SUM2
      RETURN
      END
      SUBROUTINE CINGR(NDA,NA0,L,AM0,TAU,RDN,RUP,VPE,VME,C1E,C2E
     &  ,NPLK1,DPE,DME,ALFA,BETA)
C GET ALFA AND BETA (INTEGRAL CONSTANTS).
C PARAMETERS
      PARAMETER (KNA0  =2)
      PARAMETER (KNDM  =16)
      PARAMETER (KNLN  =35)
      PARAMETER (KPLK1 =2)
C
      PARAMETER (KNLN1=KNLN+1)
      PARAMETER (KNLNM=KNLN1,KNLNM1=KNLNM+1)
      PARAMETER (PI=3.141592653,RAD=PI/180.0)
      DIMENSION AM0(KNA0),ALFA(KNDM,KNA0),BETA(KNDM,KNA0)
     &,RUP(KNDM,KNA0,KNLNM1),RDN(KNDM,KNA0,KNLNM1)
     &,DPE(KNDM,KPLK1,KNLN),DME(KNDM,KPLK1,KNLN)
     &,VPE(KNDM,KNA0,KNLN),VME(KNDM,KNA0,KNLN)
     &,C1E(KNDM,KNDM,KNLN),C2E(KNDM,KNDM,KNLN)
C WORK AREAS
      DIMENSION BUF1(KNDM),BUF2(KNDM)
C
      L1=L+1
      DO 1 J=1,NA0
        EX1=-TAU/AM0(J)
        EX1=EXPFN(EX1)
        DO 2 I=1,NDA
        SUM1=RDN(I,J,L)-VPE(I,J,L)
        SUM2=RUP(I,J,L1)-VME(I,J,L)*EX1
        IF(NPLK1.GT.0) THEN
          SUM1=SUM1-DPE(I,1,L)
          SUM=0
          DO 3 K=1,NPLK1
    3     SUM=SUM+DME(I,K,L)*TAU**(K-1)
          SUM2=SUM2-SUM
        ENDIF
        BUF1(I)=SUM2+SUM1
    2   BUF2(I)=SUM2-SUM1
        DO 4 I=1,NDA
        SUM1=0
        SUM2=0
        DO 5 K=1,NDA
        SUM1=SUM1+C1E(I,K,L)*BUF1(K)
    5   SUM2=SUM2+C2E(I,K,L)*BUF2(K)
        ALFA(I,J)=SUM1
    4   BETA(I,J)=SUM2
    1 CONTINUE
      RETURN
      END
      SUBROUTINE ADISC(M,L,NDA,NA0,AM0,WMP,TC,T1,ZEE,QE,QIE
     &  ,VPE,VME,ALFA,BETA,NPLK1,DPE,DME,UDN,UUP)
C INTENSITY AT A USER DEFINED DEPTH.
C--- HISTORY
C 87. 3. 9
C 89.11. 2 MODIFIED
C--- INPUT
C M        I          FOURIER ORDER
C L        I          LAYER NUMBER.
C NDA      I          NUMBER OF QUADRATURE POINTS.
C NA0      I          NUMBER OF SOLAR ZENITH ANGLES.
C AM0    R(KNA0)      COS(SOLAR ZENITH ANGLES).
C WMP    R(KNDM)      SQRT(W*M)
C TC       R          OPTICAL THICKNESS OF THE LAYER.
C T1       R          OPITCAL DEPTH OF INTERPOLATION MEASURED FROM TOP.
C ZEE    R(KNDM,KNLN)  EIGENVALUES
C QE    R(KNDM,KNDM,KNLN)
C QIE   R(KNDM,KNDM,KNLN)
C VPE   R(KNDM,KNA0,KNLN)
C VME   R(KNDM,KNA0,KNLN)
C DPE   R(KNDM,KPLK1,KNLN)
C DME   R(KNDM,KPLK1,KNLN)
C ALFA  R(KNDM,KNA0)  INTEGRAL CONSTANT ALFA.
C BETA  R(KNDM,KNA0)  INTEGRAL CONSTATN BETA.
C--- OUTPUT
C UUP   R(KNDM,KNA0)  UPWARD INTENSITY (UNSCALE)
C UDN   R(KNDM,KNA0)  DNWARD INTENSITY (UNSCALE)
C
      PARAMETER (KNLN  =35)
      PARAMETER (KNA0  =2)
      PARAMETER (KNDM  =16)
      PARAMETER (KPLK1 =2)
C--- AREAS FOR THE ROUTINE
      DIMENSION AM0(NA0),ZEE(KNDM,KNLN),WMP(KNDM)
     &,QE(KNDM,KNDM,KNLN),QIE(KNDM,KNDM,KNLN)
     &,VPE(KNDM,KNA0,KNLN),VME(KNDM,KNA0,KNLN)
     &,ALFA(KNDM,KNA0),BETA(KNDM,KNA0)
     &,DPE(KNDM,KPLK1,KNLN),DME(KNDM,KPLK1,KNLN)
     &,UUP(KNDM,KNA0),UDN(KNDM,KNA0)
C--- WORK AREAS
      DIMENSION C(KNDM),SL(KNDM),SL1(KNDM),E0(KNA0)
     & ,AP(KNDM),AM(KNDM),BP(KNDM),BM(KNDM)
C A+-, B+-
      DO 1 I=1,NDA
    1 CALL CSFN(TC,T1,ZEE(I,L),C(I),SL(I),SL1(I))
      DO 2 J=1,NA0
      EX1=-T1/AM0(J)
    2 E0(J)=EXPFN(EX1)
C U+ = S1D,  U- = S1U
      DO 3 I=1,NDA
        DO 4 J=1,NDA
        AP(J)=QE(I,J,L)*C  (J) - QIE(J,I,L)*SL(J)
        AM(J)=QE(I,J,L)*C  (J) + QIE(J,I,L)*SL(J)
        BP(J)=QE(I,J,L)*SL1(J) - QIE(J,I,L)*C (J)
    4   BM(J)=QE(I,J,L)*SL1(J) + QIE(J,I,L)*C (J)
        DP=0
        DM=0
        IF(M.EQ.0 .AND. NPLK1.GT.0) THEN
          TAUN=1
          DO 5 J=1,NPLK1
          DP=DP+DPE(I,J,L)*TAUN
          DM=DM+DME(I,J,L)*TAUN
    5     TAUN=TAUN*T1
        ENDIF
        DO 3 J=1,NA0
        SUM1=0
        SUM2=0
        DO 6 K=1,NDA
        SUM1=SUM1+AP(K)*ALFA(K,J)+BP(K)*BETA(K,J)
    6   SUM2=SUM2+AM(K)*ALFA(K,J)+BM(K)*BETA(K,J)
        UDN(I,J)=(SUM1+VPE(I,J,L)*E0(J)+DP)/WMP(I)
        UUP(I,J)=(SUM2+VME(I,J,L)*E0(J)+DM)/WMP(I)
    3 CONTINUE
      RETURN
      END
C
C 2002.05.21 Delete AINT for PT BY SED
C
      FUNCTION EXX(EX,T1,T2,X)
C EXP(EX)*INTEG(FROM T1 TO T2) DT EXP(XT)
C--- HISTORY
C 89.11. 3 MODIFIED
      SAVE EPS,EXMN,INIT
      DIMENSION CCP(3)
      DATA INIT/1/
      IF(INIT.EQ.1) THEN
        INIT=0
        CALL CPCON(CCP)
        EPS=CCP(1)*100
        EXMN=CCP(2)*0.8*2.3
      ENDIF
      EX1=EX+X*T1
      EX2=EXPFN(EX1)
      T22=T2-T1
      IF(ABS(T22*X).LE.EPS) THEN
         EXX=EX2*(T22+X*T22**2/2.0)
        ELSE
         EX3=EX+X*T2
         IF(EX3.LE.EXMN) THEN
           EXX=-EX2/X
          ELSE
           EXX=(EXP(EX3)-EX2)/X
         ENDIF
      ENDIF
      RETURN
      END
      SUBROUTINE EXINT(AM1U,AM0,DPT,T1,T2,C)
C INTEGRAL(FROM T1 TO T2) DT EXP(-(THK-T)/AM1U-T/AM0)
C--- HISTORY
C 90. 1.13 CREATED
C--- INPUT
C AM1U     R      USERDEFINED EMERGENT MU.
C                 IF .GT. 0 THEN DOWN,  IF .LT. 0 THEN UPWARD.
C AM0      R      SOLAR DIRECTION.
C DPT      R      OPTICAL DEPTH FOR INTERPOLATION.
C T1       R      LOWER LIMIT OF INTEGRATION.
C T2       R      UPPER LIMIT OF INTEGRATION.
C--- OUTPUT
C C        R      INTEGRATION.
      EX=-DPT/AM1U
      X=1/AM1U-1/AM0
      C=EXX(EX,T1,T2,X)
      RETURN
      END
      SUBROUTINE CSINT(AM1U,ZE,THK,DPT,T1,T2,C,SL,SL1)
C INTEGRAL(FROM T1 TO T2) DT EXP(-(DPT-T)/AM1U) C(T, THK)
C   WHERE C(T, THK) = (EXP(-Z*(THK-T)+EXP(-Z*T))/2
C WE ALSO DEFINE THE INTEGRAL FOR THE FUNCTIONS: S*Z, S/Z.
C--- HISTORY
C 90. 1.13 CREATED
C--- INPUT
C AM1U     R      USERDEFINED EMERGENT MU.
C                 IF .GT. 0 THEN DOWN,  IF .LT. 0 THEN UPWARD.
C ZE       R      EIGENVALUE (L).
C THK      R      OPTICAL THICKNESS OF LAYER.
C DPT      R      OPTICAL DEPTH FOR INTERPOLATION.
C T1       R      LOWER LIMIT OF INTEGRATION.
C T2       R      UPPER LIMIT OF INTEGRATION.
C--- OUTPUT
C C        R      INTEGRATION OF C.
C SL       R      INTEGRATION OF S*L.
C SL1      R      INTEGRATION OF S/L.
      SAVE EPS,INIT
      DIMENSION CCP(3)
      DATA INIT/1/
      IF(INIT.EQ.1) THEN
        INIT=0
        CALL CPCON(CCP)
        EPS=CCP(1)*100
      ENDIF
C C
      EX=-DPT/AM1U-ZE*THK
      X=1/AM1U+ZE
      EX1=EXX(EX,T1,T2,X)
      EX=-DPT/AM1U
      X=1/AM1U-ZE
      EX2=EXX(EX,T1,T2,X)
      C =(EX1+EX2)/2
      IF(ABS(ZE*T1).GT.EPS .OR. ABS(ZE*T2).GT.EPS) THEN
        SL1=(EX1-EX2)/2.0/ZE
       ELSE
        EX=(T1-DPT)/AM1U
        EX1=EXPFN(EX)
        EX=(T2-DPT)/AM1U
        EX2=EXPFN(EX)
        EX=-DPT/AM1U
        X=1/AM1U
        EX3=EXX(EX,T1,T2,X)
        SL1=AM1U*(T2*EX2-T1*EX1)-(AM1U+THK/2)*EX3
      ENDIF
      SL =ZE**2*SL1
      RETURN
      END
      SUBROUTINE PKINT(AM1U,DPT,T1,T2,NPLK1,C)
C INTEGRAL(FROM T1 TO T2) DT EXP(-(DPT-T)/AM1U) T**N
C--- HISTORY
C 90. 1.13 CREATED
C--- INPUT
C AM1U     R      USERDEFINED EMERGENT MU.
C                 IF .GT. 0 THEN DOWN,  IF .LT. 0 THEN UPWARD.
C DPT      R      OPTICAL DEPTH FOR INTERPOLATION.
C T1       R      LOWER LIMIT OF INTEGRATION.
C T2       R      UPPER LIMIT OF INTEGRATION.
C NPLK1    I      .GE. 1
C--- OUTPUT
C C     R(NPLK1)  INTEGRATIONS
C
      DIMENSION C(*)
      EX=-DPT/AM1U
      X=1/AM1U
      C(1)=EXX(EX,T1,T2,X)
      IF(NPLK1.GE.2) THEN
        EX=(T1-DPT)/AM1U
        EX1=EXPFN(EX)
        EX=(T2-DPT)/AM1U
        EX2=EXPFN(EX)
        DO 1 N1=2,NPLK1
        EX1=T1*EX1
        EX2=T2*EX2
    1   C(N1)=AM1U*(EX2-EX1-(N1-1)*C(N1-1))
      ENDIF
      RETURN
      END
      SUBROUTINE CSFN(TC,T,ZE,C,SL,SL1)
C GET C SL AND SL1 FUNCTIONS.
C--- HISTORY
C 89.11. 2  CREATED
C---
C 2 C = E(TC-T) + E(T),   2S = E(TC-T) - E(T)
C   E(T) = EXP(-ZE*T)
C SL = S L,   SL1 = S L**-1
C
      SAVE INIT,EPS
      DIMENSION CCP(3)
      DATA INIT/1/
      IF(INIT.EQ.1) THEN
        INIT=0
        CALL CPCON(CCP)
        EPS=CCP(1)*10
      ENDIF
      EXP1=-ZE*(TC-T)
      EXP2=-ZE*T
      EXP3=EXPFN(EXP1)
      EXP4=EXPFN(EXP2)
      C  =(EXP3+EXP4)/2
      S  =(EXP3-EXP4)/2
      SL =ZE*S
      IF(ABS(EXP1).LE.EPS .AND. ABS(EXP2).LE.EPS) THEN
        SL1 =T-TC/2
        ELSE
        SL1 =S/ZE
      ENDIF
      RETURN
      END
      FUNCTION BINTP(XX,N,X,Y)
C 2ND ORDER POLYNOMIAL INTERPOLATION
C--- HISTORY
C 88. 1.14 CREATED
C     3. 7 LINEAR EXTRAPOLATION FOR OUT-OU-BOUNDARY DATA
C--- INPUT
C XX    R     INTERPOLATION POINT
C N     R     NBR OF DATA
C X   R(NN)   INDEPENDENT VARIABLE,  X(I) .LT. X(I+1)
C Y   R(NN)   DEPENDENT   VARIABLE
C--- OUTPUT
C BINTP R     OUTPUT
C$ENDI
C
      DIMENSION X(N),Y(N)
C 1 POINT
      IF(N.LE.1) THEN
      BINTP=Y(1)
      RETURN
      ENDIF
C 2 POINTS OR XX.LE.X(1)
      IF(XX.LE.X(1) .OR. N.EQ.2) THEN
      BINTP=Y(1)+(Y(2)-Y(1))*(XX-X(1))/(X(2)-X(1))
      RETURN
      ENDIF
C XX.GE.XX(N)
      IF(XX.GE.X(N)) THEN
      BINTP=Y(N-1)+(Y(N)-Y(N-1))*(XX-X(N-1))/(X(N)-X(N-1))
      RETURN
      ENDIF
C 3 POINTS
      DO 1 I=1,N
      IF(XX.LE.X(I)) GOTO 2
    1 CONTINUE
      I=N
    2 K1=MAX(1,I-1)
      K2=K1+2
      IF(K2.GT.N) THEN
      K2=N
      K1=K2-2
      ENDIF
C
      BINTP=0.0
      I2=K1
      I3=K1+1
      DO 3 I=1,3
      I1=I2
      I2=I3
      I3=MOD(I+1,3)+K1
    3 BINTP=BINTP+(XX-X(I2))*(XX-X(I3))/(X(I1)-X(I2))/(X(I1)-X(I3))
     &     *Y(I1)
      RETURN
      END
      SUBROUTINE GTPH4B(IUK,WLC,CR3,CI3,PR,X0,GG,RP
     & ,NANG,IPOL,ANG,PH,CEXT,CABS,CG,VL,INTVL,SZP,ERR)
C GET PHASE FUNCTION AND CROSS SECTION FROM TABLE OF INTERPOLATED REFRACTIVE IND
C WITH LOG-REGULAR GRID WAVELENGTHS.
C CR3,CI3 assigned; Assume IPOL=1
C Pollack and Cuzzi method implemented
C--- HISTORY
C 95. 9.14  Created from GETPH3
C 96. 2.29  2ND ORDER POLYNOMIAL INTERPOLATION (T.Y.NAKAJIMA)
C 96. 3.11  Pollack and Cuzzi method implemented (Noguchi, Tokai)
C 96. 4. 4  DCG(IR)=DCG(IR)+CG1 -> F(INDR.LE.1) then ... else...endif
C 2001.4.11 Adding Read Error check by SED
C--- INPUT
C IUK     I      READ UNIT NUMBER OF THE KERNEL FILE.
C WLC     R      WAVELENGTH IN CM
C CR3     R      Real part of refractive index
C CI3     R      Imaginary part of refractive index (positive value)
C PR   R(6,4)    Parameter packet of size distribution (see VLSPC2)
C X0      R      Critical size parameter for Mie scattering (Pollack-Cuzzi)
C                Give 1.0E9 for Mie theory only calculations.
C GG      R      Asymmetry parameter for transmitted ray (Pollcack-Cuzzi)
C                Any value for Mie theory only calculations.
C RP      R      Surface area fraction to that of sphere (about 1.1-1.3)
C                Any value for Mie theory only calculations.
C--- OUTPUT
C NANG    I      NBR OF SCATTERING ANGLE
C INTVL   I      NBR OF SIZE PARAMETER
C IPOL    I      NBR OF POLARIZATION COMPONENT
C ANG  R(NANG)   SCATTERING ANGLE IN DEGREE
C PH   R(KNANG,IPOL) VOLUME PHASE FUNCTION
C CEXT   R       EXTINCTION CROSS SECTION (cm-1)
C CABS   R       ABSORPTION CROSS SECTION (cm-1)
C CG     R       GEOMETRICAL CROSS SECTION (cm-1)
C VL     R       Volume (cm3/cm3)
C INTVL  I       Number of size interval in the kernel file
C SZP  R(INTVL)
C---
      SAVE
      PARAMETER (KNANG =250)
      PARAMETER (KINTVL=75)
      PARAMETER (KPOL  =1)
      PARAMETER (KNANG2=KNANG+2)
      CHARACTER ERR*(*)
      DIMENSION PR(6,4),ANG(KNANG),PH(KNANG,KPOL),SZP(KINTVL)
C WORKING AREA
      PARAMETER (KSEC=3,KR=20,KI=20,KRF=200)
      PARAMETER (PI=3.141592654,RD=PI/180.0)
      CHARACTER CH*1
      DIMENSION CR(KRF),CI(KRF),Q(KINTVL,KPOL,KRF,KNANG2)
     & ,NR(KSEC),NI(KSEC),CRMN(KSEC),CRMX(KSEC),CIMN(KSEC)
     & ,CIMX(KSEC),CRT(KR,KSEC),CIT(KI,KSEC),ICT(KR,KI,KSEC)
     & ,PHM(KNANG)
CC 96.3.11
      DIMENSION PHS(KNANG),DEXT(KINTVL),DABS(KINTVL),DCG(KINTVL)
CC 96.2.29
      DIMENSION Y(3),XR(3),XI(3),XI2(3),IRF(3,3),ZI(3)
C
      DATA   NR/  3,      9,     18/
      DATA   NI/ 16,     10,      4/
      DATA CRMN/  1.3,    1.0,    1.0/
      DATA CRMX/  1.5,    1.8,    2.7/
      DATA CIMN/  1.0E-9, 1.0E-4, 1.0E-1/
      DATA CIMX/  1.0E-4, 1.0E-1, 1.0E+0/
      DATA INIT/1/
C
      IF(INIT.GT.0) THEN
        INIT=0
        REWIND IUK
C GET KERNEL ANG, SZP, AK, CEXT, CABS
C 2001.4.11 SED
C        READ(IUK,*) INTVL, NANG, IPOL
        READ(IUK,*,ERR=998) INTVL, NANG, IPOL
C SED END
        IF (NANG .GT. KNANG) THEN
          ERR = 'NANG.GT.KNANG'
          RETURN
        END IF
        IF (IPOL .GT. KPOL) THEN
          ERR = 'IPOL.GT.KPOL'
          RETURN
        END IF
        IF(IPOL.NE.1) then
          ERR='Assume only IPOL=1 case in GETPH4'
          return
        endif
C 2001.4.11 SED
C        READ(IUK,*) (SZP(I),I=1,INTVL)
C        READ(IUK,*) (ANG(I),I=1,NANG)
        READ(IUK,*,ERR=998) (SZP(I),I=1,INTVL)
        READ(IUK,*,ERR=998) (ANG(I),I=1,NANG)
C SED END
	  IF(SZP(INTVL)/SZP(1).LT.1.0E-30)THEN
		DEL=LOG(1.0E-30)/(INTVL-1)
	  ELSE
		DEL=LOG(SZP(INTVL)/SZP(1))/(INTVL-1)
	  ENDIF
        RSZP2=EXP(DEL/2)
        do 35 I=1,NANG
   35   PHM(I)=3.0/16.0/PI*(1+COS(ANG(I)*RD)**2)
CC READ KERNEL
        NRF=0
C 2001.4.11 SED
C    7   READ(IUK,2,END=5,ERR=5) CH
    7   READ(IUK,2,END=5,ERR=998) CH
C SED END
    2   FORMAT(A1)
        NRF=NRF+1
        IF(NRF.GT.KRF) THEN
          ERR='NRF .GT. KRF'
          RETURN
        ENDIF
C 2001.4.11 SED
C        READ(IUK,*) CR(NRF),CI(NRF)
        READ(IUK,*,ERR=998) CR(NRF),CI(NRF)
C SED END
        CI(NRF)=ABS(CI(NRF))
        DO 60 I = 1, INTVL
        DO 60 J = 1, IPOL
C 2001.4.11 SED
C   60     READ(IUK,*) (Q(I,J,NRF,K), K=1,NANG)
   60     READ(IUK,*,ERR=998) (Q(I,J,NRF,K), K=1,NANG)
C SED END
        DO 62 K = 1, 2
C 2001.4.11 SED
C   62     READ(IUK,*) (Q(I,1,NRF,K+NANG), I=1,INTVL)
   62     READ(IUK,*,ERR=998) (Q(I,1,NRF,K+NANG), I=1,INTVL)
C SED END
        GOTO 7
    5   CONTINUE
CC GET REFRACTIVE INDEX TABLES (3 TABLES)
        DO 8 K=1,KSEC
        DCR=(CRMX(K)-CRMN(K))/(NR(K)-1)
        DO 9 I=1,NR(K)
        CR1=CRMN(K)+DCR*(I-1)
        IF(ABS(CR1-1.0).LE.0) CR1=1.01
    9   CRT(I,K)=CR1
	  IF(CIMX(K)/CIMN(K).LT.1.0E-30)THEN
		DCI=DLOG(DBLE(1.0E-30))/(NI(K)-1)
	ELSE
        DCI=DLOG(dble(CIMX(K)/CIMN(K)))/(NI(K)-1)
	ENDIF
        DO 10 I=1,NI(K)
        CI1=DCI*(I-1)
   10   CIT(I,K)=CIMN(K)*EXP(CI1)
    8   CONTINUE
CC FIND GRIDS FOR KERNEL REFRACTIVE INDICES
        DO 11 K=1,KSEC
        DO 11 I=1,NR(K)
        DO 11 J=1,NI(K)
   11   ICT(I,J,K)=0
        DO 12 L=1,NRF
        CR1=CR(L)
        CI1=CI(L)
        DO 13 K=1,KSEC
        DO 14 I=1,NR(K)
        IF(ABS(CRT(I,K)/CR1-1).LE.0.01) GOTO 15
   14   CONTINUE
        GOTO 13
   15   DO 17 J=1,NI(K)
        IF(ABS(CIT(J,K)/CI1-1).LE.0.01) GOTO 16
   17   CONTINUE
        GOTO 13
   16   ICT(I,J,K)=L
   13   CONTINUE
   12   CONTINUE
        DO 18 K=1,KSEC
        DO 18 I=1,NR(K)
        DO 18 J=1,NI(K)
        IF(ICT(I,J,K).EQ.0) THEN
          ERR='NO MATCHING GRID'
          RETURN
        ENDIF
   18   CONTINUE
C NUMBER OF AVERAGING
        NAV=10
      ENDIF
C Initialization end
      WVN=2*PI/WLC
C Find the domain
      DO 23 KP=1,KSEC
      IF(CI3.LE.CIMX(KP)) GOTO 25
   23 CONTINUE
      KP=KSEC
   25 NS=NR(KP)
      NT=NI(KP)
      DO 26 IP=1,NS-1
      IF(CR3.LE.CRT(IP,KP)) GOTO 27
   26 CONTINUE
      IP=NS
   27 DO 28 JP=1,NT-1
      IF(CI3.LE.CIT(JP,KP)) GOTO 29
   28 CONTINUE
      JP=NT
   29 IP=MAX(1, IP-1)
      JP=MAX(1, JP-1)
C GET GRID POINTS
CC 96.2.29
      NINTPR=3
      NINTPI=3
      IF(NINTPR.GT.NR(KP))NINTPR=NR(KP)
      IF(NINTPI.GT.NI(KP))NINTPI=NI(KP)
      IPT=IP
      JPT=JP
      IF(IP.GE.NR(KP)-1)IPT=NR(KP)-2
      IF(JP.GE.NI(KP)-1)JPT=NI(KP)-2
      DO 72 IG=1,NINTPR
   72 XR(IG)=CR(ICT(IPT+IG-1,JPT     ,KP))
      DO 73 IG=1,NINTPI
   73 XI(IG)=CI(ICT(IPT,     JPT+IG-1,KP))
      DO 74 IX=1,NINTPR
      DO 74 IY=1,NINTPI
   74 IRF(IX,IY)=ICT(IPT+IX-1,JPT+IY-1,KP)
CC 96.2.29
CC      IRF00=ICT(IP  ,JP  ,KP)
CC      IRF10=ICT(IP+1,JP  ,KP)
CC      IRF01=ICT(IP  ,JP+1,KP)
CC      IRF11=ICT(IP+1,JP+1,KP)
C GET INTERPOLATED OPTICAL CONSTANTS
      CG=0
      VL=0
      CEXT=0
      CABS=0
CC 96.3.11
      DCGL=0.0
      DCGS=0.0
      IX0=0
  100 IX0=IX0+1
      IF((SZP(IX0).LE.X0).AND.(IX0.LE.INTVL)) GOTO 100
      DO 20 L=1,NANG
      PHS(L)=0
CC 96.3.11 end
   20 PH(L,1)=0
CC
      RMIN=PR(1,3)
      RMAX=PR(1,4)
      RMIN1=SZP(1)/WVN/RSZP2
      IF(RMIN1.GT.RMIN) then
	  IF(RMIN1/RMIN.LT.1.0E-30)THEN
		INTVL9=LOG(1.0E-30)/DEL+1
	ELSE
        INTVL9=LOG(RMIN1/RMIN)/DEL+1
	ENDIF
        R2=RMIN1*EXP(-INTVL9*DEL)
       else
        INTVL9=0
      endif
      INTVL8=INTVL+INTVL9
      DO 30 IR8=1,INTVL8
      IR=IR8-INTVL9
      IF(IR.LE.0) THEN
        INDR=1
        R1=R2
        R2=R1*RSZP2**2
       else
CC 96.3.11
        DCG(IR)=0
        INDR=2
        R1=SZP(IR)/WVN/RSZP2
        R2=SZP(IR)/WVN*RSZP2
      endif
      DR=(R2-R1)/NAV
      R3=R1-DR/2
      V1=0.0
      DO 34 J=1,NAV
      R3=R3+DR
C      X=R3*WVN
	IF(R3/(R3-DR).LT.1.0E-30)THEN
		DLR=LOG(1.0E-30)
	ELSE
      DLR=LOG(R3/(R3-DR))
	ENDIF
      PR(1,1)=R3
      V2=VLSPC2(PR)
CC 96.3.11
      CG1=V2*3.0/4.0/R3*DLR
      CG=CG+CG1
      VL=VL+V2*DLR
      V1=V1+V2
CC 96.3.11
      IF(IR.LE.IX0)THEN
       DCGS=DCGS+CG1
      ELSE
       DCGL=DCGL+CG1
      ENDIF
CC 96.4.4
      IF(INDR.LE.1) then
        DCG(1)=DCG(1)+CG1
       else
        DCG(IR)=DCG(IR)+CG1
      endif
CCC 96.4.4 end
CC 96.3.11 end
   34 continue
      V1=V1/REAL(NAV)
C INTERPOLATION
      IF(INDR.EQ.1) then
        X=(R1+R2)/2*WVN
        call SMLLOP(X,CR3,CI3,QEXT9,QSCA9)
        QABS9=QEXT9-QSCA9
        COF=DEL*3.0/4.0/X*V1*WVN
        CEXT=CEXT+QEXT9*COF
        CABS=CABS+QABS9*COF
        do 36 I=1,NANG
   36   PH(I,1)=PH(I,1)+PHM(I)*COF*QSCA9
       else
        DO 31 L=1,NANG+2
C INTERPOLATION TO GET CORRESPONDING Q TO CR3 AND CI3
CC 96.2.29
         DO 80 IG=1,NINTPI
   80    XI2(IG)=LOG(XI(IG))
         DO 81 IX=1,NINTPR
         IC=0
         DO 82 IY=1,NINTPI
         IC=IC+1
         IRFI=IRF(IX,IY)
		IF(Q(IR,1,IRFI,L).LT.1.0E-30)Q(IR,1,IRFI,L)=1.0E-30
         Y(IC)= LOG(Q(IR, 1, IRFI, L))
   82    CONTINUE
	IF(CI3.LT.1.0E-30)CI3=1.0E-30
         PINT=LOG(CI3)
         ZI(IX)= BINTP(PINT,NINTPI,XI2,Y)
   81    CONTINUE
         Z=BINTP(CR3,NINTPR,XR,ZI)
CC 96.2.29
CC        PZ00=LOG(Q(IR,1,IRF00,L))
CC        PZ10=LOG(Q(IR,1,IRF10,L))
CC        PZ01=LOG(Q(IR,1,IRF01,L))
CC        PZ11=LOG(Q(IR,1,IRF11,L))
CC        Z1=PZ00+(CR3-CRT(IP,KP))/(CRT(IP+1,KP)-CRT(IP,KP))*(PZ10-PZ00)
CC        Z2=PZ01+(CR3-CRT(IP,KP))/(CRT(IP+1,KP)-CRT(IP,KP))*(PZ11-PZ01)
CC        Z=Z1+LOG(CI3/CIT(JP,KP))/LOG(CIT(JP+1,KP)/CIT(JP,KP))*(Z2-Z1)
        Z=EXP(Z)*V1*WVN
        IF(L.LE.NANG) THEN
          PH(L,1)=PH(L,1)+Z
CC 96.3.11
          IF(IR.LE.IX0) THEN
           PHS(L)=PHS(L)+Z
          ENDIF
CC 96.3.11 end
         ELSE IF(L.EQ.NANG+1) THEN
          CEXT=CEXT+Z
CC 96.3.11
          DEXT(IR)=Z
        ELSE IF(L.EQ.NANG+2) THEN
          CABS=CABS+Z
CC 96.3.11
          DABS(IR)=Z
        ENDIF
   31   CONTINUE
      endif
   30 continue
CC 96.3.11
      IF(INDR.NE.1) THEN
        CALL      NONS(INTVL,IX0,NANG,NAV,CG,CR3,CI3,DCGS,DCGL,GG
     &               ,RMAX,RMIN,RP,WVN,X0,ANG,DABS,DCG,DEXT,PHS,SZP
     &               ,PH,CEXT,CABS,ERR)
      ENDIF
CC 96.3.11 end
      ERR=' '
      RETURN
C 2001.4.11 SED
  998 ERR='Reference File Read Error (KRNL.OUT)'
      RETURN
C SED END
      END
      SUBROUTINE NONS(INTVL,IX0,NANG,NAV,CG,CR1,CI1,DCGS,DCGL,GG
     &               ,RMAX,RMIN,RP,WVN,X0,ANG,DABS,DCG,DEXT,PHS,SZP
     &               ,PH,CEXT,CABS,ERR)
C Semi Empirical Theory
      PARAMETER (KNANG =250)
      PARAMETER (KINTVL=75)
      PARAMETER (KPOL  =1)
      PARAMETER (KNANG2=KNANG+2)
      PARAMETER (PI=3.141592653,RAD=PI/180.0)
      DIMENSION ANG(KNANG),DABS(KINTVL),DCG(KINTVL),DEXT(KINTVL)
     &         ,PHS(KNANG),SZP(KINTVL),PH(KNANG,KPOL)
      CHARACTER ERR*(*)
C Working Area
      DIMENSION DID(KNANG),DIR(KNANG),DIT(KNANG)
C Semi Empirical Theory
      IF(IX0.GT.INTVL) GOTO 999
      DCB=0
      XMIN=RMIN*WVN
      XMAX=RMAX*WVN
      IMN=0
  107 IMN=IMN+1
      IF(SZP(IMN).LT.XMIN) GOTO 107
      IMX=INTVL+1
  108 IMX=IMX-1
      IF(SZP(IMX).GT.XMAX) GOTO 108
      IF(IMN.NE.1) IMN=IMN-1
      IF(IMX.LE.IX0) GOTO 999
      DM2=CR1**2.0+CI1**2.0
      DO 101 IA=1,NANG
       DIDD=0.0
       AZ=ANG(IA)
       IF(AZ.LT.0.018)THEN
        AZ=0.018
       ENDIF
       IF(AZ.GT.179.982)THEN
        AZ=179.982
       ENDIF
       SN=SIN(AZ*RAD)
       SN2=SIN(AZ*RAD/2.0)
       CS=COS(AZ*RAD)
       CS2=COS(AZ*RAD/2.0)
       DO 102 IR=IX0,IMX
         XB=SQRT(RP)*SZP(IR)
         XBZ=XB*SN
         DJ=0.0
         DO 109 JJ=1,NANG-1
          DX1=ANG(JJ+1)*RAD
          DX=ANG(JJ)*RAD
          DY1=COS(DX1-XBZ*SIN(DX1))
          DY=COS(DX-XBZ*SIN(DX))
          DJ=DJ+(DY1+DY)*(DX1-DX)/2.0
  109    CONTINUE
        dd=RP*XB*XB*DJ*DJ*0.5*(1.0+CS**2.0)/(PI*XBZ*XBZ)*DCG(IR)
        DIDD=DIDD+DD
  102  CONTINUE
       IF(ANG(IA).LE.30.0) THEN
        DID(IA)=DIDD
        DCB=DIDD
       ELSE
        DID(IA)=0
       ENDIF
       SQ=SQRT(DM2-1.0+SN2**2.0)
       DIR(IA)=1.0-2.0*SN2*SQ*(1.0/(SN2+SQ)**2.0+DM2/(DM2*SN2+SQ)**2.0)
	IF(GG.LT.1.0E-30)GG=1.0E-30
       DIT(IA)=EXP(1.0-LOG(GG)/LOG(1.53171*PI)*ANG(IA)*RAD)
  101 CONTINUE
      DO 201 I=1,NANG
        IF(DID(I).GT.DCB) THEN
          DID(I)=DID(I)-DCB
        ELSE
          DID(I)=0
        ENDIF
  201 CONTINUE
      ZS=PHINTG(NANG,ANG,PHS)
      ZD=PHINTG(NANG,ANG,DID) !/4.0/PI
      ZR=PHINTG(NANG,ANG,DIR) !/4.0/PI
      ZT=PHINTG(NANG,ANG,DIT) !/4.0/PI
      DO 104 IA=1,NANG
       PHS(IA)=PHS(IA)/ZS
       DID(IA)=DID(IA)/ZD
       DIR(IA)=DIR(IA)/ZR
       DIT(IA)=DIT(IA)/ZT
  104 CONTINUE
      QSS=0.0
      QSA=0.0
      QLS=0.0
      QLA=0.0
      DO 105 IR=IMN,IMX
       IF(IR.LE.IX0)THEN
        QSS=QSS+DEXT(IR)-DABS(IR)
        QSA=QSA+DABS(IR)
       ELSE
        QLS=QLS+DEXT(IR)-DABS(IR)
        QLA=QLA+DABS(IR)
       ENDIF
  105 CONTINUE
      QSS=QSS/DCGS
      QSA=QSA/DCGS
      QLS=QLS/DCGL
      QLA=QLA/DCGL
      FLS=DCGL/CG
      QS=QLS*FLS*RP+(1-FLS)*QSS
      QA=QLA*FLS+(1-FLS)*QSA
      QE=QS+QA
      QD=1.0
      QR=ZR /4.0/PI
      QT=QLS-QD-QR
      DO 106 IA=1,NANG
       PH(IA,1)=PHS(IA)*(1-FLS)*QSS+RP*FLS
     &  *(DID(IA)*QD+DIR(IA)*QR+DIT(IA)*QT)
       PH(IA,1)=PH(IA,1)*CG
  106 CONTINUE
      CEXT=QE*CG
      CABS=QA*CG
  999 ERR=' '
      RETURN
      END

      FUNCTION PHINTG(NANG,ANG,PH)
C---INPUT
C NANG    I      NBR OF SCATTERING ANGLE
C ANG  R(NANG)   INDEPENDENT VARIABLES (SCAT. ANG.)
C PH   R(NANG)   DEPENDENT VARIABLES
C---OUTPUT
C PHINTG  FR     INTEGRAL
C
      PARAMETER (KNANG =250)
      PARAMETER (PI=3.141592653,RAD=PI/180.0,PSTD=1013.25)
      DIMENSION ANG(KNANG),PH(KNANG)
      PHINTG=0.0
      DO 100 IA=NANG,2,-1
       DX=COS(ANG(IA-1)*RAD)-COS(ANG(IA)*RAD)
       PHINTG=PHINTG+DX*(PH(IA-1)+PH(IA))/2.0
  100 CONTINUE
      PHINTG=PHINTG*2.0*PI
      RETURN
      END

      FUNCTION RAWB(RH1,R,RHO,NAW,AW,RMMD)
C Growth of wet aerosols
C RAW is r(Aw) in eq.(2) in Shettle and Fenn.
C--- HISTORY
C 94.12.26 CREATED by I. Lensky
C 95. 9.13 Modified by T. Nakajima
C 96. 5. 5 Drop CAW, Number of iterations 10 -> 6
C--- input
C RH1    R           relative humidity 0 <= RH1 < 1
C R      R           dry aerosol radius (cm)
C RHO    R           paricle density relative to water
C NAW    I           Number of AW (If 0 then no growth)
C AW     R(KAW)      water activity
C RMMD   R(KAW)      Hanel's water uptake data
C--- output
C RAW    R           r(aw) mode radius of wet aerosols (cm)
C
C$ENDI
      DIMENSION AW(*),RMMD(*)
C
      IF (NAW.LE.0 .or. RH1.LT.AW(1)) THEN
        RAWB = R
        RETURN
      ENDIF
C iteration
      AW1 = RH1
      DO 3 IT=1,6
C Eq.(5)  of Shettle & Fenn
      IF(AW1.LE.AW(1)) then
        IAW=1
       else IF(AW1.GE.AW(NAW)) then
C        IAW=NAW-1
        IAW=NAW
       else
        DO 1 IAW=1,NAW-1
        IF(AW1.GE.AW(IAW) .AND. AW1.LT.AW(IAW+1)) GOTO 2
    1   CONTINUE
      endif
    2 CAW=LOG(RMMD(IAW+1)/RMMD(IAW))/LOG((1-AW(IAW+1))/(1-AW(IAW)))
c       write(*,*)(1-AW(IAW+1))/(1-AW(IAW))
      R1=RMMD(IAW)*((1-AW1)/(1-AW(IAW)))**CAW
      RAWB=R*(1+RHO*R1)**(1.0/3.0)
C AW1 is Aw, Eq.(4) in Shettle & Fenn. RAWB in cm.
      AW1=RH1*EXP(-0.001056/(10000*RAWB))
    3 CONTINUE
      RETURN
      END
      FUNCTION VLSPC2(PR)
C Volume spectrum of partile polydisperison: v(x) = dV / d ln r
C--- HISTORY
C 95. 9.14 Created with parameter packet only
C--- INPUT
C PR    R(6,4)      Parameter packet
C
C    PR(1,1)=r       Particle radius in cm
C    PR(1,2)=NMODE   Number of mode radius
C    PR(1,3)=rmin    Minimum particle radius in cm
C    PR(1,4)=rmax    Maximum particle radius in cm
C
C    For each j-th mode (<= 4)
C
C  PR(2,j): Type of function (ITP) for the mode.
C   ITP=1: power law
C     PR(3,j)=C, PR(4,j)=R0,  PR(5,j)=P
C     vj = C * (R/R0)**(4-P) if R>R0; = C * (R/R0)**4 if R<R0
C   ITP=2: log-normal
C     PR(3,j)=C, PR(4,j)=S,   PR(5,j)=RM
C     vj = C * exp((ln(R/RM)/ln(S))**2 / 2)
C   ITP=3: modified gamma
C     PR(3,j)=C, PR(4,j)=ALFA, PR(5,j)=BETA, PR(6,j)=GAMMA
C     vj = C * (R1)**(ALFA+4) exp (-BETA*R1**GAMMA) where R1=R*1.0E4
C--- OUTPUT
C VLSPC2   RF       dV/d ln r
C$ENDI
C
      dimension PR(6,4)
C
      R    =PR(1,1)
      NMODE=PR(1,2)+0.001
      RMIN =PR(1,3)
      RMAX =PR(1,4)
      VLSPC2=0
      IF(R.LT.RMIN .OR. R.GT.RMAX) RETURN
      DO 101 M=1,NMODE
      ITP=PR(2,M)+0.001
      IF(ITP.EQ.2) GOTO 4
      IF(ITP.EQ.3) GOTO 5
C POWER LAW
      C    =PR(3,M)
      RC   =PR(4,M)
      PDNDR=PR(5,M)
      IF(R.LE.RC) THEN
      PN=4.0
      ELSE
      PN=4.0-PDNDR
      ENDIF
	IF(R/RC.LT.1.0E-30)THEN
		E1=PN*LOG(1.0E-30)
	ELSE

      E1=PN*LOG(R/RC)
	ENDIF
      GOTO 100
C LOG-NORMAL
    4 C =PR(3,M)
      S =PR(4,M)
      RM=PR(5,M)
	IF(S.LT.1.0E-30)S=1.0E-30
	IF(R/RM.LT.1.0E-30)THEN
	E1=-0.5*(LOG(1.0E-30)/LOG(S))**2
	ELSE
		
      E1=-0.5*(LOG(R/RM)/LOG(S))**2
	ENDIF
      GOTO 100
C MODIFIED GAMMA
    5 R1=R*1.0E4
      C  =PR(3,M)
      ALF=PR(4,M)
      BET=PR(5,M)
      GAM=PR(6,M)
	IF(R1.LT.1.0E-30)R1=1.0E-30
      E1=(ALF+4)*LOG(R1)-BET*R1**GAM
  100 IF(E1.GT.-100.0) VLSPC2=VLSPC2+C*EXP(E1)
  101 CONTINUE
      RETURN
      END
      FUNCTION EXPFN(X)
C EXPONENTIAL FUNCTION WITH OVER/UNDER FLOW SETTING.
C--- HISTORY
C 89. 5. 4   CREATED BY T. NAKAJIMA.
C 90. 1.17   UPDATED WITH CPCON
C--- INPUT
C X        R         INDEPENDENT VARIABLE.
C--- OUTPUT
C EXPFN    F         EXP(X).
C                     IF X.LE. VMN THEN EXP(X) IS RESET AS   0.
C                     IF X.GE. VMX THEN EXP(X) IS RESET AS EXP(VMX).
C--- PARAMETERS
C SYSTEM SET THE -VMN- AND -VMX- BY THE FUNCTION R1MACH.
C
      SAVE INIT,VMN,VMX,EXPMN,EXPMX
      DIMENSION CCP(3)
      DATA INIT/1/
C
C SET VMN      R         ENDERFLOW LIMIT.
C     VMX      R         OVERFLOW LIMT.
      IF(INIT.GT.0) THEN
        INIT=0
        CALL CPCON(CCP)
        VMN=CCP(2)*0.8*2.3
        VMX=CCP(3)*0.8*2.3
        EXPMN=0
        EXPMX=EXP(VMX)
      ENDIF
C
      IF(X.LE.VMN) THEN
        EXPFN=EXPMN
        ELSE
        IF(X.GE.VMX) THEN
          EXPFN=EXPMX
          ELSE
          EXPFN=EXP(X)
        ENDIF
      ENDIF
      RETURN
      END
      SUBROUTINE LGNDF3(LMAX1,N,X,Y,G)
C
C LEGENDRE EXPANSION (SAME AS LGNDF2 BUT GENERATING G-MOMENTS).
C--- HISTORY
C 90. 1.20 CREATED FROM LGNDF2, USE EXPFN.
C          DIRECTION OF INTEGRATION FROM X(1) TO X(N).
C 91. 2.16 STRIP NG FROM SAVE STATEMENT.
C 92. 4. 3 KILL THE STATEMENT OF GW=GW/2 AFTER QGAUSN
C     6.22 ADD GW/2 AGAIN BECAUSE THE ABOVE CHANGE IS MISTAKE
C--- INPUT
C LMAX1      I      MAXIMUM ORDER + 1.
C N          I      NUMBER OF DATA ON (-1, 1). .GE. 4.
C X        R(NA)    INDEPENDENT VARIABLES ON (-1, 1)
C Y        R(NA)    Y(X).
C--- OUTPUT
C G    R(LMAX1)     Y = SUM(L1=1,LMAX1) (2*L1-1)*G(L1)*PL(L1)
C                      WHERE PL(L1) IS (L1-1)TH ORDER LEGENDRE
C                      POLYNOMIAL.
C$ENDI
      SAVE INIT,GW,GX
C VARIABLES FOR THE ROUTINE.
      DIMENSION X(N),Y(N),G(LMAX1)
C WORKING AREAS.
      PARAMETER (PI=3.141592653589793, RAD=PI/180.0)
      PARAMETER (NG=5)
      DIMENSION GX(NG),GW(NG)
      DATA INIT/1/
C SHIFTED GAUSSIAN QUADRATURE.
      IF(INIT.GE.1) THEN
        INIT=0
        CALL QGAUSN(GW,GX,NG)
        DO 1 I=1,NG
    1   GW(I)=GW(I)/2
      ENDIF
C CLEAR
      DO 2 L1=1,LMAX1
    2 G(L1)=0
C LOOP FOR ANGLE
      DO 3 I=1,N-1
C CUBIC INTERPOLATION
      IF(I .LE. 2) THEN
        I1=1
        I4=4
       ELSE
        IF(I .LE. N-2) THEN
          I1=I-1
          I4=I+2
         ELSE
          I1=N-3
          I4=N
        ENDIF
      ENDIF
CC
      I2=I1+1
      I3=I2+1
      X1=X(I1)
      X2=X(I2)
      X3=X(I3)
      X4=X(I4)

      IF(   (Y(I1) .LE. 0) .OR. (Y(I2) .LE. 0) .OR. (Y(I3) .LE. 0)
     & .OR. (Y(I4) .LE. 0) ) THEN
        ISIGN=-1
        ALP1=Y(I1)
        ALP2=Y(I2)
        ALP3=Y(I3)
        ALP4=Y(I4)
       ELSE
        ISIGN=1
        ALP1=LOG(Y(I1))
        ALP2=LOG(Y(I2))
        ALP3=LOG(Y(I3))
        ALP4=LOG(Y(I4))
      ENDIF
C LOOP FOR GAUSSIAN INTEGRATION
      DO 4 J=1,NG
CC INTERPOLATED VALUE OF Y
      XX=X(I)+GX(J)*(X(I+1)-X(I))
      WW=GW(J)*(X(I+1)-X(I))
      PP=(XX-X2)*(XX-X3)*(XX-X4)/(X1-X2)/(X1-X3)/(X1-X4)*ALP1
     &  +(XX-X1)*(XX-X3)*(XX-X4)/(X2-X1)/(X2-X3)/(X2-X4)*ALP2
     &  +(XX-X1)*(XX-X2)*(XX-X4)/(X3-X1)/(X3-X2)/(X3-X4)*ALP3
     &  +(XX-X1)*(XX-X2)*(XX-X3)/(X4-X1)/(X4-X2)/(X4-X3)*ALP4
      IF(ISIGN .EQ. 1) PP=EXPFN(PP)
C LEGENDRE SUM
      PL1=0
      PL=1
      G(1)=G(1)+PP*WW
      IF(LMAX1.GE.2) THEN
        DO 5 L1=2,LMAX1
        PL2=PL1
        PL1=PL
        PL=((2*L1-3)*XX*PL1-(L1-2)*PL2)/(L1-1)
    5   G(L1)=G(L1)+PP*PL*WW
      ENDIF
    4 CONTINUE
    3 CONTINUE
      RETURN
      END
      FUNCTION PLGD(INIT,X)
C LEGENDRE POLYNOMIALS
C--- HISTORY
C 87.11.12 CREATED
C 90. 1.16 SAVE STATEMENT
C--- INPUT
C INIT   I    IF 1 THEN L=0
C             IF 0 THEN L=L+1
C X      R    (-1,1)
C--- OUT
C INIT=0
C$ENDI
C
      SAVE L,PL,PL1,PL2
      IF(INIT.GT.0) THEN
        INIT=0
        L=-1
      ENDIF
      L=L+1
      IF(L.EQ.0) THEN
        PL=1
       ELSE
        IF(L.EQ.1) THEN
          PL1=PL
          PL=X
         ELSE
          PL2=PL1
          PL1=PL
          PL=((2*L-1)*X*PL1-(L-1)*PL2)/REAL(L)
        ENDIF
      ENDIF
      PLGD=PL
      RETURN
      END
      FUNCTION SEARF1(AMUE,AMUI,PHI,U10,CR,CI)
C BIDIRECTIONAL REFLECTIVITY OF OCEAN SURFACE
C SHADOWING FACTOR FOR SINGLE SCATTERING
C SO THAT ENERGY CONSERVATION IS NOT SATISFIED
C--- HISTORY
C 92. 9. 1 CREATED BY HASUMI
C    12.23 MODIFIED BY NAKAJIMA
C 95. 6. 2 Generated from SEAREF with CR and CI
C 2000.2.3 Debug parameter for FRNLR.
C--- INPUT
C AMUE     R    COSINE OF ZENITH ANGLE OF EMERGENT RAY .GT. 0
C AMUI     R    COSINE OF ZENITH ANGLE OF INCIDENT RAY .GT. 0
C PHI      R    AZIMURTHAL ANGLE  (RADIAN)
C U10      R    WIND VELOCITY IN M/S AT 10M HEIGHT
C CR       R    Relative refractive index of the media
C               About 1.33 for atmosphere to ocean incidence,
C               and 1/1.33 for ocean to atmosphere incidence.
C CI       R    Relative refractive index for imaginary part
C               M = CR + I*CI
C--- OUTPUT
C SEAREF  RF    BIDIRECTIONAL REFLECTION FUNCTION
C
      PARAMETER (PI = 3.141592654,SQRTPI=1.7724539)
C2000.2.3. Add RAD by T.Y.NAKAJIMA
      PARAMETER (RAD=PI/180.0)
cs      write(6,*)'ITOFF',ITOFF
cs      IF(ITOFF.EQ.1)THEN
cs      write(6,*)'IN SEARF1'
cs      write(6,*)'AMUE',AMUE
cs      write(6,*)'AMUI',AMUI
cs      write(6,*)'PHI',PHI
cs      write(6,*)'U10',U10
cs      write(6,*)'CR',CR
cs      write(6,*)'CI',CI
cs      ENDIF
      AMUE1=MAX(AMUE,1.0E-7)
      ALPHA=-AMUE1*AMUI+SQRT((1-AMUE1**2)*(1-AMUI**2))*COS(PHI)
      COSW=SQRT((1-ALPHA)/2.0)
      AMUN=(AMUE1+AMUI)/2/COSW
C SHADOWING FACTOR
      SIGMA=SQRT(5.34E-3*U10)
      VIE =SIGMA*SQRT(1-AMUE1**2)/AMUE1
      IF (VIE. GT. 0.15) THEN
        VE =1/VIE
        FE=(EXP(- VE**2)/SQRTPI/VE-ERFC(VE))/2
       ELSE
        FE=0
      ENDIF
      VII =SIGMA*SQRT(1-AMUI**2)/AMUI
      IF (VII. GT. 0.15) THEN
        VI =1/VII
        FI=(EXP(- VI**2)/SQRTPI/VI-ERFC(VI))/2
       ELSE
        FI=0
      ENDIF
      G=1.0/(1+FE+FI)
C
C Debug 2000.2.3 T.Y.Nakajima. By report of Norman G. Loeb (NASA LaRC)
C
      WI=ACOS(COSW)/RAD
C      CALL FRNLR(CR,CI,COSW,COSWT,R1,R2)
      CALL FRNLR(CR,CI,WI,WR,R1,R2)
C
      R=(R1+R2)/2
      P=1/PI/SIGMA**2/AMUN**3*EXP(-(1-AMUN**2)/(SIGMA*AMUN)**2)
      SEARF1=1.0/4/AMUE1/AMUN*G*P*R
cs      IF(ITOFF.EQ.1)write(6,*)'SEARF1',SEARF1
      RETURN
      END
      SUBROUTINE CPCON(C)
C MACHINE CONSTANTS OF COMPUTER
C--- OUTPUT
C C     R(3)      (1)  MINIMUM POSITIVE X FOR  1+X      .NE. 1
C                      AVERAGE AS A RESULT OF COMPLEX ARITHMETIC
C                      OPERATIONS.
C                 (2)  MINIMUM EXPONENT Y FOR  10.0**Y  .NE. 0
C                 (3)  MAXIMUM EXPONENT Z FOR  10.0**Z  IS MAX. VALUE
C                  IF INIT=1 (DATA STATEMENT) THEN  SET AS Z=Y
C                  IF INIT=2 THEN THIS ROUTINE GETS ACTUAL VALUE OF Z.
C                  - SEE NOTE -
C--- HISTORY
C 90. 1.20  CREATED
C     6.27  CHANGE THE ALGORITHM TO GET X AND Y TAKING INTO ACCOUNT THE
C           HIGH ACCURACY CO-PROCESSOR AND GRACEFUL UNDERFLOW.
C 92. 3.21  N=1000 from N=200
C     7. 9  BUG IN X-DEFINITION
C--- NOTE
C THIS PROGRAM WILL GENERATE -UNDERFLOW ERROR- AND -OVERFLOW ERROR-
C  MESSAGES.  ON SOME COMPUTER -OVERFLOW ERROR- MESSAGE MAY BE
C  FATAL ERROR.  IN THAT CASE, PLEASE SET INIT = 1 IN THE DATA
C  SATEMENT FOR SUPPRESSING THE PROCEDURE OF GETTING C(3).
      DIMENSION C(*)
      CHARACTER CH*80
C RESOLUTION OF COMPUTATION
      SAVE INIT,X,Y,Z
      DATA INIT/1/
      IF(INIT.LE.0) THEN
        C(1)=X
        C(2)=Y
        C(3)=Z
        RETURN
      ENDIF
CC TEST SUM(K=1,M) COS((2K-1)*PI/(2M+1)) = 0.5
CC SIMPLE CHECK, X = X + E, IS NOT VALIDE WHEN THE COMPUTER
CC  USE A HIGH ACCURATE CO-PROCESSOR.
      N=500
      PI=ASIN(1.0)*2
      M0=10
      X=0
      DO 1 M=1,M0
      Y=0
      DO 2 K=1,M
    2 Y=Y+COS((2*K-1)*PI/(2*M+1))
      Y=ABS(2*Y-1)
      X=X+Y
    1 CONTINUE
      X=X/M0
      C(1)=X
C EXPONENT FOR MINIMUM POSITIVE VALUE
C  THIS PROCEDURE WILL GENERATE -UNDERFLOW ERROR MESSAGE-
      Y2=1
      N=1000
      DO 3 I=1,N
      Y1=Y2
      Y3=Y1/10
CC FOR GRACEFUL UNDERFLOW
CC EVEN Y2 BECOMES 0 AS OUTPUT, Y2 IS NOT 0 INSIDE
CC COMPUTER WHEN GRACEFUL UNDERFLOW IS APPLIED.
CC SO WE REPLACE THE VALUE OF Y2 BY OUTPUT.
      CH='0'
      WRITE(CH,7) Y3
    7 FORMAT(1P,E12.5)
      Y2=0
      READ(CH,*) Y2
      IF(ABS(10*Y2/Y1-1) .GT. 5*X) GOTO 4
    3 CONTINUE
      I=N+1
    4 Y=1-I
      C(2)=Y
C EXPONENT FOR MAXIMUM POSITIVE VALUE
C THIS PROCEDURE WILL GENERATE -OVERFLOW MESSAGE-
      IF(INIT.LE.1) THEN
        Z=-Y
       ELSE
        Z2=1
        DO 5 I=1,N
        Z1=Z2
        Z2=Z1*10
        IF(ABS(Z2/Z1/10-1) .GT. 5*X) GOTO 6
    5   CONTINUE
        I=N+1
    6   Z=I-1
      ENDIF
      C(3)=Z
C
      INIT=0
      RETURN
      END
      SUBROUTINE QUADA(NDA,AMUA,WA)
C CONSTRUCTION OF QUADRATURE IN THE ATMOSPHERE.
C------ HISTORY
C 1986.10.2  USE QGAUSN FROM STAMNES AND TSAY.
C--- INPUT
C NDA        I      NO. OF QUADRATURE STREAMS IN THE HEMISPHERE OF ATMOS
C--- OUTPUT
C AMUA    R(KNDA)   MUA(I), I=1,N1    MUA(1) > MUA(2) > ...
C WA      R(KNDA)   CORRESPONDING WEIGHTS.
C$ENDI
C--- VARIABLES FOR THE ROUTINE
      DIMENSION AMUA(NDA),WA(NDA)
C SHIFTED GAUSSIAN QUADRATURE ON (0, 1).
      CALL QGAUSN(WA,AMUA,NDA)
C REORDERING.
      DO 3 I=1,NDA/2
      I1=NDA+1-I
      X=AMUA(I)
      AMUA(I)=AMUA(I1)
      AMUA(I1)=X
      X=WA(I)
      WA(I)=WA(I1)
    3 WA(I1)=X
      RETURN
      END
C
C 2002.05 SGLR deleted
C
      SUBROUTINE EQ12(A,B,NI,J,NB1)
C  A(*) = B(*, J)
C--- HISTORY
C 88. 6. 6  REGISTERED BY T. NAKAJIMA
C--- INPUT
C B      R(NB1,*)    2-DIM ARRAY  B.
C NI        I        A(I) = B(I,J),   I=1,NI
C J         I
C--- OUTPUT
C A       R(NI)      1-DIM ARRAY   A=B(*,J)
C$ENDI
      DIMENSION A(NI),B(NB1,J)
      DO 1 I=1,NI
    1 A(I)=B(I,J)
      RETURN
      END
      SUBROUTINE EQ21(A,B,NI,J,NA1)
C  A(*,J) = B(*)
C--- HISTORY
C 88. 6. 6  REGISTERED BY T. NAKAJIMA
C B      R(NI)     SOURCE 1-DIM ARRAY  B.
C NI       I       A(I,J) = B(I),  I=1,NI
C J        I
C MA1      I       SIZE FOR DIMENSION A(NA1,*)
C--- OUTPUT
C A     R(NA1,*)   DESTINATION 2-DIM ARRAY A(*,J)=B
C$ENDI
      DIMENSION A(NA1,J),B(NI)
      DO 1 I=1,NI
    1 A(I,J)=B(I)
      RETURN
      END
      SUBROUTINE EQ32(A,B,NI,NJ,K,NA1,NA2,NA3,NB1,NB2)
C  A(*,*,K)=B(*,*)
C--- HISTORY
C 88. 5. 6   REGISTERED BY T. NAKAJIMA
C--- INPUT
C B      R(NB1,NB2)    SOURCE 2-DIM ARRAY
C NI,NJ,K   I          A(I,J,K)=B(I,J),   I=1,NI;  J=1,NJ
C NA1,NA2,NA3  I       DIM A(NA1,NA2,NA3)
C NB1,NB2      I       DIM B(NB1,NB2)
C--- OUTPUT
C A    R(NA1,NA2,NA3)  DESTINATION 3-DIM ARRAY A(*,*,K)=B(*,*)
C$ENDI
      DIMENSION A(NA1,NA2,NA3),B(NB1,NB2)
      DO 1 J=1,NJ
      DO 1 I=1,NI
    1 A(I,J,K)=B(I,J)
      RETURN
      END
C 2001.04.05 SED : Add Water Leaving Radiance
      SUBROUTINE GRNDO2(INDG,FSOL,U10,BGND,DPT,M,NDA,WMP,NA0,AM0
     & ,CR,CI,R,T,ER,ERT,ET,AMUA,WA
     & ,SBRF,SZA,EZA,AZANG,IUCD,IUCR,IUCSR,DDLW,
     &  R0B,KB,THB,ICANO,INITR,IBRF,XLAND,XOCEAN,NW,ir,RRS,SRS,
     &  IRFS)
C 00.03.15 BY SED: Add Snow brdf data
C      SUBROUTINE GRNDO2(INDG,FSOL,U10,BGND,DPT,M,NDA,WMP,NA0,AM0
C     & ,CR,CI,R,T,ER,ERT,ET,AMUA,WA
C     & ,SBRF,SZA,EZA,AZANG,IUCD,IUCR,IUCSR)
C      SUBROUTINE GRNDO1(INDG,FSOL,U10,BGND,DPT,M,NDA,WMP,NA0,AM0
C     &   ,CR,CI,R,T,ER,ERT,ET,AMUA,WA)
C--- HISTORY
C 89.11. 2
C
C 93. 2.26  BUG BG
C     3. 2  CHENGED CALCULATION OF EMISSIVITY
C     3.24  SUB.OCNRF3 Changed parameter ( add wa )
C     4. 5  Put SAVE
C     4. 5 NSF = 1 for initialization (Terry)
C     5. 4 NSF -> INDG; N1, N0 change name as NDA, NA0
C          IF(NSF.GT.0) -> IF(INDG.EQ.1)
C 95. 6. 2 Generated from GRNDO with CR and CI
C 96. 1.12  EMS(KNA0)
C 97. 3.17 Add ERT By Takashi Nakajima to Thermal.
C 98. 2. 4  Replace EMS(KNA0)->EMS(KNDM)
C 00. 3. 15 Generated from GRNDO1
C           Add CANOPY and Snow BRDF by SED          
C--- INPUT
C INDG       I           1 then Initialization of ocean surface matrix
C                        Otherwise use the calculated matrices.
C FSOL       R           SOLAR IRRADIANCE AT THE SYSTEM TOP.
C U10        R           WIND VELOCITY IN M/S AT 10M HEIGHT
C BGND       R           Plank INTENSITY FROM THE SURFACE
C                        Emitted thermal is calculated as (1-r)*BGND
C                        where r is unidirectional reflectivity.
C                        Note the definition of BGND is different
C                        from that for GRNDL.
C DPT        R           OPTICAL DEPTH AT THE SURFACE.
C M          I           FOURIER ORDER.
C NDA        I           NUMBER OF QUADRATURE POINTS.
C WMP     R(KNDM)        SQRT(WM)
C NA0        I           NUMBER OF SOLAR ANGLES.
C AM0     R(KNA0)        COS(SOLAR ZENITH ANGLE)
C                        Needs all the m-Fourier components
C CR         R           Relative refractive index of the media
C                        About 1.33 for atmosphere to ocean incidence,
C                        and 1/1.33 for ocean to atmosphere incidence.
C CI         R           Relative refractive index for imaginary part
C                        M = CR + I*CI
C--- OUTPUT
C R      R(KNDM,KNDM)    SCALED REFELCTION MATRIX.
C T      R(KNDM,KNDM)    SCALED TRANSMISSION MATRIX.
C ER     R(KNDM,KNA0)    SCALED SOURCE MATRIX FOR UPWARD RADIANCE.
C ET     R(KNDM,KNA0)    SCALED SOURCE MATRIX FOR DOWNWARD RADIANCE.
C
C OCEAN SURFACE
      SAVE ORF,SRR
      PARAMETER (KNDM  =16)
      PARAMETER (KNA0  =2)
      PARAMETER (PI=3.141592654)
      PARAMETER (KNDM2=KNDM*2)
      DIMENSION R(KNDM,KNDM),T(KNDM,KNDM),ER(KNDM,KNA0),ET(KNDM,KNA0)
     &         ,AM0(KNA0),WMP(KNDM),AMUA(KNDM),WA(KNDM)
C98.2.4 Replace EMS(KNA0)->EMS(KNDM)
      DIMENSION ORF(KNDM,KNDM,KNDM2),EMS(KNDM),SR(KNDM)
     &         ,SRR(KNDM,KNA0,KNDM2),ROCN(KNDM,KNDM),RLND(KNDM,KNDM),
     &          SROCN(KNDM),SRLND(KNDM),RRS(10,KNDM,KNDM,KNDM),
     &          SRS(10,KNDM,KNDM)   

      DIMENSION ERT(KNDM,KNA0)
C*****FOR CAPONY BRDF******************************************
      real R0B,KB,THB
      integer ICANO
C*****FOR CAPONY BRDF****************************************** 
C 00.03.15 BY SED: Add Snow BRDF
      DIMENSION SBRF(20,20,40),SZA(20),EZA(20),AZANG(40)
C 2001.04.05 SED Add Water Leaving Radiance
      DIMENSION DDLW(KNA0)
cs      write(6,*)'IN GRNDO2','   INITR=',INITR
C
      M1=M+1
C 2001.04.05 SED
      IF((INDG.EQ.1).OR.(INDG.EQ.5)) THEN
C      IF(INDG.EQ.1) THEN
        CALL OCNR11(M,NDA,AMUA,WA,CR,CI,U10,R,NW,ir,RRS)
        CALL EQ32(ORF,R,NDA,NDA,M1,KNDM,KNDM,KNDM2,KNDM,KNDM)
C 93.3.2
        DO 5 J=1,NA0
        CALL OCNR31(M,NDA,AMUA,WA,AM0(J),CR,CI,U10,SR,NW,ir,SRS)
        DO 5 I=1,NDA
    5   SRR(I,J,M1)=SR(I)
C
      ENDIF
C 00.03.15 BY SED CANOPY BRDF
      IF(INDG.EQ.3) THEN
           IF(ir.LE.NW)THEN
cs           write(6,*)'BEFORE OCNR11'
           CALL OCNR11(M,NDA,AMUA,WA,CR,CI,U10,ROCN,NW,ir,RRS)
cs           write(6,*)'AFTER OCNR11'
           ENDIF
           IF(ir.GT.NW)THEN
           id=ir/NW
           irs=ir-NW*id
           IF(irs.EQ.0)irs=NW
           do i1=1,KNDM
           do i2=1,KNDM
           ROCN(i1,i2)=0.
           ROCN(i1,i2)=RRS(irs,M1,i1,i2)
           enddo
           enddo
           ENDIF
           CALL CANR(M,NDA,AMUA,WA,RLND,IUCD,IUCR,
     &     R0B,KB,THB,ICANO,INITR,IBRF,IRFS)
           do i1=1,KNDM
           do i2=1,KNDM
           R(i1,i2)=XLAND*RLND(i1,i2)+XOCEAN*ROCN(i1,i2)
           enddo
           enddo
           CALL EQ32(ORF,R,NDA,NDA,M1,KNDM,KNDM,KNDM2,KNDM,KNDM)
           DO 55 J=1,NA0
           IF(ir.LE.NW)THEN
cs           write(6,*)'BEFORE OCNR31'
           CALL OCNR31(M,NDA,AMUA,WA,AM0(J),CR,CI,U10,SROCN,NW,ir,SRS)
cs           write(6,*)'AFTER OCNR31'
          ENDIF
          IF(ir.GT.NW)THEN
           do i1=1,KNDM
           SROCN(i1)=0.
           SROCN(i1)=SRS(irs,M1,i1)
           enddo
           ENDIF
           CALL CANSR(M,NDA,AMUA,WA,AM0(J),J,AM0,NA0,SRLND,IUCD,IUCSR,
     &      R0B,KB,THB,ICANO,INITR,IBRF,IRFS)
           do i1=1,KNDM
           SR(i1)=XLAND*SRLND(i1)+XOCEAN*SROCN(i1)
           enddo
           DO 55 I=1,NDA
           SRR(I,J,M1)=SR(I)
 55        CONTINUE
      ENDIF
      IF( INDG.EQ.4) THEN
C 00.03.15 BY SED SNOW BRDF
           CALL SNWR(M,NDA,AMUA,WA,SBRF,SZA,EZA,AZANG,R)
           CALL EQ32(ORF,R,NDA,NDA,M1,KNDM,KNDM,KNDM2,KNDM,KNDM)
           DO 61 J=1,NA0
           CALL SNWSR(M,NDA,AMUA,WA,AM0(J),SBRF,SZA,EZA,AZANG,SR)
           DO 61 I=1,NDA
           SRR(I,J,M1)=SR(I)
 61        CONTINUE
      ENDIF
C
      DO 2 I=1,NDA
      DO 2 J=1,NDA
      T(I,J)=0.0
   2  R(I,J)=WMP(I)*ORF(I,J,M1)/WMP(J)
C 93.3.2
      IF(M.EQ.0)THEN
        BG=2*PI*BGND

C98.2.4 Replace 'DO 3'  by T.Y.Nakajima
C Refer Katagiri's Modification.
C        DO 3 J=1,NA0
C        SUM=0
C        DO 4 I=1,NDA
C   4    SUM=SUM+AMUA(I)*WA(I)*SRR(I,J,1)
C   3    EMS(J)=1-SUM/AM0(J)

         DO 3 I=1,NDA
         SUM=0.
         DO 4 J=1,NDA
    4    SUM=SUM+ORF(I,J,1)
    3    EMS(I)=1-SUM
C
      ELSE
        BG=0
      ENDIF
        DO 1 J=1,NA0
          X=-DPT/AM0(J)
          TRNS0=EXP(X)*FSOL
C 2001.04.05 SED START
          IF(M.EQ.0) THEN
             DDDLW=2.*PI*DDLW(J)
          else
             DDDLW=0.D0
          ENDIF
C SED END
          DO 1 I=1,NDA
          ET(I,J)=0.
C 98.2.4 Replace 'ERT=' by T.Y.Nakajima
C Refer Katagiri's Modification.
C 97.3.17 Add ERT By Takashi Nakajima
C        ERT(I,J)=WMP(I)*(EMS(J)*BG)
C  1     ER(I,J)=WMP(I)*(SRR(I,J,M1)*TRNS0+EMS(J)*BG)
C 2001.04.05 SED : Adding the effect of LW on ER and ERT
C         ERT(I,J)=WMP(I)*(EMS(I)*BG)
C    1    ER(I,J)=WMP(I)*(SRR(I,J,M1)*TRNS0+EMS(I)*BG)
         ERT(I,J)=WMP(I)*(EMS(I)*BG + DDDLW)
    1    ER(I,J)=WMP(I)*(SRR(I,J,M1)*TRNS0+EMS(I)*BG + DDDLW)

      RETURN
      END
      SUBROUTINE GRNDL3(FSOL,GALB,BGND,DPT,M,N1,AM,W,N0,AM0,R,T,ER,ET)
C--- HISTORY
C 95. 5.26 Generated from GRNDL introducing AM and W
C            and R=2*GALB... -> SUMWM*GALB...
C--- INPUT
C FSOL       R           SOLAR IRRADIANCE AT THE SYSTEM TOP.
C GALB       R           FLUX REFLECTIVITY OF THE SURFACE.
C BGND       R           THERMAL INTENSITY FROM THE SURFACE=(1-r)*PLANK.
C DPT        R           OPTICAL DEPTH AT THE SURFACE.
C M          I           FOURIER ORDER.
C N1         I           NUMBER OF QUADRATURE POINTS.
C AM      R(KNDM)        MU(I)
C W       R(KNDM)        W (I)
C N0         I           NUMBER OF SOLAR ANGLES.
C AM0     R(KNA0)        COS(SOLAR ZENITH ANGLE)
C--- OUTPUT
C R      R(KNDM,KNDM)    SCALED REFELCTION MATRIX.
C T      R(KNDM,KNDM)    SCALED TRANSMISSION MATRIX.
C ER     R(KNDM,KNA0)    SCALED SOURCE MATRIX FOR UPWARD RADIANCE.
C ET     R(KNDM,KNA0)    SCALED SOURCE MATRIX FOR DOWNWARD RADIANCE.
C
C LAMBERT SURFACE
      PARAMETER (PI=3.141592654)
      PARAMETER (KNDM  =16)
      PARAMETER (KNA0  =2)
      DIMENSION R(KNDM,KNDM),T(KNDM,KNDM),ER(KNDM,KNA0),ET(KNDM,KNA0)
     & ,AM0(KNA0),AM(KNDM),W(KNDM)
      SUMWM=0
      DO 3 I=1,N1
    3 SUMWM=SUMWM+AM(I)*W(I)
      IF(GALB.LE.0.0 .OR. M.GT.0) THEN
        CALL EQ20( R,0.0,N1,N1,KNDM,KNDM)
        CALL EQ20( T,0.0,N1,N1,KNDM,KNDM)
       ELSE
        DO 2 I=1,N1
        DO 2 J=1,N1
        T (I,J)=0
    2   R (I,J)=GALB*SQRT(AM(I)*W(I)*AM(J)*W(J))/SUMWM
      ENDIF
      IF(M.GT.0) THEN
        CALL EQ20(ER,0.0,N1,N0,KNDM,KNA0)
        CALL EQ20(ET,0.0,N1,N0,KNDM,KNA0)
       ELSE
        DO 1 J=1,N0
          TRNS0=EXP(-DPT/AM0(J))*FSOL
          X=GALB*AM0(J)*TRNS0/SUMWM+2*PI*BGND
          DO 1 I=1,N1
          ET(I,J)=0
    1     ER(I,J)=SQRT(AM(I)*W(I))*X
      ENDIF
      RETURN
      END
      SUBROUTINE PHAS2(M1,MMAX1,N1,N2,KNP,KN1,KN2,PT,PR,G
     & ,PL1,PL2)
C--- HISTORY
C 90. 1.20  GENERATED FROM PHASE (SAME AS PHASE BUT USING THE MOMENTS G)
C--- INPUT
C M1         I       M + 1    FOURIER ORDER + 1.
C MMAX1      I       MMAX+1   MAX FOURIER ORDER + 1.
C N1         I       NO. OF EMERGENT ZENITH ANGLES.
C N2         I       NO. OF INCIDENT ZENITH ANGLES.
C KNP        I       SIZE OF N1 FOR ARRAY PT AND PR.
C KN1        I       SIZE OF N1 FOR ARRAY PL1.
C KN2        I       SIZE OF N2 FOR ARRAY PL2.
C G     R(MMAX1)     LEGENDRE MOMENTS OF PHASE FUNCTION.
C                      G(1)=1
C PL1   R(KN1,MMAX1) LEGENDRE POLYNOMIALS FOR EMERGENT DIRECTION.
C                    (I,M1) = (EMERGENT DIRECTIONS, ORDER+1).
C PL2   R(KN2,MMAX1) LEGENDRE POLYNOMIALS FOR INCIDENT DIRECTION.
C--- OUTPUT
C PT    R(KNP,N2)    PHASE MATRIX FOR TRANSMISSION.
C PR    R(KNP,N2)    PHASE MATRIX FOR REFLECTION.
C$ENDI
C
      PARAMETER (PI=3.141592653)
      DIMENSION PT(KNP,N2),PR(KNP,N2),G(MMAX1)
     & ,PL1(KN1,MMAX1),PL2(KN2,MMAX1)
C
      CALL EQ20(PT,0.0,N1,N2,KNP,N2)
      CALL EQ20(PR,0.0,N1,N2,KNP,N2)
      IF(M1.LE.MMAX1) THEN
        DO 1 J=1,N2
        DO 1 I=1,N1
        SIGN=-1
        DO 1 K1=M1,MMAX1
        SIGN=-SIGN
        C4=(2*K1-1)/2.0*G(K1)*PL1(I,K1)*PL2(J,K1)
        PT(I,J)=PT(I,J)+C4
    1   PR(I,J)=PR(I,J)+SIGN*C4
      ENDIF
      RETURN
      END
      SUBROUTINE PLGND(M1,MMX1,NX,NX0,X,PL)
C NORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS
C------ HISTORY
C 1987.03.04
C------ INPUT VARIABLES
C VARIABLE  TYPE    INTERPRETATION
C M1         I      ORDER OF THE FOURIER SERIES + 1
C MMX1       I      MAX ORDER OF M  + 1
C NX         I      NBR OF X
C NX0        I      DECLARED NBR OF NX
C X       R(NX)     XI, I=1,NX
C------ OUTPUT VARIABLES
C PL      R(NX0,    NORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS
C           MMX1)
C$ENDI
C------ GLOBAL PARAMETER
      PARAMETER (PI=3.141592653)
C------ VARIABLES FOR THE ROUTINE
      DIMENSION X(NX),PL(NX0,MMX1)
C------ NORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS.
C  CC=P(M,L,X)*SQRT((L-M)|/(L+M)|), WHERE X= COS(MU).
      M=M1-1
      DO 15 L1=M1,MMX1
      L=L1-1
      IF(L-(M+1)) 16,17,18
   16 IF(M) 21,21,22
   21 DO 2 I=1,NX
    2 PL(I,L1)=1.0
      GOTO 15
   22 K=2
      ETA=1.0
      DO 1 J=1,M
      EPSI=1.0-1.0/REAL(K)
      ETA=ETA*EPSI
    1 K=K+2
      ETA=SQRT(ETA)
      DO 24 I=1,NX
      IF(X(I).GE.1.0) THEN
      PL(I,L1)=0.0
      ELSE
	IF(ABS(1.0-X(I)**2).LT.1.0E-30)THEN
		EXPCC=(REAL(M)*LOG(1.0E-30))/2.0
	ELSE
      EXPCC=(REAL(M)*LOG(ABS(1.0-X(I)**2)))/2.0
	ENDIF
      IF(EXPCC.LE.-100.0) THEN
      PL(I,L1)=0.0
      ELSE
      PL(I,L1)=ETA*EXP(EXPCC)
      ENDIF
      ENDIF
   24 CONTINUE
      GO TO 15
   17 C0=SQRT(REAL(2*M+1))
      DO 26 I=1,NX
   26 PL(I,L1)=C0*PL(I,L1-1)*X(I)
      C0=SQRT(REAL((L-M)*(L+M)))
      GO TO 15
   18 C1=SQRT(REAL((L-M)*(L+M)))
      C2=REAL(2*L-1)
      DO 27 I=1,NX
   27 PL(I,L1)=(C2*X(I)*PL(I,L1-1)-C0*PL(I,L1-2))/C1
      C0=C1
   15 CONTINUE
      RETURN
      END
      SUBROUTINE TRN1(NSB,NDD,NA0,IUP,IDN,RE,TE,SER,SET,RUP,RDN,ERR)
C SOLVE THE RADIATIVE TRANSFER IN THE MULTI-SUBLAYER SYSTEM
C  BY THE ADDING METHOD.
C--- HISTORY
C 87. 3. 4 FOR INTENSITY CALCULATION.
C 89.10.30 RE-EDIT.
C 96.01.19   EQ22(S2U,ST,N2,NA0,KNDM,KNDM,KNDM,KNDM)
C          ->EQ22(S2U,ST,N2,NA0,KNDM,KNA0,KNDM,KNA0)
C--- INPUT
C NSB       I              NUMBER OF SUBLAYERS.
C NDD    R(NSB1)           NUMBER OF QUADRATURE POINTS AT INTERFACES.
C IUP    I(NSB)            ELEMENT NUMBER OF UPWELLING OPERATORS.
C IDN    I(NSB)            ELEMENT NUMBER OF DOWNGOING OPERATORS.
C NA0       I              NUMBER OF SOLAR ZENITH ANGLES.
C RE    R(KNDM,KNDM,KNLT)  REFLECTION MATRICES OF SUBLAYERS.
C TE    R(KNDM,KNDM,KNLT)  TRANSMISSION MATRICES OF SUBLAYERS.
C SER   R(KNDM,KNA0,KNSB)  UPWELLING SOURCE MATRICES.
C SET   R(KNDM,KNA0,KNSB)  DOWNGOING SOURCE MATRICES.
C--- OUTPUT
C RUP   R(KNDM,KNA0,KNSB1) UPWELLING INTERNAL INTENSITIES.
C RDN   R(KNDM,KNA0,KNSB1) DOWNGOING INTERNAL INTENSITIES.
C ERR      C*64            ERROR INDEX.
C--- PARAMETER
C KNA0      I              NUMBER OF SOLAR ZENITH ANGLES.
C KNDM      I              NUMBER OF QUADRATURE POINTS.
C KNSB      I              NUMBER OF SUBLAYERS.
C KNLT      I              TOTAL NUMBER OF ELEMENTARY OPERATORS
C                           TAKING PORALITY INTO ACCOUNT .GE. KNSB.
C--- AREAS FOR THIS ROUTINE
      PARAMETER (KNA0  =2)
      PARAMETER (KNDM  =16)
      PARAMETER (KNSB  =50)
      PARAMETER (KNLT  =50)
      PARAMETER (PI=3.141592653)
      PARAMETER (KNSB1=KNSB+1)
C--- AREAS FOR THE ROUTINE
      CHARACTER ERR*64
      DIMENSION NDD(KNSB1),IUP(KNSB),IDN(KNSB)
     &, RE(KNDM,KNDM,KNLT),  TE(KNDM,KNDM,KNLT)
     &,SER(KNDM,KNA0,KNSB), SET(KNDM,KNA0,KNSB)
     &,RUP(KNDM,KNA0,KNSB1),RDN(KNDM,KNA0,KNSB1)
C--- WORKING AREAS
      DIMENSION R1D(KNDM,KNDM),R1U(KNDM,KNDM),T1D(KNDM,KNDM)
     &,T1U(KNDM,KNDM),S1D(KNDM,KNA0),S1U(KNDM,KNA0)
     &,R2D(KNDM,KNDM),T2D(KNDM,KNDM),T2U(KNDM,KNDM)
     &,S2D(KNDM,KNA0),S2U(KNDM,KNA0),TU(KNDM,KNDM),RD(KNDM,KNDM)
     &,SD(KNDM,KNA0),SU(KNDM,KNA0),RL(KNDM,KNDM,KNSB),SL(KNDM,KNA0,KNSB)
     &,RT(KNDM,KNDM),ST(KNDM,KNA0)
C--- UPWARD ADDING
      NSB1=NSB+1
      ID=IDN(NSB)
      N2=NDD(NSB)
      N3=NDD(NSB1)
      CALL RP33(RL, RE,N2, N2,NSB, ID,KNDM,KNDM,KNSB,KNDM,KNDM,KNLT)
      CALL RP33(SL,SER,N2,NA0,NSB,NSB,KNDM,KNA0,KNSB,KNDM,KNA0,KNSB)
      IF(NSB.GE.2) THEN
        DO 52 L=NSB-1,1,-1
        LB=L
        L1=L+1
        N1=NDD(L)
        IU=IUP(L)
        ID=IDN(L)
        N2=NDD(L1)
        CALL EQ23(R1D, RE,N1, N1,ID,KNDM,KNDM,KNDM,KNDM,KNLT)
        CALL EQ23(R1U, RE,N2, N2,IU,KNDM,KNDM,KNDM,KNDM,KNLT)
        CALL EQ23(T1D, TE,N2, N1,ID,KNDM,KNDM,KNDM,KNDM,KNLT)
        CALL EQ23(T1U, TE,N1, N2,IU,KNDM,KNDM,KNDM,KNDM,KNLT)
        CALL EQ23(S1D,SET,N2,NA0,LB,KNDM,KNA0,KNDM,KNA0,KNSB)
        CALL EQ23(S1U,SER,N1,NA0,LB,KNDM,KNA0,KNDM,KNA0,KNSB)
        CALL EQ23(R2D, RL,N2, N2,L1,KNDM,KNDM,KNDM,KNDM,KNSB)
        CALL EQ23(S2U, SL,N2,NA0,L1,KNDM,KNA0,KNDM,KNA0,KNSB)
        CALL ADD(0,NA0,N1,N2,N3,R1D,R1U,T1D,T1U,S1D,S1U
     &   ,R2D,T2D,T2U,S2D,S2U,TU,RD,SD,SU,ERR)
        IF(ERR.NE.' ') THEN
          ERR='ERROR IN ADD FOR UPWARD ADDING (TRN1)'
          RETURN
        ENDIF
        CALL EQ32( RL, RD,N1, N1,LB,KNDM,KNDM,KNSB,KNDM,KNDM)
        CALL EQ32( SL, SU,N1,NA0,LB,KNDM,KNA0,KNSB,KNDM,KNA0)
   52   CONTINUE
      ENDIF
C FIELD AT THE TOP OF THE SYSTEM
      N1=NDD(1)
      CALL RP33(RUP, SL,N1,NA0,1,1,KNDM,KNA0,KNSB1,KNDM,KNA0,KNSB)
      CALL RP30(RDN,0.0,N1,NA0,1,  KNDM,KNA0,KNSB1)
C--- DOWNWARD ADDING
      IU=IUP(1)
      N1=NDD(1)
      N2=NDD(2)
      CALL EQ23(RT, RE,N2, N2,IU,KNDM,KNDM,KNDM,KNDM,KNLT)
      CALL EQ23(ST,SET,N2,NA0, 1,KNDM,KNA0,KNDM,KNA0,KNSB)
      IF(NSB.GE.2) THEN
        DO 26 L=2,NSB
        LB=L
        N2=NDD(L)
C INTERNAL FIELD
        CALL EQ23(S2U,SL,N2,NA0,LB,KNDM,KNA0,KNDM,KNA0,KNSB)
        CALL EQ23(R2D,RL,N2, N2,LB,KNDM,KNDM,KNDM,KNDM,KNSB)
        CALL AXB(SD,RT,S2U,N2,N2,NA0,KNDM,KNDM,KNDM)
        CALL AAPB(SD,ST,N2,NA0,KNDM,KNDM)
        CALL AXB(RD,RT,R2D,N2,N2,N2,KNDM,KNDM,KNDM)
        CALL MULTI(N2,RD,R1D,ERR)
        IF(ERR.NE.' ') THEN
          ERR='ERROR IN MULTI FOR INTERNAL FIELD (TRN1)'
          RETURN
        ENDIF
        CALL AXB(S1D,R1D,SD,N2,N2,NA0,KNDM,KNDM,KNDM)
        CALL AXB(S1U,R2D,S1D,N2,N2,NA0,KNDM,KNDM,KNDM)
        CALL AAPB(S1U,S2U,N2,NA0,KNDM,KNDM)
        CALL EQ32(RDN,S1D,N2,NA0,LB,KNDM,KNA0,KNSB1,KNDM,KNA0)
        CALL EQ32(RUP,S1U,N2,NA0,LB,KNDM,KNA0,KNSB1,KNDM,KNA0)
C UPSIDE DOWN DIRECTION FOR APPLICATION OF THE ROUTINE ADD.
        IU=IUP(L)
        ID=IDN(L)
        N3=NDD(L+1)
        CALL EQ23(R1D,RE,N3,N3,IU,KNDM,KNDM,KNDM,KNDM,KNLT)
        CALL EQ23(R1U,RE,N2,N2,ID,KNDM,KNDM,KNDM,KNDM,KNLT)
        CALL EQ23(T1D,TE,N2,N3,IU,KNDM,KNDM,KNDM,KNDM,KNLT)
        CALL EQ23(T1U,TE,N3,N2,ID,KNDM,KNDM,KNDM,KNDM,KNLT)
        CALL EQ22(R2D,RT,N2,N2,KNDM,KNDM,KNDM,KNDM)
        CALL EQ23(S1D,SER,N2,NA0,LB,KNDM,KNA0,KNDM,KNA0,KNSB)
        CALL EQ23(S1U,SET,N3,NA0,LB,KNDM,KNA0,KNDM,KNA0,KNSB)
C Bug9601       CALL EQ22(S2U,ST,N2,NA0,KNDM,KNDM,KNDM,KNDM)
        CALL EQ22(S2U,ST,N2,NA0,KNDM,KNA0,KNDM,KNA0)
        CALL ADD(0,NA0,N3,N2,N1,R1D,R1U,T1D,T1U,S1D,S1U
     &  ,R2D,T2D,T2U,S2D,S2U,TU,RT,SD,ST,ERR)
        IF(ERR.NE.' ') THEN
          ERR='ERROR IN ADD FOR DOWNWARD ADDING (TRN1)'
          RETURN
        ENDIF
   26   CONTINUE
      ENDIF
C FIELD AT THE BOTTOM OF THE SYSTEM.
      CALL EQ32(RDN,ST,N3,NA0,NSB1,KNDM,KNA0,KNSB1,KNDM,KNA0)
      CALL RP30(RUP,0.0,N3,NA0,NSB1,KNDM,KNA0,KNSB1)
C--- DOWNWARD ADDING END
      RETURN
      END
      FUNCTION CSPLI(X1,N,X,A,B,C,D)
C GET INTERPOLATED VALUE USING CUBIC SPLINE (PAIR WITH CSPL1)
C--- HISTORY
C 88. 6. 6  REGISTERED BY T. NAKAJIMA
C--- INPUT
C X1      R     INTERPOLATION POINT
C N       I     NO. OF INTERVALS + 1
C X     R(N)    DIVISION POINTS OF THE INTERVALS.
C A, B, C, D
C       R(N)    Y=A+X*(B+X*(C+X*D)))
C--- OUTPUT
C CSPLI  RF     INTERPOLATED VALUE
C$ENDI
      DIMENSION X(N),A(N),B(N),C(N),D(N)
      DO 7 J=1,N
      IF(X1.LE.X(J)) GOTO 8
    7 CONTINUE
      J=N
    8 CSPLI=A(J)+X1*(B(J)+X1*(C(J)+X1*D(J)))
      RETURN
      END
      SUBROUTINE CSPL1(N,X,Y,A,B,C,D)
C GETTING COEFFICIENTS OF NATURAL CUBIC SPLINE FITTING
C USE A LINEAR INTERPOLATION OUTSIDE THE MEANINGFUL RANGE OF X
C X-increasing order
C--- HISTORY
C 88. 1. 4  CREATED
C--- INPUT
C N      I     NBR OF DATA
C X    R(N)    INDEPENDENT VARIABLE DATA
C              X-increasing order
C Y    R(N)      DEPENDENT VARIABLE DATA
C--- OUTPUT
C A    R(N)    LAMBDA -> A   WHERE  Y=A+X*(B+X*(C+D*X))
C B    R(N)    D      -> B   I-TH FOR RANGE (X(I-1), X(I))
C C    R(N)    M         C   (A,B,C,D FOR I=1 ARE SAME AS THOSE FOR I=2)
C D    R(N)    M         D
C
C--- NOTES
C REF-1   P. F. DAVIS AND PHILIP RABINOWITZ (1984)
C         METHODS PF NUMERICAL INTEGRATION, SECOND EDITION
C         ACADEMIC PRESS, INC., PP612.
C$ENDI
C
      DIMENSION X(N),Y(N),A(N),B(N),C(N),D(N)
C
      GOTO (1,2,3,4), N
C N > 4
      A(1)=0.0
      A(N)=1.0
      B(1)=0.0
      B(N)=0.0
      H2=X(2)-X(1)
      S2=(Y(2)-Y(1))/H2
      DO 5 I=2,N-1
      H1=H2
      H2=X(I+1)-X(I)
      S1=S2
      S2=(Y(I+1)-Y(I))/H2
      A(I)=1.0/(1.0+H1/H2)
    5 B(I)=6.0*(S2-S1)/(H2+H1)
CC
      Q2=0.0
      U2=0.0
      DO 6 I=1,N
      Q1=Q2
      U1=U2
      P2=(1.0-A(I))*Q1+2.0
      Q2=-A(I)/P2
      U2=(B(I)-(1.0-A(I))*U1)/P2
      A(I)=Q2
    6 B(I)=U2
      C(N)=B(N)
      DO 7 I=N-1,1,-1
    7 C(I)=A(I)*C(I+1)+B(I)
CC
      X2=X(1)
      C2=C(1)/6.0
      DO 8 I=2,N
      H2=X(I)-X(I-1)
      X1=X2
      X2=X(I)
      C1=C2
      C2=C(I)/6.0
      P1=Y(I-1)/H2-C1*H2
      P2=Y(I  )/H2-C2*H2
      A(I)=    ( C1*X2**3-C2*X1**3)/H2+P1*X2-P2*X1
      B(I)=3.0*(-C1*X2**2+C2*X1**2)/H2-P1   +P2
      C(I)=3.0*( C1*X2   -C2*X1   )/H2
    8 D(I)=    (-C1      +C2      )/H2
      GOTO 11
C N=1
    1 A(1)=Y(1)
      B(1)=0.0
      C(1)=0.0
      D(1)=0.0
      RETURN
C N=2
    2 X1=X(1)
      X2=X(2)
      Z1=Y(1)/(X1-X2)
      Z2=Y(2)/(X2-X1)
      A(2)=-X2*Z1-X1*Z2
      B(2)=Z1+Z2
      C(2)=0.0
      D(2)=0.0
      GOTO 11
C N=3
    3 X1=X(1)
      X2=X(2)
      X3=X(3)
      Z1=Y(1)/(X1-X2)/(X1-X3)
      Z2=Y(2)/(X2-X3)/(X2-X1)
      Z3=Y(3)/(X3-X1)/(X3-X2)
      A(2)=X2*X3*Z1+X3*X1*Z2+X1*X2*Z3
      B(2)=-(X2+X3)*Z1-(X3+X1)*Z2-(X1+X2)*Z3
      C(2)=Z1+Z2+Z3
      D(2)=0.0
      A(3)=A(2)
      B(3)=B(2)
      C(3)=C(2)
      D(3)=D(2)
      GOTO 11
C N=4
    4 X1=X(1)
      X2=X(2)
      X3=X(3)
      X4=X(4)
      Z1=Y(1)/(X1-X2)/(X1-X3)/(X1-X4)
      Z2=Y(2)/(X2-X3)/(X2-X4)/(X2-X1)
      Z3=Y(3)/(X3-X4)/(X3-X1)/(X3-X2)
      Z4=Y(4)/(X4-X1)/(X4-X2)/(X4-X3)
      A(2)=-X2*X3*X4*Z1-X3*X4*X1*Z2-X4*X1*X2*Z3-X1*X2*X3*Z4
      B(2)=(X2*X3+X3*X4+X4*X2)*Z1+(X3*X4+X4*X1+X1*X3)*Z2
     &    +(X4*X1+X1*X2+X2*X4)*Z3+(X1*X2+X2*X3+X3*X1)*Z4
      C(2)=-(X2+X3+X4)*Z1-(X3+X4+X1)*Z2-(X4+X1+X2)*Z3-(X1+X2+X3)*Z4
      D(2)=Z1+Z2+Z3+Z4
      DO 10 I=3,4
      A(I)=A(2)
      B(I)=B(2)
      C(I)=C(2)
   10 D(I)=D(2)
C
   11 A(1)=A(2)
      B(1)=B(2)
      C(1)=C(2)
      D(1)=D(2)
      RETURN
      END
      SUBROUTINE AXB(C,A,B,NI,NK,NJ,NCI,NAI,NBI)
C  C = A*B
C--- HISTORY
C 88. 6. 6   REGISTERED BY T. NAKAJIMA
C--- INPUT
C A      R(NAI,*)     2-DIM ARRAY  A.
C B      R(NBI,*)     2-DIM ARRAY  B.
C NI, NK, NJ  I       C(I,J) = A(I,K)*B(K,J)
C                     I=1,NI; K=1,NK; J=1,NJ
C NCI      I          SIZE FOR C(NCI,*)
C NAI      I          SIZE FOR A(NAI,*)
C NBI      I          SIZE FOR B(NBI,*)
C--- OUTPUT
C C      R(NCI,*)     A*B.
C$ENDI
      DIMENSION A(NAI,NK),B(NBI,NJ),C(NCI,NJ)
      DO 1 I=1,NI
      DO 1 J=1,NJ
      S=0.0
      DO 2 K=1,NK
    2 S=S+A(I,K)*B(K,J)
    1 C(I,J)=S
      RETURN
      END
      SUBROUTINE    TNVSS2(N,A,DT,E,NN,IW,ERR)
C     INVERSION OF REAL MATRIX. SWEEP OUT, COMPLETE POSITIONING.
C--- HISTORY
C 71. 4.30 CREATED BY SAKATA MASATO
C 89.11.10 ADDED ERR
C 90. 1. 6 ERR*(*)
C--- INPUT
C N       I        DIMENSION OF THE MATRIX
C A     R(NN,N)    MATRIX
C NN      I        SIZE OF FIRST ARGUMENT
C E       R        CONVERGENCE CRITERION (0 IS OK)
C--- OUTPUT
C DT      R        DETERMINATION OF THE MATRIX
C IW    I(2*N)     WORKING AREA
C ERR    C*64      ERROR INDICATER. IF ' ' then no error.
C$ENDI
      CHARACTER ERR*(*)
      DIMENSION     A(NN,N)  ,IW(*)
      ERR=' '
      IF(N-1)    910,930,101
  101 IF(N.GT.NN)    GO TO  900
      EPS=0.0
      DT=1.0
      DO  100     K=1,N
      PIV=0.0
      DO  110       I=K,N
      DO  110       J=K,N
      IF(ABS(A(I,J)).LE.ABS(PIV))   GO TO  110
      IPIV=I
      JPIV=J
      PIV=A(I,J)
  110 CONTINUE
      DT=DT*PIV
      IF(ABS(PIV).LE.EPS)  GO TO 920
      IF(K.EQ.1)   EPS=ABS(PIV)*E
      IF(IPIV.EQ.K)      GO TO 130
      DT=-DT
      DO 120   J=1,N
      WORK=A(IPIV,J)
      A(IPIV,J)=A(K,J)
  120 A(K,J)=WORK
  130 IF(JPIV.EQ.K)      GO TO  150
      DT=-DT
      DO 140   I=1,N
      WORK=A(I,JPIV)
      A(I,JPIV)=A(I,K)
  140 A(I,K)=WORK
  150 IW(2*K-1)=IPIV
      AA=1.0/PIV
      IW(2*K)=JPIV
      DO 210   J=1,N
  210 A(K,J)=A(K,J)*AA
      DO 220  I=1,N
      IF(I.EQ.K)    GO TO  220
      AZ=A(I,K)
      IF(AZ.EQ.0.0)   GO TO  220
      DO 230   J=1,N
  230 A(I,J)=A(I,J)-A(K,J)*AZ
      A(I,K)=-AA*AZ
  220 CONTINUE
  100 A(K,K)=AA
      DO  400 KK=2,N
      K=N+1-KK
      IJ=IW(2*K)
      IF(IJ.EQ.K)   GO TO  420
      DO 410   J=1,N
      WORK=A(IJ,J)
      A(IJ,J)=A(K,J)
  410 A(K,J)=WORK
  420 IJ=IW(2*K-1)
      IF(IJ.EQ.K)   GO TO  400
      DO 430   I=1,N
      WORK=A(I,IJ)
      A(I,IJ)=A(I,K)
  430 A(I,K)=WORK
  400 CONTINUE
      RETURN
  910 ERR='ERROR IN TINVSS: N.LE.0'
      RETURN
  900 ERR='ERROR IN TINVSS: N.GT.NN'
      RETURN
  920 DT=0.0
      INDER=N-K+1
      NNN=K-1
      WRITE(ERR,1) NNN
    1 FORMAT('TINVSS: ILL CONDITIONED MATRIX WITH RANK ',I5)
      RETURN
  930 DT=A(1,1)
      K=1
      IF(DT.EQ.0.0)     GO TO  920
      A(1,1)=1.0/A(1,1)
      RETURN
      END
      SUBROUTINE SYMTRX(ROWIJ,M,ROOT,EIGV,NI,WK,IER)
C SOLVES EIGENFUNCTION PROBLEM FOR SYMMETRIC MATRIX
C--- HISTORY
C 89.12. 4 MODIFIED WITH CNCPU
C 90. 1. 6 CNCPU IS REPLACED BY PREC.
C--- INPUT
C M       I      ORDER OF ORIGINAL SYMMETRIC MATRIX
C NI      I      INITIAL DIMENSION OF -ROOT-, -EIGV- AND -WK-
C ROWIJ  R(*)    SYMMETRIC STORAGE MODE OF ORDER M*(M+1)/2
C--- OUTPUT
C EIGV   R(NI,M) EIGENVECTORS OF ORIGINAL SYMMETRIC MATRIX
C IER     I      INDEX FOR ROOT(J) FAILED TO CONVERGE (J=IER-128)
C ROOT   R(M)    EIGENVALUES OF ORIGINAL SYMMETRIC MATRIX
C ROWIJ          STORAGE OF HOUSEHOLDER REDUCTION ELEMENTS
C WK             WORK AREA
C$ENDI
      REAL ROWIJ(*),ROOT(*),WK(*),EIGV(NI,*),CCP(3)
C+++ ADD EPSCP
C     DATA  RDELP/ 1.1921E-07 /
      CALL CPCON(CCP)
      RDELP=CCP(1)*10
C+++
      IER = 0
      MP1 = M + 1
      MM = (M*MP1)/2 - 1
      MBEG = MM + 1- M
C
C+---------------------------------------------------------------------+
C|          LOOP-100 REDUCE -ROWIJ- (SYMMETRIC STORAGE MODE) TO A      |
C|          SYMMETRIC TRIDIAGONAL FORM BY HOUSEHOLDER METHOD           |
C|                      CF. WILKINSON, J.H., 1968,                     |
C|              THE ALGEBRAIC EIGENVALUE PROBLEM, PP 290-293.          |
C|          LOOP-30&40 AND 50 FORM ELEMENT OF A*U AND ELEMENT P        |
C+---------------------------------------------------------------------+
      DO 100 II=1,M
      I = MP1 - II
      L = I - 1
      H = 0.0
      SCALE = 0.0
      IF (L.LT.1) THEN
C|          SCALE ROW (ALGOL TOL THEN NOT NEEDED)
      WK(I) = 0.0
      GO TO 90
      END IF
      MK = MM
      DO 10 K=1,L
      SCALE = SCALE + ABS(ROWIJ(MK))
      MK = MK - 1
   10 CONTINUE
      IF (SCALE.LE.0.0) THEN
      WK(I) = 0.0
      GO TO 90
      END IF
C
      MK = MM
      DO 20 K = 1,L
      ROWIJ(MK) = ROWIJ(MK)/SCALE
      H = H + ROWIJ(MK)*ROWIJ(MK)
      MK = MK - 1
   20 CONTINUE
      WK(I) = SCALE*SCALE*H
      F = ROWIJ(MM)
      G = - SIGN(SQRT(H),F)
      WK(I) = SCALE*G
      H = H - F*G
      ROWIJ(MM) = F - G
      IF (L.GT.1) THEN
      F = 0.0
      JK1 = 1
      DO 50 J=1,L
      G = 0.0
      IK = MBEG + 1
      JK = JK1
      DO 30 K=1,J
      G = G + ROWIJ(JK)*ROWIJ(IK)
      JK = JK + 1
      IK = IK + 1
   30 CONTINUE
      JP1 = J + 1
      IF (L.GE.JP1) THEN
      JK = JK + J - 1
      DO 40 K=JP1,L
      G = G + ROWIJ(JK)*ROWIJ(IK)
      JK = JK + K
      IK = IK + 1
   40 CONTINUE
      END IF
      WK(J) = G/H
      F = F + WK(J)*ROWIJ(MBEG+J)
      JK1 = JK1 + J
   50 CONTINUE
      HH = F/(H+H)
C
      JK = 1
      DO 70 J=1,L
      F = ROWIJ(MBEG+J)
      G = WK(J) - HH*F
      WK(J) = G
      DO 60 K=1,J
      ROWIJ(JK) = ROWIJ(JK) - F*WK(K) - G*ROWIJ(MBEG+K)
      JK = JK + 1
   60 CONTINUE
   70 CONTINUE
      END IF
C
      DO 80 K=1,L
      ROWIJ(MBEG+K) = SCALE*ROWIJ(MBEG+K)
   80 CONTINUE
   90 ROOT(I) = ROWIJ(MBEG+I)
      ROWIJ(MBEG+I) = H*SCALE*SCALE
      MBEG = MBEG - I + 1
      MM = MM - I
  100 CONTINUE
C
C+---------------------------------------------------------------------+
C|          LOOP-210 COMPUTE EIGENVALUES AND EIGENVECTORS              |
C|          SETUP WORK AREA LOCATION EIGV TO THE IDENTITY MATRIX       |
C|          LOOP-140 FOR FINDING SMALL SUB-DIAGONAL ELEMENT            |
C|          LOOP-160 FOR CONVERGENCE OF EIGENVALUE J (MAX. 30 TIMES)   |
C|          LOOP-190 FOR QL TRANSFORMATION AND LOOP-180 FORM VECTORS   |
C+---------------------------------------------------------------------+
      DO 110 I=1,M-1
  110 WK(I) = WK(I+1)
      WK(M) = 0.0
      B = 0.0
      F = 0.0
      DO 130 I=1,M
      DO 120 J=1,M
  120 EIGV(I,J) = 0.0
      EIGV(I,I) = 1.0
  130 CONTINUE
C
      DO 210 L=1,M
      J = 0
      H = RDELP*(ABS(ROOT(L))+ABS(WK(L)))
      IF (B.LT.H) B = H
      DO 140 N=L,M
      K = N
      IF (ABS(WK(K)).LE.B) GO TO 150
  140 CONTINUE
  150 N = K
      IF (N.EQ.L) GO TO 200
C
  160 CONTINUE
      IF (J.EQ.30) THEN
      IER = 128 + L
      RETURN
      END IF
C
      J = J + 1
      L1 = L + 1
      G = ROOT(L)
      P = (ROOT(L1)-G)/(WK(L)+WK(L))
      R = ABS(P)
      IF (RDELP*ABS(P).LT.1.0) R = SQRT(P*P+1.0)
      ROOT(L) = WK(L)/(P+SIGN(R,P))
      H = G - ROOT(L)
      DO 170 I=L1,M
      ROOT(I) = ROOT(I) - H
  170 CONTINUE
      F = F + H
C
      P = ROOT(N)
      C = 1.0
      S = 0.0
      NN1 = N - 1
      NN1PL = NN1 + L
      IF (L.LE.NN1) THEN
      DO 190 II=L,NN1
      I = NN1PL - II
      G = C*WK(I)
      H = C*P
      IF (ABS(P).LT.ABS(WK(I))) THEN
      C = P/WK(I)
      R = SQRT(C*C+1.0)
      WK(I+1) = S*WK(I)*R
      S = 1.0/R
      C = C*S
      ELSE
      C = WK(I)/P
      R = SQRT(C*C+1.0)
      WK(I+1) = S*P*R
      S = C/R
      C = 1.0/R
      END IF
      P = C*ROOT(I) - S*G
      ROOT(I+1) = H + S*(C*G+S*ROOT(I))
      IF (NI.GE.M) THEN
      DO 180 K=1,M
      H = EIGV(K,I+1)
      EIGV(K,I+1) = S*EIGV(K,I) + C*H
      EIGV(K,I) = C*EIGV(K,I) - S*H
  180 CONTINUE
      END IF
  190 CONTINUE
      END IF
      WK(L) = S*P
      ROOT(L) = C*P
      IF (ABS(WK(L)).GT.B) GO TO 160
  200 ROOT(L) = ROOT(L) + F
  210 CONTINUE
C
C+---------------------------------------------------------------------+
C|          BACK TRANSFORM EIGENVECTORS OF THE ORIGINAL MATRIX FROM    |
C|          EIGENVECTORS 1 TO M OF THE SYMMETRIC TRIDIAGONAL MATRIX    |
C+---------------------------------------------------------------------+
      DO 250 I=2,M
      L = I - 1
      IA = (I*L)/2
      IF (ABS(ROWIJ(IA+I)).GT.0.0) THEN
      DO 240 J=1,M
      SUM = 0.0
      DO 220 K=1,L
      SUM = SUM + ROWIJ(IA+K)*EIGV(K,J)
  220 CONTINUE
      SUM = SUM/ROWIJ(IA+I)
      DO 230 K=1,L
      EIGV(K,J) = EIGV(K,J) - SUM*ROWIJ(IA+K)
  230 CONTINUE
  240 CONTINUE
      END IF
  250 CONTINUE
C
      RETURN
      END
      subroutine SMLLOP(X,CR,CI,QEXT,QSCA)
C Optical cross sections for small particles (X<0.1)
C In this approximation
C     Q(ANG,1)=3.0/8.0/PI*QSCA
C     Q(ANG,2)=Q(ANG,1)*COS(ANG)**2
C     Q(ANG,3)=Q(ANG,1)*COS(ANG)
C     Q(ANG,4)=0
C--- history
C 95. 9.19 Created
C--- input
C X       R       Size parameter (<1.5))
C CR      R       M= CR - i CI
C CI      R       CI>0
C--- output
C QEXT    R       Extinction efficiency factor
C QSCA    R       Scattering efficiency factor
C---
      A=(CR**2+CI**2)**2
      B=CR**2-CI**2
      G=CR*CI
      Z1=A+4*B+4
      Z2=4*A+12*B+9
      E1=24*G/Z1
      E3=G*(4.0/15.0+20.0/3.0/Z2+24.0/5.0/Z1**2*(7*A+4*B-20))
      E4=8.0/3.0/Z1**2*((A+B-2)**2-36*G**2)
      S4=8.0/3.0/Z1**2*((A+B-2)**2+36*G**2)
      QEXT=E1*X+E3*X**3+E4*X**4
      QSCA=S4*X**4
      return
      end
      SUBROUTINE  QGAUSN( GWT, GMU, M )
C  COMPUTE WEIGHTS AND ABSCISSAE FOR ORDINARY GAUSSIAN QUADRATURE
C   (NO WEIGHT FUNCTION INSIDE INTEGRAL) ON THE INTERVAL (0,1)
C--- HISTORY
C 90. 1.17  REGISTERED
C--- INPUT
C M        I       ORDER OF QUADRATURE RULE
C--- OUTPUT
C GMU    R(M)      ARRAY OF ABSCISSAE (0, 1)
C GWT    R(M)      ARRAY OF WEIGHTS   SUM=1
C--- NOTES
C REFERENCE:  DAVIS,P.J. AND P. RABINOWITZ, METHODS OF NUMERICAL
C             INTEGRATION,ACADEMIC PRESS, NEW YORK, 1975, PP. 87
C METHOD:     COMPUTE THE ABSCISSAE AS ROOTS OF THE LEGENDRE
C             POLYNOMIAL P-SUB-N USING A CUBICALLY CONVERGENT
C             REFINEMENT OF NEWTON'S METHOD.  COMPUTE THE
C             WEIGHTS FROM EQ. 2.7.3.8 OF DAVIS/RABINOWITZ.
C             ACCURACY:  AT LEAST 13 SIGNIFICANT DIGITS.
C--- INTERNAL VARIABLES
C PM2,PM1,P : 3 SUCCESSIVE LEGENDRE POLYNOMIALS
C PPR       : DERIVATIVE OF LEGENDRE POLYNOMIAL
C P2PRI     : 2ND DERIVATIVE OF LEGENDRE POLYNOMIAL
C TOL       : CONVERGENCE CRITERION
C X,XI      : SUCCESSIVE ITERATES IN CUBICALLY-
C             CONVERGENT VERSION OF NEWTON'S METHOD
C            ( SEEKING ROOTS OF LEGENDRE POLYNOMIAL )
C$ENDI
      REAL     CONA, GMU( M ), GWT( M ), PI, T
      INTEGER  LIM, M, NP1
      DOUBLE   PRECISION  EN, NNP1, P, PM1, PM2, PPR, P2PRI, PROD,
     &                    TMP, TOL, X, XI
      DATA     TOL / 1.0D-13 /
      DATA     PI  / 3.1415926535898 /
C
      IF ( M.LE.1 )  THEN
         M = 1
         GMU( 1 ) = 0.5
         GWT( 1 ) = 1.0
         RETURN
      END IF
C
      EN   = M
      NP1  = M + 1
      NNP1 = M * NP1
      CONA = REAL( M-1 ) / ( 8 * M**3 )
C+---------------------------------------------------------------------+
C|         INITIAL GUESS FOR K-TH ROOT OF LEGENDRE POLYNOMIAL,         |
C|         FROM DAVIS/RABINOWITZ  EQ. (2.7.3.3A)                       |
C+---------------------------------------------------------------------+
      LIM  = M / 2
      DO 30  K = 1, LIM
         T = ( 4*K - 1 ) * PI / ( 4*M + 2 )
         X = COS ( T + CONA / TAN( T ) )
C
C+---------------------------------------------------------------------+
C|             RECURSION RELATION FOR LEGENDRE POLYNOMIALS             |
C|       INITIALIZE LEGENDRE POLYNOMIALS: (PM2) P-SUB-0, (PM1) P-SUB-1 |
C+---------------------------------------------------------------------+
 10       PM2 = 1.D0
         PM1 = X
         DO 20 NN = 2, M
            P   = ( ( 2*NN - 1 ) * X * PM1 - ( NN-1 ) * PM2 ) / NN
            PM2 = PM1
            PM1 = P
 20       CONTINUE
C
         TMP   = 1.D0 / ( 1.D0 - X**2 )
         PPR   = EN * ( PM2 - X * P ) * TMP
         P2PRI = ( 2.D0 * X * PPR - NNP1 * P ) * TMP
         XI    = X - ( P / PPR ) * ( 1.D0 +
     &               ( P / PPR ) * P2PRI / ( 2.D0 * PPR ) )
C
         IF ( DABS(XI-X) .GT. TOL ) THEN
C|          CHECK FOR CONVERGENCE
            X = XI
            GO TO 10
         END IF
C
C       ** ITERATION FINISHED--CALC. WEIGHTS, ABSCISSAE FOR (-1,1)
         GMU( K ) = - X
         GWT( K ) = 2.D0 / ( TMP * ( EN * PM2 )**2 )
         GMU( NP1 - K ) = - GMU( K )
         GWT( NP1 - K ) =   GWT( K )
 30    CONTINUE
C
      IF ( MOD( M,2 ) .NE. 0 )  THEN
C|       SET MIDDLE ABSCISSA AND WEIGHT FOR RULES OF ODD ORDER
         GMU( LIM + 1 ) = 0.0
         PROD = 1.D0
         DO 40 K = 3, M, 2
            PROD = PROD * K / ( K-1 )
 40       CONTINUE
         GWT( LIM + 1 ) = 2.D0 / PROD**2
      END IF
C
      DO 50  K = 1, M
C|       CONVERT FROM (-1,1) TO (0,1)
         GMU( K ) = 0.5 * GMU( K ) + 0.5
         GWT( K ) = 0.5 * GWT( K )
 50    CONTINUE
C
      RETURN
      END
      FUNCTION ERFC(X)
C Complementary error function=(2/sqrt(pi) integral(x,inf) exp(-t**2)dt
C 92.12.22: Created BY NAKAJIMA
C--- INPUT
C X       R      Independent variable 0 to inf
C--- OUTPUT
C ERFC   RF      Complementary error function
C
      PARAMETER (C1 = 7.05230784E-2, C2 = 4.22820123E-2)
      PARAMETER (C3 = 9.2705272E-3,  C4 = 1.520143E-4)
      PARAMETER (C5 = 2.765672E-4,   C6 = 4.30638 E-5)
      V=ABS(X)
      IF(V.LE.7) THEN
        ERFC=1.0/(1+V*(C1+V*(C2+V*(C3+V*(C4+V*(C5+C6*V))))))**16
       ELSE
        ERFC=0
      ENDIF
      IF(X.LT.0) ERFC=2-ERFC
      RETURN
      END
      SUBROUTINE FRNLR(CR,CI,WI,WR,R1,R2)
C FRESNEL REFLECTION COEFFICIENTS
C--- HISTORY
C 88. 6.16  CREATED
C--- INPUT
C CR      R      REAL PART OF THE COMPLEX REFRACTION INDEX
C CI      R      IMAGINARY PART M = CR + I*CI
C WI      R      INCIDENT ANGLE IN DEGREES
C--- OUTPUT
C WR      R      REFRACTION ANGLE IN DEGREES
C                IF TOTAL REFLECTION, THEN WR=999
C R1      R      REFLECTIVITY FOR POLARIZATION
C R2      R      REFLECTIVITY FOR POLARIZATION
C--- REFERENCE
C K.-N. LIOU
C$ENDI
      PARAMETER (PI=3.141592653, RD=PI/180.0)
      CI1=-CI
      C=COS(WI*RD)
      S=SIN(WI*RD)
      WR=S/CR
      IF(WR.GT.1.0) THEN
      WR=999
      ELSE
      WR=ASIN(WR)/RD
      ENDIF
      EN1=CR**2-CI1**2
      EN2=2*CR*CI1
      EN3=EN1-S**2
      U=SQRT(ABS(EN3+SQRT(EN3**2+EN2**2))/2)
      V=SQRT(ABS(U**2-EN3))
      U2=U**2+V**2
      R1=1/(U*C/(U2+C**2)+0.5)-1
      R2=1/((EN1*U+EN2*V)*C/((EN1**2+EN2**2)*C**2+U2)+0.5)-1
      RETURN
      END
      SUBROUTINE OCNR11(M,NDA,AMUA,WA,CR,CI,U10,R,NW,ir,RRS)
C REFLECTION MATRIX OF OCEAN SURFACE
C--- HISTORY
C 92. 9. 1 CREATED BY HASUMI
C    12.23 MODIFIED BY NAKAJIMA
C 95. 6. 2 Generated from OCNRF1
C--- INPUT
C M      I     FOURIER ORDER
C KNDM   I     DECLARED DIMENSION OF R AND AMUA
C NDA    I     USED DIMENSION OF R AND AMUA
C AMUA  R(NDA) QUADRATURE POINTS IN HEMISPHERE
C              DECREASING ORDER (ZENITH TO HORIZON, OR, 1 -> 0)
C WA    R(NDA) QUADRATURE WEIGHTS
C EM     R     RELATIVE REFRACTIVE INDEX
C              ABOUT 1.33 FROM ATMOSPHERE TO OCEAN
C              ABOUT 1/1.33 FROM OCEAN TO ATMOSPHERE
C U10    R     WIND VELOCITY AT 10 M ABOVE THE SURFACE
C CR       R    Relative refractive index of the media
C               About 1.33 for atmosphere to ocean incidence,
C               and 1/1.33 for ocean to atmosphere incidence.
C CI       R    Relative refractive index for imaginary part
C               M = CR + I*CI
C--- OUTPUT
C R    R(KNDA,NDA)  REFLECTION MATRIX FOR M-TH FOURIER ORDER
C                    ur = R * ui
C$END
      SAVE
      PARAMETER (KNDM  =16)
      PARAMETER(PI=3.141592654)
      DIMENSION AMUA(KNDM),WA(KNDM),R(KNDM, KNDM)
C LOCAL VARIABLES
      PARAMETER (KN=30,KNDM1=KNDM+1)
      DIMENSION X(KNDM1),GX(KN),GW(KN),FI1(KN),FI2(KN),COSM1(KN)
     & ,COSM2(KN),XB(5,2),NS(2),RR(KNDM, KNDM),
     & RRS(10,KNDM,KNDM,KNDM)
      DATA INIT/1/
C
cs      write(6,*)'IN OCNR11'
      IF(INIT.GT.0) THEN
        N=KN
        INIT=0
        CALL QGAUSN(GW, GX, N )
      ENDIF
C Parameters for integration
      SIGMA=SQRT(0.00534*U10)
      DISPA=ATAN(SIGMA)
      COSDS=COS(DISPA)
      DO 4 K=1,N
      DFI2=PI-DISPA
      FI1(K)=DISPA+DFI2*GX(K)
      FI2(K)=DISPA*GX(K)
      COSM1(K)=2*COS(FI1(K)*M)*DFI2 *GW(K)
    4 COSM2(K)=2*COS(FI2(K)*M)*DISPA*GW(K)
      X(1)=1
      IF(NDA.GE.2) THEN
        DO 1 I = 2, NDA
    1   X(I)=(AMUA(I-1)+AMUA(I))/2
      ENDIF
      X(NDA+1)=0
C
      DO 2 I = 1, NDA
CC X1=COS(THETA(I)-DISPA), X2=COS(THETA(I)+DISPA)
CC   WHERE AMUA(I)=COS(THETA(I))
      B=SQRT((1-AMUA(I)**2)*(1-COSDS**2))
      X1=AMUA(I)*COSDS+B
      X2=AMUA(I)*COSDS-B
CC SETING MU-BAOUDARY FOR MU-INEGRATION
      NS1=1
      XB(1,1)=X(I)
      IF(X1.LT.X(I)) THEN
        NS1=NS1+1
        XB(NS1,1)=X1
      ENDIF
      NS1=NS1+1
      XB(NS1,1)=AMUA(I)
      IF(X2.GT.X(I+1)) THEN
        NS1=NS1+1
        XB(NS1,1)=X2
      ENDIF
      NS1=NS1+1
      XB(NS1,1)=X(I+1)
      NS(1)=NS1-1
      NS(2)=1
      XB(1,2)=X(I)
      XB(2,2)=X(I+1)
C
      DO 2 J = 1, NDA
      IF(I.EQ.J) THEN
        IEQ=1
       ELSE
        IEQ=2
      ENDIF
CC MU-INTEGRATION
      RIJ=0
      AMI = AMUA(J)
        DO 3 IS=1,NS(IEQ)
        DX=XB(IS,IEQ)-XB(IS+1,IEQ)
        DO 3 II = 1, N
          AME=XB(IS+1,IEQ)+DX*GX(II)
          W=DX*GW(II)*AME
          DO 3 K = 1, N
    3  RIJ=RIJ+(COSM1(K)*SEARF1(AME,AMI,FI1(K),U10,CR,CI)
     &   +COSM2(K)*SEARF1(AME,AMI,FI2(K),U10,CR,CI))*W
    2 RR(I,J)=RIJ/WA(I)
C SYMMETRIC OPERATION
      DO 5 I=1,NDA
      DO 5 J=1,I
      RRR=(RR(I,J)+RR(J,I))/2
      R(I,J)=RRR/AMUA(I)*WA(J)
    5 R(J,I)=RRR/AMUA(J)*WA(I)
      IF(ir.LE.NW)THEN
      do i1=1,KNDM
      do i2=1,KNDM     
      RRS(ir,M+1,i1,i2)=R(i1,i2)
      enddo
      enddo
      ENDIF
      RETURN
      END
      SUBROUTINE OCNR31(M,NDA,AMUA,WA,AM0,CR,CI,U10,SR,NW,ir,SRS)
C REFLECTION MATRIX OF OCEAN SURFACE
C--- HISTORY
C 92. 9. 1 CREATED BY HASUMI
C    12.23 MODIFIED BY NAKAJIMA
C 93. 3.22 /WA(I) debugged by Takashi
C     3.29 AMI -> AM0
C--- INPUT
C M      I     FOURIER ORDER
C KNDM   I     DECLARED DIMENSION OF R AND AMUA
C NDA    I     USED DIMENSION OF R AND AMUA
C AMUA  R(NDA) QUADRATURE POINTS IN HEMISPHERE
C              DECREASING ORDER (ZENITH TO HORIZON, OR, 1 -> 0)
C WA    R(NDA) Quadrature weights
C AM0    R     Cos (Solar Zenith Angle)
C CR       R    Relative refractive index of the media
C               About 1.33 for atmosphere to ocean incidence,
C               and 1/1.33 for ocean to atmosphere incidence.
C CI       R    Relative refractive index for imaginary part
C               M = CR + I*CI
C U10    R     WIND VELOCITY AT 10 M ABOVE THE SURFACE
C--- OUTPUT
C SR    SR(KNDA)  REFLECTION SOURCE MATRIX FOR M-TH FOURIER ORDER
C$END
      SAVE
      PARAMETER (KNDM  =16)
      PARAMETER(PI=3.141592654)
      DIMENSION AMUA(KNDM),WA(KNDM),SR(KNDM),SRS(10,KNDM,KNDM)
C LOCAL VARIABLES
      PARAMETER (KN=30,KNDM1=KNDM+1)
      DIMENSION X(KNDM1),GX(KN),GW(KN),FI1(KN),FI2(KN),COSM1(KN)
     & ,COSM2(KN),XB(5,2),NS(2)
      DATA INIT/1/
C
cs      write(6,*)'IN OCNR31'      
      IF(INIT.GT.0) THEN
        N=KN
        INIT=0
        CALL QGAUSN(GW, GX, N )
      ENDIF
C Parameters for integration
      SIGMA=SQRT(0.00534*U10)
      DISPA=ATAN(SIGMA)
      COSDS=COS(DISPA)
      DO 4 K=1,N
      DFI2=PI-DISPA
      FI1(K)=DISPA+DFI2*GX(K)
      FI2(K)=DISPA*GX(K)
      COSM1(K)=2*COS(FI1(K)*M)*DFI2 *GW(K)
    4 COSM2(K)=2*COS(FI2(K)*M)*DISPA*GW(K)
      X(1)=1
      IF(NDA.GE.2) THEN
        DO 1 I = 2, NDA
    1   X(I)=(AMUA(I-1)+AMUA(I))/2
      ENDIF
      X(NDA+1)=0
C
      DO 2 I = 1, NDA
CC X1=COS(THETA(I)-DISPA), X2=COS(THETA(I)+DISPA)
CC   WHERE AMUA(I)=COS(THETA(I))
      B=SQRT((1-AMUA(I)**2)*(1-COSDS**2))
      X1=AMUA(I)*COSDS+B
      X2=AMUA(I)*COSDS-B
CC SETING MU-BAOUDARY FOR MU-INEGRATION
      NS1=1
      XB(1,1)=X(I)
      IF(X1.LT.X(I)) THEN
        NS1=NS1+1
        XB(NS1,1)=X1
      ENDIF
      NS1=NS1+1
      XB(NS1,1)=AMUA(I)
      IF(X2.GT.X(I+1)) THEN
        NS1=NS1+1
        XB(NS1,1)=X2
      ENDIF
      NS1=NS1+1
      XB(NS1,1)=X(I+1)
      NS(1)=NS1-1
      NS(2)=1
      XB(1,2)=X(I)
      XB(2,2)=X(I+1)
C
      IF(AMUA(I).EQ.AM0) THEN
        IEQ=1
       ELSE
        IEQ=2
      ENDIF
CC MU-INTEGRATION
      RIJ=0
        DO 3 IS=1,NS(IEQ)
        DX=XB(IS,IEQ)-XB(IS+1,IEQ)
        DO 3 II = 1, N
          AME=XB(IS+1,IEQ)+DX*GX(II)
          W=DX*GW(II)*AME
          DO 3 K = 1, N
    3 RIJ=RIJ+(COSM1(K)*SEARF1(AME,AM0,FI1(K),U10,CR,CI)
     &    +COSM2(K)*SEARF1(AME,AM0,FI2(K),U10,CR,CI))*W
    2 SR(I)=RIJ/AMUA(I)/WA(I)
      IF(ir.LE.NW)THEN
      do i1=1,KNDM
      SRS(ir,M+1,i1)=SR(i1)
      enddo
      ENDIF
      RETURN
      END
      SUBROUTINE EQ20(A,B,NI,NJ,NA1,NA2)
C A(*,*) = B
C--- HISTORY
C B        R      SOURCE SCALER.
C NI       I      A(I,J)= B,    I=1,NI;   J=1,NJ
C NJ       I
C NA1      I      SIZE FOR A(NA1,NA2)
C NA2      I
C--- OUTPUT
C A   R(NA1,NA2)  DESTINATION 2-DIM ARRAY   A(*,*) = B
C$ENDI
      DIMENSION A(NA1,NA2)
      DO 1 J=1,NJ
      DO 1 I=1,NI
    1 A(I,J)=B
      RETURN
      END
      SUBROUTINE AAPB(A,B,NI,NJ,NAI,NBI)
C A=A+B
C--- HISTORY
C 88. 6. 6  REGISTERED BY T.NAKAJIMA
C--- INPUT
C A    R(NAI,*)    2-DIM ARRAY  A.
C B    R(NBI,*)    2-DIM ARRAY  B.
C NI      I        SUM (I,J) I=1,NI
C NJ      I        SUM (I,J) J=1,NI
C--- OUTPUT
C A                A+B
C$ENDI
      DIMENSION A(NAI,NJ),B(NBI,NJ)
      DO 1 J=1,NJ
      DO 1 I=1,NI
    1 A(I,J)=A(I,J)+B(I,J)
      RETURN
      END
      SUBROUTINE ADD(IT,NA0,N1,N2,N3,R1D,R1U,T1D,T1U,S1D,S1U
     & ,R2D,T2D,T2U,S2D,S2U,TU,RD,SD,SU,ERR)
C ADDING TWO LAYERS 1 AND 2.
C--- HISTORY
C 86.10.15  CHECK OK.
C 89.10.30  RE-EDIT.
C 90. 2. 2  ELEMINATE KND00
C--- INPUT
C IT         I      INDICATOR FOR CALCULATION OF TU AND SD.
C NA0        I      NO. OF SOLAR DIRECTIONS.
C N1,N2,N3   I       ----------------------------     MU1(I), I=1,N1
C R1D, R1U R(KNDM,   R1D, R1U, T1D, T1U, S1D, S1U    (LAYER-1)
C T1D, T1U   KNDM)   ----------------------------     MU2(I), I=1,N2
C R2D, R2U           R2D, R2U, T2D, T2U, S2D, S2U    (LAYER-2)
C T2D, T2U           ----------------------------     MU3(I),I=1,N3
C S1D, S1U R(KNDM,   SUFFIX U = UPGOING,   D = DOWNGOING INCIDENCES.
C S2D, S2U   KNA0)          R = REFLECTION,T = TRANSMISSION MATRICES.
C                           S = SOURCE MATRIX.
C--- OUTPUT
C RD       R(KNDM,   -----------------    MU1(I), I=1,N1
C TU         KNDM)   RD, TU, SD, SU       (LAYER 1+2)
C SD       R(KNDM,   -----------------    MU3(I), I=1,N3
C SU         KNA0)
C ERR       C*64     ERROR INDEX.
C--- PARAMETER
C KNA0       I       NUMBER OF SOLAR ZENITH ANGLES.
C KNDM       I       NUMBER OF QUADRATURE POINTS.
C--- AREAS FOR THIS ROUTINE
      PARAMETER (KNA0  =2)
      PARAMETER (KNDM  =16)
C
      CHARACTER ERR*64
      DIMENSION R1D(KNDM,KNDM),R1U(KNDM,KNDM),T1D(KNDM,KNDM)
     &,T1U(KNDM,KNDM),S1D(KNDM,KNA0),S1U(KNDM,KNA0)
     &,R2D(KNDM,KNDM),T2D(KNDM,KNDM),T2U(KNDM,KNDM)
     &,S2D(KNDM,KNA0),S2U(KNDM,KNA0)
     &,TU(KNDM,KNDM),RD(KNDM,KNDM),SD(KNDM,KNA0),SU(KNDM,KNA0)
C--- WORKING AREAS
      DIMENSION AA(KNDM,KNA0),BB(KNDM,KNDM),CC(KNDM,KNDM),DD(KNDM,KNDM)
C
      CALL AXB(AA,R2D,S1D,N2,N2,NA0,KNDM,KNDM,KNDM)
      CALL APB(SU,AA,S2U,N2,NA0,KNDM,KNDM,KNDM)
      CALL AXB(CC,R2D,R1U,N2,N2,N2,KNDM,KNDM,KNDM)
      CALL MULTI(N2,CC,BB,ERR)
      IF(ERR.NE.' ') THEN
        ERR='ERROR IN MULTI OF ADD'
        RETURN
      ENDIF
      CALL AXB(SD,BB,SU,N2,N2,NA0,KNDM,KNDM,KNDM)
      CALL AXB(SU,T1U,SD,N1,N2,NA0,KNDM,KNDM,KNDM)
      CALL AAPB(SU,S1U,N1,NA0,KNDM,KNDM)
      CALL AXB(CC,T1U,BB,N1,N2,N2,KNDM,KNDM,KNDM)
      CALL AXB(DD,R2D,T1D,N2,N2,N1,KNDM,KNDM,KNDM)
      CALL AXB(BB,CC,DD,N1,N2,N1,KNDM,KNDM,KNDM)
      CALL APB(RD,R1D,BB,N1,N1,KNDM,KNDM,KNDM)
      IF(IT.LE.0) RETURN
      CALL AXB(TU,CC,T2U,N1,N2,N3,KNDM,KNDM,KNDM)
      CALL AXB(AA,R1U,SD,N2,N2,NA0,KNDM,KNDM,KNDM)
      CALL AAPB(AA,S1D,N2,NA0,KNDM,KNDM)
      CALL AXB(SD,T2D,AA,N3,N2,NA0,KNDM,KNDM,KNDM)
      CALL AAPB(SD,S2D,N3,NA0,KNDM,KNDM)
      RETURN
      END
      SUBROUTINE EQ22(A,B,NI,NJ,NA1,NA2,NB1,NB2)
C  A = B
C--- HISTORY
C 88. 6. 6  REGISTERED BY T. NAKAJIMA
C--- INPUT
C B      R(NB1,NB2)     SOURCE 2-DIM ARRAY  B.
C NI       I           A(I,J) = B(I,J),  I=1,NI;  J=1,NJ
C NJ       I
C NA1      I           DIM  A(NA1,NA2)
C NA2      I
C NB1      I           DIM  B(NB1,NB2)
C--- OUTPUT
C A     R(NA1,NA2)     DESTINATION 2-DIM ARRAY  A = B.
C$ENDI
      DIMENSION A(NA1,NA2),B(NB1,NB2)
      DO 1 J=1,NJ
      DO 1 I=1,NI
    1 A(I,J)=B(I,J)
      RETURN
      END
      SUBROUTINE EQ23(A,B,NI,NJ,K,NA1,NA2,NB1,NB2,NB3)
C  A(*,*)= B(*,*,K)
C--- HISTORY
C 88. 6. 6  REGISTERED BY T. NAKAJIMA
C--- INPUT
C B      R(NB1,NB2,NB3)     SOURCE 3-DIM ARRAY B.
C NI         I              A(I,J)= B(I,J,K)
C NJ         I              I=1,NI;  J=1,NJ
C K          I
C NA1        I              DIM A(NA1, NA2)
C NA2        I
C NB1        I              DIM B(NB1,NB2,NB3)
C NB2, NB3   I
C--- OUTPUT
C A     R(NA1,NA2)          DESTINATION 2-DIM ARRAY A(*,*) = B(*,*,K)
C$ENDI
      DIMENSION A(NA1,NA2),B(NB1,NB2,NB3)
      DO 1 J=1,NJ
      DO 1 I=1,NI
    1 A(I,J)=B(I,J,K)
      RETURN
      END
      SUBROUTINE MULTI(ND,CD,CC,ERR)
C CALCULATION OF MULTIPLE REFLECTION BETWEEN TWO LAYERS.
C--- HISTORY
C 86.10.15  CHECK OK.
C 89.10.30  RE-EDIT.
C 94. 5. 7  ERR*64 -> ERR*(*)
C--- INPUT
C ND          I     NO. OF STREAMS AT THE INTERFACE BETWEEN TWO LAYERS.
C CD      R(KNDM,   R = R1 * R2.
C           KNDM)
C--- OUTPUT
C CC      R(KNDM,   CC = ( 1 - CD )**-1 = 1 + CD + CD**2 + CD**3 + ...
C           KNDM)
C ERR     C*64      ERROR INDEX.
C--- PARAMETER
C KNDM        I     NUMBER OF QUADRATURE POINTS.
C--- AREAS FOR THIS ROUTINE
      PARAMETER (KNDM  =16)
C
      CHARACTER ERR*(*)
      DIMENSION CD(KNDM,KNDM),CC(KNDM,KNDM)
C--- WORKING AREA
      PARAMETER (KNDM2=2*KNDM)
      DIMENSION IW(KNDM2)
C---
      DO 1 I=1,ND
      DO 2 J=1,ND
    2 CC(I,J)= -CD(I,J)
    1 CC(I,I)=1 - CD(I,I)
C INVERSION OF -CC-.
      EPS=0
      CALL TNVSS2(ND,CC,DT,EPS,KNDM,IW,ERR)
      IF(ERR.NE.' ') ERR='ERROR IN MULTI'
      RETURN
      END
      SUBROUTINE RP33(A,B,NI,NJ,K,L,NA1,NA2,NA3,NB1,NB2,NB3)
C A(*,*,K) = B(*,*,L)
C--- HISTORY
C 88. 5. 6  REGISTERED BY T. NAKAJIMA
C--- INPUT
C B      R(NB1,NB2,NB3)     SOURCE 3-DIM ARRAY  B.
C NI,NJ,K,L    I            A(I,J,K)=B(I,J,L), I=1,NI; J=1,NJ
C NA1,NA2,NA3  I            DIM A(NA1,NA2,NA3)
C NB1,NB2,NB3  I            DIM B(NB1,NB2,NB3)
C--- OUTPUT
C A      R(NA1,NA2,NA3)     DESTINATION 3-DIM ARRAY A(*,*,K)=B(*,*,L)
C$ENDI
      DIMENSION A(NA1,NA2,NA3),B(NB1,NB2,NB3)
      DO 1 J=1,NJ
      DO 1 I=1,NI
    1 A(I,J,K)=B(I,J,L)
      RETURN
      END
      SUBROUTINE RP30(A,B,NI,NJ,K,NA1,NA2,NA3)
C A(*,*,K) = B
C--- HISTORY
C 88. 5. 6   REGISTERED BY T. NAKAJIMA
C--- INPUT
C B            R       SOURCE SCALER
C NI,NJ,K      I       A(I,J,K)=B,  I=1,NI; J=1,NJ
C NA1,NA2,NA3  I       DIM A(NA1,NA2,NA3)
C--- OUTPUT
C A    R(NA1,NA2,NA3)  DESTINATION 3-DIM ARRAY A(*,*,K)=B
C$ENDI
      DIMENSION A(NA1,NA2,NA3)
      DO 1 J=1,NJ
      DO 1 I=1,NI
    1 A(I,J,K)=B
      RETURN
      END
      SUBROUTINE APB(C,A,B,NI,NJ,NCI,NAI,NBI)
C C=A+B
C--- HISTORY
C 88. 6. 6  REGISTERED BY T. NAKAJIMA
C--- INPUT
C  A       R(NAI,*)     2-DIM ARRAY A.
C  B       R(NBI,*)     2-DIM ARRAY B.
C NI         I          SUM (I,J) , I=1,NI
C NJ         I          SUM (I,J) , J=1,NJ
C NCI        I          SIZE FOR C(NCI,*)
C NAI        I          SIZE FOR A(NAI,*)
C NBI        I          SIZE FOR B(NBI,*)
C--- OUTPUT
C  C       R(NCI,*)     A+B
C$ENDI
      DIMENSION A(NAI,NJ),B(NBI,NJ),C(NCI,NJ)
      DO 1 J=1,NJ
      DO 1 I=1,NI
    1 C(I,J)=A(I,J)+B(I,J)
      RETURN
      END
      SUBROUTINE RPVBRF(R0B,KB,THB,THIRAD,EZARAD,AARAD,NSZA,NEZA,
     &                  NAA,NI,NY,NZ,BRF,IBRF)
C CALCULATION OF BRF USING RPV MODEL
C- INPUT 
C R0B,KB,THB    R        PARAMETERS OF RPV MODEL 
C THIRAD        R  RAD   INCIDENT ZENITH ANGLE
C EZARAD  R(NEZA)  RAD   EMERGING ZENITH ANGLE
C AARAD   R(NAA)   RAD   AZIMUTHAL ANGLE
C NSZA     I             SIZE OF MATRIX
C NEZA     I             SIZE OF MATRIX
C NAA      I             SIZE OF MATRIX
C NI       I            (INCIDENT ZENITH ANGLE)ORDER IN BRF DATABASE
C NY       I             NUMBER OF EMERGING ZENITH ANGLE
C NZ       I             NUMBER OF AZIMUTHAL ANGLE
C- OUTPUT
C BRF      R(NSZA,NEZA,NAA)   CANOPY BRF DATA BASAE "BRF(NI,*,*)"
C- PARAMETER
      PARAMETER (PI=3.141592653589793)
C- LOCAL
      REAL    R0B,KB,THB,THB1
      REAL    F1,F2,F3,BRF(NSZA,NEZA,NAA)
      INTEGER NI,NE,NP
      REAL    THIRAD,THERAD,PHI,PHIRAD
      DIMENSION EZARAD(NY),AARAD(NZ),AARAD1(NZ)
C
C LOOP OF EMERGING ZENITH ANGLE
cs         write(6,*)'in RPVBRF'
         THB1=THB-1.0
cs         write(6,*)'THB=',THB1
cs         write(6,*)'THIRAD',THIRAD*180./PI
cs         write(6,*)'EZARAD',EZARAD*180./PI
         do NP=1,NZ
         AARAD1(NP)=(AARAD(NP)*180./PI+180.)*PI/180.
         enddo
cs         write(6,*)'AARAD',AARAD1*180./PI
         DO 20 NE=1,NY
            THERAD=EZARAD(NE)
C LOOP OF PHI(AZIMUTH ANGLE)
           DO 30 NP=1,NZ
              PHIRAD=AARAD1(NP)
C CALC. BRF
              F1=(cos(THIRAD)**(KB-1.)*cos(THERAD)**(KB-1.))
     &           /(cos(THIRAD)+cos(THERAD))**(1.-KB)
cs              write(6,*)'F1=',F1
              xg=cos(THIRAD)*cos(THERAD)+sin(THIRAD)*
     &           sin(THERAD)*cos(PHIRAD)
cs              write(6,*)'cost1,costh2,sinth1,sinth2,cosphi',
cs     &        cos(THIRAD),cos(THERAD),sin(THIRAD),sin(THERAD),
cs     &        cos(PHIRAD)
cs              write(6,*)'cosxg',xg
              xg=acos(xg)
cs              write(6,*)'xg=',xg
              F2=(1.-THB1**2)/(1.+THB1**2-2.*THB1*cos((PI-xg)))**1.5
cs              write(6,*)'F2=',F2
              G=(abs(tan(THIRAD)**2+tan(THERAD)**2-2.*tan(THIRAD)*
     &        tan(THERAD)*cos(PHIRAD)))**0.5
              F3=1.+(1.-R0B)/(1.+G)
cs              write(6,*)'F3=',F3
              BRF(NI,NE,NP)=(R0B*F1*F2*F3)
cs              BRF(NI,NE,NP)=0.0
cs              write(6,*)'BRF',BRF
              IF(IBRF.EQ.1)BRF(NI,NE,NP)=0.
 30          CONTINUE
 20       CONTINUE
      RETURN
      END
C********************************************************************************************************

      SUBROUTINE LRSBRF(R0B,KB,THB,THIRAD,EZARAD,AARAD,NSZA,NEZA,
     &                  NAA,NI,NY,NZ,BRF,IBRF,IFUR,IRFS)
C CALCULATION OF BRF USING RPV MODEL
C- INPUT 
C R0B,KB,THB    R        PARAMETERS OF RPV MODEL 
C THIRAD        R  RAD   INCIDENT ZENITH ANGLE
C EZARAD  R(NEZA)  RAD   EMERGING ZENITH ANGLE
C AARAD   R(NAA)   RAD   AZIMUTHAL ANGLE
C NSZA     I             SIZE OF MATRIX
C NEZA     I             SIZE OF MATRIX
C NAA      I             SIZE OF MATRIX
C NI       I            (INCIDENT ZENITH ANGLE)ORDER IN BRF DATABASE
C NY       I             NUMBER OF EMERGING ZENITH ANGLE
C NZ       I             NUMBER OF AZIMUTHAL ANGLE
C- OUTPUT
C BRF      R(NSZA,NEZA,NAA)   CANOPY BRF DATA BASAE "BRF(NI,*,*)"
C- PARAMETER
      PARAMETER (PI=3.141592653589793)
C- LOCAL
      REAL    R0B,KB,THB,BRRATIO,HBRATIO
      REAL    KVOL,KGEO,BRF(NSZA,NEZA,NAA)
      INTEGER NI,NE,NP
      REAL    THIRAD,THERAD,PHI,PHIRAD
      DIMENSION EZARAD(NY),AARAD(NZ),AARAD1(NZ)
C
C LOOP OF EMERGING ZENITH ANGLE
cs         write(6,*)'in LRSBRF'
cs         THB1=THB-1.0
cs         write(6,*)'THB=',THB1
cs         write(6,*)'R0B,KB,THB',R0B,KB,THB
cs         write(6,*)'THIRAD',THIRAD*180./PI
cs         write(6,*)'EZARAD',EZARAD*180./PI
cs         write(6,*)'AARAD',AARAD*180./PI
cs         write(6,*)'IN LRSBRF',IRFS
cs         IF(IRFS.EQ.1)THEN
cs         write(6,*)'IN LRSBRF'
cs         write(6,*)'R0B,KB,THB',R0B,KB,THB
cs         ENDIF
      do NP=1,NZ
		IF((AARAD(NP)*180./PI).GT.180.)THEN
			AARAD1(NP)=(AARAD(NP)*180./PI-180.)*PI/180.
		ENDIF
         IF((AARAD(NP)*180./PI).LE.180.)THEN
			AARAD1(NP)=(180.0-AARAD(NP)*180./PI)*PI/180.
         ENDIF
      enddo
cs         write(6,*)'THIRAD',THIRAD*180./PI
cs         write(6,*)'EZARAD',EZARAD*180./PI
cs         write(6,*)'AARAD',AARAD1*180./PI
cs         write(6,*)
cs         write(6,*)
cs         write(6,*)
      BRRATIO=1.0
      HBRATIO=2.0
      DO 20 NE=1,NY
		EZARAD1=(EZARAD(NE)*180./PI)
cs            IF(EZARAD1.GT.90.)EZARAD1=180.-EZARAD1
          THERAD=EZARAD1*PI/180.
cs            WRITE(*,*)'THERAD',EZARAD1
cs            WRITE(*,*)EZARAD1
cs            THERAD=EZARAD1(NE)
          IF((THERAD*180./PI).LT.90.0)THEN
C LOOP OF PHI(AZIMUTH ANGLE)
			DO 30 NP=1,NZ
cs              PHIRAD=AARAD1(NP)
				PHIRAD=AARAD(NP)
C ***CALCULATIONS OF PHASE ANGLE FOR KVOL****************************************
cs              write(6,*)'THIRAD,THERAD,PHIRAD',THIRAD,THERAD,PHIRAD
				COS_TZETA=cos(THIRAD)*cos(THERAD)+sin(THIRAD)*
     &			sin(THERAD)*cos(PHIRAD)
cs              write(6,*)'COS_TZETA',COS_TZETA
				TZETA=acos(max(-1.,min(1.,COS_TZETA)))
cs              write(6,*)'TZETA',TZETA
				SIN_TZETA=sin(TZETA)
cs              write(6,*)'SIN_TZETA',SIN_TZETA
C****************************************************************************
C***CALCULATION OF KVOL*********************************************************
cs             write(6,*)'PI/2.-TZETA',PI/2.-TZETA
cs             write(6,*)'COS_TZETA',COS_TZETA
cs             write(6,*)'SIN_TZETA',SIN_TZETA
cs             write(6,*)'cos(THERAD)',cos(THERAD)
cs             write(6,*)'cos(THIRAD)',cos(THIRAD)
cs             write(6,*)'cos(THERAD)+cos(THIRAD)',
cs     &                  cos(THERAD)+cos(THIRAD)
				xt=((PI/2.-TZETA)*COS_TZETA+SIN_TZETA)/
     $             (cos(THERAD)+cos(THIRAD))
				KVOL=xt-PI/4.
cs             write(6,*)'KVOL',KVOL
C*******************************************************************************
***CALCULATION OF PRIME ANGLES****************************************************
				TAN_THERADP=tan(THERAD)
				IF(TAN_THERADP.LT.0.)TAN_THERADP=0.
				THERADP=atan(TAN_THERADP)
				SIN_THERADP=sin(THERADP)
				COS_THERADP=cos(THERADP)
				TAN_THIRADP=tan(THIRAD)
				IF(TAN_THIRADP.LT.0.)TAN_THIRADP=0.
				THIRADP=atan(TAN_THIRADP)
				SIN_THIRADP=sin(THIRADP)
				COS_THIRADP=cos(THIRADP)
cs             write(6,*)'TAN_THERADP',TAN_THERADP
cs             write(6,*)'THERADP',THERADP
cs             write(6,*)'SIN_THERADP',SIN_THERADP
cs             write(6,*)'COS_THERADP',COS_THERADP
cs             write(6,*)'TAN_THIRADP',TAN_THIRADP
cs             write(6,*)'THIRADP',THIRADP
cs             write(6,*)'SIN_THIRADP',SIN_THIRADP
cs             write(6,*)'COS_THIRADP',COS_THIRADP
C***********************************************************************************
C***CALCULATION OF THE DISTANCE*****************************************************
				DIST=TAN_THERADP*TAN_THERADP+TAN_THIRADP*TAN_THIRADP-
     $			2.*TAN_THERADP*TAN_THIRADP*cos(PHIRAD)
				DIST=SQRT(max(0.,DIST))
cs              write(6,*)'DIST',DIST
C***********************************************************************************
C***OVERLAP CALCULATIONS***************************************************************
cs              IF(SIN_THERADP.LT.1e-16)SIN_THERADP=1e-16
cs              IF(SIN_THIRADP.LT.1e-16)SIN_THIRADP=1e-16
				xtemp=1./COS_THERADP+1./COS_THIRADP
				COS_T=SQRT(DIST*DIST+TAN_THERADP*TAN_THERADP*
     &			TAN_THIRADP*TAN_THIRADP*sin(PHIRAD)*sin(PHIRAD))
				COS_T=2.*COS_T/xtemp
				COS_T=max(-1.,min(1.,COS_T))
				TVAR=acos(COS_T)
				SIN_T=sin(TVAR)
				OVERLAP=1./PI*(TVAR-SIN_T*COS_T)*xtemp
cs              write(6,*)'xtemp',xtemp
cs              write(6,*)'COS_T',COS_T
cs              write(6,*)'SIN_T',SIN_T
cs              write(6,*)'TVAR',TVAR
cs              write(6,*)'OVERLAP',OVERLAP
C*************************************************************************************
C***CALCULATION OF PHASE ANGLE FOR KGEO***********************************************
				COS_TZETAP=COS_THERADP*COS_THIRADP+SIN_THERADP*
     &			SIN_THIRADP*cos(PHIRAD)
				TZETAP=acos(max(-1.,min(1.,COS_TZETAP)))
				SIN_TZETAP=sin(TZETAP)
cs              write(6,*)'COS_TZETAP',COS_TZETAP
cs              write(6,*)'TZETAP',TZETAP
cs              write(6,*)'SIN_TZETAP',SIN_TZETAP
C**************************************************************************************
C***CALCULATION OF KGEO****************************************************************
				KGEO=OVERLAP-xtemp+0.5*(1.+COS_TZETAP)/
     &			COS_THIRADP/COS_THERADP
cs              write(6,*)'OVERLAP',OVERLAP
cs              write(6,*)'xtemp',xtemp
cs              write(6,*)'COS_TZETAP',COS_TZETAP
cs              write(6,*)'COS_THIRADP',COS_THIRADP
cs              write(6,*)'COS_THERADP',COS_THERADP
cs              write(6,*)'KGEO',KGEO
C***************************************************************************************
C***BRF CALCULATION***********************************************************************
				BRF(NI,NE,NP)=R0B+KB*KVOL+THB*KGEO
cs              BRF(NI,NE,NP)=0.0
cs              BRF(NI,NE,NP)=1.e-30
cs              if (BRF(NI,NE,NP).LT.0.)then
cs              write(6,*)'SZA',THIRAD*180./PI
cs              write(6,*)'VZA',THERAD*180./PI
cs              write(6,*)'PHI',PHIRAD*180./PI
cs              write(6,*)'BRF',BRF(NI,NE,NP)
cs              endif
				IF(IBRF.EQ.1)BRF(NI,NE,NP)=0.
 30		   CONTINUE
          ENDIF
 20   CONTINUE
      IF(IFUR.EQ.1)THEN
		DO i1=1,NZ
              ik=0
			DO i2=1,NY
				ik=ik+1
				if((EZARAD(i2)*180./PI).GT.76.0)THEN
					BRF(NI,i2,i1)=BRF(NI,ik-1,i1)
				endif
              ENDDO
          ENDDO
          IF(NI.GE.7)THEN
cs              write(6,*)'THIRAD IF',THIRAD*180./PI
cs              write(6,*)'NI',NI
              DO i1=1,NZ
			    DO i2=1,NY
					BRF(NI,i2,i1)=BRF(NI-1,i2,i1) 
				enddo 
              enddo
          ENDIF
      ENDIF
cs              IF(IRFS.EQ.1)THEN
cs              write(6,*)'IN LRSBRF'
cs              write(6,*)'R0B,KB,THB',R0B,KB,THB
cs              write(6,*)
cs              write(*,*)'SZA',THIRAD*180./PI
cs              write(*,*)
cs              DO i1=1,NZ
cs              write(6,*)'PHI',AARAD1(i1)*180./PI
cs              write(6,*)
cs              write(6,*)'THETA',(EZARAD(i2)*180./PI,i2=1,NY)
cs              write(6,*)
cs              write(6,*)'BRF',(BRF(NI,i2,i1),i2=1,NY)
cs              write(6,*)
cs              write(6,*)
cs             ENDDO
cs             ENDIF
      RETURN
      END
CS*********************************VECTOR CODE*******************************************************
      SUBROUTINE OS_HERMAN(IPRI,IMSC,IPROF,KNAV,za,NT1,
     &tmol,tetas,NBV,vis,fiv,isaut,wind,
     &iwat,rsurf,igrd,vk,gteta,rho,anb,bnb,
     &NM,pha11,pha12,pha22,pha33,
     &EXT,SSA,JP,ECH,WD,H,
     &NG,NN,
     &IREAD,PF11_I,PF12_I,PF22_I,PF33_I,ANGL,NANG,NQDR,
     &ITRONC,
     &thd,iop,SLout,SQout,SUout,SLPout,
     &tevtot,NAV,UFT,DFT,UFG,DFG)

      parameter(NBVM=100,NMM=5,KNT=51)
      PARAMETER(NN0=85,NG0=91)
      PARAMETER (KSD=3,KANG=2*NG0+1,KNG=NG0)
      double precision rmu,rg,xmu,xg
      double precision AMU1,PMU1,AMU2,PMU2
      REAL I1,IM,IZT,IZG,IZTN,IZGN
      INTEGER MS,LCK,LWRK,IFAIL
CD1      PARAMETER (MS=82,LCK=MS+4,LWRK=6*MS+16)
CD      PARAMETER (MS=83,LCK=MS+4,LWRK=6*MS+16)
CD      DOUBLE PRECISION KS(LCK),CS(LCK),WRK(LWRK),XS(MS),YS(MS),
CD     &YS1(MS)
CD      PARAMETER (MS0=KANG,LCK0=MS0+4,LWRK=6*MS0+16)
      DOUBLE PRECISION KS(NANG+4),KS1(NANG+4),
     &KS2(NANG+4),CS(NANG+4),CS1(NANG+4),
     &CS2(NANG+4),WRK(6*NANG+16),XS(NANG),YS(NANG),YS1(NANG),
     &KC_11(NM,NANG+4),KC_12(NM,NANG+4),
     &CS_11(NM,NANG+4),CS_12(NM,NANG+4)

CD      DOUBLE PRECISION KS(LCK0),KS1(LCK0),KS2(LCK0),CS(LCK0),CS1(LCK0),
CD     &CS2(LCK0),WRK(LWRK),XS(MS0),YS(MS0),YS1(MS0),
CD     &KC_11(KSD,LCK0),KC_12(KSD,LCK0),CS_11(KSD,LCK0),CS_12(KSD,LCK0)
      DOUBLE PRECISION XARG,FIT,XARG1
      REAL LINEARD, LINEAR_LN
c IM: MESURES: NAV-UP:1 a NN; SOL-DOWN:-NN a-1; SAUF 1
c IZT: tous ordres IS=0 de -NN a NN niveau NAV (IZT(1,NN)=0 si NAV=1)
c IZG: tous ordres IS=0 de -NN a NN niveau SOL
c IZTN,IZGN: ordre n; pour serie geometrique
      DIMENSION AMU1(-NG0:NG0),PMU1(-NG0:NG0),
     &AMU2(-NN0:NN0),PMU2(-NN0:NN0)
      dimension 
     &EXT(NMM),SSA(NMM),JP(NMM),ECH(NMM),
     &WD(KNT-1,NMM),H(KNT),
     &thv(-NN0:NN0),rg(-NN0:NN0),
     &xmu(-NG0:NG0),xg(-NG0:NG0),
     &zmu(-NG0:NG0),zg(-NG0:NG0),
     &coff(NBVM),idir(NBVM),chi(NBVM),
     &alp(0:2*NG0,NMM),bet(0:2*NG0,NMM),gam(0:2*NG0,NMM),
     &zet(0:2*NG0,NMM),
     &xalp(0:2*NG0),xbet(0:2*NG0),xgam(0:2*NG0),xzet(0:2*NG0),
     &pha11(NMM,-KNG:KNG),
     &pha12(NMM,-KNG:KNG),pha33(NMM,-KNG:KNG),pha22(NMM,-KNG:KNG),
     &qh11(-NG0:NG0),qh12(-NG0:NG0),qh33(-NG0:NG0),qh22(-NG0:NG0),
     &f11(NM+1,NBVM),f12(NM+1,NBVM),
     &rer(NN0,NN0,0:50),
     &PR11(-NN0:NN0,NMM),PR21(-NN0:NN0,NMM),PR31(-NN0:NN0,NMM),
     &P11(-NN0:NN0,NN0,NMM),P21(-NN0:NN0,NN0,NMM),P22(-NN0:NN0,NN0,NMM),
     &P31(-NN0:NN0,NN0,NMM),P32(-NN0:NN0,NN0,NMM),P33(-NN0:NN0,NN0,NMM),
     &B11(-NN0:NN0,-NN0:NN0,KNT),B12(-NN0:NN0,-NN0:NN0,KNT),
     &B13(-NN0:NN0,-NN0:NN0,KNT),B21(-NN0:NN0,-NN0:NN0,KNT),
     &B22(-NN0:NN0,-NN0:NN0,KNT),B23(-NN0:NN0,-NN0:NN0,KNT),
     &B31(-NN0:NN0,-NN0:NN0,KNT),B32(-NN0:NN0,-NN0:NN0,KNT),
     &B33(-NN0:NN0,-NN0:NN0,KNT),
     &IZT(-NN0:NN0),IZG(-NN0:NN0),IZTN(-NN0:NN0),IZGN(-NN0:NN0),
     &XIZT(-NN0:NN0),XIZG(-NN0:NN0),
     &QM(-NN0:NN0),UM(-NN0:NN0),IM(-NN0:NN0),
     &SFI(KNT-1,-NN0:NN0),SFQ(KNT-1,-NN0:NN0),SFU(KNT-1,-NN0:NN0),
     &I1(KNT,-NN0:NN0),Q1(KNT,-NN0:NN0),U1(KNT,-NN0:NN0),
     &SL(NBVM),SQos(NBVM),SUos(NBVM),SLP(NBVM),SQ(NBVM),SU(NBVM),
     &SL1(NBVM),SP1(NBVM),SLPsig(NBVM),SLPsigos(NBVM),
     &SQ1(NBVM),SU1(NBVM),RL1(NBVM),RQ1(NBVM),RU1(NBVM),
     &thd(NBVM),vis(NBVM),fiv(NBVM),
     &SLT1(NBVM),SQT1(NBVM),SUT1(NBVM),SQT1os(NBVM),
     &SUT1os(NBVM),SLP1(NBVM),SLP1sig(NBVM),SLP1sigos(NBVM),
     &SQout(NBVM),SUout(NBVM),SLPout(NBVM),SLout(NBVM),
     &R11(NN0,NN0,0:2*NG0-2),R12(NN0,NN0,0:2*NG0-2),
     &R13(NN0,NN0,0:2*NG0-2),
     &R21(NN0,NN0,0:2*NG0-2),R22(NN0,NN0,0:2*NG0-2),
     &R23(NN0,NN0,0:2*NG0-2),
     &R31(NN0,NN0,0:2*NG0-2),R32(NN0,NN0,0:2*NG0-2),
     &R33(NN0,NN0,0:2*NG0-2)
     
      dimension PF11(KANG,KSD),PF12(KANG,KSD),PF22(KANG,KSD),
     &PF33(KANG,KSD),ANGL1(KANG),
     &F1(KANG),F2(KANG),F3(KANG),F4(KANG),
     &PF11_I(NANG,KSD),PF12_I(NANG,KSD),PF22_I(NANG,KSD),
     &PF33_I(NANG,KSD),
     &ANGL(NANG)
     
      dimension rmu(-NN0:NN0),PSL(-1:2*NG0,-NN0:NN0),
     &RSL(-1:2*NG0,-NN0:NN0),TSL(-1:2*NG0,-NN0:NN0)

CD MODIFICATION SI TRONCATURE 0
      dimension P11av(KANG),P12av(KANG),Vav(KANG),Qav(KANG),
     &Q11(-NG0:NG0),Q12(-NG0:NG0),Q22(-NG0:NG0),Q33(-NG0:NG0),
     &DDL1(NBVM),DDQ1(NBVM)
cs      open(8,file="Sinyuk_input")
cs      write(8,*)WAVE,'  WAVE'
      IF(IMSC.EQ.0)THEN
      write(8,*)IPRI,'  IPRI'
      write(8,*)IMSC,'  IMSC'
      write(8,*)IPROF,'  IPROF'
      write(8,*)NBV,'  NBV'
      do iv=1,NBV
      write(8,22)vis(iv),fiv(iv),iv,' vis(IV),fiv(IV),IV'
      enddo
      write(8,*)tetas,'  tetas'
      write(8,*)isaut,'  isaut'
      write(8,*)wind,'  wind'
      write(8,*)iwat,'  iwat'
      write(8,*)rsurf,'  rsurf'
      write(8,*)igrd,'  igrd'
      write(8,*)vk,'  vk'
      write(8,*)gteta,'  gteta'
      write(8,*)rho,'  rho'
      write(8,*)anb,'  anb'
      write(8,*)bnb,'  bnb'
      write(8,*)iop,'  iop'
      write(8,*)NG,'  NG1'
      write(8,*)NN,'  NG2  '
      write(8,*)IREAD,'  IREAD'
      write(8,*)NM,'  NM'
      write(8,*)NANG,'NANG'
      write(8,*)NQDR,'NQDR'
      write(8,*)ITRONC,'ITRONC'      
      do i=1,NM
      write(8,23)EXT(i),JP(i),ECH(i),SSA(i),
     &'EXT(ISD),JP(ISD),ECH(ISD),SSA(ISD)' 
c n'importe quoi si IPROF=1
      do k=-NG,NG
      write(8,24)ANGL(NG+1+k),pha11(i,k),pha12(i,k),pha22(i,k),
     &pha33(i,k)
     &,k,
     &' ANGL,pha11(ISD,J),pha12(ISD,J),pha22(ISD,J),pha33(ISD,J),J,'
      enddo
      enddo
      write(8,23)EXT(NM+1),JP(NM+1),ECH(NM+1),SSA(NM+1)
     &,'EXT(ISD),JP(ISD),ECH(ISD),SSA(ISD)'
      write(8,*)NT1,'  NT'
      write(8,*)KNAV,za,NAV,'KNAV,za,NAV'
      ENDIF

      pi = 3.141592653
      rmus=cos(pi*tetas/180.0)

      ron=0.0279
CD      ron=0.014
CD      ron=0.03
CD      ron=0
      aaa=2*(1-ron)/(2+ron)
      betm=0.5*aaa
      gamm=-aaa*sqrt(1.5)
      alpm=3*aaa
C******removing polarization!!!*****
CD      gamm=0
CD      alpm=0
C******removing polarization!!!*****

c     ITRONC=0
c     if(ITRONC.eq.1) call GAUSS_2(NQDR,zmu,zg)
      call GAUSS_2(NQDR,zmu,zg)
C CORRIGER D'ABORD LA MATRICE PFij?
C Tres peu d'influence. Modifie de qqs 10-3 en relatif
      IF(IREAD.GE.0) THEN
       LCK=NANG+4
       DO I=1,4
        DO ISD=1,NM
         DO IA=1,NANG
         IF(I.EQ.1) THEN
          IF(ANGL(2).GT.ANGL(1)) THEN
           XS(IA)=ANGL(IA)
	     IF(PF11_I(IA,ISD).LT.1.0E-30)PF11_I(IA,ISD)=1.0E-30
           YS(IA)=log(PF11_I(IA,ISD))
          ELSE
           XS(IA)=ANGL(NANG-IA+1)
		IF(PF11_I(NANG-IA+1,ISD).LT.1.0E-30)PF11_I(NANG-IA+1,ISD)=1.0E-30
           YS(IA)=log(PF11_I(NANG-IA+1,ISD))
          ENDIF
         ENDIF
         IF(ANGL(2).GT.ANGL(1)) THEN
          IF(I.EQ.2) YS(IA)=PF12_I(IA,ISD)
          IF(I.EQ.3) THEN
			IF(PF22_I(IS,ISD).LT.1.0E-30)PF22_I(IS,ISD)=1.0E-30
			YS(IA)=log(PF22_I(IA,ISD))
		ENDIF
          IF(I.EQ.4) YS(IA)=PF33_I(IA,ISD)
         ELSE
          IF(I.EQ.2) YS(IA)=PF12_I(NANG-IA+1,ISD)
          IF(I.EQ.3) THEN
			IF(PF22_I(NANG-IA+1,ISD).LT.1.0E-30)PF22_I(NANG-IA+1,ISD)=1.0E-30
			YS(IA)=log(PF22_I(NANG-IA+1,ISD))
		ENDIF
          IF(I.EQ.4) YS(IA)=PF33_I(NANG-IA+1,ISD)
         ENDIF
         ENDDO
        CALL E01BAF(NANG,XS,YS,KS,CS,LCK,WRK,6*NANG+16,IFAIL)
         IF(I.EQ.1) THEN
          DO II=1,LCK
           KC_11(ISD,II)=KS(II)
           CS_11(ISD,II)=CS(II)
         ENDDO
         ENDIF
         IF(I.EQ.2) THEN
          DO II=1,LCK
           KC_12(ISD,II)=KS(II)
           CS_12(ISD,II)=CS(II)
          ENDDO
          ENDIF
CD        do j=-NG,NG
        do j=-NQDR,NQDR
        IFAIL=0
        XARG=acos(zmu(j))*180./pi
        IF(XARG.GT.180) XARG=180.0
        ANGL1(NQDR+j+1)=XARG
        CALL E02BBF(NANG+4,KS,CS,XARG,FIT,IFAIL)
        IF(I.EQ.1) PF11(NQDR+j+1,ISD)=exp(FIT)
        IF(I.EQ.2) PF12(NQDR+j+1,ISD)=FIT
        IF(I.EQ.3) PF22(NQDR+j+1,ISD)=exp(FIT)
        IF(I.EQ.4) PF33(NQDR+j+1,ISD)=FIT
         ENDDO
        ENDDO
       ENDDO ! DO=I=1,4
      ENDIF
cs      do i=-NQDR,NQDR
cs      xx=acos(zmu(i))*180./pi
cs      WRITE(*,*)xx
cs      enddo
cs      WRITE(*,*)''
      NANG1=2*NQDR+1
      do ISD=1,NM
      zz0=0.0
      do j=-NQDR,NQDR
      k=NQDR+j+1
      zz0=zz0+0.5*PF11(k,ISD)*zg(j)
      enddo
      stron=zz0
c     write(6,*)'stron',stron
      do k=1,NANG1
      xxx=PF11(k,ISD)/zz0
      PF12(k,ISD)=PF12(k,ISD)*xxx/PF11(k,ISD)
      PF22(k,ISD)=PF22(k,ISD)*xxx/PF11(k,ISD)
      PF33(k,ISD)=PF33(k,ISD)*xxx/PF11(k,ISD)
      PF11(k,ISD)=xxx
      enddo
      enddo

cs      IF (III.LT.1) THEN
c MODIFICATION
      call gauss(NG,xmu,xg)
      call gauss_1(NN,rmu,rg)
cs      write(*,*)'xmu'
cs      do j=-NG,NG
cs      write(*,*)acos(xmu(j))*180./pi,j
CD      WRITE(*,*) thv(j),j
cs      enddo
cs      write(*,*)'rmu'      
      do j=-NN,NN
      thv(j)=acos(rmu(j))*180./pi
cs      WRITE(*,*) thv(j),j
      enddo

CD              do j=-NG,NG
CD            tang=acos(xmu(j))*180./pi
CD              WRITE(*,*) tang,J
CD              enddo

cs      III=III+1
cs      ENDIF
CD      WRITE(*,*) xmu,'xmu'
CD      WRITE(*,*) rmu,'rmu'
CD      WRITE(*,*) xg,'xg'
CD      WRITE(*,*) rg,'rg'
CD      WRITE(*,*) thv,'thv'
CD      WRITE(*,*) rmu(15),'rmu(15)'
CD      write(6,*)'NT lu',NT
CD      write(6,*)'H(NT) lu', H(NT)

c-GEOMETRIE
      itest=+1
      do iv=1,NBV
      chi(iv)=ANGTURN(tetas,vis(iv),fiv(iv))
      xx=cos(vis(iv)*pi/180.)
      if (itest.eq.+1.and.xx.lt.0.0) itest=-1
      zz=-rmus*xx-sqrt(1-rmus*rmus)*sqrt(1-xx*xx)*cos(fiv(iv)*pi/180.)
      thd(iv)=acos(zz)*180./pi
      k=NN
  121 k=k-1
      if(thv(k).lt.vis(iv))goto 121
      idir(iv)=k
      coff(iv)=(vis(iv)-thv(k+1))/(thv(k)-thv(k+1))
      enddo
      if(IPROF.eq.1)then
       tetot=H(NT1)
       NAV=1
       goto 123
      endif
       tetot=0.0
cs       WRITE(*,*)'EXT'
       do i=1,NM+1
cs       WRITE(*,*)EXT(i)
       tetot=tetot+EXT(i)
       enddo
cs       WRITE(*,*)'tetot1',tetot
cs      WRITE(*,*)'IMSC,ITRONC',IMSC,ITRONC
      if(ITRONC.eq.0) goto 122
c  MODIFICATION DE TRONCATURE 3
c     Initialiser la pointe avant
      tpointe=0.0
      do j=1,NANG1
      P11av(j)=0.0
      P12av(j)=0.0
      enddo
c On tronque a 16?(MODIFIABLE-PARAMETRER?)
      ANGTRONC=5.0
      k=NQDR
  124 continue
      k=k-1
      xx=acos(zmu(k))*180./pi
      if(xx.lt.ANGTRONC)goto 124
      JMAX=k+NQDR+1

c Boucle sur les modeles 
      do ISD=1,NM
c Allure avant du modele
      xx=PF11(JMAX,ISD)
      yy=PF11(JMAX-1,ISD)
	IF(XX.LT.1.0E-30)XX=1.0E-30
	IF(YY.LT.1.0E-30)YY=1.0E-30
      pente=(log(xx)-log(yy))/(ANGL1(JMAX)-ANGL1(JMAX-1))
      b=-pente/2/ANGL1(JMAX)
	IF(PF11(JMAX,ISD).LT.1.0E-30)PF11(JMAX,ISD)=1.0E-30
      a=log(PF11(JMAX,ISD))+b*ANGL1(JMAX)*ANGL1(JMAX)
c Matrice tronquee
      do j=1,NANG1
      if(j.lt.JMAX)TT=PF11(j,ISD)
      if(j.gt.JMAX-1)TT=exp(a-b*ANGL1(j)*ANGL1(j))
      Vav(j)=PF11(j,ISD)-TT
      Qav(j)=Vav(j)*PF12(j,ISD)/PF11(j,ISD)
      PF12(j,ISD)=PF12(j,ISD)*TT/PF11(j,ISD)
      PF22(j,ISD)=PF22(j,ISD)*TT/PF11(j,ISD)
      PF33(j,ISD)=PF33(j,ISD)*TT/PF11(j,ISD)
      PF11(j,ISD)=TT
      enddo
c     do j=1,NANG
c     write(6,*)ANGL(j),Vav(j),Qav(j),PF11(j,ISD)
c     enddo
c Re-normalisation
      zz0=0.0
      do j=-NQDR,NQDR
      k=j+NQDR+1
      zz0=zz0+0.5*PF11(k,ISD)*zg(j)
      enddo
      stron=zz0
c     write(6,*)'stron',stron
      do j=1,NANG1
      PF11(j,ISD)=PF11(j,ISD)/zz0
      PF12(j,ISD)=PF12(j,ISD)/zz0
      PF22(j,ISD)=PF22(j,ISD)/zz0
      PF33(j,ISD)=PF33(j,ISD)/zz0
      Vav(j)=Vav(j)/(1.0-zz0)
      Qav(j)=QAV(j)/(1.0-zz0)
      enddo
c     do j=1,NANG
c     write(6,*)ANGL(j),PF11(j,ISD),PF12(j,ISD),
c    & PF22(j,ISD),PF33(j,ISD)
c     enddo
c Correction des epaisseurs optiques abs et diff
      textnew=EXT(ISD)*(1-SSA(ISD)+stron*SSA(ISD))
      tdav=EXT(ISD)*SSA(ISD)*(1-stron)
      albnew=stron*SSA(ISD)/(1.0-SSA(ISD)+stron*SSA(ISD))
      EXT(ISD)=textnew
      SSA(ISD)=albnew
      tpointe=tpointe+tdav
      do j=1,NANG1
      P11av(j)=P11av(j)+tdav*Vav(j)
      P12av(j)=P12av(j)+tdav*Qav(j)
      enddo
      enddo

      tevtot=tetot
      tetot=tetot-tpointe
cs      WRITE(*,*)'tetot2,tpointe',tetot,tpointe
      do j=1,NANG1
      k=j-NQDR-1
      P11av(j)=P11av(j)/tpointe
      P12av(j)=P12av(j)/tpointe
      Q11(k)=P11av(j)
      Q12(k)=P12av(j)
      Q22(k)=0.0
      Q33(k)=0.0
      enddo
 
c Correction des mesures dans la pointe tronquee
      if(itest.eq.+1)goto 125
      ck=(exp(-tetot/rmus)-exp(-tevtot/rmus))/4.0
      call betal_1(NQDR,zmu,zg,Q11,Q12,Q22,Q33,xalp,xbet,xgam,xzet)
      do j=1,NBV
      xx=cos(thd(j)*pi/180.)
      ppri=0.0
      psec=0.0
      ptri=0.0
      p0=0.
      p1=1.
      qpri=0.0
      pp0=0.0
      pp1=3.0*(1.0-xx*xx)/2.0/sqrt(6.0)

      do k=0,2*NQDR-2
      p2=((2*k+1.)*xx*p1-k*p0)/(k+1.)
      ppri=ppri+xbet(k)*p1
      psec=psec+xbet(k)*xbet(k)*p1/(2*k+1)
      ptri=ptri+xbet(k)*xbet(k)*xbet(k)*p1/(2*k+1)/(2*k+1)
      if(k.gt.1)then
      dd=(2*k+1.)/sqrt((k+3.)*(k-1.))
      ee=sqrt((k+2.)*(k-2.))/(2*k+1.)
      pp2=dd*(xx*pp1-ee*pp0)
      qpri=qpri+xgam(k)*pp1
      qsec=qsec+xgam(k)*xgam(k)*pp1/(2*k+1)
      pp0=pp1
      pp1=pp2
      endif
      p0=p1
      p1=p2
      enddo

      yyy=tdav/2./rmus
      zzz=tdav*tdav/6./rmus/rmus
      pav=ck*(ppri+yyy*psec+zzz*ptri)/(1.0+yyy+zzz)
      qqav=-ck*(qpri+yyy*qsec)/(1+yyy)
c     write(6,*)j,pav,qqav
      DDL1(j)=pav
      DDQ1(j)=qqav
      enddo
  125 continue
c FIN DES MODIFS

  122 continue
       dtau=0.005
       NT=int(tetot/dtau)
      if(NT.lt.2)NT=2
      if(NT.gt.NT1)NT=NT1
      if(za.eq.0)KNAV=3
      if(KNAV.eq.1.or.KNAV.eq.3)za=1000.0
      call profils(NM,KNT,tetot,EXT,SSA,JP,ECH,za,NT,H,WD,NAV)
      if(KNAV.eq.1)NAV=1
      if(KNAV.eq.3)NAV=NT
  123 continue

CD Interpolation of phase matrix to the required angles:

      IF(IREAD.GE.0) THEN
       DO ISD=1,NM
        DO IA=1,NANG1
CD         ANGL1(IA)=acos(zmu(NQDR+1-IA))*180./pi
CD         ANGL1(IA)=acos(zmu(IA-NQDR-1))*180./pi
CD         F1(IA)=PF11(NANG1-IA+1,ISD)
CD         F2(IA)=PF12(NANG1-IA+1,ISD)
CD         F3(IA)=PF22(NANG1-IA+1,ISD)
CD         F4(IA)=PF33(NANG1-IA+1,ISD)
         F1(IA)=PF11(IA,ISD)
         F2(IA)=PF12(IA,ISD)
         F3(IA)=PF22(IA,ISD)
         F4(IA)=PF33(IA,ISD)
        ENDDO
        do j=-NG,NG
        tang=acos(xmu(j))*180./pi
          pha11(ISD,j)=LINEAR_LN(ANGL1,F1,NANG1,tang) 
          pha12(ISD,j)=LINEARD(ANGL1,F2,NANG1,tang)
          pha22(ISD,j)=LINEAR_LN(ANGL1,F3,NANG1,tang)
          pha33(ISD,j)=LINEARD(ANGL1,F4,NANG1,tang)
cs         WRITE(*,*)tang,pha11(ISD,j),pha12(ISD,j)
        ENDDO
       ENDDO
      ENDIF
cs      WRITE(*,*)''
c-REFLEXION PRIMAIRE
      do 130 j=1,NBV
CD    do 130 j=2,NBV
      RL1(j)=0.0
      RQ1(j)=0.0
      xxx=cos(vis(j)*pi/180.)
      if(xxx.lt.0.0)goto 131
      AT=exp(-H(NT)/rmus)*exp(-(H(NT)-H(NAV))/xxx)   
      if(isaut.eq.1) xind=0.01*iwat
      if(isaut.eq.2) xind=0.01*igrd
      xx=cos(pi*(180.-thd(j))/360.)
      yy=sqrt(xind*xind+xx*xx-1.0)
      zz=xx*xind*xind
      rl=(zz-yy)/(zz+yy)
      rr=(xx-yy)/(xx+yy)
      if(isaut.eq.1) then
       SIG =.003+.00512*wind
       z=(rmus+xxx)*(rmus+xxx)/4./xx/xx
       PPP=exp(-(1-z)/(z*SIG))/(4.0*SIG*xxx*z*z)
       RL1(j)=AT*(rmus*rsurf+0.5*PPP*(rl*rl+rr*rr))
       RQ1(j)=-AT*0.5*PPP*(rl*rl-rr*rr)
      endif
      if(isaut.eq.2) then
       AAA=(rr*rr-rl*rl)/(rmus+xxx)/2.
c      BBB=(rr*rr+rl*rl)/(rr*rr-rl*rl)
       RQ1(j)=AT*rmus*anb*(1.-exp(-bnb*AAA))
       RL1(j)=RQ1(j)+AT*rmus*RBD(rmus,xxx,fiv(j)-180.,rho,vk,gteta)
      endif
  131 continue

c-DIFFUSION PRIMAIRE
      SL1(j)=0.
      SQ1(j)=0.
cs      k=-NG-1
cs  151 k=k+1
cs      thb=acos(xmu(k))*180./pi
cs      if(thb.gt.thd(j))goto 151
cs      thh=acos(xmu(k-1))*180./pi
cs      cof=(thd(j)-thh)/(thb-thh)
cs      if(thn.gt.0)then 
cs      cof=(log(thd(j))-log(thh))/(log(thb)-log(thh))
      do 152 m=1,NM
      thd1=thd(j)
CD      CALL E02BBF(NANG+4,KS1,CS1,XARG,FIT,IFAIL)
CD      f11(m,j)=exp(FIT)
CD        WRITE(*,*) F1
          f11(m,j)=LINEAR_LN(ANGL1,F1,NANG1,thd(j))
cs          WRITE(*,*)thd(j),f11(m,j)
CD      CALL E02BBF(NANG+4,KS2,CS2,XARG,FIT,IFAIL)
CD      f12(m,j)=FIT
cs      f12(m,j)=pha12(m,k-1)+cof*(pha12(m,k)-pha12(m,k-1))
         f12(m,j)=LINEARD(ANGL1,F2,NANG1,thd(j))
  152 continue
      xx=cos(thd(j)*pi/180.)
      f11(NM+1,j)=1.0+betm*(3*xx*xx-1.)/2.
      f12(NM+1,j)=sqrt(3./8.)*(1-xx*xx)*gamm

      if(xxx.gt.0.0.and.NAV.lt.NT)then
      do n=NAV,NT-1
      xi=0.0
      xp=0.0
      do m=1,NM+1
      xi=xi+f11(m,j)*WD(n,m)
      xp=xp+f12(m,j)*WD(n,m)
      enddo
      WW=(H(n+1)-H(n))*(1/rmus+1/xxx)
      XX=(1.0-exp(-WW))/(1.0+xxx/rmus)
      YY=exp(-H(n)/rmus)*exp(-(H(n)-H(NAV))/xxx)
      SL1(j)=SL1(j)+XX*YY*xi/4.
      SQ1(j)=SQ1(j)-XX*YY*xp/4.
      enddo
      endif

      if(xxx.lt.0.0)then
      do n=1,NT-1
      xi=0.0
      xp=0.0
      do m=1,NM+1
      xi=xi+f11(m,j)*WD(n,m)
      xp=xp+f12(m,j)*WD(n,m)
      enddo
      WW=(H(n+1)-H(n))*(1/rmus+1/xxx)
      XX=(1.0-exp(-WW))/(1.0+xxx/rmus)
	  VV=(H(n+1)-H(n))/xxx
c CORRECTION ALMUCANTAR
c      if(rmus+xxx.lt.0.001)XX=(H(n+1)-H(n))/xxx
      if(abs(rmus+xxx).lt.0.001)XX=(H(n+1)-H(n))/xxx

c      YY=-exp(-H(n)/rmus)*exp((H(NT)-H(n))/xxx)
      XX=XX*exp(VV)
	  YY=-exp(-H(n)/rmus)*exp((H(NT)-H(n+1))/xxx)
      SL1(j)=SL1(j)+XX*YY*xi/4.
      SQ1(j)=SQ1(j)-XX*YY*xp/4.
      enddo
      endif
c     write(6,2345)thd(j),f11(1,j),f11(2,j),f11(3,j),SL1(j),SQ1(j),
c    &RL1(j),RQ1(j)
c2345 format(f6.1,3f10.4,4f9.6)
  130 continue
cs       WRITE(*,*)''  
CD*** turning U and Q in meridian plane ***:
      do iv=1,NBV
      SLT1(iv)=SL1(iv)+RL1(iv)
      SQT1(iv)=SQ1(iv)+RQ1(iv)
      SUT1(iv)=0
      ENDDO
      do iv=1,NBV
      SQT1os(iv)=SQT1(iv)*cos(2*chi(iv))-SUT1(iv)*sin(2*chi(iv))
      SUT1os(iv)=SQT1(iv)*sin(2*chi(iv))+SUT1(iv)*cos(2*chi(iv))
      xx=SQT1os(iv)
      yy=SUT1os(iv)
      SLP1(iv)=sqrt(xx*xx+yy*yy)
      SLP1sig(iv)=SLP1(iv)
      if(SQT1(iv).lt.0.) SLP1sig(iv)=-SLP1(iv)
      SLP1sigos(iv)=SLP1(iv)
      if(SQT1os(iv).lt.0.) SLP1sigos(iv)=-SLP1(iv)
      ENDDO
CD*** end of turning U and Q in meridian plane ***
      IF (IMSC.EQ.1) GOTO 126
c-LA DIFFUSION PRIMAIRE EST DONNEE PAR SL1(iv) ET SP1(iv)
c-DEVELOPPEMENT DES MATRICES DE PHASE
      do m=1,NM
      do k=-NG,NG
      qh11(k)=pha11(m,k)
      qh12(k)=pha12(m,k)
      qh22(k)=pha22(m,k)
      qh33(k)=pha33(m,k)
      enddo
      call betal(NG,xmu,xg,qh11,qh12,qh22,qh33,xalp,xbet,xgam,xzet)
      do k=0,2*NG-2
      alp(k,m)=xalp(k)
      bet(k,m)=xbet(k)
      gam(k,m)=xgam(k)
      zet(k,m)=xzet(k)
      enddo
      enddo
CD      WRITE(*,*) rmu(15),'rmu(15)'
c-DEBUT DES ORDRES SUCCESSIFS
      if(isaut.eq.2) call BRDF(NN,rho,vk,gteta,rer,rmu)

c****** BOUCLE SUR IS: IS=50 N'EST JAMAIS ATTEINT
CD      do 1 IS=0,50
      do 1 IS=0,100
      NMAX=NM+1
      if(IS.gt.2)NMAX=NM
      do J=-NN,NN
      QM(J)=0.
      UM(J)=0.
      IM(J)=0.
      enddo

      IF(anb.NE.anbN.OR.bnb.NE.bnbN) THEN
      anbN=anb
      bnbN=bnb
      call developpe
     &(NG,xmu,xg,isaut,wind,anb,bnb,NN,rmu,
     &R11,R12,R13,R21,R22,R23,R31,R32,R33)
CD      II=II+1
      ENDIF
CD      write(*,*) IS,' IS'
CD      WRITE(*,*) rmu(15),rmu(16),'rmu(15),rmu(16)'
CD      WRITE(*,*) rmu(15),'rmu(15)'
CD      write(6,*)(R11(12,12,k),k=0,4)
CD      WRITE(*,*) rmu(15),'rmu(15)'
c ****** DIFFUSION PRIMAIRE   
c ****** NOYAUX DE DIFFUSION DES MODES PURS  Plm(j,k) j:diffuse, k:incident
      if(IS.gt.2)goto 1204
      do k=1,NN
      do j=-NN,NN
      aj=rmu(j)
      bj=sqrt(1.-aj*aj)
      ak=rmu(k)
      bk=sqrt(1.-ak*ak)
      if(IS.eq.0)then
      P11(j,k,NM+1)=1.+0.25*betm*(3*aj*aj-1)*(3*ak*ak-1)
      P21(j,k,NM+1)=sqrt(0.09375)*gamm*bj*bj*(3*ak*ak-1)
      P22(j,k,NM+1)=0.375*alpm*bj*bk*bj*bk
      P31(j,k,NM+1)=0.0
      P32(j,k,NM+1)=0.0
      P33(j,k,NM+1)=0.0
       endif
      if(IS.eq.1)then
      P11(j,k,NM+1)=1.5*betm*aj*bj*ak*bk
      P21(j,k,NM+1)=-sqrt(0.375)*gamm*aj*bj*ak*bk
      P22(j,k,NM+1)=0.25*alpm*aj*bj*ak*bk
      P31(j,k,NM+1)=sqrt(0.375)*gamm*bj*ak*bk
      P32(j,k,NM+1)=-0.25*alpm*bj*ak*bk
      P33(j,k,NM+1)=0.25*alpm*bj*bk
       endif
      if(IS.eq.2)then
      P11(j,k,NM+1)=0.375*betm*(1.-aj*aj)*(1.-ak*ak)
      P21(j,k,NM+1)=0.25*sqrt(0.375)*gamm*(1.+aj*aj)*(1.-ak*ak)
      P22(j,k,NM+1)=0.0625*alpm*(1.+aj*aj)*(1.+ak*ak)
      P31(j,k,NM+1)=-0.5*sqrt(0.375)*gamm*aj*(1.-ak*ak)
      P32(j,k,NM+1)=-0.125*alpm*aj*(1.+ak*ak)
      P33(j,k,NM+1)=0.25*alpm*aj*ak
       endif
      enddo
      enddo
 1204 continue

      call legendre(IS,rmu,NG,NN,PSL,TSL,RSL)
CD            WRITE(*,*) rmu(15),'rmu(15)'
      do k=1,NN
      do j=-NN,NN
      do m=1,NM
      P11(j,k,m)=0.0
      P21(j,k,m)=0.0
      P22(j,k,m)=0.0
      P31(j,k,m)=0.0
      P32(j,k,m)=0.0
      P33(j,k,m)=0.0
      enddo
      do l=IS,2*NG-2
      PP=PSL(l,j)*PSL(l,k)
      RP=RSL(l,j)*PSL(l,k)
      TP=TSL(l,j)*PSL(l,k)
      TT=TSL(L,j)*TSL(L,k)
      RR=RSL(L,j)*RSL(L,k)
      TR=TSL(L,j)*RSL(L,k)
      RT=RSL(L,j)*TSL(L,k)
      do m=1,NM
      P11(j,k,m)=P11(j,k,m)+bet(l,m)*PP
      P21(j,k,m)=P21(j,k,m)+gam(l,m)*RP
      P31(j,k,m)=P31(j,k,m)-gam(l,m)*TP
      P22(j,k,m)=P22(j,k,m)+alp(l,m)*RR+zet(l,m)*TT
      P33(j,k,m)=P33(j,k,m)+alp(l,m)*TT+zet(l,m)*RR
      P32(j,k,m)=P32(j,k,m)-alp(l,m)*TR-zet(l,m)*RT
      enddo
      enddo
      enddo
      enddo

      k=NN+1
  114 k=k-1
      if(thv(k).lt.tetas)goto 114
      if(k.eq.NN.or.k.eq.1)goto 115
  115 continue
      n0=k
      cths=(rmus-rmu(n0+1))/(rmu(n0)-rmu(n0+1))
       
      do j=-NN,NN
      do m=1,NMAX
      PR11(-j,m)=P11(j,n0+1,m)+cths*(P11(j,n0,m)-P11(j,n0+1,m))
      PR21(-j,m)=P21(j,n0+1,m)+cths*(P21(j,n0,m)-P21(j,n0+1,m))
      PR31(-j,m)=-P31(j,n0+1,m)-cths*(P31(j,n0,m)-P31(j,n0+1,m))
      enddo
      enddo

      do j=-NN,NN
      do l=1,NT-1                                                             
      C1=exp(-H(l)/rmus)/4.                                                
      C2=exp(-H(l+1)/rmus)/4.                                                
      C=(C1+C2)/2.0
      SFI(l,j)=0.0
      SFQ(l,j)=0.0
      SFU(l,j)=0.0
      do m=1,NMAX
      SFI(l,j)=SFI(l,j)+C*PR11(j,m)*WD(l,m)
      SFQ(l,j)=SFQ(l,j)+C*PR21(j,m)*WD(l,m)
      SFU(l,j)=SFU(l,j)+C*PR31(j,m)*WD(l,m)
      enddo
      enddo
      enddo
                                                                                
c ***** INTEGRATION SUR TAU-DIRECTIONS UPWARD
      do 4 j=1,NN
      I1(NT,j)=0.
      Q1(NT,j)=0.
      U1(NT,j)=0.                                       
      
c Interpolation sur tetas des noyaux rer et (-mu')RIJ
      reri=rer(j,n0+1,IS)+cths*(rer(j,n0,IS)-rer(j,n0+1,IS))
      RINEW=R11(j,n0+1,IS)+cths*(R11(j,n0,IS)-R11(j,n0+1,IS))
      RQNEW=R21(j,n0+1,IS)+cths*(R21(j,n0,IS)-R21(j,n0+1,IS))
      RUNEW=R31(j,n0+1,IS)+cths*(R31(j,n0,IS)-R31(j,n0+1,IS))
      RT=EXP(-H(NT)/rmus)
      I1(NT,j)=RINEW*RT
      Q1(NT,j)=RQNEW*RT
      U1(NT,j)=RUNEW*RT
      if(isaut.eq.2) I1(NT,j)=I1(NT,j)+reri*RT*rmus
      if(isaut.eq.1.and.IS.eq.0)I1(NT,j)=I1(NT,j)+rsurf*RT*rmus
      do l=NT-1,1,-1
      F=H(l+1)-H(l)
      C=EXP(-F/RMU(j))
      I1(l,j)=C*I1(l+1,j)+(1-C)*SFI(l,j)
      Q1(l,j)=C*Q1(l+1,j)+(1-C)*SFQ(l,j)
      U1(l,j)=C*U1(l+1,j)+(1-C)*SFU(l,j)
      enddo
    4 continue                                                                  

c ***** INTEGRATION SUR TAU-DIRECTIONS DOWNWARD
      do 5 j=-NN,-1
      I1(1,j)=0.
      Q1(1,j)=0.
      U1(1,j)=0.

      do l=2,NT
      F=H(l)-H(l-1)
      C=EXP(F/RMU(j))
      I1(l,j)=C*I1(l-1,j)+(1-C)*SFI(l-1,j)
      Q1(l,j)=C*Q1(l-1,j)+(1-C)*SFQ(l-1,j)
      U1(l,j)=C*U1(l-1,j)+(1-C)*SFU(l-1,j)
      enddo
    5 continue
c Contributions de la diffusion primaire aux flux
      if(IS.eq.0)then
      do k=-NN,NN
      IZT(k)=I1(NAV,k)
      IZTN(k)=I1(NAV,k)
      IZG(k)=I1(NT,k)
      IZGN(k)=I1(NT,k)
      enddo
      endif

C ****** FIN DIFFUSION PRIMAIRE  ********                  
      ND=1                                                                      
c     goto 507
c ****** BOUCLE SUR LES DIFFUSIONS
c ****** NOYAUX DE DIFFUSION DES MELANGES  BIJ(j,k,M) j:diffuse, k:incident
      do 700 l=1,NT-1
      do 701 k=1,NN
      do 702 j=-NN,NN
      if(j.eq.0)goto 702
      B11(j,k,l)=0.0
      B21(j,k,l)=0.0
      B22(j,k,l)=0.0
      B31(j,k,l)=0.0
      B32(j,k,l)=0.0
      B33(j,k,l)=0.0
      do 703 m=1,NMAX
      B11(j,k,l)=B11(j,k,l)+P11(j,k,m)*WD(l,m)
      B21(j,k,l)=B21(j,k,l)+P21(j,k,m)*WD(l,m)
      B22(j,k,l)=B22(j,k,l)+P22(j,k,m)*WD(l,m)
      B31(j,k,l)=B31(j,k,l)+P31(j,k,m)*WD(l,m)
      B32(j,k,l)=B32(j,k,l)+P32(j,k,m)*WD(l,m)
      B33(j,k,l)=B33(j,k,l)+P33(j,k,m)*WD(l,m)
  703 continue
      B11(-j,-k,l)=B11(j,k,l)
      B21(-j,-k,l)=B21(j,k,l)
      B22(-j,-k,l)=B22(j,k,l)
      B31(-j,-k,l)=-B31(j,k,l)
      B32(-j,-k,l)=-B32(j,k,l)
      B33(-j,-k,l)=B33(j,k,l)
      
      B12(k,j,l)=B21(j,k,l)
      B13(k,j,l)=B31(j,k,l)
      B23(k,j,l)=B32(j,k,l)
      B12(-k,-j,l)=B21(-j,-k,l)
      B13(-k,-j,l)=B31(-j,-k,l)
      B23(-k,-j,l)=B32(-j,-k,l)
  702 continue
  701 continue
  700 continue

c ICI RETOUR ND
  600 ND=ND+1 
CD      IF (IMSC.GT.1) THEN
CD      IF(ND.EQ.IMSC) GOTO 507
CD      ENDIF                                                                  

      do 800 l=1,NT-1
      do 801 j=-NN,NN
      if(j.eq.0)goto 801
      X=0.
      Y=0.
      Z=0.                                 
      do 802 k=-NN,NN
      if(k.eq.0)goto 802
      U=rg(k)
      YI=(I1(l+1,k)+I1(l,k))/2.0
      YQ=(Q1(l+1,k)+Q1(l,k))/2.0
      YU=(U1(l+1,k)+U1(l,k))/2.0
      X=X+U*(B11(j,k,l)*YI+B12(j,k,l)*YQ+B13(j,k,l)*YU)
      Y=Y+U*(B21(j,k,l)*YI+B22(j,k,l)*YQ+B23(j,k,l)*YU)
      Z=Z+U*(B31(j,k,l)*YI+B32(j,k,l)*YQ+B33(j,k,l)*YU)
  802 continue                                                                  
      SFI(l,j)=X/2.0                                                               
      SFQ(l,j)=Y/2.0
      SFU(l,j)=Z/2.0
  801 continue                                                                 
  800 continue                                                                 

c ***** INTEGRATION SUR TAU-DIRECTIONS UPWARD
      do 7 j=1,NN
      I1(NT,j)=0.
      Q1(NT,j)=0.
      U1(NT,j)=0.

      do k=1,NN
      XI=I1(NT,-k)*R11(j,k,IS)+Q1(NT,-k)*R12(j,k,IS)
     &+U1(NT,-k)*R13(j,k,IS)
      XQ=I1(NT,-k)*R21(j,k,IS)+Q1(NT,-k)*R22(j,k,IS)
     &+U1(NT,-k)*R23(j,k,IS)
      XU=I1(NT,-k)*R31(j,k,IS)+Q1(NT,-k)*R32(j,k,IS)
     &+U1(NT,-k)*R33(j,k,IS)
      I1(NT,j)=I1(NT,j)+2*rg(k)*XI
      Q1(NT,j)=Q1(NT,j)+2*rg(k)*XQ
      U1(NT,j)=U1(NT,j)+2*rg(k)*XU
      xxx=2*rg(k)*I1(NT,-k)*rmu(k)
      if(isaut.eq.2)I1(NT,j)=I1(NT,j)+xxx*rer(j,k,IS)
      if(isaut.eq.1.and.IS.eq.0)I1(NT,j)=I1(NT,j)+xxx*rsurf
      enddo

      do l=NT-1,1,-1
      F=H(l+1)-H(l)
      C=EXP(-F/RMU(j))
      I1(l,j)=C*I1(l+1,j)+(1-C)*SFI(l,j)
      Q1(l,j)=C*Q1(l+1,j)+(1-C)*SFQ(l,j)
      U1(l,j)=C*U1(l+1,j)+(1-C)*SFU(l,j)
      enddo
c     write(6,*)IS,ND,I1(1,j)
    7 continue 
c     pause

c ***** INTEGRATION SUR TAU-DIRECTIONS DOWNWARD
      do 8 j=-NN,-1
      I1(1,j)=0.
      Q1(1,j)=0.
      U1(1,j)=0.

      do 80 l=2,NT
      F=H(l)-H(l-1)
      C=EXP(F/RMU(j))
      I1(l,j)=C*I1(l-1,j)+(1-C)*SFI(l-1,j)
      Q1(l,j)=C*Q1(l-1,j)+(1-C)*SFQ(l-1,j)
      U1(l,j)=C*U1(l-1,j)+(1-C)*SFU(l-1,j)
   80 continue
    8 continue

c    CALCUL DE LA SOMME DES DIFFUSIONS
      do j=1,NN
      IM(j)=IM(j)+I1(NAV,j)
      QM(j)=QM(j)+Q1(NAV,j)
      UM(j)=UM(j)+U1(NAV,j)
      IM(-j)=IM(-j)+I1(NT,-j)
      QM(-j)=QM(-j)+Q1(NT,-j)
      UM(-j)=UM(-j)+U1(NT,-j)
      enddo

      if(IS.eq.0)then
      RS=0.0
      do j=1,NN
      RS=RS+I1(NAV,j)/IZTN(j)/NN
      enddo
      do j=-NN,NN
      IZT(j)=IZT(j)+I1(NAV,j)
      IZTN(j)=I1(NAV,j)
      IZG(j)=IZG(j)+I1(NT,j)
      IZGN(j)=I1(NT,j)
      enddo
      endif

c    TEST D ARRET SUR L'ORDRE DE DIFFUSION: CAS DE IS=0
      if(ND.eq.50)then
      if(IS.eq.0)then
        do j=-NN,NN
        IZT(j)=IZT(j)+IZTN(j)*RS/(1-RS)
        IZG(j)=IZG(j)+IZGN(j)*RS/(1-RS)
        enddo
        do j=1,NN
        IM(-j)=IM(-j)+IZGN(-j)*RS/(1-RS)
        IM(j)=IM(j)+IZTN(j)*RS/(1-RS)
        enddo
        goto 507
      endif
      endif

      z=0.
      do j=1,NN
      z=max(z,abs(I1(NAV,j)))
      z=max(z,abs(Q1(NAV,j)))
      z=max(z,abs(U1(NAV,j)))
      if(itest.eq.+1)goto 128
      z=max(z,abs(I1(NT,-j)))
      z=max(z,abs(Q1(NT,-j)))
      z=max(z,abs(U1(NT,-j)))
  128 continue
      enddo
c     write(6,*)'IS',IS,'zmax',z
c     pause
      if(z.gt.0.0001) goto 600
  507 continue

c   CALCUL CUMULATIF DES SORTIES HORS DIFF.PRIM. ET CONVERGENCE EN IS
      xx=0.0
      do 30 iv=1,NBV
      k=idir(iv)
      XI=IM(k+1)+coff(iv)*(IM(k)-IM(k+1))
      xx=max(xx,abs(XI))
      XQ=QM(k+1)+coff(iv)*(QM(k)-QM(k+1))
      xx=max(xx,abs(XQ))
      XU=UM(k+1)+coff(iv)*(UM(k)-UM(k+1))
      xx=max(xx,abs(XU))
      if(IS.eq.0)then
      SL(iv)=XI
      SQos(iv)=XQ
      SUos(iv)=0.
      goto 30
      endif
      ang=IS*pi*(fiv(iv)-180.0)/180.0
      SL(iv)=SL(iv)+2*cos(ang)*XI
      SQos(iv)=SQos(iv)+2*cos(ang)*XQ
      SUos(iv)=SUos(iv)+2*sin(ang)*XU
   30 continue
c     write(6,*)'xx',xx
c     pause
      if(xx.lt.0.0001)goto 243
    1 continue   
  243 continue
      close(1)

      UFT=0.0
      UFG=0.0
      DFT=rmus*exp(-H(NAV)/rmus)
      DFG=rmus*exp(-H(NT)/rmus)
      do k=1,NN
      UFT=UFT+2*rg(k)*IZT(k)*rmu(k)
      DFT=DFT+2*rg(k)*IZT(-k)*rmu(k)
      UFG=UFG+2*rg(k)*IZG(k)*rmu(k)
      DFG=DFG+2*rg(k)*IZG(-k)*rmu(k)
      enddo

      do iv=1,NBV
      cc=cos(2*chi(iv))
      ss=sin(2*chi(iv))
      SL(iv)=SL(iv)+SL1(iv)+RL1(iv)
      SQ(iv)=cc*SQos(iv)+ss*SUos(iv)+SQ1(iv)+RQ1(iv)
      if(ITRONC.eq.1)then
      SL(iv)=SL(iv)+DDL1(iv)
      SQ(iv)=SQ(iv)+DDQ1(iv)
      endif
      SU(iv)=-ss*SQos(iv)+cc*SUos(iv)
      SQos(iv)=cc*SQ(iv)-ss*SU(iv)
      SUos(iv)=ss*SQ(iv)+cc*SU(iv)
      xx=SQ(iv)
      yy=SU(iv)
      SLP(iv)=sqrt(xx*xx+yy*yy)
      SLPsig(iv)=SLP(iv)
      if(SQ(iv).lt.0.) SLPsig(iv)=-SLP(iv)
      enddo

  126 continue
      IF(IMSC.EQ.1) THEN
       do iv=1,NBV
       SLout(iv)=SLT1(iv)
       ENDDO
       IF(iop.EQ.0) THEN
C out is scattering plance
      do iv=1,NBV
      SQout(iv)=SQT1(iv)
      SUout(iv)=SUT1(iv)
      SLPout(iv)=SLP1sig(iv)
      ENDDO
       ENDIF
       IF(iop.EQ.1) THEN
C out is meridian plance
      do iv=1,NBV
      SQout(iv)=SQT1os(iv)
      SUout(iv)=SUT1os(iv)
      SLPout(iv)=-SLP1sigos(iv)
      ENDDO
       ENDIF
      ELSE
       do iv=1,NBV
       SLout(iv)=SL(iv)
       ENDDO
      IF(iop.EQ.0) THEN
C out is scattering plance
      do iv=1,NBV
      SQout(iv)=SQ(iv)
      SUout(iv)=SU(iv)
      SLPout(iv)=SLPsig(iv)
      ENDDO
       ENDIF
       IF(iop.EQ.1) THEN
C out is meridian plance
      do iv=1,NBV
      SQout(iv)=SQos(iv)
      SUout(iv)=SUos(iv)
c     SLPout(iv)=SLPsigos(iv)
      SLPout(iv)=SLPsig(iv)
      ENDDO
       ENDIF      
      ENDIF 
CD      WRITE(*,*) rmu(15),'rmu(15) last' \
   22 FORMAT(2f12.4,i4,a20) 
   23 FORMAT(f15.10,i5,2f15.10,a42)
   24 FORMAT(5f15.10,i5,a55)    
      return
      end                                                                       

      SUBROUTINE OS_HERMAN_1(IPRI,IMSC,IPROF,KNAV,za,NT1,
     &tmol,tetas,NBV,vis,fiv,isaut,wind,
     &iwat,rsurf,igrd,vk,gteta,rho,anb,bnb,
     &NM,pha11,pha12,pha22,pha33,
     &EXT,SSA,JP,ECH,WD,H,
     &NG,NN,
     &IREAD,PF11_I,PF12_I,PF22_I,PF33_I,ANGL,NANG,NQDR,
     &ITRONC,
     &thd,iop,SLout,SQout,SUout,SLPout,
     &tetot,NAV,UFT,DFT,UFG,DFG)

      parameter(NBVM=100,NMM=5,KNT=51)
      PARAMETER(NN0=85,NG0=91)
      PARAMETER (KSD=3,KANG=2*NG0+1,KNG=NG0)
      double precision rmu,rg,xmu,xg
      double precision AMU1,PMU1,AMU2,PMU2
      REAL I1,IM,IZT,IZG,IZTN,IZGN
      INTEGER MS,LCK,LWRK,IFAIL
CD1      PARAMETER (MS=82,LCK=MS+4,LWRK=6*MS+16)
CD      PARAMETER (MS=83,LCK=MS+4,LWRK=6*MS+16)
CD      DOUBLE PRECISION KS(LCK),CS(LCK),WRK(LWRK),XS(MS),YS(MS),
CD     &YS1(MS)
CD      PARAMETER (MS0=KANG,LCK0=MS0+4,LWRK=6*MS0+16)
      DOUBLE PRECISION KS(NANG+4),KS1(NANG+4),
     &KS2(NANG+4),CS(NANG+4),CS1(NANG+4),
     &CS2(NANG+4),WRK(6*NANG+16),XS(NANG),YS(NANG),YS1(NANG),
     &KC_11(NM,NANG+4),KC_12(NM,NANG+4),
     &CS_11(NM,NANG+4),CS_12(NM,NANG+4)

CD      DOUBLE PRECISION KS(LCK0),KS1(LCK0),KS2(LCK0),CS(LCK0),CS1(LCK0),
CD     &CS2(LCK0),WRK(LWRK),XS(MS0),YS(MS0),YS1(MS0),
CD     &KC_11(KSD,LCK0),KC_12(KSD,LCK0),CS_11(KSD,LCK0),CS_12(KSD,LCK0)
      DOUBLE PRECISION XARG,FIT,XARG1
      REAL LINEARD, LINEAR_LN
c IM: MESURES: NAV-UP:1 a NN; SOL-DOWN:-NN a-1; SAUF 1
c IZT: tous ordres IS=0 de -NN a NN niveau NAV (IZT(1,NN)=0 si NAV=1)
c IZG: tous ordres IS=0 de -NN a NN niveau SOL
c IZTN,IZGN: ordre n; pour serie geometrique
      DIMENSION AMU1(-NG0:NG0),PMU1(-NG0:NG0),
     &AMU2(-NN0:NN0),PMU2(-NN0:NN0)
      dimension 
     &EXT(NMM),SSA(NMM),JP(NMM),ECH(NMM),
     &WD(KNT-1,NMM),H(KNT),
     &thv(-NN0:NN0),rg(-NN0:NN0),
     &xmu(-NG0:NG0),xg(-NG0:NG0),
     &zmu(-NG0:NG0),zg(-NG0:NG0),
     &coff(NBVM),idir(NBVM),chi(NBVM),
     &alp(0:2*NG0,NMM),bet(0:2*NG0,NMM),gam(0:2*NG0,NMM),
     &zet(0:2*NG0,NMM),
     &xalp(0:2*NG0),xbet(0:2*NG0),xgam(0:2*NG0),xzet(0:2*NG0),
     &pha11(NMM,-KNG:KNG),
     &pha12(NMM,-KNG:KNG),pha33(NMM,-KNG:KNG),pha22(NMM,-KNG:KNG),
     &qh11(-NG0:NG0),qh12(-NG0:NG0),qh33(-NG0:NG0),qh22(-NG0:NG0),
     &f11(NM+1,NBVM),f12(NM+1,NBVM),
     &rer(NN0,NN0,0:50),
     &PR11(-NN0:NN0,NMM),PR21(-NN0:NN0,NMM),PR31(-NN0:NN0,NMM),
     &P11(-NN0:NN0,NN0,NMM),P21(-NN0:NN0,NN0,NMM),P22(-NN0:NN0,NN0,NMM),
     &P31(-NN0:NN0,NN0,NMM),P32(-NN0:NN0,NN0,NMM),P33(-NN0:NN0,NN0,NMM),
     &B11(-NN0:NN0,-NN0:NN0,KNT),B12(-NN0:NN0,-NN0:NN0,KNT),
     &B13(-NN0:NN0,-NN0:NN0,KNT),B21(-NN0:NN0,-NN0:NN0,KNT),
     &B22(-NN0:NN0,-NN0:NN0,KNT),B23(-NN0:NN0,-NN0:NN0,KNT),
     &B31(-NN0:NN0,-NN0:NN0,KNT),B32(-NN0:NN0,-NN0:NN0,KNT),
     &B33(-NN0:NN0,-NN0:NN0,KNT),
     &IZT(-NN0:NN0),IZG(-NN0:NN0),IZTN(-NN0:NN0),IZGN(-NN0:NN0),
     &XIZT(-NN0:NN0),XIZG(-NN0:NN0),
     &QM(-NN0:NN0),UM(-NN0:NN0),IM(-NN0:NN0),
     &SFI(KNT-1,-NN0:NN0),SFQ(KNT-1,-NN0:NN0),SFU(KNT-1,-NN0:NN0),
     &I1(KNT,-NN0:NN0),Q1(KNT,-NN0:NN0),U1(KNT,-NN0:NN0),
     &SL(NBVM),SQos(NBVM),SUos(NBVM),SLP(NBVM),SQ(NBVM),SU(NBVM),
     &SL1(NBVM),SP1(NBVM),SLPsig(NBVM),SLPsigos(NBVM),
     &SQ1(NBVM),SU1(NBVM),RL1(NBVM),RQ1(NBVM),RU1(NBVM),
     &thd(NBVM),vis(NBVM),fiv(NBVM),
     &SLT1(NBVM),SQT1(NBVM),SUT1(NBVM),SQT1os(NBVM),
     &SUT1os(NBVM),SLP1(NBVM),SLP1sig(NBVM),SLP1sigos(NBVM),
     &SQout(NBVM),SUout(NBVM),SLPout(NBVM),SLout(NBVM),
     &R11(NN0,NN0,0:2*NG0-2),R12(NN0,NN0,0:2*NG0-2),
     &R13(NN0,NN0,0:2*NG0-2),
     &R21(NN0,NN0,0:2*NG0-2),R22(NN0,NN0,0:2*NG0-2),
     &R23(NN0,NN0,0:2*NG0-2),
     &R31(NN0,NN0,0:2*NG0-2),R32(NN0,NN0,0:2*NG0-2),
     &R33(NN0,NN0,0:2*NG0-2)
     
      dimension PF11(KANG,KSD),PF12(KANG,KSD),PF22(KANG,KSD),
     &PF33(KANG,KSD),ANGL1(KANG),
     &F1(KANG),F2(KANG),F3(KANG),F4(KANG),
     &PF11_I(NANG,KSD),PF12_I(NANG,KSD),PF22_I(NANG,KSD),
     &PF33_I(NANG,KSD),
     &ANGL(NANG)
     
      dimension rmu(-NN0:NN0),PSL(-1:2*NG0,-NN0:NN0),
     &RSL(-1:2*NG0,-NN0:NN0),TSL(-1:2*NG0,-NN0:NN0)

CD MODIFICATION SI TRONCATURE 0
      dimension P11av(KANG),P12av(KANG),Vav(KANG),Qav(KANG),
     &Q11(-NG0:NG0),Q12(-NG0:NG0),Q22(-NG0:NG0),Q33(-NG0:NG0),
     &DDL1(NBVM),DDQ1(NBVM)
cs      open(8,file="Sinyuk_input")
cs      write(8,*)WAVE,'  WAVE'
      IF(IMSC.EQ.0)THEN
      write(8,*)IPRI,'  IPRI'
      write(8,*)IMSC,'  IMSC'
      write(8,*)IPROF,'  IPROF'
      write(8,*)NBV,'  NBV'
      do iv=1,NBV
      write(8,22)vis(iv),fiv(iv),iv,' vis(IV),fiv(IV),IV'
      enddo
      write(8,*)tetas,'  tetas'
      write(8,*)isaut,'  isaut'
      write(8,*)wind,'  wind'
      write(8,*)iwat,'  iwat'
      write(8,*)rsurf,'  rsurf'
      write(8,*)igrd,'  igrd'
      write(8,*)vk,'  vk'
      write(8,*)gteta,'  gteta'
      write(8,*)rho,'  rho'
      write(8,*)anb,'  anb'
      write(8,*)bnb,'  bnb'
      write(8,*)iop,'  iop'
      write(8,*)NG,'  NG1'
      write(8,*)NN,'  NG2  '
      write(8,*)IREAD,'  IREAD'
      write(8,*)NM,'  NM'
      write(8,*)NANG,'NANG'
      write(8,*)NQDR,'NQDR'
      write(8,*)ITRONC,'ITRONC'      
      do i=1,NM
      write(8,23)EXT(i),JP(i),ECH(i),SSA(i),
     &'EXT(ISD),JP(ISD),ECH(ISD),SSA(ISD)' 
c n'importe quoi si IPROF=1
      do k=-NG,NG
      write(8,24)ANGL(NG+1+k),pha11(i,k),pha12(i,k),pha22(i,k),
     &pha33(i,k)
     &,k,
     &' ANGL,pha11(ISD,J),pha12(ISD,J),pha22(ISD,J),pha33(ISD,J),J,'
      enddo
      enddo
      write(8,23)EXT(NM+1),JP(NM+1),ECH(NM+1),SSA(NM+1)
     &,'EXT(ISD),JP(ISD),ECH(ISD),SSA(ISD)'
      write(8,*)NT,'  NT'
      write(8,*)KNAV,za,NAV,'KNAV,za,NAV'
      ENDIF

      pi = 3.141592653
      rmus=cos(pi*tetas/180.0)

      ron=0.0279
CD      ron=0.014
CD      ron=0.03
CD      ron=0
      aaa=2*(1-ron)/(2+ron)
      betm=0.5*aaa
      gamm=-aaa*sqrt(1.5)
      alpm=3*aaa
C******removing polarization!!!*****
CD      gamm=0
CD      alpm=0
C******removing polarization!!!*****

c     ITRONC=0
c     if(ITRONC.eq.1) call GAUSS_2(NQDR,zmu,zg)
      call GAUSS_2(NQDR,zmu,zg)
C CORRIGER D'ABORD LA MATRICE PFij?
C Tres peu d'influence. Modifie de qqs 10-3 en relatif
      IF(IREAD.GE.0) THEN
       LCK=NANG+4
       DO I=1,4
        DO ISD=1,NM
         DO IA=1,NANG
         IF(I.EQ.1) THEN
          IF(ANGL(2).GT.ANGL(1)) THEN
           XS(IA)=ANGL(IA)
		 IF(PF11_I(IA,ISD).LT.1.0E-30)PF11_I(IA,ISD)=1.0E-30
           YS(IA)=log(PF11_I(IA,ISD))
          ELSE
           XS(IA)=ANGL(NANG-IA+1)
		 IF(PF11_I(NANG-IA+1,ISD).LT.1.0E-30)PF11_I(NANG-IA+1,ISD)=1.0E-30
           YS(IA)=log(PF11_I(NANG-IA+1,ISD))
          ENDIF
         ENDIF
         IF(ANGL(2).GT.ANGL(1)) THEN
          IF(I.EQ.2) YS(IA)=PF12_I(IA,ISD)
          IF(I.EQ.3) THEN
			IF(PF22_I(IA,ISD).LT.1.0E-30)PF22_I(IA,ISD)=1.0E-30
			YS(IA)=log(PF22_I(IA,ISD))
	    ENDIF
          IF(I.EQ.4) YS(IA)=PF33_I(IA,ISD)
         ELSE
          IF(I.EQ.2) YS(IA)=PF12_I(NANG-IA+1,ISD)
          IF(I.EQ.3) THEN
		 IF(PF22_I(NANG-IA+1,ISD).LT.1.0E-30)PF22_I(NANG-IA+1,ISD)=1.0E-30
		 YS(IA)=log(PF22_I(NANG-IA+1,ISD))
	    ENDIF
          IF(I.EQ.4) YS(IA)=PF33_I(NANG-IA+1,ISD)
         ENDIF
         ENDDO
        CALL E01BAF(NANG,XS,YS,KS,CS,LCK,WRK,6*NANG+16,IFAIL)
         IF(I.EQ.1) THEN
          DO II=1,LCK
           KC_11(ISD,II)=KS(II)
           CS_11(ISD,II)=CS(II)
         ENDDO
         ENDIF
         IF(I.EQ.2) THEN
          DO II=1,LCK
           KC_12(ISD,II)=KS(II)
           CS_12(ISD,II)=CS(II)
          ENDDO
          ENDIF
CD        do j=-NG,NG
        do j=-NQDR,NQDR
        IFAIL=0
        XARG=acos(zmu(j))*180./pi
        IF(XARG.GT.180) XARG=180.0
        ANGL1(NQDR+j+1)=XARG
        CALL E02BBF(NANG+4,KS,CS,XARG,FIT,IFAIL)
        IF(I.EQ.1) PF11(NQDR+j+1,ISD)=exp(FIT)
        IF(I.EQ.2) PF12(NQDR+j+1,ISD)=FIT
        IF(I.EQ.3) PF22(NQDR+j+1,ISD)=exp(FIT)
        IF(I.EQ.4) PF33(NQDR+j+1,ISD)=FIT
         ENDDO
        ENDDO
       ENDDO ! DO=I=1,4
      ENDIF
cs      do 
cs      do i=-NQDR,NQDR
cs      xx=acos(zmu(i))*180./pi
cs      WRITE(*,*)xx
cs      enddo
cs      WRITE(*,*)''
      NANG1=2*NQDR+1
      do ISD=1,NM
      zz0=0.0
      do j=-NQDR,NQDR
      k=NQDR+j+1
      zz0=zz0+0.5*PF11(k,ISD)*zg(j)
      enddo
      stron=zz0
c     write(6,*)'stron',stron
      do k=1,NANG1
      xxx=PF11(k,ISD)/zz0
      PF12(k,ISD)=PF12(k,ISD)*xxx/PF11(k,ISD)
      PF22(k,ISD)=PF22(k,ISD)*xxx/PF11(k,ISD)
      PF33(k,ISD)=PF33(k,ISD)*xxx/PF11(k,ISD)
      PF11(k,ISD)=xxx
      enddo
      enddo

cs      IF (III.LT.1) THEN
c MODIFICATION
      call gauss(NG,xmu,xg)
      call gauss_1(NN,rmu,rg)
cs      write(*,*)'xmu'
      do j=-NG,NG
cs      write(*,*)acos(xmu(j))*180./pi,j
CD      WRITE(*,*) thv(j),j
      enddo
cs      write(*,*)'rmu'      
      do j=-NN,NN
      thv(j)=acos(rmu(j))*180./pi
cs      WRITE(*,*) thv(j),j
      enddo

CD              do j=-NG,NG
CD            tang=acos(xmu(j))*180./pi
CD              WRITE(*,*) tang,J
CD              enddo

cs      III=III+1
cs      ENDIF
CD      WRITE(*,*) xmu,'xmu'
CD      WRITE(*,*) rmu,'rmu'
CD      WRITE(*,*) xg,'xg'
CD      WRITE(*,*) rg,'rg'
CD      WRITE(*,*) thv,'thv'
CD      WRITE(*,*) rmu(15),'rmu(15)'
CD      write(6,*)'NT lu',NT
CD      write(6,*)'H(NT) lu', H(NT)

c-GEOMETRIE
      itest=+1
      do iv=1,NBV
      chi(iv)=ANGTURN(tetas,vis(iv),fiv(iv))
      xx=cos(vis(iv)*pi/180.)
      if (itest.eq.+1.and.xx.lt.0.0) itest=-1
      zz=-rmus*xx-sqrt(1-rmus*rmus)*sqrt(1-xx*xx)*cos(fiv(iv)*pi/180.)
      thd(iv)=acos(zz)*180./pi
      k=NN
  121 k=k-1
      if(thv(k).lt.vis(iv))goto 121
      idir(iv)=k
      coff(iv)=(vis(iv)-thv(k+1))/(thv(k)-thv(k+1))
      enddo

      if(IPROF.eq.1)then
       tetot=H(NT1)
       NAV=1
       goto 123
      endif
       tetot=0.0
       do i=1,NM+1
       tetot=tetot+EXT(i)
       enddo
cs      WRITE(*,*)'IMSC,ITRONC',IMSC,ITRONC
      if(ITRONC.eq.0) goto 122
c  MODIFICATION DE TRONCATURE 3
c     Initialiser la pointe avant
      tpointe=0.0
      do j=1,NANG1
      P11av(j)=0.0
      P12av(j)=0.0
      enddo
c On tronque a 16?(MODIFIABLE-PARAMETRER?)
      ANGTRONC=16.0
      k=NQDR
  124 continue
      k=k-1
      xx=acos(zmu(k))*180./pi
      if(xx.lt.ANGTRONC)goto 124
      JMAX=k+NQDR+1

c Boucle sur les modeles 
      do ISD=1,NM
c Allure avant du modele
      xx=PF11(JMAX,ISD)
      yy=PF11(JMAX-1,ISD)
	IF(XX.LT.1.0E-30)XX=1.0E-30
	IF(YY.LT.1.0E-30)YY=1.0E-30
      pente=(log(xx)-log(yy))/(ANGL1(JMAX)-ANGL1(JMAX-1))
      b=-pente/2/ANGL1(JMAX)
	IF(PF11(JMAX,ISD).LT.1.0E-30)PF11(JMAX,ISD)=1.0E-30
      a=log(PF11(JMAX,ISD))+b*ANGL1(JMAX)*ANGL1(JMAX)
c Matrice tronquee
      do j=1,NANG1
      if(j.lt.JMAX)TT=PF11(j,ISD)
      if(j.gt.JMAX-1)TT=exp(a-b*ANGL1(j)*ANGL1(j))
      Vav(j)=PF11(j,ISD)-TT
      Qav(j)=Vav(j)*PF12(j,ISD)/PF11(j,ISD)
      PF12(j,ISD)=PF12(j,ISD)*TT/PF11(j,ISD)
      PF22(j,ISD)=PF22(j,ISD)*TT/PF11(j,ISD)
      PF33(j,ISD)=PF33(j,ISD)*TT/PF11(j,ISD)
      PF11(j,ISD)=TT
      enddo
c     do j=1,NANG
c     write(6,*)ANGL(j),Vav(j),Qav(j),PF11(j,ISD)
c     enddo
c Re-normalisation
      zz0=0.0
      do j=-NQDR,NQDR
      k=j+NQDR+1
      zz0=zz0+0.5*PF11(k,ISD)*zg(j)
      enddo
      stron=zz0
c     write(6,*)'stron',stron
      do j=1,NANG1
      PF11(j,ISD)=PF11(j,ISD)/zz0
      PF12(j,ISD)=PF12(j,ISD)/zz0
      PF22(j,ISD)=PF22(j,ISD)/zz0
      PF33(j,ISD)=PF33(j,ISD)/zz0
      Vav(j)=Vav(j)/(1.0-zz0)
      Qav(j)=QAV(j)/(1.0-zz0)
      enddo
c     do j=1,NANG
c     write(6,*)ANGL(j),PF11(j,ISD),PF12(j,ISD),
c    & PF22(j,ISD),PF33(j,ISD)
c     enddo
c Correction des epaisseurs optiques abs et diff
      textnew=EXT(ISD)*(1-SSA(ISD)+stron*SSA(ISD))
      tdav=EXT(ISD)*SSA(ISD)*(1-stron)
      albnew=stron*SSA(ISD)/(1.0-SSA(ISD)+stron*SSA(ISD))
      EXT(ISD)=textnew
      SSA(ISD)=albnew
      tpointe=tpointe+tdav
      do j=1,NANG1
      P11av(j)=P11av(j)+tdav*Vav(j)
      P12av(j)=P12av(j)+tdav*Qav(j)
      enddo
      enddo

      tevtot=tetot
      tetot=tetot-tpointe
      do j=1,NANG1
      k=j-NQDR-1
      P11av(j)=P11av(j)/tpointe
      P12av(j)=P12av(j)/tpointe
      Q11(k)=P11av(j)
      Q12(k)=P12av(j)
      Q22(k)=0.0
      Q33(k)=0.0
      enddo
 
c Correction des mesures dans la pointe tronquee
      if(itest.eq.+1)goto 125
      ck=(exp(-tetot/rmus)-exp(-tevtot/rmus))/4.0
      call betal_1(NQDR,zmu,zg,Q11,Q12,Q22,Q33,xalp,xbet,xgam,xzet)
      do j=1,NBV
      xx=cos(thd(j)*pi/180.)
      ppri=0.0
      psec=0.0
      ptri=0.0
      p0=0.
      p1=1.
      qpri=0.0
      pp0=0.0
      pp1=3.0*(1.0-xx*xx)/2.0/sqrt(6.0)

      do k=0,2*NQDR-2
      p2=((2*k+1.)*xx*p1-k*p0)/(k+1.)
      ppri=ppri+xbet(k)*p1
      psec=psec+xbet(k)*xbet(k)*p1/(2*k+1)
      ptri=ptri+xbet(k)*xbet(k)*xbet(k)*p1/(2*k+1)/(2*k+1)
      if(k.gt.1)then
      dd=(2*k+1.)/sqrt((k+3.)*(k-1.))
      ee=sqrt((k+2.)*(k-2.))/(2*k+1.)
      pp2=dd*(xx*pp1-ee*pp0)
      qpri=qpri+xgam(k)*pp1
      qsec=qsec+xgam(k)*xgam(k)*pp1/(2*k+1)
      pp0=pp1
      pp1=pp2
      endif
      p0=p1
      p1=p2
      enddo

      yyy=tdav/2./rmus
      zzz=tdav*tdav/6./rmus/rmus
      pav=ck*(ppri+yyy*psec+zzz*ptri)/(1.0+yyy+zzz)
      qqav=-ck*(qpri+yyy*qsec)/(1+yyy)
c     write(6,*)j,pav,qqav
      DDL1(j)=pav
      DDQ1(j)=qqav
      enddo
  125 continue
c FIN DES MODIFS

  122 continue
       dtau=0.001
       NT=int(tetot/dtau)
      if(NT.lt.2)NT=2
      if(NT.gt.NT1)NT=NT1
      if(za.eq.0)KNAV=3
      if(KNAV.eq.1.or.KNAV.eq.3)za=1000.0
      call profils(NM,KNT,tetot,EXT,SSA,JP,ECH,za,NT,H,WD,NAV)
      if(KNAV.eq.1)NAV=1
      if(KNAV.eq.3)NAV=NT
  123 continue

CD Interpolation of phase matrix to the required angles:

      IF(IREAD.GE.0) THEN
       DO ISD=1,NM
        DO IA=1,NANG1
CD         ANGL1(IA)=acos(zmu(NQDR+1-IA))*180./pi
CD         ANGL1(IA)=acos(zmu(IA-NQDR-1))*180./pi
CD         F1(IA)=PF11(NANG1-IA+1,ISD)
CD         F2(IA)=PF12(NANG1-IA+1,ISD)
CD         F3(IA)=PF22(NANG1-IA+1,ISD)
CD         F4(IA)=PF33(NANG1-IA+1,ISD)
         F1(IA)=PF11(IA,ISD)
         F2(IA)=PF12(IA,ISD)
         F3(IA)=PF22(IA,ISD)
         F4(IA)=PF33(IA,ISD)
        ENDDO
        do j=-NG,NG
        tang=acos(xmu(j))*180./pi
          pha11(ISD,j)=LINEAR_LN(ANGL1,F1,NANG1,tang) 
          pha12(ISD,j)=LINEARD(ANGL1,F2,NANG1,tang)
          pha22(ISD,j)=LINEAR_LN(ANGL1,F3,NANG1,tang)
          pha33(ISD,j)=LINEARD(ANGL1,F4,NANG1,tang)
cs         WRITE(*,*)tang,pha11(ISD,j),pha12(ISD,j)
        ENDDO
       ENDDO
      ENDIF
cs      WRITE(*,*)''
c-REFLEXION PRIMAIRE
      do 130 j=1,NBV
CD    do 130 j=2,NBV
      RL1(j)=0.0
      RQ1(j)=0.0
      xxx=cos(vis(j)*pi/180.)
      if(xxx.lt.0.0)goto 131
      AT=exp(-H(NT)/rmus)*exp(-(H(NT)-H(NAV))/xxx)   
      if(isaut.eq.1) xind=0.01*iwat
      if(isaut.eq.2) xind=0.01*igrd
      xx=cos(pi*(180.-thd(j))/360.)
      yy=sqrt(xind*xind+xx*xx-1.0)
      zz=xx*xind*xind
      rl=(zz-yy)/(zz+yy)
      rr=(xx-yy)/(xx+yy)
      if(isaut.eq.1) then
       SIG =.003+.00512*wind
       z=(rmus+xxx)*(rmus+xxx)/4./xx/xx
       PPP=exp(-(1-z)/(z*SIG))/(4.0*SIG*xxx*z*z)
       RL1(j)=AT*(rmus*rsurf+0.5*PPP*(rl*rl+rr*rr))
       RQ1(j)=-AT*0.5*PPP*(rl*rl-rr*rr)
      endif
      if(isaut.eq.2) then
       AAA=(rr*rr-rl*rl)/(rmus+xxx)/2.
c      BBB=(rr*rr+rl*rl)/(rr*rr-rl*rl)
       RQ1(j)=AT*rmus*anb*(1.-exp(-bnb*AAA))
       RL1(j)=RQ1(j)+AT*rmus*RBD(rmus,xxx,fiv(j)-180.,rho,vk,gteta)
      endif
  131 continue

c-DIFFUSION PRIMAIRE
      SL1(j)=0.
      SQ1(j)=0.
cs      k=-NG-1
cs  151 k=k+1
cs      thb=acos(xmu(k))*180./pi
cs      if(thb.gt.thd(j))goto 151
cs      thh=acos(xmu(k-1))*180./pi
cs      cof=(thd(j)-thh)/(thb-thh)
cs      if(thn.gt.0)then 
cs      cof=(log(thd(j))-log(thh))/(log(thb)-log(thh))
      do 152 m=1,NM
      thd1=thd(j)
CD      CALL E02BBF(NANG+4,KS1,CS1,XARG,FIT,IFAIL)
CD      f11(m,j)=exp(FIT)
CD        WRITE(*,*) F1
          f11(m,j)=LINEAR_LN(ANGL1,F1,NANG1,thd(j))
cs          WRITE(*,*)thd(j),f11(m,j)
CD      CALL E02BBF(NANG+4,KS2,CS2,XARG,FIT,IFAIL)
CD      f12(m,j)=FIT
cs      f12(m,j)=pha12(m,k-1)+cof*(pha12(m,k)-pha12(m,k-1))
         f12(m,j)=LINEARD(ANGL1,F2,NANG1,thd(j))
  152 continue
      xx=cos(thd(j)*pi/180.)
      f11(NM+1,j)=1.0+betm*(3*xx*xx-1.)/2.
      f12(NM+1,j)=sqrt(3./8.)*(1-xx*xx)*gamm

      if(xxx.gt.0.0.and.NAV.lt.NT)then
      do n=NAV,NT-1
      xi=0.0
      xp=0.0
      do m=1,NM+1
      xi=xi+f11(m,j)*WD(n,m)
      xp=xp+f12(m,j)*WD(n,m)
      enddo
      WW=(H(n+1)-H(n))*(1/rmus+1/xxx)
      XX=(1.0-exp(-WW))/(1.0+xxx/rmus)
      YY=exp(-H(n)/rmus)*exp(-(H(n)-H(NAV))/xxx)
      SL1(j)=SL1(j)+XX*YY*xi/4.
      SQ1(j)=SQ1(j)-XX*YY*xp/4.
      enddo
      endif

      if(xxx.lt.0.0)then
      do n=1,NT-1
      xi=0.0
      xp=0.0
      do m=1,NM+1
      xi=xi+f11(m,j)*WD(n,m)
      xp=xp+f12(m,j)*WD(n,m)
      enddo
      WW=(H(n+1)-H(n))*(1/rmus+1/xxx)
      XX=(1.0-exp(-WW))/(1.0+xxx/rmus)
!**OD&TL
      VV=(H(n+1)-H(n))/xxx
c CORRECTION ALMUCANTAR
!OD&TL      if(rmus+xxx.lt.0.001)XX=(H(n+1)-H(n))/xxx
      if(abs(rmus+xxx).lt.0.001)XX=(H(n+1)-H(n))/xxx
!OD&TL      YY=-exp(-H(n)/rmus)*exp((H(NT)-H(n))/xxx)
      XX=XX*exp(VV)
      YY=-exp(-H(n)/rmus)*exp((H(NT)-H(n+1))/xxx)
	  
      SL1(j)=SL1(j)+XX*YY*xi/4.
      SQ1(j)=SQ1(j)-XX*YY*xp/4.
      enddo
      endif
c     write(6,2345)thd(j),f11(1,j),f11(2,j),f11(3,j),SL1(j),SQ1(j),
c    &RL1(j),RQ1(j)
c2345 format(f6.1,3f10.4,4f9.6)
  130 continue
cs       WRITE(*,*)''  
CD*** turning U and Q in meridian plane ***:
      do iv=1,NBV
      SLT1(iv)=SL1(iv)+RL1(iv)
      SQT1(iv)=SQ1(iv)+RQ1(iv)
      SUT1(iv)=0
      ENDDO
      do iv=1,NBV
      SQT1os(iv)=SQT1(iv)*cos(2*chi(iv))-SUT1(iv)*sin(2*chi(iv))
      SUT1os(iv)=SQT1(iv)*sin(2*chi(iv))+SUT1(iv)*cos(2*chi(iv))
      xx=SQT1os(iv)
      yy=SUT1os(iv)
      SLP1(iv)=sqrt(xx*xx+yy*yy)
      SLP1sig(iv)=SLP1(iv)
      if(SQT1(iv).lt.0.) SLP1sig(iv)=-SLP1(iv)
      SLP1sigos(iv)=SLP1(iv)
      if(SQT1os(iv).lt.0.) SLP1sigos(iv)=-SLP1(iv)
      ENDDO
CD*** end of turning U and Q in meridian plane ***
      IF (IMSC.EQ.1) GOTO 126
c-LA DIFFUSION PRIMAIRE EST DONNEE PAR SL1(iv) ET SP1(iv)
c-DEVELOPPEMENT DES MATRICES DE PHASE
      do m=1,NM
      do k=-NG,NG
      qh11(k)=pha11(m,k)
      qh12(k)=pha12(m,k)
      qh22(k)=pha22(m,k)
      qh33(k)=pha33(m,k)
      enddo
      call betal(NG,xmu,xg,qh11,qh12,qh22,qh33,xalp,xbet,xgam,xzet)
      do k=0,2*NG-2
      alp(k,m)=xalp(k)
      bet(k,m)=xbet(k)
      gam(k,m)=xgam(k)
      zet(k,m)=xzet(k)
      enddo
      enddo
CD      WRITE(*,*) rmu(15),'rmu(15)'
c-DEBUT DES ORDRES SUCCESSIFS
      if(isaut.eq.2) call BRDF(NN,rho,vk,gteta,rer,rmu)

c****** BOUCLE SUR IS: IS=50 N'EST JAMAIS ATTEINT
CD      do 1 IS=0,50
      do 1 IS=0,100
      NMAX=NM+1
      if(IS.gt.2)NMAX=NM
      do J=-NN,NN
      QM(J)=0.
      UM(J)=0.
      IM(J)=0.
      enddo

      IF(anb.NE.anbN.OR.bnb.NE.bnbN) THEN
      anbN=anb
      bnbN=bnb
      call developpe
     &(NG,xmu,xg,isaut,wind,anb,bnb,NN,rmu,
     &R11,R12,R13,R21,R22,R23,R31,R32,R33)
CD      II=II+1
      ENDIF
CD      write(*,*) IS,' IS'
CD      WRITE(*,*) rmu(15),rmu(16),'rmu(15),rmu(16)'
CD      WRITE(*,*) rmu(15),'rmu(15)'
CD      write(6,*)(R11(12,12,k),k=0,4)
CD      WRITE(*,*) rmu(15),'rmu(15)'
c ****** DIFFUSION PRIMAIRE   
c ****** NOYAUX DE DIFFUSION DES MODES PURS  Plm(j,k) j:diffuse, k:incident
      if(IS.gt.2)goto 1204
      do k=1,NN
      do j=-NN,NN
      aj=rmu(j)
      bj=sqrt(1.-aj*aj)
      ak=rmu(k)
      bk=sqrt(1.-ak*ak)
      if(IS.eq.0)then
      P11(j,k,NM+1)=1.+0.25*betm*(3*aj*aj-1)*(3*ak*ak-1)
      P21(j,k,NM+1)=sqrt(0.09375)*gamm*bj*bj*(3*ak*ak-1)
      P22(j,k,NM+1)=0.375*alpm*bj*bk*bj*bk
      P31(j,k,NM+1)=0.0
      P32(j,k,NM+1)=0.0
      P33(j,k,NM+1)=0.0
       endif
      if(IS.eq.1)then
      P11(j,k,NM+1)=1.5*betm*aj*bj*ak*bk
      P21(j,k,NM+1)=-sqrt(0.375)*gamm*aj*bj*ak*bk
      P22(j,k,NM+1)=0.25*alpm*aj*bj*ak*bk
      P31(j,k,NM+1)=sqrt(0.375)*gamm*bj*ak*bk
      P32(j,k,NM+1)=-0.25*alpm*bj*ak*bk
      P33(j,k,NM+1)=0.25*alpm*bj*bk
       endif
      if(IS.eq.2)then
      P11(j,k,NM+1)=0.375*betm*(1.-aj*aj)*(1.-ak*ak)
      P21(j,k,NM+1)=0.25*sqrt(0.375)*gamm*(1.+aj*aj)*(1.-ak*ak)
      P22(j,k,NM+1)=0.0625*alpm*(1.+aj*aj)*(1.+ak*ak)
      P31(j,k,NM+1)=-0.5*sqrt(0.375)*gamm*aj*(1.-ak*ak)
      P32(j,k,NM+1)=-0.125*alpm*aj*(1.+ak*ak)
      P33(j,k,NM+1)=0.25*alpm*aj*ak
       endif
      enddo
      enddo
 1204 continue

      call legendre(IS,rmu,NG,NN,PSL,TSL,RSL)
CD            WRITE(*,*) rmu(15),'rmu(15)'
      do k=1,NN
      do j=-NN,NN
      do m=1,NM
      P11(j,k,m)=0.0
      P21(j,k,m)=0.0
      P22(j,k,m)=0.0
      P31(j,k,m)=0.0
      P32(j,k,m)=0.0
      P33(j,k,m)=0.0
      enddo
      do l=IS,2*NG-2
      PP=PSL(l,j)*PSL(l,k)
      RP=RSL(l,j)*PSL(l,k)
      TP=TSL(l,j)*PSL(l,k)
      TT=TSL(L,j)*TSL(L,k)
      RR=RSL(L,j)*RSL(L,k)
      TR=TSL(L,j)*RSL(L,k)
      RT=RSL(L,j)*TSL(L,k)
      do m=1,NM
      P11(j,k,m)=P11(j,k,m)+bet(l,m)*PP
      P21(j,k,m)=P21(j,k,m)+gam(l,m)*RP
      P31(j,k,m)=P31(j,k,m)-gam(l,m)*TP
      P22(j,k,m)=P22(j,k,m)+alp(l,m)*RR+zet(l,m)*TT
      P33(j,k,m)=P33(j,k,m)+alp(l,m)*TT+zet(l,m)*RR
      P32(j,k,m)=P32(j,k,m)-alp(l,m)*TR-zet(l,m)*RT
      enddo
      enddo
      enddo
      enddo

      k=NN+1
  114 k=k-1
      if(thv(k).lt.tetas)goto 114
      if(k.eq.NN.or.k.eq.1)goto 115
  115 continue
      n0=k
      cths=(rmus-rmu(n0+1))/(rmu(n0)-rmu(n0+1))
       
      do j=-NN,NN
      do m=1,NMAX
      PR11(-j,m)=P11(j,n0+1,m)+cths*(P11(j,n0,m)-P11(j,n0+1,m))
      PR21(-j,m)=P21(j,n0+1,m)+cths*(P21(j,n0,m)-P21(j,n0+1,m))
      PR31(-j,m)=-P31(j,n0+1,m)-cths*(P31(j,n0,m)-P31(j,n0+1,m))
      enddo
      enddo

      do j=-NN,NN
      do l=1,NT-1                                                             
      C1=exp(-H(l)/rmus)/4.                                                
      C2=exp(-H(l+1)/rmus)/4.                                                
      C=(C1+C2)/2.0
      SFI(l,j)=0.0
      SFQ(l,j)=0.0
      SFU(l,j)=0.0
      do m=1,NMAX
      SFI(l,j)=SFI(l,j)+C*PR11(j,m)*WD(l,m)
      SFQ(l,j)=SFQ(l,j)+C*PR21(j,m)*WD(l,m)
      SFU(l,j)=SFU(l,j)+C*PR31(j,m)*WD(l,m)
      enddo
      enddo
      enddo
                                                                                
c ***** INTEGRATION SUR TAU-DIRECTIONS UPWARD
      do 4 j=1,NN
      I1(NT,j)=0.
      Q1(NT,j)=0.
      U1(NT,j)=0.                                       
      
c Interpolation sur tetas des noyaux rer et (-mu')RIJ
      reri=rer(j,n0+1,IS)+cths*(rer(j,n0,IS)-rer(j,n0+1,IS))
      RINEW=R11(j,n0+1,IS)+cths*(R11(j,n0,IS)-R11(j,n0+1,IS))
      RQNEW=R21(j,n0+1,IS)+cths*(R21(j,n0,IS)-R21(j,n0+1,IS))
      RUNEW=R31(j,n0+1,IS)+cths*(R31(j,n0,IS)-R31(j,n0+1,IS))
      RT=EXP(-H(NT)/rmus)
      I1(NT,j)=RINEW*RT
      Q1(NT,j)=RQNEW*RT
      U1(NT,j)=RUNEW*RT
      if(isaut.eq.2) I1(NT,j)=I1(NT,j)+reri*RT*rmus
      if(isaut.eq.1.and.IS.eq.0)I1(NT,j)=I1(NT,j)+rsurf*RT*rmus
      do l=NT-1,1,-1
      F=H(l+1)-H(l)
      C=EXP(-F/RMU(j))
      I1(l,j)=C*I1(l+1,j)+(1-C)*SFI(l,j)
      Q1(l,j)=C*Q1(l+1,j)+(1-C)*SFQ(l,j)
      U1(l,j)=C*U1(l+1,j)+(1-C)*SFU(l,j)
      enddo
    4 continue                                                                  

c ***** INTEGRATION SUR TAU-DIRECTIONS DOWNWARD
      do 5 j=-NN,-1
      I1(1,j)=0.
      Q1(1,j)=0.
      U1(1,j)=0.

      do l=2,NT
      F=H(l)-H(l-1)
      C=EXP(F/RMU(j))
      I1(l,j)=C*I1(l-1,j)+(1-C)*SFI(l-1,j)
      Q1(l,j)=C*Q1(l-1,j)+(1-C)*SFQ(l-1,j)
      U1(l,j)=C*U1(l-1,j)+(1-C)*SFU(l-1,j)
      enddo
    5 continue
c Contributions de la diffusion primaire aux flux
      if(IS.eq.0)then
      do k=-NN,NN
      IZT(k)=I1(NAV,k)
      IZTN(k)=I1(NAV,k)
      IZG(k)=I1(NT,k)
      IZGN(k)=I1(NT,k)
      enddo
      endif

C ****** FIN DIFFUSION PRIMAIRE  ********                  
      ND=1                                                                      
c     goto 507
c ****** BOUCLE SUR LES DIFFUSIONS
c ****** NOYAUX DE DIFFUSION DES MELANGES  BIJ(j,k,M) j:diffuse, k:incident
      do 700 l=1,NT-1
      do 701 k=1,NN
      do 702 j=-NN,NN
      if(j.eq.0)goto 702
      B11(j,k,l)=0.0
      B21(j,k,l)=0.0
      B22(j,k,l)=0.0
      B31(j,k,l)=0.0
      B32(j,k,l)=0.0
      B33(j,k,l)=0.0
      do 703 m=1,NMAX
      B11(j,k,l)=B11(j,k,l)+P11(j,k,m)*WD(l,m)
      B21(j,k,l)=B21(j,k,l)+P21(j,k,m)*WD(l,m)
      B22(j,k,l)=B22(j,k,l)+P22(j,k,m)*WD(l,m)
      B31(j,k,l)=B31(j,k,l)+P31(j,k,m)*WD(l,m)
      B32(j,k,l)=B32(j,k,l)+P32(j,k,m)*WD(l,m)
      B33(j,k,l)=B33(j,k,l)+P33(j,k,m)*WD(l,m)
  703 continue
      B11(-j,-k,l)=B11(j,k,l)
      B21(-j,-k,l)=B21(j,k,l)
      B22(-j,-k,l)=B22(j,k,l)
      B31(-j,-k,l)=-B31(j,k,l)
      B32(-j,-k,l)=-B32(j,k,l)
      B33(-j,-k,l)=B33(j,k,l)
      
      B12(k,j,l)=B21(j,k,l)
      B13(k,j,l)=B31(j,k,l)
      B23(k,j,l)=B32(j,k,l)
      B12(-k,-j,l)=B21(-j,-k,l)
      B13(-k,-j,l)=B31(-j,-k,l)
      B23(-k,-j,l)=B32(-j,-k,l)
  702 continue
  701 continue
  700 continue

c ICI RETOUR ND
  600 ND=ND+1 
CD      IF (IMSC.GT.1) THEN
CD      IF(ND.EQ.IMSC) GOTO 507
CD      ENDIF                                                                  

      do 800 l=1,NT-1
      do 801 j=-NN,NN
      if(j.eq.0)goto 801
      X=0.
      Y=0.
      Z=0.                                 
      do 802 k=-NN,NN
      if(k.eq.0)goto 802
      U=rg(k)
      YI=(I1(l+1,k)+I1(l,k))/2.0
      YQ=(Q1(l+1,k)+Q1(l,k))/2.0
      YU=(U1(l+1,k)+U1(l,k))/2.0
      X=X+U*(B11(j,k,l)*YI+B12(j,k,l)*YQ+B13(j,k,l)*YU)
      Y=Y+U*(B21(j,k,l)*YI+B22(j,k,l)*YQ+B23(j,k,l)*YU)
      Z=Z+U*(B31(j,k,l)*YI+B32(j,k,l)*YQ+B33(j,k,l)*YU)
  802 continue                                                                  
      SFI(l,j)=X/2.0                                                               
      SFQ(l,j)=Y/2.0
      SFU(l,j)=Z/2.0
  801 continue                                                                 
  800 continue                                                                 

c ***** INTEGRATION SUR TAU-DIRECTIONS UPWARD
      do 7 j=1,NN
      I1(NT,j)=0.
      Q1(NT,j)=0.
      U1(NT,j)=0.

      do k=1,NN
      XI=I1(NT,-k)*R11(j,k,IS)+Q1(NT,-k)*R12(j,k,IS)
     &+U1(NT,-k)*R13(j,k,IS)
      XQ=I1(NT,-k)*R21(j,k,IS)+Q1(NT,-k)*R22(j,k,IS)
     &+U1(NT,-k)*R23(j,k,IS)
      XU=I1(NT,-k)*R31(j,k,IS)+Q1(NT,-k)*R32(j,k,IS)
     &+U1(NT,-k)*R33(j,k,IS)
      I1(NT,j)=I1(NT,j)+2*rg(k)*XI
      Q1(NT,j)=Q1(NT,j)+2*rg(k)*XQ
      U1(NT,j)=U1(NT,j)+2*rg(k)*XU
      xxx=2*rg(k)*I1(NT,-k)*rmu(k)
      if(isaut.eq.2)I1(NT,j)=I1(NT,j)+xxx*rer(j,k,IS)
      if(isaut.eq.1.and.IS.eq.0)I1(NT,j)=I1(NT,j)+xxx*rsurf
      enddo

      do l=NT-1,1,-1
      F=H(l+1)-H(l)
      C=EXP(-F/RMU(j))
      I1(l,j)=C*I1(l+1,j)+(1-C)*SFI(l,j)
      Q1(l,j)=C*Q1(l+1,j)+(1-C)*SFQ(l,j)
      U1(l,j)=C*U1(l+1,j)+(1-C)*SFU(l,j)
      enddo
c     write(6,*)IS,ND,I1(1,j)
    7 continue 
c     pause

c ***** INTEGRATION SUR TAU-DIRECTIONS DOWNWARD
      do 8 j=-NN,-1
      I1(1,j)=0.
      Q1(1,j)=0.
      U1(1,j)=0.

      do 80 l=2,NT
      F=H(l)-H(l-1)
      C=EXP(F/RMU(j))
      I1(l,j)=C*I1(l-1,j)+(1-C)*SFI(l-1,j)
      Q1(l,j)=C*Q1(l-1,j)+(1-C)*SFQ(l-1,j)
      U1(l,j)=C*U1(l-1,j)+(1-C)*SFU(l-1,j)
   80 continue
    8 continue

c    CALCUL DE LA SOMME DES DIFFUSIONS
      do j=1,NN
      IM(j)=IM(j)+I1(NAV,j)
      QM(j)=QM(j)+Q1(NAV,j)
      UM(j)=UM(j)+U1(NAV,j)
      IM(-j)=IM(-j)+I1(NT,-j)
      QM(-j)=QM(-j)+Q1(NT,-j)
      UM(-j)=UM(-j)+U1(NT,-j)
      enddo

      if(IS.eq.0)then
      RS=0.0
      do j=1,NN
      RS=RS+I1(NAV,j)/IZTN(j)/NN
      enddo
      do j=-NN,NN
      IZT(j)=IZT(j)+I1(NAV,j)
      IZTN(j)=I1(NAV,j)
      IZG(j)=IZG(j)+I1(NT,j)
      IZGN(j)=I1(NT,j)
      enddo
      endif

c    TEST D ARRET SUR L'ORDRE DE DIFFUSION: CAS DE IS=0
      if(ND.eq.50)then
      if(IS.eq.0)then
        do j=-NN,NN
        IZT(j)=IZT(j)+IZTN(j)*RS/(1-RS)
        IZG(j)=IZG(j)+IZGN(j)*RS/(1-RS)
        enddo
        do j=1,NN
        IM(-j)=IM(-j)+IZGN(-j)*RS/(1-RS)
        IM(j)=IM(j)+IZTN(j)*RS/(1-RS)
        enddo
        goto 507
      endif
      endif

      z=0.
      do j=1,NN
      z=max(z,abs(I1(NAV,j)))
      z=max(z,abs(Q1(NAV,j)))
      z=max(z,abs(U1(NAV,j)))
      if(itest.eq.+1)goto 128
      z=max(z,abs(I1(NT,-j)))
      z=max(z,abs(Q1(NT,-j)))
      z=max(z,abs(U1(NT,-j)))
  128 continue
      enddo
c     write(6,*)'IS',IS,'zmax',z
c     pause
      if(z.gt.0.0001) goto 600
  507 continue

c   CALCUL CUMULATIF DES SORTIES HORS DIFF.PRIM. ET CONVERGENCE EN IS
      xx=0.0
      do 30 iv=1,NBV
      k=idir(iv)
      XI=IM(k+1)+coff(iv)*(IM(k)-IM(k+1))
      xx=max(xx,abs(XI))
      XQ=QM(k+1)+coff(iv)*(QM(k)-QM(k+1))
      xx=max(xx,abs(XQ))
      XU=UM(k+1)+coff(iv)*(UM(k)-UM(k+1))
      xx=max(xx,abs(XU))
      if(IS.eq.0)then
      SL(iv)=XI
      SQos(iv)=XQ
      SUos(iv)=0.
      goto 30
      endif
      ang=IS*pi*(fiv(iv)-180.0)/180.0
      SL(iv)=SL(iv)+2*cos(ang)*XI
      SQos(iv)=SQos(iv)+2*cos(ang)*XQ
      SUos(iv)=SUos(iv)+2*sin(ang)*XU
   30 continue
c     write(6,*)'xx',xx
c     pause
      if(xx.lt.0.0001)goto 243
    1 continue   
  243 continue
      close(1)

      UFT=0.0
      UFG=0.0
      DFT=rmus*exp(-H(NAV)/rmus)
      DFG=rmus*exp(-H(NT)/rmus)
      do k=1,NN
      UFT=UFT+2*rg(k)*IZT(k)*rmu(k)
      DFT=DFT+2*rg(k)*IZT(-k)*rmu(k)
      UFG=UFG+2*rg(k)*IZG(k)*rmu(k)
      DFG=DFG+2*rg(k)*IZG(-k)*rmu(k)
      enddo

      do iv=1,NBV
      cc=cos(2*chi(iv))
      ss=sin(2*chi(iv))
      SL(iv)=SL(iv)+SL1(iv)+RL1(iv)
      SQ(iv)=cc*SQos(iv)+ss*SUos(iv)+SQ1(iv)+RQ1(iv)
      if(ITRONC.eq.1)then
      SL(iv)=SL(iv)+DDL1(iv)
      SQ(iv)=SQ(iv)+DDQ1(iv)
      endif
      SU(iv)=-ss*SQos(iv)+cc*SUos(iv)
      SQos(iv)=cc*SQ(iv)-ss*SU(iv)
      SUos(iv)=ss*SQ(iv)+cc*SU(iv)
      xx=SQ(iv)
      yy=SU(iv)
      SLP(iv)=sqrt(xx*xx+yy*yy)
      SLPsig(iv)=SLP(iv)
      if(SQ(iv).lt.0.) SLPsig(iv)=-SLP(iv)
      enddo

  126 continue
      IF(IMSC.EQ.1) THEN
       do iv=1,NBV
       SLout(iv)=SLT1(iv)
       ENDDO
       IF(iop.EQ.0) THEN
C out is scattering plance
      do iv=1,NBV
      SQout(iv)=SQT1(iv)
      SUout(iv)=SUT1(iv)
      SLPout(iv)=SLP1sig(iv)
      ENDDO
       ENDIF
       IF(iop.EQ.1) THEN
C out is meridian plance
      do iv=1,NBV
      SQout(iv)=SQT1os(iv)
      SUout(iv)=SUT1os(iv)
      SLPout(iv)=-SLP1sigos(iv)
      ENDDO
       ENDIF
      ELSE
       do iv=1,NBV
       SLout(iv)=SL(iv)
       ENDDO
      IF(iop.EQ.0) THEN
C out is scattering plance
      do iv=1,NBV
      SQout(iv)=SQ(iv)
      SUout(iv)=SU(iv)
      SLPout(iv)=SLPsig(iv)
      ENDDO
       ENDIF
       IF(iop.EQ.1) THEN
C out is meridian plance
      do iv=1,NBV
      SQout(iv)=SQos(iv)
      SUout(iv)=SUos(iv)
c     SLPout(iv)=SLPsigos(iv)
      SLPout(iv)=SLPsig(iv)
      ENDDO
       ENDIF      
      ENDIF 
CD      WRITE(*,*) rmu(15),'rmu(15) last' \
   22 FORMAT(2f12.4,i4,a20) 
   23 FORMAT(f15.10,i5,2f15.10,a42)
   24 FORMAT(5f15.10,i5,a55)    
      return
      end                                                                       


      subroutine BRDF(NN,rho,vk,gteta,RER,rmu)
      parameter(NF=6)
      PARAMETER(NN0=85)
      double precision rmu
      dimension RER(NN0,NN0,0:50),rmu(-NN0:NN0),ga(-NN0:NN0),
     &ref(NN0,NN0,0:180)
      pi = 3.141592653
      do 1 j=1,NN
      cv=rmu(j)
      do 2 k=1,NN
      cs=rmu(k)
cs      WRITE(*,*)'cv,cs,phi',acos(cv)*180./pi,
cs     &acos(cs)*180./pi
      do 3 m=0,180
      phi=1.*m
cs      ref(j,k,m)=RBD(cs,cv,phi,rho,vk,gteta,IBRF)
      ref(j,k,m)=RBDLRS(cs,cv,phi,rho,vk,gteta)
cs      WRITE(*,*)1.*m,ref(j,k,m)
    3 continue
    2 continue
    1 continue
      do i=1,NN
      if((acos(rmu(i))*180./pi).LE.82.)THEN 
      ind=i
      goto 90
      endif
      enddo
   90 continue
      do i=1,ind-1
      do j=1,NN
      do k=0,180
      ref(i,j,k)=ref(ind,j,k)
      enddo
      enddo
      enddo
      do i=1,NN
      if((acos(rmu(i))*180./pi).LE.76.)THEN 
      ind1=i
      goto 91
      endif
      enddo
   91 continue
      do i=1,NN
      do j=1,ind1-1
      do k=0,180
      ref(i,j,k)=ref(i,ind1,k)
      enddo
      enddo
      enddo
cs      do i=1,NN
cs      WRITE(*,*)'theta',acos(rmu(i))*180./pi
cs      do j=0,180
cs      WRITE(*,*)'phi',j
cs      do k=1,NN
cs      WRITE(*,*)acos(rmu(k))*180./pi, ref(i,k,j)
cs      enddo
cs      enddo
cs      enddo
      do j=1,NN
      do k=1,NN
      do 4 is=0,NF
      xsign=(-1)**is
      somm=0.5*(ref(j,k,0)+xsign*ref(j,k,180))
      do 40 m=1,179
      somm=somm+cos(is*m*pi/180.)*ref(j,k,m)
   40 continue
      RER(j,k,is)=somm/180.0
cs      RER(j,k,is)=0.0
    4 continue
      enddo
      enddo
      return
      end

      FUNCTION DEN(X,KM,HM)
      pi = 3.141592653
      if(KM.eq.1)then
      DEN=exp(-X/HM)/HM
      goto 1
      endif
      if(KM.eq.2)then
      ea=1.0
      y=(X-HM)/ea
      DEN=exp(-y*y)/sqrt(pi)/ea
      endif
    1 continue
      RETURN
      END

      FUNCTION RBD(cs,cv,phi,rho,vk,gt)
      pi = 3.141592653
      xx=cs**(vk-1.)
      yy=cv**(vk-1.)
      zz=(cs+cv)**(1.-vk)
      FF1=rho*xx*yy/zz
      xx=sqrt(1-cs*cs)
      yy=sqrt(1-cv*cv)
      ww=cs*cv-xx*yy*cos(pi*phi/180.)
      aa=1+gt*gt+2*gt*ww
      FF2=(1-gt*gt)/(aa**1.5)
      vv=xx/cs
      ww=yy/cv
      G=sqrt(vv*vv+ww*ww+2*vv*ww*cos(pi*phi/180))
      FF3=1+(1-rho)/(1+G)
      RBD=FF1*FF2*FF3
cs      RBD=0.0
      return
      end
      FUNCTION RBDLRS(cs,cv,phi,rho,vk,gt)
      pi = 3.141592653589793
cs      WRITE(*,*)'rho,vk,gt',rho,vk,gt
cs      IF(phi.GT.180.)phi=360.-phi
      THIRAD=acos(cs)
      THERAD=acos(cv)
      PHIRAD=phi*pi/180.
cs      WRITE(*,*)'THIRAD',acos(cs)*180./pi
cs      WRITE(*,*)'THERAD',acos(cv)*180./pi
cs      WRITE(*,*)'AARAD',phi
CS********CALCULATIONS OF PHASE ANGLE FOR KVOL****
              COS_TZETA=cos(THIRAD)*cos(THERAD)+sin(THIRAD)*
     &           sin(THERAD)*cos(PHIRAD)
              TZETA=acos(max(-1.,min(1.,COS_TZETA)))
              SIN_TZETA=sin(TZETA)
CS***CALCULATION OF KVOL****************************
             xt=((PI/2.-TZETA)*COS_TZETA+SIN_TZETA)/
     $             (cos(THERAD)+cos(THIRAD))
             XKVOL=xt-PI/4.
CS***CALCULATION OF PRIME ANGLES********************
             TAN_THERADP=tan(THERAD)
             IF(TAN_THERADP.LT.0.)TAN_THERADP=0.
             THERADP=atan(TAN_THERADP)
             SIN_THERADP=sin(THERADP)
             COS_THERADP=cos(THERADP)
             TAN_THIRADP=tan(THIRAD)
             IF(TAN_THIRADP.LT.0.)TAN_THIRADP=0.
             THIRADP=atan(TAN_THIRADP)
             SIN_THIRADP=sin(THIRADP)
             COS_THIRADP=cos(THIRADP)
CS***CALCULATION OF THE DISTANCE******************** 
              DIST=TAN_THERADP*TAN_THERADP+TAN_THIRADP*TAN_THIRADP-
     $              2.*TAN_THERADP*TAN_THIRADP*cos(PHIRAD)
              DIST=SQRT(max(0.,DIST))
CS***OVERLAP CALCULATIONS****************************
              xtemp=1./COS_THERADP+1./COS_THIRADP
              COS_T=SQRT(DIST*DIST+TAN_THERADP*TAN_THERADP*
     &         TAN_THIRADP*TAN_THIRADP*sin(PHIRAD)*sin(PHIRAD))
              COS_T=2.*COS_T/xtemp
              COS_T=max(-1.,min(1.,COS_T))
              TVAR=acos(COS_T)
              SIN_T=sin(TVAR)
              OVERLAP=1./PI*(TVAR-SIN_T*COS_T)*xtemp
CS***CALCULATION OF PHASE ANGLE FOR KGEO*************
              COS_TZETAP=COS_THERADP*COS_THIRADP+SIN_THERADP*
     &         SIN_THIRADP*cos(PHIRAD)
              TZETAP=acos(max(-1.,min(1.,COS_TZETAP)))
              SIN_TZETAP=sin(TZETAP)
CS***CALCULATION OF KGEO*****************************
              XKGEO=OVERLAP-xtemp+0.5*(1.+COS_TZETAP)/
     &          COS_THIRADP/COS_THERADP
cs      WRITE(*,*)'kgeo',xkgeo
CS***************************************************
      RBDLRS=rho+vk*XKVOL+gt*XKGEO
cs      WRITE(*,*)'BRF',RBDLRS
cs      RBDLRS=0.0
cs      WRITE(*,*)thetas,thetao,azim
cs      WRITE(*,*)RBDLRS
cs     return
      end

      FUNCTION ANGTURN(tsol,avis,afiv)
      pi = 3.141592653
      x0=cos(tsol*pi/180.)
      x1=cos(avis*pi/180.)
      z=cos(afiv*pi/180.)
      x2=x0*x1+z*sqrt(1-x1*x1)*sqrt(1-x0*x0)
      sbeta=(x0-x1*x2)/sqrt(1-x2*x2)/sqrt(1-x1*x1)
      if(sbeta.gt.1.0)sbeta=1.0
      if(sbeta.lt.-1.0)sbeta=-1.0
      ANGTURN=acos(sbeta)-pi/2.
c CORRECTION
      if(afiv.gt.180.)ANGTURN=-ANGTURN
      if(afiv.lt.0.1.or.afiv.gt.359.9.
     &or.abs(afiv-180.0).lt.0.1)ANGTURN=pi/2.0
      if(avis.lt.0.1.or.avis.gt.179.9)ANGTURN=-pi*afiv/180.0+pi/2.
      RETURN
      END

      subroutine gauss(MM,AMU,PMU)
C     ORDRE DE LA QUADRATURE N=2*MM-2 
      PARAMETER (IX=600)
      PARAMETER(NG0=91)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION Z(IX),PA(IX),W(IX),R(IX),AMU(-NG0:NG0),PMU(-NG0:NG0)
      TOL = 1.0D-15
      PI = 3.141592653589793D+00
      N=2*MM-2
      XL=-1
      XU=+1
      AA = 2.0D+00/PI**2
      AB = -62.0D+00/(3.0D+00*PI**4)
      AC = 15116.0D+00/(15.0D+00*PI**6)
      AD = -12554474.D+00/(105.0D+00*PI**8)
      PA(1) = 1.D0
      EN = N
      NP1 = N+1
      U= 1.0D+00-(2.0D+00/PI)**2
      D = 1.0D+00/DSQRT((EN+0.5D+00)**2+U/4.0D+00)
      DO 100 I= 1,N
      SM = I
      AZ = 4.0D+00*SM-1.0D+00
      AE = AA/AZ
      AF = AB/AZ**3
      AG = AC/AZ**5
      AH = AD/AZ**7
  100 Z(I) = 0.25D+00*PI*(AZ+AE+AF+AG+AH)
      DO 200 K = 1,N
      X = COS(Z(K)*D)
    1 PA(2) = X
      DO 201  NN = 3,NP1
      ENN = NN-1
  201 PA(NN) =
     & ((2.0D+00*ENN-1.0D+00)*X*PA(NN-1)-(ENN-1.0D+00)*PA(NN-2))/ENN
      PNP = EN*(PA(N)-X*PA(NP1))/(1.0D+00-X*X)
      XI = X-PA(NP1)/PNP
      XD = ABS(XI-X)
      XDD = XD-TOL
      IF (XDD) 3,3,2
    2 X = XI
      GO TO 1
    3 R(K) = X
  200 W(K) = 2.0D+00*(1.0D+00-X*X)/(EN*PA(N))**2
      AP = (XU-XL)/2.D0
      BP = (XU+XL)/2.D0
      do I=1,MM-1
      K=MM-I
      AMU(K)=BP+AP*R(I)
      PMU(K)=AP*W(I)
      AMU(-K)=-AMU(K)
      PMU(-K)=PMU(K)
      enddo
      AMU(-MM)=-1.
      AMU(MM)=1.
      AMU(0)=0.
      PMU(0)=0.
      PMU(-MM)=0.
      PMU(MM)=0.
      RETURN
      END

      subroutine gauss_1(MM,AMU,PMU)
C     ORDRE DE LA QUADRATURE N=2*MM-2 
      PARAMETER (IX=600)
      PARAMETER(NN0=85)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION Z(IX),PA(IX),W(IX),R(IX),AMU(-NN0:NN0),PMU(-NN0:NN0)
      TOL = 1.0D-15
      PI = 3.141592653589793D+00
      N=2*MM-2
      XL=-1
      XU=+1
      AA = 2.0D+00/PI**2
      AB = -62.0D+00/(3.0D+00*PI**4)
      AC = 15116.0D+00/(15.0D+00*PI**6)
      AD = -12554474.D+00/(105.0D+00*PI**8)
      PA(1) = 1.D0
      EN = N
      NP1 = N+1
      U= 1.0D+00-(2.0D+00/PI)**2
      D = 1.0D+00/DSQRT((EN+0.5D+00)**2+U/4.0D+00)
      DO 100 I= 1,N
      SM = I
      AZ = 4.0D+00*SM-1.0D+00
      AE = AA/AZ
      AF = AB/AZ**3
      AG = AC/AZ**5
      AH = AD/AZ**7
  100 Z(I) = 0.25D+00*PI*(AZ+AE+AF+AG+AH)
      DO 200 K = 1,N
      X = COS(Z(K)*D)
    1 PA(2) = X
      DO 201  NN = 3,NP1
      ENN = NN-1
  201 PA(NN) =
     & ((2.0D+00*ENN-1.0D+00)*X*PA(NN-1)-(ENN-1.0D+00)*PA(NN-2))/ENN
      PNP = EN*(PA(N)-X*PA(NP1))/(1.0D+00-X*X)
      XI = X-PA(NP1)/PNP
      XD = ABS(XI-X)
      XDD = XD-TOL
      IF (XDD) 3,3,2
    2 X = XI
      GO TO 1
    3 R(K) = X
  200 W(K) = 2.0D+00*(1.0D+00-X*X)/(EN*PA(N))**2
      AP = (XU-XL)/2.D0
      BP = (XU+XL)/2.D0
      do I=1,MM-1
      K=MM-I
      AMU(K)=BP+AP*R(I)
      PMU(K)=AP*W(I)
      AMU(-K)=-AMU(K)
      PMU(-K)=PMU(K)
      enddo
      AMU(-MM)=-1.
      AMU(MM)=1.
      AMU(0)=0.
      PMU(0)=0.
      PMU(-MM)=0.
      PMU(MM)=0.
      RETURN
      END

      subroutine GAUSS_2(MM,AMU,PMU)
C     N : ORDRE DE LA QUADRATURE SUR XL A XU
      PARAMETER (IX=600)
      PARAMETER (NG0=91)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION Z(IX),PA(IX),W(IX),R(IX),AMU(-NG0:NG0),PMU(-NG0:NG0)
      real AMU,PMU
      TOL = 1.0D-15
      PI = 3.141592653589793D+00
      N=2*MM-2
      XL=-1
      XU=+1
      AA = 2.0D+00/PI**2
      AB = -62.0D+00/(3.0D+00*PI**4)
      AC = 15116.0D+00/(15.0D+00*PI**6)
      AD = -12554474.D+00/(105.0D+00*PI**8)
      PA(1) = 1.D0
      EN = N
      NP1 = N+1
      U= 1.0D+00-(2.0D+00/PI)**2
      D = 1.0D+00/DSQRT((EN+0.5D+00)**2+U/4.0D+00)
      DO 100 I= 1,N
      SM = I
      AZ = 4.0D+00*SM-1.0D+00
      AE = AA/AZ
      AF = AB/AZ**3
      AG = AC/AZ**5
      AH = AD/AZ**7
  100 Z(I) = 0.25D+00*PI*(AZ+AE+AF+AG+AH)
      DO 200 K = 1,N
      X = COS(Z(K)*D)
    1 PA(2) = X
      DO 201  NN = 3,NP1
      ENN = NN-1
  201 PA(NN) =
     & ((2.0D+00*ENN-1.0D+00)*X*PA(NN-1)-(ENN-1.0D+00)*PA(NN-2))/ENN
      PNP = EN*(PA(N)-X*PA(NP1))/(1.0D+00-X*X)
      XI = X-PA(NP1)/PNP
      XD = ABS(XI-X)
      XDD = XD-TOL
      IF (XDD) 3,3,2
    2 X = XI
      GO TO 1
    3 R(K) = X
  200 W(K) = 2.0D+00*(1.0D+00-X*X)/(EN*PA(N))**2
      AP = (XU-XL)/2.D0
      BP = (XU+XL)/2.D0
      do I=MM+1,NG0
      AMU(I)=0.0
      PMU(I)=0.0
      AMU(-I)=0.0
      PMU(-I)=0.0
      enddo
      do I=1,MM-1
      K=MM-I
      AMU(K)=BP+AP*R(I)
      PMU(K)=AP*W(I)
      AMU(-K)=-AMU(K)
      PMU(-K)=PMU(K)
      enddo
      AMU(-MM)=-1.
      AMU(MM)=1.
      AMU(0)=0.
      PMU(0)=0.
      PMU(-MM)=0.
      PMU(MM)=0.
      RETURN
      END






C***********************************************************************
      REAL FUNCTION  LINEARD( X, Y, M, X1 )
C+---------------------------------------------------------------------+
C!       Linear interpolation function.                                !
C+---------------------------------------------------------------------+
      IMPLICIT  REAL   (A-H, O-Z)
      REAL      X( * ), Y( * )
C
      IF(X(2).GT.X(1)) THEN
      
      IF ( X1.LT.X(1) )  THEN
         LINEARD = Y(1) - ( X1-X(1) )*( Y(2)-Y(1) )/( X(2)-X(1) )
      ELSE IF ( X1.GT.X(M) )  THEN
         LINEARD = Y(M) + ( X1-X(M) )*( Y(M)-Y(M-1) )/( X(M)-X(M-1) )
      ELSE
         DO 10 N = 2, M
            IF ( X1.GE.X(N-1) .AND. X1.LE.X(N) ) LINEARD = Y(N-1) +
     $         ( X1-X(N-1) )*( Y(N)-Y(N-1) )/( X(N)-X(N-1) )
C*****************************
        IF(X1.EQ.X(N-1)) LINEARD =Y(N-1)
        IF(X1.EQ.X(N)) LINEARD =Y(N)
C*****************************
10       CONTINUE
      END IF
      ELSE 
      IF ( X1.GT.X(1) )  THEN
         LINEARD = Y(1) - ( X1-X(1) )*( Y(2)-Y(1) )/( X(2)-X(1) )
      ELSE IF ( X1.LT.X(M) )  THEN
         LINEARD = Y(M) + ( X1-X(M) )*( Y(M)-Y(M-1) )/( X(M)-X(M-1) )
      ELSE
         DO 110 N = 2, M
            IF ( X1.LE.X(N-1) .AND. X1.GE.X(N) ) LINEARD = Y(N-1) +
     $         ( X1-X(N-1) )*( Y(N)-Y(N-1) )/( X(N)-X(N-1) )
      
C*****************************
        IF(X1.EQ.X(N-1)) LINEAR =Y(N-1)
        IF(X1.EQ.X(N)) LINEARD =Y(N)
C*****************************
110       CONTINUE
      END IF

      ENDIF 
      RETURN
      END
C**************************  END OF LINEAR  ****************************
C***********************************************************************
      REAL FUNCTION  LINEAR_LN (X, Y, M, X1 )
C+---------------------------------------------------------------------+
C!       Linear interpolation function.                                !
C+---------------------------------------------------------------------+
      IMPLICIT  REAL   (A-H, O-Z)
      REAL      X( * ), Y( * )
C
      IF(X(2).GT.X(1)) THEN
      IF ( X1.LT.X(1) )  THEN
		IF(Y(2).LT.1.0E-30)Y2=1.0E-30
		IF(Y(1).LT.1.0E-30)Y1=1.0E-30
		IF(Y(M).LT.1.0E-30)YM=1.0E-30
		IF(Y(M-1).LT.1.0E-30)Y(M-1)=1.0E-30
         LINEAR_LN = 
     $LOG(Y(1)) - ( X(1)-X1 )*( LOG(Y(2))-LOG(Y(1)) )/( X(2)-X(1) )
         LINEAR_LN=EXP(LINEAR_LN)
      ELSE IF ( X1.GT.X(M) )  THEN
         LINEAR_LN = 
     $LOG(Y(M)) + ( X1-X(M) )*( LOG(Y(M))-LOG(Y(M-1)) )/( X(M)-X(M-1) )
         LINEAR_LN=EXP(LINEAR_LN)
      ELSE
         DO 10 N = 2, M
            IF ( X1.GE.X(N-1) .AND. X1.LE.X(N) )then
				IF(Y(N).LT.1.0E-30)Y(N)=1.0E-30
				IF(Y(N-1).LT.1.0E-30)Y(N-1)=1.0E-30
            LINEAR_LN = LOG(Y(N-1))+
     $         ( X1-X(N-1) )*( LOG(Y(N))-LOG(Y(N-1)) )/( X(N)-X(N-1) )
            LINEAR_LN=EXP(LINEAR_LN)
            endif
         
C*****************************
        IF(X1.EQ.X(N-1)) LINEAR_LN =Y(N-1)
        IF(X1.EQ.X(N)) LINEAR_LN =Y(N)
C*****************************
10       CONTINUE
      END IF
      
      ELSE 
      IF ( X1.GT.X(1) )  THEN
		IF(Y(2).LT.1.0E-30)Y(2)=1.0E-30
		IF(Y(1).LT.1.0E-30)Y(1)=1.0E-30
		IF(Y(M).LT.1.0E-30)Y(M)=1.0E-30
		IF(Y(M-1).LT.1.0E-30)Y(M-1)=1.0E-30
         LINEAR_LN = 
     $LOG(Y(1)) - ( X(1)-X1 )*( LOG(Y(2))-LOG(Y(1)) )/( X(2)-X(1) )
         LINEAR_LN=EXP(LINEAR_LN)
      ELSE IF ( X1.LT.X(M) )  THEN
         LINEAR_LN = 
     $LOG(Y(M)) + ( X1-X(M) )*( LOG(Y(M))-LOG(Y(M-1)) )/( X(M)-X(M-1) )
         LINEAR_LN=EXP(LINEAR_LN)
      ELSE
         DO 110 N = 2, M
            IF ( X1.LE.X(N-1) .AND. X1.GE.X(N) )then
			IF(Y(N-1).LT.1.0E-30)Y(N-1)=1.0E-30
			IF(Y(N).LT.1.0E-30)Y(N)=1.0E-30
            LINEAR_LN = LOG(Y(N-1))+
     $         ( X1-X(N-1) )*( LOG(Y(N))-LOG(Y(N-1)) )/( X(N)-X(N-1) )
            LINEAR_LN=EXP(LINEAR_LN)
            endif
         
C*****************************
        IF(X1.EQ.X(N-1)) LINEAR_LN =Y(N-1)
        IF(X1.EQ.X(N)) LINEAR_LN =Y(N)
C*****************************
110       CONTINUE
      END IF
      
      ENDIF
      
      RETURN
      END
C**************************  END OF LINEAR_LN  ****************************
      subroutine developpe
     &(NG,xmu,xg,isaut,wind,anb,bnb,NN,rmu,
     &P11,P12,P13,P21,P22,P23,P31,P32,P33)
      parameter(NF=90)
      PARAMETER(NN0=85,NG0=91)
      double precision rmu,xmu,xg
      dimension rmu(-NN0:NN0),xmu(-NG0:NG0),xg(-NG0:NG0),
     &t11(-NG0:NG0),t12(-NG0:NG0),t22(-NG0:NG0),t33(-NG0:NG0),
     &alp(0:2*NG0-2),bet(0:2*NG0-2),gam(0:2*NG0-2),zet(0:2*NG0-2),
     &P11(NN0,NN0,0:2*NG0-2),P12(NN0,NN0,0:2*NG0-2),
     &P13(NN0,NN0,0:2*NG0-2),
     &P21(NN0,NN0,0:2*NG0-2),P22(NN0,NN0,0:2*NG0-2),
     &P23(NN0,NN0,0:2*NG0-2),
     &P31(NN0,NN0,0:2*NG0-2),P32(NN0,NN0,0:2*NG0-2),
     &P33(NN0,NN0,0:2*NG0-2),
     &fjk(NN0,NN0,0:2*NG0-2),V(0:180),gjk(NN0,NN0,-4*NG0:4*NG0),
     &X11(NN0,NN0,0:2*NG0-2),X12(NN0,NN0,0:2*NG0-2),
     &X13(NN0,NN0,0:2*NG0-2),
     &X21(NN0,NN0,0:2*NG0-2),X22(NN0,NN0,0:2*NG0-2),
     &X23(NN0,NN0,0:2*NG0-2),
     &X31(NN0,NN0,0:2*NG0-2),X32(NN0,NN0,0:2*NG0-2),
     &X33(NN0,NN0,0:2*NG0-2)

      pi = 3.141592653
c      do j=-NN,NN
c      write(*,*) j,rmu(j)
c      ENDDO
      if(isaut.eq.1)xind=1.33
      if(isaut.eq.2)xind=1.50
      SIG =.003+.00512*wind
      call fresnel(NG,xmu,xind,t11,t12,t22,t33)
      call betal(NG,xmu,xg,t11,t12,t22,t33,alp,bet,gam,zet)
       do j=1,10
       enddo

      do j=1,NN
      do k=j,NN
      x1=rmu(j)
      x0=rmu(k)
      y1=sqrt(1-x1*x1)
      y0=sqrt(1-x0*x0)
      do i=0,180
      if(isaut.eq.2)goto 111
c isaut=1 Cox et Munk
      z=0.5*(1+x0*x1-y0*y1*cos(i*pi/180.))
      w=(x0+x1)*(x0+x1)/(4.0*z)
      V(i)=exp(-(1-w)/(w*SIG))/(4.0*SIG*w*w)/x1
      goto 112
c isaut=2 Nadal et Breon
  111 continue
      arg=x0*x1-y0*y1*cos(i*pi/180.)
      xx=cos(acos(arg)/2)
      if(j.eq.k.and.i.eq.180)xx=0
      yy=sqrt(xind*xind+xx*xx-1.0)
      zz=xx*xind*xind
      rl=(zz-yy)/(zz+yy)
      rr=(xx-yy)/(xx+yy)
      AAA=(rr*rr-rl*rl)/2
      BBB=bnb*AAA/(rmu(j)+rmu(k))
      V(i)=rmu(k)*anb*(1-exp(-BBB))/AAA
      if(BBB.lt.0.0001)V(i)=rmu(k)*anb*bnb/(rmu(j)+rmu(k))
  112 continue
      enddo

      xs=-1.
      do IS=0,2*NG-2
      xs=-xs
      fjk(j,k,IS)=(V(0)+xs*V(180))/360.0
      do i=1,179
      fjk(j,k,IS)=fjk(j,k,IS)+cos(IS*i*pi/180.)*V(i)/180.
      enddo
      fjk(k,j,IS)=fjk(j,k,IS)*x1/x0
      enddo
      enddo
      enddo

c Coefficients de reflexion
      call noyaux(NG,xmu,NN,rmu,alp,bet,gam,zet,X11,X12,X13,
     &X21,X22,X23,X31,X32,X33)
      do j=1,NN
      do k=1,NN
      do m=0,2*NG-2
      gjk(j,k,m)=fjk(j,k,m)
      enddo
      do m=2*NG-1,4*NG
      gjk(j,k,m)=0.0
      enddo 
      do m=1,4*NG
      gjk(j,k,-m)=gjk(j,k,m)
      enddo
      enddo
      enddo

      do j=1,NN
      do k=1,NN
      do IS=0,2*NG-2
      P11(j,k,IS)=X11(j,k,0)*gjk(j,k,IS)
      P12(j,k,IS)=X12(j,k,0)*gjk(j,k,IS)
      P13(j,k,IS)=0.0
      P21(j,k,IS)=X21(j,k,0)*gjk(j,k,IS)
      P22(j,k,IS)=X22(j,k,0)*gjk(j,k,IS)
      P23(j,k,IS)=0.0
      P31(j,k,IS)=0.0
      P32(j,k,IS)=0.0
      P33(j,k,IS)=X33(j,k,0)*gjk(j,k,IS)
      do m=1,2*NG-2
      P11(j,k,IS)=P11(j,k,IS)+X11(j,k,m)*(gjk(j,k,IS+m) +gjk(j,k,IS-m))
      P12(j,k,IS)=P12(j,k,IS)+X12(j,k,m)*(gjk(j,k,IS+m) +gjk(j,k,IS-m))
      P13(j,k,IS)=P13(j,k,IS)+X13(j,k,m)*(-gjk(j,k,IS+m)+gjk(j,k,IS-m))
      P21(j,k,IS)=P21(j,k,IS)+X21(j,k,m)*(gjk(j,k,IS+m) +gjk(j,k,IS-m))
      P22(j,k,IS)=P22(j,k,IS)+X22(j,k,m)*(gjk(j,k,IS+m) +gjk(j,k,IS-m))
      P23(j,k,IS)=P23(j,k,IS)+X23(j,k,m)*(-gjk(j,k,IS+m)+gjk(j,k,IS-m))
      P31(j,k,IS)=P31(j,k,IS)+X31(j,k,m)*(-gjk(j,k,IS+m)+gjk(j,k,IS-m))
      P32(j,k,IS)=P32(j,k,IS)+X32(j,k,m)*(-gjk(j,k,IS+m)+gjk(j,k,IS-m))
      P33(j,k,IS)=P33(j,k,IS)+X33(j,k,m)*(gjk(j,k,IS+m) +gjk(j,k,IS-m))
      enddo
      enddo
      enddo
      enddo
      return
      end

      subroutine noyaux(NG,xmu,NN,rmu,alp,bet,gam,zet,P11,P12,P13,
     &P21,P22,P23,P31,P32,P33)
      double precision rmu,xmu
      PARAMETER(NN0=85,NG0=91)
      dimension rmu(-NN0:NN0),xmu(-NG0:NG0),
     &alp(0:2*NG0-2),bet(0:2*NG0-2),gam(0:2*NG0-2),zet(0:2*NG0-2),
     &P11(NN0,NN0,0:2*NG0-2),P12(NN0,NN0,0:2*NG0-2),
     &P13(NN0,NN0,0:2*NG0-2),
     &P21(NN0,NN0,0:2*NG0-2),P22(NN0,NN0,0:2*NG0-2),
     &P23(NN0,NN0,0:2*NG0-2),
     &P31(NN0,NN0,0:2*NG0-2),P32(NN0,NN0,0:2*NG0-2),
     &P33(NN0,NN0,0:2*NG0-2),
     &PSL(-1:2*NG0,-NN0:NN0),RSL(-1:2*NG0,-NN0:NN0),
     &TSL(-1:2*NG0,-NN0:NN0)
      do 200 m=0,2*NG-2
      call legendre(m,rmu,NG,NN,PSL,TSL,RSL)
      do 201 j=1,NN
      do 202 k=1,NN
      P11(j,k,m)=0.0
      P12(j,k,m)=0.0
      P13(j,k,m)=0.0
      P21(j,k,m)=0.0
      P22(j,k,m)=0.0
      P23(j,k,m)=0.0
      P31(j,k,m)=0.0
      P32(j,k,m)=0.0
      P33(j,k,m)=0.0
      DO 203 L=m,2*NG-2
      TT=TSL(L,j)*TSL(L,-k)
      TR=TSL(L,j)*RSL(L,-k)
      RR=RSL(L,j)*RSL(L,-k)
      RT=RSL(L,j)*TSL(L,-k)
      P11(j,k,m)=P11(j,k,m)+bet(L)*PSL(L,j)*PSL(L,-k)
      P21(j,k,m)=P21(j,k,m)+gam(L)*RSL(L,j)*PSL(L,-k)
      P31(j,k,m)=P31(j,k,m)-gam(L)*TSL(L,j)*PSL(L,-k)
      P12(j,k,m)=P12(j,k,m)+gam(L)*PSL(L,j)*RSL(L,-k)
      P22(j,k,m)=P22(j,k,m)+alp(L)*RR+zet(L)*TT
      P32(j,k,m)=P32(j,k,m)-alp(L)*TR-zet(L)*RT
      P13(j,k,m)=P13(j,k,m)-gam(L)*PSL(L,j)*TSL(L,-k)
      P23(j,k,m)=P23(j,k,m)-alp(L)*RT-zet(L)*TR
      P33(j,k,m)=P33(j,k,m)+alp(L)*TT+zet(L)*RR
  203 continue
  202 continue
  201 continue
  200 continue
      return
      end

      subroutine fresnel(NG,xmu,xind,t11,t12,t22,t33)
      double precision xmu
      PARAMETER(NG0=91)
      dimension
     &t11(-NG0:NG0),t12(-NG0:NG0),t22(-NG0:NG0),t33(-NG0:NG0),
     &xmu(-NG0:NG0)
      pi = 3.141592653
      do j=-NG,NG
      av=acos(xmu(j))*180./pi
      ai=(180.-av)*0.5
      xx=cos(ai*pi/180.)
      yy=sqrt(xind*xind-1.0+xx*xx)
      zz=xx*xind*xind
      rl=(zz-yy)/(zz+yy)
      rr=(xx-yy)/(xx+yy)
CD      t11(j)=0.5*(rl*rl+rr*rr)
      t12(j)=0.5*(rl*rl-rr*rr)
      t11(j)=-t12(j)
      t22(j)=t11(j)
      t33(j)=rl*rr
      enddo
      return
      end

      subroutine betal(LL,zmu,zp,f11,f12,f22,f33,alpa,beta,gama,zeta)
      double precision zmu,zp
      PARAMETER(NG0=91)
      dimension zmu(-NG0:NG0),zp(-NG0:NG0),
     &pl(-1:2*NG0),pol(-1:2*NG0),ppl(-1:2*NG0),pml(-1:2*NG0),
     &f11(-NG0:NG0),f22(-NG0:NG0),f12(-NG0:NG0),f33(-NG0:NG0),
     &alpa(0:2*NG0-2),beta(0:2*NG0-2),gama(0:2*NG0-2),zeta(0:2*NG0-2),
     &betap(0:2*NG0-2),betam(0:2*NG0-2)
      do k=0,2*LL-2                                                          
      beta(k)=0. 
      gama(k)=0.
      alpa(k)=0.
      zeta(k)=0.
      betap(k)=0. 
      betam(k)=0. 
      enddo

      pl(-1)=0.                                                                 
      pl(0)=1.                                                                  
      pol(0)=0.0
      pol(1)=0.0
      ppl(0)=0.0
      ppl(1)=0.0
      pml(0)=0.0
      pml(1)=0.0
      do j=-LL,LL
      xx=zmu(j)
      pol(2)=3.0*(1.0-xx*xx)/2.0/sqrt(6.0)
      ppl(2)=(1.+xx)*(1.+xx)/4.
      pml(2)=(1.-xx)*(1.-xx)/4.

      do k=0,2*LL-2
      pl(k+1)=((2*k+1.)*xx*pl(k)-k*pl(k-1))/(k+1.)                            
      if(k.gt.1)then
        dd=(2*k+1.)/sqrt((k+3.)*(k-1.))
        ee=sqrt((k+2.)*(k-2.))/(2*k+1.)
        cc=(k+1.)*(k+2.)*(k-2.)/k/(k+3.)/(k-1.)
        bb=(2*k+1.)/k/(k+3.)/(k-1)
        pol(k+1)=dd*(xx*pol(k)-ee*pol(k-1))
        ppl(k+1)=bb*(k*(k+1.)*xx-4.)*ppl(k)-cc*ppl(k-1)
        pml(k+1)=bb*(k*(k+1.)*xx+4.)*pml(k)-cc*pml(k-1)
      endif
      beta(k)=beta(k)+zp(j)*pl(k)*f11(j)*(k+0.5)
      gama(k)=gama(k)+zp(j)*pol(k)*f12(j)*(k+0.5)
      betap(k)=betap(k)+zp(j)*ppl(k)*(f22(j)+f33(j))*(k+0.5)
      betam(k)=betam(k)+zp(j)*pml(k)*(f22(j)-f33(j))*(k+0.5)
      zeta(k)=(betap(k)-betam(k))/2.
      alpa(k)=(betap(k)+betam(k))/2.
      enddo
      enddo
      return
      end

      subroutine betal_1(LL,zmu,zp,f11,f12,f22,f33,alpa,beta,gama,zeta)
      PARAMETER(NG0=91)
      dimension zmu(-NG0:NG0),zp(-NG0:NG0),
     &pl(-1:2*NG0),pol(-1:2*NG0),ppl(-1:2*NG0),pml(-1:2*NG0),
     &f11(-NG0:NG0),f22(-NG0:NG0),f12(-NG0:NG0),f33(-NG0:NG0),
     &alpa(0:2*NG0-2),beta(0:2*NG0-2),gama(0:2*NG0-2),zeta(0:2*NG0-2),
     &betap(0:2*NG0-2),betam(0:2*NG0-2)
      do k=0,2*LL-2
      beta(k)=0.
      gama(k)=0.
      alpa(k)=0.
      zeta(k)=0.
      betap(k)=0.
      betam(k)=0.
      enddo

      pl(-1)=0.
      pl(0)=1.   
      pol(0)=0.0
      pol(1)=0.0
      ppl(0)=0.0
      ppl(1)=0.0
      pml(0)=0.0
      pml(1)=0.0
      do j=-LL,LL
      xx=zmu(j)
      pol(2)=3.0*(1.0-xx*xx)/2.0/sqrt(6.0)
      ppl(2)=(1.+xx)*(1.+xx)/4.
      pml(2)=(1.-xx)*(1.-xx)/4.
      do k=0,2*LL-2
      pl(k+1)=((2*k+1.)*xx*pl(k)-k*pl(k-1))/(k+1.)
      if(k.gt.1)then
        dd=(2*k+1.)/sqrt((k+3.)*(k-1.))
        ee=sqrt((k+2.)*(k-2.))/(2*k+1.)
        cc=(k+1.)*(k+2.)*(k-2.)/k/(k+3.)/(k-1.)
        bb=(2*k+1.)/k/(k+3.)/(k-1)
        pol(k+1)=dd*(xx*pol(k)-ee*pol(k-1))
        ppl(k+1)=bb*(k*(k+1.)*xx-4.)*ppl(k)-cc*ppl(k-1)
        pml(k+1)=bb*(k*(k+1.)*xx+4.)*pml(k)-cc*pml(k-1)
      endif
      beta(k)=beta(k)+zp(j)*pl(k)*f11(j)*(k+0.5)
      gama(k)=gama(k)+zp(j)*pol(k)*f12(j)*(k+0.5)
      betap(k)=betap(k)+zp(j)*ppl(k)*(f22(j)+f33(j))*(k+0.5)
      betam(k)=betam(k)+zp(j)*pml(k)*(f22(j)-f33(j))*(k+0.5)
      zeta(k)=(betap(k)-betam(k))/2.
      alpa(k)=(betap(k)+betam(k))/2.
      enddo
      enddo
      return
      end

      subroutine legendre(IS,rmu,JJ,LL,PSL,TSL,RSL)
      double precision rmu
      PARAMETER(NN0=85,NG0=91)
      dimension rmu(-NN0:NN0),
     &PSL(-1:2*NG0,-NN0:NN0),RSL(-1:2*NG0,-NN0:NN0),
     &TSL(-1:2*NG0,-NN0:NN0)

      if(IS.ne.0)goto 900
      do J=-LL,LL
      C=RMU(J)
      PSL(0,J)=1.
      PSL(1,J)=C 
      X=(3.*C*C-1.)*0.5
      PSL(2,J)=X
      RSL(1,J)=0.
      X=1.5*(1.-C*C)/sqrt(6.0)
      RSL(2,J)=X
      TSL(1,J)=0.
      TSL(2,J)=0.
      enddo
      goto 500
  900 if(IS.ne.1)goto 901
      do J=-LL,LL
      C=RMU(J)
      X=1.-C*C   
      PSL(0,J)=0.
      PSL(1,J)=SQRT(X*0.5)
      PSL(2,J)=C*PSL(1,J)*sqrt(3.0)
      RSL(1,J)=0
      RSL(2,J)=-C*SQRT(X)*0.5
      TSL(1,J)=0 
      TSL(2,J)=-SQRT(X)*0.5
      enddo
      goto 500
  901 A=1.
      do I=1,IS  
      A=A*SQRT(1.0*(I+IS)/I)*0.5
      enddo
      B=A*SQRT(IS/(IS+1.))*SQRT((IS-1.)/(IS+2.))
      do J=-LL,LL
      C=RMU(J)   
      XX=1.-C*C  
      YY=IS*0.5-1.
      PSL(IS-1,J)=0.
      RSL(IS-1,J)=0.
      TSL(IS-1,J)=0.
      PSL(IS,J)=A*XX**(IS*0.5)
      RSL(IS,J)=B*(1.+C*C)*XX**YY
      TSL(IS,J)=2.*B*C*XX**YY
      enddo
  500 K=2
      if(IS.gt.2)K=IS
      if(K.eq.2*JJ-2)goto 600
      do L=K,2*JJ-1
      LP=L+1
      LM=L-1     
      A=(2*L+1.)/SQRT((L+IS+1.)*(L-IS+1.))
      B=SQRT(FLOAT((L+IS)*(L-IS)))/(2.*L+1.)
      D=(L+1.)*(2*L+1.)/SQRT((L+3.)*(L-1.)*(L+IS+1.)
     &*(L-IS+1.))
      E=SQRT((L+2.)*(L-2.)*(L+IS)*(L-IS))/(L*(2.*L+1))
      F=2.*IS/(L*(L+1.))
      do J=-LL,LL
      C=RMU(J)   
      PSL(LP,J)=A*(C*PSL(L,J)-B*PSL(LM,J))
      RSL(LP,J)=D*(C*RSL(L,J)-F*TSL(L,J)-E*RSL(LM,J))
      TSL(LP,J)=D*(C*TSL(L,J)-F*RSL(L,J)-E*TSL(LM,J))
      enddo
      enddo
  600 continue   
      return     
      end

      subroutine profils(NM,NBC,texttotal,EXT,SSA,JP,ECH,za,NT,H,WD,NAV)
      parameter(NX=5)
      dimension
     &HD(NBC,NX),sdt(NX),EXT(NX),SSA(NX),zint(NBC),
     &HE(NBC,NX),set(NX),WD(NBC-1,NX),H(NBC),JP(NX),ECH(NX)
      dz=0.001
      H(1)=0.0
      H(NT)=texttotal
      do j=1,NM+1
      HD(NT,j)=EXT(j)*SSA(j)
      HE(NT,j)=EXT(j)
      HD(1,j)=0.0
      HE(1,j)=0.0
      enddo
      if(NT.eq.2)then
      do j=1,NM+1
      WD(1,j)=(HD(2,j)-HD(1,j))/(H(2)-H(1))
      enddo
      goto 333
      endif
c IF JP=2
      do j=1,NM
      if(JP(j).eq.2)then
      CCN=0.5*dz*DEN(0.0,JP(j),ECH(j))
      do n=1,40000
      CCN=CCN+dz*DEN(n*dz,JP(j),ECH(j))
      enddo
      EXT(j)=EXT(j)/CCN
      endif
      enddo

      KL=0
      ddz=za
      ttu=0.0
      do j=1,NM+1
      sdt(j)=0.0
      set(j)=0.0
      enddo
      z=-dz
   10 continue
      ICOMP=0
      KL=KL+1
      M=NT-KL
      ttuexp=KL*H(NT)/(NT-1)
   11 continue
      ICOMP=ICOMP+1
      z=z+dz
       xx=0.0
       do j=1,NM+1
       zz=0.5*dz*EXT(j)*(DEN(z,JP(j),ECH(j))+DEN(z+dz,JP(j),ECH(j)))
       set(j)=set(j)+zz
       sdt(j)=sdt(j)+zz*SSA(j)
       xx=xx+zz
       enddo
      ttu=ttu+xx
      if(KNAV.eq.1.or.KNAV.eq.3)goto 222
      ddzn=(za-z)
      if(ddzn*ddz.lt.0.) then
      ddz=ddzn
      NAV=M
      goto 111
      endif
  222 continue
      if(ttu.lt.ttuexp)goto 11
  111 continue
      zint(M)=z
      H(M)=H(NT)-ttu
      do j=1,NM+1
      HD(M,j)=HD(NT,j)-sdt(j)
      HE(M,j)=HE(NT,j)-set(j)
      enddo
      do j=1,NM+1
      WD(M,j)=(HD(M+1,j)-HD(M,j))/(H(M+1)-H(M))
      enddo
      if(KL.lt.NT-2)goto 10
      do j=1,NM+1
      WD(1,j)=HD(2,j)/H(2)
      enddo
c     write(6,1235)'M','zint','tau','t1','tm','p1','pm'
c     do M=1,NT-1
c     write(6,1234)M,zint(M),H(M+1),(HE(M+1,J),j=1,NM+1),
c    &(WD(M,j),j=1,NM+1)
c     enddo
 1235 format(a3,a6,a5,8a7)
 1234 format(i3,f6.3,f5.2,8f7.4)

  333 continue
      return
      end
C*******************SPLINE**INTEREPOLATION****ROUTINES**********************************************
      SUBROUTINE E01BAF(M,X,Y,K,C,LCK,WRK,LWRK,IFAIL)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     ******************************************************
C
C     NPL ALGORITHMS LIBRARY ROUTINE SP3INT
C
C     CREATED 16/5/79.                        RELEASE 00/00
C
C     AUTHORS ... GERALD T. ANTHONY, MAURICE G.COX
C                 J.GEOFFREY HAYES AND MICHAEL A. SINGER.
C     NATIONAL PHYSICAL LABORATORY, TEDDINGTON,
C     MIDDLESEX TW11 OLW, ENGLAND
C
C     ******************************************************
C
C     E01BAF.  AN ALGORITHM, WITH CHECKS, TO DETERMINE THE
C     COEFFICIENTS IN THE B-SPLINE REPRESENTATION OF A CUBIC
C     SPLINE WHICH INTERPOLATES (PASSES EXACTLY THROUGH) A
C     GIVEN SET OF POINTS.
C
C     INPUT PARAMETERS
C        M        THE NUMBER OF DISTINCT POINTS WHICH THE
C                    SPLINE IS TO INTERPOLATE.
C                    (M MUST BE AT LEAST 4.)
C        X        ARRAY CONTAINING THE DISTINCT VALUES OF THE
C                    INDEPENDENT VARIABLE. NB X(I) MUST BE
C                    STRICTLY GREATER THAN X(J) WHENEVER I IS
C                    STRICTLY GREATER THAN J.
C        Y        ARRAY CONTAINING THE VALUES OF THE DEPENDENT
C                    VARIABLE.
C        LCK      THE SMALLER OF THE ACTUALLY DECLARED DIMENSIONS
C                    OF K AND C. MUST BE AT LEAST M + 4.
C
C     OUTPUT PARAMETERS
C        K        ON SUCCESSFUL EXIT, K CONTAINS THE KNOTS
C                    SET UP BY THE ROUTINE. IF THE SPLINE IS
C                    TO BE EVALUATED (BY NPL ROUTINE E02BEF,
C                    FOR EXAMPLE) THE ARRAY K MUST NOT BE
C                    ALTERED BEFORE CALLING THAT ROUTINE.
C        C        ON SUCCESSFUL EXIT, C CONTAINS THE B-SPLINE
C                    COEFFICIENTS OF THE INTERPOLATING SPLINE.
C                    THESE ARE ALSO REQUIRED BY THE EVALUATING
C                    ROUTINE E02BEF.
C        IFAIL    FAILURE INDICATOR
C                    0 - SUCCESSFUL TERMINATION.
C                    1 - ONE OF THE FOLLOWING CONDITIONS HAS
C                        BEEN VIOLATED -
C                        M AT LEAST 4
C                        LK AT LEAST M + 4
C                        LWORK AT LEAST 6 * M + 16
C                    2 - THE VALUES OF THE INDEPENDENT VARIABLE
C                        ARE DISORDERED. IN OTHER WORDS, THE
C                        CONDITION MENTIONED UNDER X IS NOT
C                        SATISFIED.
C
C     WORKSPACE (AND ASSOCIATED DIMENSION) PARAMETERS
C        WRK     WORKSPACE ARRAY, OF LENGTH LWRK.
C        LWRK    ACTUAL DECLARED DIMENSION OF WRK.
C                    MUST BE AT LEAST 6 * M + 16.
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='E01BAF')
C     .. Scalar Arguments ..
      INTEGER           IFAIL, LCK, LWRK, M
C     .. Array Arguments ..
      DOUBLE PRECISION  C(LCK), K(LCK), WRK(LWRK), X(M), Y(M)
C     .. Local Scalars ..
      DOUBLE PRECISION  ONE, SS
      INTEGER           I, IERROR, M1, M2
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
C     .. External Subroutines ..
      EXTERNAL          E02BAF
C     .. Data statements ..
      DATA              ONE/1.0D+0/
C     .. Executable Statements ..
      IERROR = 1
C
C     TESTS FOR ADEQUACY OF ARRAY LENGTHS AND THAT M IS GREATER
C     THAN 4.
C
      IF (LWRK.LT.6*M+16 .OR. M.LT.4) GO TO 80
      IF (LCK.LT.M+4) GO TO 80
C
C     TESTS FOR THE CORRECT ORDERING OF THE X(I)
C
      IERROR = 2
      DO 20 I = 2, M
         IF (X(I).LE.X(I-1)) GO TO 80
   20 CONTINUE
C
C     INITIALISE THE ARRAY OF KNOTS AND THE ARRAY OF WEIGHTS
C
      WRK(1) = ONE
      WRK(2) = ONE
      WRK(3) = ONE
      WRK(4) = ONE
      IF (M.EQ.4) GO TO 60
      DO 40 I = 5, M
         K(I) = X(I-2)
         WRK(I) = ONE
   40 CONTINUE
   60 M1 = M + 1
      M2 = M1 + M
C
C     CALL THE SPLINE FITTING ROUTINE
C
      IERROR = 0
      CALL E02BAF(M,M+4,X,Y,WRK,K,WRK(M1),WRK(M2),C,SS,IERROR)
C
C     ALL THE TESTS PERFORMED BY E02BAF ARE REDUNDANT
C     BECAUSE OF THE ABOVE TESTS AND ASSIGNMENTS, AND SO
C     IERROR = 0 AFTER THIS CALL.
C
   80 IFAIL = P01ABF(IFAIL,IERROR,SRNAME,0,P01REC)
      RETURN
C
C     END OF E01BAF.
C
      END
C**********************************************************************************************************************
      SUBROUTINE E02BAF(M,NCAP7,X,Y,W,K,WORK1,WORK2,C,SS,IFAIL)
C     NAG COPYRIGHT 1975
C     MARK 5 RELEASE
C     MARK 6 REVISED  IER-84
C     MARK 8 RE-ISSUE. IER-224 (APR 1980).
C     MARK 9A REVISED. IER-356 (NOV 1981)
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     NAG LIBRARY SUBROUTINE  E02BAF
C
C     E02BAF  COMPUTES A WEIGHTED LEAST-SQUARES APPROXIMATION
C     TO AN ARBITRARY SET OF DATA POINTS BY A CUBIC SPLINE
C     WITH KNOTS PRESCRIBED BY THE USER.  CUBIC SPLINE
C     INTERPOLATION CAN ALSO BE CARRIED OUT.
C
C     COX-DE BOOR METHOD FOR EVALUATING B-SPLINES WITH
C     ADAPTATION OF GENTLEMAN*S PLANE ROTATION SCHEME FOR
C     SOLVING OVER-DETERMINED LINEAR SYSTEMS.
C
C     USES NAG LIBRARY ROUTINE  P01AAF.
C
C     STARTED - 1973.
C     COMPLETED - 1976.
C     AUTHOR - MGC AND JGH.
C
C     REDESIGNED TO USE CLASSICAL GIVENS ROTATIONS IN
C     ORDER TO AVOID THE OCCASIONAL UNDERFLOW (AND HENCE
C     OVERFLOW) PROBLEMS EXPERIENCED BY GENTLEMAN*S 3-
C     MULTIPLICATION PLANE ROTATION SCHEME
C
C     WORK1  AND  WORK2  ARE WORKSPACE AREAS.
C     WORK1(R)  CONTAINS THE VALUE OF THE  R TH  DISTINCT DATA
C     ABSCISSA AND, SUBSEQUENTLY, FOR  R = 1, 2, 3, 4,  THE
C     VALUES OF THE NON-ZERO B-SPLINES FOR EACH SUCCESSIVE
C     ABSCISSA VALUE.
C     WORK2(L, J)  CONTAINS, FOR  L = 1, 2, 3, 4,  THE VALUE OF
C     THE  J TH  ELEMENT IN THE  L TH  DIAGONAL OF THE
C     UPPER TRIANGULAR MATRIX OF BANDWIDTH  4  IN THE
C     TRIANGULAR SYSTEM DEFINING THE B-SPLINE COEFFICIENTS.
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='E02BAF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION  SS
      INTEGER           IFAIL, M, NCAP7
C     .. Array Arguments ..
      DOUBLE PRECISION  C(NCAP7), K(NCAP7), W(M), WORK1(M),
     *                  WORK2(4,NCAP7), X(M), Y(M)
C     .. Local Scalars ..
      DOUBLE PRECISION  ACOL, AROW, CCOL, COSINE, CROW, D, D4, D5, D6,
     *                  D7, D8, D9, DPRIME, E2, E3, E4, E5, K0, K1, K2,
     *                  K3, K4, K5, K6, N1, N2, N3, RELEMT, S, SIGMA,
     *                  SINE, WI, XI
      INTEGER           I, IERROR, IPLUSJ, IU, J, JOLD, JPLUSL, JREV, L,
     *                  L4, LPLUS1, LPLUSU, NCAP, NCAP3, NCAPM1, R
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, SQRT
C     .. Executable Statements ..
      IERROR = 4
C     CHECK THAT THE VALUES OF  M  AND  NCAP7  ARE REASONABLE
      IF (NCAP7.LT.8 .OR. M.LT.NCAP7-4) GO TO 420
      NCAP = NCAP7 - 7
      NCAPM1 = NCAP - 1
      NCAP3 = NCAP + 3
C
C     IN ORDER TO DEFINE THE FULL B-SPLINE BASIS, AUGMENT THE
C     PRESCRIBED INTERIOR KNOTS BY KNOTS OF MULTIPLICITY FOUR
C     AT EACH END OF THE DATA RANGE.
C
      DO 20 J = 1, 4
         I = NCAP3 + J
         K(J) = X(1)
         K(I) = X(M)
   20 CONTINUE
C
C     TEST THE VALIDITY OF THE DATA.
C
C     CHECK THAT THE KNOTS ARE ORDERED AND ARE INTERIOR
C     TO THE DATA INTERVAL.
C
      IERROR = 1
      IF (K(5).LE.X(1) .OR. K(NCAP3).GE.X(M)) GO TO 420
      DO 40 J = 4, NCAP3
         IF (K(J).GT.K(J+1)) GO TO 420
   40 CONTINUE
C
C     CHECK THAT THE WEIGHTS ARE STRICTLY POSITIVE.
C
      IERROR = 2
      DO 60 I = 1, M
         IF (W(I).LE.0.0D0) GO TO 420
   60 CONTINUE
C
C     CHECK THAT THE DATA ABSCISSAE ARE ORDERED, THEN FORM THE
C     ARRAY  WORK1  FROM THE ARRAY  X.  THE ARRAY  WORK1  CONTAINS
C     THE
C     SET OF DISTINCT DATA ABSCISSAE.
C
      IERROR = 3
      WORK1(1) = X(1)
      J = 2
      DO 80 I = 2, M
         IF (X(I).LT.WORK1(J-1)) GO TO 420
         IF (X(I).EQ.WORK1(J-1)) GO TO 80
         WORK1(J) = X(I)
         J = J + 1
   80 CONTINUE
      R = J - 1
C
C     CHECK THAT THERE ARE SUFFICIENT DISTINCT DATA ABSCISSAE FOR
C     THE PRESCRIBED NUMBER OF KNOTS.
C
      IERROR = 4
      IF (R.LT.NCAP3) GO TO 420
C
C     CHECK THE FIRST  S  AND THE LAST  S  SCHOENBERG-WHITNEY
C     CONDITIONS ( S = MIN(NCAP - 1, 4) ).
C
      IERROR = 5
      DO 100 J = 1, 4
         IF (J.GE.NCAP) GO TO 160
         I = NCAP3 - J + 1
         L = R - J + 1
         IF (WORK1(J).GE.K(J+4) .OR. K(I).GE.WORK1(L)) GO TO 420
  100 CONTINUE
C
C     CHECK ALL THE REMAINING SCHOENBERG-WHITNEY CONDITIONS.
C
      IF (NCAP.LE.5) GO TO 160
      R = R - 4
      I = 4
      DO 140 J = 5, NCAPM1
         K0 = K(J+4)
         K4 = K(J)
  120    I = I + 1
         IF (WORK1(I).LE.K4) GO TO 120
         IF (I.GT.R .OR. WORK1(I).GE.K0) GO TO 420
  140 CONTINUE
C
C     INITIALISE A BAND TRIANGULAR SYSTEM (I.E. A
C     MATRIX AND A RIGHT HAND SIDE) TO ZERO. THE
C     PROCESSING OF EACH DATA POINT IN TURN RESULTS
C     IN AN UPDATING OF THIS SYSTEM. THE SUBSEQUENT
C     SOLUTION OF THE RESULTING BAND TRIANGULAR SYSTEM
C     YIELDS THE COEFFICIENTS OF THE B-SPLINES.
C
  160 DO 200 I = 1, NCAP3
         DO 180 L = 1, 4
            WORK2(L,I) = 0.0D0
  180    CONTINUE
         C(I) = 0.0D0
  200 CONTINUE
      SIGMA = 0.0D0
      J = 0
      JOLD = 0
      DO 340 I = 1, M
C
C        FOR THE DATA POINT  (X(I), Y(I))  DETERMINE AN INTERVAL
C        K(J + 3) .LE. X .LT. K(J + 4)  CONTAINING  X(I).  (IN THE
C        CASE  J + 4 .EQ. NCAP  THE SECOND EQUALITY IS RELAXED TO
C        INCLUDE
C        EQUALITY).
C
         WI = W(I)
         XI = X(I)
  220    IF (XI.LT.K(J+4) .OR. J.GT.NCAPM1) GO TO 240
         J = J + 1
         GO TO 220
  240    IF (J.EQ.JOLD) GO TO 260
C
C        SET CERTAIN CONSTANTS RELATING TO THE INTERVAL
C        K(J + 3) .LE. X .LE. K(J + 4).
C
         K1 = K(J+1)
         K2 = K(J+2)
         K3 = K(J+3)
         K4 = K(J+4)
         K5 = K(J+5)
         K6 = K(J+6)
         D4 = 1.0D0/(K4-K1)
         D5 = 1.0D0/(K5-K2)
         D6 = 1.0D0/(K6-K3)
         D7 = 1.0D0/(K4-K2)
         D8 = 1.0D0/(K5-K3)
         D9 = 1.0D0/(K4-K3)
         JOLD = J
C
C        COMPUTE AND STORE IN  WORK1(L) (L = 1, 2, 3, 4)  THE VALUES
C        OF
C        THE FOUR NORMALIZED CUBIC B-SPLINES WHICH ARE NON-ZERO AT
C        X=X(I).
C
  260    E5 = K5 - XI
         E4 = K4 - XI
         E3 = XI - K3
         E2 = XI - K2
         N1 = WI*D9
         N2 = E3*N1*D8
         N1 = E4*N1*D7
         N3 = E3*N2*D6
         N2 = (E2*N1+E5*N2)*D5
         N1 = E4*N1*D4
         WORK1(4) = E3*N3
         WORK1(3) = E2*N2 + (K6-XI)*N3
         WORK1(2) = (XI-K1)*N1 + E5*N2
         WORK1(1) = E4*N1
         CROW = Y(I)*WI
C
C        ROTATE THIS ROW INTO THE BAND TRIANGULAR SYSTEM USING PLANE
C        ROTATIONS.
C
         DO 320 LPLUS1 = 1, 4
            L = LPLUS1 - 1
            RELEMT = WORK1(LPLUS1)
            IF (RELEMT.EQ.0.0D0) GO TO 320
            JPLUSL = J + L
            L4 = 4 - L
            D = WORK2(1,JPLUSL)
            IF (ABS(RELEMT).GE.D) DPRIME = ABS(RELEMT)
     *          *SQRT(1.0D0+(D/RELEMT)**2)
            IF (ABS(RELEMT).LT.D) DPRIME = D*SQRT(1.0D0+(RELEMT/D)**2)
            WORK2(1,JPLUSL) = DPRIME
            COSINE = D/DPRIME
            SINE = RELEMT/DPRIME
            IF (L4.LT.2) GO TO 300
            DO 280 IU = 2, L4
               LPLUSU = L + IU
               ACOL = WORK2(IU,JPLUSL)
               AROW = WORK1(LPLUSU)
               WORK2(IU,JPLUSL) = COSINE*ACOL + SINE*AROW
               WORK1(LPLUSU) = COSINE*AROW - SINE*ACOL
  280       CONTINUE
  300       CCOL = C(JPLUSL)
            C(JPLUSL) = COSINE*CCOL + SINE*CROW
            CROW = COSINE*CROW - SINE*CCOL
  320    CONTINUE
         SIGMA = SIGMA + CROW**2
  340 CONTINUE
      SS = SIGMA
C
C     SOLVE THE BAND TRIANGULAR SYSTEM FOR THE B-SPLINE
C     COEFFICIENTS. IF A DIAGONAL ELEMENT IS ZERO, AND HENCE
C     THE TRIANGULAR SYSTEM IS SINGULAR, THE IMPLICATION IS
C     THAT THE SCHOENBERG-WHITNEY CONDITIONS ARE ONLY JUST
C     SATISFIED. THUS IT IS APPROPRIATE TO EXIT IN THIS
C     CASE WITH THE SAME VALUE  (IFAIL=5)  OF THE ERROR
C     INDICATOR.
C
      L = -1
      DO 400 JREV = 1, NCAP3
         J = NCAP3 - JREV + 1
         D = WORK2(1,J)
         IF (D.EQ.0.0D0) GO TO 420
         IF (L.LT.3) L = L + 1
         S = C(J)
         IF (L.EQ.0) GO TO 380
         DO 360 I = 1, L
            IPLUSJ = I + J
            S = S - WORK2(I+1,J)*C(IPLUSJ)
  360    CONTINUE
  380    C(J) = S/D
  400 CONTINUE
      IERROR = 0
  420 IF (IERROR) 440, 460, 440
  440 IFAIL = P01ABF(IFAIL,IERROR,SRNAME,0,P01REC)
      RETURN
  460 IFAIL = 0
      RETURN
      END
C***************************************************************************************************************************
      SUBROUTINE E02BBF(NCAP7,K,C,X,S,IFAIL)
C     NAG LIBRARY SUBROUTINE  E02BBF
C
C     E02BBF  EVALUATES A CUBIC SPLINE FROM ITS
C     B-SPLINE REPRESENTATION.
C
C     DE BOOR*S METHOD OF CONVEX COMBINATIONS.
C
C     USES NAG LIBRARY ROUTINE  P01AAF.
C
C     STARTED - 1973.
C     COMPLETED - 1976.
C     AUTHOR - MGC AND JGH.
C
C     NAG COPYRIGHT 1975
C     MARK 5 RELEASE
C     MARK 7 REVISED IER-141 (DEC 1978)
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='E02BBF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION  S, X
      INTEGER           IFAIL, NCAP7
C     .. Array Arguments ..
      DOUBLE PRECISION  C(NCAP7), K(NCAP7)
C     .. Local Scalars ..
      DOUBLE PRECISION  C1, C2, C3, E2, E3, E4, E5, K1, K2, K3, K4, K5,
     *                  K6
      INTEGER           IERROR, J, J1, L
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
C     .. Executable Statements ..
      IERROR = 0
      IF (NCAP7.GE.8) GO TO 20
      IERROR = 2
      GO TO 120
   20 IF (X.GE.K(4) .AND. X.LE.K(NCAP7-3)) GO TO 40
      IERROR = 1
      S = 0.0D0
      GO TO 120
C
C     DETERMINE  J  SUCH THAT  K(J + 3) .LE. X .LE. K(J + 4).
C
   40 J1 = 0
      J = NCAP7 - 7
   60 L = (J1+J)/2
      IF (J-J1.LE.1) GO TO 100
      IF (X.GE.K(L+4)) GO TO 80
      J = L
      GO TO 60
   80 J1 = L
      GO TO 60
C
C     USE THE METHOD OF CONVEX COMBINATIONS TO COMPUTE  S(X).
C
  100 K1 = K(J+1)
      K2 = K(J+2)
      K3 = K(J+3)
      K4 = K(J+4)
      K5 = K(J+5)
      K6 = K(J+6)
      E2 = X - K2
      E3 = X - K3
      E4 = K4 - X
      E5 = K5 - X
      C2 = C(J+1)
      C3 = C(J+2)
      C1 = ((X-K1)*C2+E4*C(J))/(K4-K1)
      C2 = (E2*C3+E5*C2)/(K5-K2)
      C3 = (E3*C(J+3)+(K6-X)*C3)/(K6-K3)
      C1 = (E2*C2+E4*C1)/(K4-K2)
      C2 = (E3*C3+E5*C2)/(K5-K3)
      S = (E3*C2+E4*C1)/(K4-K3)
  120 IF (IERROR) 140, 160, 140
  140 IFAIL = P01ABF(IFAIL,IERROR,SRNAME,0,P01REC)
      RETURN
  160 IFAIL = 0
      RETURN
      END
C***************************************************************************************************************
