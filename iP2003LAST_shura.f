	SUBROUTINE iP2003LAST_shura(fname)
	USE ALLOC
	USE ALLOC1
	USE mo_par_DLS
C***         iM.f
C******************************************************
C*** THIS PROGRAM IS TO RETRIVE SIZE DISTRIBUTION
C***           AND REFRACTIVE INDEX
C***    FROM "PHASE FUNCTION" AND "EXTINCTION"
C***                   OR
C***            FROM SUN/SKY RADIANCE
C******************************************************
C*** This program implements non-linear system
C*** least square solution using general scheme.
C*** The general scheme of p and q iterations is applied
C****************************************************** 
ctl(1-scal,0-vect),IPOLP(0-w/o pol,1-w pol)

ctl version with Benjamin Torres modifications

	  
c      include 'optchar.par'

!tl      PARAMETER (KSD=3,KW=10,KMES=500,KPAR=100,KMD=3,MAXCLY=35,
!tl     &KANG=83,KBRDF=13,KSDL=6,KNA0=2,KSHAPE=2)
      PARAMETER (KSD=3,KW=10,KMES=500,KPAR=100,MAXCLY=35,
     &KANG=181,KBRDF=13,KSDL=6,KNA0=2,KSHAPE=2)

      INTEGER MAXUMU,IFAIL
      PARAMETER (MAXUMU=100)
      CHARACTER ERR*64,CH1*64
      INTEGER NBIN(KSD),ISTAR(KPAR),SOLC(KW),NBINF(MAXCLY,KSD),
     &NBINC(MAXCLY,KSD),NBINCR(KSD),NBIN1(KSD),NBINCR1(KSD),
     &IREFLOC(MAXCLY,KSD,2),ISDLOC(MAXCLY,KSD),IBRDFLOC(KBRDF)
     & ,IBRDF1LOC(KBRDF),IMIN(MAXCLY,KSD),in3(KMES),IPOLPR(KW)
     & ,IO(KPAR),INN(KMES),IK(KMES),KNOISE(KMES,KMES),FTAU(KW),
     &INDSK(KW),INDSAT(KW),INDTAU(KW),icount(4),idmin(4),
     &INDORDER(KW),FUMUY(KW,MAXCLY,KNA0),
     &FPHI(KW,MAXCLY,KNA0,MAXUMU),ISHAPELOC(KSHAPE)
       REAL LINEAR2,xlagsm
      DOUBLE PRECISION UFI(KPAR,KPAR)
      DIMENSION WAVE(KW),RADIUS(KSD,KPAR),RMIN(KSD),RMAX(KSD),
     &BRDFMIN(KBRDF),BRDFMAX(KBRDF),BRDF1MIN(KBRDF),BRDF1MAX(KBRDF)
     &,GSM(KPAR),SGMS(KMES),DNN(KMES),FPT(KMES),FP1T(KMES),
     &RDM(KMES),ANG(KANG),FP(KMES),FP1(KMES),FF(KPAR),FPC(KMES),
     &FFS(KPAR),SM(KMES,KMES),SSA(KW,MAXCLY,KSD),UF(KPAR,KPAR),
     &PF(KANG,KW,MAXCLY,KSD),F(KMES),ALS(KW),BIAS(KW),BIASG(KMES),
     &DLP(KANG,KW,MAXCLY,KSD),DLPF(KANG,KW,MAXCLY,KSD),ABIAS(KPAR,KPAR),
     &DLPC(KANG,KW,MAXCLY,KSD),BIASS(KW),BIASP(KW),FB(KPAR),
     &SHAPEMIN(KSHAPE),SHAPEMAX(KSHAPE),WAVEL(KW),TAUA(KW),
     &anstr_mod(2),xlags(2),xint(2)
cs*********************LAST****CORRECTION*********************************************
      DIMENSION BIASGL(KMES),BIASGH(KMES),BIASG1L(KMES),BIASG1H(KMES),
     & ABIAS3(KPAR,KPAR),ABIAS4(KPAR,KPAR)
cs************************************************************************************
      DIMENSION AP(KPAR),Q(KPAR),Q0(KPAR),U(KMES,KPAR),BIASGG(KMES),
     &C(KMES,KMES),APMIN(KPAR),ALM(KPAR),APMID(KPAR),
     &NPAR(KPAR),XX(KPAR,KMES),FPMID(KMES),FPMIN(KMES),FPMAX(KMES),
     &EXT(KW,MAXCLY,KSD),AF(KPAR),SM0(KMES,KMES),C1(KMES,KMES),
     &FLDD(KW),FLUP(KW),FLDR(KW),SDM(MAXCLY,KSD,7),SDMIN(MAXCLY,KSD),
     &PFF(KANG,KW,MAXCLY,KSD),PFC(KANG,KW,MAXCLY,KSD),
     &EXTF(KW,MAXCLY,KSD),EXTC(KW,MAXCLY,KSD),ALBEDOR(KW),
     &SSAF(KW,MAXCLY,KSD),SSAC(KW,MAXCLY,KSD),SDF(MAXCLY,KSD,KPAR),
     &SDC(MAXCLY,KSD,KPAR),RADIUSF(KSD,KPAR),RADIUSC(KSD,KPAR),
     &XNSDF(MAXCLY,KSD,KPAR),XNSDC(MAXCLY,KSD,KPAR),dlnr(KSD),
     &rf3(KSD),rf2(KSD),rf(KSD),rc3(KSD),rc2(KSD),rc(KSD),
     &rfl1(KSD),rfl2(KSD),rfl(KSD),rcl1(KSD),rcl2(KSD),rcl(KSD),
     &sf(KSD),rflc(KSD),sc(KSD),rclc(KSD),rt(KSD),rft(KSD),
     &st(KSD),rct(KSD),XNSD(MAXCLY,KSD,KPAR),SDT(MAXCLY,KSD,KPAR),
     &ANGLESCA(KW,KNA0,KMES),ANGLE(KMES),FW(KMES),ACOV(KPAR),
     &UFF(KPAR,KPAR),BIASG1(KMES),ACOV1(KPAR),ACOV2(KPAR),ACOVR(KPAR),
     &APMAX(KPAR),SSAMIN(KW,MAXCLY,KSD),
     &SSAMAX(KW,MAXCLY,KSD),SSAMIN1(KW,MAXCLY,KSD),
     &SSAMAX1(KW,MAXCLY,KSD),BIASGS(KW,KMES),BIASGP(KW,KMES)
      DIMENSION US(KW,MAXCLY,KSD,KPAR),UP(KANG,KW,MAXCLY,KSD,KPAR),
     &BP(KPAR),BF(KPAR),CS(KW,MAXCLY,KSD),BP1(KPAR),
     &BP2(KPAR),CSB1(KW,MAXCLY,KSD),CSB2(KW,MAXCLY,KSD),
     &CPB1(KANG,KW,MAXCLY,KSD),CPB2(KANG,KW,MAXCLY,KSD),
     &CP(KANG,KW,MAXCLY,KSD),UPOL(KANG,KW,MAXCLY,KSD,KPAR),
     &DPL(KANG,KW,MAXCLY,KSD),CPBP1(KANG,KW,MAXCLY,KSD),
     &CPBP2(KANG,KW,MAXCLY,KSD),CPP(KANG,KW,MAXCLY,KSD),
     &CSM(KMES),CPBM1(KMES),CPBM2(KMES),ABIAS1(KPAR,KPAR),
     &ABIAS2(KPAR,KPAR),APMINS(KPAR),APMAXS(KPAR),ABIASC(KPAR)
      DIMENSION UFFF(KPAR,KPAR),Z(KPAR)
C***** For A Priori Constraints **********************
      DIMENSION AP0(KPAR),AP0N(KPAR),FFS0(KPAR),GSM0(KPAR)
      INTEGER IO0(KPAR),inw(KW),inws(KW),inwp(KW),inwt(KW),
     &inwa(KW,KNA0)
C***************************************forcing***************************
      PARAMETER (MAXULV=10)
      DIMENSION ALTIT(MAXULV),FLXINTA(MAXULV,4)
      DOUBLE PRECISION FORC_BOT,FORC_TOA,TAUR(KW,MAXCLY+1)
	character*100 fname
C*****************************************************************************
      DOUBLE PRECISION RDM0
C*****************************************************
C***  for SD initial guess:
      DIMENSION CMZ(KMD,KSD),SMZ(KMD,KSD),RMMZ(KMD,KSD),
     &RMINZ(KSD),RMAXZ(KSD),APZ(KSD,KPAR)
      INTEGER KNZ(KSD),IS(KSD)
C********************************************************
      INTEGER COND
	CHARACTER*100 STNSSTR
      COMMON /FORCI/ COND
      COMMON /FLX/ ALTIT,FLXINTA,FORC_TOA,FORC_BOT,EF_TOA,EF_BOT
      COMMON /RAY/ TAUR,NLYRS
      COMMON /INIT/ APIN
	COMMON /STNS/ STNSSTR

!=============== BY DUAN=========================================      
      REAL ACZ(2)
!===============END BY DUAN=========================================
	      
      IPRI2=0
C*****************************************************
C*** Reading initial data
C*****************************************************
C	fname='Ulumqi591080217053047'
	OPEN(981,FILE='INPUT\'//TRIM(STNSSTR)//'\'//trim(fname)//'.TXT'
     &	,STATUS='OLD')
C********************************************************
      OPEN (1011,FILE='OUTPUT\'//TRIM(STNSSTR)//'\'//trim(fname)//'.dat'
     &	,status='unknown')
c*********************************************************
      READ (981,*) KM,KN,KL,IT,ISZ,IMSC,IMSC1,ISTOP,IEL
      IF(IPRI2.EQ.1) WRITE(*,*) KM,KN,KL,IT,ISZ,IMSC,IMSC1,ISTOP,IEL
     &, 'KM,KN,KL,IT,ISZ,IMSC,IMSC1,ISTOP,IEL' 
C*****************************************************
C*** KM=  total number of the measurements 
C*** KN=  total number of the retrieved parameters
C*** KL=0 - minimization of absolute errors
C*** KL=1 - minimization of log
C*** IT=0 - refractive index is assumed
C*** IT=-1 - refractive index is retrieved
C*** ISZ=0 initial guess for SD are read point by point
C*** ISZ=1 - SD is assumed lognormal with the parameters
C***          from FILE="SDguess.dat"
C*** IMSC =0 -multiple scattering regime for signal
C*** IMSC =1 -single scattering regime for signal
C*** IMSC1 =0 -multiple scattering regime for sim. matrix
C*** IMSC1 =1 -single scattering regime for sim. matrix
C*****************************************************
c*********************************************************

c ** FOR single scattering aerosol OPTICAL properties !!!*
      key=4
      keyEL=IEL
!       keyEL=2
c
c *** ALLOCATE ARRAYS to be used in subroutine USMATRIX 
c *** (in matrix_intrpl.f)
c
      IF(key.lt.4) THEN 
c
c *** ALLOCATE ARRAYS (alloc.mod)
c
        ALLOCATE(UFEA(2,KN1par,KIMpar,KREpar),    stat=ierr)
        if(ierr/=0) stop 'Can not allocate UFEA array'
        UFEA=0.
        if(keyEL.gt.0) then
        ALLOCATE(UF11(KMpar,KN1par,KIMpar,KREpar),stat=ierr)
        if(ierr/=0) stop 'Can not allocate UF11 array'
        UF11=0.

        if(keyEL.gt.1) then
          ALLOCATE(UF12(KMpar,KN1par,KIMpar,KREpar),stat=ierr)
          if(ierr/=0) stop 'Can not allocate UF12 array'
          UF12=0.
        endif
        if(keyEL.gt.2) then				
          ALLOCATE(UF22(KMpar,KN1par,KIMpar,KREpar),stat=ierr)
          if(ierr/=0) stop 'Can not allocate UF22 array'
          UF22=0.		  
        endif
        if(keyEL.gt.3) then
          ALLOCATE(UF33(KMpar,KN1par,KIMpar,KREpar),stat=ierr)
          if(ierr/=0) stop 'Can not allocate UF33 array'
          UF33=0.
        endif
        if(keyEL.gt.4) then
          ALLOCATE(UF34(KMpar,KN1par,KIMpar,KREpar),stat=ierr)
          if(ierr/=0) stop 'Can not allocate UF34 array'
          UF34=0.
        endif
        if(keyEL.gt.5) then		  
          ALLOCATE(UF44(KMpar,KN1par,KIMpar,KREpar),stat=ierr)
          if(ierr/=0) stop 'Can not allocate UF44 array'
          UF44=0.
	    endif 
        ENDIF ! keyEL
      ENDIF ! key<4

      IF(key.gt.2) THEN 
c
c *** ALLOCATE ARRAYS (alloc1.mod)
c
        ALLOCATE(UEA(KR1par,2,KN1par,KIMpar,KREpar),    stat=ierr)
CXXA        if(ierr/=0) stop 'Can not allocate UEA array'
        UEA=0.
        if(keyEL.gt.0) then
	    ALLOCATE(U11(KR1par,KMpar,KN1par,KIMpar,KREpar),stat=ierr)
CXXA       if(ierr/=0) stop 'Can not allocate U11 array'
        U11=0. 

        if(keyEL.gt.1) then
          ALLOCATE(U12(KR1par,KMpar,KN1par,KIMpar,KREpar),stat=ierr)
CXXA          if(ierr/=0) stop 'Can not allocate U12 array'
          U12=0.
        endif
        if(keyEL.gt.2) then
          ALLOCATE(U22(KR1par,KMpar,KN1par,KIMpar,KREpar),stat=ierr)
CXXA          if(ierr/=0) stop 'Can not allocate U22 array'
          U22=0.
        endif
        if(keyEL.gt.3) then
		  ALLOCATE(U33(KR1par,KMpar,KN1par,KIMpar,KREpar),stat=ierr)
CXXA          if(ierr/=0) stop 'Can not allocate U33 array'
          U33=0.
        endif
	    if(keyEL.gt.4) then
          ALLOCATE(U34(KR1par,KMpar,KN1par,KIMpar,KREpar),stat=ierr)
CXXA          if(ierr/=0) stop 'Can not allocate U34 array'
          U34=0.
        endif
	    if(keyEL.gt.5) then
          ALLOCATE(U44(KR1par,KMpar,KN1par,KIMpar,KREpar),stat=ierr)
CXXA          if(ierr/=0) stop 'Can not allocate U44 array'
          U44=0.
	    endif 
        ENDIF ! keyEL
      ENDIF ! key.gt.2    
c
c ***
c

      READ(981,*) NSD,NW,NLYR,NBRDF,NBRDF1,NSHAPE,IEND
      IF(IPRI2.EQ.1) WRITE(*,*) NSD,NW,NLYR,NBRDF,NBRDF1,NSHAPE,IEND
     &,'NSD,NW,NLYR,NBRDF,NBRDF1,NSHAPE,IEND'
      READ(981,*) (WAVE(IW),IW=1,NW) 
      IF(IPRI2.EQ.1) WRITE(*,*) (WAVE(IW),IW=1,NW) 
     &,'(WAVE(IW),IW=1,NW) '
      READ(981,*)(INDSK(IW),IW=1,NW)
      IF(IPRI2.EQ.1) WRITE(*,*) (INDSK(IW),IW=1,NW) 
     &,'(INDSK(IW),IW=1,NW) '
      READ(981,*)(INDSAT(IW),IW=1,NW)
      IF(IPRI2.EQ.1) WRITE(*,*) (INDSAT(IW),IW=1,NW) 
     &,'(INDSAT(IW),IW=1,NW) '
      READ(981,*)(INDTAU(IW),IW=1,NW)
      IF(IPRI2.EQ.1) WRITE(*,*) (INDTAU(IW),IW=1,NW) 
     &,'(INDTAU(IW),IW=1,NW) '
       READ(981,*)(INDORDER(IW),IW=1,NW)
      IF(IPRI2.EQ.1) WRITE(*,*) (INDORDER(IW),IW=1,NW) 
     &,'(INDORDER(IW),IW=1,NW)'
C*****************************************************
C***  NSD - number of the retrieved aerosol component
C***        at each atmospheric layer
C***  NW   - the number of wavelengths
C***  NLYR - the number of the athmosperic layers
C***  NBRDF- the number of the BRDF paremeters for
C***          each wavelength
C***  if NBRDF=0 when we use Lambertian approximation
C***  WAVE(IW) - values of wavelengths
C***  NBRDF1- the number of the BRDF paremeters 
C***          independent of wavelenths 
C****************************************************
      READ(981,*) IBIN, (NBIN(I),I=1,NSD)
      IF(IPRI2.EQ.1) WRITE(*,*) IBIN, (NBIN(I),I=1,NSD)
     & ,' IBIN, (NBIN(I),I=1,NSD)'
C****************************************************
C***  NBIN(NSD) - the number of the bins in 
C***                 the size distributions
C***  IBIN - index determining the binning of SD:
C***         = -1 equal in logarithms
C***         =  1 equal in absolute scale
C***         =  0 read from the file
C****************************************************
      IF (IBIN.EQ.0) THEN
		DO ISD=1,NSD
			READ(981,*) (RADIUS(ISD,I),I=1,NBIN(ISD))
			IF(IPRI2.EQ.1) WRITE(*,*) (RADIUS(ISD,I),I=1,NBIN(ISD))
     &		,'(RADIUS(ISD,I),I=1,NBIN(ISD))'
C****************************************************
C***  RADIUS(NSD,KBIN) - the radii corresponding to bins 
C***                     in the size distributions
C***  IBIN - index determining the binning of SD:
C***         = -1 equal in logarithms
C***         =  1 equal in absolute scale
C***         =  0 read from the file
C****************************************************
		ENDDO
      ENDIF

      IF (IBIN.EQ.1.OR.IBIN.EQ.-1) THEN
		READ(981,*) (RMIN(ISD),RMAX(ISD),IS(ISD),ISD=1,NSD)
		IF(IPRI2.EQ.1)  WRITE(*,*) (RMIN(ISD),RMAX(ISD),ISD=1,NSD)     
     &	,'(RMIN(ISD),RMAX(ISD),IS(ISD),ISD=1,NSD)'
		DO ISD=1,NSD
			IF(IBIN.EQ.1) 
     &			AD=(RMAX(ISD)-RMIN(ISD))/(NBIN(ISD)-1)
			IF(IBIN.EQ.-1) 
     &			AD=(LOG(RMAX(ISD))-LOG(RMIN(ISD)))/(NBIN(ISD)-1)
			IF(IPRI2.EQ.1)  WRITE(*,*) NBIN(ISD),' NBIN(ISD)'
			DO J=1,NBIN(ISD)
				IF(IBIN.EQ.1) 
     &			RADIUS(ISD,J)=RMIN(ISD)+AD*(J-1)
				IF(IBIN.EQ.-1) 
     &			RADIUS(ISD,J)=EXP(LOG(RMIN(ISD))+AD*(J-1))
				IF(IPRI2.EQ.1)  WRITE(*,*) RADIUS(ISD,J), ISD, J,' RADIUS'
			ENDDO 
		ENDDO
      ENDIF
C********************Forcing************************
COND: =0 calculate the radiative forcing with the 
C        final values of the Refractive index and 
C        size distribution    
c       =1 no calculate
C**************************************************
      IF(ISTOP.EQ.1) THEN
		COND=0 
      ELSE
		COND=1
      ENDIF	

c***************************************************
C****************************************************
C***  ANGL(NANG) - the agles for the phase function 
C***************************************************
C***************************************************
C*** !!! Checking number of the retrieved parameters KN
C***************************************************
CD      WRITE(*,*) NW, NBRDF,' NW, NBRDF'
      KNCHECK=NW*NBRDF+NBRDF1+NSHAPE
      DO ISD=1,NSD
		KNCHECK=KNCHECK+NLYR*NBIN(ISD)
		IF(IT.LT.0) KNCHECK=KNCHECK+NLYR*NW*2
      ENDDO 
      IF(KN.NE.KNCHECK) write(*,*) KN, 
     &KNCHECK," KN.NE.KNCHECK!!!"
C******************************************************
C***  Definition of the places of the parameters (Real Refr. 
C***  Index, Imag. Refr. Ind,Size. Distr, and BRDF) in
C***  the retrieved vector of unknowns AP
C******************************************************
C***  THE FOLLOWING ORDER IS ASSUMED:
C***    LOOP looking through all layers:
C***     LOOP looking through NSD
C***  NW values of REAL parts of ref. index
C***  NW values of IMAG parts of ref. index
C***  NBIN(ISD) values of SIZE distr. points
C***     END of NSD LOOP
C***    END of layer's LOOP
C***    LOOP looking through NBRDF
C***  NW values of each BRDF  parameter
C***    END of NBRDF LOOP 
      DO I=1,KN
		NPAR(I)=0.
      ENDDO
      IF(IT.LT.0) THEN
		IPAR=0
		DO IL=1,NLYR
			DO ISD=1,NSD
				IF(IPAR.NE.0) ISTAR(IPAR+1)=
     &			ISTAR(IPAR)+NPAR(IPAR)
				IF(IPAR.EQ.0) ISTAR(IPAR+1)=1
C************************************************
C*** ISTAR(NPAR) - the address of the values
C***            corresponding to IPAR in AP()
				NPAR(IPAR+1)=NW
				ISTAR(IPAR+2)=ISTAR(IPAR+1)+NPAR(IPAR+1)
				NPAR(IPAR+2)=NW
				ISTAR(IPAR+3)=ISTAR(IPAR+2)+NPAR(IPAR+2)
				NPAR(IPAR+3)=NBIN(ISD)
				IREFLOC(IL,ISD,1)=IPAR+1
				IREFLOC(IL,ISD,2)=IPAR+2
				ISDLOC(IL,ISD)=IPAR+3
				IPAR=IPAR+3
			ENDDO
		ENDDO
		IF(NBRDF.GT.0) THEN
			DO IB=1,NBRDF
				ISTAR(IPAR+1)=ISTAR(IPAR)+NPAR(IPAR)
				NPAR(IPAR+1)=NW
				IPAR=IPAR+1
				IBRDFLOC(IB)=IPAR
			ENDDO
		ENDIF
		IF(NBRDF1.GT.0) THEN
cs       ISTAR(IPAR+1)=ISTAR(IPAR)+NPAR(IPAR)
cs       write(6,*)'check'
cs       NPAR(IPAR+1)=NBRDF1
cs       write(6,*)'check1'
cs       IPAR=IPAR+1
cs       IBRDF1LOC(1)=IPAR
cs      DO IB1=1,NBRDF1
			ISTAR(IPAR+1)=ISTAR(IPAR)+NPAR(IPAR)
			NPAR(IPAR+1)=NBRDF1
			IPAR=IPAR+1
			IBRDF1LOC(1)=IPAR
cs      ENDDO
		ENDIF
		IF(NSHAPE.GT.0) THEN
			ISTAR(IPAR+1)=ISTAR(IPAR)+NPAR(IPAR)
cs       write(6,*)'check'
			NPAR(IPAR+1)=NSHAPE
cs       write(6,*)'check1'
			IPAR=IPAR+1
			ISHAPELOC(1)=IPAR
cs      DO IB1=1,NBRDF1
cs       ISTAR(IPAR+1)=ISTAR(IPAR)+NPAR(IPAR)
cs       NPAR(IPAR+1)=1
cs       IPAR=IPAR+1
cs       IBRDF1LOC(IB1)=IPAR
cs      ENDDO
		ENDIF
cs      write(6,*)'IBRDF1LOC(1)',IBRDF1LOC(1)
cs      DO I1=1,IPAR
cs      write(6,*)'IPAR,NPAR(I1)',IPAR,NPAR(I1)
cs      write(6,*)'ISTAR(I1)',ISTAR(I1)
cs     ENDDO
C*****************************
C*** Checking IPAR and NPAR:
		NBINT=0
		DO ISD=1,NSD
			NBINT=NBINT+NBIN(ISD)
		ENDDO
		NPART=0
		DO I=1,IPAR
			NPART=NPART+NPAR(I)
		ENDDO
		IF(KN.NE.NPART) 
     &	WRITE(*,*) "KN.NE.NPART - CHECK!!!"
      ENDIF
C*****************************
C*****************************
C*** IF ref. index is known:
C******************************************************
C***  THE FOLLOWING ORDER IS ASSUMED:
C***    LOOP looking through all layers:
C***     LOOP looking through NSD
C***  NBIN(ISD) values of SIZE distr. points
C***     END of NSD LOOP
C***    END of layer's LOOP
C***    LOOP looking through NBRDF
C***  NW values of each BRDF  parameter
C***    END of NBRDF LOOP 
C***    Lool Looking through NBRDF1
C***   BRDF parameteres independent of wavelenth
C******************************************************
C***  ISDLOC(NLAY,NSD)    - index of the SD in AP()
C***  IREFLOC(NLAY,NSD,1) - index of real part of REF in AP()
C***  IREFLOC(NLAY,NSD,2) - index of imag part of REF in AP()
C***  IBRDFLOC(NBRDF)      - index of BRDF in AP()
C********************************************************
      IF(IT.EQ.0) THEN
CD SHOULD BE CHECKED !!!
		IPAR=0
		DO IL=1,NLYR
			DO ISD=1,NSD
				ISTAR(IPAR+1)=ISTAR(IPAR+1)+NPAR(IPAR+1)
				NPAR(IPAR+1)=NBIN(ISD)
				IPAR=IPAR+1
				ISDLOC(IL,ISD)=IPAR
			ENDDO
		ENDDO
		IF(NBRDF.GT.0) THEN
			DO IB=1,NBRDF
				ISTAR(IPAR+1)=IPAR+1+NPAR(IPAR+1)
				NPAR(IPAR+1)=NW
				IPAR=IPAR+1
				IBRDFLOC(IB)=IPAR
			ENDDO
		ENDIF
		IF(NBRDF1.GT.0) THEN
			ISTAR(IPAR+1)=ISTAR(IPAR)+NPAR(IPAR)
			NPAR(IPAR+1)=NBRDF1
			IPAR=IPAR+1
			IBRDF1LOC(1)=IPAR
cs      DO IB1=1,NBRDF1
cs       ISTAR(IPAR+1)=ISTAR(IPAR)+NPAR(IPAR)
cs       NPAR(IPAR+1)=1
cs       IPAR=IPAR+1
cs       IBRDF1LOC(IB1)=IPAR
cs      ENDDO
		ENDIF
		IF(NSHAPE.GT.0) THEN
			ISTAR(IPAR+1)=ISTAR(IPAR)+NPAR(IPAR)
			NPAR(IPAR+1)=NSHAPE
			IPAR=IPAR+1
			ISHAPELOC(1)=IPAR
		ENDIF
C*****************************
C*** Checking IPAR and NPAR:
		NBINT=0
		DO ISD=1,NSD
			NBINT=NBINT+NBIN(ISD)
		ENDDO
		IPARCHECK=NLYR*NBINT+NBRDF*NW+NBRDF1+NSHAPE
cs      IPARCHECK=NLYR*NBINT+NBRDF*NW+1
		IF(IPAR.NE.IPARCHECK) WRITE(*,*) "CHECK IPAR!!!"
		IPART=0
		DO I=1,IPAR
			IPART=IPART+NPAR(I)
		ENDDO
		IF(KN.NE.NPART) 
     &	WRITE(*,*) "KN.NE.NPART - CHECK!!!"
      ENDIF
C******************************************************
C*** Asigning non-equal binning for smoothmness:
      DO I=1,IPAR
		DO J=1,KN
			XX(I,J)=0.
		ENDDO
      ENDDO

      DO IL=1,NLYR
		DO ISD=1,NSD
			DO I=1,NBIN(ISD)
				IF(IBIN.EQ.1) XX(ISDLOC(IL,ISD),I)=RADIUS(ISD,I)
				IF(IBIN.EQ.-1) XX(ISDLOC(IL,ISD),I)=LOG(RADIUS(ISD,I))
			ENDDO
			DO IW=1,NW
				XX(IREFLOC(IL,ISD,1),IW)=LOG(WAVE(IW))
				XX(IREFLOC(IL,ISD,2),IW)=LOG(WAVE(IW))
			ENDDO
		ENDDO
      ENDDO

      DO IB=1,NBRDF
		DO IW=1,NW
			XX(IBRDFLOC(IB),IW)=LOG(WAVE(IW))
		ENDDO
      ENDDO

      DO IBB=1,NBRDF1
		XX(IBRDF1LOC(1),IBB)=IBB+1.
      ENDDO
C*******************************CORECTION*FOR*SHAPE***************
      DO IBB=1,NSHAPE
		XX(ISHAPELOC(1),IBB)=IBB+1.
      ENDDO
C*****************************************************************
C******************************************************
      READ (981,*)
      READ (981,*) IM,NQ,IMTX,KI
      IF(IPRI2.EQ.1)  WRITE(*,*) IM,NQ,IMTX,KI,' IM,NQ,IMTX,KI'
C*****************************************************
C***  Parameters for ITERQ.f (details in "iterqP" subr.)
C***  IM=1 - q-linear iterations 
C***  IM=2 - matrix inversion
C***  NQ - key defining the prcedure for stopping 
C***  q-iterations
C***  KI - defines the type of q-iterations (if IM=1)
C***  EPSP - for stoping p-iterations
C***  EPSQ and NQ see in "ITERQ"
C***  IMTX
C***  KI   - type of k-iterations  
C****************************************************
  104 FORMAT (a100)
      READ (981,*) 
      READ(981,*) IPSTOP, RSDMIN,RSDMAX,REALMIN,REALMAX,
     &AIMAGMIN,AIMAGMAX
      IF(IPRI2.EQ.1) Write(*,*) IPSTOP, RSDMIN,RSDMAX,REALMIN,REALMAX
     &,AIMAGMIN,AIMAGMAX,' IPSTOP, RSDMIN,RSDMAX,REALMIN,REALMAX,',
     &'AIMAGMIN,AIMAGMAX'     
      IF(NBRDF.GT.0) THEN
		READ(981,*) (BRDFMIN(I),BRDFMAX(I),I=1,NBRDF)
		IF(IPRI2.EQ.1) 
     &	WRITE(*,*) (BRDFMIN(I),BRDFMAX(I),I=1,NBRDF),
     &	'(BRDFMIN(I),BRDFMAX(I),I=1,NBRDF)'
      ENDIF
      IF(NBRDF1.GT.0) THEN
		READ(981,*) (BRDF1MIN(I),BRDF1MAX(I),I=1,NBRDF1)
		IF(IPRI2.EQ.1) WRITE(*,*) (BRDF1MIN(I),BRDF1MAX(I),I=1,NBRDF1)
     &	,'(BRDF1MIN(I),BRDF1MAX(I),I=1,NBRDF1)'
      ENDIF
C*******************************CORRECTION*FOR*SHAPE****************************************
      IF(NSHAPE.GT.0) THEN
		READ(981,*) (SHAPEMIN(I),SHAPEMAX(I),I=1,NSHAPE)
		IF(IPRI2.EQ.1) WRITE(*,*) (SHAPEMIN(I),SHAPEMAX(I),I=1,NSHAPE)
     &	,'(SHAPEMIN(I),SHAPEMAX(I),I=1,NSHAPE)'
      ENDIF
C******************************************************************************************
C****************************************************
C*** PARAMETERS FOR Levenb.-Marquardt correction:
C i.e. the Lagrange parameters are adjusted with
C  each p-th iterations:
C*********
C***  ISTOP - the maximum number of IP iterations
C***           where  Levenb.-Marquardt cor. applied
C*** ...MIN - minumum 
C*** ...MAX - maximum
C***          - the variability limits of retrieved 
C***    parameters they will be used for calculating
C***    the strengh of Lev.-Marq. correction   
C****************************************************
      READ (981,104) CH1
      IF(IT.EQ.-1) THEN
		DO IL=1,NLYR
C***  A PRIORI Estimates       ****
			READ (981,*) (IO0(ISDLOC(IL,I)),GSM0(ISDLOC(IL,I)),
     &		IO0(IREFLOC(IL,I,1)),GSM0(IREFLOC(IL,I,1)),
     &		IO0(IREFLOC(IL,I,2)),GSM0(IREFLOC(IL,I,2)),I=1,NSD)
			IF(IPRI2.EQ.1) WRITE(*,*) (IO0(ISDLOC(IL,I)),GSM0(ISDLOC(IL,I)),
     &		IO0(IREFLOC(IL,I,1)),GSM0(IREFLOC(IL,I,1)),
     &		IO0(IREFLOC(IL,I,2)),GSM0(IREFLOC(IL,I,2)),I=1,NSD)
     &		,'(IO0(ISDLOC(IL,I)),GSM0(ISDLOC(IL,I)),
     &		IO0(IREFLOC(IL,I,1)),GSM0(IREFLOC(IL,I,1)),
     &		IO0(IREFLOC(IL,I,2)),GSM0(IREFLOC(IL,I,2)),I=1,NSD)'
C***  SMOOTHNESS Constraints   ****
			READ (981,*) (IO(ISDLOC(IL,I)),GSM(ISDLOC(IL,I)),
     &		IO(IREFLOC(IL,I,1)),GSM(IREFLOC(IL,I,1)),
     &		IO(IREFLOC(IL,I,2)),GSM(IREFLOC(IL,I,2)),I=1,NSD)
			IF(IPRI2.EQ.1) WRITE(*,*) (IO(ISDLOC(IL,I)),GSM(ISDLOC(IL,I)),
     &		IO(IREFLOC(IL,I,1)),GSM(IREFLOC(IL,I,1)),
     &		IO(IREFLOC(IL,I,2)),GSM(IREFLOC(IL,I,2)),I=1,NSD)
     &		,'(IO(ISDLOC(IL,I)),GSM(ISDLOC(IL,I)),
     &		IO(IREFLOC(IL,I,1)),GSM(IREFLOC(IL,I,1)),
     &		IO(IREFLOC(IL,I,2)),GSM(IREFLOC(IL,I,2)),I=1,NSD)'
		ENDDO
      ENDIF
      
	IF(IT.EQ.0) THEN
		DO IL=1,NLYR
			READ (981,*) (IO(ISDLOC(IL,I)),GSM(ISDLOC(IL,I)),I=1,NSD)
			IF(IPRI2.EQ.1) WRITE(*,*) (IO(ISDLOC(IL,I))
     &		,GSM(ISDLOC(IL,I)),I=1,NSD)
     &		,'(IO(ISDLOC(IL,I)),GSM(ISDLOC(IL,I)),I=1,NSD)'
		ENDDO
      ENDIF

      IF(NBRDF.GT.0) THEN
		READ(981,*) (IO0(IBRDFLOC(IB)),GSM0(IBRDFLOC(IB)),IB=1,NBRDF)
		IF(IPRI2.EQ.1) WRITE(*,*) (IO0(IBRDFLOC(IB))
     &	,GSM0(IBRDFLOC(IB)),IB=1,NBRDF)
     &	,'(IO0(IBRDFLOC(IB)),GSM0(IBRDFLOC(IB)),IB=1,NBRDF)'
		READ(981,*) (IO(IBRDFLOC(IB)),GSM(IBRDFLOC(IB)),IB=1,NBRDF)
		IF(IPRI2.EQ.1) 
     &	WRITE(*,*) (IO(IBRDFLOC(IB)),GSM(IBRDFLOC(IB)),IB=1,NBRDF)
     &	,'(IO(IBRDFLOC(IB)),GSM(IBRDFLOC(IB)),IB=1,NBRDF)'
      ENDIF
      
	IF(NBRDF1.GT.0) THEN
		READ(981,*) IO0(IBRDF1LOC(1)),GSM0(IBRDF1LOC(1))       
		IF(IPRI2.EQ.1) 
     &	WRITE(*,*) IO0(IBRDF1LOC(1)),GSM0(IBRDF1LOC(1))
     &	,'IO0(IBRDF1LOC(IB)),GSM0(IBRDF1LOC(IB))'
		READ(981,*) IO(IBRDF1LOC(1)),GSM(IBRDF1LOC(1))
		IF(IPRI2.EQ.1) 
     &	WRITE(*,*) IO(IBRDF1LOC(1)),GSM(IBRDF1LOC(1))
     &	,'IO(IBRDF1LOC(1)),GSM(IBRDF1LOC(1))'
      ENDIF
C***********************SORECTION*FOR*SHAPE************************
      IF(NSHAPE.GT.0) THEN
		READ(981,*) IO0(ISHAPELOC(1)),GSM0(ISHAPELOC(1))       
		IF(IPRI2.EQ.1) 
     &	WRITE(*,*) IO0(ISHAPELOC(1)),GSM0(ISHAPELOC(1))
     &	,'IO0(ISHAPELOC(IB)),GSM0(ISHAPELOC(IB))'
		READ(981,*) IO(ISHAPELOC(1)),GSM(ISHAPELOC(1))
		IF(IPRI2.EQ.1) 
     &	WRITE(*,*) IO(ISHAPELOC(1)),GSM(ISHAPELOC(1))
     &	,'IO(ISHAPELOC(1)),GSM(ISHAPELOC(1))'
      ENDIF
C**************************************************************************
C****************************************************
C***  IO0(NPAR) - order of correlations for apriori estimates
C***  GSM0(NPAR)- Lagrange multiplayer for a priori estimatesr
C****************************************************
C***  IO(NPAR) - order of smoothnes for I-th parameter depend.
C***  GSM(NPAR)- Lagrange multiplayer for  smoothnes 
C***             for I-th parameter dependence
C****************************************************
      READ (981,*) EPSP,EPST,EPSQ,DL,AREF,EPSD
      IF(IPRI2.EQ.1)  WRITE(*,*) EPSP,EPST,EPSQ,DL,' EPSP,EPST,EPSQ,DL'
C*****************************************************
C***  EPSP - for stoping p - iterations
C***  EPST - for stoping iterations in CHANGE
C***  EPSQ - for stoping q - iterations in ITERQ"
C***  DL   - for calc. derivatives, (see FMATRIX)
C*****************************************************
C***  Reading "measurements and initial guess 
C***  for the solution (absolute values)
C*****************************************************
      READ (981,104) CH1
      IF(IPRI2.EQ.1) WRITE(*,*) 'before READING F(*)'
      READ (981,*) (F(J),J=1,KM)
      IF(IPRI2.EQ.1) WRITE(*,*) 'AFTER READING F(*)'
      IF(IPRI2.EQ.1)  WRITE(*,*) F(KM), 'F(KM)'
C*** Transforming to log ***
      IF(KL.EQ.1) THEN
		DO J=1,KM
			F(J)=LOG(F(J))
		ENDDO
      ENDIF
      IF(IPRI2.EQ.1) WRITE(*,*) 'before AP reading'
      READ (981,104) CH1
      READ (981,*) (AP(I),I=1,KN)
      IF(IPRI2.EQ.1) WRITE(*,*) 'AFTER READING AP(*)'
      IF(IPRI2.EQ.1) WRITE(*,*)  (AP(I),I=1,KN),'AP'
C************************************************************
C*** !!! Define the initial guess for AP - vector of unknowns!!!
C***  The oder if unknowns in the vector AP is the following:
C***  First layer:
C***     - First size distribution:
C***        - real part of the refractive index; 
C***        - imaginary part of the refractive index;
C***        - dR/d..R
C***     - Second size dsiribution (*if NSD.GT.1*):
C***        - real part of the refractive index; 
C***        - imaginary part of the refractive index;
C***        - dR/d..R
C***  Second layer (*if NLYR.GT.1*):
C***     - First size distribution:
C***        - real part of the refractive index; 
C***        - imaginary part of the refractive index;
C***        - dR/d..R
C***     - Second size dsiribution (*if NSD.GT.1*):
C***        - real part of the refractive index; 
C***        - imaginary part of the refractive index;
C***        - dR/d..R
C***    *** LOOP by NLYR ****
C***  BRDF:
C***     - first wavelength
C***    *** LOOP by NW   ****
C*** Assuming initial SD by log-norm functions
      IF(ISZ.EQ.1) OPEN (1012, FILE="SDguess.dat")
CD      IK=0
CD?      IIK=0
      IF(ISZ.EQ.1) THEN
		DO IL=1,NLYR
			DO ISD=1,NSD
				READ(1012,*) IDZ,NSD0,NMDZ
C********************************************************
C***  IDZ  - types of Size Dist. (e.g. volume)
C***  NSD0 - the number of size distr. (in each model)
C***  NMDZ - the number of modes in each above size dis.
C********************************************************
				DO I=1,NSD0
					READ(1012,*) KNZ(I)
					IF(KNZ(I).NE.NBIN(ISD)) 
     1					WRITE(*,*) "Bins are wrong in SDguess"
					READ(1012,*) RMINZ(I), RMAXZ(I)
					READ(1012,*) (CMZ(I3,I),SMZ(I3,I),RMMZ(I3,I),I3=1,NMDZ)
C********************************************************
C***  KNZ(I) - types of Size Dist. (e.g. volume)
C***  RMINZ(I), RMAXZ(I) - min and maximum sizes
C*** (CMZ(I3,I),SMZ(I3,I),RMMZ(I3,I) - the concentration,
C***  standart deviation and median radius (in each mode)
C********************************************************
				ENDDO
				CALL SIZEDISD(KNZ,IDZ,NSD,NMDZ,CMZ,SMZ
     &			,RMMZ,RMINZ,RMAXZ,APZ,ACZ)
				DO ISD1=1,NSD0
					DO I=1,NPAR(ISDLOC(IL,ISD1))
						AP(ISTAR(ISDLOC(IL,ISD1))+I-1)
     &					=AP(ISTAR(ISDLOC(IL,ISD1))+I-1)+APZ(ISD1,I)
					ENDDO
				ENDDO
			ENDDO
		ENDDO
		CLOSE (1012)
      ENDIF
C***************************
      DO IL=1,NLYR
C***  A PRIORI Estimates       ****
		DO ISD1=1,NSD
			IF(GSM0(ISDLOC(IL,ISD1)).GT.0) THEN
				DO I=1,NPAR(ISDLOC(IL,ISD1))
         AP0(ISTAR(ISDLOC(IL,ISD1))+I-1)=AP(ISTAR(ISDLOC(IL,ISD1))+I-1)
				ENDDO
			ENDIF
			IF(GSM0(IREFLOC(IL,ISD1,1)).GT.0) THEN
				DO I=1,NPAR(IREFLOC(IL,ISD1,1))
CD         WRITE(*,*) IREFLOC(IL,ISD1,1)+I-1,' IREFLOC(IL,ISD1,1)'
					AP0(ISTAR(IREFLOC(IL,ISD1,1))+I-1)=
     &				AP(ISTAR(IREFLOC(IL,ISD1,1))+I-1)
				ENDDO
			ENDIF
			IF(GSM0(IREFLOC(IL,ISD1,2)).GT.0) THEN
				DO I=1,NPAR(IREFLOC(IL,ISD1,2))
CD         WRITE(*,*) IREFLOC(IL,ISD1,2)+I-1,' IREFLOC(IL,ISD1,2)'
					AP0(ISTAR(IREFLOC(IL,ISD1,2))+I-1)=
     &				AP(ISTAR(IREFLOC(IL,ISD1,2))+I-1)
				ENDDO
			ENDIF
		ENDDO
      ENDDO

      IF(NBRDF.GT.0) THEN
		DO IB=1,NBRDF
			DO IW=1,NW
				AP0(ISTAR(IBRDFLOC(IB))+IW-1)=
     &			AP(ISTAR(IBRDFLOC(IB))+IW-1)
			ENDDO
		ENDDO
      ENDIF
      
	IF(NBRDF1.GT.0) THEN
		DO IBB=1,NBRDF1
			AP0(ISTAR(IBRDF1LOC(1))+IBB-1)=
     &		AP(ISTAR(IBRDF1LOC(1))+IBB-1)
		ENDDO
      ENDIF 
C*************************CORRECTION*FOR*SHAPE********************
      IF(NSHAPE.GT.0) THEN
		DO IBB=1,NSHAPE
			AP0(ISTAR(ISHAPELOC(1))+IBB-1)=
     &		AP(ISTAR(ISHAPELOC(1))+IBB-1)
		ENDDO
      ENDIF 
C******************************************************************    
C*** Transforming to log ***
      IF(KL.EQ.1) THEN
		DO I=1,KN
			AP(I)=LOG(AP(I))
			IF(AP0(I).GT.0.) AP0(I)=LOG(AP0(I))
		ENDDO
      ENDIF
C*****************************************************
C***  INITIAL GUESS FOR q-ITERATIONS: ****************
      DO I=1, KN
		Q0(I)=0.0
      ENDDO
C******************************************************
C***  Accounting for different accuracy levels in the data
C***       AND   modelling   RANDOM NOISE           ***
      READ(981,*) INOISE
      IF(IPRI2.EQ.1) WRITE(*,*) INOISE,'INOISE'
      READ(981,*) SGMS(1), INN(1), DNN(1)
      IF(IPRI2.EQ.1) WRITE(*,*) SGMS(1), INN(1), DNN(1)
     &,'SGMS(1), INN(1), DNN(1)'
      IKI=0
      IF(INOISE.GE.2) THEN
		DO I=2,INOISE
CD       IF(I.NE.INOISE) READ (*,*) SGMS(I),INN(I),DNN(I),IK(I),
CD     &(KNOISE(I,K),K=1,IK(I))
			READ(981,*) SGMS(I),INN(I),DNN(I),IK(I),
     &		(KNOISE(I,K),K=1,IK(I))
			IF(IPRI2.EQ.1) WRITE (*,*) SGMS(I),INN(I),DNN(I),IK(I),
     &		(KNOISE(I,K),K=1,IK(I))
		ENDDO
          IK(1)=KM
		DO I=2,INOISE
			IK(1)=IK(1)-IK(I)
		ENDDO
		IKI=0
		DO 1 II=1,KM
			DO  I=2,INOISE
				DO  K=1,IK(I)
					IF(II.EQ.KNOISE(I,K)) GO TO 1
				ENDDO
			ENDDO
			IKI=IKI+1
			KNOISE(1,IKI)=II
    1		ENDDO
C*******************************************************************
C*** Checking if all the measurements are assigned               ***
C***          to a noise souce                                   ***
		IF(IKI.NE.IK(1)) THEN
			WRITE (*,*) IKI,IK(1),'IKI.NE.IK(1)-problem in noise assptions!'
		ENDIF
      ENDIF
C*******************************************************************
C*** INOISE  - the number of different noise sources              ***
C*** SGMS(I) - std of noise in i -th source                       ***
C*** INN(I)  - EQ.1.THEN error is absolute with                   ***
C***         - EQ.0 THEN error assumed relative
C*** DNN(I)  - variation of the noise of the I-th source
C*** IK(I)   - total number of measurements of i-th source        ***
C*** KNOISE(1,K)-specific numbers affected by i-th source        ***
C*** All the measurments which where not listed in  the INOISE-1 ***
C***  first INOISE-1 sources, they belong to last source of noise***
C*******************************************************************
      IF(SGMS(1).GT.0.) THEN
cs      write(6,*)'IN'
		RDM0=0.4
		EMG=0.
		DO I=1,INOISE
			CALL RDMG(RDM0,IK(I),EMG,SGMS(I),RDM)
			DO J=1,KM
				DO K=1,IK(I)
					IF(J.EQ.KNOISE(I,K)) THEN 
						IF(INN(I).EQ.0) THEN
C*** Relative errors: ****
							IF(KL.EQ.0) F(J)=F(J)*(1+RDM(K))
							IF(KL.EQ.1) F(J)=LOG(EXP(F(J))*(1+RDM(K)))
						ELSE
C*** Absolute errors: ****
cs         write(6,*)'before'
							IF(KL.EQ.0) F(J)=F(J)+RDM(K)
							IF(KL.EQ.1) F(J)=LOG(EXP(F(J))+RDM(K))
cs          write(6,*)'RDM',EXP(F(J)),RDM(K)
						ENDIF
					ENDIF
				ENDDO
			ENDDO
		ENDDO
      ENDIF
C******************************************************
C*** Defining matrix inverse to  covariance ***
CD      WRITE(*,*) 'BEFORE IC,IACOV, DWW'
      READ(981,*) IC,IACOV,DWW
CD      WRITE(*,*) IC,IACOV,DWW,' IC,IACOV, DWW'
C***    IC  =0 then 
C***           C is unit matrix (for logarithms)
C***           C is diagonal matrix with the ellements
C***                1/((F(j)*F(j)) (for non-logarithm case)
C***    IC  =1 then C is diagonal and defined  
C*** with accounting for different levels of the errors in
C*** different measurements (according to Dubovik and King [2000] 
C*** !!! the measurements assigned by INOISE=1 usually         !!! 
C*** !!! correspond to the largest set of optical measurements !!! 
C***    IC   <0 then C is read from cov.dat
C**************************************************************
C*** IACOV 
C***       =0 - single inversion with unique COV matrix
C***       >0 - inversion is repeated with different COV matrix
      ICC=0
CD?      IAC=IC
CD      KM3=KM
	
1112  CONTINUE
      ICC=ICC+1
cs       write(6,*)'IC=',IC
cs      IF(INOISE.GT.2) WRITE(*,*) "*** INOISE.GT.2 ***"
      IF(IC.EQ.1) THEN
		DO IN=1,INOISE
			DO I=1,IK(IN)
				IF(INN(IN).EQ.0) THEN
					XIK1=IK(1)
					XIKN=IK(IN)
					IF(KL.EQ.1) THEN
						C(KNOISE(IN,I),KNOISE(IN,I))=
     &					IK(1)/IK(IN)*(DNN(1)*DNN(1))/(DNN(IN)*DNN(IN))
					ENDIF
					IF(KL.EQ.0) THEN
        C(KNOISE(IN,I),KNOISE(IN,I))=XIK1/XIKN*(DNN(1)*DNN(1))/
     &  (F(KNOISE(IN,I))*DNN(IN)*F(KNOISE(IN,I))*DNN(IN))
					ENDIF
				ENDIF
				IF(INN(IN).EQ.1) THEN
					IF(KL.EQ.1) THEN
						XIK1=IK(1)
						XIKN=IK(IN)
        C(KNOISE(IN,I),KNOISE(IN,I))=XIK1/XIKN*(DNN(1)*DNN(1))/
     &  (DNN(IN)*DNN(IN)/exp(F(KNOISE(IN,I)))/exp(F(KNOISE(IN,I))))
						IF(IPRI2.EQ.1) THEN
        WRITE(*,*) C(KNOISE(IN,I),KNOISE(IN,I)),IK(1),IK(IN),DNN(1)
     &,DNN(IN),exp(F(KNOISE(IN,I)))
        WRITE(*,*) 'C(),IK(1),IK(IN),DNN(1)
     &,DNN(IN),exp(F(KNOISE(IN,I)))'
						ENDIF
					ENDIF
					IF(KL.EQ.0) THEN
        C(KNOISE(IN,I),KNOISE(IN,I))=XIK1/XIKN*(DNN(1)*DNN(1))/
     &  (DNN(IN)*DNN(IN))
					ENDIF
				ENDIF
			ENDDO
		ENDDO
      ENDIF

      IF(IC.EQ.0.OR.INOISE.EQ.1) THEN
		DO J=1,KM
			C(J,J)=1.
		ENDDO
      ENDIF

      IF(IC.LT.0) THEN
       READ (1009,*) ((C(J,I),I=1,KM),J=1,KM)
       DO J=1,KM
		WRITE(1010,103) (C(J,I),I=1,KM)
  103		FORMAT(7E10.3)
       ENDDO
      ENDIF  
C	WRITE(*,*)'IC=',IC	!CXXA
C	WRITE(*,*)'C(1,1)=',C(1,1) !CXXA     
cs       DO J=1,KM
cs       WRITE(*,*) C(J,J), J       
cs       ENDDO
C******************************************************
C***         p - iterations
C******************************************************
C*** LP -number of iterations
C*** LTP-number of iterations for choosing TP
      AR0=1.0
CD?      EPS=0.
      LP=0
C******************************************************
C*** DETERMINING of delta AP()     ***
C******************************************************
C**  Calculating "measurements" model from AP() vector:
C******************************************************
CD      WRITE(*,*) NLYR,' NLYR in iP'
      IF(ISTOP.EQ.1) IST=1
cs      write(6,*)'NBRDF1',NBRDF1
C***********************CORRECTION*FOR*SHAPE******************
      DO ib=1,NSHAPE
		SHAPEMAX(ib)=0.99
      ENDDO
C************************************************************
cs      write(6,*)'BRF1MAX before forw1',BRDF1MAX
cs      INDB=1
C**************************************************************************************************
C********************************************************************************************








C**************************************************************************************************
C************************ BEGINNING MODIFIED by BENJAMIN ******************************************
C**************************************************************************************************
C**************************************************************************************************
	NDP=0 !CXXA
C	WRITE(*,*)'NDP=',NDP !CXXA
      AP(KN)=EXP(AP(KN))
      AP(KN)=AP(KN)/100
      AP(KN)=LOG(AP(KN))
      xinti=AP(KN)
	APIN=0.   !CXXA
c	WRITE(250,*)'begin THE FIRST LOOP',AP(1),AP(KN) !cxxa
      CALL forwMN(IST,KM,KN,IT,KL,IMSC,NW,WAVE,NSD,NBIN1,
     &NBIN,RADIUS
     & ,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     & ,IBRDF1LOC,ISHAPELOC,ISTAR,
     & AP,SSA,EXT,PF,FP,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,
     &DLP,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
c	do kx=1,104
c		WRITE(250,'(2E12.4)')F(kx),FP(kx) !CXXA
c	enddo
c	WRITE(250,*)'end THE FIRST LOOP',AP(1),AP(KN) !cxxa
	
c	write(*,*)km,kn,it,kl,imsc,nw,wave,nsd,nbin1,nbin
c	write(*,*)nlyr,nbrdf,nbrdf1,nshape
c	write(*,*)ibrdfloc,ibrdf1loc,ishapeloc,istar
c	write(250,*)ap(1:31)
c	write(250,*)ssa
c	write(250,*)ext
c	write(250,*)pf
c	write(250,'(2e12.4)')f(2),fp(2) !cxxa
c	return

C********************************************************************************************
       IF(INDTAU(1).GT.0)THEN
	    sum1=0.
		sum2=0.
		sum3=0.
		sum4=0.
		do i1=1,NW
			tf=(TAUR(i1,NLYRS+1))
cs       write(6,*)'TAUR',tf
			TAUA(i1)=ALOG(EXP(F(KNOISE(2,i1)))-tf)
			WAVEL(i1)=ALOG(WAVE(i1))
			sum1=sum1+WAVEL(i1)
			sum2=sum2+TAUA(i1)
			sum3=sum3+WAVEL(i1)*WAVEL(i1)
			sum4=sum4+WAVEL(i1)*TAUA(i1)
		enddo
		sum1=sum1/NW
		sum2=sum2/NW
		xn1=sum4-NW*sum1*sum2
		xn2=sum3-NW*sum1*sum1
		anstr=-xn1/xn2
cs       write(6,*)'anstr',anstr
cs***********************LAST**********************CORRECTION*********************
		iks=1
		ikt=0  
		do i2=1,NW
			IF(INDTAU(i2).EQ.0)THEN
				ikt=ikt+1
				do i3=1,KM
					IF(KNOISE(2,ikt).EQ.i3)THEN
						FPT(iks)=EXP(F(i3))
					ENDIF
				enddo
			ENDIF
			iks=iks+1
		enddo
C      write(6,*)'FPT',FPT
		FPT(1)=FPT(1)-0.23
		IF(FPT(1).GE.0.9)THEN
			anstr_mod(1)=0.
			anstr_mod(2)=2.5
			xlags(1)=1.0e-4
			xlags(2)=1.0e-3
		ENDIF
		IF(FPT(1).LT.0.9)THEN
			anstr_mod(1)=0.
			anstr_mod(2)=2.3
			xlags(1)=1.0e-4
			xlags(2)=1.0e-2
		ENDIF
		xlagsm=LINEAR2(anstr_mod,alog(xlags),2,anstr)
		do i1=1,NLYR
			do i2=1,NSD
cs************************************CHANGED******************************************
				GSM(IREFLOC(i1,i2,2))=exp(xlagsm)
cs       write(6,*)'smoothness im',GSM(IREFLOC(i1,i2,2))
			enddo
		enddo
C************************************************************************
		do i1=1,NLYR
			do i2=1,NSD
				if(anstr.lt.0.2)GSM(IREFLOC(i1,i2,1))=1.e-1
				if(anstr.ge.0.2.and.anstr.le.0.65)GSM(IREFLOC(i1,i2,1))=3.e-1
cs************************************CHANGED******************************************
				if(anstr.gt.0.65)GSM(IREFLOC(i1,i2,1))=1.e-1
cs       write(6,*)'smoothness re',GSM(IREFLOC(i1,i2,1))
			enddo
		enddo
C************************************************************************
		IF(anstr.GE.2)THEN 
			xinti=alog(0.999)
			AREF1=0.05
		ENDIF
		IF(anstr.LT.2)AREF1=AREF

		IF(IM.GT.0) THEN
			AP(KN)=(xinti)
		ENDIF
       ENDIF
       IF(INDTAU(1).EQ.0)THEN
		AREF1=AREF
		xinti=alog(0.56)
		IF(IM.GT.0) THEN
			AP(KN)=(xinti)
		ENDIF
       ENDIF






	
cs**********LAST********CORRECTION*********************APIN**
       IF(EXP(AP(KN)).GT.1)AP(KN)=ALOG(0.999)
       APIN=1.-exp(AP(KN))
c	 APIN=1.
cs**********************************************************
cs       write(6,*)'APIN1',APIN
C**********************************************************************************************


      CALL forwMN(IST,KM,KN,IT,KL,IMSC,NW,WAVE,NSD,NBIN1,
     &NBIN,RADIUS
     & ,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     & ,IBRDF1LOC,ISHAPELOC,ISTAR,
     & AP,SSA,EXT,PF,FP,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,
     &DLP,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
c	WRITE(250,*)'THE SECOND LOOP',AP(1),AP(KN) !cxxa

c	do kx=1,km
c		WRITE(250,'(2E12.4)')F(kx),FP(kx) !CXXA
c	enddo





C******************************************************
C************** OUTPUT FORWARD ************************
C******************************************************
C******************************************************

C      DO j=1,KM
C      write(*,*)j,EXP(FP(j))
C      ENDDO

      IF(ISTOP.EQ.1) THEN
	    KL=1
		do i1=1,NW
			IF(INDTAU(i1).EQ.1)INTAU=1
		enddo

		IF(INTAU.EQ.1)THEN
			write(*,*)''
			write(*,*)'Optical depth'
			write(*,*)
			write(*,*)'         FORW    Measured    Difference'
		ENDIF
		iks=1
		ikt=0  
		do i2=1,NW
			IF(INDTAU(i2).EQ.1)THEN
				ikt=ikt+1
				do i3=1,KM
					IF(KNOISE(2,ikt).EQ.i3)THEN
						FPT(iks)=EXP(F(i3))
						FP1T(iks)=EXP(FP(i3))
						inws(i2)=i3
					ENDIF
				enddo
			ENDIF
			iks=iks+1
		enddo
      
		write(*,*)''
		do i1=1,NW
			IF(INDTAU(i1).EQ.1)THEN
				write(*,80)Wave(i1),FP1T(i1),FPT(i1),FPT(i1)-FP1T(i1)
			ENDIF
		enddo
      
C******************************************************
		write(*,*)''
		do i1=1,NW
			IF(INDSK(i1).EQ.1)INSK=1
		enddo
		IF(INSK.EQ.1)THEN
			write(*,*)''
			write(*,*)'Sky Radiances'
			write(*,*)
		ENDIF
      
		do i1=1,NW-1
			inws(i1)=inws(i1+1)-inws(i1)-1
		enddo
		inws(NW)=KM-inws(i1)

		ik1=1
		ik2=0
		ikt=0
	    do i1=1,NW
			IF(INDSK(i1).EQ.1)THEN
				ikt=ikt+1
				write(*,82)'Wavelength ',WAVE(i1)
				write(*,*)'Angle    FORW    Measured    Difference'
				write(*,*)
				iks=1
				ik2=ik1+inws(i1)
				ia=1
				do i2=ik1,ik2
					do i3=1,KM
						IF(KNOISE(1,i2).EQ.i3)THEN   					
							FP1T(iks)=FP(i3)
							FPT(iks)=F(i3)
							ANGLE(iks)=ANGLESCA(i1,ia,iks+1)
							iks=iks+1
						ENDIF
					enddo
				enddo
				in=inws(i1)
				do i4=1,in
					write(*,92)ANGLE(i4),EXP(FP1T(i4)),EXP(FPT(i4)),
     &							(FPT(i4)-FP1T(i4))*100.,' %'
				enddo
				write(*,*)
				ik1=ik2
			ENDIF
		enddo

		GOTO 21
      ENDIF




C**************************************************************************************************
C****************************** END MODIFIED by BENJAMIN ******************************************
C**************************************************************************************************
C**************************************************************************************************
























C***********************************************************************************************
cs      write(6,*)'TAUR'
cs      do i1=1,NW
cs      write(6,*)i1,TAUR(i1,NLYRS+1)
cs      enddo
cs      write(6,*)'after FORWARD'
cs      DO JJ=1,KM
cs      WRITE(*,*) JJ, (F(JJ)), (FP(JJ)), C(JJ,JJ)
cs     &,(F(JJ)- FP(JJ))**2*C(JJ,JJ)
cs      ENDDO
cs******Corrected by me
cs       IF(ISTOP.EQ.1)THEN
       DO JJ=1,KM
		C1(JJ,JJ)=1.0
cs       WRITE(*,*) JJ, C(JJ,JJ)
       ENDDO
cs      ENDIF
CS  ***********************
cs****Corrected by me*********
cs      do j=1,KM
cs	      write(250,*)EXP(F(j)),EXP(FP(j))
cs      ENDDO
c      write(*,*)
      CALL RES (KM,F,FP,C1,AL1)
cs      write(6,*)'optical',AL1
cs      do j=1,KM
cs      write(*,*)EXP(F(j)),EXP(FP(j))
cs      ENDDO
      CALL RES (KM,F,FP,C,AL)
c	write(*,*)'AL=',AL !CXXA
cs      write(6,*)'weighted',AL
cs      WRITE(*,*) (C(JJ,JJ),JJ=1,KN)      
cs***************************
      IF(IPRI2.EQ.1) WRITE(*,*) AL*100.,' AL, AFTER RES'
cs      write(6,*)'GSM',GSM
      IF(NDP.EQ.0) CALL SMOOTHTERM(KN,IPAR,ISTAR,NPAR,IO,
     &GSM,XX,IKS,SM)
cs      write(6,*)'after SMOOTHTERM'
      IF(IPRI2.EQ.1) THEN
		WRITE(7777,122) (II+1.-1,ii=1,KN)
		DO II=1,KN
			WRITE(7777,122) (SM(II,JJ),JJ=1,KN)
		ENDDO
cs       WRITE(7777,122) (SM(KN,JJ),JJ=1,KN)
      ENDIF
      CALL RES (KN,AF,AP,SM,AAA)
C	write(*,*)'AAA=',AAA !CXXA
cs      write(6,*)'after Res3'
      CALL VEC_MATR (KN,KN,SM,AP,FFS)
cs      write(6,*)'after VEC_MATR'
CD******* A PRIORI TERM  *******************************
      IF(NDP.EQ.0) CALL SMOOTHTERM(KN,IPAR,ISTAR,NPAR,IO0,
     &GSM0,XX,IKS1,SM0)
cs      write(6,*)'after SMOOTHTERM1'
      DO I=1,KN
       AP0N(I)=AP(I)-AP0(I)
      ENDDO
C************Corrected*by*Siniuk*************************
       IF(IEND.EQ.1)THEN
		DO I=1,KN
			SM0(I,I)=0.
		ENDDO
		DO IL=1,NLYR
			DO ISD1=1,NSD
				SM0(ISTAR(ISDLOC(IL,ISD1)),ISTAR(ISDLOC(IL,ISD1)))=1.
     &*GSM0(ISDLOC(IL,ISD1))
        SM0(ISTAR(ISDLOC(IL,ISD1))+NPAR(ISDLOC(IL,ISD1))-1,
     &ISTAR(ISDLOC(IL,ISD1))+NPAR(ISDLOC(IL,ISD1))-1)=1.
     &*GSM0(ISDLOC(IL,ISD1))
			ENDDO
		ENDDO
cs       write(6,*)(SM0(i1,i1),i1=1,KN)
       ENDIF
cs       write(6,*)(SM0(i1,i1),i1=1,KN)
C***********************************************************
      CALL RES (KN,AP0,AP,SM0,AAAA) 
C	WRITE(*,*)'AAAA=',AAAA !CXXA
cs      write(6,*)'after Res4'     
      CALL VEC_MATR (KN,KN,SM0,AP0N,FFS0)
cs      write(6,*)'after VEC_MATR1'
CD******* END of A PRIORI TERM  *******************************
C	WRITE(*,*)'AL=',AL,KM,KN
      ALCH=SQRT(AL/(KM-KN))
      IF(KM.LE.KN)ALCH=SQRT(AL/(KM-KN+IKS+IKS1))
      IF(IPRI2.EQ.1) WRITE(*,*) KM,KN,' KM,KN'
      AACOR=1.
      IF(ALCH.GT.0.01) AACOR=(ALCH/0.01)*(ALCH/0.01)
cs      IF(ALCH.GT.0.05) AACOR=(ALCH/0.05)*(ALCH/0.05)
      IF(IPRI2.EQ.1) WRITE(*,*) AAA,AACOR,' AAA,AACOR'
      IF(IPRI2.EQ.1) WRITE(*,*) AACOR*AAA, 'AAA, AFTER SECOND RES'
      IF(ISTOP.EQ.1) WRITE(*,*) SQRT(AL1/KM),'OPTICAL RESIDUAL'
cs      write(6,*)'KM,KN,IKS,IKS1',KM,KN,IKS,IKS1,KM-KN+IKS+IKS1
cs      write(6,*)'AL,AACOR,AAA,AAAA',AL,AACOR,AAA,AAAA
c	WRITE(*,*)'AACOR=',AACOR,KM,KN,IKS,IKS1 !CXXA
      AL=(AL+AACOR*(AAA)+AAAA)/(KM-KN+IKS+IKS1)
      WRITE(1011,*) SQRT(AL)*100.,'% - RESIDUAL using INITIAL GUESS'
      
	IF(ISTOP.EQ.1) THEN
	    KL=1
		GO TO 21
	ENDIF
C******************************************************
C**  Calculating matrix of the first derivatives:
C******************************************************
  12  IF(IMSC.NE.IMSC1.OR.LP.GT.0) THEN
c		WRITE(*,*)'IMSC=',IMSC,IMSC1,LP
		INDB=1
cs      write(6,*)'BRF1MAX before forw2',BRDF1MAX
		CALL forwMN(0,KM,KN,IT,KL,IMSC1,NW,WAVE,NSD,NBIN1,NBIN,
     &	RADIUS
     &	,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     &	,IBRDF1LOC,ISHAPELOC,ISTAR
     &	,AP,SSA,EXT,PF,FP1,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,
     &	DLP,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
cs      write(6,*)'AFTER FORWADR1'
		INDB=0
		CALL VEC_MATR (KN,KN,SM,AP,FFS)
		DO I=1,KN
			AP0N(I)=AP(I)-AP0(I)
		ENDDO
		CALL VEC_MATR (KN,KN,SM0,AP0N,FFS0)
      ELSE
		DO J=1,KM
			FP1(J)=FP(J)
		ENDDO
      ENDIF
c	WRITE(250,*)'THE third LOOP',AP(1),AP(KN) !cxxa
      ALPT=SQRT(AL)
      IF(ALPT.LE.EPSD) IMSC1=IMSC
cs      write(6,*)'BRDF1MAX before FMAT',BRDF1MAX
cs      write(6,*)'BEFORE FMAT'
      CALL FMAT(KM,KN,IT,KL,IMSC1,NW,WAVE,NSD
     &,NBIN,RADIUS,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ISDLOC,
     &IREFLOC
     &,IBRDFLOC,IBRDF1LOC,ISHAPELOC,ISTAR,AP,FP1,IMTX,DL,LP
     &,REALMIN,REALMAX,AIMAGMIN,AIMAGMAX
     &,BRDFMIN,BRDFMAX,BRDF1MIN,BRDF1MAX,SHAPEMIN,SHAPEMAX
     &,ALPT,AREF,AREF1,U,IEL,IS,0,US,UP,SSA,PF,UPOL,DPL,0,0,0) 
cs      write(6,*)'AFTER FMAT'
CD      WRITE(*,*) 'AFTER FMAT', EXP(AP(1)), EXP(AP(2))
      DO J=1,KM
cs      WRITE(*,*) U(J,1),J,EXP(AP(I)),' U(1,I),I'
      ENDDO
      NDP=NDP+1
      NDPD=NDPD+1
      CALL FISHMX(KM,KN,U,C,F,FP,UF,FF,GF,AL0) 
CD      DO I=1,KN
CD      WRITE(*,*) UF(I,I),I,' UF, I'
CD      ENDDO 
cs        WRITE(*,*) ' before smooth' 
cs        WRITE(*,111) (UF(I,I),I=1,KN)                                 
C*****************************************************
C*   INCLUSION of SMOOTHNESS  AND A PRIORI  EST      *
C*****************************************************
CD !!! HERE:
C	WRITE(*,*)'LP=',LP,SQRT(ALP),0.01
      CALL VEC_MATR (KN,KN,SM,AP,FFS)
	DO I=1,KN
		AP0N(I)=AP(I)-AP0(I)
	ENDDO     
	CALL VEC_MATR (KN,KN,SM0,AP0N,FFS0)
      IF(LP.GT.1) THEN
		AACOR=1.
		IF(SQRT(ALP).GT.0.01) AACOR=(SQRT(ALP)/0.01)
     &	*(SQRT(ALP)/0.01)
cs            IF(SQRT(ALP).GT.0.05) AACOR=(SQRT(ALP)/0.05)
cs     & *(SQRT(ALP)/0.05)
      ENDIF
cs      IF(AACOR.GT.100.)AACOR=100.
      DO I=1, KN
		DO J=1,KN
			UF(I,J)=UF(I,J)+AACOR*SM(I,J)+AACOR*SM0(I,J)
		ENDDO
		FF(I)=FF(I)+AACOR*FFS(I)+AACOR*FFS0(I)
      ENDDO
      DO J=1,KN
CD      IF(UF(J,J).EQ.0) WRITE(*,*) J,' F(J,J).EQ.0'
      ENDDO
CD      WRITE(*,*) ' before Lev-Mqrdt'
CD        WRITE(*,111) (UF(I,I),I=1,7)
CD      WRITE(777,*) 'UF(J,2):'
CD      WRITE(777,111) (UF(J,2),J=1,KM)
C*****************************************************
C* Lev-Mqrdt type constraints inclusion              *
C*****************************************************
C*     This correction is included according         *
C*  the methodology described by Dubovik & King [2000]
      IF(NDP.EQ.1) THEN
		IF(KL.EQ.1) THEN
			IF(RSDMAX.LT.1.0E-30)RSDMAX=1.0E-30
			IF(RSDMIN.LT.1.0E-30)RSDMIN=1.0E-30
			ALMSD=(LOG(RSDMAX)-LOG(RSDMIN))/2*
     &		(LOG(RSDMAX)-LOG(RSDMIN))/2
			ALMREAL=(LOG(REALMAX)-LOG(REALMIN))/2*
     &		(LOG(REALMAX)-LOG(REALMIN))/2
			ALMIMAG=(LOG(AIMAGMAX)-LOG(AIMAGMIN))/2*
     &		(LOG(AIMAGMAX)-LOG(AIMAGMIN))/2
			DO IL=1,NLYR
				DO ISD=1,NSD
					ALM(ISDLOC(IL,ISD))=ALMSD
					ALM(IREFLOC(IL,ISD,1))=ALMREAL
					ALM(IREFLOC(IL,ISD,2))=ALMIMAG
				ENDDO
			ENDDO
			IF (NBRDF.GT.0) THEN
				DO I=1,NBRDF
					IF(BRDFMAX(I).LT.1.0E-30)BRDFMAX(I)=1.0E-30
					IF(BRDFMIN(I).LT.1.0E-30)BRDFMIN(I)=1.0E-30
					ALM(IBRDFLOC(I))=(LOG(BRDFMAX(I))-LOG(BRDFMIN(I)))/2*
     &				(LOG(BRDFMAX(I))-LOG(BRDFMIN(I)))/2
				ENDDO
			ENDIF
			IF (NBRDF1.GT.0) THEN
				IF(BRDF1MAX(1).LT.1.0E-30)BRDF1MAX(1)=1.0E-30
				IF(BRDF1MIN(1).LT.1.0E-30)BRDF1MIN(1)=1.0E-30
				ALM(IBRDF1LOC(1))=(LOG(BRDF1MAX(1))-LOG(BRDF1MIN(1)))/2*
     &			(LOG(BRDF1MAX(1))-LOG(BRDF1MIN(1)))/2
cs        DO I=1,NBRDF1
cs        ALM(IBRDF1LOC(I))=(LOG(BRDF1MAX(I))-LOG(BRDF1MIN(I)))/2*
cs     &  (LOG(BRDF1MAX(I))-LOG(BRDF1MIN(I)))/2
cs        ENDDO
			ENDIF
C*********************************CORRECTION*FOR*SHAPE****************************
			IF (NSHAPE.GT.0) THEN
				IF(SHAPEMAX(1).LT.1.0E-30)SHAPEMAX(1)=1.0E-30
				IF(SHAPEMIN(1).LT.1.0E-30)SHAPEMIN(1)=1.0E-30
				ALM(ISHAPELOC(1))=(LOG(SHAPEMAX(1))-LOG(SHAPEMIN(1)))/2*
     &			(LOG(SHAPEMAX(1))-LOG(SHAPEMIN(1)))/2
cs        DO I=1,NBRDF1
cs        ALM(IBRDF1LOC(I))=(LOG(BRDF1MAX(I))-LOG(BRDF1MIN(I)))/2*
cs     &  (LOG(BRDF1MAX(I))-LOG(BRDF1MIN(I)))/2
cs        ENDDO
			ENDIF
C********************************************************************************
		ENDIF
		IF(KL.EQ.0) THEN
			ALMSD=(RSDMAX-RSDMIN)/2*
     &		(RSDMAX-RSDMIN)/2
			ALMREAL=(REALMAX-REALMIN)/2*
     &		(REALMAX-REALMIN)/2
			ALMIMAG=(AIMAGMAX-AIMAGMIN)/2*
     &		(AIMAGMAX-AIMAGMIN)/2
			DO IL=1,NLYR
				DO ISD=1,NSD
					ALM(ISDLOC(IL,ISD))=ALMSD
					ALM(IREFLOC(IL,ISD,1))=ALMREAL
					ALM(IREFLOC(IL,ISD,2))=ALMIMAG
				ENDDO
			ENDDO
			IF (NBRDF.GT.0) THEN
				DO I=1,NBRDF
					ALM(IBRDFLOC(I))=(BRDFMAX(I)-BRDFMIN(I))/2*
     &				(BRDFMAX(I)-BRDFMIN(I))/2
				ENDDO
			ENDIF
			IF (NBRDF1.GT.0) THEN
				ALM(IBRDF1LOC(1))=(BRDF1MAX(1)-BRDF1MIN(1))/2*
     &			(BRDF1MAX(1)-BRDF1MIN(1))/2
cs        DO I=1,NBRDF1
cs        ALM(IBRDF1LOC(I))=(BRDF1MAX(I)-BRDF1MIN(I))/2*
cs     &  (BRDF1MAX(I)-BRDF1MIN(I))/2
cs        ENDDO
			ENDIF
C************************************CORRECTION*FOR*SHAPE****************
			IF (NSHAPE.GT.0) THEN
				ALM(ISHAPELOC(1))=(SHAPEMAX(1)-SHAPEMIN(1))/2*
     &			(SHAPEMAX(1)-SHAPEMIN(1))/2
cs        DO I=1,NBRDF1
cs        ALM(IBRDF1LOC(I))=(BRDF1MAX(I)-BRDF1MIN(I))/2*
cs     &  (BRDF1MAX(I)-BRDF1MIN(I))/2
cs        ENDDO
			ENDIF
C************************************************************************
		ENDIF
      ENDIF
CD        DO I=1,NBRDF
CD        WRITE(*,*) ALM(IBRDFLOC(I)), I,' ALM, I '
CD        ENDDO
CD      WRITE(*,*) ALMSD,ALMREAL,ALMIMAG,
CD     &' ALMSD,ALMREAL,ALMIMAG,'
      IF(LP.LE.IPSTOP) THEN
		DO I=1,IPAR
			DO J=1,NPAR(I)
				UF(ISTAR(I)+J-1,ISTAR(I)+J-1)=
     &			UF(ISTAR(I)+J-1,ISTAR(I)+J-1)+AL/ALM(I)*IK(1)/KN
			ENDDO
		ENDDO
      ENDIF
C******************************************************
C** Solving linear sytem of equation on for 
C**                    each p-iterations
C******************************************************
cs      WRITE(*,*) ' before iterq' 
cs      WRITE(*,*) (UF(I,I),I=1,KN)
CD      WRITE(*,*) (FF(I), I=1,7)
      KN1=KN**2+5*(KN-1)
      CALL ITERQ(IM,KN,KI,UF,FF,GF,EPSQ,Q0,Q,APQ,NQ,UFI,KN1)                       
C******************************************************
C** Determening the optimum lengh of the correction 
C**             DELTA AP()
C******************************************************
      LP=LP+1
      TCINT=1.
      IAC=0
      AC=1.
cs       WRITE(*,*)'APMID'
    3 DO I=1,KN
		APMID(I)=AP(I)-TCINT*Q(I)
cs     WRITE(*,*) Q(I),AP(I),I,' Q(I),AP(I),I'
cs      WRITE(*,*)EXP(APMID(I))
      ENDDO
cs**********LAST********CORRECTION*********************APIN**
      IF(EXP(APMID(KN)).GT.1)APMID(KN)=ALOG(0.999)
      APIN=1.-exp(APMID(KN))
cs************************************************************
      IF(IPRI2.EQ.1)   WRITE(*,*) TCINT,' TCINTMID'
cs      INDB=1
      CALL forwMN(0,KM,KN,IT,KL,IMSC,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     & ,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     & ,IBRDF1LOC,ISHAPELOC,ISTAR
     & ,APMID,SSA,EXT,PF,FPMID,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,
     &DLP,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
c	WRITE(250,*)'THE fourth LOOP',AP(1),AP(KN) !cxxa

      CALL RES (KM,F,FPMID,C,AMID) 
      CALL RES (KN,AF,APMID,SM,ASMID)
      CALL RES (KN,AP0,APMID,SM0,AAAAMID)
C	WRITE(*,*)AMID,ASMID,AAAAMID,AACOR,ALP,'	CXXA'
      IF(IPRI2.EQ.1) THEN
       WRITE(71,122) (II+0.,II=1,KN)
       DO II=1,KN
       WRITE(71,122) (SM(II,JJ),JJ=1,KN)
       ENDDO
      ENDIF
CD      WRITE(*,*) AMID,ASMID, 'AMID,ASMID'
      ALMID=(AMID+AACOR*ASMID+AACOR*AAAAMID)/(KM-KN+IKS+IKS1)
      EPSINT=(AL-ALMID)/AL
      IF(IPRI2.EQ.1) WRITE(*,*) SQRT(ALMID),' ALMID'
CD      WRITE(*,111) (EXP(AP(I)),I=1,KN)
CD      WRITE(*,*)
CD      WRITE(*,111) (EXP(APMID(I)),I=1,KN)
      IF(IPRI2.EQ.1) 
     &WRITE(*,*) EPSINT,EPSP/10.,' EPSINT,EPSP/10.'
C	write(*,*)'EPSINT=',EPSINT,'	CXXA'
      IF(EPSINT.LE.0.) THEN
cd      WRITE(*,*) 'STEP 1-'
		IAC=IAC+1
		TCINT=TCINT/2.
		IF(IAC.LT.12.AND.IM.GT.0.OR.IAC.EQ.2.AND.EPSINT.EQ.0) THEN
			GO TO 3
		ELSE
			IAC=0
			ALP=AL
			TCINT=0.
cd       WRITE(*,*) 'STEP 1+'
			GO TO 7
		ENDIF
      ENDIF

      IF(TCINT.EQ.1) THEN
		DTC=2.*TCINT
	ELSE
		IF(DTC.GT.TCINT) DTC=TCINT
      ENDIF
      ALP=ALMID
      DO J=1,KM
		FP(J)=FPMID(J)
      ENDDO

    4 TC=TCINT
      DTC=DTC/2.
      TCMAX=TC+DTC
      IF(IPRI2.EQ.1)  WRITE(*,*) TCMAX,' TCINTMAX'
cs      WRITE(*,*)'APMAX'
      DO I=1,KN
		APMAX(I)=AP(I)-TCMAX*Q(I)
cs       WRITE(*,*)EXP(APMAX(I))
      ENDDO
cs**********LAST********CORRECTION*********************APIN**
      IF(EXP(APMAX(KN)).GT.1)APMAX(KN)=ALOG(0.999)
      APIN=1.-exp(APMAX(KN))
cs**************************************************************
CD       WRITE(*,111) (EXP(APMAX(I)),I=1,KN)
cs      INDB=1
      CALL forwMN(0 ,KM,KN,IT,KL,IMSC,NW,WAVE,NSD,NBIN1,NBIN,
     &RADIUS
     & ,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     & ,IBRDF1LOC,ISHAPELOC,ISTAR
     & ,APMAX,SSA,EXT,PF,FPMAX,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,
     &DLP,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
	WRITE(250,*)'THE fifth LOOP',AP(1),AP(KN) !cxxa

      CALL RES (KM,F,FPMAX,C,AMAX)
      CALL RES (KN,AF,APMAX,SM,ASMAX)
      CALL RES (KN,AP0,APMAX,SM0,AAAAMAX)
      IF(IPRI2.EQ.1) WRITE(*,*) AMAX,AACOR*ASMAX, 'AMAX, ASMAX'
      ALMAX=(AMAX+AACOR*ASMAX+AACOR*AAAAMAX)/(KM-KN+IKS+IKS1)
      IF(IPRI2.EQ.1) WRITE(*,*) SQRT(ALP),SQRT(ALMAX),' ALP,ALMAX'

      IF(ALMAX.GE.ALP) THEN
       TC=TCINT
       IF(DTC.GE.TCINT) DTC=DTC/2.
       TCMIN=TC-DTC
       IF(IPRI2.EQ.1) WRITE(*,*) TCMIN,' TCINTMIN'
cs       WRITE(*,*)'APMIN'
       DO I=1,KN
        APMIN(I)=AP(I)-TCMIN*Q(I)
cs       WRITE(*,*)EXP(APMIN(I))
       ENDDO
cs**********LAST********CORRECTION*********************APIN**
       IF(EXP(APMIN(KN)).GT.1)APMIN(KN)=ALOG(0.999)
       APIN=1.-exp(APMIN(KN))
cs***************************************************************
cs     Corrected by me
cs       INDB=1
cs     *****************
       CALL forwMN(0,KM,KN,IT,KL,IMSC,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     &,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     &,IBRDF1LOC,ISHAPELOC,ISTAR
     &,APMIN,SSA,EXT,PF,FPMIN,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,
     &DLP,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
c	WRITE(250,*)'THE sixth LOOP',AP(1),AP(KN) !cxxa

       CALL RES (KM,F,FPMIN,C,AMIN)
       CALL RES (KN,AF,APMIN,SM,ASMIN)
       CALL RES (KN,AP0,APMIN,SM0,AAAAMIN)
       IF(IPRI2.EQ.1) WRITE(*,*) AMIN,AACOR*ASMIN,' AMIN,ASMIN'
       ALMIN=(AMIN+AACOR*ASMIN+AACOR*AAAAMIN)/(KM-KN+IKS+IKS1)
       IF(IPRI2.EQ.1) WRITE(*,*) SQRT(ALP),SQRT(ALMIN),' ALP,ALMIN'
        IF(ALMIN.GE.ALP) THEN
         GO TO 7
        ELSE
         EPSINT=(ALP-ALMIN)/ALP
        ENDIF
        IF(IPRI2.EQ.1) WRITE(*,*) EPSINT,EPST,' EPSINT,EPST, 1'
        IF (EPSINT.LE.EPST) THEN
         TCINT=TCMIN
         ALP=ALMIN
         DO J=1,KM
         FP(J)=FPMIN(J)
         ENDDO
         GO TO 7       
        ENDIF
       ALP=ALMIN
       TCINT=TCMIN
       GO TO 4
      ELSE
       EPSINT=(ALP-ALMAX)/ALP
       IF(IPRI2.EQ.1) WRITE(*,*) EPSINT,EPST,' EPSINT,EPST, 2'
       IF (EPSINT.LE.EPST) THEN
        TCINT=TCMAX
        ALP=ALMAX
        DO J=1,KM
        FP(J)=FPMAX(J)
        ENDDO
        GO TO 7
       ENDIF
       ALP=ALMAX
       TCINT=TCMAX
       GO TO 4
      ENDIF

    7 DO I =1,KN
		AP(I)=AP(I)-TCINT*Q(I)
      ENDDO
cs**********LAST********CORRECTION*********************APIN**
      IF(EXP(AP(KN)).GT.1)AP(KN)=ALOG(0.999)
      APIN=1.-exp(AP(KN))
cs*********************************************************LAST*APIN*
C*******Assingning BRDF albedo at 1020 to be equal BRDF at 670
cs      DO I=1,NBRDF
cs      DO J=1,NW
cs      IF(J.EQ.3)XBT=AP(ISTAR(IBRDFLOC(I))+J-1)
cs      IF(J.EQ.4)AP(ISTAR(IBRDFLOC(I))+J-1)=XBT
cs      ENDDO
cs      ENDDO      
cs      DO I=31,KN
cs      write(6,*)'AP',I,EXP(AP(I))
cs      ENDDO
C******************************************************************
cs      write(6,*)'APIN',EXP(AP(KN)),APIN,EXP(AP(KN))+APIN
      IF(IPRI2.EQ.1)  WRITE(*,*) TCINT,' TCINFINAL'
      IF(IM.EQ.4)ALL=SQRT(ALP)
C	WRITE(*,*)AL,ALP,(AL-ALP)/AL,EPSP,'	CXXA'
      IF((AL-ALP)/AL.GT.EPSP) THEN
cs**********************LAST***LAST***CHANGE*******************************************
		AL=ALP
		ALL=SQRT(AL)
C		WRITE(*,*)AL,ALP,(AL-ALP)/AL,EPSP,'CXXA'
cs************************************************************************************
		WRITE(1011,*)SQRT(ALP)*100.,'%- Residual after',LP,'-th iteration'
		WRITE(777,*) SQRT(ALP), LP,KN, ' ALP,LP,KN after change'
cs      write(6,*)exp(AP(KN))
cs       DO IBB=1,NBRDF1
cs       WRITE(*,*) IBB,' -th non-spectral BRDF parameter:'
cs       WRITE(*,*)  EXP(AP(ISTAR(IBRDF1LOC(1))+IBB-1))
cs       ENDDO
C********************************CORRECTION*FOR*SHAPE***************************
cs       DO IBB=1,NSHAPE
cs       WRITE(*,*) IBB,' -th shape parameter:'
cs       WRITE(*,*)  EXP(AP(ISTAR(ISHAPELOC(1))+IBB-1))
cs       ENDDO
C*********************************************************************************
		IF (IPRI2.EQ.1) THEN
			DO IL=1,NLYR
				DO ISD=1,NSD
					WRITE(77,*) ' SINGLE SCATTERING ALBEDO:'
					DO IW=1,NW
						WRITE(77,111) WAVE(IW), SSA(IW,IL,ISD)
					ENDDO
					WRITE(77,*) ' REAL PART of REF. INDEX:'
					DO IW=1,NW
						WRITE(77,*)  
     1					WAVE(IW),EXP(AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1))
					ENDDO
					WRITE(77,*) ' IMAGINARY PART of REF. INDEX:'
					DO IW=1,NW
						WRITE(77,*)  
     1					WAVE(IW),EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1))
					ENDDO
					WRITE(77,*) ' SIZE DISTRIBUTION'
					DO I=1,NBIN(ISD)
						WRITE(77,*) 
     1					RADIUS(ISD,I), EXP(AP(ISTAR(ISDLOC(IL,ISD))+I-1))
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		GO TO 12
      ENDIF
	
c	write(*,*)'goto 1112'
      IF(ICC.LT.3.AND.IACOV.GT.0) THEN
		IF(ICC.EQ.1) IC=0
		IF(ICC.EQ.2) IC=1
		GOTO 1112
      ENDIF
c========================xxa=================================
	IF(SQRT(ALP)*100.GT.15)RETURN
c========================xxa==================================
!!!      WRITE(*,*) SQRT(AR0/ASM), LP,KN, ' AR0,LP,KN' 
CC*****************************************************************
CC*** for printing Phase function and Error calcualting !!! *******
CC*****************************************************************
cs      INDB=1
cs      CALL forwMN(0,KM,KN,IT,KL,IMSC,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
cs     &,NLYR,NBRDF,NBRDF1,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
cs     &,IBRDF1LOC,ISTAR,
cs     & AP,SSA,EXT,PF,FP1,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
cs     &ANGLESCA,FTAU,FUMUY,FPHI,0,0)
cs       DO J=1,KM
cs       C(J,J)=1.
cs       ENDDO
cs       CALL RES (KM,F,FP1,C1,ALP)
cs       CALL RES (KM,F,FP1,C,ALPW)
cs       write(6,*)'ALPW',SQRT(ALPW/KM)
cs       WRITE(*,*) SQRT(ALP/KM)*100.,' % - OPTICAL RESIDUAL'
cs      IF(IPRI2.EQ.1) THEN
cs      IF(IMSC.NE.IMSC1) THEN
cs      WRITE(*,*) 'Phase function and Error'
      INDB=1
      ip=0
      do i1=1,NW
		IF(IPOLPR(i1).EQ.1)INDPL=1
      enddo
cs      IF(INDPL.EQ.1.OR.NBRDF.GT.0)ip=1
cs      write(6,*)'CHECK1'
      IF(INDPL.EQ.1)ip=1
cs*****LAST***CORRECTION*ip=1*AFTER*KL**********************************************
      CALL forwMN(0,KM,KN,IT,KL,0,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     & ,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     & ,IBRDF1LOC,ISHAPELOC,ISTAR
     & ,AP,SSA,EXT,PF,FP1,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
     &ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
C	WRITE(*,*)'THE seventh LOOP',AP(1),AP(KN) !cxxa

       INDB=0
cs      ELSE
cs       DO J=1,KM
cs       FP1(J)=FP(J)
cs       ENDDO
cs      ENDIF
cs       write(6,*)'ip=',ip
cs      write(6,*)'CHECK2'
cs*****LATST***CORRECTION*ip=1*AFTER*KL**********************************************
      AREF1=AREF
      CALL FMAT (KM,KN,IT,KL,0,NW,WAVE,NSD
     &,NBIN,RADIUS,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ISDLOC,IREFLOC
     &,IBRDFLOC,IBRDF1LOC,ISHAPELOC,ISTAR,AP,FP1,IMTX,DL,LP
     &,REALMIN,REALMAX,AIMAGMIN,AIMAGMAX
     &,BRDFMIN,BRDFMAX,BRDF1MIN,BRDF1MAX,SHAPEMIN,SHAPEMAX
     &,ALL,AREF,AREF1,U,IEL,IS,0,US,UP,SSA,PF,UPOL,DPL,0,0,1)
cs       do I=1,KN
cs       write(6,*)I
cs       write(6,*)(U(J,I),J=1,KM)
cs       write(6,*)
cs       enddo
cs       INDB=1
      NDP=NDP+1
      NDPD=NDPD+1
      CALL FISHMX(KM,KN,U,C,F,FP1,UF,FF,GF,AL0) 
cs      ENDIF                               
C*****************************************************
C*   SMOOTHNESS INCLUSION                            *
C*****************************************************
cs       write(6,*)'UF'
cs       do i1=1,KN
cs       write(6,*)UF(i1,i1)
cs       enddo
cs       write(6,*) 
      CALL VEC_MATR (KN,KN,SM,AP,FFS)
      DO I=1,KN
		AP0N(I)=AP(I)-AP0(I)
      ENDDO     
      CALL VEC_MATR (KN,KN,SM0,AP0N,FFS0)
      DO I=1,KN
		DO J=1,KN
			UF(I,J)=UF(I,J)+SM(I,J)+SM0(I,J)
		ENDDO
		FF(I)=FF(I)+FFS(I)+FFS0(I)
      ENDDO
      do i1=1,KN
		do i2=1,KN
			UFF(i1,i2)=UF(i1,i2)
		enddo
      enddo
cs       write(6,*)'U before'
cs       do i1=1,KN
cs       write(6,*)i1,i1,UFF(i1,i1)
cs       enddo
cs       write(6,*)  
cs        CALL ITERQ(IM,KN,KI,UF,FF,GF,EPSQ,Q0,Q,APQ,NQ,UFI,KN1)  
CCD****************************************************************
CCS********************OUTPUT**FILE********************************
C**********************Selection*of*the*minimum*value*of*PSD****** 
cs      write(6,*)'Writing output'
      CALL forwMN(0,KM,KN,IT,KL,IMSC,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     & ,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     & ,IBRDF1LOC,ISHAPELOC,ISTAR
     & ,AP,SSA,EXT,PF,FP1,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,
     &DLP,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
C	WRITE(*,*)'THE eighth LOOP',AP(1),AP(KN) !cxxa

cs      write(6,*)'FTAU',FTAU
cs      write(6,*)'FTAU',FTAU
       CALL RES (KM,F,FP1,C1,ALP)
       CALL RES (KM,F,FP1,C,ALPW)
cs       write(6,*)'ALPW1',SQRT(ALPW/(KM-KN+IKS+IKS1))
cs       write(6,*)'FUMUY'
cs       do i1=1,NW
cs       do i2=1,FTAU(i1)
cs       write(6,*)'WAVE,FTAU,FUMUY',WAVE(i1),FTAU(i1),FUMUY(i1,i2,1)
cs       enddo
cs       enddo
cs       write(6,*)'FPHI'
cs       do i1=1,NW
cs       do i2=1,FTAU(i1)
cs       do i3=1,FUMUY(i1,i2,1)
cs       write(6,*)'WAVE,FTAU,FUMUY,FPHI',WAVE(i1),FTAU(i1),
cs     & FUMUY(i1,i2,1),FPHI(i1,i2,1,i3)
cs       enddo
cs       enddo
cs       enddo
cs       WRITE(*,*) SQRT(ALP/KM)*100.,' % - OPTICAL RESIDUAL'     
      is=1
      do IL=1,NLYR
		do ISD=1,NSD
			do I=9,15
				SDM(IL,ISD,is)=EXP(AP(ISTAR(ISDLOC(IL,ISD))+I-1))
cs      write(6,*)ISTAR(ISDLOC(IL,ISD))+I-1
cs      write(6,*)EXP(AP(ISTAR(ISDLOC(IL,ISD))+I-1))
				is=is+1
			enddo
		enddo
      enddo
      do IL=1,NLYR
		do ISD=1,NSD
			SDMIN(IL,ISD)=1.0e+30
		enddo
      enddo
cs      write(6,*)'SDM',SDM
      do IL=1,NLYR
		do ISD=1,NSD
			do i1=1,4
		IF(SDMIN(IL,ISD).GT.SDM(IL,ISD,i1).AND.SDM(IL,ISD,i1).GT.0)THEN
					SDMIN(IL,ISD)=SDM(IL,ISD,i1)
					IMIN(IL,ISD)=8+i1
				ENDIF
			enddo
		enddo
      enddo
c	write(*,*)'xxa1'
cs      write(6,*)'SDMIN',SDMIN
cs      do i1=1,NLYR
cs      write(6,*)(IMIN(i1,i2),i2=1,NSD)
cs      enddo
C**********************************************************************
C***********************Size*distributions*of*fine *and*coarse*modes***
C************************Changed if to if1*******************************
      if1=1
C************************************************************************
      icr=1
      icr1=1
      ict=1
      do i1=1,NLYR
		do i2=1,NSD
			do I=1,NBIN(i2)
				SDT(i1,i2,ict)=EXP(AP(ISTAR(ISDLOC(i1,i2))+I-1))
				XNSD(i1,i2,ict)=(3./(4.*(RADIUS(i2,ict)**3.)*3.1415926))*
     $			SDT(i1,i2,ict)
				ict=ict+1
				IF(I.LE.IMIN(i1,i2))THEN
					SDF(i1,i2,if1)=EXP(AP(ISTAR(ISDLOC(i1,i2))+I-1))
					IF(I.EQ.IMIN(i1,i2))SDF(i1,i2,icr1)=SDF(i1,i2,icr1)/2.
					RADIUSF(i2,if1)=RADIUS(i2,I)
			XNSDF(i1,i2,if1)=(3./(4.*(RADIUSF(i2,if1)**3.)*3.1415926))*
     $				SDF(i1,i2,if1)
					if1=if1+1
					icr1=icr1+1
				ENDIF
				IF(I.GE.IMIN(i1,i2))THEN
					SDC(i1,i2,icr)=EXP(AP(ISTAR(ISDLOC(i1,i2))+I-1))
					IF(I.EQ.IMIN(i1,i2))SDC(i1,i2,icr)=SDC(i1,i2,icr)/2.
					RADIUSC(i2,icr)=RADIUS(i2,I)
			XNSDC(i1,i2,icr)=(3./(4.*(RADIUSC(i2,icr)**3.)*3.1415926))*
     $				SDC(i1,i2,icr)
					icr=icr+1
				ENDIF
			enddo
		enddo
      enddo
C	WRITE(250,'(2F15.5)')RADIUSF(1,:),RADIUSC(1,:)
CS	WRITE(250,*)'END'
CS      do i1=1,NLYR
CS      do i2=1,NSD
CS     do I=1,NBIN(i2)
CS      write(250,*)RADIUSF(i2,I),SDF(i1,i2,I),XNSDF(i1,i2,I)
CS      enddo
CS      enddo
CS      enddo
CS      do i1=1,NLYR
CS      do i2=1,NSD
CS      do I=1,NBIN(i2)
CS      write(250,*)RADIUSC(i2,I),SDC(i1,i2,I),XNSDC(i1,i2,I)
CS      enddo
CS      enddo
CS      enddo
C*********************************************************************
C*********************The number*of*bins*for*fine *and*coarse*modes** 
C===============================XXA=====================================
	NBINF=0
	NBINC=0
C==============================XXA==================================
      do i1=1,NLYR
		do i2=1,NSD
			do i3=1,IMIN(i1,i2)
				NBINF(i1,i2)=NBINF(i1,i2)+1
			enddo
			do i3=IMIN(i1,i2),NBIN(i2)
				NBINC(i1,i2)=NBINC(i1,i2)+1
			enddo
		enddo
      enddo
      do i1=1,NLYR
cs      write(6,*)'NBINF',(NBINF(i1,i2),i2=1,NSD)
cs      write(6,*)'NBINC',(NBINC(i1,i2),i2=1,NSD)
      enddo
C*********************************************************************
C******R32(effective*radius)*for*fine*and*coarse*modes****************
      do i1=1,NSD
		rf3(i1)=0.
		rf2(i1)=0.
		rt(i1)=0.
      enddo

      do i1=1,NSD
		IF(RADIUSF(I1,2).LT.1.0E-30)RADIUSF(I1,2)=1.0E-30
		IF(RADIUSF(I1,1).LT.1.0E-30)RADIUSF(I1,1)=1.0E-30
		dlnr(i1)=ALOG(RADIUSF(i1,2))-ALOG(RADIUSF(i1,1))
      enddo

      do i1=1,NLYR
		do i2=1,NSD
			do i3=1,NBINF(i1,i2)
				rf3(i2)=rf3(i2)+XNSDF(i1,i2,i3)*RADIUSF(i2,i3)**3*dlnr(i2)     
				rf2(i2)=rf2(i2)+XNSDF(i1,i2,i3)*RADIUSF(i2,i3)**2*dlnr(i2)
			enddo
		enddo
      enddo
      do i1=1,NSD
		rf(i1)=rf3(i1)/rf2(i1)
      enddo
      do i1=1,NSD
		rc3(i1)=0.
		rc2(i1)=0.
      enddo

      do i1=1,NSD
		IF(RADIUSC(I1,2).LT.1.0E-30)RADIUSC(I1,2)=1.0E-30
		IF(RADIUSC(I1,1).LT.1.0E-30)RADIUSC(I1,1)=1.0E-30
		dlnr(i1)=ALOG(RADIUSC(i1,2))-ALOG(RADIUSC(i1,1))
      enddo
      do i1=1,NLYR
		do i2=1,NSD
			do i3=1,NBINC(i1,i2)
				rc3(i2)=rc3(i2)+XNSDC(i1,i2,i3)*RADIUSC(i2,i3)**3*dlnr(i2)     
				rc2(i2)=rc2(i2)+XNSDC(i1,i2,i3)*RADIUSC(i2,i3)**2*dlnr(i2)
			enddo
		enddo
      enddo
      do i1=1,NSD
		rc(i1)=rc3(i1)/rc2(i1)
      enddo
      do i1=1,NSD
		rc3(i1)=0.
		rc2(i1)=0.
      enddo
      do i1=1,NLYR
		do i2=1,NSD
			do i3=1,NBIN(i2)
				rc3(i2)=rc3(i2)+XNSD(i1,i2,i3)*RADIUS(i2,i3)**3*dlnr(i2)     
				rc2(i2)=rc2(i2)+XNSD(i1,i2,i3)*RADIUS(i2,i3)**2*dlnr(i2)
			enddo
		enddo
      enddo
      do i1=1,NSD
		rt(i1)=rc3(i1)/rc2(i1)
      enddo
cs      write(6,*)'rf',rf
cs      write(6,*)'rc',rc
C***********************************************************************
C***Volume*median*radius*and*standard*deviation**for*fine*and*coarse*modes*
      do i1=1,NSD
		rfl1(i1)=0.
		rfl2(i1)=0.
      enddo

      do i1=1,NSD
		IF(RADIUSF(I1,2).LT.1.0E-30)RADIUSF(I1,2)=1.0E-30
		IF(RADIUSF(I1,1).LT.1.0E-30)RADIUSF(I1,1)=1.0E-30
		dlnr(i1)=ALOG(RADIUSF(i1,2))-ALOG(RADIUSF(i1,1))
      enddo
c
      do i1=1,NLYR
		do i2=1,NSD
			do i3=1,NBINF(i1,i2)
				IF(RADIUSF(I2,I3).LT.1.0E-30)RADIUSF(I2,I3)=1.0E-30
				rfl1(i2)=rfl1(i2)+SDF(i1,i2,i3)*ALOG(RADIUSF(i2,i3))*dlnr(i2) 
				rfl2(i2)=rfl2(i2)+SDF(i1,i2,i3)*dlnr(i2)   
			enddo
		enddo
      enddo

      do i1=1,NSD
		rfl(i1)=rfl1(i1)/rfl2(i1)
      enddo
      do i1=1,NSD
		rcl1(i1)=0.
		rcl2(i1)=0.
      enddo
 
      do i1=1,NSD
		IF(RADIUSC(I1,1).LT.1.0E-30)RADIUSC(I1,1)=1.0E-30
		dlnr(i1)=ALOG(RADIUSC(i1,2))-ALOG(RADIUSC(i1,1))
      enddo

      do i1=1,NLYR
		do i2=1,NSD
			do i3=1,NBINC(i1,i2)
				IF(RADIUSC(I2,I3).LT.1.0E-30)RADIUSC(I2,I3)=1.0E-30
				rcl1(i2)=rcl1(i2)+SDC(i1,i2,i3)*ALOG(RADIUSC(i2,i3))*dlnr(i2) 
				rcl2(i2)=rcl2(i2)+SDC(i1,i2,i3)*dlnr(i2)   
			enddo
		enddo
      enddo
      do i1=1,NSD
		rcl(i1)=rcl1(i1)/rcl2(i1)
      enddo
      do i1=1,NSD
		rcl1(i1)=0.
		rcl2(i1)=0.
      enddo

      do i1=1,NLYR
		do i2=1,NSD
			do i3=1,NBIN(i2)
				IF(RADIUS(I2,I3).LT.1.0E-30)RADIUS(I2,I3)=1.0E-30
				rcl1(i2)=rcl1(i2)+SDT(i1,i2,i3)*ALOG(RADIUS(i2,i3))*dlnr(i2) 
				rcl2(i2)=rcl2(i2)+SDT(i1,i2,i3)*dlnr(i2)   
			enddo
		enddo
      enddo
      do i1=1,NSD
		rft(i1)=rcl1(i1)/rcl2(i1)
      enddo
cs      write(6,*)'rfl',rfl,exp(rfl)
cs      write(6,*)'rcl',rcl,exp(rcl)
      do i1=1,NSD
		sf(i1)=0.
		rflc(i1)=0.
      enddo

      do i1=1,NSD
		IF(RADIUSF(I1,2).LT.1.0E-30)RADIUSF(I1,2)=1.0E-30
		IF(RADIUSF(I1,1).LT.1.0E-30)RADIUSF(I1,1)=1.0E-30
		dlnr(i1)=ALOG(RADIUSF(i1,2))-ALOG(RADIUSF(i1,1))
      enddo

      do i1=1,NLYR
		do i2=1,NSD
			do i3=1,NBINF(i1,i2)
				IF(RADIUSF(I2,I3).LT.1.0E-30)RADIUSF(I2,I3)=1.0E-30
				sf(i2)=sf(i2)+
     &			(ALOG(RADIUSF(i2,i3))-(rfl(i2)))**2*SDF(i1,i2,i3)*dlnr(i2)
				rflc(i2)=rflc(i2)+SDF(i1,i2,i3)*dlnr(i2)   
			enddo
		enddo
      enddo
      do i1=1,NSD
		sf(i1)=sqrt(sf(i1)/rflc(i1))
      enddo

      do i1=1,NSD
		IF(RADIUSC(I1,2).LT.1.0E-30)RADIUSC(I1,2)=1.0E-30
		IF(RADIUSC(I1,1).LT.1.0E-30)RADIUSC(I1,1)=1.0E-30
		dlnr(i1)=ALOG(RADIUSC(i1,2))-ALOG(RADIUSC(i1,1))
		sc(i1)=0.
		rclc(i1)=0.
      enddo

      do i1=1,NLYR
		do i2=1,NSD
			do i3=1,NBINC(i1,i2)
				IF(RADIUSC(I2,I3).LT.1.0E-30)RADIUSC(I2,I3)=1.0E-30
				sc(i2)=sc(i2)+(ALOG(RADIUSC(i2,i3))-(rcl(i2)))
     &			**2*SDC(i1,i2,i3)*dlnr(i2)
				rclc(i2)=rclc(i2)+SDC(i1,i2,i3)*dlnr(i2)    
			enddo
		enddo
      enddo
      do i1=1,NSD
		sc(i1)=SQRT(sc(i1)/rclc(i1))
      enddo

      do i1=1,NSD
		IF(RADIUS(I1,2).LT.1.0E-30)RADIUS(I1,2)=1.0E-30
		IF(RADIUS(I1,1).LT.1.0E-30)RADIUS(I1,1)=1.0E-30
		dlnr(i1)=ALOG(RADIUS(i1,2))-ALOG(RADIUS(i1,1))
		st(i1)=0.
		rct(i1)=0.
      enddo

      do i1=1,NLYR
		do i2=1,NSD
			do i3=1,NBIN(i2)
				IF(RADIUS(I2,I3).LT.1.0E-30)RADIUS(I2,I3)=1.0E-30
				st(i2)=st(i2)+(ALOG(RADIUS(i2,i3))-(rft(i2)))
     &			**2*SDT(i1,i2,i3)*dlnr(i2)
				rct(i2)=rct(i2)+SDT(i1,i2,i3)*dlnr(i2)    
			enddo
		enddo
      enddo
      do i1=1,NSD
		st(i1)=SQRT(st(i1)/rct(i1))
      enddo
C	write(*,*)'xxa11' 
cs      write(6,*)'sf',sf
cs      write(6,*)'sc',sc
cs      write(6,*)'rflc',rflc
cs      write(6,*)'rclc',rclc
C**************************************************************************
C***Optical*characteristics*for*fine*and*coarse*modes*********************
       do i1=1,NLYR
		do i2=1,NSD
			NBINCR(i2)=NBINF(i1,i2)
		enddo
		CALL forwMN(0,KM,KN,IT,KL,1,NW,WAVE,NSD,NBIN1,NBINCR,RADIUS
     &,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     &,IBRDF1LOC,ISHAPELOC,ISTAR,
     & AP,SSAF,EXTF,PFF,FPC,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLPF,
     &ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
       enddo
       do i1=1,NLYR
		do i2=1,NSD
			NBINCR(i2)=NBINC(i1,i2)
			NBINCR1(i2)=NBINF(i1,i2)
c			write(*,*)NBINC(I1,I2),NBINCR1(I2) !CXXA
		enddo
		CALL forwMN(0,KM,KN,IT,KL,1,NW,WAVE,NSD,NBINCR1,NBINCR,RADIUS
     &	,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     &	,IBRDF1LOC,ISHAPELOC,ISTAR,
     &	AP,SSAC,EXTC,PFC,FPC,INDB,IEL,IS,FLDD,FLUP,FLDR,1,IPOLPR,DLPC,
     &	ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
       enddo
cs      write(6,*)'IPOLPR m',IPOLPR
      do i1=1,NSD
		write(1011,*)i1,'-th component'
		do i2=1,NLYR
cs      write(1011,*)i2,'-th layer'
			do i3=1,NW
				write(1011,66)'Wavelength',WAVE(i3)
			write(1011,*)'Extinction of aerosol: ( total  fine      corse )'//
     +'     SSA: ( total     fine      corse )'
      write(1011,67)EXT(i3,i2,i1),EXTF(i3,i2,i1),EXTC(i3,i2,i1),
     &SSA(i3,i2,i1),SSAF(i3,i2,i1),SSAC(i3,i2,i1)
				IF(IPOLPR(i3).EQ.0)THEN
      write(1011,*)'Phase function: ( angle       total       fine       
     &corse )'
				ENDIF
				IF(IPOLPR(i3).EQ.1)THEN
      write(1011,*)'Phase function: ( angle    total       fine      '//
     &'  corse )      Degree of linear polarization: (   total     fine   
     &      corse)'
				ENDIF
				IF(IPOLPR(i3).EQ.1)THEN
					do i4=1,IANGL
      write(1011,77)ANG(i4),PF(i4,i3,i2,i1),PFF(i4,i3,i2,i1),
     &PFC(i4,i3,i2,i1),-1*DLP(i4,i3,i2,i1),-1*DLPF(i4,i3,i2,i1),
     &-1*DLPC(i4,i3,i2,i1)
					enddo
				ENDIF
				IF(IPOLPR(i3).EQ.0)THEN
					do i4=1,IANGL
      write(1011,68)ANG(i4),PF(i4,i3,i2,i1),PFF(i4,i3,i2,i1),
     &PFC(i4,i3,i2,i1)
					enddo
				ENDIF
				write(1011,*)''
			enddo 
		enddo
      enddo
C***************************************************************************
C**Errors*and*bias*calculation*as*function*of*wavelength*and*measurements*type**
cs      do i1=1,INOISE
cs      write(6,*)(KNOISE(i1,i2),i2=1,IK(i1))
cs      enddo
C******The*number*of*measurements*of*each*type**********************************
      do i1=1,INOISE
		in3(i1)=0
      enddo
      do i1=1,INOISE
		do i2=1,IK(i1)
			in3(i1)=in3(i1)+1
		enddo
      enddo
C******************************************************************************
C****************Sun*error*and*bias*calculation********************************
      IF(INOISE.GT.1)THEN
		xa=0.
		do i1=1,NW
			do i2=1,KM
				IF(KNOISE(2,i1).EQ.i2)THEN
					FPT(i1)=EXP(F(i2))
					FP1T(i1)=EXP(FP1(i2))
					xa=xa+(FPT(i1)-FP1T(i1))
				ENDIF
			enddo
		enddo
		xa=xa/NW
		CALL RES (NW,FPT,FP1T,C1,ALP)
      ENDIF
C******************************************************************************
C****Introdusing*constant*bias*in*optical*depth*and*wavelegth*dependent*bias***
      IF(INOISE.GT.1)THEN
		xaa=0.
		xaa1=0.
		xaaa=0.
		do i1=1,NW
			do i2=1,KM
				IF(KNOISE(2,i1).EQ.i2)THEN
					xb=FPT(i1)-DNN(2)
					xbb=FPT(i1)+DNN(2)
					IF(XB.LT.1.0E-30)XB=1.0E-30
					IF(XBB.LT.1.0E-30)XBB=1.0E-30
					xb=LOG(xb)
					xbb=LOG(xbb)
					xaa=xaa+(xb-FP1(i2))
					xaaa=xaaa+(xbb-FP1(i2))
					xaa1=xaa1+(F(I2)-FP1(i2))
				ENDIF
			enddo
		enddo
		xaa=xaa/NW
		xaa1=xaa1/NW
		xaaa=xaaa/NW
      ENDIF
C******BIASG*and*BIASG1*are*constant*biases*BIASGG*is*wavelength*dependent*bias**
      IF(INOISE.GT.1)THEN
		do i1=1,NW
			do i2=1,KM
				IF(KNOISE(2,i1).EQ.i2)THEN
cs************LAST****************CORRECTION****************************
					BIASG(i2)=xaa
					BIASGH(i2)=xaa
					BIASGL(i2)=xaa
					BIASGG(i2)=xaa1
					BIASG1(i2)=xaaa
					BIASG1H(i2)=xaaa
					BIASG1L(i2)=xaaa
				ENDIF
			enddo
		enddo
		WRITE(1011,69)'Sun error: ', SQRT(ALP/NW)
		WRITE(1011,*)''
      ENDIF
C*********************************************************************************
C*********************************************************************************
C****************Sky*error*calculation********************************************
      IF(INOISE.GT.1)THEN
		in=in3(1)
		do i1=1,in
			do i2=1,KM
				IF(KNOISE(1,i1).EQ.i2)THEN
					FPT(i1)=F(i2)
					FP1T(i1)=FP1(i2)
				ENDIF
			enddo
		enddo
C******************************************correction for Ilya************
		xin=in
		IF(in.lt.1.)then 
			xin=1e-20
		endif
C*************************************************************************
		CALL RES (in,FPT,FP1T,C1,ALP)
		WRITE(1011,70)'Sky error: ', SQRT(ALP/xin)*100.,'%'
		write(1011,*)''
      ENDIF
C****************Polarization*error*and*bias*calculation***************************
      IF(INOISE.GT.1)THEN
		do i1=1,NW
			IF(IPOLPR(i1).EQ.1)INDPL=1
		enddo
		IF(INDPL.EQ.1)THEN
			do ip=1,KM
				FPT(ip)=0.
				FP1T(ip)=0.
			enddo
cs      write(6,*)'in3',in3
			in=in3(3)
			do i1=1,in
				do i2=1,KM
					IF(KNOISE(3,i1).EQ.i2)THEN
						FPT(i1)=EXP(F(i2))
						FP1T(i1)=EXP(FP1(i2))
					ENDIF
				enddo
			enddo
C******************************************correction for Ilya************
			xin=in
			IF(in.lt.1.)then 
				xin=1e-20
			endif
C*************************************************************************
			CALL RES (in,FPT,FP1T,C1,ALP)
			WRITE(*,75)'Polarization error: ', SQRT(ALP/xin)
		ENDIF      
		do ip=1,KM
			FPT(ip)=0.
			FP1T(ip)=0.
		enddo
      ENDIF
C***************************Satellite*meas*error*calculation*********************************
      IF(NBRDF.GT.0.OR.INOISE.EQ.1)THEN
		do ip=1,KM
			FPT(ip)=0.
			FP1T(ip)=0.
		enddo
cs      write(6,*)'in3',in3
C*****************************************************************
		IF(INOISE.GT.1)THEN
			in=in3(3)
			do i1=1,in
				do i2=1,KM
					IF(KNOISE(3,i1).EQ.i2)THEN
						FPT(i1)=F(i2)
						FP1T(i1)=FP1(i2)
					ENDIF
				enddo
			enddo
		ELSE
			in=KM
			do i1=1,in
				do i2=1,KM
					IF(i1.EQ.i2)THEN
						FPT(i1)=F(i2)
						FP1T(i1)=FP1(i2)
					ENDIF
				enddo
			enddo
		ENDIF
C******************************************correction for Ilya************
		xin=in
		IF(in.lt.1.)then 
			xin=1e-20
		endif
C*************************************************************************
		CALL RES (in,FPT,FP1T,C1,ALP)
		WRITE(*,88)'Airsraft/Spacecraft measurements error: ',
     &	SQRT(ALP/xin)*100.,' %'
      ENDIF      
      do ip=1,KM
		FPT(ip)=0.
		FP1T(ip)=0.
      enddo
c*********************************************************************************************
C******Wavelength*ang*angular*dependent*bias*for*sky*radiance*and*polarization*calculation****
C*********************************************************************************************
C*********NW-number of wavelengths, FTAU(NW)-number of different types of observations for each wavelenght
C*********FUMUY(NW,FTAU)-number of different observation angles for each (NW,FTAU) sets, FPHI(NW,FTAU,FUMUY)-number of azimuth angles for each
C********(NW,FTAU,FUMUY) sets
c================xxa============================================
	inwa=0
c===============xxa=============================================
       do i1=1,NW
		do i2=1,FTAU(i1)
			do i3=1,FUMUY(i1,i2,1)
				inwa(i1,i2)=inwa(i1,i2)+FPHI(i1,i2,1,i3)
C*********inwa(NW,FTAU)-the number of observations for each wavelength and each measurement type
			enddo
		enddo
       enddo
       do i1=1,NW
		IF(IPOLPR(i1).EQ.1)INDPL=1
       enddo
cs       do i1=1,NW
cs       do i2=1,FTAU(i1)
cs       write(6,*)'WAVE(i1),FTAU,FUMUY,inwa',WAVE(i1),
cs     &i2,FUMUY(i1,i2,1),inwa(i1,i2)
cs       enddo
cs       enddo
c==================================xxa==================
	inwt=0
	inws=0
	inwp=0
c================================xxa====================
c        write(*,*)'inwt',inwt(1:4)
c        write(*,*)'inws',inws(1:4)
c        write(*,*)'inwp',inwp(1:4)
      do i1=1,NW
		do i2=1,FTAU(i1)
C*********Default indexes for observations of different type are: optical depth and sky -1, polarimetric -2 satellite-3***********************
cs        IF (INDPL.EQ.0)THEN
			IF(INDORDER(i1).EQ.1.AND.i2.EQ.1)THEN
				inws(i1)=inwa(i1,i2)-1
				inwt(i1)=1
			ENDIF
			IF(INDORDER(i1).EQ.1.AND.i2.GT.1)THEN
				IF(IPOLPR(i1).EQ.1)inwp(i1)=inwa(i1,i2)
			ENDIF
			IF(INDORDER(i1).EQ.3.AND.i2.EQ.1)THEN
				inwp(i1)=inwa(i1,i2)
			ENDIF
			IF(INDORDER(i1).EQ.3.AND.i2.GT.1)THEN
				inws(i1)=inwa(i1,i2)-1
				inwt(i1)=1
			ENDIF
cs        ENDIF
cs        IF (INDPL.EQ.1)THEN
cs        IF(INDORDER(i1,i2).EQ.1)THEN
cs        inws(i1)=inwa(i1,i2)-1
cs        inwt(i1)=1
cs        ENDIF
cs        IF(INDORDER(i1,i2).EQ.2)THEN
cs        inwp(i1)=inwa(i1,i2)
cs        ENDIF
cs        ENDIF
cs        ELSE
cs        IF(INDORDER(1).EQ.2.AND.INDSAT(i1).EQ.0)THEN
cs        inws(i1)=inwa(i1,i2)-1
cs        inwt(i1)=1
cs        ENDIF
cs        IF(INDORDER(1).EQ.2.AND.INDSAT(i1).EQ.1)THEN
cs        inwp(i1)=inwa(i1,i2)
cs        ENDIF
cs        IF(INDORDER(1).EQ.1.AND.INDSK(i1).EQ.0)THEN
cs        inwp(i1)=inwa(i1,i2)
cs        ENDIF
cs        IF(INDORDER(1).EQ.1.AND.INDSK(i1).EQ.1)THEN
cs        inws(i1)=inwa(i1,i2)-1
cs        inwt(i1)=1
cs        ENDIF
cs        ENDIF
		enddo
      enddo
c        write(*,*)'inwt',inwt(1:4)
c        write(*,*)'inws',inws(1:4)
c        write(*,*)'inwp',inwp(1:4)
C******Wavelength*and*angular*dependent*bias*for*sky*radiances*****************************
      IF(INOISE.GT.1)THEN
		ik1=1
		ik2=inws(1)
		do i1=1,NW
			IF(INDSK(i1).GT.0)THEN
				iks=1
				IF(i1.GT.1)THEN
					ik2=ik2+inws(i1)
				ENDIF
				do i2=ik1,ik2
					do i3=1,KM
						IF(KNOISE(1,i2).EQ.i3)THEN
							FPT(iks)=F(i3)
							FP1T(iks)=FP1(i3)
							iks=iks+1
						ENDIF
					enddo
				enddo
				in=inws(i1)
				do ib=1,in
					BIAS(i1)=BIAS(i1)+(FPT(ib)-FP1T(ib))
					BIASGS(i1,ib)=FPT(ib)-FP1T(ib)
				enddo
C******************************************correction for Ilya************
				xin=in
				IF(in.lt.1.)then 
					xin=1e-20
				endif
C*************************************************************************
				BIASS(i1)=(BIAS(i1)/xin)
				BIAS(i1)=(BIAS(i1)/xin)*100.
				CALL RES (in,FPT,FP1T,C1,ALP)
				ALS(i1)=SQRT(ALP/xin)*100.
				ik1=ik1+inws(i1)
				do ip=1,KM
					FPT(ip)=0.
					FP1T(ip)=0.
				enddo
			ENDIF
		enddo
		write(1011,*)''
		write(1011,*)'Wavelength dependence of               sky error'//
     +	'          and             bias' 
		write(1011,*)''
		do i1=1,NW
			IF(INDSK(i1).GT.0)write(1011,71)WAVE(i1),ALS(i1),'%',BIAS(i1),'%'
		enddo
      ENDIF
C******Wavelength*and*angular*dependent*bias*for*polarization**********
      IF(INOISE.GT.1)THEN
		do ip=1,NW
			BIAS(ip)=0.
			BIASP(ip)=0.
		enddo       
		ik1=1
		ik2=inwp(1)
		do i1=1,NW
			IF(IPOLPR(i1).EQ.1)THEN
				iks=1
				IF(i1.GT.1)THEN
					ik2=ik2+inwp(i1)
				ENDIF
				do i2=ik1,ik2
					do i3=1,KM
						IF(KNOISE(3,i2).EQ.i3)THEN
							FPT(iks)=EXP(F(i3))
							FP1T(iks)=EXP(FP1(i3))
							iks=iks+1
						ENDIF
					enddo
				enddo
cs      write(6,*)'FPT',FPT,EXP(FPT)
cs      write(6,*)'FP1T',FP1T,EXP(FP1T)
				in=inwp(i1)
cs      write(6,*)'inwp',inwp
				do ib=1,in
					IF(FPT(IB).LT.1.0E-30)FPT(IB)=1.0E-30
					IF(FP1T(IB).LT.1.0E-30)FP1T(IB)=1.0E-30
					BIAS(i1)=BIAS(i1)+(FPT(ib)-FP1T(ib))
					BIASP(i1)=BIASP(i1)+(LOG(FPT(ib))-LOG(FP1T(ib)))
					BIASGP(i1,ib)=LOG(FPT(ib))-LOG(FP1T(ib))
				enddo
C******************************************correction for Ilya************
				xin=in
				IF(in.lt.1.)then 
					xin=1e-20
				endif
C*************************************************************************
				BIASP(i1)=BIASP(i1)/xin
				BIAS(i1)=BIAS(i1)/xin
				CALL RES (in,FPT,FP1T,C1,ALP)
				ALS(i1)=SQRT(ALP/xin)
				ik1=ik1+inwp(i1)
				do ip=1,KM
					FPT(ip)=0.
					FP1T(ip)=0.
				enddo
			ENDIF
		enddo
		IF(INDPL.EQ.1)THEN
			write(*,*)''
			write(*,*)'Wavelength dependence of               pol error'//
     +		'          and             bias' 
			write(*,*)''
			do i1=1,NW
				IF(IPOLPR(i1).EQ.1)THEN
					write(*,76)WAVE(i1),ALS(i1),BIAS(i1)
				ENDIF
			enddo
		ENDIF
      ENDIF
C******Wavelength*and*angular*dependent*bias*for*sat*meas**********
      do i1=1,NW
		IF(INDSAT(i1).EQ.1)INDST=1
      enddo
      do ip=1,NW
		BIAS(ip)=0.
		BIASP(ip)=0.
      enddo       
      ik1=1
      ik2=inwp(1)
      do i1=1,NW
		IF(INDSAT(i1).EQ.1)THEN
			iks=1
			IF(i1.GT.1)THEN
				ik2=ik2+inwp(i1)
			ENDIF
			do i2=ik1,ik2
				do i3=1,KM
C***********************************************
					IF(INOISE.GT.1)THEN
						IF(KNOISE(3,i2).EQ.i3)THEN
							FPT(iks)=F(i3)
							FP1T(iks)=FP1(i3)
							iks=iks+1
						ENDIF
					ELSE
						IF(i2.EQ.i3)THEN
							FPT(iks)=F(i3)
							FP1T(iks)=FP1(i3)
							iks=iks+1
						ENDIF
					ENDIF
C*********************************************
				enddo
			enddo
cs      write(6,*)'FPT',FPT-FP1T
cs      write(6,*)'FP1T',FP1T,EXP(FP1T)
			in=inwp(i1)
cs      write(6,*)'inwp',inwp
			xa=0.
			xaa=0.
			do ib=1,in
				xa=xa+(FPT(ib)-FP1T(ib))
				xaa=xaa+(FPT(ib)-FP1T(ib))
				BIASGP(i1,ib)=FPT(ib)-FP1T(ib)
			enddo
C******************************************correction for Ilya************
			xin=in
			IF(in.lt.1.)then 
				xin=1e-20
			endif
C*************************************************************************
			BIASP(i1)=xaa/xin
			BIAS(i1)=(xa/xin)*100.
			CALL RES (in,FPT,FP1T,C1,XALP)
			ALS(i1)=SQRT(XALP/xin)*100.
			ik1=ik1+inwp(i1)
			do ip=1,KM
				FPT(ip)=0.
				FP1T(ip)=0.
			enddo
		ENDIF
      enddo

      IF(INDST.EQ.1)THEN
		write(*,*)''
		write(*,*)'Wavelength dependence of               aer/sat error'//
     $	'          and             bias' 
		write(*,*)''
		do i1=1,NW
			IF(INDSAT(i1).EQ.1)THEN
				write(*,71)WAVE(i1),ALS(i1),'%',BIAS(i1),'%'
			ENDIF
		enddo
      ENDIF
C******Calculation*of*the*error*due*to*bias*****************************
      IF(INOISE.GT.1)THEN
		ik1=1
		ik2=inwp(1) 
		do i1=1,NW
			IF(IPOLPR(i1).EQ.1.OR.NBRDF.GT.0)THEN
				IF(i1.GT.1)THEN
					ik2=ik2+inwp(i1)
				ENDIF
				i7=1
				do i2=ik1,ik2
					do i3=1,KM
						IF(KNOISE(3,i2).EQ.i3)THEN
							BIASG(i3)=BIASGP(i1,i7)
							BIASGG(i3)=BIASP(i1)
							BIASG1(i3)=BIASGP(i1,i7)
						ENDIF
					enddo
					i7=i7+1
				enddo
				ik1=ik1+inwp(i1)
			ENDIF
		enddo
		ik1=1
		ik2=inws(1) 
		do i1=1,NW
			IF(i1.GT.1)THEN
				ik2=ik2+inws(i1)
			ENDIF
			i7=1
			do i2=ik1,ik2
				do i3=1,KM
					IF(KNOISE(1,i2).EQ.i3)THEN
						BIASG(i3)=BIASGS(i1,i7)
						BIASGG(i3)=BIASS(i1)
						BIASG1(i3)=BIASGS(i1,i7)
cs****************************LAST***CORRECTION*************
						BIASGH(i3)=BIASG(i3)+0.05
						BIASGL(i3)=BIASG(i3)-0.05
						BIASG1H(i3)=BIASG1(i3)+0.05
						BIASG1L(i3)=BIASG1(i3)-0.05
					ENDIF
				enddo
				i7=i7+1
			enddo
			ik1=ik1+inws(i1)
		enddo
cs      do i1=1,KM
cs      BIASG(i1)=0.
cs      BIASG1(i1)=0.
cs      enddo
		CALL RES (KM,BIASGG,FW,C,ALPB)
cs      write(6,*)'BIASG,BIASG1,BIASGG'
cs      do i1=1,KM
cs      write(6,*)BIASG(i1),BIASG1(i1)
cs      enddo
cs       write(6,*)'ALPB',ALPB
		IF(ALPB.GT.ALPW)THEN
cs       write(6,*)'ALPB.GT.ALPW'
			ALPB=0.
		ENDIF
cs       ALPW=ALPW-ALPB
		IF(KM.LE.KN)ALPW=ALPW/(KM-KN+IKS+IKS1)
cs        ALPW=ALPW/(KM-KN)
cs****************LAST***CORRECTION********************************
		ALPW=ALPW/(KM-KN+IKS+IKS1)
cs****************************LAST***LAST***CHANGE************************
		ALLL=SQRT(ALPW)
      ENDIF
C**************Correction*******************************************
      IF(INOISE.EQ.1)THEN
		ik1=1
		ik2=inwp(1) 
		do i1=1,NW
			IF (INDSAT(i1).EQ.0)THEN
				IF(i1.GT.1)THEN
					ik2=ik2+inwp(i1)
				ENDIF
				i7=1
				do i2=ik1,ik2
					do i3=1,KM
						IF(i2.EQ.i3)THEN
							BIASG(i3)=BIASGS(i1,i7)
							BIASGG(i3)=BIASS(i1)
							BIASG1(i3)=BIASGS(i1,i7)
							BIASGH(i3)=BIASG(i3)+0.05
							BIASGL(i3)=BIASG(i3)-0.05
							BIASG1H(i3)=BIASG1(i3)+0.05
							BIASG1L(i3)=BIASG1(i3)-0.05
						ENDIF
					enddo
					i7=i7+1
				enddo
				ik1=ik1+inwp(i1)
			ENDIF
		enddo
		do i1=1,NW
			IF (INDSAT(i1).EQ.1)THEN
				IF(i1.GT.1)THEN
					ik2=ik2+inwp(i1)
				ENDIF
				i7=1
				do i2=ik1,ik2
					do i3=1,KM
						IF(i2.EQ.i3)THEN
							BIASG(i3)=BIASGP(i1,i7)
							BIASGG(i3)=BIASP(i1)
							BIASG1(i3)=BIASGP(i1,i7)
							BIASGH(i3)=BIASG(i3)+0.0
							BIASGL(i3)=BIASG(i3)-0.0
							BIASG1H(i3)=BIASG1(i3)+0.0
							BIASG1L(i3)=BIASG1(i3)-0.0
						ENDIF
					enddo
					i7=i7+1
				enddo
				ik1=ik1+inwp(i1)
			ENDIF
		enddo
cs      do i1=1,KM
cs      BIASG(i1)=0.
cs      BIASG1(i1)=0.
cs      enddo
		CALL RES (KM,BIASGG,FW,C,ALPB)
cs      write(6,*)'BIASG,BIASG1,BIASGG'
cs      do i1=1,KM
cs      write(6,*)BIASG(i1),BIASG1(i1)
cs     enddo
cs       write(6,*)'ALPB',ALPB
		IF(ALPB.GT.ALPW)THEN
cs       write(6,*)'ALPB.GT.ALPW'
			ALPB=0.
		ENDIF
cs       ALPW=ALPW-ALPB
		ALPW=ALPW/(KM-KN+IKS+IKS1)
cs        ALPW=ALPW/(KM-KN)
cs       write(6,*)'ALPW',SQRT(ALPW)
cs       write(6,*)'BIASGH',BIASGH
cs       write(6,*)'BIASGL',BIASGL
cs       write(6,*)'BIASG1H',BIASG1H
cs       write(6,*)'BIASG1L',BIASG1L
		ALLL=SQRT(ALPW)
      ENDIF
C***************Correction*****************************************
cs*********************ib********************************************
cs************LAST*********************CORRECTION*********************
      do ib=1,4
		IF(ib.EQ.1)THEN
			do ibb=1,KM
				BIASG(ibb)=BIASGL(ibb)
cs       write(6,*)'BIASG',BIASG(ibb)
			enddo
		ENDIF
		IF(ib.EQ.2)THEN
			do ibb=1,KM
				BIASG(ibb)=BIASGH(ibb)
cs       write(6,*)'BIASG',BIASG(ibb)
			enddo
		ENDIF
		IF(ib.EQ.3)THEN
			do ibb=1,KM
				BIASG(ibb)=BIASG1L(ibb)
cs       write(6,*)'BIASG',BIASG(ibb)
			enddo
		ENDIF
		IF(ib.EQ.4)THEN
			do ibb=1,KM
				BIASG(ibb)=BIASG1H(ibb)
cs       write(6,*)'BIASG',BIASG(ibb)
			enddo
		ENDIF
cs*********LAST****CORRECTION**************************************
		DO IL=1,NLYR
			DO ISD1=1,NSD
				AP0(ISTAR(ISDLOC(IL,ISD1)))=5.e0
				AP0(ISTAR(ISDLOC(IL,ISD1))+
     &			NPAR(ISDLOC(IL,ISD1))-1)=5.e0
			ENDDO
		ENDDO
cs*******************************************************************
		CALL FISHMX(KM,KN,U,C,FW,BIASG,UF,FF,GF,AL0)
cs*********LAST***CORRECTION****************************************
		CALL VEC_MATR (KN,KN,SM0,AP0,FFS0)
		CALL VEC_MATR (KN,KN,SM,AP,FFS)
cs******************************************************************
		DO I=1, KN
			DO J=1,KN
				UF(I,J)=UF(I,J)+SM(I,J)+SM0(I,J)
			ENDDO
			FF(I)=FF(I)+FFS(I)+FFS0(I)
cs       write(6,*)'FF',I,FF(I),FFS(I),FFS0(I)
		ENDDO
cs       do I=1,KN
cs       write(6,*)(UF(I,J),J=1,KN)
cs       write(6,*)
cs       enddo
		OPEN(1111,FILE="FISHmx.dat")
		do I=1,KN
			write(1111,115)(UF(I,J),J=1,KN)
		enddo
		close(1111)
		CALL ITERQ(3,KN,KI,UF,FF,GF,EPSQ,Q0,Q,APQ,NQ,UFI,KN1)
		do i1=1,KN
			do i2=1,KN
cs*********LAST****************CORRECTION*******************
				IF(ib.EQ.1)ABIAS1(i1,i2)=Q(i1)*Q(i2)
				IF(ib.EQ.2)ABIAS2(i1,i2)=Q(i1)*Q(i2)
				IF(ib.EQ.3)ABIAS3(i1,i2)=Q(i1)*Q(i2)
				IF(ib.EQ.4)ABIAS4(i1,i2)=Q(i1)*Q(i2)
			enddo
		enddo
		CALL ITERQ(4,KN,KI,UF,FF,GF,EPSQ,Q0,Q,APQ,NQ,UFI,KN1)
cs       write(6,*)'check'
		DO I=1,KN
			DO J=1,KN
				UF(I,J)=UFI(I,J)
			ENDDO
		ENDDO
cs       write(6,*)'check'
		DO I=1,KN
			IF(UF(I,I).LT.0)UF(I,I)=ABS(UF(I,I))
		ENDDO
cs       DO I=1,KN
cs       write(6,*)(UFI(I,J),J=1,KN)
cs       ENDDO
cs       write(6,*)'ABIAS'
cs       do i1=1,KN
cs       write(6,*)ABIAS1(i1,i1),ABIAS2(i1,i1)
cs       enddo
cs       CALL TINVCH3(KN,UF,DT,KN,ERR)
		OPEN(1115,FILE="Umx.dat")
		do I=1,KM
			write(1115,115)(U(I,J),J=1,KN)
		enddo
		close(1115)
		OPEN(1111,FILE="FISHmx_i.dat")
		do I=1,KN
			write(1111,115)(UF(I,J),J=1,KN)
		enddo
		close(1111)
cs      write(6,*)'CHECK'
cs        write(6,*)'DT',DT
cs       write(6,*)(UF(I,I),I=1,KN)
cs       do i1=1,KN
cs       do i2=1,KN
cs       A=0.
cs       do i3=1,KN
cs       A=A+UF(i1,i3)*UFF(i3,i2)
cs       enddo
cs       UFFF(i1,i2)=A
cs       enddo
cs       enddo
cs       write(6,*)'UFFF'
cs       do i1=1,KN
cs       do i2=1,KN
cs       IF(i1.EQ.i2)write(6,*)i1,i2,UFFF(i1,i2)
cs       enddo
cs       enddo
		do i1=1,KN
			do i2=1,KN
				UF(i1,i2)=UF(i1,i2)*ALPW
			enddo
		enddo
cs       write(6,*)'UF'
cs       do i1=1,KN
cs       write(6,*)UF(i1,i1)
cs       enddo
		do i1=1,KN
			IF(ib.EQ.1)THEN
				ACOV1(i1)=UF(i1,i1)+ABIAS1(i1,i1)
				ACOVR(i1)=SQRT(UF(i1,i1))
				ACOV1(i1)=SQRT(ACOV1(i1))
cs      IF(i1.GE.31)then
cs      write(6,*)' UF,ABIAS1',UF(i1,i1),ABIAS1(i1,i1)
cs      endif
			ENDIF
			IF(ib.EQ.2)THEN
				ACOV2(i1)=UF(i1,i1)+ABIAS2(i1,i1)
				ACOV2(i1)=SQRT(ACOV2(i1))
cs      IF(i1.GE.31)then
cs      write(6,*)' UF,ABIAS1',UF(i1,i1),ABIAS2(i1,i1)
cs      endif
			ENDIF
		enddo
cs       write(6,*)(ACOVR(i1),i1=1,KN)
cs       write(6,*)
cs       do i1=1,KN
cs       write(6,*)UFFF(i1,i1)
cs       enddo
cs       write(6,*)
cs      write(6,*)'BIASG',BIASG
      enddo
C************************ib*******************************************
      do i1=1,KN
cs************LAST***CORRECTION*******************************************
		ABIASC(i1)=SQRT((ABIAS1(i1,i1)+ABIAS2(i1,i1)+
     &	ABIAS3(i1,i1)+ABIAS4(i1,i1))/4.)
cs      ACOV(i1)=SQRT((ACOV1(i1)*ACOV(i1)+ACOV2(i1)*ACOV2(i1))/2.)
cs        ABIASC(i1)=(SQRT(ABIAS1(i1,i1))+SQRT(ABIAS2(i1,i1)))/2.
		ACOV(i1)=SQRT(ABIASC(i1)**2+ACOVR(i1)**2)
cs      ABIASC(i1)=SQRT(ABIAS1(i1,i1))
cs      ACOV(i1)=ACOV1(i1)
      enddo
      do i1=1,KN
		do i2=1,KN
cs****************LAST***CORRECTION********************************
			ABIAS(i1,i2)=(ABIAS1(i1,i2)+ABIAS2(i1,i2)+
     &		ABIAS3(i1,i2)+ABIAS4(i1,i2))/4.
cs      ABIAS(i1,i2)=ABIAS1(i1,i2)
		enddo
      enddo
cs*********************CHECK!!!!********************
cs       do i1=1,KN
cs       do i2=1,KN
cs       UF(i1,i2)=UF(i1,i2)+ABIAS(i1,i2)
cs       enddo
cs       enddo
C***************************************************
cs      write(6,*)'ACOV',ACOV
cs      do ib=1,2
      do i1=1,KN
		APMIN(i1)=AP(i1)-ACOV(i1)
		APMAX(i1)=AP(i1)+ACOV(i1)
		APMINS(i1)=AP(i1)-ACOV(i1)
		APMAXS(i1)=AP(i1)+ACOV(i1)
cs      IF(i1.GE.31)then
cs      write(6,*)'AP,ACOV',AP(i1),ACOV(i1)
cs      endif
      enddo
C**********************************************************
      IF(NBRDF.GT.0)THEN
		do i1=1,NBRDF  
			do i3=1,NW
				XBMIN=EXP(APMIN(ISTAR(IBRDFLOC(i1))+i3-1))
				XBMAX=EXP(APMAX(ISTAR(IBRDFLOC(i1))+i3-1))
				IF(XBMIN.LT.BRDFMIN(i1))THEN
					IF(BRDFMIN(I1).LT.1.0E-30)BRDFMIN(I1)=1.0E-30
     					APMIN(ISTAR(IBRDFLOC(i1))+i3-1)=LOG(BRDFMIN(i1))
				ENDIF
				IF(XBMAX.GT.BRDFMAX(i1))THEN
					IF(BRDFMAX(I1).LT.1.0E-30)BRDFMAX(I1)=1.0E-30
     					APMAX(ISTAR(IBRDFLOC(i1))+i3-1)=LOG(BRDFMAX(i1))
				ENDIF
			enddo
		enddo
      ENDIF
C**********************************************************
cs      write(6,*)'APMAX',exp(APMAX)
cs      write(6,*)'APMIN',exp(APMIN)
      do i1=2*NW+1,KN
		APMINS(i1)=AP(i1)
		APMAXS(i1)=AP(i1)
      enddo
      do i1=1,NW
		APMINS(i1)=AP(i1)
		APMAXS(i1)=AP(i1)
      enddo
       CALL forwMN(0,KM,KN,IT,KL,1,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     &,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     &,IBRDF1LOC,ISHAPELOC,ISTAR,
     & APMINS,SSAMIN,EXT,PF,FP1,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
     &ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
       CALL forwMN(0,KM,KN,IT,KL,1,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     &,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     &,IBRDF1LOC,ISHAPELOC,ISTAR,
     & APMAXS,SSAMAX,EXT,PF,FP1,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
     &ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
C*************SSA*error*calculation*through*linear*combination*of*par.****
C**********Calculation*of*the*der*of*SSA*and*phase*function***************
        CALL forwMN(0,KM,KN,IT,KL,1,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     & ,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     & ,IBRDF1LOC,ISHAPELOC,ISTAR
     & ,AP,SSA,EXT,PF,FP1,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
     &ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
       CALL FMAT (KM,KN,IT,KL,1,NW,WAVE,NSD
     &,NBIN,RADIUS,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ISDLOC,IREFLOC
cs******************************************FP changed to FP1*********************
     &,IBRDFLOC,IBRDF1LOC,ISHAPELOC,ISTAR,AP,FP1,IMTX,DL,LP 
     &,REALMIN,REALMAX,AIMAGMIN,AIMAGMAX
     &,BRDFMIN,BRDFMAX,BRDF1MIN,BRDF1MAX,SHAPEMIN,SHAPEMAX
     &,ALLL,AREF,AREF1,U,IEL,IS,1,US,UP,SSA,PF,UPOL,DLP,0,0,1)
C***************************************************************************
C***********************************SSA**************************************
      do IL=1,NLYR
		do ISD=1,NSD
			do IW=1,NW
				do I=1,KN
					BP(I)=US(IW,IL,ISD,I)
				enddo
				xs=0.
				xs1=0.
				do i2=1,KN
					do i3=1,KN
						xs=xs+UF(i2,i3)*BP(i3)
						xs1=xs1+ABIAS(i2,i3)*BP(i3)
cs        write(6,*)'UF,BP',i2,i3,UF(i2,i3),BP(i3)
			        enddo
					BP1(i2)=xs
					BP2(i2)=xs1
					xs=0.
					xs1=0.
				enddo
				xs=0.
				xs1=0.
				do i1=1,KN
					xs=xs+BP(i1)*BP1(i1)
					xs1=xs1+BP(i1)*BP2(i1)
				enddo
				CS(IW,IL,ISD)=SQRT(xs)
				CSB1(IW,IL,ISD)=SQRT(xs1)
cs      write(6,*)'CS,CSB1',CS(IW,IL,ISD),CSB1(IW,IL,ISD)
			enddo
		enddo
      enddo
cs       do IW=1,NW
cs       write(6,*)'WAVE=',WAVE(IW)
cs       do I=1,KN
cs       write(6,*)I,US(IW,1,1,I)
cs       enddo
cs       enddo
cs       do IW=1,NW
cs       write(6,*)'WAVE=',WAVE(IW)
cs       do I=1,KN
cs       do IA=1,IANGL
cs       write(6,*)I,ANG(IA),UP(IA,IW,1,1,I)
cs       enddo
cs       write(6,*)
cs       enddo
cs       enddo
C*********************************************************************
C*************************Phase*function******************************
      do IL=1,NLYR
		do ISD=1,NSD
			do IW=1,NW
				do IA=1,IANGL
					do I=1,KN
						BF(I)=UP(IA,IW,IL,ISD,I)
					enddo
					xs=0.
					xs1=0.
					do i2=1,KN
						do i3=1,KN
							xs=xs+UF(i2,i3)*BF(i3)
							xs1=xs1+ABIAS(i2,i3)*BF(i3)
						enddo
						BP1(i2)=xs
						BP2(i2)=xs1
						xs=0.


				        xs1=0.
					enddo
					xs=0.
					xs1=0.
					do i1=1,KN
						xs=xs+BF(i1)*BP1(i1)
						xs1=xs1+BF(i1)*BP2(i1)
					enddo
					CP(IA,IW,IL,ISD)=SQRT(xs)
					CPB1(IA,IW,IL,ISD)=SQRT(xs1)
				enddo
			enddo
		enddo
      enddo
C*************************************************************************
C***********************Degree*of*linear*polarization*********************
      do i1=1,NW
		IF(IPOLPR(i1).EQ.1)INDPL=1
      enddo
      IF(INDPL.EQ.1)THEN
		do IL=1,NLYR
			do ISD=1,NSD
				do IW=1,NW
					IF(IPOLPR(IW).EQ.1)THEN
						do IA=1,IANGL
							do I=1,KN
								BF(I)=UPOL(IA,IW,IL,ISD,I)
							enddo
							xs=0.
							xs1=0.
							do i2=1,KN
								do i3=1,KN
									xs=xs+UF(i2,i3)*BF(i3)
									xs1=xs1+ABIAS(i2,i3)*BF(i3)
								enddo
								BP1(i2)=xs
								BP2(i2)=xs1
								xs=0.
								xs1=0.
							enddo
							xs=0.
							xs1=0.
							do i1=1,KN
								xs=xs+BF(i1)*BP1(i1)
								xs1=xs1+BF(i1)*BP2(i1)
							enddo
							CPP(IA,IW,IL,ISD)=SQRT(xs)
							CPBP1(IA,IW,IL,ISD)=SQRT(xs1)
						enddo
					ENDIF
				enddo
			enddo
		enddo
      ENDIF
C*************************************************************************
C*********************************Measurements****************************
      do IM=1,KM
		do I=1,KN
			BP(I)=U(IM,I)
		enddo
		xs=0.
		xs1=0.
		do i2=1,KN
			do i3=1,KN
				xs=xs+UF(i2,i3)*BP(i3)
				xs1=xs1+ABIAS(i2,i3)*BP(i3)
			enddo
			BP1(i2)=xs
			BP2(i2)=xs1
			xs=0.
			xs1=0.
		enddo
		xs=0.
		xs1=0.
		do i1=1,KN
			xs=xs+BP(i1)*BP1(i1)
			xs1=xs1+BP(i1)*BP2(i1)
          enddo
cxxa		CSM(IM)=SQRT(xs)
cxxa		CPBM1(IM)=SQRT(xs1)
      enddo
C**************************************************************************
cs      enddo
cs************ib**********************************************************
cs      do i1=1,NSD
cs      do i2=1,NLYR
cs      do i3=1,NW
cs      write(6,*)SSAMIN(i3,i2,i1),SSAMIN1(i3,i2,i1),SSA(i3,i2,i1),
cs     &SSAMAX(i3,i2,i1),SSAMAX1(i3,i2,i1)
cs      enddo
cs      enddo
cs      enddo
C*********************SSA*errors*calculations******************************
cs      write(6,*)'SSA'
      do i1=1,NSD
		do i2=1,NLYR
			do i3=1,NW
cs      write(6,*)SSAMIN(i3,i2,i1),SSA(i3,i2,i1),SSAMAX(i3,i2,i1)
			enddo
		enddo
      enddo
C*************************************************************************
C*******************SSA*error*calc*though*linear*comb*of*par**************
cs      write(6,*)'SSA L C'
cs      do ISD=1,NSD
cs      do IL=1,NLYR
cs      do IW=1,NW
cs      CSB1(IW,IL,ISD)=SQRT(CSB1(IW,IL,ISD)*CSB1(IW,IL,ISD)
cs     &+CS(IW,IL,ISD)*CS(IW,IL,ISD))
cs      write(6,*)EXP(LOG(SSA(IW,IL,ISD))-CSB1(IW,IL,ISD)),SSA(IW,IL,ISD)
cs     &,EXP(LOG(SSA(IW,IL,ISD))+CSB1(IW,IL,ISD))
cs      enddo
cs      enddo
cs      enddo
C*************************************************************************
C*********************Phase*Function*error*calculation********************
cs       write(6,*)'Phase Function'
cs       do IL=1,NLYR
cs       do ISD=1,NSD
cs       do IW=1,NW
cs       do IA=1,IANGL
cs       write(6,*)PF(IA,IW,IL,ISD),CP(IA,IW,IL,ISD),
cs     &CPB1(IA,IW,IL,ISD)
cs       enddo
cs       write(6,*)''
cs       enddo
cs       enddo
cs       enddo
C*************************************************************************
C*************************DLP*error*calculation***************************
      do i1=1,NW
		 IF(IPOLPR(i1).EQ.1)INDPL=1
	enddo
      IF(INDPL.EQ.1)THEN
cs       write(6,*)'DPL'
		do IL=1,NLYR
			do ISD=1,NSD
				do IW=1,NW
					IF(IPOLPR(IW).EQ.1)THEN
						do IA=1,IANGL
cs       write(6,*)-1*DLP(IA,IW,IL,ISD),CPP(IA,IW,IL,ISD),
cs     &CPBP1(IA,IW,IL,ISD)
				       enddo
cs       write(6,*)''
					ENDIF
				enddo
			enddo
		enddo
      ENDIF
C**************************************************************************
C************************Measurememts*error*calculation********************
cs       write(6,*)'Measurements'
      do IM=1,KM
cs       write(6,*)EXP(F(IM)),CSM(IM),
cs     &CPBM1(IM)
      enddo
C**************************************************************************
      write(1011,*)''
      write(1011,*)'Fluxes:                                  Downward'//   
     &'                        Upward                             Diffuse
     &'
      write(1011,*)''
      do i1=1,NW
		write(1011,72)WAVE(i1),FLDD(i1),FLUP(i1),FLDR(i1) 
      ENDDO
C*************************************************************************
      write(1011,*)''
      write(1011,*)'Refractive index:              real                      
     &                                            imaginary'
      do i1=1,NLYR
		write(1011,*)i1,'-th layer'
		do i2=1,NSD
			write(1011,*)i2,'-th component'
			write(1011,999)'r','min','max','er','eb','erb','r','min','max',
     &		'er','eb','erb'
cs*****************LAST***********************CORRECTION*************************
			do i3=1,NW
				xr=EXP(AP(ISTAR(IREFLOC(i1,i2,1))+i3-1))
				xrmin=EXP(APMIN(ISTAR(IREFLOC(i1,i2,1))+i3-1))
				xrmax=EXP(APMAX(ISTAR(IREFLOC(i1,i2,1))+i3-1))
				xerr=ACOVR(ISTAR(IREFLOC(i1,i2,1))+i3-1)
				xerb=ABIASC(ISTAR(IREFLOC(i1,i2,1))+i3-1)
				xerrb=ACOV(ISTAR(IREFLOC(i1,i2,1))+i3-1)
cs      if(xrmax.gt.1.6)then
cs      xrmax=1.6
cs      xer=LOG(xrmax)-LOG(xr)
cs      xrmin=EXP(LOG(xr)-xer)
cs      xratr=xer/xerrb
cs      xerr=xerr*xratr
cs      xerb=xerb*xratr
cs      xerrb=xer
cs      endif
				xi=EXP(AP(ISTAR(IREFLOC(i1,i2,2))+i3-1))
				ximin=EXP(APMIN(ISTAR(IREFLOC(i1,i2,2))+i3-1))
				ximax=EXP(APMAX(ISTAR(IREFLOC(i1,i2,2))+i3-1))
				xirr=ACOVR(ISTAR(IREFLOC(i1,i2,2))+i3-1)
				xirb=ABIASC(ISTAR(IREFLOC(i1,i2,2))+i3-1)
				xirrb=ACOV(ISTAR(IREFLOC(i1,i2,2))+i3-1)
				if(ximax.gt.0.5)then 
					ximax=0.5
					IF(XI.LT.1.0E-30)XI=1.0E-30
					xir=LOG(ximax)-LOG(xi)
					ximin=EXP(LOG(xi)-xir)
					xratr=xir/xirrb
					xirr=xirr*xratr
					xirb=xirb*xratr
					xirrb=xir
				endif
				write(1011,73)WAVE(i3),xr,
     &			xrmin,
     &			xrmax,
     &			xerr,
     &			xerb,
     &			xerrb,
     &			xi,
     &			ximin,
     &			ximax,
     &			xirr,
     &			xirb,
     &			xirrb
			enddo
		enddo
      enddo
cs*************************LAST*******************************CORRECTION*******
***********************************************************************************
      IF(NBRDF.GT.0)THEN
		write(*,*)''
		write(*,*)'BRDF parameters:'
		do i1=1,NBRDF
			write(*,995)'r','min','max','er','eb','erb'
			do i3=1,NW
				IF(i1.LT.3)THEN
					write(*,89)WAVE(i3),EXP(AP(ISTAR(IBRDFLOC(i1))+i3-1)),
     &				EXP(APMIN(ISTAR(IBRDFLOC(i1))+i3-1)),
     &				EXP(APMAX(ISTAR(IBRDFLOC(i1))+i3-1)),
     &				ACOVR(ISTAR(IBRDFLOC(i1))+i3-1),
     &				ABIASC(ISTAR(IBRDFLOC(i1))+i3-1),
     &				ACOV(ISTAR(IBRDFLOC(i1))+i3-1)
				ENDIF
				IF(i1.EQ.3)THEN
					write(*,89)WAVE(i3),EXP(AP(ISTAR(IBRDFLOC(i1))+i3-1))-1.,
     &				EXP(APMIN(ISTAR(IBRDFLOC(i1))+i3-1))-1.,
     &				EXP(APMAX(ISTAR(IBRDFLOC(i1))+i3-1))-1.,
     &				ACOVR(ISTAR(IBRDFLOC(i1))+i3-1),
     &				ABIASC(ISTAR(IBRDFLOC(i1))+i3-1),
     &				ACOV(ISTAR(IBRDFLOC(i1))+i3-1)
				ENDIF
			enddo
		enddo
      ENDIF
C*************************************************************************
      write(1011,*)''
      write(1011,*)'Particle size distribution dV/dlnR'
      write(1011,*)'Radius(micron)             psd            min'//       
     &'         max               er                eb              erb'
      DO IL=1,NLYR
		write(1011,*)IL,'-th layer'
		DO ISD=1,NSD
			write(1011,*)ISD,'-th component'
			DO I=1,NBIN(ISD)
				WRITE(1011,74) 
     &			RADIUS(ISD,I), EXP(AP(ISTAR(ISDLOC(IL,ISD))+I-1)),
     &			EXP(APMIN(ISTAR(ISDLOC(IL,ISD))+I-1)),
     &			EXP(APMAX(ISTAR(ISDLOC(IL,ISD))+I-1)),
     &			ACOVR(ISTAR(ISDLOC(IL,ISD))+I-1),
     &			ABIASC(ISTAR(ISDLOC(IL,ISD))+I-1),
     &			ACOV(ISTAR(ISDLOC(IL,ISD))+I-1)
			ENDDO
		ENDDO
      ENDDO
C****************************************************************************
      write(1011,*)
      write(1011,*)'Single Scattering Albedo'
      do IL=1,NLYR
      write(1011,*)IL,'-th layer'
      do ISD=1,NSD
      write(1011,*)ISD,'-th component'
      write(1011,*)
      write(1011,998)'r','min','max','er','eb','erb'
cs**************************LAST****************************CORRECTION*************
      do IW=1,NW
	IF(SSA(IW,IL,ISD).LT.1.0E-30)SSA(IW,IL,ISD)=1.0E-30
      CSB2(IW,IL,ISD)=SQRT(CSB1(IW,IL,ISD)*CSB1(IW,IL,ISD)
     &+CS(IW,IL,ISD)*CS(IW,IL,ISD))
      xc=EXP(LOG(SSA(IW,IL,ISD))+CSB2(IW,IL,ISD))
      xcm=EXP(LOG(SSA(IW,IL,ISD))-CSB2(IW,IL,ISD))
cs      IF(xc.GT.1.)THEN
cs      xc=1.
cs      xerb=LOG(xc)-LOG(SSA(IW,IL,ISD))
cs      xcm=EXP(LOG(SSA(IW,IL,ISD))-xerb)
cs      xratr=xerb/CSB2(IW,IL,ISD)
cs      CS(IW,IL,ISD)=CS(IW,IL,ISD)*xratr
cs      CSB1(IW,IL,ISD)=CSB1(IW,IL,ISD)*xratr
cs      CSB2(IW,IL,ISD)=xerb
cs      ENDIF
      write(1011,84)Wave(IW),SSA(IW,IL,ISD),
     &xcm,
     &xc,CS(IW,IL,ISD),
     &CSB1(IW,IL,ISD),CSB2(IW,IL,ISD)
      enddo
      enddo
      enddo
cs*********************************LAST*****************CORRECTION*****************
C***************************************************************************
      IF (INDTAU(1).GT.0)THEN
      write(1011,*)
      write(1011,*)'Aerosol extinction optical depth'
      do IL=1,NLYR
      write(1011,*)IL,'-th layer'
      do ISD=1,NSD
      write(1011,*)ISD,'-th component'
      write(1011,*)
      do IW=1,NW
      write(1011,86)Wave(IW),EXT(IW,IL,ISD)
      enddo
      enddo
      enddo
C***************************************************************************
      write(1011,*)
      write(1011,*)'Aerosol absorption optical depth'
      do IL=1,NLYR
      write(1011,*)IL,'-th layer'
      do ISD=1,NSD
      write(1011,*)ISD,'-th component'
      write(1011,*)
      write(1011,997)'r','min','max'
      do IW=1,NW
	IF(SSA(IW,IL,ISD).LT.1.0E-30)SSA(IW,IL,ISD)=1.0E-30
      CSB2(IW,IL,ISD)=SQRT(CSB1(IW,IL,ISD)*CSB1(IW,IL,ISD)
     &+CS(IW,IL,ISD)*CS(IW,IL,ISD))
      xc=EXP(LOG(SSA(IW,IL,ISD))+CSB2(IW,IL,ISD))
      IF(xc.GT.1.)xc=1.
      write(1011,85)Wave(IW),EXT(IW,IL,ISD)*(1.-SSA(IW,IL,ISD)),
     &EXT(IW,IL,ISD)*(1.-xc),
     &EXT(IW,IL,ISD)*(1.-EXP(LOG(SSA(IW,IL,ISD))-CSB2(IW,IL,ISD)))
      enddo
      enddo
      enddo
      ENDIF
C****************************************************************************
      write(1011,*)''
      write(1011,*)'        Rmin             Rmax'
      write(1011,*)''
      do i1=1,NSD
      write(1011,*)i1,'-th component'
      i2=NBIN(i1)
      write(1011,78)'Total  ',RADIUS(i1,1),RADIUS(i1,i2)
      i2=NBINF(1,i1)
      write(1011,78)'Fine   ',RADIUSF(i1,1),RADIUSF(i1,i2)
      i2=NBINC(1,i1)
      write(1011,78)'Coarse ',RADIUSC(i1,1),RADIUSC(i1,i2)
      enddo
      write(1011,*)''
      write(1011,*)'Effective Radius'
      write(1011,*)''
      do i1=1,NSD
      write(1011,*)i1,'-th component'
      write(1011,79)'Total  ',rt(i1)
      write(1011,79)'Fine   ',rf(i1)
      write(1011,79)'Coarse ',rc(i1)
      write(1011,*)
      enddo
      write(1011,*)''
      write(1011,*)'Volume Median Radius'
      write(1011,*)''
      do i1=1,NSD
      write(1011,*)i1,'-th component'
      write(1011,79)'Total  ',exp(rft(i1))
      write(1011,79)'Fine   ',exp(rfl(i1))
      write(1011,79)'Coarse ',exp(rcl(i1))
      write(1011,*)
      enddo
      write(1011,*)''
      write(1011,*)'Standard Deviation'
      write(1011,*)''
      do i1=1,NSD
      write(1011,*)i1,'-th component'
      write(1011,79)'Total  ',st(i1)
      write(1011,79)'Fine   ',sf(i1)
      write(1011,79)'Coarse ',sc(i1)
      write(1011,*)
      enddo
      write(1011,*)''
      write(1011,*)'Volume concentration'
      write(1011,*)''
      do i1=1,NSD
      write(1011,*)i1,'-th component'
      write(1011,79)'Total  ',rct(i1)
      write(1011,79)'Fine   ',rflc(i1)
      write(1011,79)'Coarse ',rclc(i1)
      write(1011,*)
      enddo
C********************************************************************
      do i1=1,NW
      IF(INDTAU(i1).EQ.1)INTAU=1
      enddo
      IF(INTAU.EQ.1)THEN
      write(1011,*)''
      write(1011,*)'Optical depth'
      write(1011,*)
      write(1011,*)'          Fit    Measured    Difference'
      ENDIF
      iks=1
      ikt=0  
      do i2=1,NW
      IF(INDTAU(i2).EQ.1)THEN
      ikt=ikt+1
      do i3=1,KM
      IF(KNOISE(2,ikt).EQ.i3)THEN
      FPT(iks)=EXP(F(i3))
      FP1T(iks)=EXP(FP(i3))
      ENDIF
      enddo
      ENDIF
      iks=iks+1
      enddo
      write(1011,*)''
CXXA
      do i1=1,NW
      IF(INDTAU(i1).EQ.1)THEN
      write(1011,80)Wave(i1),FP1T(i1),FPT(i1),FPT(i1)-FP1T(i1)
      ENDIF
      enddo
C*********************************************************************
C*********LAST****CORRECTION*****************************************************\
        CALL forwMN(0,KM,KN,IT,KL,0,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     & ,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     & ,IBRDF1LOC,ISHAPELOC,ISTAR
     & ,AP,SSA,EXT,PF,FP1,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
     &ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
      do i1=1,NW
		IF(INDSK(i1).EQ.1)INSK=1
      enddo
      IF(INSK.EQ.1)THEN
		write(1011,*)''
		write(1011,*)'Sky Radiances'
!tl      write(8,*)'Sky Radiances'
		write(1011,*)
      ENDIF
c	write(*,*)'inws(1)=',inws(1)
      ik1=1
      ik2=inws(1)
      ikt=0
      do i1=1,NW
		IF(INDSK(i1).EQ.1)THEN
			ikt=ikt+1
			write(1011,82)'Wavelength ',WAVE(i1)
			write(1011,*)'Angle     Fit    Measured    Difference'
			write(1011,*)
			iks=1
			IF(i1.GT.1)THEN
				ik2=ik2+inws(i1)
			ENDIF
cs      write(6,*)'check1'
			IF(INDORDER(1).EQ.1)ia=1
			IF(INDORDER(1).EQ.3.AND.INDSAT(i1).EQ.1)ia=2
			IF(INDORDER(1).EQ.3.AND.INDSAT(i1).EQ.0)ia=1
cs      write(6,*)'check2'
			do i2=ik1,ik2
				do i3=1,KM
					IF(KNOISE(1,i2).EQ.i3)THEN
						FPT(iks)=F(i3)
						FP1T(iks)=FP1(i3)
						ANGLE(iks)=ANGLESCA(i1,ia,iks+1)
						iks=iks+1
					ENDIF
				enddo
			enddo
			in=inws(i1)
			do i4=1,in
				write(1011,92)ANGLE(i4),EXP(FP1T(i4)),EXP(FPT(i4)),
     &			(FPT(i4)-FP1T(i4))*100.,' %'
			enddo
			write(1011,*)
			ik1=ik1+inws(i1)
		ENDIF
      enddo
cs      write(6,*)'check3'
cs      write(6,*)'ANGLESCA'
cs      do i1=1,NW
cs      do i2=1,2
cs      write(6,*)WAVE(i1),i2,(ANGLESCA(i1,i2,i3),i3=1,150)
cs      enddo
cs      enddo
C***********************************************************************
      IF(INDPL.EQ.1)THEN
		write(*,*)'Polarization'
		write(*,*)
      ENDIF
      ik1=1
      ik2=inwp(1)
      do i1=1,NW
		IF(IPOLPR(i1).EQ.1)THEN
			write(*,82)'Wavelength ',WAVE(i1)
			write(*,*)'Angle     Fit    Measured    Difference'
			write(*,*)
			iks=1
			IF(i1.GT.1)THEN
				ik2=ik2+inwp(i1)
			ENDIF
			do i2=ik1,ik2
				do i3=1,KM
					IF(KNOISE(3,i2).EQ.i3)THEN
						FPT(iks)=EXP(F(i3))-1.
						FP1T(iks)=EXP(FP(i3))-1.
						ANGLE(iks)=ANGLESCA(i1,2,iks)
						iks=iks+1
					ENDIF
				enddo
			enddo
			in=inwp(i1)
			IF(INDPL.EQ.1)THEN
				do i4=1,in
					IF(IPOLPR(i1).EQ.1)THEN
						write(*,83)ANGLE(i4),FP1T(i4),FPT(i4),
     &					FPT(i4)-FP1T(i4)
					ENDIF
				enddo
			ENDIF
			ik1=ik1+inwp(i1)
		ENDIF
      enddo
C***************************************************************************
      IF(INOISE.GT.1.OR.INDSAT(1).EQ.1)THEN
		do i1=1,NW
			IF(INDSAT(i1).EQ.1.AND.INDPL.EQ.0)INSAT=1
		enddo
		IF(INSAT.EQ.1.AND.INDPL.EQ.0)THEN
			write(*,*)''
			write(*,*)'Aer/Sat Radiances'
			write(*,*)
		ENDIF
		ik1=1
		ik2=inwp(1)
		do i1=1,NW
			IF(INDSAT(i1).EQ.1.AND.INDPL.EQ.0)THEN
				write(*,82)'Wavelength ',WAVE(i1)
				write(*,*)'Angle     Fit    Measured    Difference'
				write(*,*)
				iks=1
				IF(i1.GT.1)THEN
					ik2=ik2+inwp(i1)
				ENDIF
				IF(INDORDER(1).EQ.1)ia=2
				IF(INDORDER(1).EQ.3)ia=1
				do i2=ik1,ik2
					do i3=1,KM
C********************************************************************
						IF(INOISE.GT.1)THEN
							IF(KNOISE(3,i2).EQ.i3)THEN
								FPT(iks)=F(i3)
								FP1T(iks)=FP(i3)
								ANGLE(iks)=ANGLESCA(i1,ia,iks)
								iks=iks+1
							ENDIF
						ELSE
							IF(i2.EQ.i3)THEN
								FPT(iks)=F(i3)
								FP1T(iks)=FP(i3)
								ANGLE(iks)=ANGLESCA(i1,ia,iks)
								iks=iks+1
							ENDIF
						ENDIF
C********************************************************************
					enddo
				enddo
				in=inwp(i1)
				do i4=1,in
					write(*,81)ANGLE(i4),EXP(FP1T(i4)),EXP(FPT(i4)),
     &				(FPT(i4)-FP1T(i4))*100.,' %'
				enddo
				write(*,*)
				ik1=ik1+inwp(i1)
			ENDIF
		enddo
      ENDIF
C*********************************************************************************
      write(1011,*)
      write(1011,*)'Phase Function Errors'
      do IL=1,NLYR
		write(1011,*)IL,'-th layer'
		do ISD=1,NSD
			write(1011,*)ISD,'-th component'
			do IW=1,NW
				write(1011,*)
				write(1011,82)'Wavelength ',WAVE(IW)
				write(1011,996)'ang','r ','min','max ','er ','eb','erb'
				do IA=1,IANGL
					CPB2(IA,IW,IL,ISD)=
     &				SQRT(CPB1(IA,IW,IL,ISD)*CPB1(IA,IW,IL,ISD)+
     &				CP(IA,IW,IL,ISD)*CP(IA,IW,IL,ISD))
					write(1011,87)ANG(IA),PF(IA,IW,IL,ISD),
     &				EXP(LOG(PF(IA,IW,IL,ISD))-CPB2(IA,IW,IL,ISD)),
     &				EXP(LOG(PF(IA,IW,IL,ISD))+CPB2(IA,IW,IL,ISD)),
     &				CP(IA,IW,IL,ISD),
     &				CPB1(IA,IW,IL,ISD),CPB2(IA,IW,IL,ISD)
				enddo
			enddo
		enddo
      enddo
C**********************************************************************
      do i1=1,NW
		IF(IPOLPR(i1).EQ.1)INDPL=1
      enddo
      IF(INDPL.EQ.1)THEN
		write(*,*)
		write(*,*)'Degree of Linear Polarization Errors'
		do IL=1,NLYR
			write(*,*)IL,'-th layer'
			do ISD=1,NSD
				write(*,*)ISD,'-th component'
				do IW=1,NW
					IF(IPOLPR(IW).EQ.1)THEN
						write(*,82)'Wavelength ',WAVE(IW)
						write(*,996)'ang','r ','min','max ','er ','eb','erb'
						do IA=1,IANGL
							CPBP2(IA,IW,IL,ISD)=
     &						SQRT(CPBP1(IA,IW,IL,ISD)*CPBP1(IA,IW,IL,ISD)+
     &						CPP(IA,IW,IL,ISD)*CPP(IA,IW,IL,ISD))
				write(*,87)ANG(IA),-DLP(IA,IW,IL,ISD),
     &			-(EXP(LOG(DLP(IA,IW,IL,ISD)+1.)-CPBP2(IA,IW,IL,ISD))-1.),
     &			-(EXP(LOG(DLP(IA,IW,IL,ISD)+1.)+CPBP2(IA,IW,IL,ISD))-1.),
     &			CPP(IA,IW,IL,ISD),
     &			CPBP1(IA,IW,IL,ISD),CPBP2(IA,IW,IL,ISD)
						enddo
					ENDIF
				enddo
			enddo
		enddo
      ENDIF
C************BRDF***errors*calculation*************************************

      IF(NBRDF.GE.0)THEN
cs       do i1=31,KN
cs      APMIN(i1)=ap(etr-ACOV(i1)
cx       APMAX(i1)=AP(i1)+ACOV(i1)
cs      write(6,*)'AP,APMIN,APMAX',EXP(AP(i1)),
cs     &EXP(APMIN(i1)),EXP(APMAX(i1))
cs       enddo
C		CALL forwMN(0,KM,KN,IT,KL,0,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
C    &	,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
C     &	,IBRDF1LOC,ISHAPELOC,ISTAR
C     &	,APMIN,SSA,EXT,PF,FPMIN,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
C     &	ANGLESCA,SZAS,FTAU,FUMUY,FPHI,1,0,0)
		
C		CALL forwMN(0,KM,KN,IT,KL,0,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
C     &	,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
C     &	,IBRDF1LOC,ISHAPELOC,ISTAR
C     &	,APMAX,SSA,EXT,PF,FPMAX,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
C     &	ANGLESCA,SZAS,FTAU,FUMUY,FPHI,1,0,0)

		CALL forwMN(0,KM,KN,IT,KL,0,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     &	,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     &	,IBRDF1LOC,ISHAPELOC,ISTAR
     &	,AP,SSA,EXT,PF,FPMID,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
     &	ANGLESCA,SZAS,FTAU,FUMUY,FPHI,1,0,0)

		do i1=1,NW
			ALBEDOR(i1)=FLUP(i1)
		enddo

		do i1=1,NW
			IF(INDSAT(i1).EQ.1)INSAT=1
		enddo
		IF(INSAT.EQ.1.AND.NBRDF.GT.0)THEN
			write(*,*)''
			write(*,*)'BRDF'
			write(*,*)
		ENDIF

		ik1=1
		ik2=inwp(1)

		do i1=1,NW
			IF(INDSAT(i1).EQ.1.AND.NBRDF.GT.0)THEN
				write(*,82)'Wavelength ',WAVE(i1)
				write(*,*)'Angle  Retrieved   Min         Max'
				write(*,*)
				iks=1
				IF(i1.GT.1)THEN
					ik2=ik2+inwp(i1)
				ENDIF
				IF(INDORDER(1).EQ.1)ia=2
				IF(INDORDER(1).EQ.3)ia=1
				do i2=ik1,ik2
					do i3=1,KM
						IF(INOISE.GT.1)THEN
							IF(KNOISE(3,i2).EQ.i3)THEN
								FP1(iks)=FPMIN(i3)
								FP1T(iks)=FPMAX(i3)
								FPT(iks)=FPMID(i3)
								ANGLE(iks)=ANGLESCA(i1,ia,iks)
								iks=iks+1
							ENDIF
						ELSE
							IF(i2.EQ.i3)THEN
								FP1(iks)=FPMIN(i3)
								FP1T(iks)=FPMAX(i3)
								FPT(iks)=FPMID(i3)
								ANGLE(iks)=ANGLESCA(i1,ia,iks)
								iks=iks+1
							ENDIF
						ENDIF
					enddo
				enddo
				in=inwp(i1)

				do i4=1,in
					write(*,91)ANGLE(i4),EXP(FPT(i4)),EXP(FP1(i4)),
     &				EXP(FP1T(i4))
				enddo
				write(*,*)
				ik1=ik1+inwp(i1)
			ENDIF
		enddo
      ENDIF
C*************************************************************************
      write(1011,*)''
      write(1011,*)'Sphericity Parameter'  
      write(1011,*)     
      sp1=EXP(AP(ISTAR(ISHAPELOC(1))))
cs      sp1min=EXP(APMIN(ISTAR(ISHAPELOC(1))))
cs      sp1max=EXP(APMAX(ISTAR(ISHAPELOC(1))))
cs      sp1covr=ACOVR(ISTAR(ISHAPELOC(1)))/6.5
cs      sp1covr=ACOVR(ISTAR(ISHAPELOC(1)))
cs      IF((anstr.LE.1.0).AND.(sp1.GT.0.85))THEN
cs      sp1covr=0.1
c      write(6,*)'sp1covr',sp1covr
cs      ENDIF
cs      IF(anstr.GT.1.2)THEN 
cs      sp1covr=3.6
c      write(6,*)'sp1covr',sp1covr
cs      ENDIF
cs      sp1bias=ABIASC(ISTAR(ISHAPELOC(1)))
cs      sp1cov=ACOV(ISTAR(ISHAPELOC(1)))
cs      sp1cov=SQRT(sp1covr**2+sp1bias**2)
cs      sp1min=EXP(alog(sp1)-sp1cov)
cs      sp1max=EXP(alog(sp1)+sp1cov)
      sp2=EXP(AP(ISTAR(ISHAPELOC(1))+1))
      sp2=1.-sp1
      sp=(sp1/(sp1+sp2))*100.
      anstr_mod(1)=0.2
      anstr_mod(2)=1.7
      tair=EXP(TAUA(1))/SZAS
      xlags(1)=1.0704*tair**(-0.35139)
      xlags(2)=2.5
      sp1cov=LINEAR2(anstr_mod,alog(xlags),2,anstr)
      sp1cov=EXP(sp1cov)
      WRITE(1011,*)sp,sp1cov 
C**************************************************************************
CCS****************************OUTPUT*FILE********************
C*************************************************************
cs************************************RSDFORCE *CORRECTION***************************
        CALL forwMN(0,KM,KN,IT,KL,0,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     & ,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     & ,IBRDF1LOC,ISHAPELOC,ISTAR
     & ,AP,SSA,EXT,PF,FPMID,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
     &ANGLESCA,SZAS,FTAU,FUMUY,FPHI,1,0,1)
        CALL forwMN(0,KM,KN,IT,KL,0,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     & ,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     & ,IBRDF1LOC,ISHAPELOC,ISTAR
     & ,AP,SSA,EXT,PF,FPMID,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
     &ANGLESCA,SZAS,FTAU,FUMUY,FPHI,1,0,1)
cs*************************************************************************************
       COND=0
       CALL forwMN(0,KM,KN,IT,KL,IMSC,NW,WAVE,NSD,NBIN1,NBIN,RADIUS
     &,NLYR,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC
     &,IBRDF1LOC,ISHAPELOC,ISTAR,
     & AP,SSA,EXT,PF,FP1,INDB,IEL,IS,FLDD,FLUP,FLDR,0,IPOLPR,DLP,
     &ANGLESCA,SZAS,FTAU,FUMUY,FPHI,0,0,0)
        write(1011,*)
        write(1011,*)'Radiative Forcing'
        write(1011,*)
 	WRITE(1011,*)'ALTIT  FLXDWN   FLXUP'
 	WRITE(1011,*)' (km)  (W/m2)   (W/m2)'
        DO L=1,10
		if(L.eq.1.or.L.eq.10)then
			WRITE(1011,994)ALTIT(L),FLXINTA(L,1),FLXINTA(L,2),
     &                       FLXINTA(L,3),FLXINTA(L,4)
		endif
        ENDDO
c       WRITE(*,*)
        IF(INDTAU(1).GT.0)THEN
		write(1011,1166)(ALBEDOR(i1),i1 = 1,NW)
        ENDIF
	WRITE(1011,*)'ALTIT RADIAT FORCING FORCING EFFICIENCY'
	WRITE(1011,*)' (km)   (W/m2)        (Wm-2/AOD(0.55))'
	WRITE(1011,994) ALTIT(1), FORC_BOT,EF_BOT
	WRITE(1011,994) ALTIT(10), FORC_TOA, EF_TOA
c      IF(INSAT.EQ.1)THEN
c      write(*,*)
c      write(*,*)'Retrirved surface albedo'
c      write(*,*)
c      write (*,1166) ((WAVE(i1),ALBEDOR(i1)), i1=1,NW)
 1166 FORMAT(10F9.5)
c      do i1=1,NW
c      write(*,116)WAVE(i1),ALBEDOR(i1)
c      enddo
c      ENDIF
21    CONTINUE
      IF(KL.EQ.1) THEN
cs      WRITE(*,*) 'WAVELENGTH(mkm), EXT OPT THICKNESS:'
		DO ISD=1,NSD
			DO IL=1,NLYR
cs       WRITE(*,*) IL,' th- Layer'
				DO IW=1,NW
cs        WRITE(0,*) WAVE(IW),EXT(IW,IL,ISD)
				ENDDO
			ENDDO
		ENDDO
		IF(NBRDF.GT.0) THEN
			DO IB=1,NBRDF
				DO IW=1,NW
cs       WRITE(*,*) ' WAVELENGTH(mkm) '
cs     &,IB,' -th spectral BRDF parameter:'
cs       WRITE(*,*) WAVE(IW), EXP(AP(ISTAR(IBRDFLOC(IB))+IW-1))
				ENDDO
			ENDDO
		ENDIF
		IF(NBRDF1.GT.0) THEN
			DO IBB=1,NBRDF1
cs       WRITE(*,*) IBB,' -th non-spectral BRDF parameter:'
cs       WRITE(*,*)  EXP(AP(ISTAR(IBRDF1LOC(1))+IBB-1))
			ENDDO
		ENDIF
		IF(NSHAPE.GT.0) THEN
			DO IBB=1,NSHAPE
cs       WRITE(*,*) IBB,' -th ahape parameter:'
cs       WRITE(*,*)  EXP(AP(ISTAR(ISHAPELOC(1))+IBB-1))
			ENDDO
		ENDIF
		DO IL=1,NLYR
			DO ISD=1,NSD
cs      WRITE(*,*) 'WAVELENGTH(mkm), SINGLE SCATTERING ALBEDO:'
				DO IW=1,NW
cs      WRITE(*,111) WAVE(IW), SSA(IW,IL,ISD)
				ENDDO
cs      WRITE(*,*) ' WAVELENGTH(mkm), REAL PART of REF. INDEX:'
				DO IW=1,NW
cs      WRITE(*,*)  WAVE(IW),EXP(AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1))
				ENDDO
cs      WRITE(*,*) ' WAVELENGTH(mkm), IMAGINARY PART of REF. INDEX:'
				DO IW=1,NW
cs      WRITE(*,*)  WAVE(IW),EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1))
				ENDDO
cs      WRITE(*,*) 'RAIUS (mkm):  SIZE DISTRIBUTION: (mkm3/mkm2):'
				DO I=1,NBIN(ISD)
cs      WRITE(*,*) RADIUS(ISD,I), EXP(AP(ISTAR(ISDLOC(IL,ISD))+I-1))
				ENDDO
			ENDDO
		ENDDO
		WRITE(77,*) ' FP FITTED MEASUREMENTS: '
		DO J=1,KM
			WRITE(77,*) J,EXP(FP(J)),EXP(F(J))
		ENDDO
	ENDIF
111     FORMAT(7E16.7)
112     FORMAT(1E16.7)
122     FORMAT(60F9.3)
67      FORMAT(f31.4,f10.4,f10.4,f20.4,f10.4,f10.4)
66      FORMAT(a10,f6.3)
68      FORMAT(f23.2,f15.5,f11.5,f13.5)
69      FORMAT(a11,e18.11)
70      FORMAT(a11,f16.11,a1)
71      FORMAT(f6.3,e47.11,a1,f34.11,a1)
72      FORMAT(f6.3,f47.11,f34.11,f34.11)
73      FORMAT(f6.3,6e13.5,e23.5,5e13.5)
74      FORMAT(7e18.11)
75      FORMAT(a20,e18.11)
76      FORMAT(f6.3,e47.11,f34.11)
77      FORMAT(f23.2,f15.5,f11.5,f13.5,f47.5,2f11.5)
78      FORMAT(a8,f6.3,f20.3)
79      FORMAT(a8,f6.3)
80      FORMAT(f6.3,3f10.6)
81      FORMAT(f5.1,2f11.6,f13.6,a2)
82      FORMAT(a11,f6.3)
83      FORMAT(f5.1,2f10.6,f13.6)
84      FORMAT(f6.3,6e13.5)
85      FORMAT(f6.3,3e13.5)
86      FORMAT(f6.3,f7.4)
87      FORMAT(f5.1,6e21.10)
88      FORMAT(a40,f16.11,a2)
89      FORMAT(f6.3,6e13.5)
90      FORMAT(f5.1,2f10.6,f13.6)
91      FORMAT(f5.1,2f10.6,f13.6,f13.6)
92      FORMAT(f7.2,2f10.6,f13.6,a2)
115     FORMAT(42f18.6)
116     FORMAT(f7.4,f11.7)
999     FORMAT(2a14,a13,a12,a13,a14,a22,a14,a12,2a13,a14)
998     FORMAT(2a14,2a13,a12,a14)
997     FORMAT(2a14,a13)
996     FORMAT(a6,a13,4a21,a20)
995     FORMAT(2a14,2a13,a12,a13)
994     FORMAT(F7.3,4(2X,F10.5))
c *** DEALLOCATE ARRAYS
c
c***********************************************************
      IF(key.lt.4) THEN
c
c *** DEALLOCATE ARRAYS (alloc.mod) in subroutine USMATRIX 
c *** (in matrix_intrpl.f)
c
        DEALLOCATE(UFEA,stat=ierr)
        if(ierr/=0) stop 'Can not deallocate UFEA array'
        if(keyEL.gt.0) then
          DEALLOCATE(UF11,stat=ierr)
cxxa          if(ierr/=0) stop 'Can not deallocate UF11 array'

        if(keyEL.gt.1) then
          DEALLOCATE(UF12,stat=ierr)
cxxa          if(ierr/=0) stop 'Can not deallocate UF12 array'
		endif
        if(keyEL.gt.2) then
          DEALLOCATE(UF22,stat=ierr)
cxxa          if(ierr/=0) stop 'Can not deallocate UF22 array'
        endif
        if(keyEL.gt.3) then
          DEALLOCATE(UF33,stat=ierr)
cxxa          if(ierr/=0) stop 'Can not deallocate UF33 array'
        endif
        if(keyEL.gt.4) then
          DEALLOCATE(UF34,stat=ierr)
cxxa          if(ierr/=0) stop 'Can not deallocate UF34 array'
        endif
        if(keyEL.gt.5) then
          DEALLOCATE(UF44,stat=ierr)
cxxa          if(ierr/=0) stop 'Can not deallocate UF44 array'
		endif 
        ENDIF ! keyEL
      ENDIF ! key.lt.4
      IF(key.gt.2) THEN 
c
c *** DEALLOCATE ARRAYS (alloc1.mod)
c
      DEALLOCATE(UEA,stat=ierr)
cxxa      if(ierr/=0) stop 'Can not deallocate UEA array'
	  if(keyEL.gt.0) then
        DEALLOCATE(U11,stat=ierr)
cxxa        if(ierr/=0) stop 'Can not deallocate U11 array'

	  if(keyEL.gt.1) then
        DEALLOCATE(U12,stat=ierr)
cxxa        if(ierr/=0) stop 'Can not deallocate U12 array'
	  endif
	  if(keyEL.gt.2) then   
        DEALLOCATE(U22,stat=ierr)
cxxa        if(ierr/=0) stop 'Can not deallocate U22 array'
	  endif
      if(keyEL.gt.3) then
		DEALLOCATE(U33,stat=ierr)
cxxa        if(ierr/=0) stop 'Can not deallocate U33 array'
	  endif
	  if(keyEL.gt.4) then
        DEALLOCATE(U34,stat=ierr)
cxxa        if(ierr/=0) stop 'Can not deallocate U34 array'
	  endif
	  if(keyEL.gt.5) then
        DEALLOCATE(U44,stat=ierr)
cxxa        if(ierr/=0) stop 'Can not deallocate U44 array'
	  endif 
      ENDIF ! key_EL
      ENDIF ! key>2 

c***********************************************************
c***********************************************************
	RETURN
      STOP
      END
      
      SUBROUTINE SIZEDISD(KN,ID,NSD,NMD,CM,SM,RMM,RMIN,RMAX,AR,AC)
C*****************************************************
C* DETERMINING bi-modal LogNormal size distribution:
C         d(...)/dlnR in KN - Points
C*****************************************************
C  INPUT:
C  KN  I(NSD)  - number of radius points
C        <0 logarithmic intervals
C        >0 linear intervals
C  ID  I  - dimension:
C         = 0 - number
C         = 1 - radius
C         = 2 - area
C         = 3 - volume
C NSD I    - NUMBER of sizedistr (up to 3)
C NMD I    - number of modes (up to 2)
C CM  R(2,*) - concentrations
C SM  R(2,*) - halfwidths
C RM  R(2,*) - mean radii
C RMIN R(*)  - minimum radius
C RMAX R(*)  - maximum radius
C*****************************************************
C OUTPUT:
C AR  R(NSD,*) - dV/dlnR (in M)
C AC  R      - total VOLUME concentration (M3/M3)
C*****************************************************
C
      PARAMETER (KSD=3,KMD=2)
      INTEGER KN(KSD),KNN(KSD)
      REAL CM(KMD,*),SM(KMD,*),RMM(KMD,*),RMIN(*),RMAX(*)
      REAL AR(KSD,*)
      REAL CM0(KMD,KSD),RMM0(KMD,KSD)
      REAL CM1(KMD,KSD),RMM1(KMD,KSD)
      REAL CM2(KMD,KSD),RMM2(KMD,KSD)
      REAL CM3(KMD,KSD),RMM3(KMD,KSD)
      REAL AC(KMD)
C*****************************************************
C recalculation SizDis d(...)/dlnR parameters 
C in different dimensions:
C*****************************************************
      PI    = ACOS( -1.0 )
      DO ISD=1,NSD
      KNN(ISD)=KN(ISD)
      IF(KN(ISD).LT.0) KNN(ISD)=-KN(ISD)
      DO JJ=1,KNN(ISD)
      AR(ISD,JJ)=0.
      ENDDO
      ENDDO
        IF (ID.EQ.0) THEN
        DO IM=1,NMD
        DO I=1,NSD
        CM0(IM,I)=CM(IM,I)
        RMM0(IM,I)=RMM(IM,I)
        ENDDO
        ENDDO
        GO TO 5
        ENDIF
      IF (ID.EQ.1) THEN
      DO IM=1,NMD
      DO I=1,NSD
	IF(RMM(IM,I).LT.1.0E-30)RMM(IM,I)=1.0E-30
	IF(SM(IM,I).LT.1.0E-30)SM(IM,I)=1.0E-30
      RMM0(IM,I)=EXP(LOG(RMM(IM,I))-LOG(SM(IM,I))*LOG(SM(IM,I)))
      CM0(IM,I)=CM(IM,I)/RMM0(IM,I)*EXP(0.5*LOG(SM(IM,I))*LOG(SM(IM,I)))
      ENDDO
      ENDDO
      GO TO 5
      ENDIF
        IF (ID.EQ.2) THEN
        DO IM=1,NMD
        DO I=1,NSD
		IF(RMM(IM,I).LT.1.0E-30)RMM(IM,I)=1.0E-30
		IF(SM(IM,I).LT.1.0E-30)SM(IM,I)=1.0E-30
        RMM0(IM,I)=EXP(LOG(RMM(IM,I))-2.0*LOG(SM(IM,I))*LOG(SM(IM,I)))
        CM0(IM,I)=CM(IM,I)/(PI*RMM0(IM,I)*RMM0(IM,I))/
     &   EXP(2.0*LOG(SM(IM,I))*LOG(SM(IM,I)))
        ENDDO
        ENDDO
        GO TO 5
        ENDIF
      IF (ID.EQ.3) THEN
      DO IM=1,NMD
      DO I=1,NSD
	IF(RMM(IM,I).LT.1.0E-30)RMM(IM,I)=1.0E-30
	IF(SM(IM,I).LT.1.0E-30)SM(IM,I)=1.0E-30
      RMM0(IM,I)=EXP(LOG(RMM(IM,I))-3.0*LOG(SM(IM,I))*LOG(SM(IM,I)))
      CM0(IM,I)=CM(IM,I)/(4.0/3.0)/
     &(PI*RMM0(IM,I)*RMM0(IM,I)*RMM0(IM,I))
     &/EXP(9.0/2.0*LOG(SM(IM,I))*LOG(SM(IM,I)))
      ENDDO
      ENDDO
      GO TO 5
      ENDIF
5     I=0
      DO 4 IM=1,NMD
      DO 4 I=1,NSD
	IF(SM(IM,I).LT.1.0E-30)SM(IM,I)=1.0E-30
	IF(RMM0(IM,I).LT.1.0E-30)RMM0(IM,I)=1.0E-30
      CM1(IM,I)=CM0(IM,I)*RMM0(IM,I)
     &*EXP(0.5*LOG(SM(IM,I))*LOG(SM(IM,I)))
      RMM1(IM,I)=EXP(LOG(RMM0(IM,I))+LOG(SM(IM,I))*LOG(SM(IM,I)))
      CM2(IM,I)=CM0(IM,I)*PI*RMM0(IM,I)*RMM0(IM,I)*EXP(2.0*LOG(SM(IM,I))
     &*LOG(SM(IM,I)))
      RMM2(IM,I)=EXP(LOG(RMM0(IM,I))+2.0*LOG(SM(IM,I))*LOG(SM(IM,I)))
      CM3(IM,I)=CM0(IM,I)*4.0/3.0*PI*RMM0(IM,I)*RMM0(IM,I)*RMM0(IM,I)
     &*EXP(9.0/2.0*LOG(SM(IM,I))*LOG(SM(IM,I)))
      RMM3(IM,I)=EXP(LOG(RMM0(IM,I))+3.0*LOG(SM(IM,I))*LOG(SM(IM,I)))
4     CONTINUE
      OPEN (7,FILE='LNPAR.dat',status='unknown')
      DO I=1,NSD 
      WRITE(7,*)'  halfwidth:'
      WRITE(7,17) (SM(IM,I),IM=1,NMD)
      WRITE(7,*) ' number concentration, mean radius:'
      WRITE(7,17) (CM0(IM,I),RMM0(IM,I),IM=1,NMD)
      WRITE(7,*) ' radius concentration, mean radius:'
      WRITE(7,17) (CM1(IM,I),RMM1(IM,I),IM=1,NMD)
      WRITE(7,*) ' area concentration, mean radius:'
      WRITE(7,17) (CM2(IM,I),RMM2(IM,I),IM=1,NMD)
      WRITE(7,*) ' volume concentration, mean radius:'
      WRITE(7,17) (CM3(IM,I),RMM3(IM,I),IM=1,NMD)
      ENDDO
      CLOSE (7)
17    FORMAT (4F11.4) 
      DO II=1,NSD
CD      AA(II)=0.0
CD      AC(II)=0.0
      DO I=1,KNN(II)
      AR(II,I)=0.0
      ENDDO
      ENDDO
      DO IM=1,NMD
CD      AA(IM)=0.0
CD      AC(IM)=0.0
      ENDDO
      DO I=1,NSD
      DO IM=1,NMD
      AI=0.
      CALL SDNORM(CM(IM,I),SM(IM,I),RMM(IM,I),RMIN(I),RMAX(I),AI)
C      AA(I)=AA(I)+AI
C      AC(I)=AC(I)+CM(IM,I)
      ENDDO
      ENDDO
C      WRITE(*,*) (AA(I),AC(I),I=1,NSD),' AA,AC'
C***** KN<0 logarithmic intervals *******
      DO 1 II=1,NSD
		IF(KN(II).LT.0) THEN
			IF(RMIN(II).LT.1.0E-30)RMIN(II)=1.0E-30
			RH=(LOG(RMAX(II))-LOG(RMIN(II)))/(KNN(II)-1)
		ENDIF
		IF(KN(II).GT.0) RH=((RMAX(II))-(RMIN(II)))/(KNN(II)-1)
		DO 1 IM=1,NMD
			DO 1 I=1,KNN(II)
				IF(KN(II).LT.0) THEN
					IF(RMIN(II).LT.1.0E-30)RMIN(II)=1.0E-30
					RR=EXP(LOG(RMIN(II))+(I-1)*RH)
				ENDIF
      IF(KN(II).GT.0) RR=RMIN(II)+(I-1)*RH
            AR(II,I)=AR(II,I)+SLOG(CM(IM,II),SM(IM,II),RMM(IM,II),RR)
C      AR(II,I)=AR(II,I)+SLOG(CM(IM,II),SM(IM,II),RMM(IM,II),RR)
C     &/AA(II)*AC(II)
1     CONTINUE  
      ACC=0
      DO IM=1,NMD
      ACC=ACC+AC(IM)
      ENDDO          
      RETURN
      END

      SUBROUTINE SDNORM1(C,S,RM,RMIN,RMAX,AC)
!tl      SUBROUTINE SDNORM(C,S,RM,RMIN,RMAX,AC)
C*****************************************************
C  NORMALIZATION of LOGNORMAL FUNCTION d(...)/dlnR
C   for descrete interval of sizes: RMIN-RMAX
C******************************************************
C  INPUT:
C  C   R  - concentration
C  S   R  - halfwidth
C  RM  R  - mean radius
C RMIN R  - minimum radius
C RMAX R  - maximum radius
C***************************************************** 
C OUTPUT:
C AC  R  - normalization constant
C*****************************************************
      REAL SLOG1
C      WRITE(*,*) C,S,RM,RMIN,RMAX
      KNSIM=301
      PI    = ACOS( -1.0 )
	IF(RMIN.LT.1.0E-30)RMIN=1.0E-30 !CXXA
      AHR=(LOG(RMAX)-LOG(RMIN))/(KNSIM-1)
c      AH=1./(KNSIM-1)
      AA=0.
      DO I=1,KNSIM
	IF(RMIN.LT.1.0E-30)RMIN=1.0E-30
      RR=EXP(LOG(RMIN)+(I-1)*AHR)
      ADD=SLOG1(C,S,RM,RR)
C      WRITE(*,*) AD,' AD'
       II=I/2
       IF(II*2.LT.I) AD=4./3.
       IF(II*2.EQ.I) AD=2./3.
       IF(I.EQ.1.OR.I.EQ.KNSIM) AD=1./3.
       AA=AA+ADD*AHR
      ENDDO
      AC=AA
      RETURN
      END

      REAL FUNCTION SLOG1(C,S,RM,RR)
C*****************************************************
C LOGNORMAL FUNCTION d(RR)/dlnR
C  C   R  - concentration
C  S   R  - halfwidth
C  RM  R  - mean radius
C  RR  R  - value
C***************************************************** 
      IF(I.LE.0)       WRITE(*,*) C,S,RM
       I=I+1
      PI    = ACOS( -1.0 )
      IF(S.LE.1) WRITE(*,*) S,'TROUBLES in SLOG:S.LE.1'
      SLOG1=C/SQRT(2.0*PI)/LOG(S)*EXP((-0.5)*((LOG(RR/RM)*LOG(RR/RM))
     &/(LOG(S)*LOG(S))))
      RETURN
      END

      SUBROUTINE RDMG(R0,N,EM,SGM,R)
C RANDOM NUMBER GENERATOR
C R = exp(-(x-m)**2/2/s**2)/sqrt(2*pi)/s
C--- HISTORY
C 88.10.14 CREATED
C--- INPUT
C R0    D    INITIAL CONDITION (0 TO 1)
C N     I    NUMBER OF RANDOM NUMBERS
C EM    R    Mean (m)
C SGM   R    Sigma (s)
C--- OUTPUT
C R    R(N)  Normal distribution RANDOM NUMBERS (-inf, +inf)
C$ENDI
      SAVE
CDD      implicit real * 8 (a-h, o-z)
      PARAMETER (NU=12)
      DOUBLE PRECISION R0
      DIMENSION R(N),W(NU)
      DO 1 L=1,N
      CALL RDMU1(R0,NU,W)
      S=0
      DO 2 I=1,NU
    2 S=S+W(I)
    1 R(L)=SGM*(S-NU/2.0)+EM
      RETURN
      END
      
      SUBROUTINE RDMU1(R0,N,R)
C RANDOM NUMBER GENERATOR
C--- HISTORY
C 88.10.14 CREATED
C--- INPUT
C R0    D    INITIAL CONDITION (0 TO 1)
C N     I    NUMBER OF RANDOM NUMBERS
C--- OUTPUT
C R    R(N)  UNIFORM RANDOM NUMBERS ( 0 TO 1)
C$ENDI
      SAVE
CDD      implicit real * 8 (a-h, o-z)
      DIMENSION R(N)
      DOUBLE PRECISION R0,R1,PI
      PARAMETER (PI=3.14159265359000)
      DO 1 I=1,N
      R0=(PI+R0)**5
      R1=INT(R0)
      R0=R0-R1
    1 R(I)=R0
      RETURN
      END
