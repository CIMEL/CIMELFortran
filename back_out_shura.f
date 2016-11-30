      SUBROUTINE forwMN(IPRI,KM,KN,IT,KL,IMSC,NW,WAVE,NSD,NBIN1,NBIN,
     &RADIUS,
     & NLAY,NBRDF,NBRDF1,NSHAPE,IANGL,ANGL,ISDLOC,IREFLOC,IBRDFLOC,
     &IBRDF1LOC,ISHAPELOC,
     & ISTAR,AP,SSA,EXT,PF,FP,INDB,IEL,IS,FLDD,FLUP,FLDR,IC,IPOLPR,
     &DLP,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,ITOFF,IBRF,IRFS)
C*****************************************************
C*  This subroutine manages the forward calculations *
C*  for radiative transfer for two cases:            *
C*  1)-when AP containes parameters of d()/dlnR      *
C*  program uses MIE code writen by B. Fomin         *
C*  2)-when AP containes parameters of Phase Function*
C***                                               ***
C   INPUT:
C    IPRI I :
C             =0 - no printing of radiances
C             =1 - print radiances (in DISDS")
C      KM  I   - total number of the measurements 
C      KN  I   _ total number of the retrieved parameters
C      IT     I : 
C             = 0 - refractive index is assumed
C             =-1 - refractive index is retrieved
C      KL     I   -defines log regime
C                 = 0 - absolute calculations
C                 = 1 - log calculations
C      IMSC   I :
C             =0  - radiative transfer calc. in multiple
C                scattering regime
C             =1  - radiative transfer calc. in single
C                scattering regime
C             =-1 - phase function calculation 
C      NW     I   - number of the wavelegths
C      WAVE  R(KW) - values of the angles
C      NSD    I   - the number of considered components in
C      NBIN   I(NSD) - the number of the bins in the
C                       size distributions
C  RADIUS    R(NSD,KBIN) - the radii corresponding to  
C                the bins in the size distributions
C                size distributions
C    IANGL        - the number of angles used for 
C                  phase function
C    ANG  R(IANGL) - the values of the angles for the phase 
C                        function modelling;
C      NLAY   I   - the number of atmospheric layers
C      NBRDF  I   - the number of paramters used in
C                modeling of BRDF
C ISDLOC   I(NLAY,NSD)   - index of the SD in AP()
C IREFLOC  I(NLAY,NSD,1) - index of real part of REF in AP()
C IREFLOC  I(NLAY,NSD,2) - index of imag part of REF in AP()
C IBRDFLOC I(NBRDF)      - index of BRDF in AP()
C ISTAR(NPAR)     - the addresses of the values   
C                  corresponding to IPAR in AP()
C   AP      R(KN) -  vector of "parameters" defined as
C                                   abs.  (KL=0) 
C                               or  log   (KL=1)
C***************************************************   
C   OUTPUT:
C    SSA  R(NW,NLAY,NSD) - single scattering albedo: 
C    EXT  R(NW,NLAY,NSD) - extinction  
C    PF   R(IANGL,NW,NLAY,NSD) - phase function values
C  NO!!!  UAK  R(KM,KN) -matrix of the first derivatives
C                       for a case of IMSC=-1
C    FP   R(KM)    - vector of "measurements" calcualted on
C                   the basis of input "parameters"
C*****************************************************
      PARAMETER (KSD=3,KW=10,KMES=500,KPAR=100,KMD=3,MAXCLY=35,
!tl     &KANG=83,KBRDF=13,KNA0=2,KSHAPE=2)
     &KANG=181,KBRDF=13,KNA0=2,KSHAPE=2)
      CHARACTER ERR*64,CH1*64
      INTEGER MAXUMU
      PARAMETER (MAXUMU=100)
      DIMENSION FP(KMES),FSKY(KMES),AP(KPAR),AP1(KPAR),SD(KPAR)
      INTEGER NBIN(KSD),ISTAR(KPAR),NBIN1(KSD),FTAU(KW)
     &,IREFLOC(MAXCLY,KSD,2),IBRDFLOC(KBRDF),IBRDF1LOC(KBRDF)
     &,ISDLOC(MAXCLY,KSD),IS(KSD),IPOLPR(KW),FUMUY(KW,MAXCLY,KNA0),
     &FPHI(KW,MAXCLY,KNA0,MAXUMU),ISHAPELOC(KSHAPE)
      DIMENSION WAVE(KW),RADIUS(KSD,KPAR),ANG(KANG),RADIUS1(KPAR)
     &,SSA(KW,MAXCLY,KSD),PF(KANG,KW,MAXCLY,KSD),
     &DLP(KANG,KW,MAXCLY,KSD),ANGLESCA(KW,KNA0,KMES)
     &,PF12(KANG,KW,MAXCLY,KSD),PF22(KANG,KW,MAXCLY,KSD)
     &,PF33(KANG,KW,MAXCLY,KSD),PF34(KANG,KW,MAXCLY,KSD)
     &,PF44(KANG,KW,MAXCLY,KSD),FLDD(KW),FLUP(KW),FLDR(KW)
     &,EXT(KW,MAXCLY,KSD),EXTL(MAXCLY),SSAL(MAXCLY)
     &,BRF(KBRDF),BRF1(KBRDF),ANGL(KANG),XBRF(KBRDF),SHAPE(KSHAPE)
     &,PFL(KANG,MAXCLY),PFL12(KANG,MAXCLY)
     &,PFL22(KANG,MAXCLY),PFL33(KANG,MAXCLY)
     &,PFL34(KANG,MAXCLY),PFL44(KANG,MAXCLY)
      DIMENSION
     & PTP11(KANG),PTP12(KANG),PTP22(KANG)
     & ,PTP33(KANG),PTP34(KANG),PTP44(KANG)
CS*********************NEW**VAR**FOR**VECTOR***CODE*****************
      DIMENSION PFV(KANG,KSD),PFV12(KANG,KSD),PFV22(KANG,KSD),
     &PFV33(KANG,KSD)
     &,PFV34(KANG,KSD),PFV44(KANG,KSD),SSAV(KSD),EXTV(KSD)
CS******************************************************************
C************RADIATIVE FORCING**************************
      PARAMETER (MAXCMU=12,MAXULV=10,NBNU=208)       
      DIMENSION FLXINT(MAXULV,4),ALTIT(MAXULV)
     &,FLXNU1(MAXULV,NBNU,4),FLXNU0(MAXULV,NBNU,4)
     &,RREAL1(KW,MAXCLY,KSD),RIMAG1(KW,MAXCLY,KSD)
     &,RADIUS2(KSD,MAXCLY,KPAR),SD2(KSD,MAXCLY,KPAR)
     &,FLXINTA(MAXULV,4)
      INTEGER COND,NSTR1,NMOM,NSTR2,NBNU1,NH2O,NO3,NCO2
     &,NUMU1
      REAL NSSA
      DIMENSION UH2O1(50),UCO21(50),UO31(50),UMU2(50)
      CHARACTER PP*10
      DOUBLE PRECISION FLUX_UP_TOA,FLUX_UP_BOT,
     &FLUX_DOWN_BOT,FLUX_DOWN_TOA,FLUX_DOWN_BOT_AERO,
     &FLUX_UP_TOA_AERO, FLUX_UP_BOT_AERO,FLUX_DOWN_TOA_AERO,
     &FORC_BOT,FORC_TOA,TOTAER0,FS_BOT,FS_TOA,CORSOL,ES_BOT,
     &ES_TOA,TOTAER
         
      INTEGER ITIME,ITIME1,ITIME2,ITIME3   !TIMER

      COMMON /FORCI/ COND
      COMMON /FLX/ ALTIT,FLXINTA,FORC_TOA,FORC_BOT,EF_TOA,EF_BOT
      COMMON /INIT/ APIN 
	COMMON /ILP1/ IV
C********************************************************
cs      WRITE(*,*) ' in forwMN'
      DO I=1,KN
		IF(KL.EQ.1) THEN
			AP1(I)=EXP(AP(I))
			IF(AP(I).LT.-17.) AP1(I)=EXP(-17.)
		ENDIF
		IF(KL.EQ.0) THEN
			AP1(I)=AP(I)
		ENDIF
      ENDDO
111   FORMAT(7E16.7)
CD       IF(IMSC.EQ.-1) THEN
CD       WRITE(*,111) (AP1(I),I=1,KN)
Cd       ENDIF
      NDP=IV
      IV=1
C	WRITE(*,*)'NDP=',NDP,'	IV=',IV
C	WRITE(*,*)'NDP,IV',NDP,IV
C**********************************************************
C*     NDP    I   - index for reading initial files:
C*             = 0 - the all initial data (kernels) are read
C*                  from initial data files
C*             > 0 - the kernes are used , no reaading
C***********************************************************
      III=0
      DO IW=1,NW
C****************************************************************
C* Computing SINGLE scattering properties:
C*      EXTINCTION, SSA, PHASE FUNCTION
C*   for each aerosol layer, for each aerosol componet (ISD)
C****************************************************************
CD      WRITE(*,*) NLAY,NSD,' NLAY,NSD in Forw'
CD       WRITE(*,*) WAVE(IW),' WAVE(IW)'
		IF(IC.EQ.0)THEN
			do i1=1,NSD
				NBIN1(i1)=0
			enddo
		ENDIF
cs       IF(IC.EQ.1)THEN
cs       write(6,*)'NBIN1',(NBIN1(i1),i1=1,NSD)
cs       ENDIF
		DO IL=1,NLAY
			DO ISD=1,NSD
				DO I=1,NBIN(ISD)
					IF(IC.EQ.0)THEN
						RADIUS1(I)=RADIUS(ISD,I+NBIN1(ISD))
						SD(I)=AP1(ISTAR(ISDLOC(IL,ISD))+(I+NBIN1(ISD))-1)
cs*********************LAST************************CORRECTION*************************
						IF(I.EQ.NBIN(ISD))SD(I)=SD(I)/2.
cs************************************************************************************
 
					ENDIF
					IF(IC.EQ.1)THEN
						RADIUS1(I)=RADIUS(ISD,I+NBIN1(ISD)-1)
						SD(I)=AP1(ISTAR(ISDLOC(IL,ISD))+(I+NBIN1(ISD)-1)-1)
cs****************************LAST*****************************CORRECTION*****************
						IF(I.EQ.1)SD(I)=SD(I)/2.
cs*****************************************************************************************
					ENDIF
cs          WRITE(*,*)'IC',IC
C************FOR FORCING************************************	
					RADIUS2(ISD,IL,I)=RADIUS1(I)
					SD2(ISD,IL,I)=SD(I)
	
C***********************************************************

cs          IF(IC.EQ.1)THEN
cs          WRITE(6,*)'ISD,I+NBIN1(ISD)',ISD,I+NBIN1(ISD)
cs          WRITE(*,*) RADIUS1(I),SD(I),I,IL,' I,IL'
cs          ENDIF
				ENDDO
				RREAL=AP1(ISTAR(IREFLOC(IL,ISD,1))+IW-1)
CD          RIMAG=-AP1(ISTAR(IREFLOC(IL,ISD,2))+IW-1)
				RIMAG=AP1(ISTAR(IREFLOC(IL,ISD,2))+IW-1)
C************for forcing********************************
				RREAL1(IW,IL,ISD)=RREAL
				RIMAG1(IW,IL,ISD)=RIMAG
C*******************************************************
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C In order to use forward Mie calculations instead of using 
C kernels the following block should replace use of PHASEM1
C~          IF(IMSC.EQ.1) KSIM=-101
C~           IF(IMSC.EQ.0) KSIM=-501
C~       CALL PHASEM0(IANGL,ANGL,NBIN,KSIM,3,RADIUS1(1),
C~      & RADIUS1(NBIN(ISD)),WAVE(IW),RREAL,RIMAG,RADIUS1,SD,1,     
C~      & CM1,SM,RMM,NMD,ISD,EXT1,SSA1,PTP11,PTP12,PTP22,PTP33,PTP34,
C~      & PTP44,1,RATN)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C for using smart but non-efficient kernels use the follwing
C
CD      CALL PHASEM1(1,1,NBIN(ISD),RADIUS1,SD,WAVE(IW),
CD     & RREAL,RIMAG,IANGL,ANGL,SSA1,EXT1,PTP11,
CD     & PTP12,PTP22,PTP33,PTP34,PTP44,1,RATN)
cs        IEL=4
cs        write(6,*)'IEL',IEL
c        IEL=6
				IF(NSHAPE.GT.0) THEN
					DO IBB=1,NSHAPE
						SHAPE(IBB)=AP1(ISTAR(ISHAPELOC(1))+IBB-1)
cs        IF(SHAPE(IBB).GT.1)SHAPE(IBB)=0.999
cs       WRITE(*,*) IW, SHAPE(IBB),' IW, SHAPE'
					ENDDO
					SHAPE(NSHAPE+1)=APIN
cs        SHAPE(NSHAPE+1)=1.-SHAPE(NSHAPE)
				ENDIF
cs       write(6,*)'SHAPE',SHAPE
cs        IF(IMSC.EQ.0)THEN
cs        WRITE(*,*)'WAVE(IW)',WAVE(IW)
cs        WRITE(*,*)'RADIUS1',RADIUS1
cs        ENDIF
c			    write(250,*)'apin=',apin
				CALL  PHASE_KERNL(ISD,IEL,NBIN(ISD),RADIUS1,SD,SHAPE,NSHAPE,
     &			WAVE(IW),RREAL,RIMAG,IANGL,ANGL,SSA1,EXT1,PTP11,PTP12,
     &			PTP22,PTP33,PTP34,PTP44,1,RATN,IS(ISD))
C				write(*,*)'cxxa,ext1,ssa1',ext1,ssa1 
c				write(250,*)'cxxa,ptp11(1)',ptp11(1)
cs      write(6,*)'EXT1,SSA1',EXT1,SSA1
cs      write(6,*)'PTP12',PTP12
cs       IF(IC.EQ.1)THEN
cs       ENDIF
cs        write(*,*) RREAL,RIMAG,' RIMAG, RIMAG'
				AP1(ISTAR(IREFLOC(IL,ISD,1))+IW-1)=RREAL
				AP1(ISTAR(IREFLOC(IL,ISD,2))+IW-1)=RIMAG
CD       WRITE(*,*) IANGL,' INAGL, after phasem1'  
CD         WRITE(*,*) SSA1,EXT1,'SSA1,EXT1'     
!tl         DO IAN=1,IANGL
CD          WRITE(*,*) IAN,ANGL(IAN), PTP11(IAN) 
				IF(IEL.GT.0) PF(1:IANGL,IW,IL,ISD)=PTP11(1:IANGL) 
CS************************VEC***CODE***ADD*************
				PIN=4.0*3.1415926
				IF(IEL.GT.0) PFV(1:IANGL,ISD)=PTP11(1:IANGL)/SSA1/EXT1 
CS*****************************************************
				IF(IEL.GT.1) PF12(1:IANGL,IW,IL,ISD)=PTP12(1:IANGL)
				IF(IEL.GT.2) PF22(1:IANGL,IW,IL,ISD)=PTP22(1:IANGL)
				IF(IEL.GT.3) PF33(1:IANGL,IW,IL,ISD)=PTP33(1:IANGL)
				IF(IEL.GT.4) PF34(1:IANGL,IW,IL,ISD)=PTP34(1:IANGL)
				IF(IEL.GT.5) PF44(1:IANGL,IW,IL,ISD)=PTP44(1:IANGL)
CS************************VEC***CODE***ADD*************
				IF(IEL.GT.1) PFV12(1:IANGL,ISD)=PTP12(1:IANGL)/SSA1/EXT1
				IF(IEL.GT.2) PFV22(1:IANGL,ISD)=PTP22(1:IANGL)/SSA1/EXT1
				IF(IEL.GT.3) PFV33(1:IANGL,ISD)=PTP33(1:IANGL)/SSA1/EXT1
				IF(IEL.GT.4) PFV34(1:IANGL,ISD)=PTP34(1:IANGL)/SSA1/EXT1
				IF(IEL.GT.5) PFV44(1:IANGL,ISD)=PTP44(1:IANGL)/SSA1/EXT1
CS*****************************************************

!tl         ENDDO
CS************************VEC***CODE***ADD*************
				SSAV(ISD)=SSA1
				EXTV(ISD)=EXT1
CS          WRITE(*,*)ISD,SSA1,EXT1,SSAV(ISD),EXTV(ISD)
CS*****************************************************

CD        WRITE(*,*) EXT1,NLAY,' EXT1,NLAY, before'
				IF(EXT1.GT.10./NLAY) THEN
					AEXT=EXT1
					EXT1=EXT1/AEXT*10./NLAY
					DO I=1,NBIN(ISD)
						AP1(ISTAR(ISDLOC(IL,ISD))+I-1)=
     &					AP1(ISTAR(ISDLOC(IL,ISD))+I-1)/AEXT*10./NLAY
					ENDDO
				ENDIF
				SSA(IW,IL,ISD)=SSA1
				EXT(IW,IL,ISD)=EXT1
CD          WRITE(*,*) SSA1,EXT1,' SSA1,EXT1'
CD          WRITE(*,*) SSA(IW,IL,ISD),EXT(IW,IL,ISD),IL,EXT1,SSA1,
CD     &' SSA(IW,IL,ISD),EXT(IW,IL,ISD),IL'
			ENDDO
		ENDDO
C*****************************************************************
C* Summarizing the optical properties of all aerosol 
C* components (ISD=1,NSD) and determining SINGLE scattering properties
C* for each atmospheric layer:
C******************************************************************
		DO IL=1,NLAY
			ASUM=0.
			DO ISD=1,NSD
				ASUM=ASUM+EXT(IW,IL,ISD)
CD          WRITE(77,*) ASUM,EXT(IW,IL,ISD),' ASUM, EXT...'
			ENDDO
			EXTL(IL)=ASUM
CD        WRITE(77,*) EXTL(IL),' EXTL(IL), 1'
			ASUM=0.
			DO ISD=1,NSD
				ASUM=ASUM+SSA(IW,IL,ISD)*EXT(IW,IL,ISD)
			ENDDO
CD        WRITE(77,*) EXTL(IL),' EXTL(IL), 2'
			ASUM=ASUM/EXTL(IL)
			SSAL(IL)=ASUM
CD        WRITE(*,*) EXTL(IL), SSAL(IL), 'EXT, SSAL'
			DO ISD=1,NSD
!tl        DO IAN=1,IANGL
				IF(ISD.EQ.1) THEN
CD          PFL(IAN,IL)=PF(IAN,IW,IL,ISD)*SSA(IW,IL,ISD)*EXT(IW,IL,ISD)
					IF(IEL.GT.0) PFL(1:IANGL,IL)=PF(1:IANGL,IW,IL,ISD)
cs*********output***correction***************************************
					DLP(1:IANGL,IW,IL,ISD)=PF12(1:IANGL,IW,IL,ISD)/
     &				PF(1:IANGL,IW,IL,ISD)
					IF(IEL.GT.0) PF(1:IANGL,IW,IL,ISD)=PF(1:IANGL,IW,IL,ISD)/
     &				(SSA(IW,IL,ISD)*EXT(IW,IL,ISD))
cs***********************************************************************
					IF(IEL.GT.1) PFL12(1:IANGL,IL)=PF12(1:IANGL,IW,IL,ISD)
					IF(IEL.GT.2) PFL22(1:IANGL,IL)=PF22(1:IANGL,IW,IL,ISD)
					IF(IEL.GT.3) PFL33(1:IANGL,IL)=PF33(1:IANGL,IW,IL,ISD)
					IF(IEL.GT.4) PFL34(1:IANGL,IL)=PF34(1:IANGL,IW,IL,ISD)
					IF(IEL.GT.5) PFL44(1:IANGL,IL)=PF44(1:IANGL,IW,IL,ISD)
				ELSE
CD          PFL(IAN,IL)=PFL(IAN,IL)+PF(IAN,IW,IL,ISD)
CD     &*SSA(IW,IL,ISD)*EXT(IW,IL,ISD)
					IF(IEL.GT.0) PFL(1:IANGL,IL)=PFL(1:IANGL,IL)+
     &                                    PF(1:IANGL,IW,IL,ISD)
					IF(IEL.GT.1) PFL12(1:IANGL,IL)=PFL12(1:IANGL,IL)+
     &                                       PF12(1:IANGL,IW,IL,ISD)
					IF(IEL.GT.2) PFL22(1:IANGL,IL)=PFL22(1:IANGL,IL)+
     &                                       PF22(1:IANGL,IW,IL,ISD)
					IF(IEL.GT.3) PFL33(1:IANGL,IL)=PFL33(1:IANGL,IL)+
     &                                       PF33(1:IANGL,IW,IL,ISD)
					IF(IEL.GT.4) PFL34(1:IANGL,IL)=PFL34(1:IANGL,IL)+
     &                                       PF34(1:IANGL,IW,IL,ISD)
					IF(IEL.GT.5) PFL44(1:IANGL,IL)=PFL44(1:IANGL,IL)+
     &                                       PF44(1:IANGL,IW,IL,ISD)
				ENDIF
!tl        ENDDO 
cs        IF(IC.EQ.1)THEN
cs        WRITE(77,*) WAVE(IW),IW,' WAVE(IW), IW'
cs        WRITE(*,*) SSAL(IL),EXTL(IL),' SSAL, EXTL' 
cs        ENDIF
				IF(IEL.GT.0)    PFL(1:IANGL,IL)=PFL(1:IANGL,IL)/
     &                                      (SSAL(IL)*EXTL(IL))
				IF(IEL.GT.1)  PFL12(1:IANGL,IL)=PFL12(1:IANGL,IL)/
     &                                      (SSAL(IL)*EXTL(IL))
				IF(IEL.GT.2)  PFL22(1:IANGL,IL)=PFL22(1:IANGL,IL)/
     &                                      (SSAL(IL)*EXTL(IL))
				IF(IEL.GT.3)  PFL33(1:IANGL,IL)=PFL33(1:IANGL,IL)/
     &                                      (SSAL(IL)*EXTL(IL))
				IF(IEL.GT.4)  PFL34(1:IANGL,IL)=PFL34(1:IANGL,IL)/
     &                                      (SSAL(IL)*EXTL(IL))
				IF(IEL.GT.5)  PFL44(1:IANGL,IL)=PFL44(1:IANGL,IL)/
     &                                      (SSAL(IL)*EXTL(IL))
CD         WRITE(*,*) PFL(IAN,IL),' PFL(IAN,IL)' 
CD         WRITE(*,*) ANGL(IAN),PFL(IAN,IL),SSAL(IL),EXTL(IL),IL, 
CD     &'ANGL(IAN),PFL(IAN,IL),SSAL(IL),EXTL(IL),IL'
			ENDDO
		ENDDO 
CD       DO I=1,KN
CD       WRITE(77,*) AP1(I),I
CD       ENDDO
		IF(NBRDF.GT.0) THEN
			DO IB=1,NBRDF
				BRF(IB)=AP1(ISTAR(IBRDFLOC(IB))+IW-1)
CD       WRITE(*,*)IW,ISTAR(IBRDFLOC(IB))+IW-1, BRF(IB),'IW,Il, BRF'
CD       WRITE(*,*) EXP(AP(ISTAR(IBRDFLOC(IB))+IW-1)),'AP' 
			ENDDO
		ENDIF
C*************Correction*for*BRF *at*1020*******************************
cs        IF(IW.EQ.3)THEN
cs        DO IB=1,NBRDF
cs        XBRF(IB)=BRF(IB)
cs        ENDDO
cs        ENDIF
cs        IF(IW.EQ.4)THEN
cs        DO IB=1,NBRDF
cs        BRF(IB)=XBRF(IB)
cs        ENDDO
cs        ENDIF
c***********************************************************************
		IF(NBRDF1.GT.0) THEN
			DO IB=1,NBRDF1
				BRF1(IB)=AP1(ISTAR(IBRDF1LOC(IB)))
CD       WRITE(*,*) IW, BRF(IB),' IW, BRF'
			ENDDO
		ENDIF
C****************************************************************
C* Computing DIFUSE radiation with accounting for
C*  MULTIPLE SCATTERING effects                   :
C**************************************************************** 
CD       WRITE(*,*) IANGL,' IANGL, before ' 
cs      write(6,*)'BEFORE DISDS'
C		IF(NDP.EQ.0.AND.IW.EQ.1)THEN
C		WRITE(250,*)PFL(1:83,1),PFV(1:83,1)
C		WRITE(250,*)SSAV(1),EXTV(1),SSAL,EXTL
C		ENDIF
		IF(IMSC.GE.0) CALL DISDS(IPRI,IMSC,NSD,IW,IANGL,ANGL
     &	,PFL,PFL12,PFL22,PFL33,PFL34,PFL44,SSAV,EXTV,
     &	PFV,PFV12,PFV22,PFV33,PFV34,PFV44
     &	,SSAL,DBLE(EXTL),NBRDF,NBRDF1,BRF,BRF1,ISKY,FSKY,INDB,
     &	FLFDOWN,FLFUP,FLFDIR,IPOLPR,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,
     &	ITOFF,IBRF,IRFS)
		FLDD(IW)=FLFDOWN
		FLUP(IW)=FLFUP
		FLDR(IW)=FLFDIR

cs      write(6,*)'IW,FLDD',IW,FLDD(IW)
cs      write(6,*)'IW,FLUP',IW,FLUP(IW)
cs      write(6,*)'IW,FLDR',IW,FLDR(IW)
cs      write(6,*)
		IF(NBRDF.GT.0) THEN
			DO IB=1,NBRDF
				AP1(ISTAR(IBRDFLOC(IB))+IW-1)=BRF(IB)
CD       WRITE(*,*) BRF(IB),' BRF'
			ENDDO
		ENDIF
cs      write(6,*)'before BRF1'
		IF(NBRDF1.GT.0) THEN
			DO IBB=1,NBRDF1
				AP1(ISTAR(IBRDF1LOC(1))+IBB-1)=BRF1(IBB)
CD       WRITE(*,*) BRF1(IB),' BRF1'
			ENDDO
		ENDIF
C***********************************CORRECTION*FOR*SHAPE*******************
		IF(NSHAPE.GT.0) THEN
			DO IBB=1,NSHAPE
				AP1(ISTAR(ISHAPELOC(1))+IBB-1)=SHAPE(IBB)
CD       WRITE(*,*) BRF1(IB),' BRF1'
			ENDDO
		ENDIF
C**************************************************************************
cs       WRITE(*,*) ISKY, 'ISKY AFTER DISDS'
		DO ISK=1,ISKY
			FP(III+ISK)=FSKY(ISK)
c			write(250,*)fp(iii+isk)
CD       WRITE(*,*) FP(III+ISK), III+ISK,'III+ISK'
		ENDDO
		III=III+ISKY
cs       write(6,*)'III,ISKY',III,ISKY
      ENDDO
    
C*******************Radiative Forcing***********************************
C PARAMETERS
C MAXCMU: MAXIMAL NUMBER OF MOMENTS
C NBNU  : MAXIMAL NUMBER OF SPECTRAL INTERVALS
C MAXULV: NUMBER OF LEVELS = 10
C         7 LEVELS: FROM THE SURFACE TO 9 KM
C         3 FIXED LEVELS AT 10 KM, 15 KM AND TOA 
C         NB: TOTAL OZONE CONTENT IS BETWEEN 15 KM AND TOA
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C BROAD: =0 CALCULATION OF THE BROADBAND FLUXES
C	 >0 NO CALCULATE
C FORC:   = 0 CALCULATION OF THE RADIATIVE FORCING
C	 > 0 NO CALCULATION
c SPEC: =0 CALCULATION OF THE SPECTRAL RADIATIVE FORCING 
C        > 0 NO CALCULATION
C COND: =0 CALCULATION THE RADIATIVE FORCING WITH THE FINAL VALUES OF
C         REFRACTIVE INDEX AND SIZE DISTRIBUTION    
c       =1 NO CALCULATE
C W1,W2: INITIAL AND FINAL WAVENUMBERS (CM-1) FOR CALCULATIONS
C	IF W1 AND W2 .EQ.-1, SPECTRAL RANGE FROM INPUT FILE (CIMEL WAVENUMBERS)
C        AVAILABLE SPECTRAL RANGE: 2500 TO 50000 CM-1 (0.2 TO 4 MICRONS)
C       THIS SPECTRAL RANGE IS DIVIDED INTO 208 INTERVALS:
C       LAMB1 AND LAMB2: INITIAL AND FINAL INTERVALS FOR CALCULATIONS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C SCL1: SCALE HEIGHT
C NSTR1: NUMBER OF COMPUTATIONAL POLAR ANGLES TO BE USED
C             (= NUMBER OF 'STREAMS')  ( SHOULD BE EVEN AND .GE. 4 )
C NSTR2: NUMBER OF LEGENDRE MOMENTS FOR THE PHASE FUNCTION
C NMOM: NUMBER OF THE MOMENTS FOR THE LEGENDRE EXPANSION
C NSSA: = -1 SPECTRAL DEPENDENCE FOR THE SINGLE SCATTERING ALBEDO
C       NEQ.-1, AVERAGE OF SINGLE SCATTERING ALBEDO FROM CIMEL WAVELENGTHS 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C THE ATMOSPHERIC PROFILE FROM INVERSION INPUT FILE
C  NH2O: NUMBER OF WATER VAPOR CONTENTS
C  NO3: NUMBER OF OZONE CONTENTS
C  NCO2: NUMBER OF CARBON DIOXIDE CONTENTS
C  NUMU1: NUMBER OF POLAR ANGLE COSINE OF INCIDENT BEAM
C  UH2O : WATER VAPOR CONTENT (G.CM-2) 
C	 = -1 FROM INVERSION INPUT FILE
C  UO3CM: OZONE CONTENT (CM.ATM) 
C        = -1 FROM INVERSION INPUT FILE
C  UCO2: CARBON DIOXIDE CONTENT (ppmV)	
C        = -1 FROM INVERSION INPUT FILE
C  ZSUR: SURFACE ALTITUDE (KM)
C        = -1 FROM INVERSION INPUT FILE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C SOLAR PARAMETERS
C UMU1   : POLAR ANGLE COSINE OF INCIDENT BEAM (POSITIVE)
C        IF UMU1.EQ.-1 POLAR ANGLE COSINE FROM INVERSION INPUT FILE
C DSOL: CORRECTION FOR DISTANCE EARTH-SUN
C	IF DSOL.EQ.-1 JDAY AND MONTH FROM INVERSION INPUT FILE
C	IF DSOL.NE.-1, NO CORRECTION (DSUN=1)  	
C CSOL: SOLAR CONSTANT (W M-2) (-1 = DEFAULT VALUE:1361.1 W/m2)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C  SPECTRAL SURFACE ALBEDO (LAMBERTIAN) FROM MODIS
C  W2-LAST MODIS WAVENUMBER CM-1 (WAVELENGTH 0.47 um) EXTRAPOLATION
C  LAST-FIRST MODIS WAVENUMBER CM-1 (WAVELENGTH 0.47-2.13 um) INTERPOLATION 
C  FIRST MODIS WAVENUMBER - W1: MODIS ALBEDO AT 2.13 um 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C OUTPUT VARIABLES:
C  FLXINT: INTEGRATED IRRADIANCES (W.M-2)
C  FLXINT(L,1),FLXINT(L,2): DOWNWARD AND UPWARD IRRADIANCES
C   	 AT LEVEL L (SURFACE: L=1 AND TOA (120 KM): L=NLEVEL)
C FORC_BOT,FORC_TOA: RADIATIVE FORCING (W.M-2) AT SURFACE AND TOA
C EF_BOT,EF_TOA: RADIATIVE FORCING EFFIENCY (WM-2/AOD(0.55 um) AT SURFACE AND TOA
C FS_BOT,FS_TOA: SPECTRAL RADIATIVE FORCING (WM-2) AT SURFACE AND TOA
C ES_BOT,ES_TOA: SPECTRAL RADIATIVE FORCING EFFICIENCY (WM-2/AOD(0.55 um)) AT SURFACE AND TOA
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	IF (COND.EQ.0) THEN
		OPEN(800, FILE='INPUT_BB')
		READ(800,*)BROAD,FORC,SPEC
		READ(800,*)W1,W2
		READ(800,*)SCL1,NSTR1,NSTR2,NMOM,NSSA
		READ(800,*)NH2O,NO3,NCO2,NUMU1,ZSUR
     		READ(800,*)(UH2O1(I),I=1,NH2O)
		READ(800,*)(UO31(I),I=1,NO3)
		READ(800,*)(UCO21(I),I=1,NCO2)
 		READ(800,*)(UMU2(I),I=1,NUMU1)
 		READ(800,*)CSOL,DSOL
		READ(800,*)PP
		CLOSE(800)
		IF (BROAD.EQ.0) THEN
!tl          WRITE(80,*)
   
 64			FORMAT('Total pre Fluxes........... ',f8.5,' min.')

	  	   
			OPEN(902,FILE='SPEC_BB.dat') !SSA,AOD,G,...	
			OPEN(901,FILE='FLX_BB.dat') !BROADBAND FLUXES
			WRITE(901,*) 'RADIUS (mkm) SIZE DISTRIBUTION (mkm3/mk2)'
cs	  WRITE(*,*)'Calculating the broadband fluxes....'
	   
			DO IL=1,NLAY
				DO ISD=1,NSD
					DO I=1,NBIN(ISD)
						WRITE(901,*)RADIUS2(ISD,IL,I),SD2(ISD,IL,I)
					ENDDO
				ENDDO
			ENDDO	
			WRITE(901,*)
			WRITE(901,*)'WAVELENGTH(mkm) REAL & IMAG PART OF REF.INDEX'
	  
			DO IL=1,NLAY
				DO ISD=1,NSD
					DO IW=1,NW	
						WRITE(901,*)
     1					WAVE(IW),RREAL1(IW,IL,ISD),RIMAG1(IW,IL,ISD)
					ENDDO
				ENDDO
			ENDDO
			WRITE(901,*)
			WRITE(901,*)'SCL NSTR NSTR2 NMOM:',SCL1,NSTR1,NSTR2,NMOM  
	
			IF(W1.EQ.-1.AND.W2.EQ.-1)THEN
				W1=1.E+04/WAVE(NW)
				W2=1.E+04/WAVE(1)
			ENDIF	
C     DEFINITION OF LAMB1 AND LAMB2 
			IF(W1.LT.14400)THEN
				LAMB1=NINT((W1-2500.)/100.)+1
			ELSE
				LAMB1=NINT((W1-14400.)/400.)+120
			ENDIF
			IF(W2.LT.14400)THEN
				LAMB2=NINT((W2-2600.)/100.)+1
			ELSE
				LAMB2=NINT((W2-14800.)/400.)+120
			ENDIF
			NBNU1=(LAMB2-LAMB1)+1
			WRITE(901,*)'W1 W2 LAMB1 LAMB2:',W1,W2,LAMB1,LAMB2	     
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C RADTRS : COMPUTE FLUXES
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			DO I=1,NH2O
				UH2O=UH2O1(I)
				DO J=1,NO3
					UO3CM=UO31(J)
					DO K=1,NCO2
						UCO2=UCO21(K)
						DO KK=1,NUMU1
							UMU1=UMU2(KK)
							WRITE(902,*)
     1						'WL(um) SSA AOD ASYM FACTOR SURF_ALB RREAL RIMAG'  
							NOAERO=0 !WITH AEROSOLS
							CALL RADTRS(NMOM,NSTR1,NSTR2,PP,ANG,IANGL
     &						,RADIUS2,SD2,SHAPE, NSHAPE,NSD,NBIN,
     $						NW,WAVE,LAMB1,LAMB2,UMU1,DSOL,SSA,NSSA, CSOL,UH2O,            
     $						UO3CM,UCO2,ZSUR,NLAY,NLEVEL,RREAL1,RIMAG1,
     $						NOAERO,SCL1,ALTIT,FLXINTA,TOTAER0,FLXNU0,CORSOL)


 							WRITE(901,*)'ALTIT  FLXDWN   FLXUP'
 							WRITE(901,*)' (km)  (W/m2)   (W/m2)'

					        DO L=1,NLEVEL
								WRITE(901,999)
     1								ALTIT(L),FLXINTA(L,1),FLXINTA(L,2)
							ENDDO

							FLUX_UP_TOA_AERO=FLXINTA(NLEVEL,2)
							FLUX_UP_BOT_AERO=FLXINTA(1,2)
							FLUX_DOWN_TOA_AERO=FLXINTA(NLEVEL,1)
							FLUX_DOWN_BOT_AERO=FLXINTA(1,1)

!tl       WRITE(80,*)
   
 66						FORMAT('Fluxes with aerosols....... ',f8.3,' min.')
	
							IF (FORC.EQ.0) THEN
								NOAERO=1 !WITHOUT AEROSOLS 
						CALL RADTRS(NMOM,NSTR1,NSTR2,PP,ANG,IANGL,RADIUS2,SD2,
     &					SHAPE, NSHAPE,
     $					NSD,NBIN,NW,WAVE,LAMB1,LAMB2,UMU1,DSOL,SSA,NSSA,          
     $					CSOL,UH2O,UO3CM,UCO2,ZSUR,NLAY,NLEVEL,RREAL1,RIMAG1,
     $					NOAERO,SCL1,ALTIT,FLXINT,TOTAER,FLXNU1,CORSOL)


								WRITE(901,*)'ALTIT  FLXDWN   FLXUP'
								WRITE(901,*)' (km)  (W/m2)   (W/m2)'
       
								DO L=1,NLEVEL
   						WRITE(901,999)ALTIT(L),FLXINT(L,1),FLXINT(L,2)
								ENDDO

								FLUX_UP_TOA=FLXINT(NLEVEL,2)
								FLUX_UP_BOT=FLXINT(1,2)
								FLUX_DOWN_TOA=FLXINT(NLEVEL,1)
								FLUX_DOWN_BOT=FLXINT(1,1)

65						FORMAT('Fluxes without aerosols...... ',f8.3,' min.')


C----------------RADIATIVE FORCING----------------------

cs	FORC_BOT=((FLUX_DOWN_BOT-FLUX_UP_BOT)-
cs     &(FLUX_DOWN_BOT_AERO-FLUX_UP_BOT_AERO))
cs	FORC_TOA=((FLUX_DOWN_TOA-FLUX_UP_TOA)-
cs     &(FLUX_DOWN_TOA_AERO-FLUX_UP_TOA_AERO))
								FORC_BOT=(FLUX_DOWN_BOT-FLUX_DOWN_BOT_AERO)
								FORC_TOA=(FLUX_UP_TOA-FLUX_UP_TOA_AERO)
C-------------RADIATIVE FORCING EFFCIENCY ----------------

								EF_BOT=FORC_BOT/TOTAER0  	
								EF_TOA=FORC_TOA/TOTAER0
c--------------------------------------------------------
								WRITE(901,*)
				WRITE(901,*)'ALTIT RADIAT FORCING FORCING EFFICIENCY'
				WRITE(901,*)' (km)   (W/m2)        (Wm-2/AOD(0.55))'
				WRITE(901,999) ALTIT(1), FORC_BOT,EF_BOT
				WRITE(901,999) ALTIT(NLEVEL), FORC_TOA, EF_TOA
	
								IF(SPEC.EQ.0) THEN 
									WRITE(901,*)
				WRITE(901,*)'WAVELENGTH FORC_BOT  FORC_TOA EFF_BOT  EFF_BOT'
				WRITE(901,*)'   (mkm)        (W/m2)        (Wm-2/AOD(0.55))'
									N=NLEVEL
									DO NU=1,NBNU1
										IF(NU.GT.119)THEN
											ANU=14600+400*(NU-120)
      									ELSE 
											ANU=2550+100*(NU-1)
										ENDIF
       									WVL=1.0E+04/ANU
				FS_BOT=CORSOL*((FLXNU1(1,NU,1)-FLXNU1(1,NU,2))-
     &			(FLXNU0(1,NU,1)-FLXNU0(1,NU,2)))
				FS_TOA=CORSOL*((FLXNU1(N,NU,1)-FLXNU1(N,NU,2))-
     &			(FLXNU0(N,NU,1)-FLXNU0(N,NU,2)))
										ES_BOT=FS_BOT/TOTAER0
										ES_TOA=FS_TOA/TOTAER0
				WRITE(901,998)WVL,FS_BOT,FS_TOA,ES_BOT,ES_TOA
									ENDDO
     								ENDIF  !SPEC
							ENDIF !FORC
999							FORMAT(F7.3,2(2X,F10.5))
998							FORMAT(F8.5,4(2X,F10.5))	

	
cs	WRITE(6,*) 
cs     &        '------------------ T I M I N G ------------------' 
cs	WRITE(6,61) TIME 
cs        WRITE(6,*)
   
 61							FORMAT('Total&Broadband ......... ',f8.2,' min.')

						ENDDO !NUMU1
					ENDDO !NCO2
				ENDDO !NO3
			ENDDO !NH2O
      		CLOSE(901)	
			CLOSE(902)
		ENDIF !BROAD
	ENDIF  !COND
        
C********************************************************************

      IF(III.NE.KM) WRITE(*,*) III,KM,'III,KM,- WRONG ISKY !!!'
      IF(IMSC.EQ.-1) WRITE(*,*) "PHASE FUNCTION is INVERTED-CHECK!!!"
      IF(KL.EQ.1) THEN
		DO J=1,KM
			IF(FP(J).GT.0.) THEN
				FP(J)=LOG(FP(J))
			ELSE
				FP(J)=LOG(1.0E-9)
			ENDIF
		ENDDO
      ENDIF
      DO I=1,KN
		IF(KL.EQ.1) THEN
			IF(AP1(I).LT.1.0E-30)AP1(I)=1.0E-30
			AP(I)=LOG(AP1(I))
		ENDIF
		IF(KL.EQ.0) THEN
			AP(I)=AP1(I)
		ENDIF
      ENDDO
c	write(*,*)'end of forward'
cs      write(6,*)'END OF FORWARD'
      RETURN
      END


      SUBROUTINE PHASE_KERNL(ISD,IEL,NBIN,RADIUS,SD1,RD1,NSHAPE,
     & WL,RN,RK,IANGL,ANG,SSA,EXT1,PTP11,PTP12,
     & PTP22,PTP33,PTP34,PTP44,keys,RATN,IS1)
C*****************************************************
C***This subroutine calculates the 
C    EXTINCTION, SSA, PHASE FUNCTION using kernel matrices                     
C
C   INPUT:
C*   ISD    I  - number of size distribution component
C*   IEL    I  - number of needed ellements of phase matrix
C*            =1 - P11 only
C*            =6 - P11, P12, P22, P33, P34, P44 
C*   NBIN   I  - the number of the bins in the
C*                       size distributions
C*   RADIUS R(KBIN) - the radii corresponding to  
C*                the bins in the size distributions
C*               size distributions
C*   SD     D(NBIN) - size distribution
C*                    Usualy dV/dlnR (it can be also for 
C*                    radii, numbers, areas,
C*                    but should agree with KERNEL)
C*   WAV    R       - wavelength
C*   RREAL  R       - real part of the refractive index
C*   RIMAG  R       - imaginary part of the refractive index
C***************************************************** 
C*  IANGL        - the number of angles used for 
C*                   phase function
C*   ANG    R(IANG) - the values of the angles for the phase 
C*                        function modelling;
C*   SSA    R       - single scattering albedo
C*   EXT    R       - extinction
C*   PTPii     R(IANG)- phase matrix (UNNORMALIZED !!!)
C*****************************************************
      use mo_par_DLS
      PARAMETER (KMES=181,KPAR=100,KPARS=100,KEL=2,KSD=3)
      PARAMETER (KREF=15,KBRDF=13,KSHAPE=2)
      DIMENSION RADIUS(KPAR),SD1(KPAR)
      DIMENSION ANG(KMES)
       DIMENSION
     & PTP11(KMES),PTP12(KMES),PTP22(KMES),
     & PTP33(KMES),PTP34(KMES),PTP44(KMES)
C***************************************************************** c
C***   Definition of the parameters for OPTCHAR1               *** c
C***************************************************************** c
c **************************************************************** c
c **   02/28/03        PROGRAM OPTCHAR1                         ** c
c **   Program calculates optical characteristics for given     ** c
c **   size distribution, refractive index, aspect ratio        ** c
c **   distribution and wavelength                              ** c
c **************************************************************** c
c **                                                            ** c
c ** INPUT:                                                     ** c
c **                                                            ** c 
c **   key  = 1 - create fixed kernel matricies                 ** c
c **          2 - create fixed kernel matricies and save them   ** c
c **              into Rke...fix... files                       ** c
c **          3 - read fixed kernel matricies from              ** c
c **              Rke...fix...  files                           ** c
c **   key_RD =1 - volume mixture of spheroids                  ** c
c **           2 - sarea  mixture of spheroids                  ** c
c **   key_f11=0 - calculate scattering matrix                  ** c
c **           1 - calculate only phase function                ** c
c **   key_f344=0 - don't calculate f34 and f44                 ** c
c **            1 -       calculate f34 and f44                 ** c
c **   key_org=0 - read original matrices simultaniously make   ** c 
c **               angle interpolation and calculate opt.char.  ** c
c **           1 -  -"-, save new matrices in                   ** c
c **                /NEWORG/ directory, STOP                    ** c
c **           2 -  read new matrices and calculate opt.char.   ** c
c **   WL   - wavelength                                        ** c
c **   RN   - real part of the refractive index                 ** c
c **   RK   - imaginary part of the refractive index            ** c
c **   KN   - a number of grid radii                            ** c
c **   grid(KN) - grid radii                                    ** c
c **   SD(KN)   - size distribution for grid radii              ** c
c **   KR  - a number of aspect ratios                          ** c
c **   R(KR)  - grid aspect ratios                              ** c
c **   RD(KR) - aspect ratio distribution for grid aspect ratios** c
c **   KM   - a number of scattering angles                     ** c
c **   ANGLE(KM) - scattering angles                            ** c                         
c **************************************************************** c

!      include 'optchar.par'
      integer KN, KM, KR,NDP 
	integer key, key_RD, keyEL, keySUB, keyLS, key_org,key_fx,
     &       key_grid1,key_RD1

      real WL, RN, RK

      dimension grid(KNpar), SD(KNpar),RD1(KSHAPE)
     &            ,R(KRpar), RD(KRpar), ANGLE(KMpar)
      dimension f11(KMpar) 
     &         ,f12(KMpar)
     &         ,f22(KMpar)
     &         ,f33(KMpar)
     &         ,f34(KMpar)
     &         ,f44(KMpar)
      real ext, albedo
	  dimension CM(KMD),SM(KMD),RMM(KMD),
     &          RRR(KNpar),AR(KNpar)
      CHARACTER(60) distname_O,distname_F,distname_N
!==================== BY DUAN ============================
	COMMON /ILP4/ II  !cxxa
      REAL AC(2)
!==================END BY DUAN ============================

      KN=NBIN
      DO I=1,KN
		grid(I)=RADIUS(I)
		SD(I)=SD1(I)
cs      WRITE(*,*) grid(I),SD(I)
      ENDDO
      KR=NSHAPE+1
cs      KR=2
      DO i1=1,KR
		RD(i1)=RD1(i1)
cs      write(6,*)'RD1 IN PHASE_K',RD1(i1)
      ENDDO
cs      KR=16
cs      RD(1)=RD(1)*2.
cs      RD(2)=RD(2)*2.
cs      ss=0.
cs      do i1=1,KR
cs      ss=ss+RD(i1)
cs      enddo
cs      do i1=1,KR
cs      RD(i1)=RD(i1)/ss
cs      enddo
cs      RD(11)=RD(6)
cs      RD(12)=RD(5)
cs      RD(13)=RD(4)
cs      RD(14)=RD(3)
cs      RD(15)=RD(2)
cs      RD(16)=RD(1)
C**** ATTENSION!!! The following paramteres have not been used
C **    in curent version of the program                        ** c
CD?       KR=1.
C**      KR=8.
c **   R(KR)  - grid aspect ratios                              ** c
c **   RD(KR) - aspect ratio distribution for grid aspect ratios** c
C 
c
C	WRITE(*,*)'II=',II
	IF(II.EQ.0)THEN
		NDP=0
c
c ** READ INPUT
c       
	    open(10,file='input.dat',status='old')
		read(10,*) key,keyEL,keySUB,keyLS,
     &           key_org,key_fx,key_RD1
		if(keySUB.eq.0)  
     &	write(*,*) key,keyEL,keySUB,keyLS,
     &                    key_org,key_fx,key_RD1
c **   key_RD =1 - volume mixture of spheroids                  ** c
c **           2 - surface area  mixture of spheroids           ** c
		key_RD=1

c      read(10,*) WL,RN,RK,rgmin,rgmax,wlmin,wlmax
		READ(10,*)
CD	  	  if(keySUB.eq.0) write(*,*) WL,RN,RK,rgmin,rgmax,wlmin,wlmax
		read(10,*) key_SD,ID,NMD
		if(key_SD.eq.0) then
			read(10,*) 
			read(10,*) KN2
CD      write(*,*) 'KN=',KN
			do i=1,KN2
CD      read(10,*) grid(i), SD(i)
				read(10,*) 
			enddo ! i
		else ! key_SD=1
			read(10,*) (CM(i),SM(i),RMM(i),i=1,NMD)
			read(10,*) KN2
			if(keySUB.eq.0) write(*,*) 'KN=',KN2
			do i=1,KN2
				read(10,*) grid(i)
			enddo ! i
		endif ! key_SD
CD      write(*,*) 'after grid'
		read(10,*) distname_O
		write(1011,*) 'distname_O=',TRIM(distname_O)
		read(10,*)
!tl	  read(10,*) distname_F
!	  write(*,*) 'distname_F=',TRIM(distname_F)
		read(10,*)
!tl	  read(10,*) distname_N
!	  write(*,*) 'distname_N=',TRIM(distname_N)
		read(10,*) KR2
		do i=1,KR2
CD      read(10,*) R(i), RD(i)
CD???      read(10,*) 
			read(10,*) R(i)
		enddo ! i
		read(10,*) KM
		if(KM.gt.KMpar) then
			write(*,*) 'in input.dat KM=',KM,' .gt. ',
     &		'Kmpar=',KMpar,' in mo_par_DLS.f90'
			STOP 'STOP: KM should be < or = KMpar'
		endif ! KM&KMpar
		do j=1,KM
			read(10,*) ANGLE(j)
		enddo ! j
		close(10)
		if(key.eq.4.and.key_org.eq.1) then
			write(*,*) ' if key=4, key_org=',key_org,
     &		 ' should be 0.'
			stop 'STOP in OPTCHAR1 (matrix_optchr_ls.f)'
		endif 
		if(key_org.eq.1.and.key_fx.eq.1) then
			write(*,*) 'STOP: key_org=1 & key_fx=1'
			write(*,*) 'If you want to use key_fx=1',
     &           ' change key_org to 0'
			write(*,*) 'If key_org=0 is your choice,',
     &           ' check dir_name1 in matrix_fixget',
     &           ' and run the code again.'
		stop 
		endif

c **   key_grid1 read grid radii and scat.angles which were used** c
c **             for kernel look up tables or fixed kernels     ** c
c **          =0 - 'grid1.dat'                                  ** c
c **           1 - 'grid1.dat.fix'                              ** c

		if(key.eq.2) then
		 key_grid1=1
		  else 
		   key_grid1=0
		endif

c
c ** CALCULATE SD if key_SD=1
c      
		if(key_SD.eq.1) then
			call SIZEDISDN(-KN,1,ID,1,NMD,CM(1:NMD),SM(1:NMD),RMM(1:NMD),
     &                      grid(1),grid(KN2),RRR,AR,AC)
CD      write(*,*) 'CM=',CM(1:NMD),' AC=',AC

			do i=1,KN
CD	write(*,113) i,grid(i),RRR(i),SD(i),AR(i) 
				SD(i)=AR(i)
			enddo ! i
		endif ! key_SD
c ** rgrid min & max calculation for fixed or NEWORG kernels
		if(key.eq.1.or.key_org.eq.1) then
			pi2=2*ACOS(-1.)
			pomin=pi2*rgmin/wlmax
			pomax=pi2*rgmax/wlmin  
		endif ! key or key_org
      ENDIF ! IF(II.EQ.0)THEN

      II=II+1
c
      KN=NBIN
	DO I=1,KN
		grid(I)=RADIUS(I)
		SD(I)=SD1(I)
cs      WRITE(*,*) grid(I),SD(I)
      ENDDO
      KR=NSHAPE+1
cs      KR=2
      DO i1=1,KR
		RD(i1)=RD1(i1)
cs      write(6,*)'RD1 IN PHASE_K',RD1(i1)
      ENDDO
c
c ** GET OPTICAL CARACTERISTICS
c
      keyEL=IEL   ! tl IEL is from inversion input file
!      write(*,*) 'R=',R
c	write(250,*)'xxa,key',key,key_RD,keySUB,key_grid1
c	write(250,*)'xxa,distname_O',distname_O
      CALL OPTCHAR(key,key_RD,keyEL,keySUB,keyLS
     &            ,key_org,key_fx,key_grid1
     &            ,WL,RN,RK,KN,grid,SD
     &            ,KR,R,RD,KM,ANGLE,ext,albedo
     &            ,f11,f12,f22,f33,f34,f44,pomin,pomax
     &            ,distname_O,distname_F,distname_N,NDP,key_RD1)
c	  write(250,'(22e15.5)')sd
c	  write(250,*)'rn,rk',rn,rk
c	  write(250,*)'xxa,ext,albedo',ext,albedo
c	  write(250,*)'xxa,f11',f11(1)
cxxa
cs       write(6,*)'ext,albedo',ext,albedo
cs       write(6,*)'f11',f11
cs      write(6,*)'after opt.char'
cl      ENDDO ! i

13    FORMAT(10E12.4)
14    FORMAT(10F8.2)
      IANGL=KM
      DO I=1,IANGL
		ANG(I)=ANGLE(I)
      ENDDO
      sca=ext*albedo

CD!!!      PTP11(I)=f11(I)*sca

	IF(keyEL.GT.0) THEN
		PTP11(1:KM)=f11(1:KM)	
		IF(keyEL.GT.1) PTP12(1:KM)=f12(1:KM)
		IF(keyEL.GT.2) PTP22(1:KM)=f22(1:KM)
		IF(keyEL.GT.3) PTP33(1:KM)=f33(1:KM)
		IF(keyEL.GT.4) PTP34(1:KM)=f34(1:KM)
		IF(keyEL.GT.5) PTP44(1:KM)=f44(1:KM)
      ENDIF ! keyEL		 
      EXT1=EXT
      SSA=albedo
c      WRITE(*,*) EXT1,albedo,' EXT,albedo'
c      WRITE(*,*) f11(1),f11(KM),' f11(1),f11(KM)'
      RETURN 
      END SUBROUTINE PHASE_KERNL

c ****************************************************************

      SUBROUTINE DISDS(IPRI,IMSC,NSD,IW,NANG,ANG,PDDI,
     &PFL12,PFL22,PFL33,PFL34,PFL44,SSAV,EXTV,
     &PFV,PFV12,PFV22,PFV33,PFV34,PFV44,
     &SSALBEDO,TAUU,
     &NBRDF,NBRDF1,BRF,BRF1,ISKY,FPP,INDB,FLFDOWN,FLFUP,FLFDIR,
     &IPOLPR,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,ITOFF,IBRF,IRFS)
C********************************************************************
C*** This subroutine is written by O. Dubovik for calculating 
C*** sun/sky radiances in radiance inversion code
C********************************************************************
C
C   INPUT:
C    IPRI  I :
C             =0 - no printing of radiances
C             =1 - print radiances
C     NSD  I    - number of p-th iteration in inversion process
C     IMSC I    -defines the order of scattering:
C             = 0 -multiple scattering
C             = 1 -sinlge scattering
C     IW   I    -number of wavelength in mkm
C     PDDI R(*,NLYR) - aerosol phase function 
C SSALBEDO R(NLYR)   - aerosol SSALBEDO
C     TAUA R(NLYR)   - total optical 
C     BRF  R(NBRDF)  - parameters of BRDF function
C********************************************************************
C*** Angle structure of sun/sky measurements
C***                               are read from input FILE
C********************************************************************
C  OUTPUT:
C ISKY  - number of sky measurements at WAV(IW)
C FSKY     FPP  R(NMES) -sky radiation
C      AAA  R       -dirrect radiation
C********************************************************************
c  Routines called :

c    DISORT:   The discrete ordinates radiative transfer program

c+---------------------------------------------------------------------+
      INTEGER  MAXCLY,MAXPHI,MAXUMU,nbmu,nbetal,nos
      parameter(NBVM=100,NMM=5,KNT=51,NG=91)
      PARAMETER ( MAXCLY=35, MAXPHI=200,MAXUMU=100,MAXULV=35)
      PARAMETER ( MAXL=36)
      PARAMETER ( NNMAX=150,nbmu=41,nbetal=80,nos=150)
c**********
!tl      PARAMETER (KW=10,KBRDF=13,KANG=83,KMES=500)
      PARAMETER (KW=10,KBRDF=13,KANG=181,KMES=500)
      PARAMETER (KPAR=100,KNA0T=3,KSHAPE=2,KSD=3)
c**********
      PARAMETER (KNANG =250)
      PARAMETER (KNA1U  =100)
      PARAMETER (KNFI  =200)
      PARAMETER (KNLN  =35)
      PARAMETER (KLGN1 =400)
      PARAMETER (KNA0  =2)
      PARAMETER (KNTAU =2)
      PARAMETER (KPLK1 =2)
      PARAMETER (NWM=10,NG0=91,KANGL=2*NG0+1)
**********
      LOGICAL RAYLI,lamber,ALM
      CHARACTER fbetal*16
      INTEGER NLYR,NSTR
      INTEGER jday,month,jyear,nn
      INTEGER N0,ISAUT
      REAL ALBEDO,FBEAM,angle
      REAL FPP(KMES),ANG(KANG),WAV(KW)
      REAL x11,x12,x22,x33,x34,x44
C*** FOR "reflec" 
        REAL gaussd
        real gg1,gg2,fr,omg1
      	real hlyr(MAXCLY+1),houtput(KW,MAXCLY+1),
     $  houtput1(MAXCLY+1),hplyr(MAXCLY+1),hpl(MAXCLY+1),
     $  hl(MAXCLY+1) 
      DIMENSION FWAVE(122), WFBEAM(122),BRF(KBRDF),BRF1(KBRDF)
      DIMENSION PDDI(KANG,MAXCLY),ANGLESCA(KW,KNA0,KMES)
     &,PFL12(KANG,MAXCLY),ANGL(KANG)
     &,PFL22(KANG,MAXCLY),PFL33(KANG,MAXCLY)
     &,PFL34(KANG,MAXCLY),PFL44(KANG,MAXCLY)
     &,SSALBEDO(MAXCLY),
     &H0(MAXCLY+1),W(MAXCLY+1),SSALBEDOS(MAXCLY),
     &PDDIS(KANG,MAXCLY),PDDISP(KANG,MAXCLY)
      DOUBLE PRECISION TAUU(MAXCLY),HTAUU(MAXCLY),
     &TAUR(KW,MAXCLY+1),TAURC(MAXCLY),TAURD(MAXCLY)
CS*********************NEW**VAR**FOR**VECTOR***CODE*****************
      DIMENSION PFV(KANG,KSD),PFV12(KANG,KSD),
     &PFV22(KANG,KSD),
     &PFV33(KANG,KSD),PFVC(KANG,KSD),PFVC12(KANG,KSD),
     &PFVC22(KANG,KSD),PFVC33(KANG,KSD)
     &,PFV34(KANG,KSD),PFV44(KANG,KSD),SSAV(KSD),EXTV(KSD)
C*****FOR CAPONY BRDF******************************************
      real R0B,KB,THB
      real R0BW,KBW,THBW
      DIMENSION R0BW(KW),KBW(KW),THBW(KW)
      integer ICANO
C*****FOR CAPONY BRDF******************************************  
C**********
      INTEGER NTAU(KW),NSZA(KW,MAXCLY),NZSZA0(KW),FTAU(KW)
     &,NUMUY(KW,MAXCLY,KNA0),IPOLPR(KW),FUMUY(KW,MAXCLY,KNA0)
     &,NUMUY0(KW),NPHI(KW,MAXCLY,KNA0,MAXUMU)
     &,NPHIY0(KW),LSZA(KW,MAXCLY,KNA0),FPHI(KW,MAXCLY,KNA0,MAXUMU)
     &,LOZA(KW,MAXCLY,KNA0,MAXUMU)
     &,LPHI(KW,MAXCLY,KNA0,MAXUMU,MAXPHI)
      REAL SZA(KW,MAXCLY,KNA0),LAT
     &,SZA0(KW,KNA0T),CSZA0(KW,KNA0)
     &,ZENOUTY(KW,MAXCLY,KNA0,MAXUMU)
     &,ZENOUT0(KW,MAXUMU),CZENOUT0(KW,MAXUMU)
     &,PHI(KW,MAXCLY,KNA0,MAXUMU,MAXPHI)
     &,PHI0(KW,MAXPHI)
      DOUBLE PRECISION TA,TR,HA,HN,HR
      DOUBLE PRECISION rmu(-nbmu:nbmu),chr(-nbmu:nbmu) 
      CHARACTER ERC*64
      REAL DTAUC( MAXCLY ),SSALB( MAXCLY )
      REAL xthetas,xthetav(nos),xltot(nos),xlpol(nos),xlt
      DIMENSION CBRF3(KNA0,KNA1U,KNFI),CBRF3W(KW,KNA0,KNA1U,KNFI)
C*****************
      REAL ZET(MAXCLY+1),TEM(MAXCLY+1),PRES(MAXCLY+1),
     & WTAUR(MAXCLY),WTAUA(MAXCLY),ANGR(KANG),
     & AIR(MAXCLY+1),AIRM(MAXCLY+1),TAURH(MAXCLY+1),
     & UO3(KW),DO3(KW,MAXCLY),H2O(MAXCLY),
     & UU(MAXUMU,MAXULV,MAXPHI),
     & O3(MAXCLY),ks(3000),REM(KW)
C******************
      REAL O3T(KW),H2OT(KW)
C      INTEGER FORC
      CHARACTER	 CH2*100
C******************
       REAL XIMM(KW)
      real angles(500,MAXCLY),phases(500,MAXCLY),
     &     phasesp(500,MAXCLY)
      integer nangle(MAXCLY)
      DIMENSION AM1U(KNA1U),AM0(KNA0),THK(KNLN),OMG(KNLN)
     &         ,NLGN1(KNLN),G(KLGN1,KNLN),PHSF(KNANG,KNLN),
     &          PHSFP(KNANG,KNLN)
     &         ,CPLK(KPLK1,KNLN),UTAUN(KNTAU),FLXD(KNA0,KNTAU)
     &         ,FLXU(KNA0,KNTAU),AI(KNA1U,KNA0,KNFI,KNTAU)
     &         ,FAI(KNFI),THETA(KNANG),ind1(MAXCLY+1)
     &          ,llyr(MAXCLY),albedoa(KW),AM1UB(KNA1U),
     &          hft(MAXCLY),ind2(MAXCLY+1),indl(MAXCLY+1),
     &          AM1US(KNA1U),AIP(KNA1U,KNA0,KNFI,KNTAU)
     &          ,ALBMODIS_1(NWM),WAVEMOD(NWM)
CS****************************Radforce correction********************************
       DIMENSION F1(KW),F2(KW),F3(KW)
CS**********************************************************************************
CS***********************FOR VECTOR CODE*****************************************
       DIMENSION H0V(KSD),WV(KSD),
     &JP1(NMM),ECH1(NMM),EXTH(NMM),SSAH(NMM),taugas(MAXCLY),
     &WA(KSD,MAXCLY),WAT(KSD),TAUA(KSD,MAXCLY),TH(MAXCLY),
     &THS(MAXCLY),THE(MAXCLY),SSAM(NMM)
C*** FOR OS_HERMAN:
      DIMENSION pha11(NMM,-NG:NG),
     &pha12(NMM,-NG:NG),pha33(NMM,-NG:NG),pha22(NMM,-NG:NG),
     &thd(NBVM),vis(NBVM),fiv(NBVM),
     &SQout(NBVM),SUout(NBVM),SLPout(NBVM),SLout(NBVM),
     &WD1(KNT-1,NMM),TH1(KNT),WD(KNT-1,NMM)

CXXA
	DOUBLE PRECISION TAUR1,TAURRR
CS********************************************************************************

c     .. External Subroutines ..
      REAL LINEAR,raylei            
c     .. Intrinsic Functions ..
      INTRINSIC ASIN, FLOAT, INDEX
!tl      DOUBLE PRECISION FUNCTION D1MACH 
      DIMENSION  SOLC(KW)
c***************************************************************
      COMMON /FORCII/ ALBEDOA,hlyr,O3T,H2OT,SZA,NTAU,NSZA,CH2
      COMMON /ALB/ ALBMODIS_1,WAVEMOD,NWM1 
      COMMON /RAY/ TAUR,NLYRS
c***************************************************************
	COMMON /ILP3/ III !CXXA

      logical lvect, lpolr
      DATA FWAVE /0.30, 0.31, 0.31, 0.32, 0.32, 0.33,
     & 0.33, 0.34, 0.34, 0.35, 0.35, 0.36, 0.37, 0.38, 0.39,
     & 0.40, 0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48,
     & 0.49, 0.50, 0.51, 0.52, 0.53, 0.54, 0.55, 0.57, 0.59,
     & 0.61, 0.63, 0.66, 0.67, 0.69, 0.71, 0.72, 0.72, 0.74,
     & 0.75, 0.76, 0.76, 0.77, 0.78, 0.80, 0.82, 0.82, 0.83,
     & 0.84, 0.86, 0.88, 0.91, 0.92, 0.93, 0.93, 0.94, 0.95,
     & 0.97, 0.98, 0.99, 1.04, 1.07, 1.10, 1.12, 1.13, 1.15,
     & 1.16, 1.17, 1.20, 1.24, 1.27, 1.29, 1.32, 1.35, 1.40,
     & 1.44, 1.46, 1.48, 1.50, 1.52, 1.54, 1.56, 1.58, 1.59,
     & 1.61, 1.63, 1.65, 1.68, 1.74, 1.80, 1.86, 1.92, 1.96,
     & 1.99, 2.01, 2.04, 2.07, 2.10, 2.15, 2.20, 2.27, 2.36,
     & 2.45, 2.50, 2.60, 2.70, 2.80, 2.90, 3.00, 3.10, 3.20,
     & 3.30, 3.40, 3.50, 3.60, 3.70, 3.80, 3.90, 4.00/ 

      DATA WFBEAM /535.90,	558.30,	622.00,	692.70,
     & 715.10, 832.90, 961.90, 931.90, 900.60, 911.30, 
     & 975.50, 975.90, 1119.90,	1103.80, 1033.80, 1479.10,	
     & 1701.30,	1740.40, 1587.20, 1837.00, 2005.00,	2043.00,
     & 1987.00,	2027.00, 1896.00, 1909.00, 1927.00, 1831.00,
     & 1891.00,	1898.00, 1892.00, 1840.00, 1768.00,	1728.00,
     & 1658.00,	1524.00, 1531.00, 1420.00, 1399.00,	1374.00,
     & 1373.00,	1298.00, 1269.00, 1245.00, 1223.00,	1205.00,
     & 1183.00,	1148.00, 1091.00, 1062.00, 1038.00,	1022.00,
     & 998.70, 947.20, 893.20, 868.20, 829.70, 830.30, 
     & 814.00, 786.90, 768.30, 767.00, 757.60, 688.10,
     & 640.70, 606.20, 585.90, 570.20, 564.10, 544.20,
     & 533.40, 501.60, 477.50, 442.70, 440.00, 416.80,
     & 391.40, 358.90, 327.50, 317.50, 307.30, 300.40, 
     & 292.80, 275.50, 272.10, 259.30, 246.90, 244.00,	
     & 243.50, 234.80, 220.00, 190.80, 171.10, 144.50,
     & 135.70, 123.00, 123.80, 113.00, 108.50, 97.50,
     & 92.40, 82.40, 74.60,	68.30, 63.80, 49.50, 48.50,
     & 38.60, 36.60, 32.00, 28.10, 24.80, 22.10, 19.60,
     & 17.50, 15.70, 14.10, 12.70, 11.50, 10.40, 9.50, 8.60/
      
C*************Calculations*********************
      PI = 2.* ASIN( 1.0 )
      RAD=PI/180.0
!tl      EPSIL = 10.*D1MACH(4)
C**********************************************
      XIK=1.0
      INDA=1
      INDT=1
      INDP=1
      IMTHD=3
      IK=1
      IWS=1
      IUCD=60
      IUCO=61
      IUCR=62
      IUCSR=63
CS      WRITE(*,*)'EXTV'
CS      WRITE(*,*)EXTV
CS      WRITE(*,*)'SSAV'
CS      WRITE(*,*)SSAV 
c **********************************************************************
c ****  Test Problem 4:  Haze-L Scattering, Beam Source             ****
c ****  (Compare to Ref. GS, Tables 12-16)                          ****
c **********************************************************************
        RAYLI=.TRUE.
cs        RAYLI=.FALSE.
        IPRI2=0
CD      IF(IW.EQ.-2.AND.IDI.LT.2)THEN
CD      IPRI=1 
CD      IDI=IDI+1
CD      ELSE
CD      IPRI=0
CD      ENDIF   
      IF(III.EQ.0) THEN
c		WRITE(*,*)'READ 981 AT BACK_OUT_SHURA'
		READ(981,*) NSTR,NLYR,NLYRS,NW,IGEOM,IDF,IDN,DPF
		IF(IPRI2.EQ.1)  write(6,*) NSTR,NLYR,NLYRS,NW,IGEOM,IDF,IDN,DPF,
     &					'NSTR,NLYR,NLYRS,NW,IGEOM,IDF,IDN,DPF'
		READ(981,*) H0(1),W(1),H0(2),W(2),NS
		IF(IPRI2.EQ.1) write(6,*) H0(1),W(1),H0(2),W(2),NS, 
     &					'H0(1),W(1),H0(2),W(2),NS'
		READ(981,*) iatmos,suhei,INDG1,INDG2,lvect,lpolr
!   lvect (.T. - vector code, .F. - scalar code), 
!   lpolr (.T. - with polarization, .F. - no polarization)

		IF(IPRI2.EQ.1) write(6,*) iatmos,suhei,INDG1,INDG2,lvect,lpolr,
     &				  'iatmos,suhei,INDG,INDG2,lvect,lpolr'
		IF(lvect)THEN
			READ(981,*)(H0V(i1),WV(i1),i1=1,NSD)
		ENDIF
		IF(IPRI2.EQ.1) write(6,*) NBRDF1, 'NBRDF1'
		IF(NBRDF.GT.0) THEN
			READ(981,*)LAD,ICANO
			IF(IPRI2.EQ.1) write(6,*) LAD,ICANO, 'LAD,ICANO'
		ENDIF
cs**************************************************************************************
		IF(INDG1.EQ.1.OR.INDG1.EQ.3)THEN
			READ(981,*)(REM(I),I=1,NW)
			IF(IPRI2.EQ.1) write(6,*) (REM(I),I=1,NW), 'REM'
			READ(981,*)(XIMM(I),I=1,NW)
			IF(IPRI2.EQ.1) write(6,*) (XIMM(I),I=1,NW), 'IMM'
		ENDIF
C*******************************************************************************************
		IF(BRF(1).EQ.0.AND.(INDG2.EQ.0.AND.INDG1.EQ.3)) THEN
cs       IF(INDG2.EQ.3.AND.INDG1.EQ.3) THEN
			READ(981,*) WSPD
			IF(IPRI2.EQ.1) write(6,*) WSPD, 'WSPD'
		ENDIF
C**********************************************************************************************
		IF(BRF(1).EQ.0.AND.INDG2.EQ.0.AND.INDG1.EQ.3)THEN
cs       IF(INDG2.EQ.3.AND.INDG1.EQ.3)THEN
			READ(981,*)XLAND,XOCEAN
			IF(IPRI2.EQ.1) write(6,*) XLAND,XOCEAN, 'XLAND,XOCEAN'
		ENDIF
C*********************************************************************************************
		READ(981,*) (hlyr(NLYR-I+2),I=1,NLYR+1)
		IF(IPRI2.EQ.1) write(6,*) (hlyr(NLYR-I+2),I=1,NLYR+1),' hlyr'
		READ(981,*) LAT
		IF(IPRI2.EQ.1) write(6,*) LAT,' LAT'
		NSTR1=NSTR+1
		NDA=NSTR
   20		FORMAT (L6)
c***************************
		DO IW1=1,NW
			READ(981,*) WAV(IW1)
			IF(IPRI2.EQ.1) write(6,*) WAV(IW1), 'WAV(IW1)'
			IF(BRF(1).EQ.0.AND.INDG1.EQ.0) THEN
cs       WRITE(*,*) 'BRF(1).EQ.0 - CODE needs ALBEDO in INPUT!!!'
cs       write(6,*)'albedo'
				READ(981,*) albedoa(IW1)
c				write(6,*)  albedoa(IW1)
			ENDIF
C************************************************************************
			IF(BRF(1).EQ.0.AND.INDG2.EQ.0.AND.INDG1.EQ.3) THEN
cs       WRITE(*,*) 'BRF(1).NE.0 - CODE needs BRDF in INPUT!!!'
cs       write(6,*)'BRDF'
				READ(981,*) R0BW(IW1),KBW(IW1),THBW(IW1)
cs       write(6,*)R0BW(IW1),KBW(IW1),THBW(IW1)
			ENDIF
C*************************************************************************
			READ(981,*) UO3(IW1), O3T(IW1),H2OT(IW1)
			IF(IPRI2.EQ.1) write(6,*) UO3(IW1), 'UO3(IW1)'
			READ(981,*) NTAU(IW1)
			FTAU(IW1)=NTAU(IW1)
			IF(IPRI2.EQ.1) write(6,*) NTAU(IW1), 'NTAU(IW1)'
			DO ITAU=1,NTAU(IW1)
				READ(981,*) houtput(IW1,ITAU)
				READ(981,*) NSZA(IW1,ITAU)
				DO ISZA=1,NSZA(IW1,ITAU)
					READ(981,*) SZA(IW1,ITAU,ISZA)
					READ(981,*) NUMUY(IW1,ITAU,ISZA)
					FUMUY(IW1,ITAU,ISZA)=NUMUY(IW1,ITAU,ISZA)
					DO IOZA=1,NUMUY(IW1,ITAU,ISZA)
						READ(981,*) ZENOUTY(IW1,ITAU,ISZA,IOZA)
						ZENOUTY(IW1,ITAU,ISZA,IOZA)=
     &					ZENOUTY(IW1,ITAU,ISZA,IOZA)+0.0
						READ(981,*) NPHI(IW1,ITAU,ISZA,IOZA)
						FPHI(IW1,ITAU,ISZA,IOZA)=NPHI(IW1,ITAU,ISZA,IOZA)
						READ(981,*) (PHI(IW1,ITAU,ISZA,IOZA,IPHI),
     &					IPHI=1,NPHI(IW1,ITAU,ISZA,IOZA))
						IF(IPRI2.EQ.1) 
     &					WRITE(*,*) (PHI(IW1,ITAU,ISZA,IOZA,IPHI),
     &					IPHI=1,NPHI(IW1,ITAU,ISZA,IOZA))
					ENDDO
				ENDDO
			ENDDO
		ENDDO
c***************************
		READ(981,*)NWM1
		IF(INDG1.EQ.0)THEN
			DO IR=1,NWM1
				READ(981,*)WAVEMOD(IR),ALBMODIS_1(IR)
			ENDDO
		ENDIF
cs**********************************Radforce correction*********************************
		IF(INDG1.GT.0)THEN
			DO IR=1,NWM1
				READ(981,*)WAVEMOD(IR),F1(IR),F2(IR),F3(IR)
			ENDDO
		ENDIF
cs**************************************************************************************
cs      do i=1,NWM1
cc      write(6,*)WAVEMOD(I),ALBMODIS_1(I)
cs      enddo
		READ(981,905)CH2
cs      write(6,905)CH2
905		format(a100) 
c***************************
		close(981)
		CLOSE(1)
	ENDIF
CS*******************Scattering *angles*calculation******************

      DO IW1=1,NW
         DO ITAU=1,NTAU(IW1)
         ic=1
           DO ISZA=1,NSZA(IW1,ITAU)
             DO IOZA=1,NUMUY(IW1,ITAU,ISZA)
              DO IPHI=1,NPHI(IW1,ITAU,ISZA,IOZA)
              ANGLESCA(IW1,ITAU,ic)=cos(RAD*SZA(IW1,ITAU,ISZA))
     &*cos(RAD*ZENOUTY(IW1,ITAU,ISZA,IOZA))+sin(RAD*SZA(IW1,ITAU,ISZA))*
     &sin(RAD*ZENOUTY(IW1,ITAU,ISZA,IOZA))*
     &cos(RAD*PHI(IW1,ITAU,ISZA,IOZA,IPHI))
      IF(ANGLESCA(IW1,ITAU,ic).GE.1.)ANGLESCA(IW1,ITAU,ic)=1.
      ANGLESCA(IW1,ITAU,ic)=acos(ANGLESCA(IW1,ITAU,ic))/RAD
             ic=ic+1
             ENDDO
            ENDDO
           ENDDO
         ENDDO
      ENDDO

      SZAS=cos(RAD*SZA(1,1,1))
cs         DO IW1=1,NW
cs         DO ITAU=1,NTAU(IW1)
cs          write(6,*)(ANGLESCA(IW1,ITAU,i3),i3=1,30)
cs          write(6,*)
cs      enddo
cs      enddo
CS*******************************************************************
      INDG=INDG1
cs       write(6,*)'WAV',WAV(IW)
cs       write(6,*)'lvect,',lvect
C	WRITE(*,*)'III=',III
	IF(III.LT.NW) THEN
C**************************************************
c***   for multipl scattering                   ***
C***           WRITE(*,*) ' DISORT'             ***
C***Y *** RADIATIVE TRANSFER by NAKAJIMA et al **** 
C**************************************************
C**************************************************
C****** Defining angular structure for DISORT *****
C**************************************************
C*** Definig number of the angles               ***
C*** --- Defining the number and the values of  ***
C***     SOLAR zenith angles                    ***
		NZSZA0(IW)=1
		SZA0(IW,1)=SZA(IW,1,1)
		ISZA=0
CD      DO ISZA=1,NZSZA0(IW)+1
cD      WRITE(*,*) ISZA,' ISZA'
   2		DO ITAU1=1,NTAU(IW)
CD      WRITE(*,*) ITAU1,NTAU(IW),' ITAU1,NTAU(IW)'
			DO  ISZA1=1,NSZA(IW,ITAU1)
CD       WRITE(*,*) ISZA1,NSZA(IW,ITAU1),' ISZA1,NSZA(IW,ITAU1)'
				IF(SZA0(IW,NZSZA0(IW)).GT.SZA(IW,ITAU1,ISZA1).AND.ISZA.EQ.0)
     &				THEN
					SZA0(IW,NZSZA0(IW))=SZA(IW,ITAU1,ISZA1)
				ENDIF
				IF(ISZA.GT.0) THEN
					IF(SZA0(IW,ISZA).LT.SZA(IW,ITAU1,ISZA1).AND.
     &					SZA0(IW,ISZA+1).EQ.0) THEN
						SZA0(IW,ISZA+1)=SZA(IW,ITAU1,ISZA1)
						NZSZA0(IW)=NZSZA0(IW)+1
CD           WRITE(*,*) NZSZA0(IW),' NZSZA0(IW) i selections'
					ENDIF
					IF(ISZA.LT.NZSZA0(IW)) THEN
						IF(SZA0(IW,ISZA).LT.SZA(IW,ITAU1,ISZA1).AND.
     &						SZA0(IW,NZSZA0(IW)).GT.SZA(IW,ITAU1,ISZA1)) 
     &						SZA0(IW,NZSZA0(IW)) = SZA(IW,ITAU1,ISZA1)      
					ENDIF
				ENDIF
			ENDDO
		ENDDO
CD      ENDDO
		IF(ISZA.LT.NZSZA0(IW)) THEN
			ISZA=ISZA+1
			GOTO 2
		ENDIF
		IF(IPRI2.EQ.1) WRITE(*,*) NZSZA0(IW),' NZSZA0(IW)'
		DO ISZA=1,NZSZA0(IW)
			IF(IPRI2.EQ.1) WRITE(*,*) SZA0(IW,ISZA),'SZA0'
		ENDDO
C*** --- Defining the number and the values of  ***
C***     OBSERVATION zenith angles              ***
		NUMUY0(IW)=1
		ZENOUT0(IW,1)=ZENOUTY(IW,1,1,1)
		IOZA=0 
    3		DO ITAU1=1,NTAU(IW)
			DO ISZA1=1,NSZA(IW,ITAU1)
				DO IOZA1=1,NUMUY(IW,ITAU1,ISZA1)
					IF(ZENOUT0(IW,NUMUY0(IW)).GT.ZENOUTY(IW,ITAU1,ISZA1,IOZA1)
     &					.AND.IOZA.EQ.0) THEN
						ZENOUT0(IW,NUMUY0(IW))=ZENOUTY(IW,ITAU1,ISZA1,IOZA1) 
					ENDIF
					IF(IOZA.GT.0) THEN
						IF(ZENOUT0(IW,IOZA).LT.ZENOUTY(IW,ITAU1,ISZA1,IOZA1)
     &						.AND.ZENOUT0(IW,IOZA+1).EQ.0) THEN
							ZENOUT0(IW,IOZA+1)=ZENOUTY(IW,ITAU1,ISZA1,IOZA1)
							NUMUY0(IW)=NUMUY0(IW)+1
						ENDIF
						IF(IOZA.LT.NUMUY0(IW)) THEN
							IF(ZENOUT0(IW,IOZA).LT.
     &							ZENOUTY(IW,ITAU1,ISZA1,IOZA1)
     &							.AND.ZENOUT0(IW,NUMUY0(IW)).GT.
     &							ZENOUTY(IW,ITAU1,ISZA1,IOZA1))
     &							ZENOUT0(IW,NUMUY0(IW))=
     &							ZENOUTY(IW,ITAU1,ISZA1,IOZA1)     
						ENDIF
					ENDIF     
				ENDDO
			ENDDO
		ENDDO
CD      ENDDO
		IF(IOZA.LT.NUMUY0(IW)) THEN
			IOZA=IOZA+1
			GOTO 3
		ENDIF
C*** --- Defining the number and the values of  ***
C***     OBSERVATION azimuth angles             ***
C===================================BY XXA===============
		PHI0(IW,:)=PHI(IW,1,1,1,1) !CXXA
C		IF(III.EQ.0)WRITE(*,*)IW,PHI0(IW,1:2),PHI(IW,1,1,1,1)
C========================================================
		NPHIY0(IW)=1
		PHI0(IW,1)=PHI(IW,1,1,1,1)  
		IPHI=0
C		IF(III.EQ.0.AND.IW.EQ.1)THEN
C			WRITE(*,*)NTAU(IW),NSZA(IW,1),NUMUY(IW,1,1),NPHI(IW,1,1,1)
C		ENDIF
    5		DO ITAU1=1,NTAU(IW)
			DO ISZA1=1,NSZA(IW,ITAU1)
				DO IOZA1=1,NUMUY(IW,ITAU1,ISZA1)
					DO IPHI1=1,NPHI(IW,ITAU1,ISZA1,IOZA1)
						IF(PHI0(IW,NPHIY0(IW)).GT.
     &						PHI(IW,ITAU1,ISZA1,IOZA1,IPHI1)
     &						.AND.IPHI.EQ.0) THEN
							PHI0(IW,NPHIY0(IW))=
     &						PHI(IW,ITAU1,ISZA1,IOZA1,IPHI1)
						ENDIF 
						IF(IPHI.GT.0) THEN
							IF(PHI0(IW,IPHI).LT.
     &						PHI(IW,ITAU1,ISZA1,IOZA1,IPHI1)
     &						.AND.PHI0(IW,IPHI+1).EQ.0) THEN
								PHI0(IW,IPHI+1)=
     &							PHI(IW,ITAU1,ISZA1,IOZA1,IPHI1)
								NPHIY0(IW)=NPHIY0(IW)+1
							ENDIF
							IF(IPHI.LT.NPHIY0(IW)) THEN
								IF(PHI0(IW,IPHI).LT.
     &							PHI(IW,ITAU1,ISZA1,IOZA1,IPHI1)
     &							.AND.PHI0(IW,NPHIY0(IW)).GT.
     &							PHI(IW,ITAU1,ISZA1,IOZA1,IPHI1))
     &							PHI0(IW,NPHIY0(IW))=
     &							PHI(IW,ITAU1,ISZA1,IOZA1,IPHI1) 
							ENDIF    
						ENDIF               
					ENDDO
				ENDDO
			ENDDO
		ENDDO  
CD      ENDDO
		IF(IPHI.LT.NPHIY0(IW)) THEN
C			IF(III.EQ.0.AND.IW.EQ.1)WRITE(*,*)NPHIY0(IW),IPHI
			IPHI=IPHI+1
			GOTO 5
		ENDIF   
C		IF(III.EQ.0.AND.IW.EQ.1)WRITE(*,*)NPHIY0(IW)

C		IF(IPRI2.EQ.1) WRITE(*,*) NPHIY0(IW),' NPHIY0(IW)' 
C*** --- Transforming ZOLAR and OBSERVATION     ***
C***     zenith angles into COSs                ***
		DO ISZA=1,NZSZA0(IW)
			CSZA0(IW,ISZA)=COS(RAD*SZA0(IW,ISZA))
		ENDDO
		DO IOZA=1,NUMUY0(IW)
CD      WRITE(*,*) IOZA,ZENOUT0(IW,IOZA),' ZENOUT0(IW,IOZA)'
			CZENOUT0(IW,IOZA)=COS(RAD*(ZENOUT0(IW,IOZA)))
CD      WRITE(*,*) CZENOUT0(IW,IOZA),' CZENOUT0(IW,IOZA)'
		ENDDO
C**************************************************      
C*** Finding the geometry   for OUTPUT          *** 
C***   in the geometry if DisOrds               ***
C***   -  SOLAR zenith angles                   ***
		DO ITAU1=1,NTAU(IW)
			DO  ISZA1=1,NSZA(IW,ITAU1)
				DO ISZA=1,NZSZA0(IW)
					IF(SZA0(IW,ISZA).EQ.SZA(IW,ITAU1,ISZA1))
     &				LSZA(IW,ITAU1,ISZA1)=ISZA
				ENDDO
			ENDDO
		ENDDO 
C***   - OBSERVATION zenith angles              *** 
		DO ITAU1=1,NTAU(IW)
			DO ISZA1=1,NSZA(IW,ITAU1)
				DO IOZA1=1,NUMUY(IW,ITAU1,ISZA1)
					DO IOZA=1,NUMUY0(IW)
						IF(ZENOUT0(IW,IOZA).EQ.ZENOUTY(IW,ITAU1,ISZA1,IOZA1))
     &					LOZA(IW,ITAU1,ISZA1,IOZA1)=IOZA
					ENDDO
				ENDDO
			ENDDO
		ENDDO     
C***     OBSERVATION azimuth angles             ***
		DO ITAU1=1,NTAU(IW)
			DO ISZA1=1,NSZA(IW,ITAU1)
				DO IOZA1=1,NUMUY(IW,ITAU1,ISZA1)
					DO IPHI1=1,NPHI(IW,ITAU1,ISZA1,IOZA1)
						DO IPHI=1,NPHIY0(IW)
      IF(PHI0(IW,IPHI).EQ.PHI(IW,ITAU1,ISZA1,IOZA1,IPHI1))
     &LPHI(IW,ITAU1,ISZA1,IOZA1,IPHI1)=IPHI
						ENDDO
					ENDDO
				ENDDO
			ENDDO
		ENDDO  
		IF(IDF.EQ.1) FBEAM=PI
		IF(IPRI2.EQ.1) write(6,*)'FBEAM',FBEAM
		DO IW1=1,NW
			SOLC(IW1)=LINEAR(FWAVE,WFBEAM,122,WAV(IW1))
		ENDDO
C****************************************************************
      ENDIF !END OF III

cxxa		IF(III.EQ.0)WRITE(*,*)NPHIY0(IW),IPHI

cs      write(6,*)'III',III
c	write(*,*)hlyr
      IF(.not.lvect)THEN
		IF(III.LE.2*NW)THEN
C****Divide atmosphere into sublayers
C    Convert hlyr to pressure levels hplyr
			NLN=NLYRS
			DO L=1,NLN
				NLGN1(L)=150
			ENDDO
			do ih=1,NLYR+1
				hplyr(ih)=1039.0404*1./exp(hlyr(ih)/7.28657)
			enddo
C     Calculate the increment for pressure sublayers
			hdp=(hplyr(NLYR+1)-hplyr(1))/NLYRS
			hpl(1)=hplyr(1)
C     Calculate the pressure sublayers hpl
			do ih=1,NLYRS
				hpl(ih+1)=hpl(1)+ih*hdp
			enddo
C     Convert the pressure sulayers to heights hl
c	stop
			do ih=1,NLYRS+1
				hl(ih)=7.28657*(alog(1039.0404)-alog(hpl(ih)))
			enddo
C     Matching the hl values with hlyr
			hl(1)=hlyr(1)
			hl(NLYRS+1)=hlyr(NLYR+1)
			do ih=1,NLYR+1
				do ip=1,NLYRS
					if(hlyr(ih).le.hl(ip).and.hlyr(ih).ge.hl(ip+1))then
						ind1(ih)=ip
						ind2(ih)=ip+1
					endif
				enddo
			enddo
      
			do ih=1,NLYR+1
				hdif1=abs(hlyr(ih)-hl(ind1(ih)))
				hdif2=abs(hlyr(ih)-hl(ind2(ih)))
				if(hdif1.lt.hdif2.and.ind2(ih).lt.NLYRS+1.and.
     &				ind1(ih).gt.1)then
					hl(ind1(ih))=hlyr(ih)
					indl(ih)=ind1(ih)
				endif
				if(hdif1.gt.hdif2.and.ind2(ih).lt.NLYRS+1.and.
     &				ind1(ih).gt.1)then
					hl(ind2(ih))=hlyr(ih)
					indl(ih)=ind2(ih)
				endif
				if(ind1(ih).eq.1.and.hlyr(ih).ne.hl(1))then
					hl(ind2(ih))=hlyr(ih)
					indl(ih)=ind2(ih)
				endif
				if(ind1(ih).eq.1.and.hlyr(ih).eq.hl(1))then
					hl(ind1(ih))=hlyr(ih)
					indl(ih)=ind1(ih)
				endif
				if(ind2(ih).eq.NLYRS+1.and.hlyr(ih).
     &				ne.hl(NLYRS+1))then
					hl(ind1(ih))=hlyr(ih)
					indl(ih)=ind1(ih)
				endif
				if(ind2(ih).eq.NLYRS+1.and.hlyr(ih).eq.
     &				hl(NLYRS+1))then
					hl(ind2(ih))=hlyr(ih)
					indl(ih)=ind2(ih)
				endif
			enddo
C*******************************************CORRECTION************
			if(NLYRS.EQ.2)then
				hl(1)=hlyr(1)
				hl(2)=H0(1)
				hl(3)=hlyr(2)
			endif
cs      write(6,*)'hl',hl
C******************************************************************
C*** Matching Atmosphere Profiles:
			CALL gas(NLYRS,iatmos,suhei,hl,MAXL,MAXCLY,ZET,
     *			TEM,PRES,AIR,AIRM,H2O,O3)
			IF(IPRI2.EQ.1) WRITE(*,*) "after gas"
			AO3=0.
			DO I=1,NLYRS
				AO3=AO3+O3(I)
			ENDDO
		ENDIF 

		IF(III.LT.NW)THEN  
C*** Get Rayilegh optical thickness profile
			DPL=DPF/(2-DPF)
			IF(RAYLI) THEN
				do ih=1,NLYRS+1
					xl=hl(ih)*1000.
					CALL rayleia(DBLE(WAV(IW)),DBLE(xl),DBLE(LAT),TAUR(IW,ih))
				enddo
			ELSE
				do ih=1,NLYRS+1
					TAUR(IW,ih)=0.0
				enddo
			ENDIF
			DO I=1,NLYRS
				DO3(IW,I)=O3(I)/AO3*UO3(IW)
cs      WRITE(*,*)'DO3',DO3(IW,I)
			ENDDO
		ENDIF
cs      write(6,*)'DO3',DO3
C****Divide aerosol optical depth into sublayers
C      Distribute TAUU(I) over each layer
		 ks(1)=1.
		 ks(2)=4.
		 ks(3)=2.
		 ks(2*NS+1)=1.
		 do ih2=4,2*NS
			 ks(ih2)=4.
		 enddo
cs       write(6,*)'ks',ks
cs       write(6,*)'indl',indl
cs       write(6,*)'hl',hl
		do ih =1,NLYR
			do ip=indl(ih),indl(ih+1)-1
				hft(ip)=0.
				hss=(hl(ip)-hl(ip+1))/(2*NS)
				hft(ip)=hft(ip)+ks(1)*(gaussd(H0(1),W(1),hl(ip+1))
     &				+gaussd(H0(2),W(2),hl(ip+1)))
cs       write(6,*)'gauss',gaussd(H0(ih),W(ih),hl(ip+1))
				do ip1=2,2*NS+1
					xg1=gaussd(H0(1),W(1),hl(ip+1)+(ip1-1)*hss)
					xg2=gaussd(H0(2),W(2),hl(ip+1)+(ip1-1)*hss)
					hft(ip)=hft(ip)+ks(ip1)*(xg1+xg2)
cs      write(6,*)'hft(ip)',hft(ip)
cs      write(6,*)'hss,hl(ip+1)+(ip1-1))',hss,hl(ip+1)+(ip1-1)
				enddo
				hft(ip)=hft(ip)*hss/3.
cs       write(6,*)'hft',ip,hft(ip)
			enddo
			hftf=0.
			do ip=indl(ih),indl(ih+1)-1
				hftf=hftf+hft(ip)
			enddo

cs       write(6,*)'hftf',hftf
			do ip=indl(ih),indl(ih+1)-1
				IF(ITOFF.EQ.0)then
cs       write(6,*)'TAUU(ih),hft(ip)/hftf',TAUU(ih),hft(ip)/hftf
					HTAUU(ip)=TAUU(ih)*hft(ip)/hftf
cs       write(6,*)'HTAUU(ip)',HTAUU(ip)
					if(HTAUU(ip).LT.1.e-30)HTAUU(ip)=1.e-30
				endif
				IF(ITOFF.EQ.1) HTAUU(ip)=1.e-30
cs       HTAUU(ip)=1.e-30
			enddo
		enddo
C**********************************************CORRECTION****

		 if(NLYRS.EQ.2)then
			HTAUU(1)=1.e-30
			HTAUU(2)=TAUU(1)
			IF(ITOFF.EQ.1)then
				HTAUU(1)=1.e-30
				HTAUU(2)=1.e-30
			endif
		 endif
C************************************************************
		 IF(ITOFF.EQ.1)THEN
			do ih=1,NLYRS+1
				TAUR(IW,ih)=1.e-30
			enddo
			DO I=1,NLYRS
				DO3(IW,I)=1.e-30
			ENDDO
		 ENDIF
cs       do ip=1,NLYR
cs       write(6,*)'TAUU',TAUU(ip)
cs       enddo
cs       write(6,*)
cs       do ip=1,NLYRS
cs       write(6,*)HTAUU(ip),hl(ip),TAUR(IW,ip)
cs       write(6,*)'HTAUU',HTAUU
cs       enddo
cs       write(6,*)
C*** Get weights for aerosol and Rayilegh Phase Func. combining
C****Distribute SSA over the sublayers
		do ih=1,NLYR
			 do ip=indl(ih),indl(ih+1)-1
				 SSALBEDOS(ip)=SSALBEDO(ih)
			enddo
		enddo
C*************************************************CORRECTION****
		if(NLYRS.EQ.2)then
			  SSALBEDOS(1)=SSALBEDO(1)
			  SSALBEDOS(2)=SSALBEDO(1)
		endif
C****************************************************************
cs      write(6,*)'SSALBEDOS',SSALBEDOS
C****Distribute phase function over the sublayers
		DO ih=1,NLYR
			DO ih1=1,NANG
				do ip=indl(ih),indl(ih+1)-1
					PDDIS(ih1,ip)=PDDI(ih1,ih)
					PDDISP(ih1,ip)=PFL12(ih1,ih)
				enddo
			enddo
		enddo
C************************************************CORRECTION*********
		if(NLYRS.EQ.2)then
			DO ih1=1,NANG
				PDDIS(ih1,1)=PDDI(ih1,1)
				PDDISP(ih1,1)=PFL12(ih1,1)
				PDDIS(ih1,2)=PDDI(ih1,1)
				PDDISP(ih1,2)=PFL12(ih1,1) 
			enddo
		endif
C*******************************************************************      
cs      write(6,*)'PDDIS',PDDIS
		DO I=1,NLYRS
			DTAUC(I)=(TAUR(IW,I+1)-TAUR(IW,I))+HTAUU(I)+DO3(IW,I)
cs      write(6,*)'TAUR(IW,I+1)-TAUR(IW,I)',TAUR(IW,I+1),TAUR(IW,I),
cs     & TAUR(IW,I+1)-TAUR(IW,I),DO3(IW,I)
cs      write(6,*)'DTAUC(I)',DTAUC(I)
			SSALB(I)=(HTAUU(I)*SSALBEDOS(I)
     &			+(TAUR(IW,I+1)-TAUR(IW,I)))/DTAUC(I)
			IF(ITOFF.EQ.1)SSALB(I)=1.e-30
cs      SSALB(I)=1.e-30
			WTAUR(I)=(TAUR(IW,I+1)-TAUR(IW,I))/
     &			((TAUR(IW,I+1)-TAUR(IW,I))+HTAUU(I)*SSALBEDOS(I))
			WTAUA(I)=(HTAUU(I)*SSALBEDOS(I))/
     &			((TAUR(IW,I+1)-TAUR(IW,I))+HTAUU(I)*SSALBEDOS(I))
cs      WRITE(*,*)WTAUR(I),WTAUA(I)
		ENDDO
		DO I=1,NANG
			ANGR(I)=(ANG(I)/180*PI)
		ENDDO
cs      write(6,*)'DTAUC',DTAUC
cs      write(6,*)'SSALB',SSALB
cs      write(6,*)'WTAUR',WTAUR
cs      write(6,*)'WTAUA',WTAUA
C*** Get Phase Function:
		DO 10 IL=1,NLYRS
			DO I=1,NANG
				AREY=3./(2.*(2.+DPL))*(1.+DPL+(1.-DPL)
     &			*COS(ANGR(I))*COS(ANGR(I))) 
				PHSF(I,IL)=WTAUR(IL)*AREY+WTAUA(IL)*PDDIS(I,IL)
cs      WRITE(*,*)ANG(I),PDDIS(I,IL),PHSF(I,IL)
				AREY1=3./(2.*(2.+DPL))*(-1.+DPL+(1.-DPL)
     &			*COS(ANGR(I))*COS(ANGR(I)))
				PHSFP(I,IL)=WTAUR(IL)*AREY1+WTAUA(IL)*PDDISP(I,IL)
			ENDDO
   10		CONTINUE
cs      write(6,*)'PHSF',PHSF
		NLN=NLYRS
C      THK <= DTAUC
		DO IAN=1,NLYRS
			THK(IAN)=DTAUC(IAN)
cs      if(ITOFF.EQ.1) write(6,*)'THK',THK(IAN)
		ENDDO
C      OMG <= SSALB
		DO IAN=1,NLYRS
			OMG(IAN)=SSALB(IAN)
cs      write(6,*)'OMG',OMG(IAN)
		ENDDO
		DO IAN=1,NANG
			THETA(IAN)=ANG(IAN)
cs        write(6,*)'THETA',THETA(IAN)
		ENDDO
C***Y " the paprameters (omg,gg1,gg2,fr) are defined only as an example"
c omg:      equivalent single scattering albedo for the surface layer
c      omg=BRF(1)
c gg1:      forward g after scaling    ( 0<gg1<0.65, gg1 ~0.)
c      gg1=BRF(2)
c gg2:      backward g                 ( -0.8<gg2<0, hot-spot )
c      gg2=-BRF(3)
c fr:       the fraction of forward direction ( 0.0<fr<0.999 )
c      fr=BRF(4)
C***Y
cs        write(6,*)'NBRDF,INDG,INDG2',NBRDF,INDG,INDG2   
		IF(INDG.eq.0) THEN
			LAMBER=.TRUE.
		ELSE
			LAMBER=.FALSE.
		ENDIF
		IF(LAMBER.AND.BRF(1).EQ.0) THEN
			albedo=albedoa(IW)
			GALB=albedo
		ENDIF
		IF(LAMBER.AND.BRF(1).NE.0) THEN
			albedo=BRF(1)
			GALB=albedo
		ENDIF
		IF(albedo.gt.0.9999) albedo=0.9999
C     ENDIF IPOL
      ENDIF  !END OF .NOT.LVECT
!tl      IF(IPOLD.EQ.1.OR.IPOLD.EQ.0)THEN
C******************************************************
C*** Solar zenith COSs angle for disOrds            ***
      NA0=NZSZA0(IW)
      DO ISZA=1,NZSZA0(IW)
       AM0(ISZA)=CSZA0(IW,ISZA)
CD       WRITE(*,*) AM0(ISZA),' COS of SOlar ZEN ANGLE'
      ENDDO
C******************************************************
      NTAU1=ntau(IW)
      NTAUN=NTAU1
      NA1U=NUMUY0(IW)
      NFI=NPHIY0(IW)
CXXA	IF(III.EQ.0.AND.IW.EQ.1)WRITE(*,*)'NPHIY0=',NPHIY0(IW)
      DO I=1,NA1U
CD        WRITE(*,*) NA1U,'NUMUY0(IW)' 
		AM1U(I)=CZENOUT0(IW,I)
CD        WRITE(*,*) I, AM1U(I),' COS of OBS ZEN ANGLE'
      ENDDO
      DO I=1,NTAU(IW)
		houtput1(I)=houtput(IW,I)
		DO IPHI=1,NPHIY0(IW)
			FAI(IPHI)=PHI0(IW,IPHI)
CD         WRITE(*,*) IPHI,FAI(IPHI)
		ENDDO
      ENDDO
C     ENDIF IPOL
!tl      ENDIF 
      IF(.not.lvect)THEN  
		EPSP=0.
		EPSU=0.
		IF(IDF.EQ.2) FBEAM=SOLC(IW)
		FSOL=fbeam
		NPLK1=0
		BGND=0.
		do k=1,ntaun
			utaun(k)=0.
			hplane=houtput1(k)
			kout=1
			do i=1,NLYRS
				if (hl(i+1).gt.hplane) then
					utaun(k)=utaun(k)+THK(i)
					kout=i+1
				endif
			enddo
			llyr(k)=kout
			utaun(k)=utaun(k)+THK(kout)
     $		*(hl(kout)-hplane)/(hl(kout)-hl(kout+1))
		enddo
		IF(IMSC.EQ.1) THEN
			DO I=1,NLYRS
				TAURH(I)=0.0
			ENDDO
			ltop=1
			lbot=NLYRS
			do i=ltop,lbot
				nangle(i)=NANG
				do j=1,nangle(i)
					angles(j,i)=ANG(J)
					phases(j,i)=PHSF(j,i)
					phasesp(j,i)=PHSFP(j,i)
				enddo
			enddo
cs        do i1=1,nangle(1)
cs        write(6,*)ANG(i1),phasesp(i1,1)
cs        enddo
			DO I=1,NA1U
				AM1UB(I)=AM1U(I)
				AM1US(I)=-AM1U(I)
			ENDDO
			IF(INDG.EQ.3) THEN
				IF(NBRDF.GT.1.AND.INDG2.EQ.3) THEN
					R0B=BRF(1)
					KB=BRF(2)
					THB=BRF(3)
					GALB=WSPD
					SCR=REM(IW)
					SCI=XIMM(IW)
				ENDIF
				IF(NBRDF.EQ.0.AND.INDG2.EQ.0) THEN
					R0B=R0BW(IW)
					KB=KBW(IW)
					THB=THBW(IW)
					GALB=WSPD
					SCR=REM(IW)
					SCI=XIMM(IW)
				ENDIF
cs        IF(INDB.EQ.1)THEN
				CALL GETCN2(IUCD,IUCO,KNA0,KNA1U,KNFI
     &			,NZSZA0(IW),NA1U,NFI,AM0,AM1UB,FAI,CBRF3,
     &			R0B,KB,THB,ICANO,IBRF,IRFS)
				do i1=1,NZSZA0(IW)
					do i2=1,NA1U
						do i3=1,NFI
							CBRF3W(IW,i1,i2,i3)=CBRF3(i1,i2,i3)
						enddo
					enddo
				enddo
cs        ELSE
cs       do i1=1,NZSZA0(IW)
cs       do i2=1,NA1U
cs       do i3=1,NFI
cs       CBRF3(i1,i2,i3)=CBRF3W(IW,i1,i2,i3)
cs       enddo
cs       enddo
cs       enddo
cs        ENDIF
			ENDIF
			IF(INDG.EQ.1)THEN
				IF(NBRDF1.GT.0.AND.INDG2.EQ.1) THEN
					GALB=BRF1(1)
					SCR=REM(IW)
					SCI=XIMM(IW)
cs        write(6,*)'GALB',GALB
cs        write(6,*)'IW,SCR',IW,SCR
cs       write(6,*)'IW,SCI',IW,SCI
				ENDIF
				IF(NBRDF1.EQ.0.AND.INDG2.EQ.0) THEN
					GALB=WSPD
					SCR=REM(IW)
					SCI=XIMM(IW)
cs        write(6,*)'WSPD',WSPD
cs        write(6,*)'IW,SCR',IW,SCR
cs        write(6,*)'IW,SCI',IW,SCI
				ENDIF
			ENDIF
cs         write(6,*)'single'
cs        WRITE(*,*)'llyr',llyr
cs        WRITE(*,*)'utau',UTAUN
			DO 1200 ISZA=1,NZSZA0(IW)
				call single(NLN,THK,OMG,TAURH,HTAUU,SSALBEDOS,
     $			fbeam,INDG,GALB,SCR,SCI,AM0(ISZA),ISZA,NA1U,AM1US,NFI,FAI,
     $            nangle,angles,phases,ltop,lbot,lamber,
     $            albedo,gg1,gg2,fr,NTAUN,llyr,UTAUN,uu,
     $            CBRF3,XLAND,XOCEAN)
			DO 1200 LU=1,NTAU(IW)
			DO 1200 IU=1,NUMUY0(IW)
			DO 1200 J=1,NPHIY0(IW)
				AI(IU,ISZA,J,LU)=UU(IU,LU,J)
 1200			CONTINUE
cs        write(6,*)'AI'
cs        write(6,*)AI
		ENDIF
		IF(IMSC.EQ.0) THEN 
cs        IF(I.not.lvect) THEN
			IF(INDG.EQ.3) THEN
				IF(NBRDF.GT.1.AND.INDG2.EQ.3) THEN
					R0B=BRF(1)
					KB=BRF(2)
					THB=BRF(3)
					GALB=WSPD
					SCR=REM(IW)
					SCI=XIMM(IW)
				ENDIF
				IF(NBRDF.EQ.0.AND.INDG2.EQ.0) THEN
cs         write(6,*)'IN'
cs         write(6,*)R0BW(IW),KBW(IW),THBW(KW)
					R0B=R0BW(IW)
					KB=KBW(IW)
					THB=THBW(IW)
					GALB=WSPD
					SCR=REM(IW)
					SCI=XIMM(IW)
cs**********************************RADFORCE CORRECTION****************************
cs****************LAST***********CORRECTION****************************************
					IF(IRFS.EQ.1.AND.INDG.GT.0)THEN
						IF(NW.GE.7)THEN
							R0B=F1(IW)
							KB=F2(IW)
							THB=F3(IW)
							GALB=WSPD
							SCR=REM(IW)
							SCI=XIMM(IW)
cs           write(6,*)'IW,F1,F2,F3',IW,F1(IW),F2(IW),F3(IW)
						ENDIF
						IF(NW.LT.7)THEN
							irf=irf+1
							IF(irf.LE.NW)THEN
								R0B=F1(IW)
								KB=F2(IW)
								THB=F3(IW)
								GALB=WSPD
								SCR=REM(IW)
								SCI=XIMM(IW)
cs           write(6,*)'IW,F1,F2,F3',IW,F1(IW),F2(IW),F3(IW)
							ENDIF
							IF(irf.GT.NW)THEN
								R0B=F1(IW+NW)
								KB=F2(IW+NW)
								THB=F3(IW+NW)
								GALB=WSPD
								SCR=REM(IW)
								SCI=XIMM(IW)
cs           write(6,*)'IW,F1,F2,F3',IW+NW,F1(IW+NW),F2(IW+NW)
cs     &      ,F3(IW+NW)
							ENDIF
						ENDIF
					ENDIF
cs**********************************************************************************
				ENDIF
			ENDIF
			IF(INDG.EQ.1)then
				IF(NBRDF1.GT.0) THEN
cs        write(6,*)'IN CONDITION'
					GALB=BRF1(1)
					SCR=REM(IW)
					SCI=XIMM(IW)
				ENDIF
			ENDIF
cs        write(6,*)'RTRN'
cs       write(6,*)'INDG',INDG
cs       write(6,*)'GALB',GALB
			INDG1=INDG
cs       write(6,*)'BEFORE RTRN'
cs       write(6,*)'PHSF',PHSF
cs       write(6,*)
cs       write(6,*)'OMG',OMG
cs       write(6,*)
c       write(6,*)'THK',THK
			if(iii.eq.0)then
C				WRITE(*,*)'NFI=',NFI
C			write(250,*)indg,inda,indt,indp,imthd,nda,na1u
C			WRITE(250,*)am1u(1),na0,am0,NFI,NLN,NLGN1
C			write(250,*)fai(1:3),thk(1:2),omg(1:2)
C			write(250,*)nang,epsp,epsu
C			write(250,*)nplk1,cplk(1,1),bgnd,ntaun,utaun(1:2),scr,sci
			endif
			CALL RTRN22(INDG,INDA,INDT,INDP,IMTHD,NDA,NA1U,AM1U,NA0,AM0
     &		,NFI,FAI,NLN,THK,OMG,NLGN1,G,NANG,THETA,PHSF,EPSP,EPSU,GALB,FSOL
     &		,NPLK1,CPLK,BGND,NTAUN,UTAUN,SCR,SCI,FLXD,FLXU,AI,ERC
     &		,IUSN,IUCD,IUCO,IUCR,IUCSR,DDLW
     &		,IWS,IK,R0B,KB,THB,ICANO,IBRF,ITOFF,XLAND,XOCEAN,NW,IRFS)
		ENDIF 
C      ENDIF IPOL
      ENDIF
      
	IF(lvect) THEN
		tgas=0.
		hplyr(1)=1039.0404*1./exp(hlyr(1)/7.28657)
		hplyr(NLYRS+1)=1039.0404*1./exp(hlyr(NLYRS+1)/7.28657)
C     Calculate the increment for pressure sublayers
		hdp=(hplyr(NLYRS+1)-hplyr(1))/NLYRS
		hpl(1)=hplyr(1)
C     Calculate the pressure sublayers hpl
		do ih=1,NLYRS
			hpl(ih+1)=hpl(1)+ih*hdp
		enddo
C     Convert the pressure sulayers to heights hl
		do ih=1,NLYRS+1
			hl(ih)=7.28657*(alog(1039.0404)-alog(hpl(ih)))
CD      WRITE(*,*) hl(ih),ih,' hl(ih),ih'
		enddo
C*** Get Rayilegh optical thickness profile
		do ih=1,NLYRS+1
			xl=hl(ih)*1000.
			CALL rayleia(DBLE(WAV(IW)),DBLE(xl),DBLE(LAT),TAUR1)
			TAUR(IW,ih)=TAUR1
CD      WRITE(*,*) WAVE,xl,LAT,TAUR(ih),'WAVE,xl,LAT,TAUR(ih)'
		enddo
		DO ih=1,NLYRS
			TAURC(ih)=TAUR(IW,ih+1)-TAUR(IW,ih)
cs      WRITE(*,*)'TAURC',TAURC(ih)
		ENDDO
C**********************************gas********tau******************
		CALL gas(NLYRS,iatmos,suhei,hl,MAXL,MAXCLY,ZET,
     *	TEM,PRES,AIR,AIRM,H2O,O3)
		AO3=0.
		DO I=1,NLYRS
			AO3=AO3+O3(I)
		ENDDO
		DO I=1,NLYRS
			DO3(IW,I)=O3(I)/AO3*UO3(IW)
cs      WRITE(*,*)'DO3',DO3(IW,I)
		ENDDO 
		tmol=0.
		DO ih=1,NLYRS
			taugas(ih)=DO3(IW,ih)
			tmol=tmol+taugas(ih)
		ENDDO
C**************************************gas****************tau*****
CD****Divide aerosol optical depth into sublayers
		KSIMP=31
		DO ISD=1,NSD
			do ih=1,NLYRS
				WA(ISD,ih)=0
				hdels =  (hl(ih+1)- hl(ih))/(KSIMP-1)
CD       WRITE(*,*) H0V(ISD),WV(ISD),h,'H0V(ISD),WV(ISD),h'
				do IK=1,KSIMP
					dh = hl(ih)+hdels *(IK-1)
CS        WRITE(*,*)'h',dh
					II=IK/2
					IF(II*2.LT.IK) AD=4./3.
					IF(II*2.EQ.IK) AD=2./3.
					IF(IK.EQ.1.OR.IK.EQ.KSIMP) AD=1./3.
CS        WRITE(*,*)'AD',AD
CS        WRITE(*,*)'H0V(ISD),WV(ISD)'
CS        WRITE(*,*)H0V(ISD),WV(ISD)
					k=gaussd(H0V(ISD),WV(ISD),dh)
CS        WRITE(*,*)'gauss,h',k,dh
					WA(ISD,ih)=WA(ISD,ih)+AD*gaussd(H0V(ISD),WV(ISD),dh)
				enddo
CS        WRITE(*,*)'WA',WA(ISD,ih)
				WAT(ISD)=0
			enddo
			DO ih=1,NLYRS
				WAT(ISD)=WAT(ISD)+WA(ISD,ih)
			ENDDO
CS       WRITE(*,*)'WAT',WAT(ISD)
			DO ih=1,NLYRS
				IF(ITOFF.EQ.0)THEN
					TAUA(ISD,ih)=EXTV(ISD)*WA(ISD,ih)/WAT(ISD)
cs        WRITE(*,*)'rat',WA(ISD,ih),WAT(ISD)
cs        WRITE(*,*)'TAUA',hl(ih),TAUA(ISD,ih)
cs        WRITE(*,*)'SSAV',SSAV(ISD)
				ENDIF
				IF(ITOFF.EQ.1)TAUA(ISD,ih)=1.e-30
CD       WRITE(*,*) EXT(ISD),TAUA(ISD,Ih),ISD,ih,
CD     & 'EXT(ISD),TAUA(ISD,Ih),TAURC(ih)ISD,ih'
			ENDDO
		ENDDO
cs      WRITE(*,*)
CS      WRITE(*,*)'EXTV(ISD)',EXTV
CS      WRITE(*,*)'hl',hl
CS      WRITE(*,*)'TAUA'
CS      DO ih=1,NLYRS
CS      WRITE(*,*)hl(ih),TAUA(1,ih)
CS      ENDDO
C****Divide aerosol optical depth into sublayers
		TH(1)=0
		THS(1)=0
		DO ih=1,NLYRS
			TH(ih+1)=0
			THS(ih)=0
			THE(ih)=0
			THC=0
			DO ISD=1,NSD
				THC=THC+TAUA(ISD,Ih)
				THS(ih)=THS(ih)+SSAV(ih)*TAUA(ISD,Ih)
				THE(ih)=THE(ih)+TAUA(ISD,Ih)
CD       WRITE(*,*)THS(ih),TAUA(ISD,Ih),TAURC(ih),SSAV(ISD),ih,
CD     &'THS(ih),TAUA(ISD,Ih),TAURC(ih),SSAV(ISD)'  
			ENDDO
			TH(ih+1)=TH(ih)+THC+TAURC(ih)+taugas(ih)
			IF(ITOFF.EQ.1)TH(ih+1)=0.0
			THS(ih)=THS(ih)+TAURC(ih)
			THE(ih)=THE(ih)+TAURC(ih)
			DO ISD=1,NSD
				WD(ih,ISD)= TAUA(ISD,Ih)*SSAV(ISD)/THE(ih)
cs        WRITE(*,*)'TAUAS',TAUA(ISD,Ih)*SSAV(ISD)
cs        WRITE(*,*)'THS',THS(ih)
				SSAM(ISD)=THS(ih)/THE(ih)
cs        WRITE(*,*)'SSAM',SSAM(ISD)
CD        WRITE(*,*)TAUA(ISD,Ih),SSAV(ISD),THS(ih),
CD     &'TAUA(ISD,Ih),SSAV(ISD),THS(ih)'
CD        WRITE(*,*) WD(ih,ISD),TH(ih),ih,ISD,'WD(ih,ISD),TH(ih),ih,ISD'
			ENDDO
			WD(ih,NSD+1)= TAURC(ih)/THE(ih)
cs        WRITE(*,*)'TAURC',TAURC(ih)
cs        WRITE(*,*)'THS',THS(ih)
cs        WRITE(*,*)'THS',THS(ih)
CD        WRITE(*,*) WD(ih,NSD+1),ih,'WD(ih,NSD+1),ih'
		ENDDO
		xl=hlyr(2)*1000.0
		CALL rayleia(DBLE(WAV(IW)),DBLE(xl),DBLE(LAT),TAURRR)
cs       WRITE(*,*)'TAURRR',TAURRR
		DO ISD=1,NSD
			JP1(ISD)=2.0
			ECH1(ISD)=H0V(ISD)
			EXTH(ISD)=EXTV(ISD)
cs       tetot=tetot+EXTV(ISD)
			SSAH(ISD)=SSAV(ISD)
			IF(ISD.EQ.1)THEN
				EXTH(ISD)=EXTV(ISD)+tmol
				x2=tmol/(EXTH(ISD)*SSAV(ISD))
				x2=x2+1./SSAV(ISD)
				SSAH(ISD)=1./x2
			ENDIF
		ENDDO
		JP1(NSD+1)=1.0
		ECH1(NSD+1)=8.0
		KNAV=3
		za=2.
		EXTH(NSD+1)=TAURRR
		SSAH(NSD+1)=1.0
		IF(ITOFF.EQ.1)THEN
			DO i=1,NSD+1
				EXTH(i)=1.0e-30
			ENDDO
		ENDIF
CS**************************************************************
		isurf=2
c********isaut-surface**type:1-ocean, 2-land********************
		isaut=isurf
c********wind-wind*speed****************************************
		wind=WSPD
c********iwat-index*of*refraction*of*water**********************
		iwat =133
c********rsurf-*Lambertian*reflectance**of*water****************
		rsurf=0.02
c********igrd-index*of*refraction*of*surface********************
		igrd=150 
c********vk*gteta*rho*are*parameters*of*BRF*model*************** 
		vk=KBW(IW)
		gteta=THBW(IW)
cs       gteta=THBW(IW)-1.
		rho=R0BW(IW) 
		IF(INDG2.EQ.3) THEN
			rho=BRF(1)
			vt=BRF(2)
			gteta=BRF(3)
		ENDIF 
c********anb*bnb*are*parameters*of*polar*BRF*model**************
		anb=0.0
		bnb=60.00000000
c********NM*is*number*of*aerosol*components*********************
		NM=NSD
c********iop=0*lives Stokes*vektor*in*scattering*plane*1*-*meridian*plane
		iop=1
c********IREAD*is*always**GE*0**and*related**to****interpolation****
		IREAD=1
c********
		IPROF=0
		NQDR=91
		ITRONC=1
		NG1=31
		NG2=15
		NT=30
		DO j=1,NT-1
			DO m=1, NM+1
				WD1(j,m)=WD(j,m)
			ENDDO
		ENDDO
cs       WRITE(*,*)'WD1(j,m)',WD1
		DO j=1,NT
			TH1(j)=TH(j)
		ENDDO
		IF(IREAD.GE.0) THEN
			NGN=NG1
			DO j=1,2*NGN+1
				DO ISD=1,NSD
					pha11(ISD,J-NGN-1)=PFV(2*NGN+1-J+1,ISD)
					pha12(ISD,J-NGN-1)=PFV12(2*NGN+1-J+1,ISD)
					pha22(ISD,J-NGN-1)=PFV22(2*NGN+1-J+1,ISD)
					pha33(ISD,J-NGN-1)=PFV33(2*NGN+1-J+1,ISD)
				ENDDO
			ENDDO
			ik=NANG+1
			DO i=1,NANG
				ANGL(ik-1)=ANG(i)
				ik=ik-1
			ENDDO
			ik=NANG+1
			DO j=1,NSD
				DO i=1,NANG
					PFVC(ik-1,j)=PFV(i,j)
					PFVC12(ik-1,j)=PFV12(i,j)
					PFVC22(ik-1,j)=PFV22(i,j)
					PFVC33(ik-1,j)=PFV33(i,j)
					ik=ik-1      
				ENDDO
			ENDDO
		ENDIF
		NBV=NFI*NA1U
		ik=0
		DO i1=1,NA1U
			DO j1=1,NFI
				vis(j1+ik)=180.0-ACOS(AM1U(i1))/RAD
				fiv(j1+ik)=FAI(j1)
			ENDDO
			ik=i1*NFI
		ENDDO
cs       WRITE(*,*)'NFI,NA1U',NFI,NA1U
		DO i1=1,NFI*NA1U
			IF(fiv(i1).GE.180.0)THEN
				fiv(i1)=fiv(i1)-180.0
			ELSE
				fiv(i1)=fiv(i1)+180.0
			ENDIF
		ENDDO
		open(8,file="Sinyuk_input")
		write(8,*)WAV(IW),'  WAVE'
cs      WRITE(*,*) WAV(IW), "   WAVE"
cs      WRITE(*,*) IPRI, "   IPRI"
cs      WRITE(*,*) IMSC, "   IMSC"
cs      WRITE(*,*) IPROF,'IPROF'
cs      WRITE(*,*) NBV,  "   NBV"
cs      DO IV=1,NBV
cs      WRITE(*,28) vis(IV),fiv(IV),IV, 
cs     &'vis(IV),fiv(IV), IV'
cs      ENDDO 
cs      WRITE(*,*) tetas,"   tetas"
cs      WRITE(*,*) isaut ,"  isaut"
cs      WRITE(*,*)  wind, "   wind"
cs      WRITE(*,*)  iwat, "   iwat"
cs      WRITE(*,*)  rsurf,"   rsurf"
cs      WRITE(*,*)  igrd, "   igrd"
cs      WRITE(*,*)  vk,   "   vk"
cs      WRITE(*,*)  gteta,"   vgteta"
cs      WRITE(*,*)  rho,  "   rho"
cs      WRITE(*,*) anb, 'anb'
cs      WRITE(*,*) bnb, 'bnb'
cs      WRITE(*,*)  iop,   "   iop"
cs      WRITE(*,*)  NG1,   "   NG1"
cs      WRITE(*,*)  NG2,   "   NG2"
cs      WRITE(*,*)  IREAD,   "   IREAD"
cs      WRITE(*,*)  NM,   "   NM"  
cs       DO ISD=1,NM
cs       Write(*,29) EXTH(ISD),JP1(ISD),ECH1(ISD),SSAH(ISD),
cs     &'EXT1(ISD),JP1(ISD),ECH1(ISD),SSA1(ISD)'
cs         IF(ISD.LE.NM)THEN
cs         DO j=1,NANG
cs        WRITE(*,30) PFVC(J,ISD),PFVC12(J,ISD),PFVC22(J,ISD),
cs     &PFVC33(J,ISD),J
cs     &,"  pha11(ISD,J),pha12(ISD,J),pha22(ISD,J),pha33(ISD,J),J,"
cs        ENDDO
cs      WRITE(*,*)
cs        ENDIF
cs       ENDDO
cs       Write(*,*) NT, 'NT'
cs       Write(*,*) KNAV, za, NAV,'KNAV, za, NAV'
cs       WRITE(*,*)'   '
cs      open(8,file="Sinyuk_input")
cs      DO i=1,NANG
cs      WRITE(*,*)ANGL(i),PFV(i,1)
cs      ENDDO
cs      IF(IMSC.EQ.0)write(8,*)WAV(IW),'  WAVE'
		DO ISZA=1,NZSZA0(IW)
			tetas=ACOS(AM0(ISZA))/RAD
			IF(IMSC.EQ.0)THEN
				CALL OS_HERMAN(IPRI,IMSC,IPROF,KNAV,za,NT,
     &			tmol,tetas,NBV,vis,fiv,isaut,wind,
     &			iwat,rsurf,igrd,vk,gteta,rho,anb,bnb,
     &			NM,pha11,pha12,pha22,pha33,
     &			EXTH,SSAH,JP1,ECH1,WD1,TH1,
     &			NG1,NG2,
     &			IREAD,PFVC,PFVC12,PFVC22,PFVC33,ANGL,NANG,NQDR,
     &			ITRONC,
     &			thd,iop,SLout,SQout,SUout,SLPout,
     &			tetot,NAV,UFT,DFT,UFG,DFG)
			ENDIF
			IF (IMSC.EQ.1)THEN
				ITRONC1=0
				NG11=41
				NG21=25
				IMSC1=1
				CALL OS_HERMAN_1(IPRI,IMSC1,IPROF,KNAV,za,NT,
     &			tmol,tetas,NBV,vis,fiv,isaut,wind,
     &			iwat,rsurf,igrd,vk,gteta,rho,anb,bnb,
     &			NM,pha11,pha12,pha22,pha33,
     &			EXTH,SSAH,JP1,ECH1,WD1,TH1,
     &			NG11,NG21,
     &			IREAD,PFVC,PFVC12,PFVC22,PFVC33,ANGL,NANG,NQDR,
     &			ITRONC1,
     &			thd,iop,SLout,SQout,SUout,SLPout,
     &			tetot,NAV,UFT,DFT,UFG,DFG)
			ENDIF
cs       WRITE(*,*)'SLout'
cs       WRITE(*,*)SLout
CS       WRITE(*,*)'thd',thd
cs         WRITE(*,*)WAV(IW)
cs       WRITE(*,*)
cs       DO i1=1,NBV
cs       WRITE(*,*)thd(i1),SLout(i1)
cs       ENDDO
cs       DO ISZA=1,NZSZA0(IW)
			DO LU=1,NTAU(IW)
				ik=0
				DO IU=1,NUMUY0(IW)
					DO  J=1,NPHIY0(IW)
						AI(IU,ISZA,J,LU)=SLout(J+ik)
cs       WRITE(*,*)ISZA,LU,IU,J,AI(IU,ISZA,J,LU)      
						tmp=SLPout(J+ik)/SLout(J+ik)
						AIP(IU,ISZA,J,LU)=tmp+1.0
					ENDDO
					ik=IU*NPHIY0(IW)
				ENDDO
			ENDDO
		ENDDO 
C      ENDIF IPOL
      ENDIF 
C*****************************************************************************
C*****************************************************************************
      IF(ITOFF.EQ.1.AND.INDG1.EQ.0)THEN
cs       write(6,*)'IN'
		DO 1203 ISZA=1,NZSZA0(IW)
		DO 1203 LU=1,NTAU(IW)
		DO 1203 IU=1,NUMUY0(IW)
		DO 1203 J=1,NPHIY0(IW)
			AI(IU,ISZA,J,LU)=AI(IU,ISZA,J,LU)/AM0(ISZA)
cs       AI(IU,ISZA,J,LU)=AI(IU,ISZA,J,LU)
 1203		CONTINUE
      ENDIF

      IF(ITOFF.EQ.1.AND.INDG1.EQ.3)THEN
		DO 1205 ISZA=1,NZSZA0(IW)
		DO 1205 LU=1,NTAU(IW)
		DO 1205 IU=1,NUMUY0(IW)
		DO 1205 J=1,NPHIY0(IW)
			AI(IU,ISZA,J,LU)=AI(IU,ISZA,J,LU)/AM0(ISZA)
cs       AI(IU,ISZA,J,LU)=AI(IU,ISZA,J,LU)
 1205		CONTINUE
      ENDIF
C******************************************************************************
c*********************************************
c***  addition direct light to intensity at ASIM=0  ***
c*********************************************
cs          IF(IW.NE.2)THEN
      DOMEGA=-2.*PI*(COS(0.6*RAD)-COS(0.))
CD?       if(IMSC.eq.0)THEN
      IF(lvect)THEN
cs         write(*,*)'tetot',tetot
         UTAUN(1)=tetot
      ENDIF
 	
	DO 1201 LU=1,NTAU(IW)
 	DO 1201 ISZA=1,NZSZA0(IW)
 	DO 1201 IU=1,NUMUY0(IW)
      DO 1201 J=1,NPHIY0(IW)
		IF(ABS(ZENOUT0(IW,IU)-SZA0(IW,ISZA)).LE.1.0E-10) THEN
			IF(PHI0(IW,J).LE.1.0E-10) THEN
cs      write(6,*)'phi',ZENOUT0(IW,IU),PHI0(IW,J)
				IPH=J
				IF(IDN.EQ.1)
     &			AI(IU,ISZA,J,LU)=-CSZA0(IW,ISZA)
     &			*LOG(EXP(- UTAUN(LU)/CSZA0(IW,ISZA))
     &			+AI(IU,ISZA,J,LU)*DOMEGA/FBEAM)
				IF(IDN.EQ.2)
     &			AI(IU,ISZA,J,LU)=FBEAM*EXP(- UTAUN(LU)/CSZA0(IW,ISZA))+
     &			AI(IU,ISZA,J,LU)*DOMEGA
			ENDIF  
		ENDIF    
 1201	CONTINUE
CD?       endif
      ISKY=0
CD?       if(IMSC.eq.0)then
!       IF(IPOLP.EQ.0)THEN
      IF(.not.lpolr)THEN
		DO ITAU=1,NTAU(IW)
			IF(ITAU.GE.1)THEN
cs        write(6,*)'ITAU',ITAU
				DO ISZA=1,NSZA(IW,ITAU)
cs        write(6,*)'IW,ITAU,ISZA,NUMUY(IW,ITAU,ISZA)'
cs     &,IW,ITAU,ISZA,NUMUY(IW,ITAU,ISZA)
					DO  IOZA=1,NUMUY(IW,ITAU,ISZA)
cs         if(IMSC.eq.0.AND.IPRI.EQ.1) THEN
cs       WRITE(*,*) "WAVELENGTH:"
cs      WRITE(*,26) WAV(IW)
cs       write(*,*) "HIGHT of observations (km):"
cs       write(*,26) houtput(IW,ITAU)
cs       write(*,*) "SOLAR, OBSERVATION Zenith angles (degrees):"
cs       IF(IPRI2.EQ.1) WRITE(*,*) LSZA(IW,ITAU,ISZA)
cs     & ,LOZA(IW,ITAU,ISZA,IOZA),ITAU,
cs     &' LSZA(IW,ITAU,ISZA),LOZA(IW,ITAU,ISZA),ITAU'
cs        IF(IPRI2.EQ.1)  write(*,26) SZA0(IW,LSZA(IW,ITAU,ISZA)),
cs     & ZENOUT0(IW,LOZA(IW,ITAU,ISZA,IOZA))
cs       write(*,26) SZA(IW,ITAU,ISZA),ZENOUTY(IW,ITAU,ISZA,IOZA)
cs       write(*,*) "AZIMUTH angles of Observations (degrees):"
cs       WRITE(*,*) NPHI(IW,ITAU,ISZA,IOZA)," number of angles"
cs       IF(IPRI2.EQ.1) write(*,*) (LPHI(IW,ITAU,ISZA,IOZA,IPHI)
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs       IF(IPRI2.EQ.1) write(*,26) (PHI0(IW,LPHI(IW,ITAU,ISZA,IOZA,IPHI))
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs       write(*,26) (PHI(IW,ITAU,ISZA,IOZA,IPHI)
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs       write(*,*) "RADIANCES:"
cs       IF(IMSC.EQ.0)THEN
cs       WRITE(*,26) WAV(IW)
cs       write(*,25) (AI(LOZA(IW,ITAU,ISZA,IOZA)
cs     &,LSZA(IW,ITAU,ISZA)
cs     &,LPHI(IW,ITAU,ISZA,IOZA,IPHI),ITAU)
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs      ENDIF
cs           ENDIF
CD         WRITE(*,*) IW,ITAU,ISZA,IOZA,' IW,ITAU,ISZA,IOZA'
CD         WRITE(*,*) NPHI(IW,ITAU,ISZA,IOZA),'NPHI...'         
						DO  IPHI=1,NPHI(IW,ITAU,ISZA,IOZA)
							ISKY=ISKY+1
							FPP(ISKY)=AI(LOZA(IW,ITAU,ISZA,IOZA)
     &						,LSZA(IW,ITAU,ISZA)
     &						,LPHI(IW,ITAU,ISZA,IOZA,IPHI),ITAU)
						ENDDO
cs          WRITE(*,*) FPP(ISKY),ISKY,'ISKY'
					ENDDO
				ENDDO
			ENDIF
		ENDDO
      ENDIF
cs       WRITE(*,*)
      IF(lpolr)THEN
		DO ITAU=1,NTAU(IW)
			IF(ITAU.EQ.1)THEN
cs        write(6,*)'ITAU',ITAU
				DO ISZA=1,NSZA(IW,ITAU)
					DO  IOZA=1,NUMUY(IW,ITAU,ISZA)
cs         if(IMSC.eq.0.AND.IPRI.EQ.1) THEN
cs       WRITE(*,*) "WAVELENGTH:"
cs       WRITE(*,26) WAV(IW)
cs       write(*,*) "HIGHT of observations (km):"
cs       write(*,26) houtput(IW,ITAU)
cs       write(*,*) "SOLAR, OBSERVATION Zenith angles (degrees):"
cs       IF(IPRI2.EQ.1) WRITE(*,*) LSZA(IW,ITAU,ISZA)
cs     & ,LOZA(IW,ITAU,ISZA,IOZA),ITAU,
cs     &' LSZA(IW,ITAU,ISZA),LOZA(IW,ITAU,ISZA),ITAU'
cs        IF(IPRI2.EQ.1)  write(*,26) SZA0(IW,LSZA(IW,ITAU,ISZA)),
cs     & ZENOUT0(IW,LOZA(IW,ITAU,ISZA,IOZA))
cs       write(*,26) SZA(IW,ITAU,ISZA),ZENOUTY(IW,ITAU,ISZA,IOZA)
cs       write(*,*) "AZIMUTH angles of Observations (degrees):"
cs       WRITE(*,*) NPHI(IW,ITAU,ISZA,IOZA)," number of angles"
cs       IF(IPRI2.EQ.1) write(*,*) (LPHI(IW,ITAU,ISZA,IOZA,IPHI)
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs       IF(IPRI2.EQ.1) write(*,26) (PHI0(IW,LPHI(IW,ITAU,ISZA,IOZA,IPHI))
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs       write(*,26) (PHI(IW,ITAU,ISZA,IOZA,IPHI)
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs       write(*,*) "RADIANCES:"
cs        IF(IMSC.EQ.0)THEN
cs       WRITE(*,26) WAV(IW)
cs       write(*,25) (AI(LOZA(IW,ITAU,ISZA,IOZA)
cs     &,LSZA(IW,ITAU,ISZA)
cs     &,LPHI(IW,ITAU,ISZA,IOZA,IPHI),ITAU)
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs      ENDIF
cs           ENDIF
CD         WRITE(*,*) IW,ITAU,ISZA,IOZA,' IW,ITAU,ISZA,IOZA'
CD         WRITE(*,*) NPHI(IW,ITAU,ISZA,IOZA),'NPHI...'         
						DO  IPHI=1,NPHI(IW,ITAU,ISZA,IOZA)
							ISKY=ISKY+1
							FPP(ISKY)=AI(LOZA(IW,ITAU,ISZA,IOZA)
     &						,LSZA(IW,ITAU,ISZA)
     &						,LPHI(IW,ITAU,ISZA,IOZA,IPHI),ITAU)
						ENDDO
cs          WRITE(*,*) FPP(ISKY),ISKY,'ISKY'
					ENDDO
				ENDDO
			ENDIF
		ENDDO
      ENDIF
cs       write(*,*)
cs       ENDIF
cs       IF(IW.EQ.2)ISKY=0
      IF(lpolr)THEN
		IPOLPR(IW)=1
		DO ITAU=1,NTAU(IW)
			IF(ITAU.EQ.2)THEN
cs       write(6,*)'ITAU',ITAU
				DO ISZA=1,NSZA(IW,ITAU)
					DO  IOZA=1,NUMUY(IW,ITAU,ISZA)
cs         if(IMSC.eq.0.AND.IPRI.EQ.1) THEN
cs       WRITE(*,*) "WAVELENGTH:"
cs       WRITE(*,26) WAV(IW)
cs       write(*,*) "HIGHT of observations (km):"
cs       write(*,26) houtput(IW,ITAU)
cs       write(*,*) "SOLAR, OBSERVATION Zenith angles (degrees):"
cs       IF(IPRI2.EQ.1) WRITE(*,*) LSZA(IW,ITAU,ISZA)
cs     & ,LOZA(IW,ITAU,ISZA,IOZA),ITAU,
cs     &' LSZA(IW,ITAU,ISZA),LOZA(IW,ITAU,ISZA),ITAU'
cs        IF(IPRI2.EQ.1)  write(*,26) SZA0(IW,LSZA(IW,ITAU,ISZA)),
cs     & ZENOUT0(IW,LOZA(IW,ITAU,ISZA,IOZA))
cs       write(*,26) SZA(IW,ITAU,ISZA),ZENOUTY(IW,ITAU,ISZA,IOZA)
cs       write(*,*) "AZIMUTH angles of Observations (degrees):"
cs       WRITE(*,*) NPHI(IW,ITAU,ISZA,IOZA)," number of angles"
cs       IF(IPRI2.EQ.1) write(*,*) (LPHI(IW,ITAU,ISZA,IOZA,IPHI)
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs       IF(IPRI2.EQ.1) write(*,26) (PHI0(IW,LPHI(IW,ITAU,ISZA,IOZA,IPHI))
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs       write(*,26) (PHI(IW,ITAU,ISZA,IOZA,IPHI)
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs       write(*,*) "RADIANCES:"
cs       write(*,25) (AIP(LOZA(IW,ITAU,ISZA,IOZA)
cs     &,LSZA(IW,ITAU,ISZA)
cs     &,LPHI(IW,ITAU,ISZA,IOZA,IPHI),ITAU)
cs     &,IPHI=1,NPHI(IW,ITAU,ISZA,IOZA))
cs        ENDIF
CD         WRITE(*,*) IW,ITAU,ISZA,IOZA,' IW,ITAU,ISZA,IOZA'
CD         WRITE(*,*) NPHI(IW,ITAU,ISZA,IOZA),'NPHI...'
						DO  IPHI=1,NPHI(IW,ITAU,ISZA,IOZA)
							ISKY=ISKY+1
							FPP(ISKY)=AIP(LOZA(IW,ITAU,ISZA,IOZA)
     &						,LSZA(IW,ITAU,ISZA)
     &						,LPHI(IW,ITAU,ISZA,IOZA,IPHI),ITAU)
						ENDDO
cs           write(6,*)LOZA(IW,ITAU,ISZA,IOZA),LSZA(IW,ITAU,ISZA),
cs     &LPHI(IW,ITAU,ISZA,IOZA,IPHI),ITAU
cs          WRITE(*,*) FPP(ISKY),ISKY,'ISKY'
					ENDDO
				ENDDO
			ENDIF
		ENDDO
      ENDIF
CD?      endif
cs1       if(IMSC.eq.0.AND.IPRI.EQ.1) then
      DO ISZA=1,NZSZA0(IW)
cs       WRITE(*,*) "SOLAR ZENITH ANGLE: ", SZA0(IW,ISZA)
cs       WRITE(*,*) " Downward FLUX:"       
		FLFDOWN=FLXD(ISZA,NTAUN)/FBEAM*SOLC(IW)
          IF(lvect)THEN
			FLFDOWN=DFG*PI/FBEAM*SOLC(IW)
		ENDIF
cs       WRITE(*,25) FLFDOWN
cs       Upward new
cs       WRITE(*,*) " Upward FLUX:" 
		FLFUP=FLXU(ISZA,1)/FBEAM*SOLC(IW)
		IF(lvect)THEN
			FLFUP=UFG*PI/FBEAM*SOLC(IW)
		ENDIF
cs       WRITE(*,25) FLFUP
		FLFDIR=SOLC(IW)*CSZA0(IW,ISZA)*
     &	EXP(-UTAUN(NTAUN)/CSZA0(IW,ISZA))
		FLFDIR=(FLFDOWN-FLFDIR)/FLFDOWN
cd       WRITE(*,*) " (Downward - Direct)/Downward :" 
cs       WRITE(*,25) FLFDIR
		IF(ITOFF.EQ.1)THEN
cs       FLFUP=FLXU(ISZA,NTAUN)/(PI*CSZA0(IW,ISZA))
			FLFUP=FLXU(ISZA,NTAUN)/FLXD(ISZA,NTAUN)
cs       WRITE(*,*)'FLXU,FLXD',
cs     &FLXU(ISZA,NTAUN),FLXD(ISZA,NTAUN)
cs       WRITE(*,*)'FLXU,CSZA0,SOLC(IW)',FLXU(ISZA,NTAUN)
cs     &,CSZA0(IW,ISZA),SOLC(IW)
			IF(lvect)THEN
				FLFUP=UFG/DFG
cs       FLFUP=UFG/(PI*CSZA0(IW,ISZA))
			ENDIF
cs**********RADFORCE*CORRECTION************************************************************
			IF(IRFS.EQ.1.AND.INDG.GT.0)THEN
				irs=irs+1
cs**********LAST****CORRECTION*************************************************************
				IF (irs.LE.7) THEN
					ALBMODIS_1(irs)=FLFUP
cs      write(6,*)'irs,ALBMODIS_1(irs)',irs,ALBMODIS_1(irs)
				ENDIF
			ENDIF
cs***************************************************************************************
cs       write(6,*)'ALBEDO',FLFUP
		ENDIF
      ENDDO
cs       write(6,*)'END OF DISDS'
cs1       endif
CS
c********************************************* 
      III=III+1
   25 FORMAT(7E16.7) 
   26 FORMAT(7F7.2)
    1 FORMAT(7E16.7)
   28 FORMAT(2f12.4,i3,a22)
   29 FORMAT(f15.10,i5,2f15.10,a42)
   30 FORMAT(4f15.10,i5,a55)
      RETURN
      END
C**********************************************************

C-----------------------------------------------------------------------
C--------------------RADTRS---------------------------------------------------
     
      SUBROUTINE RADTRS(NMOM,NSTR,NSTR2,PP,ANG,IANGL,RADIUS2,SD2,
     &SHAPE,NSHAPE,
     $ NSD,NBIN,NW,WAVE,LAMB1,LAMB2,UMU1,DSOL,SSA_1,NSSA,             
     $ CSOL,UH2O,UO3CM,UCO2,ZSUR,NLAY,NLEVEL,RREAL1,RIMAG1,
     $ NOAERO,SCL1,ALTIT,FLXINT,TOTAER,FLXNU,CORSOL)


C This subroutine is for calculating the instantaneous aerosol 
C radiative forcing
C
C----------------------------------------------------------------------
C  PARAMETERS
C----------------------------------------------------------------------
C
      PARAMETER (MAXCMU=12,MAXULV=10,MAXUMU=12,MAXCLY=10,MAXPHI=1)

      PARAMETER (NKDMAX=7,NBABS=3,NBNU=208,NBTEMP=14)
      PARAMETER (MXCMU=MAXCMU,MXULV=MAXULV,MXUMU=MAXUMU,MXCLY=MAXCLY,
     $           MXPHI=MAXPHI)
      PARAMETER (MI=MXCMU/2,MI9M2=9*MI-2,NNLYRI= MXCMU*MXCLY )
      
!tl      PARAMETER (MAXCL=35,KW=10,KSD=3,KANG=83,KMD=3,KPAR=100,!from FORWN
      PARAMETER (MAXCL=35,KW=10,KSD=3,KANG=181,KMD=3,KPAR=100,!from FORWN
     $           KNA0=2,KSHAPE=2)
      PARAMETER (PI1=3.141592654,RAD1=PI1/180.0)	
      PARAMETER (MAXMOM=200,MAXUM=100)	
C      PARAMETER (NWM=10,NDM=23,NWM1=7,NWM2=3)	
      PARAMETER (NWM=10,NDM=23) !ALBEDO MODIS	
C
C----------------------------------------------------------------------
C  DISCRETE ORDINATES INPUT VARIABLES
C----------------------------------------------------------------------
C
      CHARACTER  HEADER*127
      LOGICAL  DELTAM, LAMBER, NOPLNK, ONLYFL, PRNT(7), USRANG, USRTAU
      INTEGER  IBCND, NLYR,NUMU,NSTR,NPHI,NTAU,VAL(MAXULV)
      REAL     ACCUR, ALBEDO11, BTEMP, DTAUC( MAXCLY ), FBEAM, FISOT,
     $         HL( 0:MAXCMU ), PHI( MAXPHI ), PMOM( 0:MAXCMU, MAXCLY ),
     $         PHI0, SSALB( MAXCLY ), TEMPER( 0:MAXCLY ), TEMIS, TTEMP,
     $         WVNMLO, WVNMHI, UMU( MAXUMU ), UMU0, UTAU( MAXULV )

      REAL     RFLDIR( MAXULV ), RFLDN( MAXULV ), FLUP( MAXULV ),
     $         UAVG( MAXULV ), DFDT( MAXULV ), U0U( MAXUMU, MAXULV ),
     $         UU( MAXUMU, MAXULV, MAXPHI ), ALBMED( MAXUMU ),
     $         TRNMED( MAXUMU )
	DOUBLE PRECISION CORSOL
C
C---------------------------------------------------------------------
C  INTERNAL VARIABLES: 
C---------------------------------------------------------------------
C
C      LOGICAL AERO2
      DIMENSION AL(NKDMAX,NBABS),RKLM(NKDMAX,MAXCLY,NBABS),
     $          AERTAU(MAXCLY),PMOMA(MAXCMU,MAXCLY),AERP(MAXCLY),
     $          ALB(NBNU),RAY(MAXULV),
     $          SA(NBTEMP,NBNU,NBABS),SOI(NBTEMP,NBNU,NBABS),
     $          ALTIT(MAXULV),PRES(MAXULV),TT(MAXULV),
     $          DH(MAXULV),UD(NBABS,MAXULV),
     $          DP(MAXULV),REFGLIT(MAXUMU,MAXULV),
     $          FDN(MAXULV),FUP(MAXULV),DIRRADNU(MAXUMU,MAXULV),
     $          Y1(NBTEMP),TPAR(NBTEMP),X1(NBTEMP),
     $          WL(7),SHAPE(KSHAPE)
      DOUBLE PRECISION GKL(0:NKDMAX),RKL(0:NKDMAX),ALDP(NKDMAX),
     $          H(0:NKDMAX),RKLMDP(NKDMAX),TMP(NBTEMP),FSOL(NBNU)
C
C
C******************NEW************************************************
      REAL LINEAR2,gaussd
      DIMENSION ALBEDOA(KW),RREAL_1(KW),RIMAG_1(KW),SSA_2(KW)
     &,RREAL1(KW,MAXCL,KSD),RIMAG1(KW,MAXCL,KSD)
     &,SSA_1(KW,MAXCL,KSD),RADIUS2(KSD,MAXCL,KPAR)
     &,SD2(KSD,MAXCL,KPAR),WAVEMOD(NWM),ALBMODIS1(NWM,NDM)
     &,ALBMODIS_1(NWM),hlyr(MAXCL+1)  
      REAL RREAL,RIMAG,GG0,AOD,DSOL,UMU1
      DOUBLE PRECISION TOTAER
      CHARACTER CH2*100,DAY*2,MONT*2,CH3*100,SITE*64,CH4*100
     &,FICHE*64,SITE1*64,CH*64,PP*10
      INTEGER CONT,Q,QQ,IATMOS,COND,NLAY,NMOM,NSTR2
     &,NTAU1(KW),NSZA(KW,MAXCL),NBIN(KSD),IS(KSD),IEL	
c      REAL JDAY,MONTH 	
      REAL O3T(KW),H2OT(KW),SZA(KW,MAXCL,KNA0),SZA0(KNA0),
     &SZA1(MAXCL),SZA2(NW),SZA3,CSZA3,NSSA
      DIMENSION WAVE(KW),ANG(KANG),RADIUS1(KPAR)
     &,SSA(NBNU,MAXCLY,KSD),PF(KANG,NBNU,MAXCLY,KSD)
     &,DLP(KANG,NBNU,MAXCLY,KSD),SD(KPAR)
     &,PF12(KANG,NBNU,MAXCLY,KSD),PF22(KANG,NBNU,MAXCLY,KSD)
     &,PF33(KANG,NBNU,MAXCLY,KSD),PF34(KANG,NBNU,MAXCLY,KSD)
     &,PF44(KANG,NBNU,MAXCLY,KSD),EXT(NBNU,MAXCLY,KSD),EXTL(MAXCLY)
     &,SSAL(MAXCLY)
     &,ANGL(KANG),CS(KANG),YY(KANG),GG(MAXMOM)
     &,PFL(KANG,MAXCLY),PFL12(KANG,MAXCLY)
     &,PFL22(KANG,MAXCLY),PFL33(KANG,MAXCLY)
     &,PFL34(KANG,MAXCLY),PFL44(KANG,MAXCLY)
      DIMENSION
     & PTP11(KANG),PTP12(KANG),PTP22(KANG)
     &,PTP33(KANG),PTP34(KANG),PTP44(KANG)	
    
      INTEGER ITIME1,ITIME,ITIME2,ITIME4   !TIME 


C---------------------------------------------------------------------
C  DISCRETE ORDINATES INTERNAL VARIABLES: 
C---------------------------------------------------------------------
C
      INTEGER IPVT( NNLYRI ), LAYRU( MXULV )
      REAL    ALBSAV( MXUMU ), AMB( MI,MI ), APB( MI,MI ),
     $        ARRAY( MXCMU,MXCMU ), B( NNLYRI ), BDR( MI,0:MI ),
     $        BEM( MI ), CBAND( MI9M2,NNLYRI ), CC( MXCMU,MXCMU ),
     $        CMU( MXCMU ), CWT( MXCMU ), EMU( MXUMU ), EVAL( MI ),
     $        EVECC( MXCMU, MXCMU ), EXPBEA( 0:MXCLY ), FLYR( MXCLY ),
     $        FLDN( MXULV ), FLDIR( MXULV ), GL( 0:MXCMU,MXCLY ),
     $        GC( MXCMU,MXCMU,MXCLY ), GU( MXUMU,MXCMU,MXCLY ),
     $        HLPR( 0:MXCMU ), KK( MXCMU,MXCLY ), LL( MXCMU,MXCLY ),
     $        OPRIM( MXCLY ), PHIRAD( MXPHI ), PKAG( 0:MXCLY ),
     $        PSI( MXCMU ), RMU( MXUMU,0:MI ), TAUC( 0:MXCLY ),
     $        TAUCPR( 0:MXCLY ), U0C( MXCMU,MXULV ), UTAUPR( MXULV ),
     $        UUM( MXUMU,MXULV,0:MXCMU ), WK( MXCMU ), XR0( MXCLY ),
     $        XR1( MXCLY ), YLM0( 0:MXCMU ), YLMC( 0:MXCMU,MXCMU ),
     $        YLMU( 0:MXCMU,MXUMU ), Z( NNLYRI ), Z0( MXCMU ),
     $        Z0U( MXUMU,MXCLY ), Z1( MXCMU ),
     $        Z1U( MXUMU,MXCLY ), ZJ( MXCMU ), ZZ( MXCMU,MXCLY ),
     $        ZPLK0( MXCMU,MXCLY ), ZPLK1( MXCMU,MXCLY ),
     $        ZBEAM( MXUMU,MXCLY )

C---------------------------------------------------------------------
C  OUTPUT VARIABLES:
C
      DIMENSION FLXNU(MAXULV,NBNU,4),FLXINT(MAXULV,4)

C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     = = = = =     CONSTANTS      = = = = =
C
      DATA TMP /190.,200.,210.,220.,230.,240.,250.,260.,270.,280.,
     $            290.,300.,310.,320./
C
      DATA PI/3.1415926/
      
c      DATA PPMCO2,PPMO2/360.,209000./
      DATA PPMO2/209000./
C
      DATA AZ,BZ,CZ /84.35E-4,1.225E-4,1.400E-4/

C***********************************************************************
      COMMON /FORCI/ COND
      COMMON /FORCII/ ALBEDOA,hlyr,O3T,H2OT,SZA,NTAU1,NSZA,CH2
      COMMON /ALB/ ALBMODIS_1,WAVEMOD,NWM1 
      COMMON /ATMOS/ IATMOS 	
C***********************************************************************      
	
      COEFCO2=1.56712E-03
      COEFO2=1.13972E-03
   
     
      NLEVEL=10
C     
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     NSTR  : NUMBER OF COMPUTATIONAL POLAR ANGLES TO BE USED
C             (= NUMBER OF 'STREAMS')  ( SHOULD BE EVEN AND .GE. 4 )
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	IF (UMU1.EQ.-1) THEN  !SZA
         DO IW=1,NW
	  DO ITAU=1,NTAU1(IW)
	   DO ISZA=1,NSZA(IW,ITAU)
	    SZA0(ISZA)=SZA(IW,ITAU,ISZA)
	   ENDDO
	    SZA1(ITAU)=SUM(SZA0)/NSZA(IW,ITAU)	
	   ENDDO
	    SZA2(IW)=SUM(SZA1)/NTAU1(IW)
	  ENDDO
	    SZA3=SUM(SZA2)/NW
	    CSZA3=COS(RAD1*SZA3)
	    UMU0=CSZA3
	ELSE
	    UMU0=UMU1
	ENDIF

      CALL CHECK_NSTR(NSTR,UMU0,MAXCMU)
	
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PHI0=0.
      ONLYFL=.TRUE.
      USRANG=.FALSE.
      MOLSCT=1
      IRESOL=1
      XABSH2O=1.0
      XABSCO2=1.0
      XABSO2=1.0
      XABSO3=1.0
      ICNTWV=2
      ICNTCO2=2

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SOMFSOL=0.
      FSOL=0
      OPEN(990,FILE='SUN100')
      DO I=1,NBNU
         READ(990,*)II,FSOL(I)
      ENDDO
      CLOSE(990) 
C********JDAY,MONTH,ZSURF,OZONE,H2O FROM INVERSION INPUT FILE**********
	CONT=INDEX(CH2,':')
	IF (NOAERO.EQ.0) WRITE(901,*)'STATION:',TRIM(CH2)
	IF (DSOL.EQ.-1) THEN
      	 DAY=CH2(CONT-2:CONT-1)  !DAY
      	 MONT=CH2(CONT+1:CONT+2) !MONTH
     	 READ(DAY,*)JDAY
      	 READ(MONT,*)MONTH
	 CALL VARSOL1 (JDAY,MONTH,DSUN)
	ELSE
	  DSUN=1.0
	ENDIF
        IF (NOAERO.EQ.0) WRITE(901,*)'DAY MONTH DSUN:',JDAY,MONTH,DSUN

	IF (ZSUR.EQ.-1) THEN ! ZSURF (km)
	  ZSURF=hlyr(2)
	ELSE
	  ZSURF=ZSUR
	ENDIF
        IF (UH2O.EQ.-1) THEN ! WATER CONTENT  (cm)	
	  UH=H2OT(1)	
	ELSE
	  UH=UH2O
	ENDIF
	IF (UO3CM.EQ.-1) THEN ! OZONE CONTENT  (atm cm)
	   UO3=O3T(1)/1000 
	ELSE
	   UO3=UO3CM
	ENDIF
        IF (UCO2.EQ.-1) THEN ! CARBON DIOXIDE (ppmV)
	    PPMCO2=360.
	ELSE
	    PPMCO2=UCO2
	ENDIF

	IF (NOAERO.EQ.0) WRITE(901,*)'UMU0,SZA:',UMU0,SZA3

C********************* AVERAGE SURFACE ALBEDO***********************************
cs	Q=INDEX(CH2,',',BACK=.TRUE.)
cs	CH3=CH2(1:Q-1)
cs	QQ=INDEX(CH3,',',BACK=.TRUE.)	
cs	SITE=CH2(QQ+1:Q-1)
cs	SITE1=TRIM(PP)//'.'//TRIM(SITE)
cs	OPEN(990,FILE=SITE1)
cs	READ(990,*) NWM1,NWM2
cs	READ(990,*) CH
cs        write(6,*)'NWM1,NWM2',NWM1,NWM2
cs        write(6,*)'CH',CH
	
c	DO K=1,3
c	READ(990,*)CH4
c	ENDDO
cs	DO J=1,NWM1
C	 READ(990,*)WAVEMOD(J),(ALBMODIS1(J,K),K=1,2)
cs	READ(990,*)WAVEMOD(J),ALBMODIS_1(J)
C	 ALBMODIS_1(J)=ALBMODIS1(J,PP)
cs        write(6,*)'WAVEMOD(J),ALBMODIS_1(J)',WAVEMOD(J),ALBMODIS_1(J)
cs	ENDDO
	
C	DO J=1,NWM2
C	  READ(990,*)CH4,(ALBMODIS2(J,K),K=1,2)
C	  ALBMODIS_2(J)=ALBMODIS2(J,PP)
C	ENDDO
cs	CLOSE(990)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      OPEN(990,FILE='SPECTREH2O')
      DO I=1,138
         READ(990,*)II
         READ(990,*)(SOI(IT,I,1),IT=1,NBTEMP)
         READ(990,*)(SA(IT,I,1),IT=1,NBTEMP)
         READ(990,*)
      ENDDO
      CLOSE(990)
      OPEN(990,FILE='SPECTRECO2')
      DO I=1,138
         READ(990,*)II
         READ(990,*)(SOI(IT,I,2),IT=1,NBTEMP)
         READ(990,*)(SA(IT,I,2),IT=1,NBTEMP)
         READ(990,*)
      ENDDO
      CLOSE(990)
C---------------------------------------------------------------------

      CALL ATM(IATMOS,PRES,TT,ALTIT,DH,UH2OT,ZSURF) !IATMOS FROM INVERSION INPUT FILE
      IF(NOAERO.EQ.0)WRITE(901,*)'IATMOS UH02T&UH(grcm-2) UO3(atmcm) ZSUR 
     &F(km):',iatmos,UH2OT,UH,UO3,zsurf
      WRITE(901,*)

      PS=1013.25
      DO  I=1,NLEVEL-1
        IP=I+1
        UD(1,I)=(DH(I)+DH(IP))/2.*(PRES(I)-PRES(IP))/0.981
        UD(1,I)=UD(1,I)*XABSH2O*UH/UH2OT
      ENDDO   
C
      DO I=1,NLEVEL-1
       UD(3,I)=0.0
      ENDDO
       UD(3,NLEVEL-1)=UO3
	

C
      DO I=1,NLEVEL
        PRES(I)=PRES(I)/PS
      ENDDO   

      DO  I=1,NLEVEL-1
        IP=I+1
        DP(I)=PRES(I)-PRES(IP)
      ENDDO   
C
      DO  I=1,NLEVEL-1
        UD(2,I)=DP(I)
      ENDDO   
C
      DO  I=2,NLEVEL
        J=I-1
        RAY(J)=(PRES(J)-PRES(I))*MOLSCT
      ENDDO
c
C
C                 --------------------
C     = = = = =     INPUT DISORT DATA      = = = = =
C                 --------------------
C
      FBEAM=1.0
      FISOT=0.
      NOPLNK=.TRUE.
      DELTAM=.TRUE.
      USRTAU=.TRUE.
      IBCND=0
      LAMBER=.TRUE.
      ACCUR=.0
      NTAU=NLEVEL
      NLYR=NLEVEL-1
      DO I1=1,NTAU
        VAL(I1)=NLEVEL-I1+1
      ENDDO
      DO I=1,7
         PRNT(I)=.FALSE.
      ENDDO
C----------------------------------------------------------
      CALL ZEROIT (FLXNU,MAXULV*NBNU*4)
      CALL ZEROIT (FLXINT,4*MAXULV)

C----------------------------------------------------------
   
 63   FORMAT('Total Pre-bucle lambda ...... ',f8.4,' min.')

      NDP=IV
      IV=1
C**********************************************************
C*     NDP    I   - index for reading initial files:
C*             = 0 - the all initial data (kernels) are read
C*                  from initial data files
C*             > 0 - the kernes are used , no reaading
C***********************************************************

C  ======================================================
C
C                 ----------------------
C     = = = = =    SPECTRAL INTEGRATION      = = = = =
C                 ----------------------
C
C  ======================================================
	
      IW=1
      DO LAMB=LAMB1,LAMB2
C
C----------------------------------------------------------
C
      IF( LAMB.GT.119)THEN
         ANU=14600+400*(LAMB-120)
      ELSE 
         ANU=2550+100*(LAMB-1)
      ENDIF

      WVL=1.0E+04/ANU

	
C----------------------------------------------------------
      IF( LAMB.GT.119)THEN
         wvn1=14400+400.*(LAMB-120)
         wv2=1.0E+04/wvn1
         wvn2=14400+400.*(LAMB-119)
         wv1=1.0E+04/wvn2
      ELSE 
         wvn1=2500+100*(LAMB-1)
         wv2=1.0E+04/wvn1
         wvn2=2500+100*(LAMB)
         wv1=1.0E+04/wvn2
      ENDIF
      
c TO PRINT WAVELENGTHS AND WAVENUMBERS FOR EACH SPECTRAL INTERVAL
c     write(6,13)lamb,wvn1,wvn2,wv2,wv1
c13   format(i3.3,3X,2(f7.1,1x),2x,2(f6.4,1x))

C--------------SURFACE ALBEDO FROM MODIS  ------------------
  
       IF(WVL.LT.WAVEMOD(1)) THEN
	ALBEDO11=LINEAR2(WAVEMOD,ALBMODIS_1,NWM1,WVL) !EXTRAPOLATION
	IF (ALBEDO11.LT.0) ALBEDO11=0.0	
       ELSEIF (WVL.GE.WAVEMOD(1).AND.WVL.LE.WAVEMOD(NWM1)) THEN
	ALBEDO11=LINEAR2(WAVEMOD,ALBMODIS_1,NWM1,WVL) !0.47-2.13
       ELSEIF (WVL.GT.WAVEMOD(NWM1))THEN
	ALBEDO11=ALBMODIS_1(NWM1) 
       ENDIF
C----------SSA,EXT, PHASE FUNCTION-------------------------
***********************************************************
C* Computing SINGLE scattering properties:
C*      EXTINCTION, SSA, PHASE FUNCTION
C*   for each aerosol layer, for each aerosol componet (ISD)
C****************************************************************

      IF (NOAERO.EQ.1) GOTO 10
 
       DO IL=1,NLAY
        DO ISD=1,NSD
         DO I=1,NBIN(ISD)
	    RADIUS1(I)=RADIUS2(ISD,IL,I)
	    SD(I)=SD2(ISD,IL,I)
	 ENDDO

C-----------REFRACTIVE INDEX-------------------------
	DO J=1,NW
	RREAL_1(J)=RREAL1(J,IL,ISD)
	RIMAG_1(J)=DLOG(dble(RIMAG1(J,IL,ISD)))
	
	ENDDO
	
	IF(WVL.GT.WAVE(NW).OR.WVL.LT.WAVE(1))THEN
	RREAL_AVE=SUM(RREAL_1)/NW
	RIMAG_AVE=EXP(SUM(RIMAG_1)/NW)
	RREAL=RREAL_AVE
	RIMAG=RIMAG_AVE
	ELSE
	RREAL11=LINEAR2(WAVE,RREAL_1,NW,WVL)
	RIMAG11=EXP(LINEAR2(WAVE,RIMAG_1,NW,WVL))
	RREAL=RREAL11
	RIMAG=RIMAG11
	ENDIF
	
c------------------------------------------------------
		
	IEL=1 !P11
      CALL  PHASE_KERNL(ISD,IEL,NBIN(ISD),RADIUS1,SD,SHAPE,NSHAPE,
     & WVL,RREAL,RIMAG,IANGL,ANGL,SSA1,EXT1,PTP11,PTP12,
     & PTP22,PTP33,PTP34,PTP44,1,RATN,IS(ISD))
cs      write(6,*)'SSA',SSA1
cs      write(6,*)'EXT1',EXT1
cs      write(6,*)'PTP11'
cs      do i=1,IANGL
cs      write(6,*)ANGL(i),PTP11(i)
cs      enddo

         DO IAN=1,IANGL
         
          PF(IAN,IW,IL,ISD)=PTP11(IAN) 

         ENDDO
        
        IF(EXT1.GT.10./NLAY) THEN
        AEXT=EXT1
        EXT1=EXT1/AEXT*10./NLAY

        ENDIF

        IF (NSSA.EQ.-1)THEN 
	  SSA(IW,IL,ISD)=SSA1
	ELSE !SSA AVERAGE
	  DO J=1,NW
	   SSA_2(J)=SSA_1(J,IL,ISD)
	  ENDDO
	   SSA(IW,IL,ISD)=SUM(SSA_2)/NW
	ENDIF
          EXT(IW,IL,ISD)=EXT1
        ENDDO
       ENDDO
C*****************************************************************
C* Summarizing the optical properties of all aerosol 
C* components (ISD=1,NSD) and determining SINGLE scattering properties
C* for each atmospheric layer:
C******************************************************************
       AOD=0.
       DO IL=1,NLAY
       ASUM=0.
        DO ISD=1,NSD
          ASUM=ASUM+EXT(IW,IL,ISD)
          
        ENDDO
        EXTL(IL)=ASUM
	AOD=AOD+EXTL(IL)  !AOD EN COLUMNA
       
        ASUM=0.
        DO ISD=1,NSD
          ASUM=ASUM+SSA(IW,IL,ISD)*EXT(IW,IL,ISD)
        ENDDO
       
          ASUM=ASUM/EXTL(IL)
          SSAL(IL)=ASUM

       
        DO ISD=1,NSD
        DO IAN=1,IANGL
         IF(ISD.EQ.1) THEN
CD          PFL(IAN,IL)=PF(IAN,IW,IL,ISD)*SSA(IW,IL,ISD)*EXT(IW,IL,ISD)
            PFL(IAN,IL)=PF(IAN,IW,IL,ISD)
cs*********output***correction***************************************
            DLP(IAN,IW,IL,ISD)=PF12(IAN,IW,IL,ISD)/PF(IAN,IW,IL,ISD)
            PF(IAN,IW,IL,ISD)=PF(IAN,IW,IL,ISD)/(SSA(IW,IL,ISD)
     &*EXT(IW,IL,ISD))
cs***********************************************************************
C            
         ELSE
CD          PFL(IAN,IL)=PFL(IAN,IL)+PF(IAN,IW,IL,ISD)
CD     &*SSA(IW,IL,ISD)*EXT(IW,IL,ISD)
          PFL(IAN,IL)=PFL(IAN,IL)+PF(IAN,IW,IL,ISD)
C            
         ENDIF
        ENDDO 

        DO IAN=1,IANGL
         PFL(IAN,IL)=PFL(IAN,IL)/(SSAL(IL)*EXTL(IL))
        ENDDO

        ENDDO
       ENDDO 

	
C------------------------------------------------------------------
C
C-----  AEROSOL PROFILE ------------------------------------------
        
      DO IAN=1,IANGL !LEGENDRE EXPANSION OF THE PHASE FUNCTION
          CS(IAN)=COS(ANGL(IAN)*RAD1)
          YY(IAN)=PFL(IAN,1)
      ENDDO
      CALL LGNDF3(NMOM,IANGL,CS,YY,GG)
cs      do i=1,NMOM
cs      write(6,*)i,GG(i)
cs      enddo
      GG0=GG(1)
      DO L=1,NLEVEL-1  !PHASE FUNCTION AND SSA
C        AERP(L)=API(LAMB)
	AERP(L)=SSAL(1)! ojo en este caso porque aeronet solo tiene una capa, si no habria que calcular el total en toda la columna
        IF(NSTR2.LT.NSTR) THEN  
	  DO K2=1,NSTR2
	  PMOMA(K2,L)=GG(K2+1)/GG0
	  ENDDO
	  DO K1=NSTR2+1,NSTR
	  PMOMA(K1,L)=0.0
          ENDDO
	ELSEIF (NSTR2.EQ.NSTR) THEN
	  DO K1=1,NSTR
	  PMOMA(K1,L)=GG(K1+1)/GG0
	  ENDDO
	ELSE
	  WRITE(*,*)'ERROR: NSTR2 > NSTR'
	ENDIF
      ENDDO
		
10     IF (NOAERO.EQ.1) AOD=0.0
      	
       CST1=0.
       DO L=1,NLEVEL-1
         CST1=CST1+(EXP(-ALTIT(L)/SCL1)-EXP(-ALTIT(L+1)/SCL1))
       ENDDO
C      IF(AERO2)THEN
C      CST2=0.
C      DO L=1,NLEVEL-1
C       CST2=CST2+EXP(-((ALTIT(L)-ALTIT0)/SCL2)**2)
C      ENDDO
C      ENDIF

      TAUTOT=0
      DO L=1,NLEVEL-1
C        TAU1=AOT1(LAMB)*(EXP(-ALTIT(L)/SCL1)-EXP(-ALTIT(L+1)/SCL1))/CST1
        TAU1=AOD*(EXP(-ALTIT(L)/SCL1)-EXP(-ALTIT(L+1)/SCL1))/CST1
	
        IF(TAU1.LT.1.0E-06)TAU1=0.0
C        IF(AERO2)THEN
C          TAU2=AOT2(LAMB)*EXP(-((ALTIT(L)-ALTIT0)/SCL2)**2)/CST2
C          IF(TAU2.LT.1.0E-06)TAU2=0.0
C        ELSE
C          TAU2=0.
C        ENDIF
C        AERTAU(L)=TAU1+TAU2
C	AERTAU(L)=EXTL(L)
	AERTAU(L)=TAU1
        TAUTOT=TAUTOT+AERTAU(L)
  
      ENDDO
      IF(NOAERO.EQ.0)WRITE(902,*)lamb,WVL,SSAL(1),AOD,pmoma(1,1)
     &,ALBEDO11,RREAL,RIMAG

      AERO=0.
      IF (TAUTOT.GT.0.0)AERO=1
	
C------------------------------------------------
C
      RAYL=AZ/(WVL**4)-BZ/(WVL**5)+CZ/(WVL**6)
	
C
C------------------------------------------------
C

      IF(UH2O.LT.1.0E-06.AND.UH2O.NE.-1.)XABSH2O=0.
      CALL       KD(NKDMAX,MAXCLY,MAXULV,NKDH2O,NLEVEL,NBTEMP,
     $              NBABS,NBNU,ICNTWV,
     $              LAMB,UD,PRES,TT,AL,RKLM,UMU0,XABSH2O,1,SA,SOI,
     $              Y1,TPAR,X1,GKL,RKL,ALDP,H,RKLMDP,TMP,UDCO2)
C
      XABS=0.
	
      IF(XABSCO2.GT.0.AND.ANU.LT.11000)THEN
           XABS=1.
           UDCO2=PPMCO2*COEFCO2*XABSCO2
      ENDIF
      IF(XABSO2.GT.0.AND.(ANU.GE.11000.OR.
     $   (ANU.GT.7800.AND.ANU.LT.8000)))THEN
           XABS=1.
           UDCO2=PPMO2*COEFO2*XABSO2
      ENDIF
	
      CALL       KD(NKDMAX,MAXCLY,MAXULV,NKDCO2,NLEVEL,NBTEMP,
     $              NBABS,NBNU,ICNTCO2,
     $              LAMB,UD,PRES,TT,AL,RKLM,UMU0,XABS,2,SA,SOI,
     $              Y1,TPAR,X1,GKL,RKL,ALDP,H,RKLMDP,TMP,UDCO2)
	
C------------------------------------------------
      DO   IK1=1,NKDH2O
         AIH2O=AL(IK1,1)
	
         DO   IK2=1,NKDCO2
           AICO2=AL(IK2,2)
           AI=AIH2O*AICO2

      IMOLABS=1
      CALL       ATMPROF(IMOLABS,ILAMB,RAYL,UD,RAY,RKLM,IK1,IK2,MOLSCT,
     &                   MAXULV,MAXCLY,MAXCMU,NKDMAX,NBABS,
     &                   NLEVEL,PRES,TT,ALTIT,
     &                   AERO,PMOMA,AERTAU,AERP,
     &                   SSALB,DTAUC,PMOM,NSTR,ANU,
     &                   PPMCO2,PPMO2,COEFCO2,COEFO2,ICNTWV,PS,XABSH2O,
     &                   XABSCO2,XABSO2,XABSO3,IRESOL,ICNTCO2)

	
C-------------------------------------------------------------
C        DISCRETE ORDINATES METHOD
C-------------------------------------------------------------
	

           CALL DISORT      ( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $                        WVNMHI, USRTAU, NTAU, UTAU, NSTR, USRANG,
     $                        NUMU, UMU, NPHI, PHI, IBCND, FBEAM, UMU0,
     $                        PHI0, FISOT, LAMBER, ALBEDO11, HL, BTEMP,
     $                        TTEMP, TEMIS, DELTAM, NOPLNK, ONLYFL,
     $                        ACCUR, PRNT, HEADER, MAXCLY, MAXULV,
     $                        MAXUMU, MAXCMU, MAXPHI, RFLDIR, RFLDN,
     $                        FLUP, DFDT, UAVG, UU, U0U, ALBMED, TRNMED,
     $                        VAL ,MXCLY,MXULV,MXCMU,MXUMU,MXPHI,
     $                        MI, MI9M2 , NNLYRI ,
     $                        IPVT,LAYRU,ALBSAV,AMB, APB, ARRAY, B, BDR,
     $                        BEM, CBAND, CC, CMU, CWT, EMU, EVAL,
     $                        EVECC, EXPBEA, FLYR, FLDN, FLDIR, GL,
     $                        GC, GU, HLPR, KK, LL, OPRIM, PHIRAD, PKAG,
     $                        PSI, RMU, TAUC, TAUCPR, U0C, UTAUPR,
     $                        UUM, WK, XR0,XR1, YLM0, YLMC, YLMU, Z, Z0,
     $                        Z0U, Z1, Z1U, ZJ, ZZ, ZPLK0, ZPLK1, ZBEAM)
C-------------------------------------------------------------
           DO L=1,NLEVEL
cs             IF(L.EQ.NLEVEL)AI=1.0
             FLXNU(L,IW,1)=FLXNU(L,IW,1)+AI*(RFLDN(L)+RFLDIR(L))
             FLXNU(L,IW,2)=FLXNU(L,IW,2)+AI*FLUP(L)
	     FLXNU(L,IW,3)=FLXNU(L,IW,3)+AI*RFLDN(L)
             FLXNU(L,IW,4)=FLXNU(L,IW,4)+AI*RFLDIR(L)
cs     &AI,RFLDIR(L),RFLDN(L)
cs           IF(L.EQ.10) WRITE(*,*)RFLDIR(L),AI*RFLDIR(L)
           ENDDO
C------------------------------------------------
         ENDDO   
      ENDDO  
C------------------------------------------------
cs      DSUN=1.0
      FILT=FSOL(LAMB)*DSUN
      SOMFSOL=SOMFSOL+FILT
cs      WRITE(*,*)'FSOL(LAMB),DSUN,FILT',FSOL(LAMB),DSUN,FILT
cs      WRITE(*,*)'SOMFSOL',SOMFSOL

      DO L=1 , NLEVEL
         FLXNU(L,IW,1)=FLXNU(L,IW,1)*FILT
         FLXNU(L,IW,2)=FLXNU(L,IW,2)*FILT
         FLXNU(L,IW,3)=FLXNU(L,IW,3)*FILT
         FLXNU(L,IW,4)=FLXNU(L,IW,4)*FILT

         FLXINT(L,1)=FLXINT(L,1)+FLXNU(L,IW,1)
         FLXINT(L,2)=FLXINT(L,2)+FLXNU(L,IW,2)
         FLXINT(L,3)=FLXINT(L,3)+FLXNU(L,IW,3)
         FLXINT(L,4)=FLXINT(L,4)+FLXNU(L,IW,4)
       ENDDO
cs      WRITE(*,*)'LAMB,NLEVEL',LAMB,NLEVEL
cs      DO L=1 , NLEVEL
cs      IF(L.EQ.NLEVEL)WRITE(*,*)FLXINT(L,1),FLXINT(L,2)
cs      ENDDO
C
C  ======================================================
C
C                 ---------------------------
C     = = = = =   END OF SPECTRAL INTEGRATION   = = = = =
C                 ---------------------------
C  ======================================================
C--------------  TO CHECK PROFILES AT 550nm -------------
      IF(NOAERO.EQ.0.AND.IW.EQ.1)WRITE(901,*)'WITH AEROSOLS:'
      IF(NOAERO.EQ.1.AND.IW.EQ.1)WRITE(901,*)'WITHOUT AEROSOLS:'
     			
      IF(LAMB.EQ.129)THEN
      UTOT=0.
      AERTOT=0.
c      WRITE(901,*)	
c      WRITE(901,*)'ALTIT PRESSU  UH2O   TAUAER'
c      WRITE(901,*)' (km)  (hPa) (g/cm2) 550 nm'

      UTOT=0.
      TOTAER=0.
      DO L=1,NLEVEL
        UTOT=UTOT+UD(1,L)
        TOTAER=TOTAER+AERTAU(L)
c        WRITE(901,998)ALTIT(L),PRES(L)*PS,UD(1,L),AERTAU(L)
      ENDDO
c      WRITE(901,997)UTOT,TOTAER
c      WRITE(901,*)
998   FORMAT(F7.3,1X,F7.2,1X,F5.2,3X,F6.3)
997   FORMAT(14X,F5.2,3X,F6.3)
      ENDIF
C------------------------------------------------
	IW=IW+1
cs        WRITE(*,*)''
      ENDDO !END LAMB
C
      IF(CSOL.EQ.-1)THEN
              CONSTSOL=SOMFSOL
      ELSE
              CONSTSOL=CSOL
      ENDIF
      IF(NOAERO.EQ.0)WRITE(901,*)'CONSTSOL(W/m2):',CONSTSOL
      CORSOL=CONSTSOL/SOMFSOL
      DO L=1 , NLEVEL
         FLXINT(L,1)=FLXINT(L,1)/SOMFSOL*CONSTSOL
         FLXINT(L,2)=FLXINT(L,2)/SOMFSOL*CONSTSOL
         FLXINT(L,3)=FLXINT(L,3)/SOMFSOL*CONSTSOL
         FLXINT(L,4)=FLXINT(L,4)/SOMFSOL*CONSTSOL
      ENDDO
      RETURN
      END


C*****************ENDSUBROUTINE RADTRS**********************************




C***********************************************************************
      REAL FUNCTION  HG( A,G1,G2,B)
C+---------------------------------------------------------------------+
C!      Heney-Greenstein Phase Function                                !
C+---------------------------------------------------------------------+
C    A - scattering angle =COS(ANGLE)
C    G1,G2,B -  parameters
C***********************************************************************
      IF(G1.GE.1.0) THEN
C      WRITE(*,*) G1,' G1 Heney-Greenstein > 1'
      G1=0.999
      ENDIF
      IF(G2.GT.0.98005) THEN
C      WRITE(*,*) G2, ' G2 Heney-Greenstein > 1'
      G2=0.99
      ENDIF
      IF(B.GE.1.0) THEN
C      WRITE(*,*) B, ' B Heney-Greenstein > 1'
      B=0.999
      ENDIF
      IF(B.EQ.0.0) THEN
      HG=(1.0-G1*G1)/(1.0-2.0*G1*A+G1*G1)**(3.0/2.0)
      ELSE
      HG1=(1.0-G1*G1)/(1.0-2.0*G1*A+G1*G1)**(3.0/2.0)
      HG2=(1.0-G2*G2)/(1.0+2.0*G2*A+G2*G2)**(3.0/2.0)
      HG=B*HG1+(1-B)*HG2
      ENDIF
      RETURN
      END
C**************************  END OF WVCONT  ****************************
C***********************************************************************
      REAL FUNCTION  LINEAR( X, Y, M, X1 )
C+---------------------------------------------------------------------+
C!       Linear interpolation function.                                !
C+---------------------------------------------------------------------+
      IMPLICIT  REAL   (A-H, O-Z)
      REAL      X( * ), Y( * )
C
      IF ( X1.LT.X(1) )  THEN
         LINEAR = Y(1) - ( X1-X(1) )*( Y(2)-Y(1) )/( X(2)-X(1) )
      ELSE IF ( X1.GT.X(M) )  THEN
         LINEAR = Y(M) + ( X1-X(M) )*( Y(M)-Y(M-1) )/( X(M)-X(M-1) )
      ELSE
         DO 10 N = 2, M
            IF ( X1.GE.X(N-1) .AND. X1.LE.X(N) ) LINEAR = Y(N-1) +
     $         ( X1-X(N-1) )*( Y(N)-Y(N-1) )/( X(N)-X(N-1) )
C*****************************
        IF(X1.EQ.X(N-1)) LINEAR =Y(N-1)
        IF(X1.EQ.X(N)) LINEAR =Y(N)
C*****************************
10       CONTINUE
      END IF

      RETURN
      END
C**************************  END OF LINEAR  ****************************
      real function gaussd(HG,WG,HC)
      real HG,WG,HC
CXXA	OVERFLOW OF EXP

cxxa	write(*,*)'hg=',hg,'hc=',hc,'wg=',wg,
cxxa     &	1./exp(dble(0.5*(((HG-HC)/WG)**2)))
	if(0.5*((HG-HC)/WG)**2.gt.600)then
		gaussd=0.1e-37
	else
		gaussd=1./exp(dble(0.5*(((HG-HC)/WG)**2)))
	endif

      return
      end
      SUBROUTINE FMAT(KM,KN,IT,KL,IMSC,NW,WAVE,NSD
     &,NBIN,RADIUS,NLAY,NBRDF,NBRDF1,NSHAPE,IANGL,ISDLOC,IREFLOC
     &,IBRDFLOC,IBRDF1LOC,ISHAPELOC,ISTAR,AP,FP,IMTX,DL,LP
     &,REALMIN,REALMAX,AIMAGMIN,AIMAGMAX
     &,BRDFMIN,BRDFMAX,BRDF1MIN,BRDF1MAX,SHAPEMIN,SHAPEMAX
     &,AL,AREF,AREF1,U,IEL,IS,INDLD,US,UP,SSA,PF,UPOL,DLP,ITOFF,
     &IBRF,IERR)
C*****************************************************
C*** This subroutine manages the calculations of   ***
C*** first derivatives matrix calculations  for    ***
C*** sun/sky radiance inversion                    ***
C 
C   INPUT:
C      DL     R   - for calculating the derivatives:
C                   (F(a+DP)-F(a))/DP
C      AP   R(KN) -  vector of "parameters" defined as
C                                   abs.  (KL=0) 
C                               or  log   (KL=1)
C      FP   R(KM) -  vector of "measurements" calculated 
C                    for the "parameters" AP()
C***   
C   other input variables are described in 'forwM'!!!
C***
C   OUTPUT:
C
C      U R(KM,KN) -  matrix of the first derivatives
C
C*****************************************************
      PARAMETER( KMES=500,KPAR=100,KMESS=100,KSD=3
     &,KW=10,MAXCLY=35,KANG=181,KBRDF=13,KNA0=2,KSHAPE=2)
!tl     &,KW=10,MAXCLY=35,KANG=83,KBRDF=13,KNA0=2,KSHAPE=2)
      INTEGER MAXUMU
      REAL LINEAR2
      PARAMETER (MAXUMU=100)
      DIMENSION AP(KPAR),FP(KMES),U(KMES,KPAR)
      INTEGER FTAU(KW),FUMUY(KW,MAXCLY,KNA0),
     &FPHI(KW,MAXCLY,KNA0,MAXUMU)
      DIMENSION AP1(KPAR),FP1(KMES),US(KW,MAXCLY,KSD,KPAR),
     &UP(KANG,KW,MAXCLY,KSD,KPAR),UPOL(KANG,KW,MAXCLY,KSD,KPAR)
      INTEGER NBIN(KSD),ISTAR(KPAR),IREFLOC(MAXCLY,KSD,2),
     &ISDLOC(MAXCLY,KSD),IBRDFLOC(KBRDF),IBRDF1LOC(KBRDF),
     &IS(KSD),NBIN1(KSD),IPOLPR(KW),ISHAPELOC(KSHAPE)
      DIMENSION RADIUS(KSD,KPAR),SSA(KW,MAXCLY,KSD)
     &,BRDFMIN(KBRDF),BRDFMAX(KBRDF),BRDF1MIN(KBRDF)
     &,BRDF1MAX(KBRDF),SBRDF(KBRDF),SBRDF1(KBRDF),
     &SHAPEMIN(KSHAPE),SHAPEMAX(KSHAPE),SSHAPE(KSHAPE) 
      DIMENSION WAVE(KW),ANG(KANG),FLDD(KW),FLUP(KW),FLDR(KW),
     &PF(KANG,KW,MAXCLY,KSD),EXT(KW,MAXCLY,KSD),
     &DLP(KANG,KW,MAXCLY,KSD),ANGLESCA(KW,2,KMES)
      DIMENSION SSAN(KW,MAXCLY,KSD),
     &PFN(KANG,KW,MAXCLY,KSD),
     &DLPN(KANG,KW,MAXCLY,KSD),anstr_mod(2),xlags(2)
CD      WRITE(*,*) 'in FMAT'
      DO ISD=1,NSD
Cd       DO I=1,NBIN(ISD)
CD      WRITE(*,*) RADIUS(ISD,I),I
CD       ENDDO
      ENDDO
C*****************************************************
C*** Calculating matrix with fixed DL              ***
CD      WRITE(*,*) DL, IMSC, ' DL, IMSC'
      DO I=1,KN
      AP1(I)=AP(I)
CD      WRITE(6,*) 'AP ',EXP(AP(I))
      ENDDO
CD      WRITE(*,*) REALMIN,REALMAX,AIMAGMIN,AIMAGMAX
CD     &,'REALMIN,REALMAX,AIMAGMIN,AIMAGMAX'
CD      DO IB=1,NBRDF
CD      WRITE(*,*) BRDFMIN(IB),BRDFMAX(IB)
CD     &,'BRDFMIN(IB),BRDFMAX(IB)'
CD      ENDDO
CDE      GOTO 12
cs      write(6,*)'IN FMAT1'
cs      write(6,*)'SHAPEMIN',SHAPEMIN
cs      write(6,*)'SHAPEMAX',SHAPEMAX
cs      write(6,*)'AREF',AREF
      DO IW=1,NW
       DO IL=1,NLAY
        DO ISD=1,NSD
      IF(KL.EQ.0) THEN
       IF(AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1).LE.REALMIN)
     & AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1)=REALMIN
       IF(AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1).GE.REALMAX) 
     & AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1)=REALMAX
       IF(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1).LE.AIMAGMIN) 
     & AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1)=AIMAGMIN
       IF(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1).GE.AIMAGMAX) 
     & AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1)=AIMAGMAX
      ENDIF   
      IF(KL.EQ.1) THEN   
      IF(EXP(AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1)).LE.REALMIN) 
     &  AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1)=LOG(REALMIN)
      IF(EXP(AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1)).GE.REALMAX) 
     &AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1)=LOG(REALMAX)
      IF(EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1)).LE.AIMAGMIN) 
     & AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1)=LOG(AIMAGMIN)
      IF(EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1)).GE.AIMAGMAX) 
     &AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1)=LOG(AIMAGMAX)
      ENDIF   
C     WRITE(6,*) 'oleg ',EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1)),IL,ISD
        ENDDO
       ENDDO
cs       write(6,*)'IN FMAT2'
       DO IB=1,NBRDF
         IF(KL.EQ.0) THEN
        IF(AP(ISTAR(IBRDFLOC(IB))+IW-1).LE.BRDFMIN(IB))
     & AP(ISTAR(IBRDFLOC(IB))+IW-1)=BRDFMIN(IB)
        IF(AP(ISTAR(IBRDFLOC(IB))+IW-1).GE.BRDFMAX(IB))
     & AP(ISTAR(IBRDFLOC(IB))+IW-1)=BRDFMAX(IB)
         ENDIF
         IF(KL.EQ.1) THEN
        IF(EXP(AP(ISTAR(IBRDFLOC(IB))+IW-1)).LE.BRDFMIN(IB))
     & AP(ISTAR(IBRDFLOC(IB))+IW-1)=LOG(BRDFMIN(IB))
        IF(EXP(AP(ISTAR(IBRDFLOC(IB))+IW-1)).GE.BRDFMAX(IB))
     & AP(ISTAR(IBRDFLOC(IB))+IW-1)=log(BRDFMAX(IB))
         ENDIF
       ENDDO
      ENDDO
cs       write(6,*)'IN FMAT21'
       DO IBB=1,NBRDF1
         IF(KL.EQ.0) THEN
        IF(AP(ISTAR(IBRDF1LOC(1))+IBB-1).LE.BRDF1MIN(IBB))
     & AP(ISTAR(IBRDF1LOC(1))+IBB-1)=BRDF1MIN(IBB)
        IF(AP(ISTAR(IBRDF1LOC(1))+IBB-1).GE.BRDF1MAX(IBB))
     & AP(ISTAR(IBRDF1LOC(1))+IBB-1)=BRDF1MAX(IBB)
         ENDIF
         IF(KL.EQ.1) THEN
        IF(EXP(AP(ISTAR(IBRDF1LOC(1))+IBB-1)).LE.BRDF1MIN(IBB))
     & AP(ISTAR(IBRDF1LOC(1))+IBB-1)=LOG(BRDF1MIN(IBB))
        IF(EXP(AP(ISTAR(IBRDF1LOC(1))+IBB-1)).GE.BRDF1MAX(IBB))
     & AP(ISTAR(IBRDF1LOC(1))+IBB-1)=log(BRDF1MAX(IBB))
         ENDIF
       ENDDO
cs       write(6,*)'CHECK21'
C****************************************CORRECTION*FOR*SHAPE********************
       DO IBB=1,NSHAPE
         IF(KL.EQ.0) THEN
        IF(AP(ISTAR(ISHAPELOC(1))+IBB-1).LE.SHAPEMIN(IBB))
     & AP(ISTAR(ISHAPELOC(1))+IBB-1)=SHAPEMIN(IBB)
        IF(AP(ISTAR(ISHAPELOC(1))+IBB-1).GE.SHAPEMAX(IBB))
     & AP(ISTAR(ISHAPELOC(1))+IBB-1)=SHAPEMAX(IBB)
         ENDIF
         IF(KL.EQ.1) THEN
        IF(EXP(AP(ISTAR(ISHAPELOC(1))+IBB-1)).LE.SHAPEMIN(IBB))
     & AP(ISTAR(ISHAPELOC(1))+IBB-1)=LOG(SHAPEMIN(IBB))
        IF(EXP(AP(ISTAR(ISHAPELOC(1))+IBB-1)).GE.SHAPEMAX(IBB))
     & AP(ISTAR(ISHAPELOC(1))+IBB-1)=log(SHAPEMAX(IBB))
         ENDIF
       ENDDO
C*******************************************************************************
cs       write(6,*)'CHECK22'
cs       write(6,*)'IN FMAT3'
CDE  12  CONTINUE
      DO I=1,KN
      DL1=DL
      DO IW=1,NW
       DO IL=1,NLAY
        DO ISD=1,NSD
        IF(I.GE.ISTAR(IREFLOC(IL,ISD,1)).AND.I.LE.
     &ISTAR(IREFLOC(IL,ISD,1))+IW-1.AND.AL.LE.05) DL1=DL*AL/0.5
        IF(I.GE.ISTAR(IREFLOC(IL,ISD,2)).AND.I.LE.
     &ISTAR(IREFLOC(IL,ISD,2))+IW-1.AND.AL.LE.05) DL1=10.*DL*AL/0.5
        IF(I.GE.ISTAR(IREFLOC(IL,ISD,2)).AND.I.LE.
     &ISTAR(IREFLOC(IL,ISD,2))+IW-1) DL1=10.*DL
cs*************************************TEST*********************CORRECTION********************   
      IF(IERR.EQ.1)THEN
      dif=abs(EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1))-AIMAGMIN)
      IF(I.GE.ISTAR(IREFLOC(IL,ISD,2)).AND.I.LE.
     &ISTAR(IREFLOC(IL,ISD,2))+IW-1.AND.
     &dif.LE.2.0e-3)THEN
cs       anstr=EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1))
cs       anstr_mod(1)=0.0005
cs       anstr_mod(2)=0.0017
cs       xlags(1)=600.0
cs       xlags(2)=400.0
cs       xlagsm=LINEAR2(anstr_mod,xlags,2,anstr)
       xlagsm=400.0
       DL1=xlagsm*DL
cs      write(6,*)EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1))
cs      write(6,*)'DL1',DL1
cs      write(6,*)'DLIM',EXP(xlagsm)
cs      write(6,*)
      ENDIF
cs      write(6,*)EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1))
cs      write(6,*)'DL1',DL1
      ENDIF
cs**************************************TEST********************CORRECTION********************
        IF(I.GE.ISTAR(ISDLOC(IL,ISD)).AND.I.LE.ISTAR(ISDLOC(IL,ISD))
     &+NBIN(ISD)-1.AND.AL.LE.0.5) DL1=100.*DL*AL/0.5
        IF(I.GE.ISTAR(ISDLOC(IL,ISD)).AND.I.LE.ISTAR(ISDLOC(IL,ISD))
     &+NBIN(ISD)-1.AND.AL.GE.0.5) DL1=100.*DL
        ENDDO
       ENDDO
      ENDDO
cs      write(6,*)'IN FMAT4'
      DO IB=1, NBRDF
      IF(KL.EQ.1) THEN
      IF(IB.EQ.1)THEN
	IF(BRDFMAX(IB).LT.1.0E-30)BRDFMAX(IB)=1.0E-30
	IF(BRDFMIN(IB).LT.1.0E-30)BRDFMIN(IB)=1.0E-30
      SBRDF(IB)=(LOG(BRDFMAX(IB))-LOG(BRDFMIN(IB))*1.0)
      ENDIF
      IF(IB.EQ.2)THEN
	IF(BRDFMAX(IB).LT.1.0E-30)BRDFMAX(IB)=1.0E-30
	IF(BRDFMIN(IB).LT.1.0E-30)BRDFMIN(IB)=1.0E-30
      SBRDF(IB)=(LOG(BRDFMAX(IB))-LOG(BRDFMIN(IB))*1.0)
      ENDIF
      IF(IB.EQ.3)THEN
	IF(BRDFMAX(IB).LT.1.0E-30)BRDFMAX(IB)=1.0E-30
	IF(BRDFMIN(IB).LT.1.0E-30)BRDFMIN(IB)=1.0E-30
      SBRDF(IB)=(LOG(BRDFMAX(IB))-LOG(BRDFMIN(IB))*1.0)
      ENDIF
      IF(IB.EQ.4)THEN
	IF(BRDFMAX(IB).LT.1.0E-30)BRDFMAX(IB)=1.0E-30
	IF(BRDFMIN(IB).LT.1.0E-30)BRDFMIN(IB)=1.0E-30
      SBRDF(IB)=(LOG(BRDFMAX(IB))-LOG(BRDFMIN(IB))*1.0)
      ENDIF
      ENDIF
      IF(KL.EQ.0) THEN
      IF(IB.EQ.1) THEN
 	IF(BRDFMAX(IB).LT.1.0E-30)BRDFMAX(IB)=1.0E-30
	IF(BRDFMIN(IB).LT.1.0E-30)BRDFMIN(IB)=1.0E-30
      SBRDF(IB)=(BRDFMAX(IB)-BRDFMIN(IB))*1.0
      ENDIF
      IF(IB.EQ.2) THEN
	IF(BRDFMAX(IB).LT.1.0E-30)BRDFMAX(IB)=1.0E-30
	IF(BRDFMIN(IB).LT.1.0E-30)BRDFMIN(IB)=1.0E-30
      SBRDF(IB)=(BRDFMAX(IB)-BRDFMIN(IB))*1.0
      ENDIF
      IF(IB.EQ.3) THEN
	IF(BRDFMAX(IB).LT.1.0E-30)BRDFMAX(IB)=1.0E-30
	IF(BRDFMIN(IB).LT.1.0E-30)BRDFMIN(IB)=1.0E-30
      SBRDF(IB)=(BRDFMAX(IB)-BRDFMIN(IB))*1.0
      ENDIF
      IF(IB.EQ.4)THEN
	IF(BRDFMAX(IB).LT.1.0E-30)BRDFMAX(IB)=1.0E-30
	IF(BRDFMIN(IB).LT.1.0E-30)BRDFMIN(IB)=1.0E-30
      SBRDF(IB)=((BRDFMAX(IB))-(BRDFMIN(IB))*1.0)
      ENDIF
      ENDIF
cs      write(6,*)'IN FMAT5'
cs      WRITE(*,*) IB,SBRDF(IB),' brdf 1'
      ENDDO
      DO IB=1,NBRDF1
      IF(KL.EQ.0) THEN
	IF(BRDF1MAX(IB).LT.1.0E-30)BRDF1MAX(IB)=1.0E-30
	IF(BRDF1MIN(IB).LT.1.0E-30)BRDF1MIN(IB)=1.0E-30
      SBRDF1(IB)=(BRDF1MAX(IB)-BRDF1MIN(IB))*1.0
      ENDIF
      IF(KL.EQ.1) THEN
	IF(BRDF1MAX(IB).LT.1.0E-30)BRDF1MAX(IB)=1.0E-30
	IF(BRDF1MIN(IB).LT.1.0E-30)BRDF1MIN(IB)=1.0E-30
      SBRDF1(IB)=(LOG(BRDF1MAX(IB))-LOG(BRDF1MIN(IB))*1.0)
      ENDIF
      ENDDO
cs      write(6,*)'CHECK23'
C***********************************CORRECTION*FOR*SHAPE***********************
      DO IB=1,NSHAPE
      IF(KL.EQ.0) THEN
      SSHAPE(IB)=(SHAPEMAX(IB)-SHAPEMIN(IB))*10.0
      ENDIF
      IF(KL.EQ.1) THEN
	IF(SHAPEMAX(IB).LT.1.0E-30)SHAPEMAX(IB)=1.0E-30
	IF(SHAPEMIN(IB).LT.1.0E-30)SHAPEMIN(IB)=1.0E-30
      SSHAPE(IB)=(LOG(SHAPEMAX(IB))-LOG(SHAPEMIN(IB))*10.0)
      ENDIF
      ENDDO
C*****************************************************************************
cs      write(6,*)'CHECK24'
cs
cs      DO IB=1,NBRDF1
cs      IF(KL.EQ.1) THEN
cs      IF(IB.EQ.1) THEN
cs      SBRDF1(IB)=(LOG(BRDF1MAX(IB))-LOG(BRDF1MIN(IB))*1.0)
cs      ENDIF
cs      IF(IB.EQ.2) THEN
cs      SBRDF1(IB)=(LOG(BRDF1MAX(IB))-LOG(BRDF1MIN(IB))*1.0)
cs      ENDIF
cs      IF(IB.EQ.3) THEN
cs      SBRDF1(IB)=(LOG(BRDF1MAX(IB))-LOG(BRDF1MIN(IB))*1.0)
cs      ENDIF
cs      ENDIF
cs      IF(KL.EQ.0) THEN
cs      IF(IB.EQ.1) THEN
cs      SBRDF1(IB)=(BRDF1MAX(IB)-BRDF1MIN(IB))*1.0
cs      ENDIF
cs      IF(IB.EQ.2) THEN
cs      SBRDF1(IB)=(BRDF1MAX(IB)-BRDF1MIN(IB))*1.0
cs      ENDIF
cs      IF(IB.EQ.3) THEN
cs      SBRDF1(IB)=(BRDF1MAX(IB)-BRDF1MIN(IB))*1.0
cs      ENDIF
cs      ENDIF

cs      WRITE(*,*) IB,SBRDF1(IB),' brdf 2'
cs      ENDDO
      DO IW=1,NW
      DO IB=1, NBRDF
         IF(I.EQ.ISTAR(IBRDFLOC(IB))+IW-1) THEN 
           IF(AL.LE.0.5) THEN
                DL1=SBRDF(IB)*DL*AL/0.5
           ELSE
              DL1=SBRDF(IB)*DL
           ENDIF
         ENDIF 
      ENDDO
      ENDDO 
      DO IBB=1, NBRDF1
         IF(I.EQ.ISTAR(IBRDF1LOC(1))+IBB-1) THEN 
           IF(AL.LE.0.5) THEN
                DL1=SBRDF1(IBB)*DL*AL/0.5
           ELSE
              DL1=SBRDF1(IBB)*DL
           ENDIF
         ENDIF 
      ENDDO  
cs       write(6,*)'CHECK25'    
C********************************CORRECTION*FOR*SHAPE***************************
      DO IBB=1, NSHAPE
         IF(I.EQ.ISTAR(ISHAPELOC(1))+IBB-1) THEN 
           IF(AL.LE.0.5) THEN
                DL1=SSHAPE(IBB)*DL*AL/0.5
           ELSE
              DL1=SSHAPE(IBB)*DL
           ENDIF
         ENDIF 
      ENDDO 
C*******************************************************************************
cs      write(6,*)'CHECK26'
      IF(ABS(DL1).LT.0.001) DL1=DL1/ABS(DL1)*0.001
cs      write(6,*)'CHECK261'
      DO IW=1,NW
       DO IL=1,NLAY
        DO ISD=1,NSD
CD        WRITE(*,*) 'before GOTO'
      IF(AL.GT.AREF.AND.LP.LT.50.AND.
     &I.EQ.ISTAR(IREFLOC(IL,ISD,1))+IW-1) GOTO 21
      IF(AL.GT.AREF.AND.LP.LT.50.AND.
     &I.EQ.ISTAR(IREFLOC(IL,ISD,2))+IW-1) GOTO 21
      IF(KL.EQ.0) THEN
         IF(I.EQ.ISTAR(IREFLOC(IL,ISD,1))+IW-1) THEN
       IF(REALMIN-AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1).LE.-DL1.
     & AND.DL1.LT.0) THEN
        DL1=-DL1
       ENDIF
       IF(REALMAX-AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1).LT.DL1.AND.DL1.GT.0) 
     & THEN
        DL1=-DL1
       ENDIF
         ENDIF
         IF(I.EQ.ISTAR(IREFLOC(IL,ISD,2))+IW-1) THEN
       IF(AIMAGMIN-AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1).LT.-DL1
     &.AND.DL1.LT.0) THEN
        DL1=-DL1
       ENDIF
       IF(AIMAGMAX-AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1).LT.DL1
     &.AND.DL1.GT.0) THEN
        DL1=-DL1
       ENDIF
         ENDIF
      ENDIF 
cs      write(6,*)'CHECK2611'  
      IF(KL.EQ.1) THEN 
         IF(I.EQ.ISTAR(IREFLOC(IL,ISD,1))+IW-1) THEN
      IF(REALMIN-EXP(AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1)).LT.-DL1.
     & AND.DL1.LT.0) THEN
       DL1=-DL1
      ENDIF
      IF(REALMAX-EXP(AP(ISTAR(IREFLOC(IL,ISD,1))+IW-1)).LT.DL1.
     & AND.DL1.GT.0) THEN
       DL1=-DL1
      ENDIF
         ENDIF
         IF(I.EQ.ISTAR(IREFLOC(IL,ISD,2))+IW-1) THEN
      IF(AIMAGMIN-EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1))
     &.LT.-DL1.AND.DL1.LT.0) 
     &THEN
       DL1=-DL1
      ENDIF
      IF(AIMAGMAX-EXP(AP(ISTAR(IREFLOC(IL,ISD,2))+IW-1)).LT.DL1.
     &AND.DL1.GT.0) THEN
       DL1=-DL1
      ENDIF
         ENDIF
      ENDIF   
        ENDDO
       ENDDO
cs       write(6,*)'CHECK262'
       DO IB=1,NBRDF
         IF(I.EQ.ISTAR(IBRDFLOC(IB))+IW-1) THEN         
          IF(KL.EQ.0) THEN
       IF(BRDFMIN(IB)-AP(ISTAR(IBRDFLOC(IB))+IW-1).LT.-DL1
     &.AND.DL1.LT.0) THEN
       DL1=-DL1
           ENDIF
           IF(BRDFMAX(IB)-AP(ISTAR(IBRDFLOC(IB))+IW-1).LT.DL1
     & .AND.DL1.GT.0) THEN
       DL1=-DL1
           ENDIF
          ENDIF
         IF(KL.EQ.1) THEN
      IF(BRDFMIN(IB)-EXP(AP(ISTAR(IBRDFLOC(IB))+IW-1)).LT.-DL1
     & .AND.DL1.LT.0) THEN
       DL1=-DL1
          ENDIF
      IF(BRDFMAX(IB)-EXP(AP(ISTAR(IBRDFLOC(IB))+IW-1)).LT.DL1
     &.AND.DL1.GT.0) THEN
       DL1=-DL1
          ENDIF
         ENDIF         
         ENDIF
       ENDDO
      ENDDO
       DO IBB=1,NBRDF1
         IF(I.EQ.ISTAR(IBRDF1LOC(1))+IBB-1) THEN         
          IF(KL.EQ.0) THEN
       IF(BRDF1MIN(IBB)-AP(ISTAR(IBRDF1LOC(1))+IBB-1).LT.-DL1
     &.AND.DL1.LT.0) THEN
       DL1=-DL1
           ENDIF
           IF(BRDF1MAX(IBB)-AP(ISTAR(IBRDF1LOC(1))+IBB-1).LT.DL1
     &.AND.DL1.GT.0) THEN
       DL1=-DL1
           ENDIF
          ENDIF
         IF(KL.EQ.1) THEN
      IF(BRDF1MIN(IBB)-EXP(AP(ISTAR(IBRDF1LOC(1))+IBB-1)).LT.-DL1
     &.AND.DL1.LT.0) THEN
       DL1=-DL1
          ENDIF
      IF(BRDF1MAX(IBB)-EXP(AP(ISTAR(IBRDF1LOC(1))+IBB-1)).LT.DL1
     &.AND.DL1.GT.0) THEN
       DL1=-DL1
          ENDIF
         ENDIF         
         ENDIF
       ENDDO
cs        write(6,*)'CHECK27'
C***********************************************CORRECTION*FOR*SHAPE********************
      IF(AL.GT.AREF1.AND.LP.LT.20.AND.
     &I.EQ.ISTAR(ISHAPELOC(1))) GOTO 21
       DO IBB=1,NSHAPE
         IF(I.EQ.ISTAR(ISHAPELOC(1))+IBB-1) THEN        
          IF(KL.EQ.0) THEN
       IF(SHAPEMIN(IBB)-AP(ISTAR(ISHAPELOC(1))+IBB-1).LT.-DL1
     &.AND.DL1.LT.0) THEN
       DL1=-DL1
           ENDIF
           IF(SHAPEMAX(IBB)-AP(ISTAR(ISHAPELOC(1))+IBB-1).LT.DL1
     &.AND.DL1.GT.0) THEN
       DL1=-DL1
           ENDIF
          ENDIF
         IF(KL.EQ.1) THEN
      IF(SHAPEMIN(IBB)-EXP(AP(ISTAR(ISHAPELOC(1))+IBB-1)).LT.-DL1
     &.AND.DL1.LT.0) THEN
       DL1=-DL1
          ENDIF
      IF(SHAPEMAX(IBB)-EXP(AP(ISTAR(ISHAPELOC(1))+IBB-1)).LT.DL1
     &.AND.DL1.GT.0) THEN
       DL1=-DL1
          ENDIF
         ENDIF         
         ENDIF
       ENDDO
C***************************************************************************************
cs      write(6,*)'CHECK28'
CDE  121  CONTINUE
cs      WRITE(6,*) I,' -th parameter derivative'
      AP1(I)=AP(I)+DL1
cs      write(6,*)'I,WAVE,NW',I,WAVE,NW
cs      IF(I.GE.1)INDB=1
cs      WRITE(*,*) 'AP(I),DL1 ',I, exp(AP(I)),DL1
cs      WRITE(*,*) 'AP(I),DL1 ',I, exp(AP(I)),DL1 
C*** Forward calculations of phisical problem ***
      CALL forwMN(0,KM,KN,IT,KL,IMSC,NW,WAVE,NSD,NBIN1,NBIN,
     &RADIUS,
     & NLAY,NBRDF,NBRDF1,NSHAPE,IANGL,ANG,ISDLOC,IREFLOC,IBRDFLOC,
     & IBRDF1LOC,ISHAPELOC,ISTAR,
     & AP1,SSAN,EXT,PFN,FP1,INDB,IEL,IS,FLDD,FLUP,FLDR,IC,
     & IPOLPR,DLPN,ANGLESCA,SZAS,FTAU,FUMUY,FPHI,ITOFF,IBRF,IRFS)
cs       INDB=0
cs      IF(I.LT.2)WRITE(*,*) 'AP1(I),DL1 after forw',I,
cs     &exp(AP1(I)),DL1
cs        WRITE(*,*) 'AP1(I),DL1 after forw',I,
cs     &exp(AP1(I)),DL1  
     
      IF(I.Le.2) THEN
      ENDIF
C************************************************
      AP1(I)=AP1(I)-DL1
       IF(INDLD.EQ.0)THEN
       DO J=1, KM
       U(J,I)=(FP1(J)-FP(J))/(DL1)
       IF(I.EQ.-1.AND.J.GE.110.AND.J.LE.KM) THEN
       DO II=34,KN
       WRITE(*,*) II,EXP(AP(II)),EXP(AP1(II))
       ENDDO
       WRITE(*,*) U(J,I),I,
     & EXP(FP1(J)),EXP(FP(J)),
     & EXP(AP(I)),EXP(AP1(I)+DL1)
CD       IF(II.NE.I) WRITE(*,*) EXP(AP(II)),EXP(AP1(II))
       ENDIF
       ENDDO
       ENDIF
       IF(INDLD.EQ.1)THEN
       do IW=1,NW
       do IL=1,NLAY
       do ISD=1,NSD
	IF(SSAN(IW,IL,ISD).LT.1.0E-30)SSAN(IW,IL,ISD)=1.0E-30
	IF(SSA(IW,IL,ISD).LT.1.0E-30)SSA(IW,IL,ISD)=1.0E-30
       US(IW,IL,ISD,I)=(LOG(SSAN(IW,IL,ISD))-LOG(SSA(IW,IL,ISD)))/(DL1)
       enddo
       enddo
       enddo
       do IAN=1,IANGL
       do IW=1,NW
       do IL=1,NLAY
       do ISD=1,NSD
	IF(PFN(IAN,IW,IL,ISD).LT.1.0E-30)PFN(IAN,IW,IL,ISD)=1.0E-30
	IF(PF(IAN,IW,IL,ISD).LT.1.0E-30)PF(IAN,IW,IL,ISD)=1.0E-30
	IF(DLPN(IAN,IW,IL,ISD).LT.1.0E-30)DLPN(IAN,IW,IL,ISD)=1.0E-30
	IF(DLP(IAN,IW,IL,ISD).LT.1.0E-30)DLP(IAN,IW,IL,ISD)=1.0E-30
       UP(IAN,IW,IL,ISD,I)=(LOG(PFN(IAN,IW,IL,ISD))-
     &LOG(PF(IAN,IW,IL,ISD)))/(DL1)
      UPOL(IAN,IW,IL,ISD,I)=(LOG(DLPN(IAN,IW,IL,ISD)+1.)-
     &LOG(DLP(IAN,IW,IL,ISD)+1.))/(DL1)
       enddo
       enddo
       enddo
       enddo
       ENDIF
cs       if(I.LT.2)write(6,*)'DER',(U(J,I),J=1,KM)
cs         write(6,*)'FP1(J)-FP(J)',(FP1(J)-FP(J),J=1,KM)
cs        write(6,*)'DER',(U(J,I),J=1,KM)
cs       IF(I.LE.2) WRITE(6,*) EXP(AP(I)),EXP(AP1(I)+DL1), DL1
   21   ENDDO
cs        WRITE(*,*) DL1,' DL1'
   25 FORMAT(7E12.5)
   
      DO I=1,KN
C     WRITE(6,*) 'AP ',EXP(AP(I))
      ENDDO
      RETURN
      END

      SUBROUTINE PHASEM1(ISD,IEL,NBIN,RADIUS,SD,
     & WAV,RREAL,RIMAG,IANGL,ANG,SSA,EXT,PTP11,PTP12,
     & PTP22,PTP33,PTP34,PTP44,keys,RATN)
C*****************************************************
C***This subroutine reads the kernel matrices for  
C   Phase Function in respect to d(..)/dlnr                  
C
C   INPUT:
C*   NDP    I      - index for reading initial files:
C*             = 0 - the all initial data (kernels) are read
C*                  from initial data files
C*             > 0 - the kernes are used , no reaading
C*   ISD    I  - number of size distribution component
C*   IEL    I  - number of needed ellements of phase matrix
C*            =1 - P11 only
C*            =6 - P11, P12, P22, P33, P34, P44 
C*   NBIN   I  - the number of the bins in the
C*                       size distributions
C*   RADIUS R(KBIN) - the radii corresponding to  
C*                the bins in the size distributions
C*               size distributions
C*   SD     D(NBIN) - size distribution
C*                    Usualy dV/dlnR (it can be also for 
C*                    radii, numbers, areas,
C*                    but should agree with KERNEL)
C*   WAV    R       - wavelength
C*   RREAL  R       - real part of the refractive index
C*   RIMAG  R       - imaginary part of the refractive index
C***************************************************** 
C*  IANG        - the number of angles used for 
C*                   phase function
C*   ANG    R(IANG) - the values of the angles for the phase 
C*                        function modelling;
C*   SSA    R       - single scattering albedo
C*   EXT    R       - extinction
C*   PTPii     R(IANG)- phase matrix (UNNORMALIZED !!!)
C*****************************************************
      PARAMETER (KMES=180,KPAR=100,KPARS=100,KEL=2,KSD=3)
      PARAMETER (KREF=15)
      DIMENSION RADIUS(KPAR),SD(KPAR)
      DIMENSION APF(KEL,KREF,KREF,KMES,KPARS),
     & BPF(KEL,KREF,KREF,KMES,KPARS),
     & ARE(KREF),AIM(KREF),PHASE(KMES)
      DIMENSION US(KMES,KPARS),USB(KMES,KPARS),
     & USFIN(KEL,KMES,KPARS),RW(KPARS),
     & RADIUST(KPARS)
      DIMENSION ANG(KMES)
      DIMENSION A1(KMES,KPARS),A2(KMES,KPARS),
     & B1(KMES,KPARS),B2(KMES,KPARS),
     & AA(KMES,KPARS),BB(KMES,KPARS)
      DIMENSION X(2),Y(2)
      INTEGER KIMM,IANG(KEL)
       DIMENSION
     & PTP11(KMES),PTP12(KMES),PTP22(KMES),
     & PTP33(KMES),PTP34(KMES),PTP44(KMES)
      REAL LINEAR
      PI = 2.* ASIN( 1.0 )
CD      WRITE(*,*) 'IN PHASEM1 !!!'
CD      WRITE(*,*) 'in the BEGINING'
CD      WRITE(*,*) WAV,RREAL,RIMAG,' WAV,RREAL,RIMAG'
CD      DO I=1,NBIN
CD      WRITE(77,*) RADIUS(I), SD(I),' RADIUS, SD'
CD      ENDDO
CD      DO I=1,5
CD       WRITE(77,*) ANG(I), PTP11(I), I,' ANG(I), PTP11(I), I'
CD      ENDDO
      IF(NDP.LT.1) THEN
C****************************************************
C*** READ EXTINCTION, ABSORPTION and PHASE FUNCTION:
C****************************************************
        IF(ISD.EQ.1) THEN
      OPEN (13,file='Rkext1.dat',status='unknown')
      OPEN (23,file='Rkernel1.11.dat',status='unknown')
      OPEN (33,file='Rkernel1.12.dat',status='unknown')
      OPEN (43,file='Rkernel1.22.dat',status='unknown')
      OPEN (53,file='Rkernel1.33.dat',status='unknown')
      OPEN (63,file='Rkernel1.34.dat',status='unknown')
      OPEN (73,file='Rkernel1.44.dat',status='unknown')
      OPEN (113,file='Rkext1.1.dat',status='unknown')
      OPEN (123,file='Rkernel1.11.2.dat',status='unknown')
      OPEN (133,file='Rkernel1.12.2.dat',status='unknown')
      OPEN (143,file='Rkernel1.22.2.dat',status='unknown')
      OPEN (153,file='Rkernel1.33.2.dat',status='unknown')
      OPEN (163,file='Rkernel1.34.2.dat',status='unknown')
      OPEN (173,file='Rkernel1.44.2.dat',status='unknown')
        ENDIF
        IF(ISD.EQ.2) THEN
      OPEN (13,file='Rkext2.dat',status='unknown')
      OPEN (23,file='Rkernel2.11.dat',status='unknown')
      OPEN (33,file='Rkernel2.12.dat',status='unknown')
      OPEN (43,file='Rkernel2.22.dat',status='unknown')
      OPEN (53,file='Rkernel2.33.dat',status='unknown')
      OPEN (63,file='Rkernel2.34.dat',status='unknown')
      OPEN (73,file='Rkernel2.44.dat',status='unknown')
      OPEN (113,file='Rkext2.1.dat',status='unknown')
      OPEN (123,file='Rkernel2.11.2.dat',status='unknown')
      OPEN (133,file='Rkernel2.12.2.dat',status='unknown')
      OPEN (143,file='Rkernel2.22.2.dat',status='unknown')
      OPEN (153,file='Rkernel2.33.2.dat',status='unknown')
      OPEN (163,file='Rkernel2.34.2.dat',status='unknown')
      OPEN (173,file='Rkernel2.44.2.dat',status='unknown')
        ENDIF
        IF(ISD.EQ.3) THEN
      OPEN (13,file='Rkext3.dat',status='unknown')
      OPEN (23,file='Rkernel3.11.dat',status='unknown')
      OPEN (33,file='Rkernel3.12.dat',status='unknown')
      OPEN (43,file='Rkernel3.22.dat',status='unknown')
      OPEN (53,file='Rkernel3.33.dat',status='unknown')
      OPEN (63,file='Rkernel3.34.dat',status='unknown')
      OPEN (73,file='Rkernel3.44.dat',status='unknown')
      OPEN (113,file='Rkext3.1.dat',status='unknown')
      OPEN (123,file='Rkernel3.11.2.dat',status='unknown')
      OPEN (133,file='Rkernel3.12.2.dat',status='unknown')
      OPEN (143,file='Rkernel3.22.2.dat',status='unknown')
      OPEN (153,file='Rkernel3.33.2.dat',status='unknown')
      OPEN (163,file='Rkernel3.34.2.dat',status='unknown')
      OPEN (173,file='Rkernel3.44.2.dat',status='unknown')
       ENDIF
   12  FORMAT (7E16.7) 
      DO IELL=1,IEL+1
      READ(3+10*IELL,*) KNN, ID, RWMIN,RWMAX,RATIO
      IF(KNN.LT.0) THEN
       KNN1=-KNN
      ELSE
       KNN1=KNN
      ENDIF
      READ(3+10*IELL,*)  (RW(IP),IP=1, KNN1)
      READ(3+10*IELL,*)   IANG(IELL),(ANG(I),I=1,IANG(IELL))
      READ(3+10*IELL,*)   RREMIN, RREMAX
      READ(3+10*IELL,*)   RIMMIN, RIMMAX
      READ(3+10*IELL,*) KRR, KIMM,IA
      IF(KNN.LT.0) THEN
       KNN1=-KNN
        DO IB=1,NBIN-2
        IF((LOG(RADIUS(I+1))-LOG(RADIUS(I)))-(LOG(RADIUS(I+1))-
     & LOG(RADIUS(I))) .GT.0.001) 
     & WRITE(*,*) "Bining in matrix disagree with vector"
        ENDDO   
      ELSE
        DO IB=1,NBIN-2
        IF((RADIUS(I+1)-RADIUS(I))-(RADIUS(I+1)-RADIUS(I)) 
     & .GT.0.001) 
     & WRITE(*,*) "Bining in matrix disagree with vector"
        ENDDO
       KNN1=KNN
      ENDIF
      IF(KRR.LT.0) THEN
       KRR1=-KRR
      ELSE
       KRR1=KRR
      ENDIF
      IF(KIMM.LT.0) THEN
       KIMM1=-KIMM
      ELSE
       KIMM1=KIMM
      ENDIF
      DO IR=1, KRR1
       DO IM=1,KIMM1
          READ (3+10*IELL,*) ARE(IR),AIM(IM)
          AIM(IM)=-AIM(IM)
        DO IAN=1, IANG(IELL)
          READ (3+10*IELL,*) (APF(IELL,IR,IM,IAN,IRW),IRW=1,KNN1) 
        ENDDO
       ENDDO
      ENDDO
      CLOSE (3+10*IELL)
      IF(IA.EQ.1) THEN
      READ(103+10*IELL,*) KNN, ID, RWMIN,RWMAX,RATIO
      READ(103+10*IELL,*)  (RW(I),I=1, KNN1)
      READ(103+10*IELL,*)  IANG(IELL), (ANG(I),I=1, IANG(IELL))
      READ(103+10*IELL,*)   RREMIN, RREMAX
      READ(103+10*IELL,*)   RIMMIN, RIMMAX
      READ(103+10*IELL,*) KRR, KIMM,IA
      IF(KNN.LT.0) THEN
       KNN1=-KNN
        DO IB=1,NBIN-2
        IF((LOG(RADIUS(I+1))-LOG(RADIUS(I)))-(LOG(RADIUS(I))-
     & LOG(RADIUS(I-1))) .GT.0.001) 
     & WRITE(*,*) "Bining in matrix disagree with vector"
        ENDDO   
      ELSE
        DO IB=1,NBIN-2
        IF((RADIUS(I+1)-RADIUS(I))-(RADIUS(I)-RADIUS(I-1))
     & .GT.0.001) 
     & WRITE(*,*) "Bining in matrix disagree with vector"
        ENDDO
       KNN1=KNN
      ENDIF
      IF(KRR.LT.0) THEN
       KRR1=-KRR
      ELSE
       KRR1=KRR
      ENDIF
      IF(KIMM.LT.0) THEN
       KIMM1=-KIMM
      ELSE
       KIMM1=KIMM
      ENDIF
      DO IR=1, KRR1
       DO IM=1,KIMM1
          READ (103+10*IELL,*) ARE(IR),AIM(IM)
          AIM(IM)=-AIM(IM)
        DO IAN=1, IANG(IELL)
          READ (103+10*IELL,*) (BPF(IELL,IR,IM,IAN,IRW),IRW=1,KNN1) 
        ENDDO
       ENDDO
      ENDDO
      CLOSE (103+10*IELL)
      ENDIF
      IANGL=IANG(2)
      WRITE(*,*) IELL, 'AFTER READING ELLEMENT KERNEL'
      ENDDO ! for IELL loop1
      NDP=NDP+1
      ENDIF
      RL=RREAL
      RIMAG=-RIMAG
CD      WRITE(*,*) WAV,RREAL,RIMAG,' WAV,RREAL,RIMAG, 1'
      RI=RIMAG
CD      WRITE(*,*) RREAL,RI,' REAL, IMAG'
CD      WRITE(*,*) (AIM(I),I=1,KIMM1),' AIM'
      I0=0
      I1=0
      K0=0
      K1=0
      DO I=1,KRR1
      IF(RL.GE.ARE(I).AND.RL.LE.ARE(I+1)) THEN
          I0=I
          I1=I+1
      ENDIF
      ENDDO
      DO I=1,KIMM1
      IF(RI.GE.AIM(I).AND.RI.LE.AIM(I+1)) THEN
CD      IF(RI.LE.AIM(I).AND.RI.GE.AIM(I+1)) THEN
          K0=I
          K1=I+1
      ENDIF
      ENDDO
      IF(RL.LE.ARE(1)) THEN
      I0=1
      I1=2
      RREAL=ARE(1)
      RL=ARE(1)
      ENDIF
      IF(RL.GE.ARE(KRR1)) THEN
      I0=KRR1-1
      I1=KRR1
      RREAL=ARE(KRR1)
      RL=ARE(KRR1)
      ENDIF
CD      WRITE(*,*) RI,AIM(1),' RI,AIM(1)'
      IF(RI.LE.AIM(1)) THEN
CD      IF(RI.GE.AIM(1)) THEN
      K0=1
      K1=2
      RIMAG=AIM(1)
      RI=AIM(1)
      ENDIF
      IF(RI.GE.AIM(KIMM1)) THEN
CD      IF(RI.LE.AIM(KIMM1)) THEN
      K0=KIMM1-1
      K1=KIMM1
      RIMAG=AIM(KIMM1)
      RI=AIM(KIMM1)
      ENDIF
CD      WRITE(*,*) RIMAG,' RIMAG, 2'
C*** Determination of the partial derivatives regarding
C*** size distribution
      DO IELL=1,IEL+1
      DO I=1,KNN1
       DO J=1,IANG(IELL)
        IF(IELL.EQ.1.OR.IELL.EQ.2.OR.IELL.EQ.4) THEN
		IF(APF(IELL,I0,K0,J,I).LT.1.0E-30)APF(IELL,I0,K0,J,I)=1.0E-30
		IF(APF(IELL,I0,K1,J,I).LT.1.0E-30)APF(IELL,I0,K1,J,I)=1.0E-30
		IF(APF(IELL,I1,K0,J,I).LT.1.0E-30)APF(IELL,I1,K0,J,I)=1.0E-30
		IF(APF(IELL,I1,K1,J,I).LT.1.0E-30)APF(IELL,I1,K1,J,I)=1.0E-30

           A1(J,I)=LOG(APF(IELL,I0,K0,J,I))
           A2(J,I)=LOG(APF(IELL,I0,K1,J,I))
           B1(J,I)=LOG(APF(IELL,I1,K0,J,I))
           B2(J,I)=LOG(APF(IELL,I1,K1,J,I))
        ELSE
           A1(J,I)=APF(IELL,I0,K0,J,I)
           A2(J,I)=APF(IELL,I0,K1,J,I)
           B1(J,I)=APF(IELL,I1,K0,J,I)
           B2(J,I)=APF(IELL,I1,K1,J,I)
        ENDIF
        X(1)=ARE(I0)
        X(2)=ARE(I1)
        IF(IELL.EQ.1.OR.IELL.EQ.2.OR.IELL.EQ.4) THEN
		IF(APF(IELL,I0,K0,J,I).LT.1.0E-30)APF(IELL,I0,K0,J,I)=1.0E-30
		IF(APF(IELL,I1,K0,J,I).LT.1.0E-30)APF(IELL,I1,K0,J,I)=1.0E-30
         Y(1)=LOG(APF(IELL,I0,K0,J,I))
         Y(2)=LOG(APF(IELL,I1,K0,J,I))
        ELSE
         Y(1)=APF(IELL,I0,K0,J,I)
         Y(2)=APF(IELL,I1,K0,J,I)
        ENDIF
        AA(J,I)=LINEAR( X, Y, 2, RL )
        IF(IELL.EQ.1.OR.IELL.EQ.2.OR.IELL.EQ.4) THEN
		IF(APF(IELL,I0,K1,J,I).LT.1.0E-30)APF(IELL,I0,K1,J,I)=1.0E-30
		IF(APF(IELL,I1,K1,J,I).LT.1.0E-30)APF(IELL,I1,K1,J,I)=1.0E-30
          Y(1)=LOG(APF(IELL,I0,K1,J,I))
          Y(2)=LOG(APF(IELL,I1,K1,J,I))
        ELSE
          Y(1)=APF(IELL,I0,K1,J,I)
          Y(2)=APF(IELL,I1,K1,J,I)  
        ENDIF      
        BB(J,I)=LINEAR( X, Y, 2, RL )
CD        X(1)=AIM(K0)
CD        X(2)=AIM(K1)
	  IF(AIM(K0).LT.1.0E-30)AIM(K0)=1.0E-30
	  IF(AIM(K1).LT.1.0E-30)AIM(K1)=1.0E-30
	  IF(RI.LT.1.0E-30)RI=1.0E-30
        X(1)=LOG(AIM(K0))
        X(2)=LOG(AIM(K1))
        RI=LOG(RI)
        Y(1)=AA(J,I)
        Y(2)=BB(J,I)
        IF(IELL.EQ.1.OR.IELL.EQ.2.OR.IELL.EQ.4) THEN
          US(J,I)=EXP(LINEAR( X, Y, 2, RI ))
        ELSE
          US(J,I)=LINEAR( X, Y, 2, RI )
        ENDIF
        RI=(EXP(RI))
       ENDDO
      ENDDO
      IF(IELL.EQ.1) THEN 
      ENDIF
C***  In the case if we use trapezium approxinamtion' (IA.EQ.2)
C***  we have two coefficients A and B and 
C***  we make all interpolation separately for A and B, i.e.
C***  all above was for A and below will be for B    
      IF(IA.EQ.1) THEN
C*** B-coef. of kernel matrix for trapezioum approximation ***
C***
C*** Determination of the partial derivatives regarding
C*** size distribution
       DO I=1,KNN1
        DO J=1,IANG(IELL)
        IF(IELL.EQ.1.OR.IELL.EQ.2.OR.IELL.EQ.4) THEN
		IF(BPF(IELL,I0,K0,J,I).LT.1.0E-30)BPF(IELL,I0,K0,J,I)=1.0E-30
		IF(BPF(IELL,I0,K1,J,I).LT.1.0E-30)BPF(IELL,I0,K1,J,I)=1.0E-30
		IF(BPF(IELL,I1,K0,J,I).LT.1.0E-30)BPF(IELL,I1,K0,J,I)=1.0E-30
		IF(BPF(IELL,I1,K1,J,I).LT.1.0E-30)BPF(IELL,I1,K1,J,I)=1.0E-30
          A1(J,I)=LOG(BPF(IELL,I0,K0,J,I))
          A2(J,I)=LOG(BPF(IELL,I0,K1,J,I))
          B1(J,I)=LOG(BPF(IELL,I1,K0,J,I))
          B2(J,I)=LOG(BPF(IELL,I1,K1,J,I))
        ELSE
          A1(J,I)=BPF(IELL,I0,K0,J,I)
          A2(J,I)=BPF(IELL,I0,K1,J,I)
          B1(J,I)=BPF(IELL,I1,K0,J,I)
          B2(J,I)=BPF(IELL,I1,K1,J,I)
        ENDIF
        X(1)=ARE(I0)
        X(2)=ARE(I1)
        IF(IELL.EQ.1.OR.IELL.EQ.2.OR.IELL.EQ.4) THEN
         IF(BPF(IELL,I0,K0,J,I).LT.1.0E-30)BPF(IELL,I0,K0,J,I)=1.0E-30
         IF(BPF(IELL,I1,K0,J,I).LT.1.0E-30)BPF(IELL,I1,K0,J,I)=1.0E-30
         Y(1)=LOG(BPF(IELL,I0,K0,J,I))
         Y(2)=LOG(BPF(IELL,I1,K0,J,I))
        ELSE
         Y(1)=BPF(IELL,I0,K0,J,I)
         Y(2)=BPF(IELL,I1,K0,J,I)
        ENDIF
        AA(J,I)=LINEAR( X, Y, 2, RL )
        IF(IELL.EQ.1.OR.IELL.EQ.2.OR.IELL.EQ.4) THEN
         IF(BPF(IELL,I0,K1,J,I).LT.1.0E-30)BPF(IELL,I0,K1,J,I)=1.0E-30
         IF(BPF(IELL,I1,K1,J,I).LT.1.0E-30)BPF(IELL,I1,K1,J,I)=1.0E-30
         Y(1)=LOG(BPF(IELL,I0,K1,J,I))
         Y(2)=LOG(BPF(IELL,I1,K1,J,I))
        ELSE
          Y(1)=BPF(IELL,I0,K1,J,I)
          Y(2)=BPF(IELL,I1,K1,J,I) 
        ENDIF       
        BB(J,I)=LINEAR( X, Y, 2, RL )
CD        X(1)=AIM(K0)
CD        X(2)=AIM(K1)
	  IF(AIM(K0).LT.1.0E-30)AIM(K0)=1.0E-30
	  IF(AIM(K1).LT.1.0E-30)AIM(K1)=1.0E-30
	  IF(RI.LT.1.0E-30)RI=1.0E-30
        X(1)=LOG(AIM(K0))
        X(2)=LOG(AIM(K1))
        RI=LOG(RI)
        Y(1)=AA(J,I)
        Y(2)=BB(J,I)
        IF(IELL.EQ.1.OR.IELL.EQ.2.OR.IELL.EQ.4) THEN
          USB(J,I)=EXP(LINEAR( X, Y, 2, RI ))
        ELSE
          USB(J,I)=LINEAR( X, Y, 2, RI )
        ENDIF
        RI=(EXP(RI))
        ENDDO
       ENDDO
      ENDIF
C*** Implementing the transition from partial derivatives
C*** given via size parameter and complete set scattering 
C*** angle (as it is given in matrices "Rkernel") to
C*** actual wavelenghs and sizes
      DO IAN=1,IANG(IELL)
       DO I=1,NBIN
            USFIN(IELL,IAN,I)=0.0
       ENDDO
      ENDDO       
      DO I=1,KNN1
       RADIUST(I)=RW(I)*WAV/2./PI
      ENDDO
      IF((RADIUST(1)-RADIUS(1)).GT.0.01*RADIUST(1)) WRITE(*,*) 
     &'matrix is not prepared for such small particles'
       IF((RADIUS(NBIN)-RADIUST(KNN1)).GT.0.01*RADIUST(1)) WRITE(*,*) 
     & 'matrix is not prepared for such large particles'
      IF(KNN.GT.0) THEN
       DDR=RADIUS(2)-RADIUS(1)
       DDRT=RADIUST(2)-RADIUST(1)
      ENDIF
      IF(KNN.LT.0) THEN
	 IF(RADIUS(2).LT.1.0E-30)RADIUS(2)=1.0E-30
	 IF(RADIUS(1).LT.1.0E-30)RADIUS(1)=1.0E-30
       DDR=LOG(RADIUS(2))-LOG(RADIUS(1))
       DDRT=LOG(RADIUST(2))-LOG(RADIUST(1))
      ENDIF
*** calculation of kernel matrix for rectangular approximation ***
       IF (IA.EQ.0) THEN
       DO I=1,NBIN
        IF(KNN.LT.0) THEN
		IF(RADIUS(I).LT.1.0E-30)RADIUS(I)=1.0E-30
        RMIN=LOG(RADIUS(I))-DDR/2.
CD!        IF(I.EQ.1) RMIN=LOG(RADIUS(1))
        RMAX=LOG(RADIUS(I))+DDR/2.
CD!        IF(I.EQ.NBIN) RMAX=LOG(RADIUS(NBIN))
        ENDIF
        IF(KNN.GT.0) THEN
        RMIN=RADIUS(I)-DDR/2.
CD!        IF(I.EQ.1) RMIN=RADIUS(1)
        RMAX=RADIUS(I)+DDR/2.
CD!        IF(I.EQ.NBIN) RMAX=RADIUS(NBIN)
        ENDIF
        DO IAN=1,IANG(IELL)
          DO II=1,KNN1
            IF(KNN.LT.0) THEN
		IF(RADIUS(II).LT.1.0E-30)RADIUS(II)=1.0E-30
		IF(RADIUS(1).LT.1.0E-30)RADIUS(1)=1.0E-30
		IF(RADIUS(KNN1).LT.1.0E-30)RADIUS(KNN1)=1.0E-30
             RMINT=LOG(RADIUST(II))-DDRT/2.
             IF(II.EQ.1) RMINT=LOG(RADIUST(1))
             RMAXT=LOG(RADIUST(II))+DDRT/2.  
             IF(II.EQ.KNN1) RMAXT=LOG(RADIUST(KNN1))     
            ENDIF
            IF(KNN.GT.0) THEN
             RMINT=RADIUST(II)-DDRT/2.
             IF(II.EQ.1) RMINT=RADIUST(1)
             RMAXT=RADIUST(II)+DDRT/2.
             IF(II.EQ.KNN1) RMAXT=RADIUST(KNN1)  
            ENDIF
       IF(RMIN.LT.RMAXT.AND.RMAX.GT.RMINT) THEN       
           IF(RMIN.LE.RMINT) THEN
            IF(RMAX.GE.RMAXT) THEN
            USFIN(IELL,IAN,I)=USFIN(IELL,IAN,I)+US(IAN,II)
            ENDIF
            IF(RMAX.GT.RMINT.AND.RMAX.LT.RMAXT) THEN
            USFIN(IELL,IAN,I)=
     & USFIN(IELL,IAN,I)+US(IAN,II)*(RMAX-RMINT)/(RMAXT-RMINT)
             ENDIF
           ELSE
            IF(RMIN.LT.RMAXT) THEN
             IF(RMAX.GT.RMAXT) THEN
       USFIN(IELL,IAN,I)=
     & USFIN(IELL,IAN,I)+US(IAN,II)*(RMAXT-RMIN)/(RMAXT-RMINT)
             ELSE
        USFIN(IELL,IAN,I)=
     & USFIN(IELL,IAN,I)+US(IAN,II)*(RMAX-RMIN)/(RMAXT-RMINT)
             ENDIF
            ENDIF 
           ENDIF
         ENDIF
          ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF (IA.EQ.1) THEN
       DO I=1,NBIN-1
        IF(KNN.LT.0) THEN
		IF(RADIUS(I+1).LT.1.0E-30)RADIUS(I+1)=1.0E-30
		IF(RADIUS(I).LT.1.0E-30)RADIUS(I)=1.0E-30
        RMAX=LOG(RADIUS(I+1))
        RMIN=LOG(RADIUS(I))
        ENDIF
        IF(KNN.GT.0) THEN
        RMAX=RADIUS(I+1)
        RMIN=RADIUS(I)
        ENDIF
        DO IAN=1,IANG(IELL)
          DO II=1,KNN1-1
           IF(KNN.LT.0) THEN
       	 IF(RADIUS(II+1).LT.1.0E-30)RADIUS(II+1)=1.0E-30
           IF(RADIUS(II).LT.1.0E-30)RADIUS(II)=1.0E-30
           RMAXT=LOG(RADIUST(II+1))
           RMINT=LOG(RADIUST(II))
           ENDIF
           IF(KNN.GT.0) THEN
           RMAXT=RADIUST(II+1)
           RMINT=RADIUST(II)
           ENDIF
       IF(RMIN.LT.RMAXT.AND.RMAX.GT.RMINT) THEN       
           IF(RMIN.LE.RMINT) THEN
            IF(RMAX.GE.RMAXT) THEN
            USFIN(IELL,IAN,I)=USFIN(IELL,IAN,I)+US(IAN,II)
            USFIN(IELL,IAN,I+1)=USFIN(IELL,IAN,I+1)+USB(IAN,II+1)
            ENDIF
            IF(RMAX.GT.RMINT.AND.RMAX.LT.RMAXT) THEN
            USFIN(IELL,IAN,I)=
     & USFIN(IELL,IAN,I)+US(IAN,II)*(RMAX-RMINT)/(RMAXT-RMINT)
            USFIN(IELL,IAN,I+1)=
     & USFIN(IELL,IAN,I+1)+USB(IAN,II+1)*(RMAX-RMINT)/(RMAXT-RMINT)
             ENDIF
           ELSE
            IF(RMIN.LT.RMAXT) THEN
             IF(RMAX.GT.RMAXT) THEN
       USFIN(IELL,IAN,I)=
     & USFIN(IELL,IAN,I)+US(IAN,II)*(RMAXT-RMIN)/(RMAXT-RMINT)
       USFIN(IELL,IAN,I+1)=
     & USFIN(IELL,IAN,I+1)+USB(IAN,II+1)*(RMAXT-RMIN)/(RMAXT-RMINT)
             ELSE
        USFIN(IELL,IAN,I)=
     & USFIN(IELL,IAN,I)+US(IAN,II)*(RMAX-RMIN)/(RMAXT-RMINT)
        USFIN(IELL,IAN,I+1)=
     & USFIN(IELL,IAN,I+1)+USB(IAN,II+1)*(RMAX-RMIN)/(RMAXT-RMINT)
             ENDIF
            ENDIF 
           ENDIF
         ENDIF
          ENDDO
        ENDDO
       ENDDO
      ENDIF
C*** Calculation of EXT, SSA and Phase Function:
C*** SSA,EXT,PTP11,PTP12,PTP22,PTP33,PTP34,PTP44
      IF(IELL.EQ.1) THEN
       EXT=0.
       SSA=0.
       ABS0=0.
        DO IB=1,NBIN 
         EXT=EXT+USFIN(IELL,1,IB)*SD(IB)/WAV
CD         WRITE(*,*) SD(IB), USFIN(IELL,1,IB)/WAV,IB
         ABS0=ABS0+USFIN(IELL,2,IB)*SD(IB)/WAV
        ENDDO
         SSA=(EXT-ABS0)/EXT
       ENDIF
      IF (IELL.GT.1) THEN
       DO I=1,IANG(IELL)
       PHASE(I)=0.
       ENDDO
       DO I=1,IANG(IELL)
        DO IB=1,NBIN 
        PHASE(I)=PHASE(I)+USFIN(IELL,I,IB)*SD(IB)/WAV
       ENDDO
        IF(IELL.EQ.2) PTP11(I)=PHASE(I)
        IF(IELL.EQ.3) PTP12(I)=PHASE(I)
        IF(IELL.EQ.4) PTP22(I)=PHASE(I)
        IF(IELL.EQ.5) PTP33(I)=PHASE(I)
        IF(IELL.EQ.6) PTP34(I)=PHASE(I)
        IF(IELL.EQ.7) PTP44(I)=PHASE(I)
       ENDDO
       ENDIF
      ENDDO ! for IELL loop1
CD      WRITE(*,*) 'in the END'
CD      WRITE(*,*) WAV,RREAL,RIMAG,' WAV,RREAL,RIMAG'
CD      DO I=1,NBIN
CD      WRITE(77,*) RADIUS(I), SD(I),' RADIUS, SD'
CD      ENDDO
CD      WRITE(*,*) EXT,SSA,' EXT,SSA in PHASEM'
CD      DO I=1,10
CD       WRITE(*,*) ANG(I), PTP11(I)/EXT/SSA, I,' ANG(I), PTP11(I), I'
CD      ENDDO
CD      DO I=175,180
CD       WRITE(*,*) ANG(I), PTP11(I)/EXT/SSA, I,' ANG(I), PTP11(I), I'
CD      ENDDO
   33 FORMAT(7E16.7)
      RETURN
      END
C*******************************************************************




      SUBROUTINE PHASEM0(KM,ANGL,KN,KSIM,ID,RMIN,RMAX,WAV,RRE,RIM,X,Y,
     & KSDD,CM1,SM,RMM,NMD,ISD,AE,SSA1,PTP11,PTP12,PTP22,PTP33,PTP34,
     & PTP44,keys,RATN)
C****************************************************************************
C**    Calculates phase function, extinction and absorption for given      ** 
C**    size distributions d()/dlnR                                         **
C****************************************************************************
C   INPUT:
C************
C    KM    I   - number of angles for phase function on 1 wavelength
C    KN    I(KSD)   - number of grid radius points 
C       <0 -logarithmic intervals
C       >0 -linear intervals
C    KSIM  I   - number of points for Simpson integration
C       <0 -logarithmic intervals
C       >0 -linear intervals
C    ID    I - dimension of size distribution d(...)/dlnR
C       = 0 - number
C       = 1 - radius
C       = 2 - area
C       = 3 - volume
C    RMIN  R   - minimun radius (mkm)
C    RMAX  R   - maximum radius (mkm)
C    WAV   R   - wavelength  (mkm)
C    RRE   R   - real part of refrective index
C    RIM   R   - imaginary part of refrective index
C    KSDD I
C      = 0 - size distribution is simmualted as muli-modal log-normal
C      = 1 - size distribution is read from file: SizeDIs.dat 

C    CM1    R(NMD,NSD) - columnar concentrations for each component 
C                       and mode ( 1/mkm**2 - ID=0, mkm/mkm**2 - ID=1,  
C                       mkm**2/mkm**2 - ID=2, mkm**3/mkm**2 - ID=3)
C    SM    R(NMD,NSD) - halfwidths for each component and mode
C    RMM   R(NMD,NSD) - mean radii for each component and mode (mkm)
C    ISD I - the number of the particle component
C    NMD I - number of modes in each component
C   OUTPUT:
C*************
C    PTP11,PTP12,PTP22,PTP33,PTP34,PTP44    R(IME0) - phase matrix
C                                         
C    AE    R       - extinction 
C    AAB   R       - absorption 
C*****************************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
******************************************************************************
*   Electromagnetic Scattering on Single Spherical Particle                  *
*                   MIE - program                                           *
* -------------------------------------------------------------------------- *
*    System of units :                                                       *
*     [mkm]     - (for sizes of particles and wave length)                   *
******************************************************************************
      PARAMETER (IME=180) !-MAXIMAL number of the angles (0,1,...,IME)
      PARAMETER (KMES=180,KSD=3,KMD=2,KPAR=100)
      PARAMETER (PI=3.14159265359,PI2=PI+PI)
      INTEGER KN(KSD)
      REAL A(KPAR),B(KPAR),R
* ---------------------------------------------------------------------------- *
      COMPLEX*16 CM,s1(0:ime),s2(0:ime),S1C
      COMPLEX*16 S2C
      REAL*8 cugol(0:ime),si2(0:ime),UGOL(0:IME)
     *,p(0:ime),pp(0:ime),ppp(0:ime),t(0:ime),tt(0:ime),ttt(0:ime)
     *,P1(0:IME),P2(0:IME),P3(0:IME),P4(0:IME)
       REAL*4 pppp1,pppp2,pppp3,pppp4,pppp5
*------------------------------------------------------------------------------*
      DIMENSION CM1(KMD,KSD),SM(KMD,KSD),RMM(KMD,KSD)
      REAL PTP11(KMES),PTP12(KMES),PTP22(KMES),PTP33(KMES)
     &            ,PTP34(KMES),PTP44(KMES),
     & PTP011(KMES),PTP012(KMES),PTP022(KMES),PTP033(KMES)
     &            ,PTP034(KMES),PTP044(KMES),
     & ANGL(KMES)
      REAL LINEAR
      REAL X(KPAR),X1(KPAR),X2(KPAR),X3(KPAR),Y(KPAR),Y1(KPAR),
     &Y2(KPAR),Y3(KPAR)
      IF(III.EQ.0) THEN
      OPEN (9,file='angles.d',status='old')
      READ (9,*) KM
      KMM=KM
      READ(9,*) (UGOL(J),J=0,KMM-1)
      CLOSE (9)
                DO I=0,KMM-1
CD      ANGL(I+1)=UGOL(I)
      CUGOL(I)=DCOS(UGOL(I)*PI/180.D0)
                END DO
      ENDIF
      DO I=0,KMM-1
      ANGL(I+1)=UGOL(I)
      ENDDO
      III=III+1
CD      WRITE(*,*) KN(ISD),' KN(ISD)'
CD      WRITE(*,*) RMIN,RMAX,' RMIN,RMAX'
CD      WRITE(*,*) WAV,RRE,RIM,' WAV,RRE,RIM'
CD      WRITE(*,*) (X(I),I=1,KN(ISD)),' X'
CD      WRITE(*,*) (Y(I),I=1,KN(ISD)),' Y'
      KSIMP=KSIM
      IF(KSIM.LT.0) KSIMP=-KSIM
***************************************************************
*** Definition od SD from file:
CD      WRITE(*,*) keys, "keys, STEP 1"
CD      WRITE(*,*) ISD,KSDD,' ISD,KSDD'
CD      IF (KSDD.EQ.1) THEN
CD 
CD      ENDIF
***************************************************************
CD           WRITE(*,*) "STEP 2"
      DO J=1,KM
      PTP11(J)=0.
      PTP12(J)=0.
      PTP22(J)=0.
      PTP33(J)=0.
      PTP34(J)=0.
      PTP44(J)=0.
      ENDDO
      AE=0.
      AAB=0.
      AC=0.
        do 999 IK=1,KSIMP
      IF(KSIM.LT.0) THEN
	IF(RMAX.LT.1.0E-30)RMAX=1.0E-30
	IF(RMIN.LT.1.0E-30)RMIN=1.0E-30
	IF(DRMIN.LT.1.0E-30)DRMIN=1.0E-30
      rdels   = (dlog(dble(rmax)) -dlog(dble(drmin)))/(KSIMP-1)
      r   = EXP(dlog(dble(rmin))+rdels*(IK-1))
      ELSE
      rdels   = (rmax -rmin)/(KSIMP-1)
      r   = rmin+rdels*(IK-1)
      ENDIF
       II=IK/2
       IF(II*2.LT.IK) AD=2./3.
       IF(II*2.EQ.IK) AD=4./3.
CD       IF(II*2.LT.IK) AD=4./3.
CD       IF(II*2.EQ.IK) AD=2./3.
       IF(IK.EQ.1.OR.IK.EQ.KSIMP) AD=1./3.
CD         WRITE(*,*) 'STEP 2.1'
      IF(keys.eq.1) then
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FOMIN
*****     Initial information  (as an example) *****
*       R  = 10.        ! mkm  - Radius
*       PQ = 1.33       ! Real part of the complex refractiv index (CM)
*       QQ = -0.1       ! Imaginary part of the complex refractiv index
*       BDA= 0.55       ! mkm  - Wavelength
*                do i=0,ime           ! you can use any angular grid
*                ugol(i)=i*180.d0/ime ! here uniform grid is used
*                end do
*----------------------------------------------------------------------*
       PQ = RRE
       QQ = RIM
       CM=CMPLX(PQ,QQ)
***********************************************************************
*  SC,EXT       - Scattering and Total coefficients                   *
*  P1,P2,P3,P4  - the parameters of Stokes matrix (devided by SC)     *
*  P=(P1+P2)/2  - Phase function                                      *
*  UGOL         - the array (0,1,...,IME) of the angles (in degries)  *
*  IME          - its dimension                                       *
*  CM           - complex refractive index at wave length BDA         *
*  BDA          - wave length                                         *
***********************************************************************
      BDA=WAV
CD             WRITE(*,*) RRE,RIM,WAV,' RRE,RIM,WAV'
      IME1=KMM-1
                     XMAX=PI2*R/BDA
CD                WRITE(*,*) 'STEP 2.1'
      CALL MIE(XMAX,CM,IME1,S1,S2,CUGOL,SC,EXT,P,PP,PPP,T,TT,TTT,SI2) 
CD                WRITE(*,*) ' after MIE'
      CALL  BLL(XMAX,CM,SC,EXT)
CD UNIT adjastment:
           CONST=BDA**2/PI/(SC*PI*R**2)/(4.*PI)  ! norma = 1/4pi)
            SC=SC*R**2
            EXT=EXT*R**2
          RAZMER=PI*(1D-9)**2 *1D15 ! to obtain cross sections
CD**** TO transfer to mkm:
          RAZMER=RAZMER*1000.0
            SC=SC*RAZMER   ! scattering cross section
           EXT=EXT*RAZMER  ! total cross section                 
                                    DO I=0,IME1
                          S1C=CONJG(S1(I))
                          S2C=CONJG(S2(I))
                    P1(I)=S1(I)*S1C*CONST ! P1 Stokes matrix element
                    P2(I)=S2(I)*S2C*CONST ! P2  .....
                    S1C=S1(I)*S2C
                    P3(I)=REAL(S1C)*CONST ! P3   ....
                    P4(I)=-DIMAG(S1C)*CONST ! P4 ....
                                    END DO
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CD        WRITE(*,*) "STEP 3"
       ENDIF
C***   Size dIstribution definition:
      ASZD=0. 
CD      WRITE(*,*) 'BEFORE SLOG'
      IF (KSDD.EQ.0) THEN
      DO IM=1,NMD
CD         WRITE(*,*) IM,NMD,'IM,NMD'
CD         WRITE(*,*) CM1(IM,ISD),SM(IM,ISD),RMM(IM,ISD),' CM,SM,RMM'
         ASZD1=SLOG(CM1(IM,ISD),EXP(SM(IM,ISD)),RMM(IM,ISD),R)
         ASZD=ASZD+ASZD1
      ENDDO
      ENDIF
CD      WRITE(*,*) 'AFTER SLOG'
      IF (KSDD.EQ.1) THEN
      IF(KN(ISD).LT.0) KNN=-KN(ISD)
      IF(KN(ISD).GT.0) KNN=KN(ISD)
CD!      IF(KN(ISD).LT.0) R11=LOG(R)
CD!       IF (KN(ISD).LT.0) ASZD=EXP(LINEAR( X, Y, KNN, R11))
CD!       IF (KN(ISD).GT.0) ASZD=LINEAR( X, Y, KNN, R)
       ASZD=LINEAR( X, Y, KNN, R)
      ENDIF
C********************************************************************
C*  Size Distribution Dimension:
C********************************************************************
      IF(ID.EQ.0) RDIM=1.0
      IF(ID.EQ.1) RDIM=R
      IF(ID.EQ.2) RDIM=PI*R*R
      IF(ID.EQ.3) RDIM=4./3.*(PI*R*R*R)
      IF(ID.EQ.4) RDIM=4./3.*(PI*R*R*R*R)
      AAA=ASZD*AD*RDELS/RDIM
CD      WRITE(*,*)  ASZD,AD,RDELS/RDIM,' ASZD,AD,RDELS/RDIM'
      DO J=1,KM
        IF(keys.eq.1) then
c*** from MIE
          PTP11(J)=PTP11(J)+0.5D0*(P1(J-1)+P2(J-1)) *SC*AAA*(4.*PI)
CD          IF(J.EQ.1) THEN
CD          write(*,*) PTP11(J),(P1(J-1)+P2(J-1)),SC,AAA,IK,
CD     &' PP,SC,AAA,IK'
CD          ENDIF
C !!!          PTP12(J)=PTP12(J)+0.5D0*(P2(J-1)-P1(J-1))*SC*AAA*(4.*PI)
C !!!          PTP22(J)=PTP11(J)
C !!!          PTP33(J)=PTP33(J)+P3(J-1)*SC*AAA*(4.*PI)                 
C !!!          PTP34(J)=PTP34(J)+P4(J-1)*SC*AAA*(4.*PI)  
C !!!          PTP44(J)=PTP33(J)
        ELSE
c*** from T-MATRIX or PING YANG code
		write(*,*)'CSCAT USED BUT NOT DEFINED'
		STOP
          PTP11(J)=PTP11(J)+PTP011(J)*AAA*CSCAT/1000.
          PTP12(J)=PTP12(J)+PTP012(J)*AAA*CSCAT/1000.
          PTP22(J)=PTP22(J)+PTP022(J)*AAA*CSCAT/1000.
          PTP33(J)=PTP33(J)+PTP033(J)*AAA*CSCAT/1000.
          PTP34(J)=PTP34(J)+PTP034(J)*AAA*CSCAT/1000.
          PTP44(J)=PTP44(J)+PTP044(J)*AAA*CSCAT/1000.             
        ENDIF
      END DO
          AC=AC+ASZD*AD*RDELS
        IF(keys.eq.1) then        
          AE=AE+EXT*AAA
CD          write(*,*) AE,EXT,AAA,IK,' AE,EXT,AAA,IK'
          AAB=AAB+(EXT-SC)*AAA
        ELSE
          AE=AE+CEXTIN*AAA/1000.
          AAB=AAB+(CEXTIN-CSCAT)*AAA/1000.
        ENDIF
999   CONTINUE
      SSA1=(AE-AAB)/AE
C!!!      DO J=1,KM
c*** NORMALIZATION *** from 
C!!!          PTP11(J)=PTP11(J)/(AE-AAB)
C!!!          PTP12(J)=PTP12(J)/(AE-AAB)
C!!!          PTP22(J)=PTP22(J)/(AE-AAB)
C!!!          PTP33(J)=PTP33(J)/(AE-AAB)
C!!!          PTP34(J)=PTP34(J)/(AE-AAB)
C!!!          PTP44(J)=PTP44(J)/(AE-AAB)
C!!!      ENDDO
C   7 FORMAT (7E16.7) 
CD      WRITE(*,*) 'END of PHUNC'
       RIM=-RIM
       RETURN
       END

***********************************************************************
***       ELECTROMAGNETIC SCATTERING ON SPHERE -    Mie theory      ***
***   (see D.Deirmendjian, Electromagnetic scatering on spherical   ***
***   polydispersions,The RAND Corporation,Santa Monica,California, ***
***   American ELSEVIER Publishing Company,INC.,New York,1969.      ***
***********************************************************************
      SUBROUTINE MIE(X,CM,IME,S1,S2,CUGOL,SC,EXT,P,PP,PPP,T,TT,TTT,SI2)
      IMPLICIT INTEGER*4 (I-N)
      PARAMETER (PI=3.14159265359,N000=11000)
      REAL*8 DN,P1,P2,T2,CUGOL(0:IME),SI2(0:IME),X,SC,EXT,XFICT,
     *P(0:IME),PP(0:IME),PPP(0:IME),T(0:IME),TT(0:IME),TTT(0:IME)
     *,REWN,REWNO,REWNOO,SCAT,EXTIN,DNP,DDNN,RJN7,RJN77
      COMPLEX*16 CM,AN,BN,W,WO,WOO,AA,Y,DNY,FIR,S1(0:IME),S2(0:IME),CMO
      COMPLEX*16 ZER,JN(0:N000),JNN,JNN1,JNN2
* ---------------------------------------------------------------------*
* X = K*R = (2*PI*R)/(wave length)  |  CM = complex refractive index   *
* S1(0:IME),S2(0:IME) are functions (in IME+1 points) of the cosines of*
* the scattering angles CUGOL(0:IME) given by the Mie series (see Ref.)*
* SC,EXT are scattering and extintion factors (see Ref.), respectively.*
* P,PP,PPP,T,TT,TTT,SI2 are subsidiary arrays (in IME+1 points).       *
*                    ----------------------                            *
* N000 - dimension of the internal array JN(0:N000) depending of Xmax  *
*        N000 = Xmax * 1.1     (now Xmax=10000 and N000=11000)         *
* ---------------------------------------------------------------------*
CD      WRITE(*,*) 'inside of MIE'
      N100=X+4.*X**0.3+2 ! number of non-neglig. terms in the Mie-series
                 Y=CM*X  ! see Ref.
                       ZER=CMPLX(0.0D0,0.0D0)
                       FIR=CMPLX(1.0D0,0.0D0)
* -    -    -    -    -    -    -    -    -    -    -    -    -    -   *
* The calc. of Bessel funct. 'jn(y)' by Miller's recur.(reverse) proc. *
                                                 N101=N100+1
                                                  RJN7=0.D0
                                                 N200N=N100/100+3
                                                 N110=N100+N200N
                      DO N200=N110,32767,N200N
                                  JNN2=ZER
                                  JNN1=FIR*1D-100
                                   DO N=N200,N101,-1
                                   DNY=(N+N+3)/Y
                                   JNN=DNY*JNN1-JNN2
                                       RJN88=JNN*CONJG(JNN)
                                     IF(RJN88.GT.1D100)THEN
                                       JNN=JNN/RJN88
                                       JNN1=JNN1/RJN88
                                     END IF
                                   JNN2=JNN1
                                   JNN1=JNN
                                   END DO
                              RJN77=JNN1*CONJG(JNN1)
                            DN=JNN2*CONJG(JNN2)
                            RJN77=RJN77/DN
                   IF(ABS((RJN7-RJN77)/RJN77).LT.0.000001D0)GOTO 100
                                  RJN7=RJN77
                                   END DO
 100   CONTINUE
                                   JN(N+2)=JNN2
                                   JN(N+1)=JNN1
                                 DO N=N100,0,-1
                                   DNY=(N+N+3)/Y
                                   JN(N)=DNY*JN(N+1)-JN(N+2)
                JN(N+2)=JN(N)/JN(N+1)
                  RJN77=JN(N)*CONJG(JN(N))
                  IF(RJN77.GT.1D100)THEN
                  JN(N+1)=JN(N+1)/RJN77
                  JN(N)=JN(N)/RJN77
                              END IF
                                   END DO
* -    -    -    -    -    -    -    -    -    -    -    -    -    -   *
*              Deirmedjian's code (see Ref.)                           *
                       SCAT=0.D0
                       EXTIN=0.D0
          DO I=0,IME
          S1(I)=ZER
          S2(I)=ZER
          SI2(I)=1.D0-CUGOL(I)**2
          PPP(I)=0.D0
          PP(I)=1.D0
          TTT(I)=0.D0
          TT(I)=CUGOL(I)
          P(I)=1.D0
          T(I)=TT(I)
          END DO
                    REWNOO=COS(X)
                    REWNO=SIN(X)
                       WOO=CMPLX(REWNOO,-REWNO)
                       WO=CMPLX(REWNO,REWNOO)
*
               DO NNN=1,N100
               DN=NNN
               T2=DN+DN-1.D0
                   IF(NNN.GT.1)THEN
               P1=T2/(DN-1.D0)
               P2=DN/(DN-1.D0)
                   END IF
                            DNY=NNN/Y
                            AA=JN(NNN+1)-DNY
                               W=T2/X*WO-WOO
                               REWN=T2/X*REWNO-REWNOO
                               WOO=WO
                               WO=W
                               REWNOO=REWNO
                               REWNO=REWN
                                AN=DN/X
                                BN=AN+CM*AA
                                AN=AN+AA/CM
                                AN=(AN*REWN-REWNOO)/(AN*W-WOO)
                                BN=(BN*REWN-REWNOO)/(BN*W-WOO)
       DNP=DN+DN+1.D0
       SCAT=SCAT+DNP*(AN*CONJG(AN)+BN*CONJG(BN))
       EXTIN=EXTIN+DNP*REAL(AN+BN)
                  DDNN=DNP/(DN*(DN+1.D0))
             DO I=0,IME
             IF(NNN.GT.1)THEN
             P(I)=CUGOL(I)*P1*PP(I)-P2*PPP(I)
             T(I)=CUGOL(I)*(P(I)-PPP(I))-T2*SI2(I)*PP(I)+TTT(I)
             PPP(I)=PP(I)
             TTT(I)=TT(I)
             PP(I)=P(I)
             TT(I)=T(I)
                         END IF
                 S1(I)=S1(I)+DDNN*(AN*P(I)+BN*T(I))
                 S2(I)=S2(I)+DDNN*(BN*P(I)+AN*T(I))
             END DO
               END DO
                  SC=SCAT*2./X**2
                  EXT=EXTIN*2./X**2
* -    -    -    -    -    -    -    -    -    -    -    -    -    -   *
                                      RETURN
************************************************************************
*                 for CROSS-SECTIONS only (the same code)              *
      ENTRY BLL(XFICT,CM,SC,EXT)
CD      WRITE(*,*) XFICT,CM,SC,EXT,'XFICT,CM,SC,EXT, in BLL'
* --- Attention : here is the change (X-limitation for the cross sect.)*
        XLIMIT=3000.567887D0 ! X - limitation
        IF(XFICT.GT.XLIMIT) THEN
          IF(X.EQ.XLIMIT.AND.CM.EQ.CMO) RETURN
            CMO=CM
          X=XLIMIT
                           ELSE
          X=XFICT
         END IF
* ----------------------------------------------------------------------*
                       Y=CM*X
                       FIR=CMPLX(1.0D0,0.D0)
            N100=X+4.*X**0.3+2
                                                 N101=N100+1
                                                  RJN7=0.D0
                                                 N200N=N100/100+3
                                                 N110=N100+N200N
                      DO N200=N110,32767,N200N
                                  JNN2=CMPLX(0D0,0D0)
                                  JNN1=FIR *1D-100
                                   DO N=N200,N101,-1
                                   DNY=(N+N+3)/Y
                                   JNN=DNY*JNN1-JNN2
                                       RJN88=JNN*CONJG(JNN)
                                     IF(RJN88.GT.1D100)THEN
                                       JNN=JNN/RJN88
                                       JNN1=JNN1/RJN88
                                     END IF
                                   JNN2=JNN1
                                   JNN1=JNN
                                   END DO
*                                                     goto 300
                              RJN77=JNN1*CONJG(JNN1)
                            DN=JNN2*CONJG(JNN2)
                            RJN77=RJN77/DN
              IF(ABS((RJN7-RJN77)/rjn77).LT.0.000001D0)GOTO 300
                                  RJN7=RJN77
                                   END DO
 300   CONTINUE
                                   JN(N+2)=JNN2
                                   JN(N+1)=JNN1
                                 DO N=N100,0,-1
                                   DNY=(N+N+3)/Y
                                   JN(N)=DNY*JN(N+1)-JN(N+2)
                JN(N+2)=JN(N)/JN(N+1)
                  RJN77=JN(N)*CONJG(JN(N))
                  IF(RJN77.GT.1D100)THEN
                  JN(N+1)=JN(N+1)/RJN77
                  JN(N)=JN(N)/RJN77
                              END IF
                                   END DO
                       SCAT=0.D0
                       EXTIN=0.D0
                    REWNOO=COS(X)
                    REWNO=SIN(X)
                       WOO=CMPLX(REWNOO,-REWNO)
                       WO=CMPLX(REWNO,REWNOO)
* -------
               DO NNN=1,N100
               DN=NNN
               T2=DN+DN-1.D0
                   IF(NNN.GT.1)THEN
               P1=T2/(DN-1.D0)
               P2=DN/(DN-1.D0)
                   END IF
                            DNY=NNN/Y
                            AA=JN(NNN+1)-DNY
                               W=T2/X*WO-WOO
                               REWN=T2/X*REWNO-REWNOO
                               WOO=WO
                               WO=W
                               REWNOO=REWNO
                               REWNO=REWN
                                AN=DN/X
                                BN=AN+CM*AA
                                AN=AN+AA/CM
                                AN=(AN*REWN-REWNOO)/(AN*W-WOO)
                                BN=(BN*REWN-REWNOO)/(BN*W-WOO)
       DNP=DN+DN+1.D0
       SCAT=SCAT+DNP*(AN*CONJG(AN)+BN*CONJG(BN))
       EXTIN=EXTIN+DNP*REAL(AN+BN)
                  DDNN=DNP/(DN*(DN+1.D0))
               END DO
                  SC=SCAT*2./X**2
                  EXT=EXTIN*2./X**2

      RETURN
      END
       subroutine gas(NLN,iatmos1,suhei,hlyr,MAXL,MAXCLY,ZET,
     * TEM,PRES,AIR,AIRM,H2O,O3)
c
c..prepare the gas-pathlengths for (standard) atmospheric layers 
c     (single precision) 
c
      parameter (lvl=101,lyr=lvl-1,mpa=10)
      dimension ZET(MAXL),TEM(MAXL),PRES(MAXL),AIR(0:MAXL)
      dimension AIRM(0:MAXL),H2O(MAXCLY),O3(MAXCLY)
      dimension hlyr(MAXCLY+1)
c
      common /level/ zkl(lvl),tkl(lvl),pbl(lvl),
     *               dal(lvl),cgl(28,lvl)
      common /layer/ zky(lyr),tky(lyr),pby(lyr),
     *               day(lyr),cgy(28,lyr)
      common /delta/ zkd(lyr),tkd(lyr),pbd(lyr),
     *               dag(lyr),cgg(28,lyr)
      common /accus/ ssmall,sexpon,ssigni
      common /gases/ avoga,amair,gmo(28),igases	
      common /micro/ om(lyr),gf(lyr),ot(lyr)
      common /gdata/ sualb,pival
      common /sdata/ solar,angle
      common /ldata/ bo(lvl),bot,bob
      common /wflux/ fldn(lvl)
      common /daily/ saw(mpa),hco(lyr)   
      common /atmos/ iatmos	
      common /print/ iprint
        iatmos=iatmos1
        la=NLN
        igases=22
        iprint=1
        ppm=350.00
        solar=1360.00
        cza=0.00
        tkt=0.00
        sualb=0.15
        sutdf=0.00
        do i=1,la
        zkl(i)=hlyr(i)
        enddo
c
c..determination of the effective number of levels ....................
      do 10 i=1,la
      levels = i
c      read (8,*) zkl(i)
      if(zkl(i).lt.suhei) goto 20
   10 continue
      levels = levels +1
   20 zkl(levels) = suhei
c
      le     = levels
      la     = le -1
      lm     = le -2
c
      mp     = 1
      ssigni = 1.0e-06
      ssmall = 1.0e-35
      sexpon = -dlog(dble(ssmall))
      pival  = 4.0 *atan(1.0)
c
c----------------------------------------------------------------------
c..prepare atmospheric data
      call atmosp (le,mp,ppm)
c----------------------------------------------------------------------
      tkb    = tkl(le) +sutdf
c
c     close (6)
cs      close (7)
c
      DO I1=1,NLN+1
      ZET(I1)=zkl(I1)
      TEM(I1)=tkl(I1)
      PRES(I1)=pbl(I1)
      AIR(I1)=dal(I1) 
cs      IF(K.NE.I) WRITE(*,*) 'K.NE.I in atmos profiles*!!!*'
      ENDDO 
      DO I1=1,NLN
      AIRM(I1)=day(I1)
      H2O(I1)=cgy(1,I1)
      O3(I1)=cgy(3,I1)
cs      IF(K.NE.I) WRITE(*,*) 'K.NE.I in atmos profiles*!!!*'
      ENDDO
      return
      end
c--------------------------------------------------------
      subroutine atmosp (le,mp,ppm)
c
c..prepare atmospheric data
c
      parameter (lvl=101,lyr=lvl-1,mpa=10)
c
      common /level/ zkl(lvl),tkl(lvl),pbl(lvl),
     *               dal(lvl),cgl(28,lvl)
      common /layer/ zky(lyr),tky(lyr),pby(lyr),
     *               day(lyr),cgy(28,lyr)
      common /delta/ zkd(lyr),tkd(lyr),pbd(lyr),
     *               dag(lyr),cgg(28,lyr)
      common /accus/ ssmall,sexpon,ssigni
      common /daily/ saw(mpa),hco(lyr)   
      common /gases/ avoga,amair,gmo(28),igases	
      common /atmos/ iatmos	
      common /print/ iprint 
c
      dimension  iatype(4),atmos1(4,200),atmos2(4,200),ir(200)
      dimension  igtype(28),gases1(28,200),gases2(28,200)
      dimension  ntype(20),cg1(28),cg2(28)
c
      la = le -1
c
c..initialization for atmospheric gas-data
      do 100 j=1,28
      gmo(j)  = 0.0
      do 100 i=1,200
      gases1(j,i) = 0.0
      gases2(j,i) = 0.0
  100 continue
      if(igases.le.0) goto 160
c
c..molecular weights of air,h2o,co2,o3,n2o,co,ch4,o2
      avoga  = 6.023e+23
      amair  = 28.97
      gmo(1) = 18.0
      gmo(2) = 44.0
      gmo(3) = 48.0
      gmo(4) = 44.0
      gmo(5) = 28.0
      gmo(6) = 16.0
      gmo(7) = 32.0
c
c..ppmv constant for co2 and o2
      do 110 i=1,74
c     gases1(2,j) = 3.50e+02
      gases1(2,j) = ppm
      gases1(7,j) = 2.09e+05
  110 continue
      if(igases.le. 7) igases =  7
      if(igases.le.28) igases = 28
      if(igases.eq. 7) goto 160
c
c..atmospheric gases (28): h2o,co2,o3,n2o,co,ch4,o2,.. [ppmv] 
cs      open (10,file='/home/ilya/CODEILYA_FINAL3/modata.d',status='old')
      open (10,file='modata.d',status='old')
      do 150 m=1,4
      jstart = 7*(m-1) +1
      jend   = 7*m
      read (10,120) (gmo(j),j=jstart,jend)
  120 format (//,6x,7f10.1,//)
      do 140 l=1,74
      i      = 75 -l
      read (10,130) (gases1(j,i),j=jstart,jend)
  130 format (7x,7e10.2)
  140 continue
  150 continue
      close (10)
  160 continue
c
c..initialization for atmospheric data
      itest  = iatmos
      if(itest.eq.0) itest = 6
      if(itest.lt.0) itest = -itest
      if(itest.gt.8) itest = 6
c
      do 170 j=1,4
      do 170 i=1,200
      atmos1(j,i) = 0.0
      atmos2(j,i) = 0.0
  170 continue
c
c..atmospheric data: z[km],p[mb],t[k],ro[#/cm3] and h2o,o3,n2o,co,ch4
cs      open (10,file='/home/ilya/CODEILYA_FINAL3/sadata.d',status='old')
      open (10,file='sadata.d',status='old')
      do 260 m=1,8
      read (10,180) imodel,sun,slt
  180 format (/,9x,i4,39x,f5.1,18x,f5.1)
      if(itest.ne.imodel) goto 240
      read (10,190)
  190 format (///)
      ppo    = gases1(2,74)
      do 230 l=1,74
      i      = 75 -l
      if(igases.eq.0) goto 210
      if(ppm.ne.ppo) gases1(2,74) = gases1(2,74)*(ppm/ppo)
      read (10,200) (atmos1(j,i),j=1,4),
     *               gases1(1,i),(gases1(j,i),j=3,6)
  200 format (f7.2,e10.3,f7.1,e11.3,5e9.2)
      goto 230
  210 read (10,220) (atmos1(j,i),j=1,4)
  220 format (f7.2,e10.3,f7.1,e11.3)
  230 continue
      goto 270
  240 read (10,250) 
  250 format (77(/))
  260 continue
  270 close (10)
c
      if(iatmos.gt.0) goto 710
c
c..non-standard atmospheric data [profil.d]
cs      open (10,file='/home/ilya/CODEILYA_FINAL3/profil.d',status='old')
      open (10,file='profil.d',status='old')
      read (10,*) nrow,ncol,sun,slt
      if(nrow.gt.0) goto 280
      close (10)
      goto 710
c
c..override new atmospheric data 
  280 read (10,*) nref
      kp     = 0
      read (10,*) vdata
      if (nref.eq.-1) goto 310
c
c..calculate pressure equivalent altitude
  290 kp     = kp +1
      pdelt  = vdata -atmos1(2,kp)
      if(pdelt.gt. ssigni .and. kp.lt.74) goto 290
      if(pdelt.lt.-ssigni) goto 300
      vdata  = atmos1(1,kp)
      goto 310
  300 pgrad  = (dlog(dble(atmos1(2,kp))) -dlog(dble(vdata))) /
     *         (dlog(dble(atmos1(2,kp))) -dlog(dble(atmos1(2,kp-1))))
      vdata  = atmos1(1,kp) -(atmos1(1,kp)-atmos1(1,kp-1)) *pgrad 
      kp     = kp -1
c
  310 altitude = vdata
      iatm   = 1
      iread  = 1
      next   = 1
      zdelt  = altitude -zkl(1)
      if(zdelt.gt.-ssigni) goto 340
c
  320 atmos2(1,next) = atmos1(1,iatm)
      iatm   = iatm +1
      zdelt  = atmos1(1,iatm) -zkl(1)
      if(zdelt.gt.-ssigni) goto 320
  330 next   = next +1
      zdelt  = altitude -atmos1(1,iatm)
      if(zdelt.gt.-ssigni) goto 340
      atmos2(1,next) = atmos1(1,iatm)
      iatm   = iatm +1
      goto 330
c
  340 ir(iread) = next
      atmos2(1,next) = altitude
      next   = next +1  
      read (10,*) vdata   
      if (nref.eq.-1) goto 370
c
c..calculate pressure equivalent altitude
  350 kp     = kp +1
      pdelt  = vdata -atmos1(2,kp)
      if(pdelt.gt. ssigni .and. kp.lt.74) goto 350
      if(pdelt.lt.-ssigni) goto 360
      vdata  = atmos1(1,kp)
      goto 370
  360 pgrad  = (dlog(dble(atmos1(2,kp))) -dlog(dble(vdata))) /
     *         (dlog(dble(atmos1(2,kp))) -dlog(dble(atmos1(2,kp-1))))
      vdata  = atmos1(1,kp) -(atmos1(1,kp)-atmos1(1,kp-1)) *pgrad 
      kp     = kp -1
c
  370 altitude = vdata
      iread  = iread +1
      if(iread.lt.nrow) goto 340
      zdelt  = zkl(le) -altitude
      if(zdelt.gt.-ssigni) goto 390
      ir(iread) = next
      atmos2(1,next) = altitude
c
  380 iatm   = iatm +1
      zdelt  = atmos1(1,iatm) -altitude
      if(zdelt.gt.-ssigni) goto 380
      next   = next +1
      atmos2(1,next) = atmos1(1,iatm)
      zdelt  = atmos2(1,next) - zkl(le)
      if(zdelt.gt. ssigni) goto 380 
  390 continue
      close (10)
c
c..interpolate other data to new altitudes
      k      = 0
      do 450 l=1,next
  400 k      = k +1
      zdelt  = atmos1(1,k)-atmos2(1,l)
      if(zdelt.gt. ssigni) goto 400
      if(zdelt.lt.-ssigni) goto 430
      atmos2(2,l) = atmos1(2,k)
      atmos2(3,l) = atmos1(3,k)
      atmos2(4,l) = atmos1(4,k)
      if(igases.eq.0) goto 430
      do 410 m=1,igases  
      gases2(m,l) = gases1(m,k)
  410 continue
  420 goto 450
  430 k      = k -1
      zgrad  = atmos1(1,k) -atmos1(1,k+1)
      atmos2(2,l) = exp(dlog(dble(atmos1(2,k+1)))-
     *  (dlog(dble(atmos1(2,k)))-dlog(dble(atmos1(2,k+1))))*zdelt/zgrad)
      atmos2(3,l) = atmos1(3,k+1) -(atmos1(3,k)-atmos1(3,k+1)) 
     *                                              *zdelt/zgrad
      atmos2(4,l) = exp(dlog(dble(atmos1(4,k+1)))-
     *  (dlog(dble(atmos1(4,k)))-dlog(dble(atmos1(4,k+1))))*zdelt/zgrad)
      if(igases.eq.0) goto 450
      do 440 m=1,igases
      gases2(m,l) = exp(dlog(dble(gases1(m,k+1)))-
     *  (dlog(dble(gases1(m,k)))-dlog(dble(gases1(m,k+1))))*zdelt/zgrad)
  440 continue
  450 continue
c
      if(ncol.eq.0) goto 710
c
c..override new atmospheric data 
cs      open (10,file='/home/ilya/CODEILYA_FINAL3/profil.d',status='old')
      open (10,file='profil.d',status='old')
      read (10,*) nrow,ncol,sun,slt
      read (10,*) (ntype(j),j=1,ncol)
      ja     = 0
      jg     = 0
      do 470 j=1,ncol
      if(ntype(j).ge. 0) goto 460
      ja     = ja +1
      iatype(ja) = -ntype(j)
      goto 470
  460 jg     = jg +1
      igtype(jg) =  ntype(j)
  470 continue
c
      do 500 i=1,nrow
      if(ja.eq.0 .or. jg.eq.0) goto 480
      read (10,*) (atmos2(iatype(j),ir(i)),j=1,ja),
     *                   (gases2(igtype(j),ir(i)),j=1,jg)
      goto 500
  480 if(ja.eq.0) goto 490
      read (10,*) (gases2(igtype(j),ir(i)),j=1,jg)
      goto 500
  490 read (10,*) (atmos2(iatype(j),ir(i)),j=1,ja)
  500 continue
      close (10)
c
c..check for correct atmospheric units
      if(ja.eq.0) goto 570
      do 560 j=1,ja
      valo   = atmos2(iatype(j),ir(nrow))
      if(iatype(j).ne.2) goto 520
      if(valo.lt.1.5e+3) goto 520
c..pressure (pascal into milli-bar)
      do 510 i=1,nrow
      atmos2(2,ir(i)) = atmos2(2,ir(i)) /1.0e+2
  510 continue
  520 if(iatype(j).ne.3) goto 540
      if(valo.gt.1.0e+2) goto 540
c..temperature (celsius into kelvin)
      do 530 i=1,nrow
      atmos2(3,ir(i)) = atmos2(3,ir(i)) +273.2
  530 continue
  540 if(iatype(j).ne.4) goto 560
      if(valo.lt.2.0e+3) goto 560
c..air-density (g/m3 into #/cm3)
      do 550 i=1,nrow
      atmos2(4,ir(i)) = (atmos2(4,ir(i)) *avoga) /(1.0e+6 *amair) 
  550 continue
  560 continue
c
c..check for correct gas units
  570 if(jg.eq.0) goto 680
      do 670 j=1,jg
      valo   = gases2(igtype(j),ir(nrow))
      if(igtype(j).ne.1) goto 650
      if(valo.gt.3.8e+2) goto 650
      vahi   = gases2(igtype(j),ir(   1))
      vaqu   = vahi/valo
      if(vaqu.lt.1.0e-3) goto 630
      if(valo.gt.1.0e+2) goto 610
      if(vahi.gt.1.0e+0) goto 590
      if(vahi.lt.0.0   ) goto 590
      if(valo.gt.1.0e+0) goto 590
      if(valo.lt.0.0   ) goto 590
c..water vapor (rel.humidity into g/m3)
      do 580 i=1,nrow
      con = 19.83
      if(atmos2(3,ir(i)).le.273.2) con = 22.52
      gases2(1,ir(i)) = gases2(1,ir(i)) *6107.0 
     *                /(atmos2(3,ir(i)) *4.61)
     *                *exp(con*(1.0-273.2/atmos2(3,ir(i))))
  580 continue
      goto 630
  590 continue
c..dew-point temperature (celsius into kelvin)
      do 600 i=1,nrow
      gases2(1,ir(i)) = gases2(1,ir(i)) +273.2
  600 continue
  610 continue
c..water vapor (kelvin into g/m3)
      do 620 i=1,nrow
      con = 19.83
      if(atmos2(3,ir(i)).le.273.2) con = 22.52
      gases2(1,ir(i)) = 6107.0 /(gases2(1,ir(i)) *4.61) 
     *                  *exp(con*(1.0-273.2/gases2(1,ir(i))))
  620 continue
  630 continue
c..water vapor (g/m3 into ppmv)
      do 640 i=1,nrow
      gases2(1,ir(i)) = (gases2(1,ir(i)) *avoga) 
     *                / (atmos2(4,ir(i)) *gmo(1))
  640 continue
  650 if(igtype(j).ne.3) goto 670
      if(valo.gt.1.0e-2) goto 670
c..ozone (g/m3 into ppmv)
      do 660 i=1,nrow
      gases2(3,ir(i)) = (gases2(3,ir(i)) *avoga) 
     *                / (atmos2(4,ir(i)) *gmo(3))
  660 continue
  670 continue
  680 continue
c
c..resubstitude fields
      do 690 j=1,4
      do 690 i=1,next
      atmos1(j,i) = atmos2(j,i)
  690 continue
      do 700 j=1,igases
      do 700 i=1,next
      gases1(j,i) = gases2(j,i)
  700 continue
  710 continue
c   
c..determine the level-values   
      k      = 0
      do 770 l=1,le
  720 k      = k +1
      zdelt  = atmos1(1,k)-zkl(l)
      if(zdelt.gt.ssigni) goto 720
      if(zdelt.lt.-ssigni) goto 750
      pbl(l) = atmos1(2,k)
      tkl(l) = atmos1(3,k)
      dal(l) = atmos1(4,k)
      if(igases.eq.0) goto 740
      do 730 m =1,igases
      cgl(m,l) = gases1(m,k)
  730 continue
  740 continue
      goto 770   
  750 k      = k -1
      zgrad  = atmos1(1,k) -atmos1(1,k+1)
      pbl(l) = exp(dlog(dble(atmos1(2,k+1)))-
     * (dlog(dble(atmos1(2,k)))-dlog(dble(atmos1(2,k+1))))*zdelt/zgrad)
      tkl(l) = atmos1(3,k+1) -(atmos1(3,k)-atmos1(3,k+1))*zdelt/zgrad
      dal(l) = exp(dlog(dble(atmos1(4,k+1)))-
     * (dlog(dble(atmos1(4,k)))-dlog(dble(atmos1(4,k+1))))*zdelt/zgrad)
      if(igases.eq.0) goto 770
      do 760 m =1,igases
      cgl(m,l) = exp(dlog(dble(gases1(m,k+1)))-
     * (dlog(dble(gases1(m,k)))-dlog(dble(gases1(m,k+1))))*zdelt/zgrad)
  760 continue
  770 continue
c   
c..determine the layer-values   
      k      = 0
      pb1    = pbl(1)
      tk1    = tkl(1)
      da1    = dal(1)
      if(igases.eq.0) goto 790
      do 780 m=1,igases
      cg1(m) = cgl(m,1)
  780 continue
  790 continue
      do 970 l=1,la
      zkd(l) = zkl(l) -zkl(l+1)
      tkd(l) = tkl(l) -tkl(l+1)
      pbd(l) = pbl(l+1) -pbl(l)
      zky(l) = (zkl(l)+zkl(l+1)) /2.0
      tky(l) = 0.0
      pby(l) = 0.0
      day(l) = 0.0
      if(igases.eq.0) goto 810
      do 800 m=1,igases
      cgy(m,l) = 0.0
  800 continue
  810 continue
  820 k      = k +1
      zd1    = zkl(l) -atmos1(1,k)
      if(zd1.lt.-ssigni) goto 820
  830 zd2    = zkl(l+1) -atmos1(1,k)
      if(zd2.ge.-ssigni) goto 900
      pb2    = atmos1(2,k)
      tk2    = atmos1(3,k)
      da2    = atmos1(4,k)
      if(igases.eq.0) goto 850
      do 840 m=1,igases
      cg2(m) = gases1(m,k)
  840 continue
  850 continue
	IF(PB1.LT.1.0E-30)PB1=1.0E-30
	IF(PB2.LT.1.0E-30)PB2=1.0E-30 !CXXA

      pby(l) = 
     +pby(l)+exp((dlog(dble(pb1))+dlog(dble(pb2)))/2.0) *(zd1/zkd(l))
      tky(l) = tky(l)+(tk1+tk2)/2.0 *(zd1/zkd(l))

      day(l) = 
     +day(l)+exp((dlog(dble(da1))+dlog(dble(da2)))/2.0) *(zd1/zkd(l))
      if(igases.eq.0) goto 870
      do 860 m=1,igases
	IF(CG1(M).LT.1.0E-30)CG1(M)=1.0E-30 !CXXA
	IF(CG2(M).LT.1.0E-30)CG2(M)=1.0E-30 !CXXA
      cgy(m,l) = cgy(m,l)
     *+exp((dlog(dble(cg1(m)))+dlog(dble(cg2(m))))/2.0) *(zd1/zkd(l))
  860 continue
  870 continue
      pb1    = pb2
      tk1    = tk2
      da1    = da2
      if(igases.eq.0) goto 890
      do 880 m=1,igases
      cg1(m) = cg2(m)
  880 continue
  890 continue
      k      = k +1
      zd1    = atmos1(1,k-1) -atmos1(1,k)
      goto 830   
  900 zd1    = zd1 -zd2
      pb2    = pbl(l+1)
      tk2    = tkl(l+1)
      da2    = dal(l+1)
      if(igases.eq.0) goto 920
      do 910 m=1,igases
      cg2(m) = cgl(m,l+1)
  910 continue
  920 continue
	IF(PB1.LT.1.0E-30)PB1=1.0E-30
	IF(PB2.LT.1.0E-30)PB2=1.0E-30 !CXXA
      pby(l) = 
     +pby(l)+exp((dlog(dble(pb1))+dlog(dble(pb2)))/2.0) *(zd1/zkd(l))
      tky(l) = tky(l)+(tk1+tk2)/2.0 *(zd1/zkd(l))
      day(l) = 
     +day(l)+exp((dlog(dble(da1))+dlog(dble(da2)))/2.0) *(zd1/zkd(l))
      if(igases.eq.0) goto 940
      do 930 m=1,igases
      cgy(m,l) = cgy(m,l)
	IF(CG1(M).LT.1.0E-30)CG1(M)=1.0E-30 !CXXA
	IF(CG2(M).LT.1.0E-30)CG2(M)=1.0E-30 !CXXA
     *+exp((dlog(dble(cg1(m)))+dlog(dble(cg2(m))))/2.0e+0) *(zd1/zkd(l))
  930 continue
  940 continue
      pb1    = pb2
      tk1    = tk2
      da1    = da2
      if(igases.eq.0) goto 960
      do 950 m=1,igases
      cg1(m) = cg2(m)
  950 continue
  960 continue
      k      = k -1
  970 continue  
c
c..layer densities in #/m2 (-y) and g/m2 (-g)
      do 990 l=1,la
      day(l) = day(l) * 1.0e+6 * (1.0e+3 *zkd(l)) 
      dag(l) = day(l) * (amair /avoga)  
      hco(l) = -(60.0*60.0*24.0) /(1.005*dag(l))
      if(igases.eq.0) goto 990
      do 980 m=1,igases  
      cgy(m,l) = day(l) * (1.0e-6 * cgy(m,l))
      cgg(m,l) = cgy(m,l) * (gmo(m)/avoga)
  980 continue
  990 continue
c
c..determine daily fractions of solar zenith angle
      if(mp.gt.1) call sunday (sun,slt,mp)
c
c..print-out of atmospheric layer and level data   
cs      call atmopr (sun,slt,le)
c
      return
      end
c--------------------------------------------------------
      subroutine sunday (sun,slt,mp)
c   
c..weights for solar zenith angles during the day
c   
      parameter (lvl=41,lyr=lvl-1,mpa=10)
c   
      common /daily/ saw(mpa),hco(lyr)   
c   
      dimension san(mpa)
c   
      co     = atan(1.0)/45.0
      s3     = 0.0
      s12    = abs(slt-sun)
      if(s12.ge.90.0) goto 100
      s1     = co*slt
      s2     = co*sun
      s3     = cos(co*s12)
c
  100 saw(1) = 0.0
      san(1) = 0.0
      if(s3.le.0.0) goto 120
c 
      ic     = 1
      do 110 i=2,mp 
      saw(i) = 0.0
      san(i) = float(i-1)/float(mp)
      if(san(i).ge.s3) goto 110 
      ic     = ic +1
  110 continue  
c 
  120 if(ic.eq.0) goto 170   
      dtb    = 0.0
      alf    = (san(1)-sin(s1)*sin(s2))/(cos(s1)*cos(s2))
      if(abs(alf).gt.1.0) goto 130
      dtb    = (180.0-acos(alf)/co) /180.0
  130 if(ic.eq.1) goto 160   
c 
      do 150 i=2,ic  
      alf    = (san(i)-sin(s1)*sin(s2))/(cos(s1)*cos(s2))
      if(abs(alf).gt.1.0) goto 140
      dte    = (180.0-acos(alf)/co) /180.0
  140 continue  
      mc     = mp +2 -i
      saw(mc)= dte -dtb
      dtb    = dte
  150 continue  
c 
  160 dte    = 1
      saw(mp+1-ic)= dte -dtb
  170 continue 
c 
      return
      end   
C**********************************************************
      subroutine rayleia(wavel,h,LAT,TAURA)
      double precision twofi,zc,costwofi,costwofi2,F,wr,wr2,
     &n360,n2,n21,n22
      double precision wavel,h,LAT,TAURA
CXXA	REAL WAVEL,H,LAT,TAURA
cs      write(6,*)'wavel,h,LAT',wavel,h,LAT
      twofi = LAT*0.03490658504
      costwofi = cos(twofi)
      costwofi2 = costwofi * costwofi
      wr=1./(wavel*wavel)
      wr2=wr*wr
      zc=0.73737*h+5517.56
      F=0.78084*(1.034+3.17e-4*wr)+0.20946*
     &(1.096+1.385e-3*wr+1.448e-4*wr2)+0.009754
      n360=8.06077e-5+2.481070e-2/(132.274-wr)+1.74563e-4/
     &(39.32957-wr)+1
      n2=n360*n360
      n21=(n2-1)
      n22=1./(n2+2)
      TAURA=241675320.0752709888768463125378*n21*n21*F*n22*n22*wr2 * 
     &exp(-1.188e-4*h-1.16e-9*h*h)/ 
     &(980.6160*( 1.-0.002637*costwofi+0.0000059*costwofi2)-
     &(3.085462e-4+2.27e-7*costwofi)*zc+ 
     &(7.254e-11+1.0e-13*costwofi)*zc*zc
     &-(1.517e-17+6.0e-20*costwofi)*zc*zc*zc)
	WRITE(1011,*)'TAURA=',TAURA
      return
      end
      subroutine rayleiaold(wavel,h,TAURA)
      w2=1./(wavel**2)
      xh=h*1e+3
      r2=(1.e-8)*(8342.13+2406030./(130.-w2)+15997./(38.9-w2))
      r3=r2*w2*(r2+2.)
      R=r3**2*28773.597886
      c=1./exp(xh/(29.3*273.))
      TAURA=R*c
      return
      end
C**********************************************************
      	real function raylei(lambda,zbot,ztop)
        real lambda,zbot,ztop
        raylei=(0.0088*(lambda**(-4.15 + 0.2*lambda)) *         
     $          exp(-0.1188*zbot - 0.0016*(zbot**2)) ) -     
     $          (0.0088*(lambda**(-4.15 + 0.2*lambda)) *         
     $          exp(-0.1188*ztop - 0.0016*(ztop**2)) )
	 end
c
c**********************************************************
      subroutine rayleig (wavel,ex,bk)
c
c..rayleigh (back-)scattering
c
C      WRITE(*,*) wavel,'wavel'
      pi = 4.0*atan(1.0)
      co = 2.68684e+25
      al = wavel/1.0e+6
c
c..refractive index of air
      rf = 2876.0305 +16.2876/wavel**2 +0.13638/wavel**4
      rf = (1.0+rf/1.0e+7)**2 -1.0
c      write (6,*) rf
C      write (*,*) rf,' rf'
c
c..extinction and backscattering (1/m)
      ex = co*(8.0 *pi**3 *rf**2 /(3.0 *co**2 *al**4)) *1.061
      bk = ex*(3.0/(8.0*pi))
c
      return
      end
cl*******************************************************
      	subroutine  errmsg( messag, fatal )

c        print out a warning or error message;  abort if error

	logical       fatal, once
	character*(*) messag
	integer       maxmsg, nummsg
	save          maxmsg, nummsg, once
	data          nummsg / 0 /,  maxmsg / 100 /,  once / .false. /


	if ( fatal )  then
	   write ( *, '(/,2a)' )  ' ******* error >>>>>>  ', messag
	   stop
	end if

	nummsg = nummsg + 1
	if ( nummsg.gt.maxmsg )  then
	   if ( .not.once )  write ( *,100 )
	   once = .true.
	else
	   write ( *, '(/,2a)' )  ' ******* warning >>>>>>  ', messag
	endif

	return

100   format( ///,' >>>>>>  too many warning messages --  ',
     $   'they will no longer be printed  <<<<<<<', /// )
	end
C*************************************************************
       subroutine single(nlyr,dtauc,ssalb,taurl,xtau,xssa,
     $            fbeam,INDG,GALB,SCR,SCI,umu0,is,numu,umu,nphi,phi,
     $                  nangs,xangs,xphase,ltop,lbot,lamber,
     $                  albedo,gg1,gg2,fr,ntaus,llyr,utau,uu,
     $                  CBRF3,XLAND,XOCEAN)
CD       parameter (maxlyr=40,maxumu=60,maxphi=30)
         parameter (maxlyr=35,maxumu=100,maxphi=200)
         PARAMETER (KNA1U =100)
         PARAMETER (KNA0  =2)
         PARAMETER (KNFI  =200)
         PARAMETER(PI=3.141592654,RAD=PI/180.)
         REAL LINEAR
       logical lamber
       integer nlyr,numu,nphi,i,j,k,l,ltop,lbot,ntaus
       real fbeam,umu0,dtauc(*),ssalb(*),albedo,gg1,gg2,fr
	 DOUBLE PRECISION XTAU(*)
       real xssa(*),taurl(*),umu(*),phi(*)
       real phs(maxumu*maxphi,maxlyr),phsrl(maxumu*maxphi),
     $      cs(maxumu*maxphi),xphase(500,*),xangs(500,*),
     $      xmu, xxmu(500,maxlyr),xxxmu(500),xphases(500)
       real phs1(maxlyr), mysins
       integer nangs(*),llyr(*)
       real phases(500,maxlyr)
       real tau(0:maxlyr),utau(*),uu(maxumu,maxlyr,maxphi)
       DIMENSION CBRF3(KNA0,KNA1U,KNFI)
c-----equivalent phase function (mie+rayleigh)---------
cs        write(6,*)'umu0',umu0
cs        write(6,*)'umu',(umu(i),i=1,numu)
cs        write(6,*)'phi',(phi(i)*RAD,i=1,nphi)
	do l=ltop,lbot
	 do i=1,nangs(l)
	  xmu=cosd( xangs(i,l) )
	  xxmu(i,l)=xmu
	  phases(i,l)=
     $    (taurl(l)*rlfunc(xmu)+xssa(l)*xtau(l)*xphase(i,l))
     $    /(taurl(l)+xssa(l)*xtau(l))
cs         write(6,*)taurl(l),xphase(i,l)
	 enddo 
	enddo
c------estimate the phase functions at each viewing angle------
       do j=1,nphi
	do i=1,numu
         k=i+(j-1)*numu
	 cs(k)=-umu0*umu(i) + cosd( phi(j) )
     $    *sqrt( (1.-umu0*umu0)*(1.-umu(i)*umu(i)) )
        enddo
       enddo
       nang=numu*nphi 
c--------rayleigh phase function at the scattering angles-----
        do k=1,nang
         phsrl(k)=rlfunc(cs(k))
	enddo       
c-------calculating phase function: start loop over each layer---------------
       do l=1,nlyr
         if (l.ge.ltop .and. l.le.lbot) then
cs       call myspline(nangs(l),xxmu(1,l),phases(1,l),nang,cs,phs(1,l))
        do i1=1,nangs(l)
        xxxmu(i1)=ACOS(xxmu(i1,l))*180./PI
	  IF(PHASES(I1,1).LT.1.0E-30)PHASES(I1,1)=1.0E-30
        xphases(i1)=LOG(phases(i1,l))
cs        write(6,*)'ang,phph',xxxmu(i1),xphases(i1)
        enddo
        do k=1,nang
        xcs=ACOS(cs(k))*180./PI
        phs(k,l)=EXP(LINEAR(xxxmu,xphases,nangs(l),xcs)) 
        enddo
       else 
        do k=1,nang
         phs(k,l)=phsrl(k)
	enddo       
       endif
       enddo
c-------end loop over each layer---------------
c
c-------compute the accumulative tau----------------------------
       tau(0)=0.
       do l=1,nlyr
	tau(l)=tau(l-1)+dtauc(l)	
       enddo
c
c--------perform the single scattering calculations-------------
       
       do j=1,nphi
	do i=1,numu
c
         k=i+(j-1)*numu
	 do l=1,nlyr
	 phs1(l)=phs(k,l)
	 enddo
c
         do k=1,ntaus
           if (.not.lamber) then
           if(INDG.eq.3)then
cs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            albedo=XLAND*CBRF3(is,i,j)+XOCEAN*
     &       SEARF1(umu11,umu0,sfis,GALB,SCR,SCI)
cs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           endif
           if(INDG.eq.1)then
           umu11=-umu(i)
           if(umu11.lt.0)then 
            umu11=abs(umu11)
           sfis=phi(j)*RAD
cs           write(6,*)'umu11',umu11
cs           write(6,*)'umu0',umu0
cs           write(6,*)'sfis',sfis
           albedo=SEARF1(umu11,umu0,sfis,GALB,SCR,SCI)
cs           write(6,*)'albedo',albedo
           endif
           endif
           endif
cs           if(lamber)albedo=GALB/PI
cs          WRITE(*,*)phs1(1)
         call sscat(nlyr,fbeam,umu0,umu(i),tau,ssalb,llyr(k),
     $                      utau(k),phs1,lamber,albedo,mysins)
         uu(i,k,j) = mysins
	 enddo
	enddo
       enddo
       return
       end
C*************************************************************
       subroutine singlep(nlyr,dtauc,ssalb,taurl,xtau,xssa,
     $            fbeam,INDG,GALB,SCR,SCI,umu0,is,numu,umu,nphi,phi,
     $                  nangs,xangs,xphase,ltop,lbot,lamber,
     $                  albedo,gg1,gg2,fr,ntaus,llyr,utau,uu,
     $                  CBRF3)
CD       parameter (maxlyr=40,maxumu=60,maxphi=30)
         parameter (maxlyr=35,maxumu=100,maxphi=200)
         PARAMETER (KNA1U =100)
         PARAMETER (KNA0  =2)
         PARAMETER (KNFI  =200)
         PARAMETER(PI=3.141592654,RAD=PI/180.)
         REAL LINEAR
       logical lamber
       integer nlyr,numu,nphi,i,j,k,l,ltop,lbot,ntaus
       real fbeam,umu0,dtauc(*),ssalb(*),albedo,gg1,gg2,fr
       real xtau(*),xssa(*),taurl(*),umu(*),phi(*)
       real phs(maxumu*maxphi,maxlyr),phsrl(maxumu*maxphi),
     $      cs(maxumu*maxphi),xphase(500,*),xangs(500,*),
     $      xmu, xxmu(500,maxlyr),xxxmu(500),xphases(500)
       real phs1(maxlyr), mysins
       integer nangs(*),llyr(*)
       real phases(500,maxlyr)
       real tau(0:maxlyr),utau(*),uu(maxumu,maxlyr,maxphi)
       DIMENSION CBRF3(KNA0,KNA1U,KNFI)
c-----equivalent phase function (mie+rayleigh)---------
cs        write(6,*)'umu0',umu0
cs        write(6,*)'umu',(umu(i),i=1,numu)
cs        write(6,*)'phi',(phi(i)*RAD,i=1,nphi)
	do l=ltop,lbot
	 do i=1,nangs(l)
	  xmu=cosd( xangs(i,l) )
	  xxmu(i,l)=xmu
	  phases(i,l)=
     $    (taurl(l)*rlfunc(xmu)+xssa(l)*xtau(l)*xphase(i,l))
     $    /(taurl(l)+xssa(l)*xtau(l))
cs         write(6,*)xangs(i,l),'taurl',taurl(l),'xphase',xphase(i,l),
cs     & 'phases',phases(i,l)
	 enddo 
	enddo
c------estimate the phase functions at each viewing angle------
       do j=1,nphi
	do i=1,numu
         k=i+(j-1)*numu
	 cs(k)=-umu0*umu(i) + cosd( phi(j) )
     $    *sqrt( (1.-umu0*umu0)*(1.-umu(i)*umu(i)) )
        enddo
       enddo
       nang=numu*nphi 
c--------rayleigh phase function at the scattering angles-----
        do k=1,nang
         phsrl(k)=rlfunc(cs(k))
	enddo       
c-------calculating phase function: start loop over each layer---------------
       do l=1,nlyr
         if (l.ge.ltop .and. l.le.lbot) then
cs       call myspline(nangs(l),xxmu(1,l),phases(1,l),nang,cs,phs(1,l))
        do i1=1,nangs(l)
        xxxmu(i1)=ACOS(xxmu(i1,l))*180./PI
        xphases(i1)=phases(i1,l)
cs        write(6,*)'ang,phph',xxxmu(i1),xphases(i1)
        enddo
        do k=1,nang
        xcs=ACOS(cs(k))*180./PI
        phs(k,l)=LINEAR(xxxmu,xphases,nangs(l),xcs) 
        enddo
       else 
        do k=1,nang
         phs(k,l)=phsrl(k)
	enddo       
       endif
       enddo
c-------end loop over each layer---------------
c
c-------compute the accumulative tau----------------------------
       tau(0)=0.
       do l=1,nlyr
	tau(l)=tau(l-1)+dtauc(l)	
       enddo
c
c--------perform the single scattering calculations-------------
       
       do j=1,nphi
	do i=1,numu
c
         k=i+(j-1)*numu
	 do l=1,nlyr
	 phs1(l)=phs(k,l)
	 enddo
c
         do k=1,ntaus
           if (.not.lamber) then
           if(INDG.eq.3)then
            albedo=CBRF3(is,i,j)
           endif
           if(INDG.eq.1)then
           umu11=-umu(i)
           if(umu11.lt.0)then 
            umu11=abs(umu11)
           sfis=phi(j)*RAD
cs           write(6,*)'umu11',umu11
cs           write(6,*)'umu0',umu0
cs           write(6,*)'sfis',sfis
           albedo=SEARF1(umu11,umu0,sfis,GALB,SCR,SCI)
cs           write(6,*)'albedo',albedo
           endif
           endif
           endif
cs           if(lamber)albedo=GALB
         call sscat(nlyr,fbeam,umu0,umu(i),tau,ssalb,llyr(k),
     $                      utau(k),phs1,lamber,albedo,mysins)
         uu(i,k,j) = mysins
	 enddo
	enddo
       enddo
       return
       end
C*************************************************************
       	subroutine myspline(n,x,y,nn,xn,yn)
c---------spline fit to derive the yn value at point xn
c  Inputs:
c     n:    the length of x and y
c     x(n): the x values which x(1) < x(2) ... < x(n)
c     y(n): the y value which correspondent to x(n)
c     nn:  the length of vector xx and yy
c     xn:  the x value at which y value is wanted
c
c  Outputs:
c     yn: the wanted y value from the fitting
c
c  Internal variables:
c     yp1: the derivative of y over x at x(1), for natural bc, yp1=1.e31
c     ypn: the derivative of y over x at x(n), for natural bc, ypn=1.e31
c     y2(n): the second derivatives
c
      integer n,nn,ny2,i
      parameter (ny2=5000) 
      real x(*),y(*),xn(*),yn(*),y2(ny2),xx,yy,yp1,ypn
c--------the sorting which makes sure x(1)<x(2)<...<x(n)-------
        call sort2(n,x,y) 
c--------start spline------------
        yp1=1.e31
        ypn=1.e31
	call spline(x,y,n,yp1,ypn,y2)
	do i=1,nn
        xx = xn(i)
	call splint(x,y,y2,n,xx,yy)
        yn(i) = yy
	enddo
	return
     	end 
C***************************************************************
CD      subroutine sscat(fbeam,umu0,umu,tau,omega,layru,utau,
CD     $                     phase,lamber,albedo,mysins)
      subroutine sscat(nlyr,fbeam,umu0,umu,tau,omega,layru,utau,
     $                     phase,lamber,albedo,mysins)
      real fbeam,umu0,umu,tau(0:*),omega(*),utau,phase(*),mysins
      integer layru
      logical lamber
c                                                         initialization
      exp0   = exp( -utau/umu0 )
      mysins = 0.
cs      WRITE(*,*)'layru',lauru
c     
      if ( abs(umu+umu0).gt.2.e-6 )  then
c
      if ( umu.gt.0. )  then
         do 20  lyr = layru, nlyr
            exp1 = exp( - ( (tau(lyr)-utau)/umu + tau(lyr)/umu0 ) )
            mysins = mysins + omega(lyr) * phase(lyr) * (exp0-exp1)
            exp0 = exp1
CD      WRITE(*,*) mysins,phase(lyr),lyr,' mysins,phase(layru), before'
20       continue
      else
c
         do 30  lyr = layru, 1, -1
            exp1 = exp( - ( (tau(lyr-1)-utau)/umu + tau(lyr-1)/umu0 ) )
            mysins = mysins + omega(lyr) * phase(lyr) * (exp0-exp1)
            exp0 = exp1
cs         WRITE(*,*) mysins,phase(lyr),lyr,' mysins,phase(layru), before'
30       continue
      end if

      mysins = fbeam / 12.5664 * mysins * umu0 / (umu0 +umu)
cs      WRITE(*,*) mysins,layru,UMU,UMU0,' mysins2,layru,UMU,UMU0'
        if (umu.gt.0 ) 
cs     $     mysins = mysins + umu0*albedo/3.1415926
     $     mysins = mysins + umu0*albedo
     $     *fbeam*exp( (tau(lyr)-tau(nlyr))/umu - tau(nlyr)/umu0 )
       else
        if (layru.gt.1) then
         do 10  lyr = 1, layru-1
10       mysins = mysins + omega(lyr)*phase(lyr)*(tau(lyr)-tau(lyr-1))
CD        WRITE(*,*) mysins,omega(lyr), phase(lyr),
CD     $ tau(lyr)-tau(lyr-1),' mysins before !!!'
        endif
cs         WRITE(*,*)'fbeam',fbeam
cs         WRITE(*,*)'exp0',exp0
cs         WRITE(*,*)'umu0',umu0
cs         WRITE(*,*)'omega(layru)',omega(layru)
cs         WRITE(*,*)'phase(layru)',phase(layru)
cs         WRITE(*,*)'utau',utau
         mysins = fbeam / 12.5664 *
     $            exp0 / umu0 * ( mysins + omega(layru)*phase(layru)
     $                                   * ( utau-tau(layru-1) ) )
        endif
c  
CD         WRITE(*,*) mysins,nlyr,UMU,UMU0,' mysins, nlyr,UMU,UMU0'
      return
      end
C********************************************************************
c
        real function rlfunc(x)
	real x
	rlfunc=3./4.*(1.+x*x)
	return
	end
c
        real function hgfunc(g,x)
	real x,g
	hgfunc=(1.-g*g)*(1.+g*g-2.*g*x)**(-1.5)
	return
	end
c
      SUBROUTINE sort2(n,arr,brr) 
      INTEGER n,M,NSTACK 
      REAL arr(n),brr(n) 
      PARAMETER (M=7,NSTACK=50) 
      INTEGER i,ir,j,jstack,k,l,istack(NSTACK) 
      REAL a,b,temp 
      jstack=0 
      l=1 
      ir=n 
1     if(ir-l.lt.M)then 
        do 12 j=l+1,ir 
          a=arr(j) 
          b=brr(j) 
          do 11 i=j-1,1,-1 
            if(arr(i).le.a)goto 2 
            arr(i+1)=arr(i) 
            brr(i+1)=brr(i) 
11        continue 
          i=0 
2         arr(i+1)=a 
          brr(i+1)=b 
12      continue 
        if(jstack.eq.0)return 
        ir=istack(jstack) 
        l=istack(jstack-1) 
        jstack=jstack-2 
      else 
        k=(l+ir)/2 
        temp=arr(k) 
        arr(k)=arr(l+1) 
        arr(l+1)=temp 
        temp=brr(k) 
        brr(k)=brr(l+1) 
        brr(l+1)=temp 
        if(arr(l+1).gt.arr(ir))then 
          temp=arr(l+1) 
          arr(l+1)=arr(ir) 
          arr(ir)=temp 
          temp=brr(l+1) 
          brr(l+1)=brr(ir) 
          brr(ir)=temp 
        endif 
        if(arr(l).gt.arr(ir))then 
          temp=arr(l) 
          arr(l)=arr(ir) 
          arr(ir)=temp 
          temp=brr(l) 
          brr(l)=brr(ir) 
          brr(ir)=temp 
        endif 
        if(arr(l+1).gt.arr(l))then 
          temp=arr(l+1) 
          arr(l+1)=arr(l) 
          arr(l)=temp 
          temp=brr(l+1) 
          brr(l+1)=brr(l) 
          brr(l)=temp 
        endif 
        i=l+1 
        j=ir 
        a=arr(l) 
        b=brr(l) 
3       continue 
          i=i+1 
        if(arr(i).lt.a)goto 3 
4       continue 
          j=j-1 
        if(arr(j).gt.a)goto 4 
        if(j.lt.i)goto 5 
        temp=arr(i) 
        arr(i)=arr(j) 
        arr(j)=temp 
        temp=brr(i) 
        brr(i)=brr(j) 
        brr(j)=temp 
        goto 3 
5       arr(l)=arr(j) 
        arr(j)=a 
        brr(l)=brr(j) 
        brr(j)=b 
        jstack=jstack+2 
        if(jstack.gt.NSTACK)pause 'NSTACK too small in sort2' 
        if(ir-i+1.ge.j-l)then 
          istack(jstack)=ir 
          istack(jstack-1)=i 
          ir=j-1 
        else 
          istack(jstack)=j-1 
          istack(jstack-1)=l 
          l=i 
        endif 
      endif 
      goto 1 
      END 
C  (C) Copr. 1986-92 Numerical Recipes Software 71a. 
      SUBROUTINE spline(x,y,n,yp1,ypn,y2) 
      INTEGER n,NMAX, ny2
      PARAMETER (NMAX=5000) 
      parameter (ny2=5000) 
      REAL yp1,ypn,x(n),y(n),y2(ny2) 
      INTEGER i,k 
      REAL p,qn,sig,un,u(NMAX) 
      if (yp1.gt..99e30) then 
        y2(1)=0. 
        u(1)=0. 
      else 
        y2(1)=-0.5 
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1) 
      endif 
      do 11 i=2,n-1 
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1)) 
        p=sig*y2(i-1)+2. 
        y2(i)=(sig-1.)/p 
        u(i)=(6.*((y(i+1)-y(i))/(x(i+1)
     $ -x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig* 
     $ u(i-1))/p 
11    continue 
      if (ypn.gt..99e30) then 
        qn=0. 
        un=0. 
      else 
        qn=0.5 
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1))) 
      endif 
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.) 
      do 12 k=n-1,1,-1 
        y2(k)=y2(k)*y2(k+1)+u(k) 
12    continue 
      return 
      END 
c
      SUBROUTINE splint(xa,ya,y2a,n,x,y) 
      INTEGER n 
      REAL x,y,xa(n),y2a(n),ya(n) 
      INTEGER k,khi,klo 
      REAL a,b,h 
      klo=1 
      khi=n 
1     if (khi-klo.gt.1) then 
        k=(khi+klo)/2 
        if(xa(k).gt.x)then 
          khi=k 
        else 
          klo=k 
        endif 
      goto 1 
      endif 
      h=xa(khi)-xa(klo) 
      if (h.eq.0.) pause 'bad xa input in splint' 
      a=(xa(khi)-x)/h 
      b=(x-xa(klo))/h 
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+
     $   (b**3-b)*y2a(khi))*(h*h)/6. 
      return 
      END 
C***************************************************
C***************************************************
C***********************************************************************
      REAL FUNCTION  LINEAR2( X, Y, M, X1 )
C+---------------------------------------------------------------------+
C!       Linear interpolation function.                                !
C+---------------------------------------------------------------------+
      IMPLICIT  REAL   (A-H, O-Z)
      REAL      X( * ), Y( * )
C
cs      if(M.EQ.2)then
cs      write(6,*)'X'
cs      write(6,*)(X(i),i=1,M)
cs      write(6,*)'Y'
cs      write(6,*)(Y(i),i=1,M)
cs      write(6,*)'X1',X1
cs      endif
      IF ( X1.LT.X(1) )  THEN
         LINEAR2 = Y(1) - ( X(1)-X1 )*( Y(2)-Y(1) )/( X(2)-X(1) )
      ELSE IF ( X1.GT.X(M) )  THEN
         LINEAR2 = Y(M) + ( X1-X(M) )*( Y(M)-Y(M-1) )/( X(M)-X(M-1) )
      ELSE
         DO 10 N = 2, M
            IF ( X1.GE.X(N-1) .AND. X1.LE.X(N) )then
            LINEAR2 = Y(N-1) +
     $         ( X1-X(N-1) )*( Y(N)-Y(N-1) )/( X(N)-X(N-1) )
            endif
         
C*****************************
        IF(X1.EQ.X(N-1)) LINEAR2 =Y(N-1)
        IF(X1.EQ.X(N)) LINEAR2 =Y(N)
C*****************************
10       CONTINUE
      END IF
      RETURN
      END
C**************************  END OF LINEAR2  ****************************
      


