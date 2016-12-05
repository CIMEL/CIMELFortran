      PROGRAM ceformA
!
!     Task: 
!     Formats and filters ce318 data for AERONET inversion 
!
!     Written by: 
!     Victor Estelles (victor.estelles@uv.es)
!     Debugged and Modified by:
!     Xia Xiangao (xxa@mail.iap.ac.cn)
!     modiefied by 
!     Che Huizheng (chehz@cams.cma.gov.cn) on 2013-6-12
!     Track of versions:
!     110914 ceform.0.8 version employed for EAC2011 (mixed mode)
!
!     I/O files:
!     I   ceform.par    ceform parameters file 
!     I   toa.dat       top of atmosphere irradiance data 
!     I   meteo.dat     meteo data for corrections
!     O  yymmdd.DT0    formatted calibration files (if IDT0=1)
!     O  yymmdd.DT4    formatted almucantar files (if IDT4=1)
!     O  yymmdd.tag    formatted tag files 
!
!     Notes:
!     This ceform version is a prelease beta version. Documentation, program estructure
!     and quality filters are being currently modified and/or implemented. Please contact the author
!     for updates
!
!     References:
!     -V.Estelles, M.Campanelli, T.J.Smyth, M.P.Utrillas, J.A.Martinez-Lozano, F.Exposito,
!      "Comparison of SKYRAD4.2 and AERONET inversions on a CIMEL CE318 radiometer", 
!      European Aerosol Conference 2011, Manchester (United Kingdom). 
!     -V.Estelles, M.Campanelli, M.P.Utrillas, F.Exposito, J.A.Martinez-Lozano,
!      "Comparison of AERONET and SKYRAD4.2 inversion products retrieved from a Cimel CE318 sunphotometer", 
!      Atmospheric Measurement Techniques, 2011.
!
      PARAMETER(KNDAYS=1000)
      PARAMETER(KNMET=1000)
      PARAMETER(KNW=10)
      PARAMETER(KNF=12)
      PARAMETER(KCAL=10)
      PARAMETER(KNBR=1000)
      PARAMETER(KNBLK=10)
      PARAMETER(KNAOT=1000) 
      PARAMETER(KNSUN=1000) 

      PARAMETER(NKR=51)
      PARAMETER(KNGAUSS=18)
      PARAMETER(KMA=3)
      PARAMETER(KNCA=3)

      PARAMETER(SELMODEOUTPUT=0) ! SELMODEOUTPUT:0 COUNTS, :1 NM, :2 UM 
      PARAMETER(SELCORRSIM=0) ! SELCORRSIM:0 DESACTIVADA,  :1 ACTIVADA

      PARAMETER(SIMCRIT=10.)
      PARAMETER(AIRTEMP=25.)

      PARAMETER(LOZ=1) ! Pos. number to include the oz channel
C      PARAMETER(LWV=5) ! Pos. number to include the wv channel
      PARAMETER(WVLOZ=0.315) ! Nominal wvl for oz channel
      PARAMETER(WVLWV=0.936) ! Nominal wvl for wv channel

      PARAMETER (PI=3.141592653589793D0,RAD=PI/180.0)
      
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      DOUBLE PRECISION WVL9(KNW),WVL(KNW),ANGKRN(NKR),ANG0(29)
      DOUBLE PRECISION DIRCAL9(KCAL,KNW),DIRCAL(KCAL,KNW),DCAL(KNW)
      DOUBLE PRECISION AURCAL9(KCAL,KNW),AURCAL(KCAL,KNW),ACAL(KNW)
      DOUBLE PRECISION SKYCAL9(KCAL,KNW),SKYCAL(KCAL,KNW),SCAL(KNW)
      DOUBLE PRECISION AURSUM,SKYSUM
      DOUBLE PRECISION RTEMP(KNBR),LTEMP(KNBR)
      DOUBLE PRECISION RMSDI,RMSD(KNBR),SAVG,SMDV(KNBR)
      DOUBLE PRECISION GAMMA,LATRAD,ET,DECL,TST,W0,ATMPRES
      DOUBLE PRECISION ZNR(KNBR),ZNL(KNBR),AZR(KNBR),AZL(KNBR)
      DOUBLE PRECISION MASAR(KNBR),MASAL(KNBR),MASAO(KNBR)
      DOUBLE PRECISION RFO(KNBR),LFO(KNBR),OFO(KNBR)
      DOUBLE PRECISION RFOIRRAD(KNBR),LFOIRRAD(KNBR),OFOIRRAD(KNBR)

	DOUBLE PRECISION ROD(KNBR),LOD(KNBR),OOD(KNBR),FSOL(KNBR)
	DOUBLE PRECISION OBS1(28,KNW),SCA1(28,KNW)
	DOUBLE PRECISION OBS2(28,KNW),SCA2(28,KNW),ANG1(28),ANG2(28)
	DOUBLE PRECISION TOD(KNW),DSZA(KNW)
	INTEGER IND(28,KNW) 

      DOUBLE PRECISION RRATIO(28,KNBR),LRATIO(28,KNBR),RMSDO(KNBR)
      DOUBLE PRECISION ORATIO(28,KNBR),ERATIO(28,KNBR)
      DOUBLE PRECISION RSCAT(28,KNBR),LSCAT(28,KNBR),OSCAT(28,KNBR)
      DOUBLE PRECISION SCMN(KNBR),SCMX(KNBR)
      DOUBLE PRECISION SCATKER(NKR,KNBR),RATKER(NKR,KNBR)
      DOUBLE PRECISION X1,X2,X3,Y1,Y2,Y3,P1,P2,P3,EO
      DOUBLE PRECISION ZEN1,ZEN2,ZEN3,ZEN4,ZEN0
      DOUBLE PRECISION FAI1,FAI2,FAI3,FAI4,FAI0,DST
      DOUBLE PRECISION ANGLEMIN,HORADECIMAL
      DOUBLE PRECISION ANGA(29,KNBR),ANGB(29,KNBR)
      DOUBLE PRECISION ALMA(29,KNBR),ALMB(29,KNBR)
      DOUBLE PRECISION ALNGS,LATDEG,LONDEG,HGTASL 
      DOUBLE PRECISION WVLTOA(2000),FOTOA(2000),FTOP(KNW)
      DOUBLE PRECISION RKK,LKK,AEROZEN(KNBR),CHNLBL(12),DIF,DIFOLD
      DOUBLE PRECISION AOT(KNAOT,11),ZENAOT(KNAOT),TIMEAOT(KNAOT)
      DOUBLE PRECISION AOTI(KNAOT,11),AOTRAY(KNBR),AOTAER(KNBR)
      DOUBLE PRECISION AOTO3(KNBR),AOTNO2(KNBR),AOTWAT(KNBR)
      DOUBLE PRECISION FILTR(KNBR)
      DOUBLE PRECISION A1SMRT,A2SMRT,A3SMRT,A4SMRT
      DOUBLE PRECISION UO,TEO,TRO,C1,C2,AOCORR
      DOUBLE PRECISION O3ABS(KNBR),H2OABS(KNBR),NO2ABS(KNBR)
      DOUBLE PRECISION MIXGABS(KNBR),MIXGTOA(2000),TEMPSUN(KNSUN)
      DOUBLE PRECISION O3TOA(2000),H2OTOA(2000),NO2TOA(2000)
      DOUBLE PRECISION SUN1(11),SUN2(11),SUN3(11),MASSUN(KNSUN)
      DOUBLE PRECISION TIMESUN(KNSUN),SUN(KNSUN,11),SUNI(KNBR,11)
      DOUBLE PRECISION ANGDEVA(KNBR),ANGDEVB(KNBR)
      DOUBLE PRECISION X(KNGAUSS),Y(KNGAUSS),SIG(KNGAUSS)
      DOUBLE PRECISION CHISQ,OCHISQ,AGAUSS(KMA),COVAR(KNCA,KNCA)
      DOUBLE PRECISION ALPHA(KNCA,KNCA),ALAMDA,ERRAGAUSS(KMA)
      DOUBLE PRECISION YCALCMED,SIG2I,DYSUM,RCORRFIT(KNBR)
      DOUBLE PRECISION ALMAAUX(29,KNBR),ALMBAUX(29,KNBR)
      DOUBLE PRECISION S2CALDAY(KCAL),A2CALDAY(KCAL)
      DOUBLE PRECISION DRAUXR(KNBR),TPAUXR(KNBR)
      DOUBLE PRECISION DRAUXL(KNBR),TPAUXL(KNBR)
      DOUBLE PRECISION SKAUXR(24,KNBR),AUAUXR(5,KNBR)
      DOUBLE PRECISION SKAUXL(24,KNBR),AUAUXL(5,KNBR)
c      DOUBLE PRECISION WVLOZ,WVLWV
      DOUBLE PRECISION TCF,MATCH
      DOUBLE PRECISION HRMET(KNMET),PRSMET(KNMET),O3MET(KNMET)
      INTEGER ITST,MA,NCA,IAGAUSS(KMA)
      INTEGER NSETS,NANGLES,NMET
      INTEGER YY,MM,DD,HR(KNBR),MR(KNBR),SR(KNBR)
 	INTEGER YYR(KNBR),MMR(KNBR),DDR(KNBR),IXR(KNBR)
	INTEGER YYL(KNBR),MML(KNBR),DDL(KNBR),IXL(KNBR)
      INTEGER HB(KNBLK),MB(KNBLK),SB(KNBLK)
      INTEGER HL(KNBR),ML(KNBR),SL(KNBR)
      INTEGER IWVL9(KNW),IWVL(KNW),NW9,NW,NDIRCAL,NSKYCAL,SFLT(KNW)
      INTEGER NFILTROR(KNBR),NFILTROL(KNBR),IFLT9(KNW),IFLT(KNW)
      INTEGER RVDIR(KNBR),RVAUR(5,KNBR),RVSKY(24,KNBR)
      INTEGER LVDIR(KNBR),LVAUR(5,KNBR),LVSKY(24,KNBR)
      INTEGER NBR,NBL,NALM,IFLAG(29,KNBR),IDAY
      INTEGER NALIVE,FLGSUM(NKR)
      INTEGER FLAGKER(NKR,KNBR),OFLAG(28,KNBR)
      INTEGER JAUX,DYCAL(KCAL),DMCAL(KCAL),DDCAL(KCAL)
      INTEGER SYCAL(KCAL),SMCAL(KCAL),SDCAL(KCAL)
      INTEGER SHHCAL(KCAL),SMMCAL(KCAL)
      INTEGER HAOT(KNAOT),MAOT(KNAOT),SAOT(KNAOT)
      INTEGER DCALDAY(KCAL),SCALDAY(KCAL),ACALDAY(KCAL)
      INTEGER DAYSXMONTH(12),MMM,DDD,NAOT,SELCIMPROC
      INTEGER HSUN(KNSUN),MSUN(KNSUN),SSUN(KNSUN)
      INTEGER SERIALNUMBER
      INTEGER HSR(KNBR),HSL(KNBR)
      INTEGER HSAUXR(KNBR),HHAUXR(KNBR),MMAUXR(KNBR),SSAUXR(KNBR)
      INTEGER HSAUXL(KNBR),HHAUXL(KNBR),MMAUXL(KNBR),SSAUXL(KNBR)
      INTEGER NFAUXL(KNBR),NFAUXR(KNBR)
      INTEGER HMIN,JMIN,II,HDIFMIN,HDIF
      INTEGER IFLG(KNBR),IL(KNW)
c      INTEGER LOZ,LWV
      INTEGER BA1,BA2,BS1,BS2,NBLK
      INTEGER AURSKY,NAURCAL
      INTEGER AYCAL(KCAL),AMCAL(KCAL),ADCAL(KCAL)
      INTEGER AHHCAL(KCAL),AMMCAL(KCAL)
      INTEGER YYMET(KNMET),MMMET(KNMET),DDMET(KNMET)
      CHARACTER HEADER*3,CDATE*6,FILER*100
      CHARACTER FILEB*14,FILEL*100,CK*10,FILOUT*100,FILTAG*100
      CHARACTER*6 DIRCALDATE9(KCAL),DIRCALTIME9(KCAL)
      CHARACTER*6 AURCALDATE9(KCAL),AURCALTIME9(KCAL)
      CHARACTER*6 SKYCALDATE9(KCAL),SKYCALTIME9(KCAL)
      CHARACTER*1 LINE(500),HDR
      CHARACTER*500 STRING
	CHARACTER*2 MON(12),DAY(31),YEAR(2),TT(60)
	DIMENSION MONTHDAY(12)
	CHARACTER STNS_FN*100,STNS_ID*100,FDATA*256
	CHARACTER FOUT*256,UTC*6,FIPT*256,FDAT*256,FBRDF*256
	CHARACTER OBSDATE1*10,OBSDATE2*10,TDATE(KNDAYS)*6
	DOUBLE PRECISION CIMELWAVE(4),MODISWAVE(7)
	DOUBLE PRECISION BRDF(KNDAYS,7,3),DAYBRDF(7,3),CIMELBRDF(4,3)
	INTEGER MODISDATE(KNDAYS,3),METEODATE(KNDAYS,4),MODISJD(KNDAYS)
	DOUBLE PRECISION OZONE(KNDAYS),UO3
      EQUIVALENCE (LINE,STRING)
!	SKYRAD INPUT WAVELENGTH SEQUENCE: 
!     (1) 315,(2) 440,(3)500,(4)675,
!     (5) 870,(6) 940,(7)1020,(8)16404/1020 NM
!	TONGYU CIMEL NO. 770: WAVELENGTH SEQUENCE: 
!	 1. 1020 NM
!	 2. 870 NM
!	 3. 675 NM			
!      4. 440 NM
!      5. 500 NM
!      17. 1640 NM
!     注意用中心波长最好!
	DATA CIMELWAVE/0.440,0.675,0.870,1.020/
	DATA MODISWAVE/0.470,0.555,0.659,0.858,1.240,1.640,2.130/
      DATA ANGKRN/0.6,1.,1.5,2.,2.5,3.,3.5,4.,4.5,5.,6.,7.,8.,9.,10.,
     112.5,15.,17.5,20.,25.,30.,35.,40.,45.,50.,55.,60.,65.,70.,75.,80.,
     185.,90.,95.,100.,105.,110.,115.,120.,125.,130.,135.,140.,145.,
     1150.,155.,160.,165.,170.,174.,180./

      DATA ANG0/3.,3.5,4.,5.,6.,6.,7.,8.,10.,12.,14.,16.,18.,20.,25.,
     130.,35.,40.,45.,50.,60.,70.,80.,90.,100.,120.,140.,160.,180./
C     注意中心波长!    
      DATA CHNLBL/0.34,0.38,0.44,0.50,0.53,0.55,0.675,0.87,
     10.936,1.02,1.24,1.64/

	DATA ANG1/3.,3.5,4.,5.,6.,7.,8.,10.,12.,14.,16.,18.,20.,25.,
     130.,35.,40.,45.,50.,60.,70.,80.,90.,100.,120.,140.,160.,180./

	
      DATA DAYSXMONTH/0,31,28,31,30,31,30,31,31,30,31,30/
	DATA MON/'01','02','03','04','05','06','07','08','09','10','11','12'/
	DATA DAY/'01','02','03','04','05','06','07','08','09','10',
     1	     '11','12','13','14','15','16','17','18','19','20',
     1         '21','22','23','24','25','26','27','28','29','30','31'/

	DATA TT/'00','01','02','03','04','05','06','07','08','09','10',
     1	     '11','12','13','14','15','16','17','18','19','20',
     1         '21','22','23','24','25','26','27','28','29','30',
     1         '31','32','33','34','35','36','37','38','39','40',
     1         '41','42','43','44','45','46','47','48','49','50',
     1         '51','52','53','54','55','56','57','58','59' /

	DATA MONTHDAY/31,29,31,30,31,30,31,31,30,31,30,31/
	DATA YEAR/'08','09'/


c     台站名称以及台站仪器标号，数据文件前缀
c	STNS_FN='hangzhou' !!! from command line arguments
	getarg(1,STNS_FN)
c	STNS_ID='808' !!! from command line arguments
	getarg(2,STNS_ID)
c	FDATA='hangzhou-808-1' !!! from command line arguments
	getarg(3,FDATA)
C	TO CREATE INPUT FILE TITLE FOR AERONET INVERSION
c	存放参数文件以及数据文件主目录
C     	FIPT='G:\AERONET_INVERSION\ins_para\'
c      FIPT='H:\AERONET_INVERSION\ins_para\' !!! from command line arguments
	getarg(4,FIPT)
c	存储AERONET反演算法所需输入输入文件的目录
c	FOUT='H:\AERONET_INVERSION\input\' !!! from command line arguments
c     &	//trim(STNS_FN)//'\'
	getarg(5,FOUT)
c	FBRDF='H:\AERONET_INVERSION\modis_brdf\'  !!! from command line arguments
	getarg(6,FBRDF)
c	FDAT='H:\AERONET_INVERSION\data\'	!!! from command line arguments
	getarg(7,FDAT)
	OPEN(250,FILE=TRIM(FOUT)//'FNAME.TXT',STATUS='UNKNOWN')

	FILER=TRIM(FDAT)//TRIM(STNS_FN)//'\'//TRIM(FDATA)//'.ALR' 
	FILEL=TRIM(FDAT)//TRIM(STNS_FN)//'\'//TRIM(FDATA)//'.ALL'

c	生成FNAME
      OPEN(UNIT=20,FILE=TRIM(FOUT)//'FNAME',STATUS='UNKNOWN')
	OPEN(10,FILE=TRIM(FILER),STATUS='OLD')
	OBSDATE2='01/01/1990'
	ND=0
	DO I=1,10000000
		READ(10,'(A10)',END=991)OBSDATE1
		IF(OBSDATE1(1:1).NE.'D')THEN
			IF(TRIM(OBSDATE1).NE.TRIM(OBSDATE2))THEN
				ND=ND+1
				TDATE(ND)=OBSDATE1(9:10)//OBSDATE1(4:5)//OBSDATE1(1:2)
c				WRITE(20,*)OBSDATE1(9:10)//OBSDATE1(4:5)//OBSDATE1(1:2)
				OBSDATE2=OBSDATE1
			ENDIF
		ENDIF
	ENDDO
991	CONTINUE

	DO I=1,ND
		DO J=I+1,ND
			IF(TDATE(I).EQ.TDATE(J))THEN
				TDATE(J)='999999'
			ENDIF
		ENDDO
	IF(TDATE(I).NE.'999999')WRITE(20,*)TDATE(I)
	ENDDO
c				
	CLOSE(10)
	CLOSE(20)
C	STOP


c	to read modis brdf parameter
	OPEN(125,
     &FILE=TRIM(FBRDF)//'MCD43C3_'//TRIM(STNS_FN)//
     &'_'//TRIM(STNS_ID)//'.BRDF'
     &	,STATUS='OLD')
	NUMBRDF=0
	DO ID=1,KNDAYS
		READ(125,*,END=999)(MODISDATE(ID,K),K=1,3)
		NUMBRDF=NUMBRDF+1
		IDAY=0
		DO J=1,MODISDATE(ID,2)
			IDAY=IDAY+DAYSXMONTH(J)
		ENDDO
		IDAY=IDAY+MODISDATE(ID,3)
		MODISJD(NUMBRDF)=IDAY+(MODISDATE(ID,1)-1990)*365

		DO IW=1,7
			READ(125,*,END=999)(BRDF(ID,IW,K),K=1,3)
		ENDDO
	ENDDO
999	CONTINUE
	CLOSE(125)

C	STOP	
C	TO READ OZONE DATA
	OPEN(120,FILE=TRIM(FIPT)//'METEO_'//TRIM(STNS_ID)//'.DAT',STATUS='OLD')
	DO I=1,6
		READ(120,*)
	ENDDO
	NOZONE=0
	DO I=1,KNDAYS
		READ(120,*,END=998)(METEODATE(I,K),K=1,4),XX,OZONE(I)
		NOZONE=NOZONE+1
	ENDDO
998	CONTINUE
cz      write(*,*)NOZONE


	
!	TO READ PARAMETERS FROM PARAMETER FILE 'CEFORM.PAR'
	CALL RDPAR(FIPT,ALNGS,LATDEG,LONDEG,HGTASL,NW9,WVL9,IFLT9,IWVL9,
     1SELCIMPROC,SERIALNUMBER,AURSKY,NDIRCAL,DYCAL,DMCAL,DDCAL,
	1DIRCALDATE9,DIRCALTIME9,DIRCAL9,NAURCAL,NSKYCAL,
     1AYCAL,AMCAL,ADCAL,AHHCAL,AMMCAL,
	1SYCAL,SMCAL,SDCAL,SHHCAL,SMMCAL,
	1AURCALDATE9,AURCALTIME9,AURCAL9,SKYCAL9,STNS_ID)


!     Reads date to be processed
      OPEN(UNIT=30,FILE=TRIM(FOUT)//'FNAME',STATUS='OLD')

      DO 190 IJK=1,KNDAYS
C	DO 190 IJK=1,1
		READ(30,*,END=9999) CDATE
C		BACKSPACE(30)
C		CDATE='120620'
C     注释掉，加快处理数据的速度！2013-6-15 CHE HZ
C		WRITE(*,*)CDATE
		READ(CDATE,1000) YY,MM,DD 
 1000		FORMAT(3I2)
		
		
!	to obtain daily right and left Almucantar measurements	
		CALL GET_DAILY_ALM(FILER,CDATE,NBR,
     &	NFILTROR,DDR,MMR,YYR,HR,MR,SR,RVDIR,RVAUR,RVSKY,RTEMP,LONDEG)
		CALL GET_DAILY_ALM(FILEL,CDATE,NBL,
     &	NFILTROL,DDL,MML,YYL,HL,ML,SL,LVDIR,LVAUR,LVSKY,LTEMP,LONDEG)

!     Filter for corrupted data (code 1: unpaired branches)
!	Right and left almucantar scanning number should be equal
		IF(NBR.GT.0.AND.NBR.GT.0)THEN
			IF(NBR.NE.NBL) THEN
				WRITE(*,*) 'Warning!: uncomplete almucantar files (code 1)' 
			ENDIF
C======================================================
!	to derive MODIS BRDF data,找到最近且有MODIS观测数据的BRDF作为该日BRDF参数
			IDAY=0
			DO J=1,MM
				IDAY=IDAY+DAYSXMONTH(J)
			ENDDO
			IDAY=IDAY+DD
			IJUL=IDAY+(YY+2000-1990)*365	!OBSERVATION DAY
			
			
			IX1=1000
			IPOS1=0
			DO ID=1,NUMBRDF
				IF(ABS(IJUL-MODISJD(ID)).LT.IX1)THEN
					IX1=ABS(IJUL-MODISJD(ID))
					IPOS1=ID
				ENDIF
			ENDDO

			IX2=1000
			IPOS2=0
			DO ID=IPOS1-1,IPOS1+1,2
				IF(ABS((MODISJD(ID)-IJUL)).LT.IX2)THEN
					IX2=ABS(IJUL-MODISJD(ID))
					IPOS2=ID
				ENDIF
			ENDDO
			
			DO IW=1,7
				DO IP=1,3
					DAYBRDF(IW,IP)=(BRDF(IPOS1,IW,IP)*IPOS2+
     &				BRDF(IPOS2,IW,IP)*IPOS1)/(IPOS1+IPOS2)
				ENDDO
			ENDDO

C				STOP
					

C			将MODIS波段BRDF参数差值到CIMEL波段
			DO IW=1,4
				IF(IW.EQ.1)KPOS=1
				IF(IW.EQ.2)KPOS=3
				IF(IW.EQ.3)KPOS=4
				IF(IW.EQ.4)KPOS=4
				X1=MODISWAVE(KPOS)
				X2=MODISWAVE(KPOS+1)
				DO IP=1,3
					Y1=DAYBRDF(KPOS,IP)
					Y2=DAYBRDF(KPOS+1,IP)
					CIMELBRDF(IW,IP)=(X2*Y1-X1*Y2)/(X2-X1)
     &				+(Y2-Y1)/(X2-X1)*CIMELWAVE(IW)
				ENDDO
			ENDDO

c	处理海上平台，将渤海反照率均设为0.10 CAMS这里，假设albedo取的是AERONET-cams的平均值。
			IF(STNS_ID.EQ.'000')THEN

C      26:03:2013	0:58:05 0.05547	0.13721	0.19378	0.20048 地表反照率
c					CIMELBRDF(1,1)=0.05547
c	                CIMELBRDF(2,1)=0.13721	
c	                CIMELBRDF(3,1)=0.19378
c					CIMELBRDF(4,1)=0.20048

c       				CIMELBRDF(1,1)=0.05394463
c	                CIMELBRDF(2,1)=0.131445556	
c	                CIMELBRDF(3,1)=0.178351481
c					CIMELBRDF(4,1)=0.186438111			
C					CIMELBRDF(1,1)=CIMELBRDF(1,1)/1.6
C	                CIMELBRDF(2,1)=CIMELBRDF(2,1)*2.3
C	                CIMELBRDF(3,1)=CIMELBRDF(3,1)*2
C					CIMELBRDF(4,1)=CIMELBRDF(4,1)*1.55
			
			ENDIF
					


C======================================================
!	to derive OZONE
			X0=1000
			DO ID=1,NOZONE
				IDAY=0
				DO J=1,METEODATE(ID,2)
					IDAY=IDAY+DAYSXMONTH(J)
				ENDDO
				IDAY=IDAY+METEODATE(ID,3)
				KJUL=IDAY+(METEODATE(ID,1)-1990)*365
				IF(OZONE(ID).GT.0.AND.ABS(IJUL-KJUL).LT.X0)THEN
					X0=ABS(IJUL-KJUL)
					UO3=OZONE(ID)
				ENDIF
			ENDDO			

!	to write output file for the AEROENT inversion 
!	to sort right and left almucantar measurements according measurement time
			CALL sortX(NBR,LONDEG,YYR,MMR,DDR,HR,MR,SR,
     1		   NFILTROR,RVDIR,RVAUR,RVSKY,RTEMP,HSR)
			CALL sortX(NBL,LONDEG,YYL,MML,DDL,HL,ML,SL,
     1		   NFILTROL,LVDIR,LVAUR,LVSKY,LTEMP,HSL)

			CALL CHKALM(NBR,YYR,MMR,DDR,HR,MR,SR,NFILTROR,
     1	        RVDIR,RVAUR,RVSKY,RTEMP,HSR,
     1            NBL,YYL,MML,DDL,HL,ML,SL,NFILTROL,
     1            LVDIR,LVAUR,LVSKY,LTEMP,HSL,NW9)

!	to pair right and left almucantar measurements if difference in time between left and right scanning less than 60 s
			CALL PAIR(NBR,YYR,MMR,DDR,HR,MR,SR,NFILTROR,
     1		  RVDIR,RVAUR,RVSKY,RTEMP,HSR,
     1          NBL,YYL,MML,DDL,HL,ML,SL,NFILTROL,
     1          LVDIR,LVAUR,LVSKY,LTEMP,HSL)

!     Filter for corrupted data (code 2: incomplete series)
			NALM=NBR/NW9
			IF(NALM*NW9.LT.NBR) THEN
				WRITE(*,*) 'Warning!: uncomplete almucantar files (code 2)'
			ENDIF

c			CALL CHKALM(NBR,YYR,MMR,DDR,HR,MR,SR,NFILTROR,
c     1	        RVDIR,RVAUR,RVSKY,RTEMP,HSR,
c     1            NBL,YYL,MML,DDL,HL,ML,SL,NFILTROL,
c     1            LVDIR,LVAUR,LVSKY,LTEMP,HSL,NW9)

!     Reduces arrays for active wavelengths 
!	To select active wavelengths deterimined by ins.para
			NW=0
			DO I=1,NW9
				IF(IWVL9(I).GT.0) THEN
					NW=NW+1
					WVL(NW)=WVL9(I) !ACTIVE WAVELENGTHS
					IFLT(NW)=IFLT9(I)
					IWVL(NW)=IWVL9(I)
					DO J=1,NDIRCAL
						DIRCAL(J,NW)=DIRCAL9(J,I)	!DIRECT CALIBRATION COEFFICIENTS 
					ENDDO
					DO J=1,NAURCAL
						AURCAL(J,NW)=AURCAL9(J,I)	!AUROELE CALIBRATION COEFFICIENTS
					ENDDO
					DO J=1,NSKYCAL
						SKYCAL(J,NW)=SKYCAL9(J,I)	!SKY RADIANCE CALIBRATION COEFFICIENTS
					ENDDO
				ENDIF
   10			ENDDO
 8765			FORMAT(I2,F5.2,2I3) 
 
!	WHAT'S FUNCTION OF THESE CODES
!	选择那些只有在参数文件中设定有的观测波段的数据
			I=0
			II=0
			DO WHILE(I.LT.NBR)
				I=I+1
				DO L=1,NW
					IF(NFILTROR(I).EQ.IFLT(L)) THEN
						II=II+1
						IXR(II)=I
					ENDIF
				ENDDO
			ENDDO 

			NBR=II
			NBL=NBR

			CALL SELECT_OBS(YYR,MMR,DDR,HR,MR,SR,NBR,IXR,
     1                      NFILTROR,RVDIR,RVAUR,RVSKY,RTEMP)
			CALL SELECT_OBS(YYL,MML,DDL,HL,ML,SL,NBR,IXR,
     1                      NFILTROL,LVDIR,LVAUR,LVSKY,LTEMP)


C     orders the almucantars by the new filter number 
c	The objective is to match wavelengths in ins.para with their corresponding sky radiance
c	有3个排序：1、观测数据中由波段号确定的排序(NFILTROR)；2、参数文件中给定的排序(IFLT)；3、按照波长确定的排序(440, 500, 675, 870, 1020, 1640 nm)
c	确保输出中的排列顺序是按照参数文件中给定的排序：先读取IFLT的排序并确定相应的波长，尔后据此将观测按此排序
			II=0 
			NALM=NBR/NW
			DO I=1,NALM
				DO K=1,NW
					DO J=(I-1)*NW+1,(I-1)*NW+NW
						IF(NFILTROR(J).EQ.IFLT(K)) THEN
							II=II+1
							IXR(II)=J
						ENDIF
					ENDDO
				ENDDO
			ENDDO
			CALL SELECT_OBS(YYR,MMR,DDR,HR,MR,SR,NBR,IXR,
     1                      NFILTROR,RVDIR,RVAUR,RVSKY,RTEMP)
			CALL SELECT_OBS(YYL,MML,DDL,HL,ML,SL,NBR,IXR,
     1                      NFILTROL,LVDIR,LVAUR,LVSKY,LTEMP)


!	THE END OF QUALITY CHECK OF FILTER MEASUREMENTS
	

!	to derive direct, aureole and sky calibration coefficients for a specified day
			CALL INTERPOLATION_CALIBRATION(SELCIMPROC,DYCAL,DMCAL,DDCAL,
     1	                           NDIRCAL,DIRCAL,YY,MM,DD,DCAL,NW)

			CALL INTERPOLATION_CALIBRATION(SELCIMPROC,AYCAL,AMCAL,ADCAL,
     1	                           NAURCAL,AURCAL,YY,MM,DD,ACAL,NW)

			CALL INTERPOLATION_CALIBRATION(SELCIMPROC,SYCAL,SMCAL,SDCAL,
     1	                           NSKYCAL,SKYCAL,YY,MM,DD,SCAL,NW)

!     Reads extraterrestrial irradiance and gases absorption coefs
			CALL TOA(FIPT,FTOP,H2OABS,MIXGABS,O3ABS,NO2ABS,WVL,NW)
			FTOP(1)=1.864
			FTOP(2)=1.515
			FTOP(3)=0.960
			FTOP(4)=0.716
			DO I=1,NBR
				DO J=1,29 
					ANGA(J,I)=ANG0(J)
					ANGB(J,I)=ANG0(J)
				ENDDO
			ENDDO


!     Checking radiance calibration availablity and calculate sky and aureole radiance using calibration coefficients

			CALL AURSKYRAD(AURSKY,IFLAG,NBR,NW,
     1	           RVAUR,RVSKY,LVAUR,LVSKY,ACAL,SCAL,ALMA,ALMB)
!     Temperature correction 
			DO I=1,NBR
				CALL TCOF(RTEMP(I),NFILTROR(I),1,IFLT9,WVL9,TCF)
				RVDIR(I)=RVDIR(I)/(1.+TCF/100.*(RTEMP(I)-25.))
				LVDIR(I)=LVDIR(I)/(1.+TCF/100.*(LTEMP(I)-25.))
				CALL TCOF(RTEMP(I),NFILTROR(I),2,IFLT9,WVL9,TCF)
				DO J=1,5
					ALMA(J,I)=ALMA(J,I)/(1.+TCF/100.*(RTEMP(I)-25.))
					ALMB(J,I)=ALMB(J,I)/(1.+TCF/100.*(LTEMP(I)-25.))
				ENDDO
				CALL TCOF(RTEMP(I),NFILTROR(I),3,IFLT9,WVL9,TCF)
				DO J=6,29
					ALMA(J,I)=ALMA(J,I)/(1.+TCF/100.*(RTEMP(I)-25.))
					ALMB(J,I)=ALMB(J,I)/(1.+TCF/100.*(LTEMP(I)-25.))
				ENDDO
			ENDDO

!	to obtain calibration applied and temerature correction (angle 1-29, NBR scanning, NBR/NW)
	
!     Sun earth distance
			CALL SZA(NBR,NBL,MM,DD,HR,MR,SR,HL,ML,SL,LATDEG,LONDEG,
     1	       ZNR,ZNL,AZR,AZL,MASAR,MASAL,MASAO,DST)

!     Sun direct measurements 
			DO I=1,NBR
				L=I-((I-1)/NW)*NW
				IF(SELMODEOUTPUT.EQ.0) THEN
!     Direct in digital numbers
					RFO(I)=RVDIR(I)
					LFO(I)=LVDIR(I)
					OFO(I)=0.5*(RFO(I)+LFO(I)) !the average of right and left measurement 
				ELSE IF(SELMODEOUTPUT.EQ.1) THEN
!     Direct in W/m2NM
				RFO(I)=RVDIR(I)*FTOP(L)/DCAL(L)
				LFO(I)=LVDIR(I)*FTOP(L)/DCAL(L)
				OFO(I)=0.5*(RFO(I)+LFO(I))
				ELSE IF(SELMODEOUTPUT.EQ.2) THEN
!     Direct in W/m2/uM
					RFO(I)=1.E+3*RVDIR(I)*FTOP(L)/DCAL(L)
					LFO(I)=1.E+3*LVDIR(I)*FTOP(L)/DCAL(L)
					OFO(I)=0.5*(RFO(I)+LFO(I))
				ENDIF
			ENDDO

!     Direct irradiance for ratio always in W/m2/NM for cancel radiance units
			DO I=1,NBR
				L=I-((I-1)/NW)*NW
				RFOIRRAD(I)=RVDIR(I)*FTOP(L)/DCAL(L)
				LFOIRRAD(I)=LVDIR(I)*FTOP(L)/DCAL(L)
				OFOIRRAD(I)=0.5*(RFOIRRAD(I)+LFOIRRAD(I))
		!	to calculate optical depth
				IF(RVDIR(I).GT.50.AND.LVDIR(I).GT.50)THEN
					ROD(I)=-LOG(RVDIR(I)/DCAL(L)/DST**2)/MASAR(I)
					LOD(I)=-LOG(LVDIR(I)/DCAL(L)/DST**2)/MASAL(I)
					OOD(I)=(ROD(I)+LOD(I))/2
C	WRITE(*,*) 'ROD(I)=', ROD(I),'LOD(I)=', LOD(I)
				ELSE
					ROD(I)=-9.9
					LOD(I)=-9.9
					OOD(I)=-9.9
				ENDIF
				FSOL(I)=FTOP(L)
		!		
c      write(*,'(2I3,i6,4F12.5)') nfiltrol(i),l,lvdir(i),
C     1ftop(l),dcal(l),lfoirrad(i)
			ENDDO

!     Ratio computation 
			DO I=1,NBR
				JJ=1
				DO J=1,29
					IF(J.NE.6) THEN
						IF(RVDIR(I).GT.50.AND.LVDIR(I).GT.50) THEN
		!					RRATIO(JJ,I)=ALMA(J,I)/(RFOIRRAD(I)*MASAR(I))
		!					LRATIO(JJ,I)=ALMB(J,I)/(LFOIRRAD(I)*MASAL(I))
						RRATIO(JJ,I)=ALMA(J,I)*PI/(FSOL(I)/DST**2)*1.0
						LRATIO(JJ,I)=ALMB(J,I)*PI/(FSOL(I)/DST**2)*1.0
		!				RRATIO(JJ,I)=ALMA(J,I)
		!				LRATIO(JJ,I)=ALMB(J,I)
							ORATIO(JJ,I)=0.5*(RRATIO(JJ,I)+LRATIO(JJ,I))
							ERATIO(JJ,I)=0.5*(RRATIO(JJ,I)-LRATIO(JJ,I))
							OFLAG(JJ,I)=IFLAG(J,I)
						ELSE 
							RRATIO(JJ,I)=9.9
							LRATIO(JJ,I)=9.9
							ORATIO(JJ,I)=9.9
							ERATIO(JJ,I)=9.9
							OFLAG(JJ,I)=0
						ENDIF
					JJ=JJ+1
					ENDIF
				ENDDO
			ENDDO

!     Scattering angle computation
	        DO I=1,NBR
				JJ=1
				DO J=1,29
					IF(J.NE.6) THEN
						AM=COS(ZNR(I))
						AM00=COS(ZNR(I))
C				WRITE(*,*)AM00*AM+SQRT((1.-AM00**2)*(1.-AM**2))
				RSCAT(JJ,I)=ACOS(AM00*AM+SQRT((1.-AM00**2)*(1.-AM**2))
     1						*COS(ANGA(J,I)*RAD))/RAD
						AM=COS(ZNL(I))
				LSCAT(JJ,I)=ACOS(AM*AM+SQRT((1.-AM**2)*(1.-AM**2))
     1						*COS(ANGB(J,I)*RAD))/RAD
		OSCAT(JJ,I)=0.5*(RSCAT(JJ,I)+LSCAT(JJ,I))  !SCATTERING ANGLE
		SCMN(I)=ACOS(AM*AM+SQRT((1.-AM**2)*(1.-AM**2))*COS(0.))/RAD
		SCMX(I)=ACOS(AM*AM+SQRT((1.-AM**2)*(1.-AM**2))*COS(PI))/RAD
						JJ=JJ+1
					ENDIF
				ENDDO
			ENDDO
C     VARIABLE 1: ATMOSPHERIC OPTICAL DEPTH (ROD(NBR),LOD(NBR),OOD(NBR))
C	VARIABLE 2: PI*I/(FO/DST^2) RRATIO,LRATIO,ORATIO(28,NBR), 
C     THE FIRST DIMENSION OF V2: AZIMUTH ANGLE FROM 3, 3.5, 4, 5,...180
C	VARIABLE 3: OFLAG(28,NBR), INDICATOR OF VALID/INVALID MEASUREMENTS


			DO I=1,NBR/NW
C     车慧正测试UTC，2013-6-11 (HR((I-1)*NW+1)+1)是440nm观测的时间，+4是1020nm的观测时间；NW是选择的波段，4个波段，440-675-870-1020. BY CHE
c		WRITE(*,*)HR((I-1)*NW+4),MR((I-1)*NW+4),SR((I-1)*NW+4)
C          WRITE(*,*)((I-1)*NW+1),'nw',NW
c		WRITE(*,*)TT(HR((I-1)*NW+4)+1)
c		WRITE(*,*)TT(MR((I-1)*NW+4)+1)
c		WRITE(*,*)TT(SR((I-1)*NW+4)+1)
				NVALID=0
				OBS1(:,1:NW)=ORATIO(:,(I-1)*NW+1:I*NW)	!PI*I/F0
				IND(:,1:NW)=OFLAG(:,(I-1)*NW+1:I*NW)	!VALID INDICATOR
				SCA1(:,1:NW)=OSCAT(:,(I-1)*NW+1:I*NW)	!SCATTERING ANGLE
				TOD(1:NW)=OOD((I-1)*NW+1:I*NW)			!TOTAL ATMOSPHERIC OPTICAL DEPTH
				NTOD=0
				DO IW=1,NW
					IF(TOD(IW).GT.0)NTOD=NTOD+1
				ENDDO
				 
				DSZA(1:NW)=
     &			(ZNR((I-1)*NW+1:I*NW)+ZNL((I-1)*NW+1:I*NW))/2*180/PI
				DO J=1,28
					IF(SUM(IND(J,:)).EQ.4.AND.ANG1(J).GE.4)THEN			!OBSERVATIONS AT FOUR WAVELENGTH ARE VALID
						NVALID=NVALID+1
						OBS2(NVALID,1:NW)=OBS1(J,1:NW)
						SCA2(NVALID,1:NW)=SCA1(J,1:NW)
						ANG2(NVALID)=ANG1(J)
					ENDIF
				ENDDO
!		有效观测数据：每次观测有15个角度有效，所有TOD大于0，2013-6-13 corrected as: HR((I-1)*NW+4)+1，1020nm观测的时间，原来HR((I-1)*NW+1)+1，是440的时间-BY CHE
				IF(NVALID.GE.15.AND.NTOD.EQ.NW)THEN 
					UTC=TT(HR((I-1)*NW+4)+1)//TT(MR((I-1)*NW+4)+1)
	1				//TT(SR((I-1)*NW+4)+1)

					WRITE(250,'(A30)')TRIM(STNS_FN)//'_'//
     1				TRIM(STNS_ID)//'_'//CDATE//'_'//UTC

					OPEN(101,FILE=
     1				TRIM(FOUT)//TRIM(STNS_FN)//'_'//TRIM(STNS_ID)//'_'
     2				//CDATE//'_'//UTC//'.TXT',STATUS='UNKNOWN')

					WRITE(101,'(9I5)')NW*(NVALID+1),31,1,-1,0,0,1,0,1
					WRITE(101,'(7I5)')1,4,1,0,0,1,1
					WRITE(101,'(4F8.4)')0.440,0.675,0.870,1.020
					WRITE(101,'(4I4)')1,1,1,1
					WRITE(101,'(4I4)')0,0,0,0
					WRITE(101,'(4I4)')1,1,1,1
					WRITE(101,'(4I4)')1,1,1,1
					WRITE(101,'(5I4)')-1,22,22,0,-1
					WRITE(101,'(F6.2,F6.1,I4)')0.05,15.0,0
					WRITE(101,'(A50)')
     1				'PARAMETERS IF MATRIX INVERSION (OR Q-iterations):'
					WRITE(101,'(I2,I6,I2,I2)')3,17000,0,0
					WRITE(101,'(A44)')
     1				'PARAMETERS FOR LEVENB.-MARQUARDT CORRECTION:'
					WRITE(101,'(I4,6F8.4)')15,0.0005,0.5,1.33,1.6,0.0005,0.5
					WRITE(101,'(19(F6.3,F6.0))')0.001, 1000., 0.001, 1000., 
     1											0.001, 1000., 0.001, 1000., 
     1											0.001, 1000., 0.001, 1000., 
     1  								            0.001, 1000., 0.001, 1000., 
     1 											0.001, 1000., 0.001, 1000., 
     1 											0.001, 1000., 0.001, 1000.,
     1											0.001, 1000., 0.001, 1000., 
     1											0.001, 1000., 0.001, 1000., 
     1											0.001, 1000., 0.001, 1000., 
     1											0.001, 1000.

					WRITE(101,'(A22)')'SMOOTHNESS PARAMETERS:'
					WRITE(101,'(I3,E10.1,4I3)')0,1.0E-4,0,0,0,0
					WRITE(101,'(I3,E10.1,I3,E10.1,I3,E10.1)')
     1				3,1.0E-3,1,1.0E-1,1,1.0E-4
					WRITE(101,'(2I2)')0,0
					WRITE(101,'(I3,E10.2)')0,0.00E-0	
					WRITE(101,'(6E10.1)')1.0E-3,1.0E-2,1.0E-3,
     1				5.0E-3,5.0E-0,1.0E-7
					WRITE(101,'(A13)')'MEASUREMENTS:'
					DO II=1,NW
						WRITE(101,'(28F8.4)')TOD(II),OBS2(1:NVALID,II)
					ENDDO
					WRITE(101,*)'INITIAL GUESS FOR THE SOLUTION:'
					WRITE(101,'(8F8.3)')1.50,1.5,1.5,1.5,0.005,
     1				0.005,0.005,0.005
					WRITE(101,'(F8.6)')0.001459
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.007293
					WRITE(101,'(F8.6)')0.001459
					WRITE(101,'(F8.1,F5.1)')100.0,1.1
					WRITE(101,'(I2)')2
					WRITE(101,'(F4.1,I3,F5.2)')0.0,0,0.05
					WRITE(101,'(F3.1,I2,F8.4,5I4)')0.0,1,0.0078,4,1,
     1				NVALID+2,2*NVALID+3,3*NVALID+4
					WRITE(101,'(2I3,F5.2)')1,0,0.01
					WRITE(101,'(7I3,F6.2)')7,1,1,4,1,1,1,0.00
					WRITE(101,'(F6.3,3F4.1,I6)')HGTASL/1000,1.0,2.0,1.0,1000
					WRITE(101,'(I3,F5.1,2I3,2A3)')6,0.0,0,0,'F','F'
c					WRITE(101,'(I3,F5.1,2I3,2A3)')6,0.0,3,0,'F','F'
c					WRITE(101,'(4F8.5)')1.33739,1.33739,1.33739,1.33739
c					WRITE(101,'(4E10.2)')1.12E-09,1.12E-09,1.12E-09,1.12E-09
c					WRITE(101,'(F6.3)')4.573
c					WRITE(101,'(2F6.3)')1.0,0.0
					WRITE(101,'(F6.3,F6.0)')HGTASL/1000,70.
					WRITE(101,'(F8.4)')LATDEG
!	to write parameters for each wavelength
					DO IW=1,4
						WRITE(101,'(F6.4)')CIMELWAVE(IW)
						WRITE(101,'(3F10.6)')(CIMELBRDF(IW,K),K=1,3)	
						IF(IW.EQ.2)THEN
						WRITE(101,'(F6.4,F8.1,F4.1)')0.0124,UO3,1.3
						ELSE
						WRITE(101,'(F6.4,F8.1,F4.1)')0.0,UO3,1.3
						ENDIF

						WRITE(101,'(I2)')1
						WRITE(101,'(F6.4)')HGTASL/1000
						WRITE(101,'(I2)')1
						WRITE(101,'(F8.4)')DSZA(1)	!SZA
						WRITE(101,'(I2)')1
						WRITE(101,'(F8.4)')DSZA(1)	!SZA
						WRITE(101,'(I3)')NVALID+1
						WRITE(101,'(56F6.1)')
     1					0.0,ANG2(1:NVALID),0.0,SCA2(1:NVALID,1)
					ENDDO
						


					WRITE(101,'(I2)')7
					DO IW=1,7
						WRITE(101,'(4F10.6)')MODISWAVE(IW),
     &					(DAYBRDF(IW,K),K=1,3)
					ENDDO

					WRITE(101,'(A30)')
     1				CDATE(5:6)//':'//CDATE(3:4)//':20'//CDATE(1:2)//','
     2				//UTC(1:2)//':'//UTC(3:4)//':'//UTC(5:6)//',Almucantar,'//
	3				TRIM(STNS_FN)//','//TRIM(STNS_ID)

				ENDIF
			ENDDO
			
				

 1090			FORMAT(I5,2I3,3I3,F6.2,' : IY IM ID IHH IMM ISS TM(hr)')
 1091			FORMAT(F8.2,F8.2,F8.2,F8.1,' : ALNGS ALNG ALAT ALT(m)')
 1092			FORMAT(I4,2F7.2,F8.4' : NA TH0 FI0 DST')
 1093			FORMAT(I4,' : NW / WL')
 1001			FORMAT(2F8.2,10E20.12)



 1096			FORMAT(10E10.3)
 1097			FORMAT(10(2F8.2,E20.12))
 1098			FORMAT(10(2F8.2,E20.12))
 1099			FORMAT(I4,I5,2I3,F6.2,F8.2,F7.2,F7.2,F6.1)

 1100			FORMAT('VALID ALMUCANTARS DAY',A7,':',I3,'/',I2)
		ENDIF
! 1100 FORMAT(A7,':',I3,'/',I2)

  190 ENDDO

 9999 CLOSE(30)
      STOP
      END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTION TCOF
!     Returns the termic coefficient 

      SUBROUTINE TCOF(TP,ICH,ISC,IFLT9,WVL9,TCF)
	PARAMETER(KNBR=1000,KNW=10)
      PARAMETER(KNF=12)
      DOUBLE PRECISION TP,CFMTX(3,3,12)
      INTEGER ISC,ICH
	INTEGER IFLT9(KNW)

	DOUBLE PRECISION WV(12),TCF,WVL9(KNW)

      DATA WV/0.34,0.38,0.44,0.50,0.53,0.55,
	1        0.67,0.87,0.94,1.02,1.246,1.61/
      DATA (((CFMTX(I,J,K),K=1,KNF),J=1,3),I=1,3)/
!     340,380,440,500,530,550,670,870,940,1020,1246,1610
     10.,0.,-0.0000,0.,0.,0.,-0.0000,-0.0000,0.,0.2337,0.,0., ! dir, T<=21
     10.,0.,-0.0125,0.,0.,0.,-0.0167,-0.0120,0.,0.2337,0.,0., ! aur, T<=21
     10.,0.,-0.0382,0.,0.,0.,-0.0447,-0.0465,0.,0.2172,0.,0., ! sky, T<=21
     10.,0.,-0.0000,0.,0.,0.,-0.0000,-0.0000,0.,0.2567,0.,0., ! dir, 21<T<=32
     10.,0.,-0.0006,0.,0.,0.,-0.0124,-0.0070,0.,0.2567,0.,0., ! aur, 21<T<=32
     10.,0.,-0.0359,0.,0.,0.,-0.0383,-0.0407,0.,0.2283,0.,0., ! sky, 21<T<=32
     10.,0.,-0.0000,0.,0.,0.,-0.0000,-0.0000,0.,0.2797,0.,0., ! dir, 32<T
     10.,0.,-0.0136,0.,0.,0.,-0.0080,-0.0021,0.,0.2797,0.,0., ! aur, 32<T
     10.,0.,-0.0336,0.,0.,0.,-0.0319,-0.0349,0.,0.2394,0.,0./ ! sky, 32<T
c     10.,0.,-0.0000,0.,0.,0.,-0.0,0.0688,0.,0.2918,0.,0., ! dir, T<=21
c     10.,0.,-0.0000,0.,0.,0.,-0.0,0.0688,0.,0.2918,0.,0., ! aur, T<=21
c     10.,0.,-0.0000,0.,0.,0.,-0.0,0.0688,0.,0.2918,0.,0., ! sky, T<=21
c     10.,0.,-0.0000,0.,0.,0.,-0.0,0.0688,0.,0.2918,0.,0., ! dir, 21<T<=32
c     10.,0.,-0.0000,0.,0.,0.,-0.0,0.0688,0.,0.2918,0.,0., ! aur, 21<T<=32
c     10.,0.,-0.0000,0.,0.,0.,-0.0,0.0688,0.,0.2918,0.,0., ! sky, 21<T<=32
c     10.,0.,-0.0000,0.,0.,0.,-0.0,0.0688,0.,0.2918,0.,0., ! dir, 32<T
c     10.,0.,-0.0000,0.,0.,0.,-0.0,0.0688,0.,0.2918,0.,0., ! aur, 32<T
c     10.,0.,-0.0000,0.,0.,0.,-0.0,0.0688,0.,0.2918,0.,0./ ! sky, 32<T

!     Selects temperature range   
      IF(TP.LE.21) THEN
	    ITP=1
		ELSE IF(TP.GT.21.AND.TP.LE.32) THEN
		ITP=2
		ELSE IF(TP.GT.32) THEN
		ITP=3
      ENDIF

      IX=0
	DO I=1,KNW
		IF(ICH.EQ.IFLT9(I))THEN
			IX=I
		ENDIF
	ENDDO
C	WRITE(*,*)WVL9(IX),IX
	JX=0
	XMIN=100
	DO I=1,KNF
		IF(ABS(WVL9(IX)-WV(I)).LT.XMIN)THEN
			XMIN=ABS(WVL9(IX)-WV(I))
			JX=I
		ENDIF
	ENDDO
C	WRITE(*,*)WV(JX),JX
	TCF=CFMTX(ITP,ISC,JX)
	RETURN
C      TCOF=CFMTX(ITP,ISC,ICH)
      END

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C	SUBROUTINE MRQMIN
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,nca,
     1chisq,alamda,ierr)
      INTEGER ma,nca,ndata,ia(ma),MMAX,j,k,l,mfit
      PARAMETER (MMAX=20) ! Set to largest number of fit parameters
      DOUBLEPRECISION alamda,chisq,funcs,a(ma),alpha(nca,nca)
      DOUBLEPRECISION sig(ndata),x(ndata),y(ndata),covar(nca,nca)
      DOUBLEPRECISION ochisq,atry(MMAX),beta(MMAX),da(MMAX)
      SAVE ochisq,atry,beta,da,mfit
      if(alamda.lt.0.) then ! Initialization.
      mfit=0
      do 11 j=1,ma
      if (ia(j).ne.0) mfit=mfit+1
   11 enddo
      alamda=0.001
      call mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nca,chisq)
      ochisq=chisq
      do 12 j=1,ma
      atry(j)=a(j)
   12 enddo
      endif
      do 14 j=1,mfit ! Alter linearized fitting matrix, by augmenting diagonal elements. 
      do 13 k=1,mfit
      covar(j,k)=alpha(j,k)
   13 enddo
      covar(j,j)=alpha(j,j)*(1.+alamda)
      da(j)=beta(j)
   14 enddo
      call gaussj(covar,mfit,nca,da,1,1,ierr) ! Matrix solution.
      IF(IERR.EQ.1) GOTO 999
      if(alamda.eq.0.)then ! Once converged, evaluate covariance matrix.	
      call covsrt(covar,nca,ma,ia,mfit)
      call covsrt(alpha,nca,ma,ia,mfit) ! Spread out alpha to its full size too.
      return
      endif
      j=0
      do 15 l=1,ma ! Did the trial succeed?
      if(ia(l).ne.0) then
      j=j+1
      atry(l)=a(l)+da(j)
      endif
   15 enddo
      call mrqcof(x,y,sig,ndata,atry,ia,ma,covar,da,nca,chisq)
      if(chisq.lt.ochisq)then ! Success, accept the new solution.
      alamda=0.1*alamda
      ochisq=chisq
      do 17 j=1,mfit
      do 16 k=1,mfit
      alpha(j,k)=covar(j,k)
   16 enddo
      beta(j)=da(j)
   17 enddo
      do 18 l=1,ma
      a(l)=atry(l)
   18 enddo
      else ! Failure, increase alamda and return.
      alamda=10.*alamda
      chisq=ochisq
      endif
  999 CONTINUE
      return
      END

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C	SUBROUTINE MRQCOF
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	SUBROUTINE mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nalp,chisq)
	INTEGER ma,nalp,ndata,ia(ma),MMAX
	PARAMETER (MMAX=20)
	DOUBLEPRECISION chisq,a(ma),alpha(nalp,nalp),beta(ma),sig(ndata)
	DOUBLEPRECISION dy,sig2i,wt,ymod,dyda(MMAX),x(ndata),y(ndata)
C	Used by mrqmin to evaluate the linearized .tting matrix alpha, and vector beta as in (15.5.8), and calculate .2.
	INTEGER mfit,i,j,k,l,m
	mfit=0
	do 11 j=1,ma
	if (ia(j).ne.0) mfit=mfit+1
   11	enddo
	do 13 j=1,mfit ! Initialize (symmetric) alpha, beta.
	do 12 k=1,j
	alpha(j,k)=0.
   12	enddo
	beta(j)=0.
   13	enddo
	chisq=0.
	do 16 i=1,ndata ! Summation loop over all data.
	call AURFIT(x(i),a,ymod,dyda,ma)
	sig2i=1./(sig(i)*sig(i))
	dy=y(i)-ymod
	j=0
	do 15 l=1,ma
	if(ia(l).ne.0) then
	j=j+1
	wt=dyda(l)*sig2i
	k=0
	do 14 m=1,l
	if(ia(m).ne.0) then
	k=k+1
	alpha(j,k)=alpha(j,k)+wt*dyda(m)
	endif
   14	enddo
	beta(j)=beta(j)+dy*wt
	endif
   15	enddo
	chisq=chisq+dy*dy*sig2i ! And .nd .2.
   16	enddo
	do 18 j=2,mfit ! Fill in the symmetric side.
	do 17 k=1,j-1
	alpha(k,j)=alpha(j,k)
   17	enddo
   18	enddo
	return
	END

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C	SUBROUTINE FUNCS
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE AURFIT(X,A,Y,DYDA,NA)
      INTEGER I,NA
      DOUBLEPRECISION X,Y,A(NA),DYDA(NA)
      DOUBLEPRECISION ARG,EX,FAC
      ARG=(X-A(2))/A(3)
      EX=dEXP(-ARG**2)
      FAC=A(1)*EX*2.*ARG
      Y=A(1)*EX
      DYDA(1)=EX
      DYDA(2)=FAC/A(2)
      DYDA(3)=FAC*ARG/A(3)
      RETURN
      END

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C	SUBROUTINE GAUSSJ
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	SUBROUTINE gaussj(a,n,np,b,m,mp,ierr)
	INTEGER m,mp,n,np,NMAX
	DOUBLEPRECISION a(np,np),b(np,mp)
	DOUBLEPRECISION big,dum,pivinv	
	PARAMETER (NMAX=50)
	INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX) 
C The integer arrays ipiv, indxr, and indxc are used for bookkeeping on the pivoting. 
C	Linear equation solution by Gauss-Jordan elimination, equation (2.1.1) above. a(1:n,1:n)
C	is an input matrix stored in an array of physical dimensions np by np. b(1:n,1:m) is an input
C	matrix containing the m right-hand side vectors, stored in an array of physical dimensions
C	np by mp. On output, a(1:n,1:n) is replaced by its matrix inverse, and b(1:n,1:m) is
C	replaced by the corresponding set of solution vectors.
C	Parameter: NMAX is the largest anticipated value of n.
	do 11 j=1,n
	ipiv(j)=0
   11	enddo
	do 22 i=1,n ! This is the main loop over the columns to be reduced.
	big=0.
	do 13 j=1,n ! This is the outer loop of the search for a pivot element.
	if(ipiv(j).ne.1)then
	do 12 k=1,n
	if (ipiv(k).eq.0) then
	if (abs(a(j,k)).ge.big) then
	big=abs(a(j,k))
	irow=j
	icol=k
	endif
	endif
   12	enddo
	endif
   13	enddo
	ipiv(icol)=ipiv(icol)+1
	if (irow.ne.icol) then
	do 14 l=1,n
	dum=a(irow,l)
	a(irow,l)=a(icol,l)
	a(icol,l)=dum
   14	enddo
	do 15 l=1,m
	dum=b(irow,l)
	b(irow,l)=b(icol,l)
	b(icol,l)=dum
   15	enddo
	endif
	indxr(i)=irow ! We are now ready to divide the pivot row by the pivot element, located at irow and icol. 
	indxc(i)=icol
	if (a(icol,icol).eq.0.) THEN 
C	IF(IERR.NE.1) write(*,*) 'singular matrix in gaussj, skipping fit'
	IERR=1
	GOTO 999
	ENDIF
	pivinv=1./a(icol,icol)
	a(icol,icol)=1.
	do 16 l=1,n
	a(icol,l)=a(icol,l)*pivinv
   16	enddo
	do 17 l=1,m
	b(icol,l)=b(icol,l)*pivinv
   17	enddo
	do 21 ll=1,n ! Next, we reduce the rows...
	if(ll.ne.icol)then !...except for the pivot one, of course.
	dum=a(ll,icol)
	a(ll,icol)=0.
	do 18 l=1,n
	a(ll,l)=a(ll,l)-a(icol,l)*dum
   18	enddo
	do 19 l=1,m
	b(ll,l)=b(ll,l)-b(icol,l)*dum
   19	enddo
	endif
   21	enddo
   22	enddo ! This is the end of the main loop over columns of the reduction.
	do 24 l=n,1,-1 ! It only remains to unscramble the solution in view of the column interchanges. We do this by interchanging pairs of columns in the reverse order that the permutation was built up.
	if(indxr(l).ne.indxc(l)) then
	do 23 k=1,n
	dum=a(k,indxr(l))
	a(k,indxr(l))=a(k,indxc(l))
	a(k,indxc(l))=dum
   23	enddo
	endif
   24	enddo
  999 CONTINUE
	return ! And we are done.
	END

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C	SUBROUTINE COVSRT
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	SUBROUTINE covsrt(covar,npc,ma,ia,mfit)
	DOUBLEPRECISION covar(npc,npc)
	DOUBLEPRECISION swap
	INTEGER ma,mfit,npc,ia(ma)
	INTEGER i,j,k
C	Expand in storage the covariance matrix covar, so as to take into account parameters that
C	are being held .xed. (For the latter, return zero covariances.)
	do 12 i=mfit+1,ma
	do 11 j=1,i
	covar(i,j)=0.
	covar(j,i)=0.
   11	enddo
   12	enddo
	k=mfit
	do 15 j=ma,1,-1
	if(ia(j).ne.0)then
	do 13 i=1,ma
	swap=covar(i,k)
	covar(i,k)=covar(i,j)
	covar(i,j)=swap
   13	enddo
	do 14 i=1,ma
	swap=covar(k,i)
	covar(k,i)=covar(j,i)
	covar(j,i)=swap
   14	enddo
	k=k-1
	endif
   15	enddo
	return
	END


	SUBROUTINE INTERPOLATION_CALIBRATION(SELCIMPROC,DYCAL,DMCAL,DDCAL,
     1	                                 NDIRCAL,DIRCAL,YY,MM,DD,DCAL,NW)

	PARAMETER (KCAL=10,KNW=10)
!	INPUT PARAMETERS
      INTEGER DYCAL(KCAL),DMCAL(KCAL),DDCAL(KCAL)			!CALIBRATION DATE(YY,MM,DD)
	DOUBLE PRECISION DIRCAL(KCAL,KNW)					!CALIBRATION COEFFICIENTS DERIVED AT CALIBRATION DATE
	INTEGER YY,MM,DD									!OBSERVATION DATE
	INTEGER SELCIMPROC
	INTEGER DAYSXMONTH(12),DCALDAY(KCAL)

!	OUTPUT PARAMETERS
	DOUBLE PRECISION DCAL(KNW)							!CALIBRATION COEFFICIENTS AT MEASUREMENT DATE

      DATA DAYSXMONTH/0,31,28,31,30,31,30,31,31,30,31,30/

      IDAY=0
      DO J=1,MM
		IDAY=IDAY+DAYSXMONTH(J)
      ENDDO
      IDAY=IDAY+DD
      ICALDAY=IDAY+(YY+2000-1990)*365	!OBSERVATION DAY

      IF(SELCIMPROC.EQ.0) THEN
		DO I=1,NW
			DCAL(I)=DIRCAL(1,I)
		ENDDO
      ELSE
		DO I=1,NDIRCAL
			IDAY=0
			DO J=1,DMCAL(I)
				IDAY=IDAY+DAYSXMONTH(J)
			ENDDO
			IDAY=IDAY+DDCAL(I)
			DCALDAY(I)=IDAY+(DYCAL(I)+2000-1990)*365 !CALIBRATION DAY
		ENDDO
		IF(ICALDAY.LE.DCALDAY(1)) THEN
			DO I=1,NW
				DCAL(I)=DIRCAL(1,I)
			ENDDO
			ELSE IF(ICALDAY.GE.DCALDAY(NDIRCAL)) THEN
			DO I=1,NW
				DCAL(I)=DIRCAL(NDIRCAL,I)
			ENDDO
		ELSE
			XO=ICALDAY
			IEXP1=1
			DO WHILE(XO.GE.DCALDAY(IEXP1))
				IEXP1=IEXP1+1
			ENDDO
			IEXP1=IEXP1-1
			IEXP2=IEXP1+1
			X1=DCALDAY(IEXP1)
			X2=DCALDAY(IEXP2)
			DO J=1,NW
				Y1=DIRCAL(IEXP1,J)
				Y2=DIRCAL(IEXP2,J)
				DCAL(J)=Y1+(XO-X1)*(Y2-Y1)/(X2-X1)
			ENDDO
		ENDIF
      ENDIF
	RETURN
	END

	SUBROUTINE RDPAR(FIPT,ALNGS,LATDEG,LONDEG,HGTASL,NW9,WVL9,IFLT9,IWVL9,
     1   SELCIMPROC,SERIALNUMBER,AURSKY,NDIRCAL,DYCAL,DMCAL,DDCAL,
	1   DIRCALDATE9,DIRCALTIME9,DIRCAL9,NAURCAL,NSKYCAL,
     1   AYCAL,AMCAL,ADCAL,AHHCAL,AMMCAL,
	1   SYCAL,SMCAL,SDCAL,SHHCAL,SMMCAL,
	1   AURCALDATE9,AURCALTIME9,AURCAL9,SKYCAL9,STNS_ID)
	CHARACTER FIPT*100
	PARAMETER (KCAL=10,KNW=10)
	INTEGER NW9,IFLT9(KNW),IWVL9(KNW),SELCIMPROC,SERIALNUMBER,AURSKY
	INTEGER NDIRCAL,DYCAL(KCAL),DMCAL(KCAL),DDCAL(KCAL)
	INTEGER NAURCAL,NSKYCAL,AYCAL(KCAL),AMCAL(KCAL),ADCAL(KCAL)
	INTEGER AMMCAL(KCAL),AHHCAL(KCAL)
      INTEGER SYCAL(KCAL),SMCAL(KCAL),SDCAL(KCAL)
      INTEGER SHHCAL(KCAL),SMMCAL(KCAL)
	CHARACTER HEADER*3,STNS_ID*100
      CHARACTER*6 DIRCALDATE9(KCAL),DIRCALTIME9(KCAL)
      CHARACTER*6 AURCALDATE9(KCAL),AURCALTIME9(KCAL)
      CHARACTER*6 SKYCALDATE9(KCAL),SKYCALTIME9(KCAL)
      DOUBLE PRECISION ALNGS,LATDEG,LONDEG,HGTASL,WVL9(KNW) 
      DOUBLE PRECISION DIRCAL9(KCAL,KNW),DIRCAL(KCAL,KNW),DCAL(KNW)
      DOUBLE PRECISION AURCAL9(KCAL,KNW),AURCAL(KCAL,KNW),ACAL(KNW)
      DOUBLE PRECISION SKYCAL9(KCAL,KNW),SKYCAL(KCAL,KNW),SCAL(KNW)

!     Reads instrumental file 
      OPEN(UNIT=10,FILE=TRIM(FIPT)//'ceform_'//TRIM(STNS_ID)//'.par',
     1	STATUS='OLD')
      READ(10,*) HEADER
c     che UTC 2013-6-11ccc
C     加快速度所以不显示！
C      write(*,*)HEADER
      READ(10,*) ALNGS,LATDEG,LONDEG,HGTASL
c     CHE 测试UTC 2013-6-11
C      write(*,*)LATDEG,LONDEG
      READ(10,*) NW9
C      write(*,*)NW9
      READ(10,*) (WVL9(I),I=1,NW9) !wavelength (microns)
      READ(10,*) (IFLT9(I),I=1,NW9)!Filter number
      READ(10,*) (IWVL9(I),I=1,NW9)!Filter flags
      READ(10,*) SELCIMPROC
      READ(10,*) SERIALNUMBER
      READ(10,*) AURSKY !Radiance calibration source (0:both, 1:aur, 2:sky)
      READ(10,*) HEADER
      READ(10,*) NDIRCAL !Direct calibration number
      DO I=1,NDIRCAL
      READ(10,1998) DYCAL(I),DMCAL(I),DDCAL(I) !Direct calibration time (yymmdd)
      BACKSPACE(10)
      READ(10,*) DIRCALDATE9(I),DIRCALTIME9(I),(DIRCAL9(I,J),J=1,NW9) !Direct calibration time and calibration coefficient
      ENDDO
      READ(10,*) HEADER
      READ(10,*) NAURCAL ! Radiance calibration number
      DO I=1,NAURCAL
      READ(10,1998) AYCAL(I),AMCAL(I),ADCAL(I),AHHCAL(I),AMMCAL(I)
      BACKSPACE(10)
      READ(10,*) AURCALDATE9(I),AURCALTIME9(I),
     1(AURCAL9(I,J),J=1,NW9),(SKYCAL9(I,J),J=1,NW9) !Read aureole and sky calibration coefficients
      ENDDO
      CLOSE(10)
 1005 FORMAT(A6,9E8.3)
 1998 FORMAT(3I2,' ',2I2)
      NSKYCAL=NAURCAL !sky calibration number = aureole calibration number
!	sky calibration time = aureole calibration number
      DO I=1,NAURCAL
		SYCAL(I)=AYCAL(I)
		SMCAL(I)=AMCAL(I)
		SDCAL(I)=ADCAL(I)
		SHHCAL(I)=AHHCAL(I)
		SMMCAL(I)=AMMCAL(I)
		SKYCALDATE9(I)=AURCALDATE9(I)
		SKYCALTIME9(I)=AURCALTIME9(I)
      ENDDO
	RETURN
	END

	SUBROUTINE sortX(NBR,LONDEG,YYR,MMR,DDR,HR,MR,SR,NFILTROR,
     1				 RVDIR,RVAUR,RVSKY,RTEMP,HSR)
	PARAMETER(KNBR=1000)
	INTEGER YYR(KNBR),MMR(KNBR),DDR(KNBR)
	INTEGER HR(KNBR),MR(KNBR),SR(KNBR),NFILTROR(KNBR)
	INTEGER RVDIR(KNBR),RVAUR(5,KNBR),RVSKY(24,KNBR)
	INTEGER HSR(KNBR),IXR(KNBR)
	DOUBLE PRECISION RTEMP(KNBR),LONDEG
	 

      DO I=1,NBR
		IF(HR(I)+INT(LONDEG/15).GT.24)THEN
			HSR(I)=(HR(I)+INT(LONDEG/15)-24)*3600+MR(I)*60+SR(I)
		ELSE
			HSR(I)=(HR(I)+INT(LONDEG/15))*3600+MR(I)*60+SR(I)
		ENDIF
      ENDDO
	CALL SORT(NBR,HSR,IXR)
	HSR(1:NBR)=HSR(IXR)

	CALL SELECT_OBS(YYR,MMR,DDR,HR,MR,SR,NBR,IXR,
     1                      NFILTROR,RVDIR,RVAUR,RVSKY,RTEMP)

	RETURN
	END

      SUBROUTINE SORT(NX,X,IX)
C SORTING OF X1,X2,... -> Y1<=Y2<=Y3...
C Y(I)=X(IX(I))
C IX: LABEL(ORDER)
C$ENDI
      INTEGER X(NX),IX(NX)
      DO 1 I=1,NX
    1 IX(I)=I
      IF(NX.LE.1) RETURN
      DO 2 I=1,NX-1
      DO 3 J=I+1,NX
      IF(X(IX(J))-X(IX(I))) 4,3,3
    4 IX1=IX(I)
      IX(I)=IX(J)
      IX(J)=IX1
    3 CONTINUE
    2 CONTINUE
      RETURN
      END

	SUBROUTINE PAIR(NBR,YYR,MMR,DDR,HR,MR,SR,
     1	            NFILTROR,RVDIR,RVAUR,RVSKY,RTEMP,HSR,
     1                NBL,YYL,MML,DDL,HL,ML,SL,
     1                NFILTROL,LVDIR,LVAUR,LVSKY,LTEMP,HSL)
      PARAMETER(KNBR=1000)
	INTEGER YYR(KNBR),MMR(KNBR),DDR(KNBR)
	INTEGER YYL(KNBR),MML(KNBR),DDL(KNBR)
	
	INTEGER HR(KNBR),MR(KNBR),SR(KNBR),NFILTROR(KNBR),RVDIR(KNBR)
	INTEGER RVAUR(5,KNBR),RVSKY(24,KNBR),HSR(KNBR),NBR

	INTEGER HL(KNBR),ML(KNBR),SL(KNBR),NFILTROL(KNBR),LVDIR(KNBR)
	INTEGER LVAUR(5,KNBR),LVSKY(24,KNBR),HSL(KNBR),NBL

	INTEGER IXR(KNBR),IXL(KNBR)
	DOUBLE PRECISION RTEMP(KNBR),LTEMP(KNBR)

!	左右扫描间隔小于60s，则表示扫描正常，接受，否则剔除		
      II=0 ! accepts paired branches
      DO I=1,NBR 
		HDIFMIN=10E+6
		DO J=1,NBL
			IF(NFILTROR(I).EQ.NFILTROL(J)) THEN
				HDIF=ABS(HSR(I)-HSL(J))
				IF(HDIF.LE.HDIFMIN) THEN
					HDIFMIN=HDIF
					JMIN=J
				ENDIF
			ENDIF
		ENDDO
		IF(HDIFMIN.LE.90) THEN !time difference between right and left scanning < 60s
			II=II+1
			IXR(II)=I
			IXL(II)=JMIN
		ENDIF
      ENDDO

      NBR=II
      NBL=NBR

	CALL SELECT_OBS(YYR,MMR,DDR,HR,MR,SR,NBR,IXR,
     1                      NFILTROR,RVDIR,RVAUR,RVSKY,RTEMP)
	CALL SELECT_OBS(YYL,MML,DDL,HL,ML,SL,NBR,IXR,
     1                      NFILTROL,LVDIR,LVAUR,LVSKY,LTEMP)
	
	
	RETURN
	END

	SUBROUTINE GET_DAILY_ALM(FILER,CDATA,NBR,
     &	 NFILTROL,DDL,MML,YYL,HL,ML,SL,LVDIR,LVAUR,LVSKY,LTEMP,LONDEG)

	PARAMETER(KNBR=1000000)
	DOUBLE PRECISION LONDEG
	INTEGER NBR,NBL

!	TEMPORARY DIMENSION USED TO LOAD MEASUREMENTS	
	INTEGER DDR(KNBR),MMR(KNBR),YYR(KNBR)
	INTEGER HR(KNBR),MR(KNBR),SR(KNBR)
	INTEGER RVDIR(KNBR),RVAUR(5,KNBR),RVSKY(24,KNBR)
	INTEGER IXR(KNBR),NFILTROR(KNBR)
	DOUBLE PRECISION RTEMP(KNBR)
	INTEGER DDT(KNBR),MMT(KNBR),YYT(KNBR)

!	OUTPUT DIMENSION
	INTEGER DDL(KNBR/10),MML(KNBR/10),YYL(KNBR/10)
	INTEGER HL(KNBR/10),ML(KNBR/10),SL(KNBR/10)
	INTEGER LVDIR(KNBR/10),LVAUR(5,KNBR/10),LVSKY(24,KNBR/10)
	INTEGER IXL(KNBR/10),NFILTROL(KNBR/10)
	DOUBLE PRECISION LTEMP(KNBR/10)

	CHARACTER CDATA*6
	INTEGER YY,MM,DD
	CHARACTER FILER*100,FILEL*100

      CHARACTER*1 LINE(500)
      CHARACTER*500 STRING
      EQUIVALENCE (LINE,STRING)


	READ(CDATA,1000) YY,MM,DD 
 1000	FORMAT(3I2)


      OPEN(UNIT=10,FILE=TRIM(FILER),STATUS='OLD')
C      READ(10,1007) HDR
C      IF(HDR.EQ.'1'.OR.HDR.EQ.'2'.OR.HDR.EQ.'3'.OR.HDR.EQ.'4'.OR.
C     1HDR.EQ.'5'.OR.HDR.EQ.'6'.OR.HDR.EQ.'7'.OR.HDR.EQ.'8'.OR.
C     1HDR.EQ.'9'.OR.HDR.EQ.'0') THEN 
C      REWIND(10)
C      ENDIF
!	Some data with value of !!!! WHICH IS REPLACED BY 9999
      II=0
      DO 111 I=1,KNBR
		READ(10,1010,END=1011) LINE
		DO K=1,500
	   	  IF(LINE(K).EQ.'/'.OR.LINE(K).EQ.':'.OR.LINE(K).EQ.',') THEN
			LINE(K)=' '
		  ENDIF
		  IF(LINE(K).EQ.'!')THEN
			LINE(K)='9'
		  ENDIF
		ENDDO
		IF(LINE(1).EQ.'D')GOTO 111
		II=II+1
	
		READ(STRING,*) DDR(II),MMR(II),YYR(II),HR(II),MR(II),SR(II),
     1					NFILTROR(II),
     1	RVDIR(II),(RVAUR(J,II),J=1,5),(RVSKY(J,II),J=1,24),RTEMP(II) 
!Direct irradiance, aureole radiance at 5 angles and sky radiance at 24 angels
c     车慧正测试UTC 2013-6-11
C		write(*,*)DDR(II),MMR(II),YYR(II)
		IF(RVDIR(II).EQ.9999)RVDIR(II)=65529
		DO J=1,5
			IF(RVAUR(J,II).EQ.9999)RVAUR(J,II)=65529
		ENDDO
		DO J=1,24
			IF(RVSKY(J,II).EQ.9999)RVSKY(J,II)=65529
		ENDDO
 111  ENDDO
 1011 CLOSE(10)
 1010 FORMAT(500A1)
      
      NB=II

!	TO TRANSFORM UTC TO LTC IN ORDER TO GET REAL DAILY OBSERVATIONS
	CALL UTC_LTC(HR,YYR,MMR,DDR,YYT,MMT,DDT,LONDEG)
	NBR=0
	NUM=0
	DO I=1,KNBR
		IF(YYT(I).EQ.2000+YY.AND.MMT(I).EQ.MM.AND.DDT(I).EQ.DD)THEN
			NBR=NBR+1
			NUM=NUM+1
			IXR(NBR)=I
		ENDIF
	ENDDO	


	YYL(1:NBR)=YYR(IXR(1:NBR))
	MML(1:NBR)=MMR(IXR(1:NBR))
	DDL(1:NBR)=DDR(IXR(1:NBR))
	HL(1:NBR)=HR(IXR(1:NBR))
	ML(1:NBR)=MR(IXR(1:NBR))
	SL(1:NBR)=SR(IXR(1:NBR))
	NFILTROL(1:NBR)=NFILTROR(IXR(1:NBR))
	LVDIR(1:NBR)=RVDIR(IXR(1:NBR))
	LVAUR(:,1:NBR)=RVAUR(:,IXR(1:NBR))
	LVSKY(:,1:NBR)=RVSKY(:,IXR(1:NBR))
	LTEMP(1:NBR)=RTEMP(IXR(1:NBR))
	

	RETURN
C	STOP
	END



	SUBROUTINE UTC_LTC(HR,YYR,MMR,DDR,YYL,MML,DDL,LONDEG)
!	TO GET DAILY ALMUCANTAR SCANNING MEASUREMENT DATA GIVEN THE DATE IS PROVIDED
!	UTC transformed to LTC when selecting daily data
!	INPUT : YYR,MMR,DDR,HR UTC(YEAR,MONTH,DAY,HOUR)
!	OUTPUT: YYL,MML,DDL,   LTC(YEAR,MONTH,DAY)

	PARAMETER(KNBR=10000)
	DOUBLE PRECISION LONDEG
	INTEGER HR(KNBR),YYR(KNBR),MMR(KNBR),DDR(KNBR)
	INTEGER	YYL(KNBR),MML(KNBR),DDL(KNBR)
	INTEGER NMON1(12),NMON2(12),JUL1(365,3),JUL2(366,3)
	INTEGER THH(KNBR) !TEMPORARY Dimensions used to save 
	DATA NMON1/31,28,31,30,31,30,31,31,30,31,30,31/
	DATA NMON2/31,29,31,30,31,30,31,31,30,31,30,31/

	K=0
	DO I=1,12
		DO J=1,NMON1(I)
			K=K+1
			JUL1(K,1)=I
			JUL1(K,2)=J
			JUL1(K,3)=K
		ENDDO
	ENDDO

	K=0
	DO I=1,12
		DO J=1,NMON2(I)
			K=K+1
			JUL2(K,1)=I
			JUL2(K,2)=J
			JUL2(K,3)=K
		ENDDO
	ENDDO

	DO I=1,KNBR
C	DO I=7,7
		THH(I)=HR(I)+INT(LONDEG/15)
		YYL(I)=YYR(I)
		MML(I)=MMR(I)
		DDL(I)=DDR(I)
		IF(THH(I).GT.24)THEN
			IF(MMR(I).EQ.12 .AND. DDR(I).EQ.31)THEN
C				WRITE(*,*)YYR(I),MMR(I),DDR(I)
				MML(I)=1
				DDL(I)=1
				YYL(I)=YYR(I)+1
C				WRITE(*,*)YYR(I),MMR(I),DDR(I)
			ELSE
			    LEAPFLAG = 0
				IF ((MOD(YYR(I),4) .EQ. 0 .AND. MOD(YYR(I),100) .NE.0) 
     1			.OR. MOD(YYR(I),400) .EQ. 0)  LEAPFLAG = 1
C	IF (MOD(YYR(I),100) .EQ. 0) LEAPFLAG = 0
C	IF (MOD(YYR(I),400) .EQ. 0) LEAPFLAG = 1
				IF(LEAPFLAG.EQ.0)THEN
					DO IP=1,365
						IF(JUL1(IP,1).EQ.MMR(I).AND.JUL1(IP,2).EQ.DDR(I))THEN
C							JPOS=IP
C							WRITE(*,*)MMR(I),DDR(I)
							YYL(I)=YYR(I)
							MML(I)=JUL1(IP+1,1)
							DDL(I)=JUL1(IP+1,2)
C							WRITE(*,*)MMR(I),DDR(I)
							GOTO 99
						ENDIF
					ENDDO
				ELSE
					DO IP=1,366
						IF(JUL2(IP,1).EQ.MMR(I).AND.JUL2(IP,2).EQ.DDR(I))THEN
C							JPOS=IP
C							WRITE(*,*)MMR(I),DDR(I)
							YYL(I)=YYR(I)
							MML(I)=JUL2(IP+1,1)
							DDL(I)=JUL2(IP+1,2)
C							WRITE(*,*)MMR(I),DDR(I)
							GOTO 99
						ENDIF
					ENDDO
				ENDIF
			ENDIF
		ENDIF
99		CONTINUE
	ENDDO
	RETURN
	END

	SUBROUTINE CHKALM(NBR,YYR,MMR,DDR,HR,MR,SR,NFILTROR,
     1	              RVDIR,RVAUR,RVSKY,RTEMP,HSR,
     1                  NBL,YYL,MML,DDL,HL,ML,SL,NFILTROL,
     1                  LVDIR,LVAUR,LVSKY,LTEMP,HSL,NW9)
      PARAMETER(KNBR=1000)
	INTEGER YYR(KNBR),MMR(KNBR),DDR(KNBR)
	INTEGER YYL(KNBR),MML(KNBR),DDL(KNBR)
	INTEGER HR(KNBR),MR(KNBR),SR(KNBR),NFILTROR(KNBR),RVDIR(KNBR)
	INTEGER RVAUR(5,KNBR),RVSKY(24,KNBR),HSR(KNBR),NBR

	INTEGER HL(KNBR),ML(KNBR),SL(KNBR),NFILTROL(KNBR),LVDIR(KNBR)
	INTEGER LVAUR(5,KNBR),LVSKY(24,KNBR),HSL(KNBR),NBL

	INTEGER IFLG(KNBR),IXR(KNBR),IXL(KNBR),NW9

	DOUBLE PRECISION RTEMP(KNBR),LTEMP(KNBR)
	

!	确定每个动作中所有波段扫描均已完成，否则剔除本次动作所有观测   
!	如果两次扫描时间间隔小于120s，则表示属同一扫描（不同波段）
c	right almucantar
      I=0
      ICH=0
      DO WHILE(I.LT.NBR)
		I=I+1
		IF(ABS(HSR(I)-HSR(I+1)).LE.120) THEN ! 120s 
			ICH=ICH+1
		ELSE
			IF(ICH.GT.NW9-1) THEN	!MORE ALMUCANTAR SCANNING, DELETE 
				DO J=I-ICH,I
					NDEL=0
					DO K=J,I
						IF(NFILTROR(J).EQ.NFILTROR(K))NDEL=NDEL+1
					ENDDO
					IF(NDEL.EQ.1)THEN
						IFLG(J)=1
					ELSE
						IFLG(J)=-1
					ENDIF
				ENDDO
			ENDIF
			IF(ICH.EQ.NW9-1) THEN	!ALMUCANTAR OK
				DO J=I-ICH,I
					IFLG(J)=1
				ENDDO
			ENDIF
			IF(ICH.LT.NW9-1) THEN	!LESS ALMUCANTAR SCANNING
				DO J=I-ICH,I
					IFLG(J)=-1
				ENDDO 
			ENDIF
			ICH=0
		ENDIF
      ENDDO

      II=0 
      DO I=1,NBR 
		IF(IFLG(I).EQ.1) THEN
			II=II+1
			IXR(II)=I
		ENDIF
		IFLG(I)=0
      ENDDO

      NBR=II


c	left almucantar
      I=0
      ICH=0
      DO WHILE(I.LT.NBL)
		I=I+1
		IF(ABS(HSL(I)-HSL(I+1)).LE.120) THEN ! 120s 
			ICH=ICH+1
		ELSE
			IF(ICH.GT.NW9-1) THEN	!MORE ALMUCANTAR SCANNING, DELETE 
				DO J=I-ICH,I
					NDEL=0
					DO K=J,I
						IF(NFILTROL(J).EQ.NFILTROL(K))NDEL=NDEL+1
					ENDDO
					IF(NDEL.EQ.1)THEN
						IFLG(J)=1
					ELSE
						IFLG(J)=-1
					ENDIF
				ENDDO
			ENDIF
			IF(ICH.EQ.NW9-1) THEN	!ALMUCANTAR OK
				DO J=I-ICH,I
					IFLG(J)=1
				ENDDO
			ENDIF
			IF(ICH.LT.NW9-1) THEN	!LESS ALMUCANTAR SCANNING
				DO J=I-ICH,I
					IFLG(J)=-1
				ENDDO 
			ENDIF
			ICH=0
		ENDIF
      ENDDO

      II=0 
      DO I=1,NBL 
		IF(IFLG(I).EQ.1) THEN
			II=II+1
			IXL(II)=I
		ENDIF
		IFLG(I)=0
      ENDDO

      NBL=II

	CALL SELECT_OBS(YYR,MMR,DDR,HR,MR,SR,NBR,IXR,
     1                      NFILTROR,RVDIR,RVAUR,RVSKY,RTEMP)
	CALL SELECT_OBS(YYL,MML,DDL,HL,ML,SL,NBL,IXL,
     1                      NFILTROL,LVDIR,LVAUR,LVSKY,LTEMP)
	HSR(1:NBR)=HSR(IXR)
	HSL(1:NBL)=HSL(IXL)

	RETURN
	END

	SUBROUTINE TOA(FIPT,FTOP,H2OABS,MIXGABS,O3ABS,NO2ABS,WVL,NW)
	PARAMETER(KNBR=1000,KNW=10)
	CHARACTER HEADER,FIPT*100
      DOUBLE PRECISION WVLTOA(2000),FOTOA(2000),FTOP(KNW)
      DOUBLE PRECISION MIXGTOA(2000),O3TOA(2000)
	DOUBLE PRECISION H2OTOA(2000),NO2TOA(2000)
      DOUBLE PRECISION O3ABS(KNBR),H2OABS(KNBR)
	DOUBLE PRECISION NO2ABS(KNBR),MIXGABS(KNBR),WVL(KNW)

      OPEN(UNIT=10,FILE=TRIM(FIPT)//'toa.dat',STATUS='OLD')
      READ(10,*) HEADER
      DO I=1,2000
		READ(10,*,END=9998) WVLTOA(I),FOTOA(I),
     1	H2OTOA(I),MIXGTOA(I),O3TOA(I),NO2TOA(I)
		WVLTOA(I)=WVLTOA(I)*1E-3
      ENDDO
 9998 CLOSE(10)
 
!     Selects corresponding irradiances for the used channels
      DO I=1,NW
		XO=WVL(I)
		IEXP1=1
		DO WHILE(XO.GE.WVLTOA(IEXP1))
			IEXP1=IEXP1+1
		ENDDO
		IEXP1=IEXP1-1
		IEXP2=IEXP1+1
		X1=WVLTOA(IEXP1)
		X2=WVLTOA(IEXP2)
		Y1=FOTOA(IEXP1)
		Y2=FOTOA(IEXP2)
		FTOP(I)=Y1+(XO-X1)*(Y2-Y1)/(X2-X1)
		Y1=H2OTOA(IEXP1)
		Y2=H2OTOA(IEXP2)
		H2OABS(I)=Y1+(XO-X1)*(Y2-Y1)/(X2-X1)
		Y1=MIXGTOA(IEXP1)
		Y2=MIXGTOA(IEXP2)
		MIXGABS(I)=Y1+(XO-X1)*(Y2-Y1)/(X2-X1)
		Y1=O3TOA(IEXP1)
		Y2=O3TOA(IEXP2)
		O3ABS(I)=Y1+(XO-X1)*(Y2-Y1)/(X2-X1)
		Y1=NO2TOA(IEXP1)
		Y2=NO2TOA(IEXP2)
		NO2ABS(I)=Y1+(XO-X1)*(Y2-Y1)/(X2-X1)
	ENDDO
	RETURN
	END


	SUBROUTINE AURSKYRAD(AURSKY,IFLAG,NBR,NW,
     1	                 RVAUR,RVSKY,LVAUR,LVSKY,ACAL,SCAL,ALMA,ALMB)

	PARAMETER(KNBR=1000,KNW=10)
C     左右对称度，设置阈值，original 原来(SIMCRIT=15.)，Now: 现在更加严格控制为(SIMCRIT=10.) CHE HUIZHENG 2013-6-13
      PARAMETER(SIMCRIT=10.)
      INTEGER RVAUR(5,KNBR),RVSKY(24,KNBR)
	INTEGER LVAUR(5,KNBR),LVSKY(24,KNBR)
      DOUBLE PRECISION ALMA(29,KNBR),ALMB(29,KNBR)
	DOUBLE PRECISION ACAL(KNW),SCAL(KNW)
	INTEGER AURSKY,IFLAG(29,KNBR)
      DOUBLE PRECISION RMSDI,RMSD(KNBR),SAVG,SMDV(KNBR)

      DO I=1,NBR
		DO K1=1,5
			ALMA(K1,I)=RVAUR(K1,I)
			ALMB(K1,I)=LVAUR(K1,I)
		ENDDO
		DO K2=1,24
			ALMA(K2+5,I)=RVSKY(K2,I)
			ALMB(K2+5,I)=LVSKY(K2,I)
		ENDDO
      ENDDO

      DO I=1,NBR
		DO K=1,29
			IFLAG(K,I)=1
CC    tEST
CC	write(*,*)IFLAG(K,I),'k=',k,'i=',i
		ENDDO
      ENDDO

!     Filter for measurements with zero values
      DO I=1,NBR
		DO K=1,29
			IF(ALMA(K,I).EQ.0.OR.ALMB(K,I).EQ.0) THEN
				ALMA(K,I)=9.99
				ALMB(K,I)=9.99
				IFLAG(K,I)=0
			ENDIF
		ENDDO
      ENDDO

!     Filter for saturated measurements
      DO I=1,NBR
		DO J=1,29
			IF(ALMA(J,I).GE.65529) THEN
				IFLAG(J,I)=2
				ALMA(J,I)=9.99 
			ENDIF
			IF(ALMB(J,I).GE.65529) THEN
				IFLAG(J,I)=2 
				ALMB(J,I)=9.99 
			ENDIF
		ENDDO
      ENDDO
	
      AURSUM=0.
      SKYSUM=0.
      DO L=1,NW
		AURSUM=AURSUM+ACAL(L)
		SKYSUM=SKYSUM+SCAL(L)
      ENDDO
      IF(AURSUM.EQ.0.AND.AURSKY.NE.2) THEN
		WRITE(*,*) 'Warning! AUR cal is zero, using SKY cal instead'  
		AURSKY=2
	ELSE IF(SKYSUM.EQ.0.AND.AURSKY.NE.1) THEN 
		WRITE(*,*) 'Warning! SKY cal is zero, using AUR cal instead'  
		AURSKY=1
      ENDIF

!     If aur or sky cals are not available, matches both at 6 degrees
!     (a double aur/sky measurement is measured at this angle)
      IF(AURSKY.EQ.1) THEN
		DO I=1,NBR
			DO J=6,29
				IF(IFLAG(5,I).EQ.2.OR.IFLAG(6,I).EQ.2) THEN
					IFLAG(J,I)=2 
					ALMA(J,I)=9.99
					ALMB(J,I)=9.99
				ELSE IF(ALMA(6,I).EQ.0.AND.ALMB(6,I).EQ.0) THEN
					IFLAG(J,I)=0
					ALMA(J,I)=9.99
					ALMB(J,I)=9.99
				ELSE 
					ALMA(J,I)=ALMA(J,I)*ALMA(5,I)/ALMA(6,I)
					ALMB(J,I)=ALMB(J,I)*ALMB(5,I)/ALMB(6,I)
				ENDIF
			ENDDO
		ENDDO
		DO L=1,NW
			SCAL(L)=ACAL(L)
		ENDDO
	ELSE IF(AURSKY.EQ.2) THEN
		DO I=1,NBR
			DO J=1,5
				IF(IFLAG(5,I).EQ.2.OR.IFLAG(6,I).EQ.2) THEN
					IFLAG(J,I)=2
					ALMA(J,I)=9.99
					ALMB(J,I)=9.99
				ELSE IF(ALMA(5,I).EQ.0.AND.ALMB(5,I).EQ.0) THEN
					IFLAG(J,I)=0
					ALMA(J,I)=9.99
					ALMB(J,I)=9.99
				ELSE 
					ALMA(J,I)=ALMA(J,I)*ALMA(6,I)/ALMA(5,I)
					ALMB(J,I)=ALMB(J,I)*ALMB(6,I)/ALMB(5,I)
				ENDIF
			ENDDO
		ENDDO
		DO L=1,NW
			ACAL(L)=SCAL(L)
		ENDDO
      ENDIF

!     Applies calibration coefficients
      DO I=1,NBR
		L=I-((I-1)/NW)*NW
		DO J=1,5
			ALMA(J,I)=ALMA(J,I)*ACAL(L)
			ALMB(J,I)=ALMB(J,I)*ACAL(L)
		ENDDO
		DO J=6,29
			ALMA(J,I)=ALMA(J,I)*SCAL(L)
			ALMB(J,I)=ALMB(J,I)*SCAL(L)
		ENDDO
      ENDDO

c     Checking for the degree 6 match aur/sky
      DO I=1,NBR      
		MATCH=(ALMA(5,I)-ALMA(6,I))/ALMA(5,I)*100.
		IF(MATCH.GE.1.0) THEN
			WRITE(*,*) 'Warning!: Aur & Sky do not match (>1%):',MATCH
		ENDIF
      ENDDO
!-------------------------------------------------------------------------
!     The following parts should be corrected by VICTOR!!!!!!! Maybe also other related parts?? 
!-------------------------------------------------------------------------
!     Simmetry filter over radiance in the almucantar (Holben, 1998)
!-------------------------------------------------------------------------
      DO I=1,NBR
		RMSD(I)=0.
      ENDDO
      DO I=1,NBR 
		SAVG=0.
		NALIVE=0
		DO J=1,29
			IF(IFLAG(J,I).EQ.1.AND.J.NE.6) THEN
				XMED=0.5*(ALMA(J,I)+ALMB(J,I))
				RMSDI=ABS(ALMA(J,I)-ALMB(J,I))/XMED
				IF(RMSDI.GT.SIMCRIT/100.) THEN
					IFLAG(J,I)=0
	
				ELSE 
					SAVG=SAVG+XMED
					RMSDI=RMSDI**2.
					RMSD(I)=RMSD(I)+RMSDI
					NALIVE=NALIVE+1
				ENDIF
			ENDIF
		ENDDO
		IF(NALIVE.NE.0.0) THEN
			RMSD(I)=RMSD(I)/(NALIVE*1.0)
			RMSD(I)=100.*RMSD(I)**0.5
			SAVG=SAVG/(NALIVE*1.0)
			SMDV(I)=RMSD(I)/SAVG
		ENDIF
      ENDDO

	DO I=1,NBR
		IF(IFLAG(5,I).EQ.0) THEN 
			IFLAG(6,I)=0
		ENDIF
      ENDDO

      DO I=1,NBR
		IF(RMSD(I).GE.SIMCRIT) THEN
			DO J=1,29
				IFLAG(J,I)=0 
			ENDDO
		ENDIF
      ENDDO

	RETURN
	END

	SUBROUTINE SELECT_OBS(YYR,MMR,DDR,HR,MR,SR,NBR,IXR,
     1                      NFILTROR,RVDIR,RVAUR,RVSKY,RTEMP)
	PARAMETER(KNBR=1000)
	INTEGER YYR(KNBR),MMR(KNBR),DDR(KNBR)
	INTEGER HR(KNBR),MR(KNBR),SR(KNBR),NFILTROR(KNBR),IXR(KNBR)
	INTEGER RVDIR(KNBR),RVAUR(5,KNBR),RVSKY(24,KNBR),NBR
	DOUBLE PRECISION RTEMP(KNBR)	

!	function: to derive IXR measurements from total NBR measurements

	YYR(1:NBR)=YYR(IXR)
	MMR(1:NBR)=MMR(IXR)
	DDR(1:NBR)=DDR(IXR)
	HR(1:NBR)=HR(IXR)
	MR(1:NBR)=MR(IXR)
	SR(1:NBR)=SR(IXR)
	NFILTROR(1:NBR)=NFILTROR(IXR)
	RVDIR(1:NBR)=RVDIR(IXR)
	RVAUR(:,1:NBR)=RVAUR(:,IXR)
	RVSKY(:,1:NBR)=RVSKY(:,IXR)
	RTEMP(1:NBR)=RTEMP(IXR)
	RETURN
	END

	SUBROUTINE SZA(NBR,NBL,MM,DD,HR,MR,SR,HL,ML,SL,LATDEG,LONDEG,
     1	       ZNR,ZNL,AZR,AZL,MASAR,MASAL,MASAO,DST)
	PARAMETER(KNBR=1000)
      PARAMETER (PI=3.141592653589793D0,RAD=PI/180.0)
	INTEGER DAYSXMONTH(12)
	INTEGER HR(KNBR),MR(KNBR),SR(KNBR)
	INTEGER HL(KNBR),ML(KNBR),SL(KNBR),MM,DD
	DOUBLE PRECISION ET,DECL,EO,TST,W0,GAMMA,LATRAD,ELV,DST
	DOUBLE PRECISION ZNR(KNBR),ZNL(KNBR),AZR(KNBR),AZL(KNBR)
	DOUBLE PRECISION MASAR(KNBR),MASAL(KNBR),MASAO(KNBR)
	DOUBLE PRECISION LATDEG,LONDEG
      DATA DAYSXMONTH/0,31,28,31,30,31,30,31,31,30,31,30/

      IDAY=0
      DO J=1,MM
		IDAY=IDAY+DAYSXMONTH(J)
      ENDDO
      IDAY=IDAY+DD

      GAMMA=2.*PI*(IDAY-1)/365.
      LATRAD=LATDEG*PI/180.
      DECL=0.006918-0.399912*dCOS(GAMMA)+0.070257*dSIN(GAMMA)-
     1	0.006758*dCOS(2.*GAMMA)+0.000907*dSIN(2.*GAMMA)-
     1	0.002697*dCOS(3.*GAMMA)+0.00148*dSIN(3.*GAMMA)
      ET=(0.000075+0.001868*dCOS(GAMMA)-0.032077*dSIN(GAMMA)-
     1	0.014615*dCOS(2.*GAMMA)-0.040849*dSIN(2.*GAMMA))*229.18
      EO=1.000110+0.034221*dCOS(GAMMA)+0.001280*dSIN(GAMMA)+0.000719*
     1	dCOS(2*GAMMA)+0.000077*dSIN(2*GAMMA)
      DO I=1,NBR
		TST=HR(I)+LONDEG/15.+(MR(I)+ET)/60.+SR(I)/3600.
		W0=(12.-TST)*15.*PI/180.
		ZNR(I)=dACOS(dSIN(DECL)*dSIN(LATRAD)+
     1			dCOS(DECL)*dCOS(LATRAD)*dCOS(W0))
		ELV=PI/2.-ZNR(I)
		IF((dSIN(ELV)*dSIN(LATRAD)-dSIN(DECL))/
     1			(COS(ELV)*COS(LATRAD)).GE.1)THEN
			AZR(I)=dACOS(DBLE(1.0))
			ELSE
		AZR(I)=dACOS((dSIN(ELV)*dSIN(LATRAD)-dSIN(DECL))/
     1			(COS(ELV)*COS(LATRAD)))
		ENDIF
		IF(W0.LT.0.) AZR(I)=-AZR(I)
      ENDDO
      DO I=1,NBL
		TST=(HL(I)+LONDEG/15.+(ML(I)+ET)/60.+SL(I)/3600.)
		W0=(12.-TST)*15.*PI/180.
		ZNL(I)=dACOS(dSIN(DECL)*dSIN(LATRAD)+
     1			dCOS(DECL)*dCOS(LATRAD)*dCOS(W0))   ! solar zenith angle
		ELV=PI/2.-ZNL(I)							! solar elevation angle
	IF((dSIN(ELV)*dSIN(LATRAD)-dSIN(DECL))/(COS(ELV)*COS(LATRAD)).LE.1)THEN
		AZL(I)=dACOS((dSIN(ELV)*dSIN(LATRAD)-dSIN(DECL))/
     1			(COS(ELV)*COS(LATRAD)))
		ELSE
		AZL(I)=ACOS(1.)
	ENDIF
		IF(W0.LT.0.) AZL(I)=-AZL(I)
      ENDDO
      DST=1./SQRT(EO)

!     Optical mass by Kasten89
      DO I=1,NBR
		MASAR(I)=1./(dCOS(ZNR(I))+0.50572*
     1		(96.07995-ZNR(I)*180./PI)**(-1.6364))
		MASAL(I)=1./(dCOS(ZNL(I))+0.50572*
     1		(96.07995-ZNL(I)*180./PI)**(-1.6364))
		MASAO(I)=0.5*(MASAR(I)+MASAL(I))
      ENDDO
	RETURN
	END

	FUNCTION ILEAP(IYEAR)
	ILEAP=0
	IF((MOD(IYEAR,4).EQ.0.AND.MOD(IYEAR,100).NE.0)
     1	.OR.MOD(IYEAR,400).EQ.0)ILEAP=1
	RETURN
	END

