      SUBROUTINE USMATRIX(key,key_RD,keyEL,keySUB,keyLS
     &                   ,key_org,key_fx,key_grid1
     &                   ,WL,RN,RK,KN,grid
     &                   ,KR,R,RD,KM,ANGLE,dlnr,pomin,pomax
     &                   ,distname_O,distname_F,distname_N,NDP
     R                   ,key_RD1)

! RD version
!c ** ANGLE & PO SPLINE interpolation version
!c ** 12/04/03 f22 interpolation is logarithmic
!c ** 05/05/03 this version can be used to retrieve an aspect ratio
!c ** 13/10/03 IF(ANGLE<=40.)ln(f33)-interpol.
!c **          IF(ANGLE<=50.)ln(f44)-interpol.
!c **************************************************************** c
!c **                                                            ** c
!c ** INPUT:                                                     ** c
!c **                                                            ** c 
!c **   key  = 1 - create fixed kernels (for fixed axis          ** c 
!c **              ratio distr.) and save them                   ** c
!c **              into 'Rke...fix...' files and calculate       ** c
!c **              opt.characteristics                           ** c
!c **          2 - read fixed kernels from 'Rke...fix...'  files ** c
!c **          3 - create fixed kernels but don't save them      ** c
!c **          4 - don't create fixed kernels, calculate         ** c
!c **              opt.characteristics from original kernels     ** c
!c **   key_RD =1 - volume mixture of spheroids                  ** c
!c **           2 - surface area  mixture of spheroids           ** c
!c **   key_org=0 - read original kernels simultaneously make    ** c 
!c **               angle interpolation and calculate opt.char.  ** c
!c **           1 -  -"-, save new kernels in                    ** c
!c **                /distname_N/ directory, STOP                ** c
!c **   key_fx works when key=1                                  ** c
!c **        =0 -  create fixed kernels (for fixed axis          ** c 
!c **              ratio distr.) and save them                   ** c
!c **              into 'Rke...fix...' files and calculate       ** c
!c **              opt.characteristics                           ** c
!c **         1 - save fixed kernels with original kernel format ** c
!c **             in order to be used as input kernels;          ** c
!c **             'Rke...fix' kernels have to be renamed and moved* c
!c **             into directory 'dir_name'(see 'matrix_fixget.f')* c
!c **             The files can not be used if key=2.            ** c
!c **                                                            ** c
!c **   WL   - wavelength                                        ** c
!c **   RN   - real part of the refractive index                 ** c
!c **   RK   - imaginary part of the refractive index            ** c
!c **   rgmin,rgmax &                                            ** c 
!c **   wlmin,wlmax - min,max radii and wlmin,wlmax wavelengths  ** c
!c **                 that are used to recalculate grid radii for** c
!c **                 fixed kernels. New input file              ** c
!c **                'grid1.dat.new' will be created if key=1    ** c
!c **                 or key_org=1. Use key_grid1 to choose      ** c
!c **                 'grid1.dat' or 'grid1.dat.new' will be read** c
!c **                 for further calculations                   ** c  
!c **   key_SD=0 - read Size Distribution table dV/dlnR          ** c
!c **         =1 - calculate Size Distribution for grid radii    ** c
!c **              using Log Normal function                     ** c
!c **   ID    - dimension of d(...)/dlnR or d(...)/dR            ** c
!c **       = 0 - number                                         ** c
!c **       = 1 - radius                                         ** c
!c **       = 2 - area                                           ** c
!c **       = 3 - volume                                         ** c
!c **   NMD   - number of modes (up to 2)                        ** c
!c **   KN   - number of grid radii                              ** c
!c **   grid(KN) - grid radii                                    ** c
!c **   SD(KN)   - size distribution for grid radii              ** c
!c **   (CM(i),SM(i),RMM(i),i=1,NMD) - size distribution         ** c
!c **   function (LogNormal) parameters:                         ** c
!c **                         CM - concentration                 ** c
!c **                         SM - standard deviation            ** c
!c **                         RMM - median radius                ** c
!c **   distname_O - original kernel directory name              ** c
!c **   distname_F - .fix kernel directory name                  ** c
!c **   distname_N - new original kernel directory               ** c
!c **                                      name (key_org=1)      ** c
!c **   KR  - number of axis ratios                              ** c
!c **   R(KR)  - grid axis ratios                                ** c
!c **   RD(KR) - axis ratio distribution for grid axis ratios    ** c
!c **   KM   - number of scattering angles                       ** c
!c **   ANGLE(KM) - scattering angles                            ** c
!c **                                                            ** c
!c ** OUTPUT:                                                    ** c
!c **                                                            ** c
!c **   ext     - extinction                                     ** c
!c **   albedo  - albedo                                         ** c
!c **   f... - scattering matrix elements                        ** c
!c **************************************************************** c
      use alloc
      USE mo_par_DLS
	  
      integer KN, KR, KM, NDP,
     &     key, key_RD, keyEL, keySUB, keyLS, key_org,
     &     key_RD1
      real RN, RK, WAVEL, dlnr
!      real*4 R(KRpar), RATIO(KR1par)
      real R(KRpar), RATIO(KR1par)
      dimension grid(KNpar),RD(KRpar)
      dimension grid1(KN1par)
     &         ,ANGLE1(KMpar), ANGLE(KMpar)
      COMMON /US1/ US11(KMpar,KNpar) 
      COMMON /US2/ US12(KMpar,KNpar)
      COMMON /US3/ US22(KMpar,KNpar)
      COMMON /US4/ US33(KMpar,KNpar)
      COMMON /US5/ US34(KMpar,KNpar)
      COMMON /US6/ US44(KMpar,KNpar)
      COMMON /US0/ USEA(2,KNpar)
      dimension RDc(KR),AA(6),BB(6),AB(6)
      dimension XPO(2), YPO(2), X(2), Y(2)
     &         ,ARE(KREpar), AIM(KIMpar)
      CHARACTER(60) distname_O,distname_F,distname_N
!c      real LINEAR
!cl     &, LINEAR_LN
!c ----- Timer ------
      real*4 tarray(2),T_INT,T_INT0
      real*4 external etime,dtime
      real time_begin, time_end, T_CPU
      real time_begin1, time_end1, T_CPU1
      real*4 T_INT1,T_INT01
	save WAVEL,KN1,grid1,KRE,KIM,ARE,AIM
		
      PI=ACOS(-1.)
      PI2=2.*PI

      IF(NDP.EQ.0) THEN 
		T_INT=0.
		T_CPU=0.
		T_INT1=0.
		T_CPU1=0.						
!c      write(*,*) 'before MATRIX_FIX'
	    if(keySUB.eq.0) then
			T_INT01=dtime(tarray) !+++
			CALL CPU_TIME (time_begin1)
		endif

		CALL MATRIX_FIX(key,key_RD,keyEL,keySUB,
     &            key_org,key_fx,key_grid1,
     &            KR,R,RD,
     &            KN1,grid1,KM,ANGLE,WAVEL,KRE,KIM,ARE,AIM,pomin,pomax,
     &            NRATN,RATIO
     &           ,distname_O,distname_F,distname_N,NDP)
!       write(*,*) 'after MATRIX_FIX:'
!       write(*,*) 'R    =',R,' KR   =',KR
!       write(*,*) 'RATIO=',RATIO,' NRATN=',NRATN
								if(keySUB.eq.0) then				                       
								  T_INT01=dtime(tarray)
								  T_INT1=T_INT1+T_INT01   !+++ 
								  CALL CPU_TIME (time_end1)
								  T_CPU1=T_CPU1+(time_end1-time_begin1)   !+++ 	  
!                                  WRITE(*,51) T_INT1/60.     
								  WRITE(*,51) T_INT1
								  WRITE(*,52) T_CPU1
								endif
   51		format('  Read kernels .................... ',f8.3,' sec')
   52		format('  Read kernels CPU_time ........... ',f8.3,' sec')
!c ** in order to adjust Size Distribution (SD) in OPTCHAR subroutine
		IF(GRID(KN).LT.1.0E-30)GRID(KN)=1.0E-30
		IF(GRID(1).LT.1.0E-30)GRID(1)=1.0E-30
		IF(GRID1(KN1).LT.1.0E-30)GRID1(KN1)=1.0E-30
		IF(GRID1(1).LT.1.0E-30)GRID1(1)=1.0E-30
	    dlnr=((LOG(grid(KN))-LOG(grid(1)))/(KN-1))
     &       /((LOG(grid1(KN1))-LOG(grid1(1)))/(KN1-1))
		if((dlnr-1.).gt.1e-5) then
			write(*,*) 'dlnr=',dlnr
			write(*,'(A,/,A,/,A)') 'STOP(dlnr > 1.):',
     &		'The code does not support grid radius interval bigger than',
     &		'the interval for Kernel lookup table; see grid.dat file'
!	  STOP 
		endif

		NDP=1
      ENDIF ! NDP  
!c      write(*,*) 'after MATRIX_FIX'
!c
!c ** CHECK OF SIZE PARAMETER (PO) RANGE
!c 
!cl       write(*,*) '********** in usmatrix.f: WAVEL=',WAVEL
									if(keySUB.eq.0) then
									   T_INT0=dtime(tarray) !+++
							           CALL CPU_TIME (time_begin)
									endif
      POS =PI2*grid1(1)/WAVEL
      POF =PI2*grid1(KN1)/WAVEL
      PO1S=PI2*grid(1)/WL
      PO1F=PI2*grid(KN)/WL
      IF(PO1S.LT.POS.OR.PO1S.GT.POF) THEN
      WRITE(*,*) 'PO is out of look-up table in USMATRIX 1'
      WRITE(*,*) 'POS=',POS,' PO1S=',PO1S,' POF=',POF
      STOP
      ENDIF
      IF(PO1F.LT.POS.OR.PO1F.GT.POF) THEN
      WRITE(*,*) 'PO is out of look-up table in USMATRIX 2'
      WRITE(*,*) 'POS=',POS,' PO1F=',PO1F,' POF=',POF
      STOP
      ENDIF

      if(key.eq.4) then
       if(key_RD1.eq.1) then
	   call USU_LS(key_RD,keyEL,keySUB,keyLS,KR,R,RD
     &           ,RATIO,NRATN
     &           ,KN1,grid1,KM,ANGLE,WAVEL,KRE,KIM,ARE,AIM
     &           ,KN,grid,RN,RK,WL)
	   else
       call USU_LS_RD(key_RD,keyEL,keySUB,keyLS,KR,R,RD
     &           ,RATIO,NRATN
     &           ,KN1,grid1,KM,ANGLE,WAVEL,KRE,KIM,ARE,AIM
     &           ,KN,grid,RN,RK,WL)
       if(keySUB.eq.0) write(*,*) 'key_RD1=',key_RD1
	   endif ! key_RD1	  
	else
      call USUF_LS(keyEL,keySUB,keyLS
     &            ,KN1,grid1,KM,ANGLE,WAVEL,KRE,KIM,ARE,AIM
     &            ,KN,grid,RN,RK,WL) 
	endif ! key
							if(keySUB.eq.0) then
				              T_INT0=dtime(tarray)
	                          T_INT=T_INT+T_INT0   !+++ 
                              CALL CPU_TIME (time_end)
	                          T_CPU=T_CPU+(time_end-time_begin)   !+++ 	  
!                              WRITE(*,61) T_INT/60.     
                              WRITE(*,61) T_INT
                              WRITE(*,62) T_CPU
							endif

   61 format('  Interpolation .................... ',f8.3,' sec')
   62 format('  Interpolation CPU_time ........... ',f8.3,' sec')

      RETURN
      END SUBROUTINE USMATRIX

!c****************************************************************
      SUBROUTINE USUF_LS(keyEL,keySUB,keyLS
     &               ,KN1,grid1,KM,ANGLE,WAVEL,KRE,KIM,ARE,AIM
     &               ,KN,grid,RN,RK,WL)   
      use alloc 
      USE mo_par_DLS
!	  use mod_os
	  use interpl_spline
		use intrpl_linear
      dimension grid(KNpar),grid1(KN1par),ANGLE(KMpar)
      COMMON /US1/ US11(KMpar,KNpar) 
      COMMON /US2/ US12(KMpar,KNpar)
      COMMON /US3/ US22(KMpar,KNpar)
      COMMON /US4/ US33(KMpar,KNpar)
      COMMON /US5/ US34(KMpar,KNpar)
      COMMON /US6/ US44(KMpar,KNpar)
      COMMON /US0/ USEA(2,KNpar)
      dimension AA(6),BB(6),AB(6)
      dimension XPO(2), YPO(2), X(2), Y(2)
     &         ,ARE(KREpar), AIM(KIMpar)
!      real LINEAR		!AH original

!c **  for SPLINE subroutine

      INTEGER           :: key_spln, keyEL, keySUB, keyLS
!cl      REAL              :: cinv
      DOUBLE PRECISION  :: XXS1(KN1par),YYS1(KN1par)
      DOUBLE PRECISION  :: XARG,YFIT
	  DOUBLE PRECISION  :: KS1(KN1par+4),CS1(KN1par+4)
      PI=ACOS(-1.)
      PI2=2.*PI

      RL=RN
      RI=RK
      I0=0
      I1=0
      K0=0
      K1=0
      DO I=1,KRE-1
      IF(RL.GE.ARE(I).AND.RL.LE.ARE(I+1)) THEN
       I0=I
       I1=I+1
      ENDIF
      ENDDO
      DO I=1,KIM-1
      IF(RI.GE.AIM(I).AND.RI.LE.AIM(I+1)) THEN
        K0=I
        K1=I+1
      ENDIF
      ENDDO
      IF(RL.LE.ARE(1)) THEN
        if(keySUB.eq.0) then
        IF(RL.LT.ARE(1)) THEN
        WRITE(*,*) 'n=',RN,' is out of the range:',
     &                  ARE(1),'< n <',ARE(KRE)
        WRITE(*,*) 'n has been changed',RN,' => ',ARE(1)
        ENDIF
		endif
	    I0=1
        I1=2
        RN=ARE(1)
        RL=ARE(1)
      ENDIF
      IF(RL.GE.ARE(KRE)) THEN
        if(keySUB.eq.0) then
        IF(RL.GT.ARE(KRE)) THEN
        WRITE(*,*) 'n=',RN,' is out of the range:',
     &                  ARE(1),'< n <',ARE(KRE)
        WRITE(*,*) 'n has been changed',RN,' => ',ARE(KRE)
        ENDIF
		endif
        I0=KRE-1
        I1=KRE
        RN=ARE(KRE)
        RL=ARE(KRE)
      ENDIF
      IF(RI.LE.AIM(1)) THEN
        if(keySUB.eq.0) then
        IF(RI.LT.AIM(1)) THEN
        WRITE(*,*) 'k=',RK,' is out of the range:',
     &                  AIM(1),'< k <',AIM(KIM)
        WRITE(*,*) 'k has been changed',RK,' => ',AIM(1)
        ENDIF
		endif
        K0=1
        K1=2
        RK=AIM(1)
        RI=AIM(1)
      ENDIF
      IF(RI.GE.AIM(KIM)) THEN
        if(keySUB.eq.0) then
        IF(RI.GT.AIM(KIM)) THEN
        WRITE(*,*) 'k=',RK,' is out of the range:',
     &                  AIM(1),'< k <',AIM(KIM)
        WRITE(*,*) 'k has been changed',RK,' => ',AIM(KIM)
        ENDIF
		endif
        K0=KIM-1
        K1=KIM
        RK=AIM(KIM)
        RI=AIM(KIM)
      ENDIF
!C      write(*,*) I0,I1,K0,K1,' I0,I1,K0,K1'
!C      WRITE(*,*) ARE(I0),ARE(I1),' ARE'
!C      WRITE(*,*) AIM(K0),AIM(K1),' AIM'

!cl   moved to subroutine MATRIX_FIX, after READ Rke...fix
!cl      UF11(1:KM,1:KN1,1:KIM,1:KRE)=
!cl     &     LOG( UF11(1:KM,1:KN1,1:KIM,1:KRE) )
!cl      if(keyEL.gt.2)
!cl     &  UF22(1:KM,1:KN1,1:KIM,1:KRE)=
!cl     &         LOG( UF22(1:KM,1:KN1,1:KIM,1:KRE) )
!cl	  if(keyEL.gt.3) then
!cl      DO J=1,KM
!cl      IF(ANGLE(J).LE.40.) 
!cl     &     UF33(J,1:KN1,1:KIM,1:KRE)=
!cl     &         LOG( UF33(J,1:KN1,1:KIM,1:KRE) )
!cl      ENDDO ! J
!cl	  endif
!cl	  if(keyEL.gt.5) then
!cl      DO J=1,KM	  	  
!cl      IF(ANGLE(J).LE.50.) 
!cl     &     UF44(J,1:KN1,1:KIM,1:KRE)=
!cl     &         LOG( UF44(J,1:KN1,1:KIM,1:KRE) )
!cl      ENDDO ! J
!cl	  endif

      DO I=1,KN
      IP0=0
      IP1=0
      PO=grid(I)*PI2/WL            
      DO IP=1,KN1-1
      PO1=grid1(IP)  *PI2/WAVEL   
      PO2=grid1(IP+1)*PI2/WAVEL 
      IF(PO.GE.PO1.AND.PO.LE.PO2) THEN
      IP0=IP
      IP1=IP+1
      ENDIF
      IF(PO.LE.(grid1(1)*PI2/WAVEL)) THEN
      IP0=1
      IP1=2
      ENDIF
      IF(PO.GE.(grid1(KN1)*PI2/WAVEL)) THEN
      IP0=KN1-1
      IP1=KN1
      ENDIF      
      ENDDO ! IP
!c
!c **  SCATTERING MATRIX ELEMENTS
!c
!cl	  cinv=WAVEL/WL
      IF(keyLS.EQ.1) THEN
        XPO(1)=grid1(IP0)*PI2/WAVEL
        XPO(2)=grid1(IP1)*PI2/WAVEL	
      ELSE
	    XXS1(1:KN1)=grid1(1:KN1)*PI2/WAVEL
	    XARG=PO  
	  ENDIF

	  IF(keyEL.gt.0) THEN
	  
      DO J=1,KM

      X(1)=ARE(I0)
      X(2)=ARE(I1)
!c
!c ** U11     AA(1)
!c
      IF(keyLS.EQ.1) THEN
		YPO(1)=UF11(J,IP0,K0,I0)
	    YPO(2)=UF11(J,IP1,K0,I0)
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=UF11(J,IP0,K0,I1)
        YPO(2)=UF11(J,IP1,K0,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
      ELSE
	    YYS1(1:KN1)=UF11(J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=EXP(YFIT) 
	    YYS1(1:KN1)=UF11(J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=EXP(YFIT) 
	  ENDIF
      AA(1)=LINEAR( X, Y, 2, RL )
!c
!c ** U12     AA(2)
!c
	  IF(keyEL.gt.1) THEN
      IF(keyLS.EQ.1) THEN
        YPO(1)=UF12(J,IP0,K0,I0)
        YPO(2)=UF12(J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UF12(J,IP0,K0,I1)
        YPO(2)=UF12(J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE	  
	    YYS1(1:KN1)=UF12(J,1:KN1,K0,I0) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT 
	    YYS1(1:KN1)=UF12(J,1:KN1,K0,I1) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT 
	  ENDIF
        AA(2)=LINEAR( X, Y, 2, RL )
	  ENDIF
!c
!c ** U22     AA(3)
!c
	  IF(keyEL.gt.2) THEN
      IF(keyLS.EQ.1) THEN
        YPO(1)=UF22(J,IP0,K0,I0)
        YPO(2)=UF22(J,IP1,K0,I0)
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=UF22(J,IP0,K0,I1)
        YPO(2)=UF22(J,IP1,K0,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
	  ELSE
	    YYS1(1:KN1)=UF22(J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(1)=EXP(YFIT) 
	    YYS1(1:KN1)=UF22(J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(2)=EXP(YFIT)
	  ENDIF 
        AA(3)=LINEAR( X, Y, 2, RL )
      ENDIF
!c
!c ** U33     AA(4)
!c
	  IF(keyEL.gt.3) THEN
	  IF(keyLS.EQ.1) THEN
        YPO(1)=UF33(J,IP0,K0,I0)
        YPO(2)=UF33(J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UF33(J,IP0,K0,I1)
        YPO(2)=UF33(J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )	  
	  ELSE
	    YYS1(1:KN1)=UF33(J,1:KN1,K0,I0) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT  
	    YYS1(1:KN1)=UF33(J,1:KN1,K0,I1) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT 
      ENDIF
        IF(ANGLE(J).LE.40.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2))  
        ENDIF ! ANGLE
        AA(4)=LINEAR( X, Y, 2, RL )
      ENDIF
!c
!c ** U34     AA(5)
!c
	  IF(keyEL.gt.4) THEN
      IF(keyLS.EQ.1) THEN
        YPO(1)=UF34(J,IP0,K0,I0)
        YPO(2)=UF34(J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UF34(J,IP0,K0,I1)
        YPO(2)=UF34(J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=UF34(J,1:KN1,K0,I0) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT 
	    YYS1(1:KN1)=UF34(J,1:KN1,K0,I1) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(2)=YFIT 
      ENDIF
        AA(5)=LINEAR( X, Y, 2, RL )
      ENDIF
!c
!c ** U44     AA(6)
!c
	  IF(keyEL.gt.5) THEN
	  IF(keyLS.EQ.1) THEN
        YPO(1)=UF44(J,IP0,K0,I0)
        YPO(2)=UF44(J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UF44(J,IP0,K0,I1)
        YPO(2)=UF44(J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE	  
	    YYS1(1:KN1)=UF44(J,1:KN1,K0,I0) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(1)=YFIT  
		YYS1(1:KN1)=UF44(J,1:KN1,K0,I1) 
		key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT 
	  ENDIF
        IF(ANGLE(J).LE.50.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2))   
        ENDIF ! ANGLE
        AA(6)=LINEAR( X, Y, 2, RL )     
      ENDIF 
!c
!c ** U11     BB(1)
!c
      IF(keyLS.EQ.1) THEN
        YPO(1)=UF11(J,IP0,K1,I0)
        YPO(2)=UF11(J,IP1,K1,I0) 
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=UF11(J,IP0,K1,I1)
        YPO(2)=UF11(J,IP1,K1,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))	  
	  ELSE
	    YYS1(1:KN1)=UF11(J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(1)=EXP(YFIT) 
	    YYS1(1:KN1)=UF11(J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(2)=EXP(YFIT)
	  ENDIF 
      BB(1)=LINEAR( X, Y, 2, RL )
!c
!c ** U12     BB(2)
!c
	  IF(keyEL.gt.1) THEN
	  IF(keyLS.EQ.1) THEN
        YPO(1)=UF12(J,IP0,K1,I0)
        YPO(2)=UF12(J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UF12(J,IP0,K1,I1)
        YPO(2)=UF12(J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=UF12(J,1:KN1,K1,I0) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT 
	    YYS1(1:KN1)=UF12(J,1:KN1,K1,I1) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF 
      BB(2)=LINEAR( X, Y, 2, RL )
      ENDIF
!c
!c ** U22     BB(3)
!c
	  IF(keyEL.gt.2) THEN
	  IF(keyLS.EQ.1) THEN
        YPO(1)=UF22(J,IP0,K1,I0)
        YPO(2)=UF22(J,IP1,K1,I0) 
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=UF22(J,IP0,K1,I1)
        YPO(2)=UF22(J,IP1,K1,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))  
	  ELSE
	    YYS1(1:KN1)=UF22(J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=EXP(YFIT) 
	    YYS1(1:KN1)=UF22(J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=EXP(YFIT) 
	  ENDIF
        BB(3)=LINEAR( X, Y, 2, RL )
      ENDIF
!c
!c ** U33     BB(4)
!c
	  IF(keyEL.gt.3) THEN
	  IF(keyLS.EQ.1) THEN
        YPO(1)=UF33(J,IP0,K1,I0)
        YPO(2)=UF33(J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UF33(J,IP0,K1,I1)
        YPO(2)=UF33(J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=UF33(J,1:KN1,K1,I0) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(1)=YFIT  
	    YYS1(1:KN1)=UF33(J,1:KN1,K1,I1) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(2)=YFIT 
	  ENDIF 
        IF(ANGLE(J).LE.40.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2))  
        ENDIF ! ANGLE
        BB(4)=LINEAR( X, Y, 2, RL )
      ENDIF
!c
!c ** U34     BB(5)
!c
	  IF(keyEL.gt.4) THEN
	  IF(keyLS.EQ.1) THEN
        YPO(1)=UF34(J,IP0,K1,I0)
        YPO(2)=UF34(J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UF34(J,IP0,K1,I1)
        YPO(2)=UF34(J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
     	YYS1(1:KN1)=UF34(J,1:KN1,K1,I0) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT 
	    YYS1(1:KN1)=UF34(J,1:KN1,K1,I1) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(2)=YFIT 
	  ENDIF
      BB(5)=LINEAR( X, Y, 2, RL )
      ENDIF
!c
!c ** U44     BB(6)
!c
	  IF(keyEL.gt.5) THEN
	  IF(keyLS.EQ.1) THEN
        YPO(1)=UF44(J,IP0,K1,I0)
        YPO(2)=UF44(J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UF44(J,IP0,K1,I1)
        YPO(2)=UF44(J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=UF44(J,1:KN1,K1,I0) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(1)=YFIT  
	    YYS1(1:KN1)=UF44(J,1:KN1,K1,I1) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT  
	  ENDIF
        IF(ANGLE(J).LE.50.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2))  
        ENDIF ! ANGLE
        BB(6)=LINEAR( X, Y, 2, RL )
      ENDIF 

      X(1)=AIM(K0)
      X(2)=AIM(K1)

      Y(1)=AA(1)
      Y(2)=BB(1)
      AB(1)=LINEAR( X, Y, 2, RI )
      US11(J,I)=AB(1)/WL
!c
!c ** US12
!c
      IF(keyEL.gt.1) THEN
      Y(1)=AA(2)
      Y(2)=BB(2)
      AB(2)=LINEAR( X, Y, 2, RI )
      US12(J,I)=AB(2)/WL
      ENDIF
!c
!c ** US22
!c
      IF(keyEL.gt.2) THEN
      Y(1)=AA(3)
      Y(2)=BB(3)
      AB(3)=LINEAR( X, Y, 2, RI )
      US22(J,I)=AB(3)/WL
      ENDIF	
!c
!c ** US33
!c
      IF(keyEL.gt.3) THEN
      Y(1)=AA(4)
      Y(2)=BB(4)
      AB(4)=LINEAR( X, Y, 2, RI )
      US33(J,I)=AB(4)/WL
      ENDIF	
!c
!c ** US34
!c
      IF(keyEL.gt.4) THEN
      Y(1)=AA(5)
      Y(2)=BB(5)
      AB(5)=LINEAR( X, Y, 2, RI )
      US34(J,I)=AB(5)/WL
      ENDIF	
!c
!c ** US44
!c
      IF(keyEL.gt.5) THEN
      Y(1)=AA(6)
      Y(2)=BB(6)
      AB(6)=LINEAR( X, Y, 2, RI )
      US44(J,I)=AB(6)/WL
      ENDIF
      
      ENDDO   ! J KM

      ENDIF ! keyEL>0
!c
!c ** EXTINCTION & ABSORPTION
!c
      DO J=1,2

      X(1)=ARE(I0)
      X(2)=ARE(I1)

      IF(keyLS.EQ.1) THEN
        YPO(1)=UFEA(J,IP0,K0,I0)
        YPO(2)=UFEA(J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UFEA(J,IP0,K0,I1)
        YPO(2)=UFEA(J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=UFEA(J,1:KN1,K0,I0) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT 
	    YYS1(1:KN1)=UFEA(J,1:KN1,K0,I1) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(2)=YFIT 
      ENDIF
        AA(1)=LINEAR( X, Y, 2, RL )

      IF(keyLS.EQ.1) THEN
        YPO(1)=UFEA(J,IP0,K1,I0)
        YPO(2)=UFEA(J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UFEA(J,IP0,K1,I1)
        YPO(2)=UFEA(J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=UFEA(J,1:KN1,K1,I0) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT 
	    YYS1(1:KN1)=UFEA(J,1:KN1,K1,I1) 
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF 
        BB(1)=LINEAR( X, Y, 2, RL )

      X(1)=AIM(K0)
      X(2)=AIM(K1)
      Y(1)=AA(1)
      Y(2)=BB(1)
      AB(1)=LINEAR( X, Y, 2, RI )
      USEA(J,I)=AB(1)*WAVEL/WL

      ENDDO ! J 2

      ENDDO ! I KN

      RETURN
      END SUBROUTINE USUF_LS      

!c **************************************************************** 
      SUBROUTINE USU_LS(key_RD,keyEL,keySUB,keyLS,KR,R,RD
     &                 ,RATIO,NRATN
     &                 ,KN1,grid1,KM,ANGLE,WAVEL,KRE,KIM,ARE,AIM
     &                 ,KN,grid,RN,RK,WL) 
      use alloc1
      USE mo_par_DLS
!	  use mod_os
	  use interpl_spline
		use intrpl_linear
      real*4 R(KRpar), RATIO(KR1par), RRATN
      dimension grid(KNpar),RD(KRpar)
      dimension grid1(KN1par)
     &         , ANGLE(KMpar)
      COMMON /US1/ US11(KMpar,KNpar) 
      COMMON /US2/ US12(KMpar,KNpar)
      COMMON /US3/ US22(KMpar,KNpar)
      COMMON /US4/ US33(KMpar,KNpar)
      COMMON /US5/ US34(KMpar,KNpar)
      COMMON /US6/ US44(KMpar,KNpar)
      COMMON /US0/ USEA(2,KNpar)
      dimension RDc(KR),AA(6),BB(6),AB(6)
     &                 ,CC(6),DD(6),CD(6)
     &                 ,sumUS(6)
      dimension XPO(2), YPO(2), X(2), Y(2)
     &         ,ARE(KREpar), AIM(KIMpar)
!      real LINEAR		!AH original
!c **  for SPLINE subroutine

      INTEGER           :: key_spln,keyEL,keySUB,keyLS
      DOUBLE PRECISION  :: XXS1(KN1par),YYS1(KN1par)
      DOUBLE PRECISION  :: XARG,YFIT
	DOUBLE PRECISION  :: KS1(KN1par+4),CS1(KN1par+4)

      PI=ACOS(-1.)
      PI2=2.*PI
!cl      cinv=WAVEL/WL  ! invariant for dV(..)/dlnr
	  
       IF(key_RD.eq.2) then
!c 
!c*** RECALCULATE ASPECT RATIO DISTRIBUTION (RDc()=SAREA/VOLUME)
!c*** RDc()=RD()/RDc(); sumRD=sum(RDc())
!c
!c ** OBLATE
        do IR=1,KR
        if(R(IR).lt.1.) then              
        E=SQRT( 1.-R(IR)*R(IR) )
	  IF((1.+E)/(1.-E).LT.1.0E-30)THEN
		XA1=LOG(1.0E-30)/E
	  ELSE
		xa1=LOG((1.+E)/(1.-E))/E
	  ENDIF
        RDc(IR)=1.5*(R(IR)**(-2./3.)+
     &            0.5*xa1*R(IR)**(4./3.))
!c ** PROLATE            
        elseif(R(IR).gt.1.) then          
        E=SQRT( 1.-1./R(IR)/R(IR) )
        xa2=ASIN(E)/E
        RDc(IR)=1.5*(R(IR)**(-2./3.)+
     &               xa2*R(IR)**(1./3.))
!c ** SPHERE
        elseif(R(IR).eq.1.) then             
        RDc(IR)=3.
        endif ! R()
!c ** WRITE ASPECT RATIO DISTRIBUTION 
!c          write(*,*) 'R=',R(IR),' B=',RDc(IR),
!c     &  ' 1/B=',1./RDc(IR),' RD=',RD(IR)

        enddo ! IR
        RDc(:KR)=RD(:KR)/RDc(:KR)
      ENDIF ! key_RD
      IF(key_RD.eq.1) RDc(:KR)=RD(:KR)
!c
!c ** LOG(U...)
!c
!cl   moved to subroutine MATRIX_FIX, under if(key.eq.4), RETURN
!cl      if(NDP4.eq.0) then
!cl      U11(1:KR,1:KM,1:KN1,1:KIM,1:KRE)=
!cl     &     LOG( U11(1:KR,1:KM,1:KN1,1:KIM,1:KRE) )
!cl      if(keyEL.gt.2)
!cl     &         U22(1:KR,1:KM,1:KN1,1:KIM,1:KRE)=
!cl     &         LOG( U22(1:KR,1:KM,1:KN1,1:KIM,1:KRE) )
!cl      if(keyEL.gt.3) then
!cl      DO J=1,KM
!cl      IF(ANGLE(J).LE.40.) 
!cl     &     U33(1:KR,J,1:KN1,1:KIM,1:KRE)=
!cl     &         LOG( U33(1:KR,J,1:KN1,1:KIM,1:KRE) )
!cl      ENDDO ! J
!cl      endif
!cl      if(keyEL.gt.5) then
!cl      DO J=1,KM
!cl      IF(ANGLE(J).LE.50.) 
!cl     &     U44(1:KR,J,1:KN1,1:KIM,1:KRE)=
!cl     &         LOG( U44(1:KR,J,1:KN1,1:KIM,1:KRE) )
!clcl       write(*,*) 'U34 before intrpl orgnl:',U34(1,j,1,1,1)
!cl      ENDDO ! J
!cl      endif
!cl      endif ! NDP4

      sumRD=sum(RDc(:KR))
	  	  	  
      if(keySUB.eq.0) then
      do IR=1,KR 
      write(*,'(''R='',f8.4,4x,'' RDc='',e13.5)') R(IR),RDc(IR)
      enddo ! IR
      write(*,'(''sumRD='',e13.5)') sumRD
	  endif
	
      RL=RN
      RI=RK
      I0=0
      I1=0
      K0=0
      K1=0
      DO I=1,KRE-1
      IF(RL.GE.ARE(I).AND.RL.LE.ARE(I+1)) THEN
       I0=I
       I1=I+1
      ENDIF
      ENDDO
      DO I=1,KIM-1
      IF(RI.GE.AIM(I).AND.RI.LE.AIM(I+1)) THEN
        K0=I
        K1=I+1
      ENDIF
      ENDDO
      IF(RL.LE.ARE(1)) THEN
        if(keySUB.eq.0) then
        IF(RL.LT.ARE(1)) THEN
        WRITE(*,*) 'n=',RN,' is out of the range:',
     &                  ARE(1),'< n <',ARE(KRE)
        WRITE(*,*) 'n has been changed',RN,' => ',ARE(1)
        ENDIF
		endif
        I0=1
        I1=2
        RN=ARE(1)
        RL=ARE(1)
      ENDIF
      IF(RL.GE.ARE(KRE)) THEN
        if(keySUB.eq.0) then
        IF(RL.GT.ARE(KRE)) THEN
        WRITE(*,*) 'n=',RN,' is out of the range:',
     &                  ARE(1),'< n <',ARE(KRE)
        WRITE(*,*) 'n has been changed',RN,' => ',ARE(KRE)
        ENDIF
        endif
        I0=KRE-1
        I1=KRE
        RN=ARE(KRE)
        RL=ARE(KRE)
      ENDIF
      IF(RI.LE.AIM(1)) THEN
        if(keySUB.eq.0) then
        IF(RI.LT.AIM(1)) THEN
        WRITE(*,*) 'k=',RK,' is out of the range:',
     &                  AIM(1),'< k <',AIM(KIM)
        WRITE(*,*) 'k has been changed',RK,' => ',AIM(1)
        ENDIF
        endif
        K0=1
        K1=2
        RK=AIM(1)
        RI=AIM(1)
      ENDIF
      IF(RI.GE.AIM(KIM)) THEN
        if(keySUB.eq.0) then
        IF(RI.GT.AIM(KIM)) THEN
        WRITE(*,*) 'k=',RK,' is out of the range:',
     &                  AIM(1),'< k <',AIM(KIM)
        WRITE(*,*) 'k has been changed',RK,' => ',AIM(KIM)
        ENDIF
        endif
		
        K0=KIM-1
        K1=KIM
        RK=AIM(KIM)
        RI=AIM(KIM)
      ENDIF
!C      write(*,*) I0,I1,K0,K1,' I0,I1,K0,K1'
!C      WRITE(*,*) ARE(I0),ARE(I1),' ARE'
!C      WRITE(*,*) AIM(K0),AIM(K1),' AIM'
      DO I=1,KN
      IP0=0
      IP1=0
      PO=grid(I)*PI2/WL            

      DO IP=1,KN1-1
      PO1=grid1(IP)  *PI2/WAVEL   
      PO2=grid1(IP+1)*PI2/WAVEL 
      IF(PO.GE.PO1.AND.PO.LE.PO2) THEN
      IP0=IP
      IP1=IP+1
      ENDIF
      IF(PO.LE.(grid1(1)*PI2/WAVEL)) THEN
      IP0=1
      IP1=2
      ENDIF
      IF(PO.GE.(grid1(KN1)*PI2/WAVEL)) THEN
      IP0=KN1-1
      IP1=KN1
      ENDIF      
      ENDDO ! IP
      IF(keyLS.EQ.1) THEN
        XPO(1)=grid1(IP0)*PI2/WAVEL
        XPO(2)=grid1(IP1)*PI2/WAVEL
	  ELSE
	    XXS1(1:KN1)=grid1(1:KN1)*PI2/WAVEL
	    XARG=PO
	  ENDIF
!c
!c **  SCATTERING MATRIX ELEMENTS
!c
	  IF(keyEL.gt.0) THEN
	  
      DO J=1,KM
      sumUS(:6)=0.
      DO IR=1,KR
      IF(NRATN.EQ.1) then
        RRATN=RATIO(1)
        IF(RRATN.NE.R(IR)) THEN
        WRITE(*,*) 'R=',R(IR),' .NE. RATIO=',RATIO(1)
        WRITE(*,*) 'R has been changed',
     &                  R(IR),' => ',RATIO(1)
        R(IR)=RRATN
        ENDIF
      ELSE
        RRATN=R(IR)
      ENDIF
!c
!c ** AXIS RATIO LOOP
!c
      IF(NRATN.NE.1) THEN
      DO IRATN=1,NRATN-1
      IF(RRATN.GE.RATIO(IRATN).AND.RRATN.LE.RATIO(IRATN+1)) THEN
        L0=IRATN
        L1=IRATN+1
      ENDIF
      ENDDO
      IF(RRATN.LE.RATIO(1)) THEN
        IF(RRATN.LT.RATIO(1)) THEN
        WRITE(*,*) 'R=',R(IR),' is out of the range:',
     &                  RATIO(1),'< R <',RATIO(NRATN)
        WRITE(*,*) 'R has been changed',R(IR),' => ',RATIO(1)
        ENDIF
        L0=1
        L1=2
        R(IR)=RATIO(1)
        RRATN=RATIO(1)
      ENDIF
      IF(RRATN.GE.RATIO(NRATN)) THEN
        IF(RRATN.GT.RATIO(NRATN)) THEN
        WRITE(*,*) 'R=',R(IR),' is out of the range:',
     &                  RATIO(1),'< R <',RATIO(NRATN)
        WRITE(*,*) 'R has been changed',R(IR),
     &                            ' => ',RATIO(NRATN)
        ENDIF
        L0=NRATN-1
        L1=NRATN
        R(IR)=RATIO(NRATN)
        RRATN=RATIO(NRATN)
      ENDIF
      ELSE
        L0=1
        L1=1
      ENDIF

!      IF(keyLS.EQ.1) THEN
!        XPO(1)=grid1(IP0)*PI2/WAVEL
!        XPO(2)=grid1(IP1)*PI2/WAVEL
!	  ELSE
!	    XXS1(1:KN1)=grid1(1:KN1)*PI2/WAVEL
!	    XARG=PO
!	  ENDIF

      X(1)=ARE(I0)
      X(2)=ARE(I1)
!c
!c ** U11     AA(1)
!c
      IF(keyLS.EQ.1) THEN
        YPO(1)=U11(L0,J,IP0,K0,I0)
        YPO(2)=U11(L0,J,IP1,K0,I0)
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=U11(L0,J,IP0,K0,I1)
        YPO(2)=U11(L0,J,IP1,K0,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
	  ELSE
	    YYS1(1:KN1)=U11(L0,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=EXP(YFIT)
	    YYS1(1:KN1)=U11(L0,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=EXP(YFIT)
	  ENDIF
      AA(1)=LINEAR( X, Y, 2, RL )
!c
!c ** U12     AA(2)
!c
      if(keyEL.gt.1) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U12(L0,J,IP0,K0,I0)
        YPO(2)=U12(L0,J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U12(L0,J,IP0,K0,I1)
        YPO(2)=U12(L0,J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U12(L0,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U12(L0,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        AA(2)=LINEAR( X, Y, 2, RL )
	  endif
!c
!c ** U22     AA(3)
!c
      if(keyEL.gt.2) then      
      IF(keyLS.EQ.1) THEN
        YPO(1)=U22(L0,J,IP0,K0,I0)
        YPO(2)=U22(L0,J,IP1,K0,I0)
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=U22(L0,J,IP0,K0,I1)
        YPO(2)=U22(L0,J,IP1,K0,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
	  ELSE
	    YYS1(1:KN1)=U22(L0,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=EXP(YFIT)
	    YYS1(1:KN1)=U22(L0,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(2)=EXP(YFIT)
	  ENDIF
		AA(3)=LINEAR( X, Y, 2, RL )
	  endif
!c
!c ** U33     AA(4)
!c
      if(keyEL.gt.3) then
	  IF(keyLS.EQ.1) THEN
        YPO(1)=U33(L0,J,IP0,K0,I0)
        YPO(2)=U33(L0,J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U33(L0,J,IP0,K0,I1)
        YPO(2)=U33(L0,J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U33(L0,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U33(L0,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        IF(ANGLE(J).LE.40.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2)) 
        ENDIF ! ANGLE
        AA(4)=LINEAR( X, Y, 2, RL )
	  endif
!c
!c ** U34     AA(5)
!c
      if(keyEL.gt.4) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U34(L0,J,IP0,K0,I0)
        YPO(2)=U34(L0,J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U34(L0,J,IP0,K0,I1)
        YPO(2)=U34(L0,J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U34(L0,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U34(L0,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(2)=YFIT
	  ENDIF
        AA(5)=LINEAR( X, Y, 2, RL )
	  endif
!c
!c ** U44     AA(6)
!c
      if(keyEL.gt.5) then
      IF(keyLS.EQ.1) THEN
	    YPO(1)=U44(L0,J,IP0,K0,I0)
        YPO(2)=U44(L0,J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U44(L0,J,IP0,K0,I1)
        YPO(2)=U44(L0,J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U44(L0,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U44(L0,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        IF(ANGLE(J).LE.50.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2))
	    ENDIF ! ANGLE
        AA(6)=LINEAR( X, Y, 2, RL )
	  endif  
!c
!c ** U11     BB(1)
!c
      IF(keyLS.eq.1) THEN
        YPO(1)=U11(L0,J,IP0,K1,I0)
		YPO(2)=U11(L0,J,IP1,K1,I0) 
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=U11(L0,J,IP0,K1,I1)
        YPO(2)=U11(L0,J,IP1,K1,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
	  ELSE
	    YYS1(1:KN1)=U11(L0,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=EXP(YFIT)
	    YYS1(1:KN1)=U11(L0,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=EXP(YFIT)
	  ENDIF
        BB(1)=LINEAR( X, Y, 2, RL )
!c
!c ** U12     BB(2)
!c
      if(keyEL.gt.1) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U12(L0,J,IP0,K1,I0)
        YPO(2)=U12(L0,J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U12(L0,J,IP0,K1,I1)
        YPO(2)=U12(L0,J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U12(L0,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U12(L0,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        BB(2)=LINEAR( X, Y, 2, RL )
      endif
!c
!c ** U22     BB(3)
!c
      if(keyEL.gt.2) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U22(L0,J,IP0,K1,I0)
        YPO(2)=U22(L0,J,IP1,K1,I0) 
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=U22(L0,J,IP0,K1,I1)
        YPO(2)=U22(L0,J,IP1,K1,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
	  ELSE
	    YYS1(1:KN1)=U22(L0,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=EXP(YFIT)
	    YYS1(1:KN1)=U22(L0,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=EXP(YFIT)
	  ENDIF
        BB(3)=LINEAR( X, Y, 2, RL )
      endif
!c
!c ** U33     BB(4)
!c
      if(keyEL.gt.3) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U33(L0,J,IP0,K1,I0)
        YPO(2)=U33(L0,J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U33(L0,J,IP0,K1,I1)
        YPO(2)=U33(L0,J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U33(L0,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U33(L0,J,1:KN1,K1,I1)
		key_spln=0
		CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
         Y(2)=YFIT
	  ENDIF
	    IF(ANGLE(J).LE.40.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2)) 
        ENDIF ! ANGLE
        BB(4)=LINEAR( X, Y, 2, RL )
      endif
!c
!c ** U34     BB(5)
!c
      if(keyEL.gt.4) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U34(L0,J,IP0,K1,I0)
        YPO(2)=U34(L0,J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U34(L0,J,IP0,K1,I1)
        YPO(2)=U34(L0,J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
      ELSE
	    YYS1(1:KN1)=U34(L0,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U34(L0,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
		BB(5)=LINEAR( X, Y, 2, RL )
	  endif
!c
!c ** U44     BB(6)
!c
      if(keyEL.gt.5) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U44(L0,J,IP0,K1,I0)
        YPO(2)=U44(L0,J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U44(L0,J,IP0,K1,I1)
        YPO(2)=U44(L0,J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U44(L0,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U44(L0,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        IF(ANGLE(J).LE.50.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2))	  
        ENDIF ! ANGLE
        BB(6)=LINEAR( X, Y, 2, RL )
      endif 

      X(1)=AIM(K0)
      X(2)=AIM(K1)

      DO II=1,keyEL
      Y(1)=AA(II)
      Y(2)=BB(II)
      AB(II)=LINEAR( X, Y, 2, RI )
      ENDDO ! II

      IF(NRATN.NE.1) THEN
      X(1)=ARE(I0)
      X(2)=ARE(I1)
!c
!c ** U11     CC(1)
!c
      IF(keyLS.EQ.1) THEN
        YPO(1)=U11(L1,J,IP0,K0,I0)
        YPO(2)=U11(L1,J,IP1,K0,I0)
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=U11(L1,J,IP0,K0,I1)
        YPO(2)=U11(L1,J,IP1,K0,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
      ELSE
	    YYS1(1:KN1)=U11(L1,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=EXP(YFIT)
	    YYS1(1:KN1)=U11(L1,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=EXP(YFIT)
	  ENDIF
        CC(1)=LINEAR( X, Y, 2, RL )
!c
!c ** U12     CC(2)
!c
      if(keyEL.gt.1) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U12(L1,J,IP0,K0,I0)
        YPO(2)=U12(L1,J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U12(L1,J,IP0,K0,I1)
        YPO(2)=U12(L1,J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U12(L1,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U12(L1,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        CC(2)=LINEAR( X, Y, 2, RL )
      endif
!c
!c ** U22     CC(3)
!c
      if(keyEL.gt.2) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U22(L1,J,IP0,K0,I0)
        YPO(2)=U22(L1,J,IP1,K0,I0)
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=U22(L1,J,IP0,K0,I1)
        YPO(2)=U22(L1,J,IP1,K0,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
	  ELSE
	    YYS1(1:KN1)=U22(L1,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=EXP(YFIT)
	    YYS1(1:KN1)=U22(L1,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=EXP(YFIT)
	  ENDIF
        CC(3)=LINEAR( X, Y, 2, RL )
      endif
!c
!c ** U33     CC(4)
!c
      if(keyEL.gt.3) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U33(L1,J,IP0,K0,I0)
        YPO(2)=U33(L1,J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U33(L1,J,IP0,K0,I1)
        YPO(2)=U33(L1,J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U33(L1,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U33(L1,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        IF(ANGLE(J).LE.40.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2)) 	  
        ENDIF ! ANGLE
        CC(4)=LINEAR( X, Y, 2, RL )
      endif
!c
!c ** U34     CC(5)
!c
      if(keyEL.gt.4) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U34(L1,J,IP0,K0,I0)
        YPO(2)=U34(L1,J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U34(L1,J,IP0,K0,I1)
        YPO(2)=U34(L1,J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U34(L1,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U34(L1,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        CC(5)=LINEAR( X, Y, 2, RL )
	  endif
!c
!c ** U44     CC(6)
!c
      if(keyEL.gt.5) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U44(L1,J,IP0,K0,I0)
        YPO(2)=U44(L1,J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U44(L1,J,IP0,K0,I1)
        YPO(2)=U44(L1,J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U44(L1,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U44(L1,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        IF(ANGLE(J).LE.50.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2))  	  
        ENDIF ! ANGLE
        CC(6)=LINEAR( X, Y, 2, RL )
      endif 
!c
!c ** U11     DD(1)
!c
      IF(keyLS.EQ.1) THEN
        YPO(1)=U11(L1,J,IP0,K1,I0)
		YPO(2)=U11(L1,J,IP1,K1,I0)
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=U11(L1,J,IP0,K1,I1)
        YPO(2)=U11(L1,J,IP1,K1,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
	  ELSE
	    YYS1(1:KN1)=U11(L1,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=EXP(YFIT)
	    YYS1(1:KN1)=U11(L1,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=EXP(YFIT)
	  ENDIF
        DD(1)=LINEAR( X, Y, 2, RL )
!c
!c ** U12     DD(2)
!c
      if(keyEL.gt.1) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U12(L1,J,IP0,K1,I0)
        YPO(2)=U12(L1,J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U12(L1,J,IP0,K1,I1)
        YPO(2)=U12(L1,J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U12(L1,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U12(L1,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        DD(2)=LINEAR( X, Y, 2, RL )
      endif
!c
!c ** U22     DD(3)
!c
      if(keyEL.gt.2) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U22(L1,J,IP0,K1,I0)
        YPO(2)=U22(L1,J,IP1,K1,I0) 
        Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
        YPO(1)=U22(L1,J,IP0,K1,I1)
        YPO(2)=U22(L1,J,IP1,K1,I1)
        Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
	  ELSE
	    YYS1(1:KN1)=U22(L1,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=EXP(YFIT)
	    YYS1(1:KN1)=U22(L1,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=EXP(YFIT)
	  ENDIF
        DD(3)=LINEAR( X, Y, 2, RL )
       endif
!c
!c ** U33     DD(4)
!c
      if(keyEL.gt.3) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U33(L1,J,IP0,K1,I0)
        YPO(2)=U33(L1,J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U33(L1,J,IP0,K1,I1)
        YPO(2)=U33(L1,J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U33(L1,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U33(L1,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
      ENDIF
        IF(ANGLE(J).LE.40.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2))	  
        ENDIF ! ANGLE
        DD(4)=LINEAR( X, Y, 2, RL )
      endif
!c
!c ** U34     DD(5)
!c
      if(keyEL.gt.4) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U34(L1,J,IP0,K1,I0)
        YPO(2)=U34(L1,J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U34(L1,J,IP0,K1,I1)
        YPO(2)=U34(L1,J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U34(L1,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U34(L1,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
		Y(2)=YFIT
      ENDIF
        DD(5)=LINEAR( X, Y, 2, RL )
      endif
!c
!c ** U44     DD(6)
!c
      if(keyEL.gt.5) then
      IF(keyLS.EQ.1) THEN
        YPO(1)=U44(L1,J,IP0,K1,I0)
        YPO(2)=U44(L1,J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=U44(L1,J,IP0,K1,I1)
        YPO(2)=U44(L1,J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=U44(L1,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=U44(L1,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        IF(ANGLE(J).LE.50.) THEN
        Y(1)=EXP(Y(1))  
        Y(2)=EXP(Y(2)) 
        ENDIF ! ANGLE
        DD(6)=LINEAR( X, Y, 2, RL )
      endif 

      X(1)=AIM(K0)
      X(2)=AIM(K1)
      DO II=1,keyEL
      Y(1)=CC(II)
      Y(2)=DD(II)
      CD(II)=LINEAR( X, Y, 2, RI )
      ENDDO ! II

      RRATN1=RRATN

      X(1)=RATIO(L0)
      X(2)=RATIO(L1)

!c ** US11
      Y(1)=AB(1)
      Y(2)=CD(1)
      sumUS(1)=sumUS(1)+LINEAR( X, Y, 2, RRATN1 )*RDc(IR)
!c ** US12
      if(keyEL.gt.1) then
      Y(1)=AB(2)
      Y(2)=CD(2)
      sumUS(2)=sumUS(2)+LINEAR( X, Y, 2, RRATN1 )*RDc(IR)
      endif
!c ** US22
      if(keyEL.gt.2) then
      Y(1)=AB(3)
      Y(2)=CD(3)
      sumUS(3)=sumUS(3)+LINEAR( X, Y, 2, RRATN1 )*RDc(IR)
	  endif
!c ** US33
      if(keyEL.gt.3) then
	  Y(1)=AB(4)
      Y(2)=CD(4)
      sumUS(4)=sumUS(4)+LINEAR( X, Y, 2, RRATN1 )*RDc(IR)
	  endif
!c ** US34
      if(keyEL.gt.4) then
	  Y(1)=AB(5)
      Y(2)=CD(5)
      sumUS(5)=sumUS(5)+LINEAR( X, Y, 2, RRATN1 )*RDc(IR)
      endif
!c ** US44
      if(keyEL.gt.5) then
      Y(1)=AB(6)
      Y(2)=CD(6)
      sumUS(6)=sumUS(6)+LINEAR( X, Y, 2, RRATN1 )*RDc(IR)
	  endif

      ELSEIF(NRATN.EQ.1) THEN

      sumUS(1)=AB(1)*RDc(IR)
	  if(keyEL.gt.1) sumUS(2)=AB(2)*RDc(IR)		
	  if(keyEL.gt.2) sumUS(3)=AB(3)*RDc(IR)
	  if(keyEL.gt.3) sumUS(4)=AB(4)*RDc(IR)
      if(keyEL.gt.4) sumUS(5)=AB(5)*RDc(IR)
	  if(keyEL.gt.5) sumUS(6)=AB(6)*RDc(IR)

      ENDIF ! NRATN

      ENDDO ! IR RD

      US11(J,I)=sumUS(1)/WL/sumRD
      if(keyEL.gt.1) US12(J,I)=sumUS(2)/WL/sumRD
      if(keyEL.gt.2) US22(J,I)=sumUS(3)/WL/sumRD
      if(keyEL.gt.3) US33(J,I)=sumUS(4)/WL/sumRD
      if(keyEL.gt.4) US34(J,I)=sumUS(5)/WL/sumRD
      if(keyEL.gt.5) US44(J,I)=sumUS(6)/WL/sumRD

      ENDDO   ! J KM
      ENDIF ! keyEL>0
!c
!c ** EXTINCTION & ABSORPTION
!c
      DO J=1,2
      sumUSEA=0.
      DO IR=1,KR
      IF(NRATN.EQ.1) then
      RRATN=RATIO(NRATN)
      ELSE
      RRATN=R(IR)
      ENDIF
      IF(NRATN.NE.1) THEN
      DO IRATN=1,NRATN-1
      IF(RRATN.GE.RATIO(IRATN).AND.RRATN.LE.RATIO(IRATN+1)) THEN
          L0=IRATN
          L1=IRATN+1
!cl      write(*,*) IRATN,RATIO(IRATN),L0,L1,R(IR),RRATN, 
!cl     & ' IRATN,RATIO(IRATN),L0,L1 R(IR),RRATN',
!cl     & ' for ext & abs'
      ENDIF
      ENDDO

      IF(RRATN.GE.RATIO(NRATN)) THEN
        L0=NRATN-1
        L1=NRATN
        R(IR)=RATIO(NRATN)
        RRATN=RATIO(NRATN)
      ENDIF
      IF(RRATN.LE.RATIO(1)) THEN
        L0=1
        L1=2
        R(IR)=RATIO(1)
        RRATN=RATIO(1)
      ENDIF
      ELSE
        L0=1
        L1=1
      ENDIF

!cl	XXS1(1:KN1)=grid1(1:KN1)*PI2/WAVEL
!cl	XARG=PO
!cl      key_spln=0

      RRATN1=RRATN

      X(1)=ARE(I0)
      X(2)=ARE(I1)

      IF(keyLS.EQ.1) THEN      
        YPO(1)=UEA(L0,J,IP0,K0,I0)
        YPO(2)=UEA(L0,J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UEA(L0,J,IP0,K0,I1)
        YPO(2)=UEA(L0,J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=UEA(L0,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=UEA(L0,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        AA(1)=LINEAR( X, Y, 2, RL )

      IF(keyLS.EQ.1) THEN
        YPO(1)=UEA(L0,J,IP0,K1,I0)
        YPO(2)=UEA(L0,J,IP1,K1,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UEA(L0,J,IP0,K1,I1)
        YPO(2)=UEA(L0,J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=UEA(L0,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=UEA(L0,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        BB(1)=LINEAR( X, Y, 2, RL )      

      X(1)=AIM(K0)
      X(2)=AIM(K1)
      Y(1)=AA(1)
      Y(2)=BB(1)
      AB(1)=LINEAR( X, Y, 2, RI )

      IF(NRATN.NE.1) THEN    
      X(1)=ARE(I0)
      X(2)=ARE(I1)

      IF(keyLS.EQ.1) THEN
        YPO(1)=UEA(L1,J,IP0,K0,I0)
        YPO(2)=UEA(L1,J,IP1,K0,I0)
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UEA(L1,J,IP0,K0,I1)
        YPO(2)=UEA(L1,J,IP1,K0,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=UEA(L1,J,1:KN1,K0,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=UEA(L1,J,1:KN1,K0,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        CC(1)=LINEAR( X, Y, 2, RL )

      IF(keyLS.EQ.1) THEN
        YPO(1)=UEA(L1,J,IP0,K1,I0)
        YPO(2)=UEA(L1,J,IP1,K1,I0) 
        Y(1)=LINEAR( XPO, YPO, 2, PO )
        YPO(1)=UEA(L1,J,IP0,K1,I1)
        YPO(2)=UEA(L1,J,IP1,K1,I1)
        Y(2)=LINEAR( XPO, YPO, 2, PO )
	  ELSE
	    YYS1(1:KN1)=UEA(L1,J,1:KN1,K1,I0)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(1)=YFIT
	    YYS1(1:KN1)=UEA(L1,J,1:KN1,K1,I1)
	    key_spln=0
        CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
        Y(2)=YFIT
	  ENDIF
        DD(1)=LINEAR( X, Y, 2, RL )     

      X(1)=AIM(K0)
      X(2)=AIM(K1)
      Y(1)=CC(1)
      Y(2)=DD(1)
      CD(1)=LINEAR( X, Y, 2, RI )

      X(1)=RATIO(L0)
      X(2)=RATIO(L1)
      Y(1)=AB(1)
      Y(2)=CD(1)
      sumUSEA=sumUSEA+LINEAR( X, Y, 2, RRATN1 )*RDc(IR)
      ELSEIF(NRATN.EQ.1) THEN
      sumUSEA=AB(1)*RDc(IR)      
      ENDIF
      ENDDO ! IR KR

      USEA(J,I)=sumUSEA*WAVEL/WL/sumRD     
      
      ENDDO ! J 2

      ENDDO ! I KN            
      
	RETURN 
	END SUBROUTINE USU_LS

!c **************************************************************** 
      SUBROUTINE USU_LS_RD(key_RD,keyEL,keySUB,keyLS,KR,R,RD
     &                 ,RATIO,NRATN
     &                 ,KN1,grid1,KM,ANGLE,WAVEL,KRE,KIM,ARE,AIM
     &                 ,KN,grid,RN,RK,WL) 
      use alloc1
      USE mo_par_DLS
!	  use mod_os
	  use interpl_spline
		use intrpl_linear
      real*4 R(KRpar), RATIO(KR1par), RRATN
      dimension grid(KNpar),RD(KRpar)
      dimension grid1(KN1par)
     &         , ANGLE(KMpar)
      COMMON /US1/ US11(KMpar,KNpar) 
      COMMON /US2/ US12(KMpar,KNpar)
      COMMON /US3/ US22(KMpar,KNpar)
      COMMON /US4/ US33(KMpar,KNpar)
      COMMON /US5/ US34(KMpar,KNpar)
      COMMON /US6/ US44(KMpar,KNpar)
      COMMON /US0/ USEA(2,KNpar)
      dimension RDc(KR),AA(6),BB(6),AB(6),sumUS(6)
      dimension XPO(2), YPO(2), X(2), Y(2)
     &         ,ARE(KREpar), AIM(KIMpar)
!      real LINEAR		!AH original
!c **  for SPLINE subroutine

      INTEGER           :: key_spln,keyEL,keySUB,keyLS
      DOUBLE PRECISION  :: XXS1(KN1par),YYS1(KN1par)
      DOUBLE PRECISION  :: XARG,YFIT
	DOUBLE PRECISION  :: KS1(KN1par+4),CS1(KN1par+4)

      PI=ACOS(-1.)
      PI2=2.*PI
!cl      cinv=WAVEL/WL  ! invariant for dV(..)/dlnr
	  
!cl      IF(key_f11.eq.1) THEN
!cl	 KEL=1
!cl	ELSE
!cl       IF(key_f344.eq.0) THEN
!cl	 KEL=6
!cl	 ELSE
!cl	 KEL=4
!cl	 ENDIF
!cl	ENDIF 

      IF(key_RD.eq.2) then
!c 
!c*** RECALCULATE ASPECT RATIO DISTRIBUTION (RDc()=SAREA/VOLUME)
!c*** RDc()=RD()/RDc(); sumRD=sum(RDc())
!c
!c ** OBLATE
		do IR=1,KR
			if(R(IR).lt.1.) then              
				E=SQRT( 1.-R(IR)*R(IR) )
				IF((1.+E)/(1.-E).LT.1.0E-30)THEN
					XA1=LOG(1.0E-30)/E
					ELSE
					xa1=LOG((1.+E)/(1.-E))/E
				ENDIF
				RDc(IR)=1.5*(R(IR)**(-2./3.)+
     &            0.5*xa1*R(IR)**(4./3.))
!c ** PROLATE            
			elseif(R(IR).gt.1.) then          
				E=SQRT( 1.-1./R(IR)/R(IR) )
				xa2=ASIN(E)/E
				RDc(IR)=1.5*(R(IR)**(-2./3.)+
     &               xa2*R(IR)**(1./3.))
!c ** SPHERE
			elseif(R(IR).eq.1.) then             
				RDc(IR)=3.
			endif ! R()
!c ** WRITE ASPECT RATIO DISTRIBUTION 
!c          write(*,*) 'R=',R(IR),' B=',RDc(IR),
!c     &  ' 1/B=',1./RDc(IR),' RD=',RD(IR)

		enddo ! IR
		RDc(:KR)=RD(:KR)/RDc(:KR)
      ENDIF ! key_RD

      IF(key_RD.eq.1) RDc(:KR)=RD(:KR)
!c
!c ** LOG(U...)
!c
!cl   moved to subroutine MATRIX_FIX, under if(key.eq.4), RETURN
!cl      if(NDP4.eq.0) then
!cl      U11(1:KR,1:KM,1:KN1,1:KIM,1:KRE)=
!cl     &     LOG( U11(1:KR,1:KM,1:KN1,1:KIM,1:KRE) )
!cl      if(keyEL.gt.2)
!cl     &         U22(1:KR,1:KM,1:KN1,1:KIM,1:KRE)=
!cl     &         LOG( U22(1:KR,1:KM,1:KN1,1:KIM,1:KRE) )
!cl      if(keyEL.gt.3) then
!cl      DO J=1,KM
!cl      IF(ANGLE(J).LE.40.) 
!cl     &     U33(1:KR,J,1:KN1,1:KIM,1:KRE)=
!cl     &         LOG( U33(1:KR,J,1:KN1,1:KIM,1:KRE) )
!cl      ENDDO ! J
!cl      endif
!cl      if(keyEL.gt.5) then
!cl      DO J=1,KM
!cl      IF(ANGLE(J).LE.50.) 
!cl     &     U44(1:KR,J,1:KN1,1:KIM,1:KRE)=
!cl     &         LOG( U44(1:KR,J,1:KN1,1:KIM,1:KRE) )
!clcl       write(*,*) 'U34 before intrpl orgnl:',U34(1,j,1,1,1)
!cl      ENDDO ! J
!cl      endif
!cl      endif ! NDP4

	sumRD=sum(RDc(:KR))
	  	  	  
	if(keySUB.eq.0) then
		do IR=1,KR 
			write(*,'(''R='',f8.4,4x,'' RDc='',e13.5)') R(IR),RDc(IR)
		enddo ! IR
		write(*,'(''sumRD='',e13.5)') sumRD
	endif
	
	RL=RN
	RI=RK
	I0=0
	I1=0
	K0=0
	K1=0
	DO I=1,KRE-1
		IF(RL.GE.ARE(I).AND.RL.LE.ARE(I+1)) THEN
			I0=I
			I1=I+1
		ENDIF
	ENDDO

	DO I=1,KIM-1
		IF(RI.GE.AIM(I).AND.RI.LE.AIM(I+1)) THEN
			K0=I
			K1=I+1
		ENDIF
	ENDDO

	IF(RL.LE.ARE(1)) THEN
		if(keySUB.eq.0) then
			IF(RL.LT.ARE(1)) THEN
				WRITE(*,*) 'n=',RN,' is out of the range:',
     &                 ARE(1),'< n <',ARE(KRE)
				WRITE(*,*) 'n has been changed',RN,' => ',ARE(1)
			ENDIF
		endif
		I0=1
		I1=2
		RN=ARE(1)
		RL=ARE(1)
	ENDIF

	IF(RL.GE.ARE(KRE)) THEN
		if(keySUB.eq.0) then
			IF(RL.GT.ARE(KRE)) THEN
				WRITE(*,*) 'n=',RN,' is out of the range:',
     &                 ARE(1),'< n <',ARE(KRE)
				WRITE(*,*) 'n has been changed',RN,' => ',ARE(KRE)
			ENDIF
		endif
		I0=KRE-1
		I1=KRE
		RN=ARE(KRE)
		RL=ARE(KRE)
	ENDIF

	IF(RI.LE.AIM(1)) THEN
		if(keySUB.eq.0) then
			IF(RI.LT.AIM(1)) THEN
				WRITE(*,*) 'k=',RK,' is out of the range:',
     &                 AIM(1),'< k <',AIM(KIM)
				WRITE(*,*) 'k has been changed',RK,' => ',AIM(1)
			ENDIF
		endif
		K0=1
		K1=2
		RK=AIM(1)
		RI=AIM(1)
	ENDIF

	IF(RI.GE.AIM(KIM)) THEN
		if(keySUB.eq.0) then
			IF(RI.GT.AIM(KIM)) THEN
				WRITE(*,*) 'k=',RK,' is out of the range:',
     &                 AIM(1),'< k <',AIM(KIM)
				WRITE(*,*) 'k has been changed',RK,' => ',AIM(KIM)
			ENDIF
		endif
	
		K0=KIM-1
		K1=KIM
		RK=AIM(KIM)
		RI=AIM(KIM)
	ENDIF

!C      write(*,*) I0,I1,K0,K1,' I0,I1,K0,K1'
!C      WRITE(*,*) ARE(I0),ARE(I1),' ARE'
!C      WRITE(*,*) AIM(K0),AIM(K1),' AIM'
	DO I=1,KN
		IP0=0
		IP1=0
		PO=grid(I)*PI2/WL            
	    DO IP=1,KN1-1
			PO1=grid1(IP)  *PI2/WAVEL   
			PO2=grid1(IP+1)*PI2/WAVEL 
			IF(PO.GE.PO1.AND.PO.LE.PO2) THEN
				IP0=IP
				IP1=IP+1
			ENDIF
			IF(PO.LE.(grid1(1)*PI2/WAVEL)) THEN
				IP0=1
				IP1=2
			ENDIF
			IF(PO.GE.(grid1(KN1)*PI2/WAVEL)) THEN
				IP0=KN1-1
				IP1=KN1
			ENDIF      
		ENDDO ! IP
		IF(keyLS.EQ.1) THEN
			XPO(1)=grid1(IP0)*PI2/WAVEL
			XPO(2)=grid1(IP1)*PI2/WAVEL
		ELSE
			XXS1(1:KN1)=grid1(1:KN1)*PI2/WAVEL
			XARG=PO
		ENDIF

		IF(keyEL.gt.0) THEN
!c
!c **  SCATTERING MATRIX ELEMENTS
!c
			DO J=1,KM      ! angle loop
				sumUS(:6)=0.
				DO IR=1,KR     ! axis ratio loop
					IF(RATIO(IR).NE.R(IR)) THEN
						WRITE(*,*) 'key_RD1=2 and R=',R(IR),
     &					' .NE. RATIO=',RATIO(IR)
						WRITE(*,*) 'STOP in matrix_intrpl_...f'
						STOP
					ENDIF
					X(1)=ARE(I0)
					X(2)=ARE(I1)
!c
!c ** U11     AA(1)
!c
					IF(keyLS.EQ.1) THEN
						YPO(1)=U11(IR,J,IP0,K0,I0)
						YPO(2)=U11(IR,J,IP1,K0,I0)
						Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
						YPO(1)=U11(IR,J,IP0,K0,I1)
						YPO(2)=U11(IR,J,IP1,K0,I1)
						Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
					ELSE
						YYS1(1:KN1)=U11(IR,J,1:KN1,K0,I0)
						key_spln=0
						CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &			      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
						Y(1)=EXP(YFIT)
						YYS1(1:KN1)=U11(IR,J,1:KN1,K0,I1)
						key_spln=0
						CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &					,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
						Y(2)=EXP(YFIT)
					ENDIF
					AA(1)=LINEAR( X, Y, 2, RL )
!c!c ** U12     AA(2)
!c
					if(keyEL.gt.1) then
						IF(keyLS.EQ.1) THEN
							YPO(1)=U12(IR,J,IP0,K0,I0)
							YPO(2)=U12(IR,J,IP1,K0,I0)
							Y(1)=LINEAR( XPO, YPO, 2, PO )
							YPO(1)=U12(IR,J,IP0,K0,I1)
							YPO(2)=U12(IR,J,IP1,K0,I1)
							Y(2)=LINEAR( XPO, YPO, 2, PO )
						ELSE
							YYS1(1:KN1)=U12(IR,J,1:KN1,K0,I0)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(1)=YFIT
							YYS1(1:KN1)=U12(IR,J,1:KN1,K0,I1)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(2)=YFIT
						ENDIF
						AA(2)=LINEAR( X, Y, 2, RL )
					endif
!c
!c ** U22     AA(3)
!c
					if(keyEL.gt.2) then      
						IF(keyLS.EQ.1) THEN
							YPO(1)=U22(IR,J,IP0,K0,I0)
							YPO(2)=U22(IR,J,IP1,K0,I0)
							Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
							YPO(1)=U22(IR,J,IP0,K0,I1)
							YPO(2)=U22(IR,J,IP1,K0,I1)
							Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
						ELSE
							YYS1(1:KN1)=U22(IR,J,1:KN1,K0,I0)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(1)=EXP(YFIT)
							YYS1(1:KN1)=U22(IR,J,1:KN1,K0,I1)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(2)=EXP(YFIT)
						ENDIF
						AA(3)=LINEAR( X, Y, 2, RL )
					endif
!c
!c ** U33     AA(4)
!c
					if(keyEL.gt.3) then
						IF(keyLS.EQ.1) THEN
							YPO(1)=U33(IR,J,IP0,K0,I0)
							YPO(2)=U33(IR,J,IP1,K0,I0)
							Y(1)=LINEAR( XPO, YPO, 2, PO )
							YPO(1)=U33(IR,J,IP0,K0,I1)
							YPO(2)=U33(IR,J,IP1,K0,I1)
							Y(2)=LINEAR( XPO, YPO, 2, PO )
						ELSE
							YYS1(1:KN1)=U33(IR,J,1:KN1,K0,I0)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(1)=YFIT
							YYS1(1:KN1)=U33(IR,J,1:KN1,K0,I1)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(2)=YFIT
						ENDIF
						IF(ANGLE(J).LE.40.) THEN
							Y(1)=EXP(Y(1))  
							Y(2)=EXP(Y(2)) 
						ENDIF ! ANGLE
						AA(4)=LINEAR( X, Y, 2, RL )
					endif
!c
!c ** U34     AA(5)
!c
					if(keyEL.gt.4) then
						IF(keyLS.EQ.1) THEN
							YPO(1)=U34(IR,J,IP0,K0,I0)
							YPO(2)=U34(IR,J,IP1,K0,I0)
							Y(1)=LINEAR( XPO, YPO, 2, PO )
							YPO(1)=U34(IR,J,IP0,K0,I1)
							YPO(2)=U34(IR,J,IP1,K0,I1)
							Y(2)=LINEAR( XPO, YPO, 2, PO )
						ELSE
							YYS1(1:KN1)=U34(IR,J,1:KN1,K0,I0)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(1)=YFIT
							YYS1(1:KN1)=U34(IR,J,1:KN1,K0,I1)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(2)=YFIT
						ENDIF
						AA(5)=LINEAR( X, Y, 2, RL )
					endif
!c
!c ** U44     AA(6)
!c
					if(keyEL.gt.5) then
						IF(keyLS.EQ.1) THEN
							YPO(1)=U44(IR,J,IP0,K0,I0)
							YPO(2)=U44(IR,J,IP1,K0,I0)
							Y(1)=LINEAR( XPO, YPO, 2, PO )
							YPO(1)=U44(IR,J,IP0,K0,I1)
							YPO(2)=U44(IR,J,IP1,K0,I1)
							Y(2)=LINEAR( XPO, YPO, 2, PO )
						ELSE
							YYS1(1:KN1)=U44(IR,J,1:KN1,K0,I0)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(1)=YFIT
							YYS1(1:KN1)=U44(IR,J,1:KN1,K0,I1)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(2)=YFIT
						ENDIF
						IF(ANGLE(J).LE.50.) THEN
							Y(1)=EXP(Y(1))  
							Y(2)=EXP(Y(2))
						ENDIF ! ANGLE
						AA(6)=LINEAR( X, Y, 2, RL )
					endif  
!c
!c ** U11     BB(1)
!c
					IF(keyLS.eq.1) THEN
						YPO(1)=U11(IR,J,IP0,K1,I0)
						YPO(2)=U11(IR,J,IP1,K1,I0) 
						Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
						YPO(1)=U11(IR,J,IP0,K1,I1)
						YPO(2)=U11(IR,J,IP1,K1,I1)
						Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
					ELSE
						YYS1(1:KN1)=U11(IR,J,1:KN1,K1,I0)
						key_spln=0
						CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &			      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
						Y(1)=EXP(YFIT)
						YYS1(1:KN1)=U11(IR,J,1:KN1,K1,I1)
						key_spln=0
						CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &			      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
						Y(2)=EXP(YFIT)
					ENDIF
					BB(1)=LINEAR( X, Y, 2, RL )
!c
!c ** U12     BB(2)
!c
					if(keyEL.gt.1) then
						IF(keyLS.EQ.1) THEN
							YPO(1)=U12(IR,J,IP0,K1,I0)
							YPO(2)=U12(IR,J,IP1,K1,I0) 
							Y(1)=LINEAR( XPO, YPO, 2, PO )
							YPO(1)=U12(IR,J,IP0,K1,I1)
							YPO(2)=U12(IR,J,IP1,K1,I1)
							Y(2)=LINEAR( XPO, YPO, 2, PO )
						ELSE
							YYS1(1:KN1)=U12(IR,J,1:KN1,K1,I0)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(1)=YFIT
							YYS1(1:KN1)=U12(IR,J,1:KN1,K1,I1)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				      ,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(2)=YFIT
						ENDIF
						BB(2)=LINEAR( X, Y, 2, RL )
					endif
!c
!c ** U22     BB(3)
!c
					if(keyEL.gt.2) then
						IF(keyLS.EQ.1) THEN
							YPO(1)=U22(IR,J,IP0,K1,I0)
							YPO(2)=U22(IR,J,IP1,K1,I0) 
							Y(1)=EXP(LINEAR( XPO, YPO, 2, PO ))
							YPO(1)=U22(IR,J,IP0,K1,I1)
							YPO(2)=U22(IR,J,IP1,K1,I1)
							Y(2)=EXP(LINEAR( XPO, YPO, 2, PO ))
						ELSE
							YYS1(1:KN1)=U22(IR,J,1:KN1,K1,I0)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &						,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(1)=EXP(YFIT)
							YYS1(1:KN1)=U22(IR,J,1:KN1,K1,I1)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &						,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(2)=EXP(YFIT)
						ENDIF
						BB(3)=LINEAR( X, Y, 2, RL )
					endif
!c
!c ** U33     BB(4)
!c
					if(keyEL.gt.3) then
						IF(keyLS.EQ.1) THEN
							YPO(1)=U33(IR,J,IP0,K1,I0)
							YPO(2)=U33(IR,J,IP1,K1,I0) 
							Y(1)=LINEAR( XPO, YPO, 2, PO )
							YPO(1)=U33(IR,J,IP0,K1,I1)
							YPO(2)=U33(IR,J,IP1,K1,I1)
							Y(2)=LINEAR( XPO, YPO, 2, PO )
						ELSE
							YYS1(1:KN1)=U33(IR,J,1:KN1,K1,I0)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &						,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(1)=YFIT
							YYS1(1:KN1)=U33(IR,J,1:KN1,K1,I1)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &						,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(2)=YFIT
						ENDIF
						IF(ANGLE(J).LE.40.) THEN
							Y(1)=EXP(Y(1))  
							Y(2)=EXP(Y(2)) 
						ENDIF ! ANGLE
						BB(4)=LINEAR( X, Y, 2, RL )
					endif
!c
!c ** U34     BB(5)
!c
					if(keyEL.gt.4) then
						IF(keyLS.EQ.1) THEN
							YPO(1)=U34(IR,J,IP0,K1,I0)
							YPO(2)=U34(IR,J,IP1,K1,I0) 
							Y(1)=LINEAR( XPO, YPO, 2, PO )
							YPO(1)=U34(IR,J,IP0,K1,I1)
							YPO(2)=U34(IR,J,IP1,K1,I1)
							Y(2)=LINEAR( XPO, YPO, 2, PO )
						ELSE
							YYS1(1:KN1)=U34(IR,J,1:KN1,K1,I0)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &						,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(1)=YFIT
							YYS1(1:KN1)=U34(IR,J,1:KN1,K1,I1)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &						,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(2)=YFIT
						ENDIF
						BB(5)=LINEAR( X, Y, 2, RL )
					endif
!c
!c ** U44     BB(6)
!c
					if(keyEL.gt.5) then
						IF(keyLS.EQ.1) THEN
							YPO(1)=U44(IR,J,IP0,K1,I0)
							YPO(2)=U44(IR,J,IP1,K1,I0) 
							Y(1)=LINEAR( XPO, YPO, 2, PO )
							YPO(1)=U44(IR,J,IP0,K1,I1)
							YPO(2)=U44(IR,J,IP1,K1,I1)
							Y(2)=LINEAR( XPO, YPO, 2, PO )
						ELSE
							YYS1(1:KN1)=U44(IR,J,1:KN1,K1,I0)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &						,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(1)=YFIT
							YYS1(1:KN1)=U44(IR,J,1:KN1,K1,I1)
							key_spln=0
							CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &						,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
							Y(2)=YFIT
						ENDIF
						IF(ANGLE(J).LE.50.) THEN
							Y(1)=EXP(Y(1))  
							Y(2)=EXP(Y(2))	  
						ENDIF ! ANGLE
						BB(6)=LINEAR( X, Y, 2, RL )
					endif 

					X(1)=AIM(K0)
					X(2)=AIM(K1)

					DO II=1,keyEL
						Y(1)=AA(II)
						Y(2)=BB(II)
						AB(II)=LINEAR( X, Y, 2, RI )
						sumUS(II)=sumUS(II)+AB(II)*RDc(IR)
					ENDDO ! II

				ENDDO ! IR RD

				US11(J,I)=sumUS(1)/WL/sumRD
				if(keyEL.gt.1) US12(J,I)=sumUS(2)/WL/sumRD
				if(keyEL.gt.2) US22(J,I)=sumUS(3)/WL/sumRD
				if(keyEL.gt.3) US33(J,I)=sumUS(4)/WL/sumRD
				if(keyEL.gt.4) US34(J,I)=sumUS(5)/WL/sumRD
				if(keyEL.gt.5) US44(J,I)=sumUS(6)/WL/sumRD

			ENDDO   ! J KM
		ENDIF ! keyEL>0
!c
!c ** EXTINCTION & ABSORPTION
!c
		DO J=1,2
			sumUSEA=0.
			DO IR=1,KR

!cl	XXS1(1:KN1)=grid1(1:KN1)*PI2/WAVEL
!cl	XARG=PO
!cl      key_spln=0

				X(1)=ARE(I0)
				X(2)=ARE(I1)

				IF(keyLS.EQ.1) THEN      
					YPO(1)=UEA(IR,J,IP0,K0,I0)
					YPO(2)=UEA(IR,J,IP1,K0,I0)
					Y(1)=LINEAR( XPO, YPO, 2, PO )
					YPO(1)=UEA(IR,J,IP0,K0,I1)
					YPO(2)=UEA(IR,J,IP1,K0,I1)
					Y(2)=LINEAR( XPO, YPO, 2, PO )
				ELSE
					YYS1(1:KN1)=UEA(IR,J,1:KN1,K0,I0)
					key_spln=0
					CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
					Y(1)=YFIT
					YYS1(1:KN1)=UEA(IR,J,1:KN1,K0,I1)
					key_spln=0
					CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
					Y(2)=YFIT
				ENDIF
				AA(1)=LINEAR( X, Y, 2, RL )

				IF(keyLS.EQ.1) THEN
					YPO(1)=UEA(IR,J,IP0,K1,I0)
					YPO(2)=UEA(IR,J,IP1,K1,I0)
					Y(1)=LINEAR( XPO, YPO, 2, PO )
					YPO(1)=UEA(IR,J,IP0,K1,I1)
					YPO(2)=UEA(IR,J,IP1,K1,I1)
					Y(2)=LINEAR( XPO, YPO, 2, PO )
				ELSE
					YYS1(1:KN1)=UEA(IR,J,1:KN1,K1,I0)
					key_spln=0
					CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
					Y(1)=YFIT
					YYS1(1:KN1)=UEA(IR,J,1:KN1,K1,I1)
					key_spln=0
					CALL intrpl_spline(KN1,XXS1(1:KN1),YYS1(1:KN1)
     &				,XARG,YFIT,key_spln,KS1(1:KN1+4),CS1(1:KN1+4))
					Y(2)=YFIT
				ENDIF
				BB(1)=LINEAR( X, Y, 2, RL )      

				X(1)=AIM(K0)
				X(2)=AIM(K1)
				Y(1)=AA(1)
				Y(2)=BB(1)
				AB(1)=LINEAR( X, Y, 2, RI )
				sumUSEA=sumUSEA+AB(1)*RDc(IR)
			ENDDO ! IR KR

			USEA(J,I)=sumUSEA*WAVEL/WL/sumRD 
c			write(250,*)sumUSEA,WAVEL,WL,SUMRD    
      
		ENDDO ! J 2

      ENDDO ! I KN            
      
	RETURN 
	END SUBROUTINE USU_LS_RD
!c***************************************

