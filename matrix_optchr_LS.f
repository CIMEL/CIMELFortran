      SUBROUTINE OPTCHAR(key,key_RD,keyEL,keySUB,keyLS
     &                  ,key_org,key_fx,key_grid1
     &                  ,WL,RN,RK,KN,grid,SD
     &                  ,KR,R,RD,KM,ANGLE,ext,albedo
     &                  ,f11,f12,f22,f33,f34,f44,pomin,pomax
     &                  ,distname_O,distname_F,distname_N,NDP
     &                  ,key_RD1)

!c **************************************************************** c
!c **   08/26/02                                                 ** c
!c **   Subroutine calculates optical characteristics for given  ** c
!c **   size distribution, refractive index, axis ratio          ** c
!c **   distribution and wavelength.                             ** c
!c **                                                            ** c
!c **   In case RN or RK or R() is out of corresponding ranges   ** c 
!c **   subroutine changes RN or RK or R() for edge value and    ** c
!c **   gives optical characteristics for new values             ** c 
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
!c **   keyEL=  0 - do not calculate angular characreristics     ** c
!c **           1 - calculate F11                                ** c
!c **           2 - F11,F12                                      ** c
!c **           3 - F11,F12,F22                                  ** c
!c **           4 - F11,F12,F22,F33                              ** c
!c **           5 - F11,F12,F22,F33,F34                          ** c
!c **           6 - F11,F12,F22,F33,F34,F44                      ** c
!c **   key_org=0 - read original kernels simultaneously make    ** c 
!c **               angle interpolation and calculate opt.char.  ** c
!c **           1 -  -"-, save new kernels in                    ** c
!c **                /NEWORG/ directory, STOP                    ** c
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
!c **   key_grid1 read grid radii and scat.angles which were used** c
!c **             for kernel look up tables or fixed kernels     ** c
!c **          =0 - 'grid1.dat'                                  ** c
!c **           1 - 'grid1.dat.new'                              ** c
!c **   WL   - wavelength                                        ** c
!c **   RN   - real part of the refractive index                 ** c
!c **   RK   - imaginary part of the refractive index            ** c
!c **   KN   - number of grid radii                              ** c
!c **   grid(KN) - grid radii                                    ** c
!c **   SD(KN)   - size distribution for grid radii              ** c
!c **   distname_O - original kernel directory name              ** c
!c **   distname_F - .fix kernel directory name                  ** c
!c **   distname_N - new original kernel directory name          ** c
!c **   KR  - number of axis ratios                              ** c
!c **   R(KR)  - grid axis ratios                                ** c
!c **   RD(KR) - axis ratio distribution for grid axis ratios    ** c
!c **   KM   - number of scattering angles                       ** c
!c **   ANGLE(KM) - scattering angles                            ** c
!c **                                                            ** c
!c ** OUTPUT:                                                    ** c
!c **                                                            ** c
!c **   ext     - extinction                                     ** c
!c **   albedo  - absorption                                     ** c
!c **   f... - scattering matrix elements                        ** c
!c **************************************************************** c
      USE mo_par_DLS
!      use mod_os
      use intrpl_linear
      use phase_func
      integer KN, KM, KR, NDP,
     &       key, key_RD, keyEL, keySUB, keyLS, key_org,key_fx,
     &       key_grid1,key_RD1
!c     =0 original code, !=0 to be used as Subroutine
!c     in inversion code
      real WL, RN, RK, dlnr, xnorm
      dimension grid(KNpar), SD(KNpar),
     &            RD(KRpar), ANGLE(KMpar)
!tl      real*4 R(KRpar)
      real R(KRpar)
      COMMON /US1/ US11(KMpar,KNpar) 
      COMMON /US2/ US12(KMpar,KNpar)
      COMMON /US3/ US22(KMpar,KNpar)
      COMMON /US4/ US33(KMpar,KNpar)
      COMMON /US5/ US34(KMpar,KNpar)
      COMMON /US6/ US44(KMpar,KNpar)
      COMMON /US0/ USEA(2,KNpar)
      dimension f11(KMpar) 
     &         ,f12(KMpar)
     &         ,f22(KMpar)
     &         ,f33(KMpar)
     &         ,f34(KMpar)
     &         ,f44(KMpar)
      real ext, abs, sca, albedo
      dimension X(2), Y(2)
	  real XBLR,XLDR,PI
 !     real LINEAR
      CHARACTER(60) distname_O,distname_F,distname_N
      PI=ACOS(-1.)
!c
!c ** GET MATRICES
!c
!c      write(*,*) 'before USMATRIX '
!c      do itest=1,2  ! delete after test
cxxa
C	WRITE(250,*)'CXXA,NDP',NDP
!CZJ      write(*,*)key,key_RD,keyEL,keySUB,keyLS
      CALL USMATRIX(key,key_RD,keyEL,keySUB,keyLS
     &             ,key_org,key_fx,key_grid1
     &             ,WL,RN,RK,KN,grid
     &             ,KR,R,RD,KM,ANGLE,dlnr,pomin,pomax
     &             ,distname_O,distname_F,distname_N,NDP
     &             ,key_RD1)
C	WRITE(*,*)'CXXA,US11',US11(1,1),USEA(1,1)
!      write(*,*) 'after USMATRIX,NDP=',NDP

!cl         write(*,*) 'in OPTCHAR11: US11=',US11(1,1),
!cl     &    ' US11=',US11(KM,KN)
!c
!c ** CALCULATE APPROXIMATED OPTICAL CHARACTERISTICS      
!c
!cl      f11(:)=0.
!cl      f12(:)=0.
!cl      f22(:)=0.
!cl      f33(:)=0.
!cl      f34(:)=0.
!cl      f44(:)=0.
!cl      ext=0.
!cl      abs=0.
!c      coeff=1.
 
!c	write(*,*) 'in OPTCHAR - SD1: dlnr=',dlnr
!c	write(*,*) SD(1:KN)

      coeff=1.0e+3                  ! for AERONET dV/dlnR
      SD(1:KN)=SD(1:KN)*coeff*dlnr


!c	write(*,*) 'in OPTCHAR - SD2:'
!c	write(*,*) SD(1:KN)
C	write(250,'(2e12.5)')usea
c	write(250,*)sd
      ext=DOT_PRODUCT(USEA(1,1:KN),SD(1:KN))
      abs=DOT_PRODUCT(USEA(2,1:KN),SD(1:KN))
      sca=ext-abs
      albedo=sca/ext

      if(keyEL.gt.0) then
		do j=1,KM
			f11(j)=DOT_PRODUCT(US11(j,1:KN),SD(1:KN))
		enddo ! j
	    if(keySUB.eq.0) f11(1:KM)=f11(1:KM)/sca
	endif 

	if(keyEL.gt.1) then
		do j=1,KM
			f12(j)=DOT_PRODUCT(US12(j,1:KN),SD(1:KN))
		enddo ! j
		if(keySUB.eq.0) f12(1:KM)=-f12(1:KM)/sca/f11(1:KM)
	endif 

	if(keyEL.gt.2) then	    
		do j=1,KM
			f22(j)=DOT_PRODUCT(US22(j,1:KN),SD(1:KN))
		enddo ! j	  
		if(keySUB.eq.0) f22(1:KM)= f22(1:KM)/sca/f11(1:KM)
	endif   
	
	if(keyEL.gt.3) then
		do j=1,KM
			f33(j)=DOT_PRODUCT(US33(j,1:KN),SD(1:KN))
		enddo ! j
		if(keySUB.eq.0) f33(1:KM)= f33(1:KM)/sca/f11(1:KM)
	endif      
		
	if(keyEL.gt.4) then
		do j=1,KM
			f34(j)=DOT_PRODUCT(US34(j,1:KN),SD(1:KN))
		enddo ! j 
		if(keySUB.eq.0) f34(1:KM)= f34(1:KM)/sca/f11(1:KM)
	endif
	
	if(keyEL.gt.5) then
		do j=1,KM
			f44(j)=DOT_PRODUCT(US44(j,1:KN),SD(1:KN))
		enddo ! j
		if(keySUB.eq.0) f44(1:KM)= f44(1:KM)/sca/f11(1:KM)
	endif 

!      if(keySUB.eq.0) then
!		SD(1:KN)=SD(1:KN)/coeff
!      else
!		SD(1:KN)=SD(1:KN)/coeff/dlnr
!      endif
	SD(1:KN)=SD(1:KN)/coeff/dlnr
!c
!c ** SMOOTH f33 and f44 
!c
	if(keyEL.gt.3) then
		j=1
		do while(ANGLE(j).lt.(40.).or.j.gt.KM)
			j1=j
			j=j+1
		enddo
		j=1
		do while(ANGLE(j).lt.(50.).or.j.gt.KM)
			j2=j
			j=j+1
		enddo
		if(j2.gt.j1) then
			X(1)=ANGLE(j1-1)
			X(2)=ANGLE(j2+1)
			Y(1)=f33(j1-1)
			Y(2)=f33(j2+1)
			IF(Y(1).LT.1.0E-30)Y(1)=1.0E-30
			IF(Y(2).LT.1.0E-30)Y(2)=1.0E-30
			Y(1)=LOG(Y(1))
			Y(2)=LOG(Y(2))
			do j=j1,j2
				f33(j)=EXP(LINEAR(X, Y, 2, ANGLE(j)))
			enddo ! j
		endif ! j2&j1
	endif ! keyEL>3
!c
	if(keyEL.gt.5) then
		j=1
		do while(ANGLE(j).lt.(50.).or.j.gt.KM)
			j1=j
			j=j+1
		enddo
		j=1
		do while(ANGLE(j).lt.(60.).or.j.gt.KM)
			j2=j
			j=j+1
		enddo
		if(j2.gt.j1) then
			X(1)=ANGLE(j1-1)
			X(2)=ANGLE(j2+1)
			Y(1)=f44(j1-1)
			Y(2)=f44(j2+1)
			IF(Y(1).LT.1.0E-30)Y(1)=1.0E-30
			IF(Y(2).LT.1.0E-30)Y(2)=1.0E-30
			Y(1)=LOG(Y(1))
			Y(2)=LOG(Y(2))
			do j=j1,j2
				f44(j)=EXP(LINEAR(X, Y, 2, ANGLE(j)))
			enddo ! j
		endif ! j2&j1
	endif ! keyEL>5

!c ** Check f11 norma
!c	
	IF(keyEL.gt.0) THEN
		if((KM.eq.180.or.KM.eq.181).and.
     &		(ANGLE(1).eq.(0.).and.ANGLE(KM).eq.(180.))) then
			if(keySUB.eq.0) then
				call SINT  ( ANGLE, f11, KM, xnorm )
				call ASYPAR( ANGLE, f11, KM, g )
			else ! keySUB=1
				call SINT  ( ANGLE, f11/sca, KM, xnorm )
				call ASYPAR( ANGLE, f11/sca, KM, g )	   
			endif
		endif ! KM 
	ENDIF ! keyEL>0
!c ** WRITE APPROXIMATED OPTICAL CHARACTERISTICS
!c
	IF(keySUB.eq.0) THEN      
		if(NDP.eq.1) then
			open(10,file='sca_mtrx.dat',status='unknown')
			NDP=NDP+1
		else
			open(10,file='sca_mtrx.dat',status='unknown',
     &		position='append')
		  endif

		write(10,*)
!c      write(10,*)'<<<     WELCOME TO NONSPHERICAL',
!c     &      ' AEROSOL WORLD     >>>'
!c      write(10,*)
		if(keyLS.eq.1) then
			write(10,*) 'version: Linear interpolation'
		else
			write(10,*) 'version: Spline interpolation'
		endif
		if(dlnr.lt.1.) write(10,'(A,e12.4,A)') '!!! dlnr=',dlnr,
     &       ' Kernels have been multiplyed by coefficient dlnr'
			write(10,*)'Size distribution:'
			write(10,*) '      r(mkm)             Sd(r)'
			do i=1,KN
				write(10,'(2e16.5)') grid(i),SD(i)
			enddo ! i
			write(10,*)
			if(key_RD1.eq.1) 
     &			 write(10,*)'Axis ratio distribution:'
			if(key_RD1.eq.2) 
     &			 write(10,*)'Axis ratio distribution',
     &			' (look-up-table aspect ratios):'
			write(10,*) '         R                Rd(R)'
			do i=1,KR
				write(10,'(2e16.5)') R(i),RD(i)
			enddo ! i
			write(10,*) 
			write(10,10) WL, RN, RK
			write(10,*)
			if(key_RD.eq.1) then
				write(10,*) '        APPROXIMATED OPTICAL CHARACTERISTICS'
				write(10,*) '                 (volume mixture)'
			endif ! key_RD
			if(key_RD.eq.2) then 
				write(10,*) '        APPROXIMATED OPTICAL CHARACTERISTICS'
				write(10,*) '               (surface area mixture)'
			endif ! key_RD
			write(10,*) 
			write(10,11) ext, abs, sca, albedo
			write(10,*) 
			if(keyEL.eq.1) then
				write(10,'(2x,''ANGLE'',7x,''F11'')') 
				do j=1,KM       
					write(10,12) ANGLE(j),f11(j)
				enddo ! j
			endif

		if(keyEL.eq.2) then
			write(10,'(2x,''ANGLE'',7x,''F11'',7x,''-F12/F11'')') 
			do j=1,KM       
				write(10,12) ANGLE(j),f11(j),f12(j)
			enddo ! j
		endif

		if(keyEL.eq.3) then
			write(10,'(2x,''ANGLE'',7x,''F11'',7x,''-F12/F11'',7x,
     &		''F22/F11'')') 
			do j=1,KM       
				write(10,12) ANGLE(j),f11(j),f12(j),f22(j)
			enddo ! j
		endif

		if(keyEL.eq.4) then
			write(10,'(2x,''ANGLE'',7x,''F11'',7x,''-F12/F11'',7x,
     &		''F22/F11'',7x,''F33/F11'')') 
			do j=1,KM       
				write(10,12) ANGLE(j),f11(j),f12(j),f22(j)
     &                  ,f33(j)
			enddo ! j
		endif

		if(keyEL.eq.5) then
			write(10,'(2x,''ANGLE'',7x,''F11'',7x,''-F12/F11'',7x,
     &		''F22/F11'',7x,''F33/F11'',7x,''F34/F11'')') 
			do j=1,KM       
				write(10,12) ANGLE(j),f11(j),f12(j),f22(j)
     &                  ,f33(j),f34(j)
			enddo ! j
		endif

		if(keyEL.eq.6) then
			write(10,'(2x,''ANGLE'',7x,''F11'',7x,''-F12/F11'',7x,
     &		''F22/F11'',7x,''F33/F11'',7x,''F34/F11'',7x,''F44/F11'')') 
			do j=1,KM     
				write(10,12) ANGLE(j),f11(j),f12(j),f22(j)
     &			,f33(j),f34(j),f44(j)
			enddo ! j
		endif
!cl	write(10,13) xnorm,1.
!c
		IF(keyEL.gt.0) THEN
			if((KM.eq.180.or.KM.eq.181).and.
     &			(ANGLE(1).eq.(0.).and.ANGLE(KM).eq.(180.))) then
		      write(10,'('' asymmetry parameter         ='',e13.4)') g
			  XBLR=4.*PI/(albedo*f11(KM))
	          XLDR=(1.-f22(KM))/(1.+f22(KM))*100.     	   
                write(10,'('' backscatter lidar ratio     ='',e13.4)') XBLR
                write(10,'('' linear depolarization ratio ='',e13.4)') XLDR
	        endif
           ENDIF ! keyEL>0
           close(10)
      ENDIF ! key_SUB=0
13    format('check: f11 norma=',f7.3,'   one=',f7.3) 
10    FORMAT('wavelength=',e11.4,'  n=',e11.4,'  k=',e11.4) 
11    FORMAT('ext=',e13.5,'  abs=',e13.5,
     &       '  sca=',e13.5,'  albedo=',e13.5) 
12    FORMAT(F7.2,6E14.5)
!c      enddo ! itest  - delete after test
      RETURN 
      END SUBROUTINE OPTCHAR

!c ***************************************************************** c
