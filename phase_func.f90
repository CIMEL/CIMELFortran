module phase_func
     contains
!c **************************************************************** 	 
      subroutine SINT(X1, Y1, KM, xnorm)
      use intrpl_linear
!c ** Simpson integration
!c ** xh - angle step (degree)
      dimension X1( KM ), Y1( KM ), Y(2)
!      real LINEAR		!AH original
      real xnorm
      KSIMP=721
      xh=180./float(KSIMP-1)
      KSIMP1=(180.-X1(1))/xh+1
!c	write(*,*) 'KSIMP1=',KSIMP1
	pi=ACOS(-1.)
      Y(1)=Y1(1)
	Y(2)=Y1(KM)
      Y1(1:KM)=LOG(Y1(1:KM))
	      
	xnorm=0.
	XA=X1(1)
      do IK=1,KSIMP1
       II=IK/2
       IF(IK.EQ.1.OR.IK.EQ.KSIMP) THEN
	  if(IK.eq.1) then
	   F11=1./3.*Y(1)
	  else
         F11=1./3.*Y(2)
	  endif
	 GOTO 1
	 ENDIF
       IF(II*2.LT.IK) THEN
	 F11=2./3.*EXP(LINEAR(X1, Y1, KM, XA))
	 GOTO 1
	 ENDIF
       IF(II*2.EQ.IK) THEN
	 F11=4./3.*EXP(LINEAR(X1, Y1, KM, XA))
	 GOTO 1
	 ENDIF
1      CONTINUE
       xnorm=xnorm+F11*SIN(XA*pi/180.)
	 XA=XA+xh
	enddo ! IK
	xnorm=0.5*xnorm*xh*pi/180.
!cl      write(*,'(''xnorm='',e13.5)') xnorm
	Y1(1:KM)=EXP(Y1(1:KM))/xnorm

	return
	end subroutine SINT
!c
!c ***************************************************************** c
      subroutine ASYPAR(X1, Y1, KM, g)

      use intrpl_linear
!c ** Asymmetry parameter
!c ** xh - angle step (degree)
      dimension X1( KM ), Y1( KM ), Y(2)
!      real,external	:: LINEAR
      real g, XX
      KSIMP=721
      xh=180./float(KSIMP-1)
	pi=ACOS(-1.)
      xpi=pi/180.
      Y(1)=Y1(1)
	Y(2)=Y1(KM)
      Y1(1:KM)=LOG(Y1(1:KM))	      
	g=0.
	XA=0.
      do IK=1,KSIMP
       II=IK/2
       IF(IK.EQ.1.OR.IK.EQ.KSIMP) THEN
	  if(IK.eq.1) then
	   F11=1./3.*Y(1)
	  else
         F11=1./3.*Y(2)
	  endif
	 GOTO 1
	 ENDIF
       IF(II*2.LT.IK) THEN
	 F11=2./3.*EXP(LINEAR(X1, Y1, KM, XA))
	 GOTO 1
	 ENDIF
       IF(II*2.EQ.IK) THEN
	 F11=4./3.*EXP(LINEAR(X1, Y1, KM, XA))
	 GOTO 1
	 ENDIF
1      CONTINUE
       XX=XA*xpi
       g=g+F11*SIN(XX)*COS(XX)
	 XA=XA+xh
	enddo ! IK
	g=0.5*g*(xh*xpi)

	Y1(1:KM)=EXP(Y1(1:KM))

	return
	end subroutine ASYPAR
!c
end module phase_func