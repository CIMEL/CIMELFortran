module intrpl_linear
	contains
!c **************************************************************** c
      FUNCTION LINEAR( X, Y, M, X1 )
!c ***************************************************************** c
!c **       Linear interpolation function.                        ** c
!c ***************************************************************** c
      IMPLICIT  REAL   (A-H, O-Z)
      REAL      X( * ), Y( * )
	  REAL		::	LINEAR
!C
      ind=0
      IF ( X1.LT.X(1) )  THEN
         LINEAR = Y(1) - ( X1-X(1) )*( Y(2)-Y(1) )/( X(2)-X(1) )
      ELSE IF ( X1.GT.X(M) )  THEN
         LINEAR = Y(M) + ( X1-X(M) )*( Y(M)-Y(M-1) )/( X(M)-X(M-1) )
      ELSE
      DO N=2,M
       IF ( X1.GE.X(N-1).AND.X1.LE.X(N).AND.X(N-1).NE.X(N) ) THEN
       LINEAR = Y(N-1) + ( X1-X(N-1) )*( Y(N)-Y(N-1) )/		&
                                             ( X(N)-X(N-1) )
!c         IF ( X1.GE.X(N-1) .AND. X1.LE.X(N) ) LINEAR = Y(N-1) +
!c     $         ( X1-X(N-1) )*( Y(N)-Y(N-1) )/( X(N)-X(N-1) )
       ind=ind+1
       ENDIF
!C*****************************
        IF(X1.EQ.X(N-1)) LINEAR =Y(N-1)
        IF(X1.EQ.X(N))   LINEAR =Y(N)
!C*****************************

		if(ind.eq.1) goto 10
      ENDDO ! N
      ENDIF

10    RETURN
      END FUNCTION LINEAR
end module intrpl_linear
