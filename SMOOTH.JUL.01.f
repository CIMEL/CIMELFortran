      SUBROUTINE SMOOTHTERM(KN,IPAR,ISTAR,NPAR,IO,GSM,
     &XX,KSM,SM)
C**************************************************
C  THIS SUBROUTINE CALCULATES  MATRIX DESCCRIBING
C  COMPLETE SMOOTHNESS TERM (i.e. includes all smoothnesses:
C  size distribution, refractive index, etc.)
C    SUM(gamma(i)*S**T*S)       
C**************************************************
C  INPUT:
C     KN   I        - number of columns
C     IPAR I        - number of inidvidual smoothness terms
C                    included in complete smoothness term
C     ISTAR(IPAR) I - index first parameter in AP corresponding
C                     each retrieved characterisitic (size distr, 
C                     Real part of ref index, Imag. part, etc.)
C     NPAR(IPAR) I  - total number of parameters in AP corresponding
C                     each retrieved characterisitic (size distr, 
C                     real part of ref index, Imag. part, etc.)
C     IO(IPAR)   I  - order of smoothing differences used 
C                     for each retrieved characterisitic (size distr, 
C                     real part of ref index, Imag. part, etc.)
C     GSM(IPAR)  R  - values of Lagrange parameters used 
C                     for each retrieved characterisitic (size distr,
C     XX(IPAR,KN)  R   - the values of ordiantes Xi of the grid point 
C                     for each smoothed characterisitic 
C  OUTPUT:
C      KSM       I  - total number of equations used for
C                     smoothness constrains
C      SM R(KN,KN)  - MATRIX DESCCRIBING COMPLETE SMOOTHNESS TERM
C                     SUM(gamma(i)*S**T*S)
C                see Dubovik & King [2000]
C***************************************************
      PARAMETER (KMES=500,KPAR=100)
      DIMENSION SM(KMES,KMES),SM1(KMES,KMES),DIF(KMES,KMES),
     &C(KMES),XX(KPAR,KMES),X(KMES),GSM(KPAR),NPAR(KPAR)
      INTEGER   ISTAR(KPAR),IO(KPAR)
CD      WRITE(*,*) 'in smoothterm'
      KSM=0
      DO I=1,KN
       DO J=1,KN
       SM(J,I)=0.
       ENDDO
      ENDDO
CD       WRITE(*,*) IPAR,' IPAR'
       DO I=1,IPAR
CD       WRITE(*,*) I,' I'
cs       WRITE(*,*) I,GSM(I),' I,GSM'
cs       WRITE(*,*) I,IO(I),' I,IO'
       IF(GSM(I).GT.0) THEN
       DO J=1,NPAR(I)
       X(J)=XX(I,J)
CD       WRITE(777,*) EXP(X(J)),EXP(XX(I,J)), J, 'X(J),  J,' 
       ENDDO
cs       write(6,*)'IN SMOOTH, before DIFERW'
       CALL DIFERW(NPAR(I),IO(I),X,G,DIF)
cs*************Modified by Siniuk******************
cs       IF(I.EQ.5.OR.I.EQ.6)THEN
cs       do is=2,NPAR(I)-IO(I)
cs       do is1=1,NPAR(I)
cs       DIF(is,is1)=0
cs       enddo
cs       enddo
cs       endif
cs**************************************************
cs       write(6,*)'IN SMOOTH, after DIFERW'
CD       WRITE(*,*) G,GSM(I),' G, GSM(I), DIF :'
       IF (G.NE.1) THEN
       AA=SQRT(0.05*0.05/(G*GSM(I))) 
CD ???       WRITE(*,*) AA, I," Norm of differences for IPAR"
       ENDIF     
       KM1=NPAR(I)-IO(I)
       KSM=KSM+KM1
C************modified**by***Siniuk***********
cs       IF(I.EQ.3)THEN
cs       C(1)=1000
cs       C(2)=50
cs       C(3)=20
cs       C(NPAR(I)-IO(I))=10
cs       C(NPAR(I)-1-IO(I))=5
cs       C(NPAR(I)-2-IO(I))=2
cs       DO i1=3,NPAR(I)-IO(I)
cs       C(i1)=1
cs       ENDDO
cs       ELSE
cs       DO i1=1,NPAR(I)-IO(I)
cs       C(i1)=0
cs       ENDDO
cs       ENDIF
cs       do i1=1,KN
cs       write(6,*)i1,C(i1)
cs       enddo
C**********************************************
       CALL SMOOM(KM1,KN,DIF,C,SM1)
cs       WRITE(777,*) 'SM1 :'
cs       DO II=1,NPAR(I)
cs       WRITE(777,111) (SM1(II,JJ),JJ=1,NPAR(I))
cs       ENDDO
       DO I1=1,NPAR(I)
        DO J1=1,NPAR(I)
        SM(ISTAR(I)+I1-1,ISTAR(I)+J1-1)=
     & SM(ISTAR(I)+I1-1,ISTAR(I)+J1-1)+GSM(I)*SM1(I1,J1)
        ENDDO
       ENDDO
       ENDIF
       ENDDO
cs       SM(ISTAR(3),ISTAR(3))=SM(ISTAR(3),ISTAR(3))*1.01
cs       SM(ISTAR(3)-1+NPAR(3),ISTAR(3)-1+NPAR(3))=SM(ISTAR(3)
cs     &-1+NPAR(3),ISTAR(3)-1+NPAR(3))*1.01
       WRITE(777,*) 'SM : '
      DO II=1,KN
      WRITE(777,111) (SM(II,JJ),JJ=1,KN)
      ENDDO
 111     FORMAT(30E21.12)
      RETURN
      END   

      SUBROUTINE SMOOM(KM,KN,DIF,C,SM)
C**************************************************
C  THIS SUBROUTINE CALCULATES "SMOOTHNESS MATRIX":
C           
C**************************************************
C  INPUT:
C        KM  I        - number of lines
C        KN  I        - number of columns
C        DIF R(KN,KN) - matrix of differences
C        C   R(KM)    - vector for putting weights in
C                       smoothness matrix
C  OUTPUT:
C        SM R(KN,KN)  - smoothness matrix
C                       SM=(DIF)**T(C)(DIF)
C        GF           - the parameter coming from
C              DELTA(ordinate), this parameter can be
C              can be used for indicating norm of
C              of dervatives A assumed in Lagrange 
C              parameter: 
C               Gamma=Sigma(mes)**2/(GF*A**2)
C                see Dubovik & King [2000]
C***************************************************
      PARAMETER (KPAR=500)
      DIMENSION DIF(KPAR,KPAR), C(KPAR),SM(KPAR,KPAR)
      DO I=1,KN
C SO FAR no weighting... !!!
C*************modified**by**Siniuk***********
       C(I)=0.
C*********************************************
       DO J=1,KN
        SM(I,J)=0.
       ENDDO
      ENDDO
      IF(C(1).EQ.0) THEN
      DO I =1,KN
       DO I1=1,KN
       AA=0.
        DO J2=1,KM
        AA=AA+DIF(J2,I)*DIF(J2,I1)
        ENDDO
       SM(I,I1)=AA 
       ENDDO     
      ENDDO
      ELSE
      DO I =1,KN
       DO I1=1,KN
       AA=0.
        DO J2=1,KM
        AA=AA+DIF(J2,I)*DIF(J2,I1)*C(J2)
        ENDDO
       SM(I,I1)=AA 
       ENDDO     
      ENDDO
      ENDIF
      RETURN
      END   
      SUBROUTINE DIFERW(KN,IO,X,G,DIF)
C**********************************************************
C*** THIS SUBROUTINE DEFINES MATRIX of N-th DIFFERENCES ***
C*** for the function Y(X) defined in the points Xi     ***
C    INPUT:
C        KN     I   - number of parameters
C        IO     I   - order of differences
C        X(KN)  R   - the values of ordiantes Xi of the ***
C                     grid point
C
C***
C    OUTPUT:
C      G    R   - coefficient realting differences with
C                 derivatives
C            - G.NE.1 if Xi+1-Xi is non constant 
C            - G=1/(A**IO) if Xi+1-Xi=const=A
C              this parameter can be used for indicating norm
C              of dervatives A assumed in Lagrange parameter: 
C               Gamma=Sigma(mes)**2/(GF*A**2)
C                see Dubovik & King [2000]
C      DIF  R(KN-IO,KN) -matrix of IO-th differences     
C****                                                   ***
C**********************************************************
      PARAMETER (KPAR=500)
      DIMENSION DIF(KPAR,KPAR),X(KPAR),DX(KPAR),DIFT(KPAR,KPAR),
     &DX1(KPAR)
cs      write(6,*)'check1'
      DO J=1,KN
       DO I=1,KN
        DIFT(J,I)=0.
        DIF(J,I)=0.
       ENDDO
      ENDDO
      DO J=1,KN
       DIFT(J,J)=1.0
       DX(J)=X(J)
       WRITE(777,*) X(J)
      ENDDO
C*********FOr a priori estimates *************************
cs      write(6,*)'check2'
      IF(IO.EQ.0) THEN
      DO J=1,KN
       DIF(J,J)=1.0
      ENDDO
      G=1.
      GOTO 5
      ENDIF
C**********************************************************
C*** Checking if Xi+1-Xi=const or not                   ***
cs      write(6,*)'check3'
      IF(KN.GT.2) THEN
CD      DO J=2,KN-1
CD      WRITE(777,*) J,KN,IO,
CD     &ABS(((X(J-1)-X(J))-(X(J)-X(J+1)))/(X(J-1)-X(J)))
CD      ENDDO
      DO J=2,KN-1
       IF(ABS(((X(J-1)-X(J))-(X(J)-X(J+1)))/(X(J-1)-X(J)))
     &.GT.1.0e-3) GO TO 1
      ENDDO
      ENDIF
C**********************************************************
C*** Calcualting matrix of differences and G            ***
C***   for  Xi+1-Xi=const                               ***
cs      write(6,*)'check4'
cs      write(6,*)'check41'
      G=1.0/(ABS((X(1)-X(2)))**IO)
cs      write(6,*)'check42'
      WRITE(777,*) G,' G'
      WRITE(777,*) KN,IO,' KN,IO'
cs      write(6,*)'check43'
      DO I=1,KN
        DO J=1,KN
        DIF(I,J)=0.
        ENDDO 
      ENDDO
cs      write(6,*)'check44'
      DO IIO=1,IO 
       DO I=1,KN-IIO
        DO J=1,KN
         DIF(I,J)=DIFT(I,J)-DIFT(I+1,J)
        ENDDO
       ENDDO
       DO J=1,KN
        DO I=1,KN-IIO
         DIFT(I,J)=DIF(I,J)
        ENDDO
       ENDDO
      ENDDO
cs      write(6,*)'check45'
      GO TO 2
C**********************************************************
C*** Calcualting matrix of differences and G            ***
C***   for  Xi+1-Xi non const                           ***
    1 G=1.0
cs      write(6,*)'check5'
      DO I=1,KN
       DX(I)=X(I)
       DX1(I)=X(I)
      ENDDO
      DO IIO=1,IO
             WRITE(777,*) IIO,' IIO'
       DO I=1,KN-IIO
        DO J=1,KN
         DIF(I,J)=1.0/(DX(I)-DX(I+1))*(DIFT(I,J)-DIFT(I+1,J))
        ENDDO
       ENDDO
       DO II=1,KN-IIO
        WRITE(777,111) (DIF(II,JJ),JJ=1,KN)
       ENDDO
       DO I=1,KN-IIO
        DO J=1,KN
         DIFT(I,J)=DIF(I,J)
        ENDDO
       ENDDO
       DO I=KN-IIO+1,KN
        DO J=1,KN
         DIF(I,J)=0.0
         DIFT(I,J)=0.0
        ENDDO
       ENDDO
       DO I=1,KN-IIO
        DX(I)=(DX1(I)+DX1(I+1))/2.0
       ENDDO
       DO I=1,KN-IIO
          DX1(I)=DX(I)
       ENDDO
      ENDDO
    2 DO I=KN-IO+1,KN
cs        write(6,*)'I=',I
        DO J=1,KN
cs        write(6,*)'J=',J
         DIF(I,J)=0.0
         DIFT(I,J)=0.0
        ENDDO
      ENDDO
cs       write(6,*)'check47'
   5    WRITE(777,*) ' DIFT final:'
       WRITE(777,*) IO,' IO'
       DO II=1,KN-IO
        WRITE(777,111) (DIF(II,JJ),JJ=1,KN)
       ENDDO
 111     FORMAT(7E21.12)
   3  RETURN
      END
      SUBROUTINE RES (KM,F,FP,C,AR)
C******************************************************
C*** Subroutine for calcualating residuals ***
C***
C
C   INPUT:
C      KN     I       -number of "meaurements"
C      F     R(KM)    -vector of "measurements"
C      FP    R(KM)    -vector of fitted "measurements"
C      C     R(KM,KM) -matrix inverse to covariance 
C
C***
C  
C   OUTPUT:
C       AR     R      -residual (FP-F)**T C (FP-F)
C*******************************************************
      PARAMETER (KMES=500)
      DIMENSION F( * ),FP( * ),C(KMES , KMES )
C      DIMENSION F( : ),FP( : ),C(:,:)
CD      WRITE(*,*) 'RES' 
Cd      DO JJ=1,2
CD      WRITE(*,*) F(1),FP(1),C(1,1),C(1,2),' in RES'
CD      WRITE(*,*) F(2),FP(2),C(2,1),C(2,2),' in RES'
CD      ENDDO  
      AR=0.
      DO 1 J=1,KM
      AA=0.
      DO 2 I=1,KM
      AA=AA+C(J,I)*(F(I)-FP(I))
CD      IF (I.EQ.5. AND.J.LE.10) WRITE(*,*) AA, C(J,I),
CD     &(F(I)-FP(I)),'AA, C(J,I),
CD     &(F(I)-FP(I))'
    2 CONTINUE
      AR=AR+AA*(F(J)-FP(J))
CD      IF (I.EQ.5. AND.J.EQ.5) WRITE(*,*) AR,AA,(F(J)-FP(J)),' 
CD     &AR, (F(J)-FP(J))'
CD      WRITE(*,*) AR, J,' AR, J'
    1 CONTINUE
CD      WRITE(*,*) AR,' AR-final'
      RETURN
      END
      SUBROUTINE VEC_MATR (KM,KN,C,F,FF)
C******************************************************
C*** Subroutine for calcualating residuals ***
C***
C
C   INPUT:
C      KM     I       -number of lines in matrix
C      KN     I       -number of columns in matrix
C      C     R(KM,KN) -matrix 
C      F     R(KN)    -vector
C
C***
C  
C   OUTPUT:
C      FF     R      -vector C*F
C*******************************************************
      PARAMETER (KMES=500)
      DIMENSION F( * ),FF( * ),C(KMES , KMES)
C      DIMENSION F( : ),FF( : ),C(:,:)  
      AR=0
      DO 1 J=1,KM
      FF(J)=0.
      DO 2 I=1,KN
      FF(J)=FF(J)+C(J,I)*F(I)
    2 CONTINUE
    1 CONTINUE
      RETURN
      END
