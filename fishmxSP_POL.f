      SUBROUTINE FISHMX(KM,KN,U,C,F,FP,UF,FF,GF,AL)
C**************************************************
C  THIS SUBROUTINE CALCULATES "FISHER MATRIX":
C            (U)**T (C) (U)
C                and
C  "gradient" (U)**T (C) (F(p)-F(*))
C                and
C   "residual" (F(p)-F(*))**T (C) (F(p)-F(*))
C**************************************************
C  INPUT:
C        KN  I        - number of lines
C        KM  I        - number of columns
C        U   R(KM,KN) - (derivatives) matrix
C        C   R(KM,KM) - matrix inverse to covariance
C        FP  R(KM)    - P-th approximation of vector F
C        F   R(KM)    - "measurements" vector 
C  OUTPUT:
C         UF R(KN,KN) -"Fisher matrix" normalized by
C                      maximum diagonal ellement
C         FF R(KN)    - "gradient" vector normalized by
C                      maximum diagonal ellement of UF  
C         GF R        -maximum diagonal ellement of 
C                       "Fisher matrix"
C         AL R        - residual
C***************************************************
      PARAMETER (KMES=500,KPAR=100)
      DIMENSION U(KMES,KPAR),C(KMES,KMES),UF(KPAR,KPAR),FF(KPAR)
      DIMENSION F(KMES),FP(KMES)
      DIMENSION U1(KPAR,KMES)
C*** calculating "Fisher matrix" 
CD       WRITE(*,*) 'FISHMX'
CD      DO I=1,KM
CD      WRITE (*,*) C(I,I), I, ' COV'
CD      ENDDO
      GF=1.0
      DO 1 I =1,KN
      DO 1 J1=1,KM
      A=0.
      U1(I,J1)=0.
      DO 3 J=1,KM
   3  A=A+U(J,I)*C(J,J1)
   1  U1(I,J1)=A
      DO 2 I =1,KN
      DO 4 I1=1,KN
      AA=0
      DO 5 J2=1,KM
   5  AA=AA+U1(I,J2)*U(J2,I1)
   4  UF(I,I1)=AA      
   2  CONTINUE
C*** calculating GF
      GF=UF(1,1) 
      DO 6 I=2,KN
   6  IF(ABS(GF).LT.ABS(UF(I,I))) GF=UF(I,I)
CD      WRITE (*,*) GF,' GF'
            GF=1.0
C*** NORMALYZING "Fisher matrix"
CP      DO 7 I =1,KN
CP      DO 7 I1=1,KN
CP      write(*,*) UF(I,I1), I, I1,'   UF'
CP   7  UF(I,I1)=UF(I,I1)/GF
C*** calculating "gradient" FF              
      DO 8 I=1,KN
      A=0
      DO 9 J=1,KM
   9  A=A+U1(I,J)*(FP(J)-F(J))
   		FF(I)=A
*CP      FF(I)=A/GF
   8  CONTINUE
C*** calculating "residual" AL
      AL=0
      DO 11 J1=1,KM
      AL1=0
      DO 10 J=1,KM
   10 AL1=AL1+C(J1,J)*(FP(J)-F(J)) 
      AL=AL+AL1*(FP(J1)-F(J1))
   11 CONTINUE
      RETURN
      END   

      
