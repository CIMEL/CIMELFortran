      SUBROUTINE ITERQ(IM,KN,KI,UF,FF,GF,EPSQ,Q0,Q,APQ,NQ,UFI,KN1)    
!C******************************************************
!C  THIS SUBROUTINE IMPLEMENTS q-th (general) ITERATIONS
!C******************************************************
!C  INPUT:
!C         IM    I   IM=1 - linear iterations:
!C             .AND.KI=0 -steepest descent method
!C             .AND.KI=2 - Dub-Oshch with ABS
!C                   IM=2 - matrix inversion
!C                   IM=3 - matrix inversion by SVD
!C                   IM=7 - linear Dubovik's iterations
!C                   IM=8 - linear inversion with SVD
!C             .AND.KI=...-order of inverted matrices
!C         KN   I        - number of parameters
!C         KI   I        - order of approximation if IM=7
!C         KM   I        - number of measurements
!C         UF   R(KN,KN) - "Fisher matrix" normalized by
!C                          maximum diagonal ellement
!C         FF   R(KM)    - "gradient" vector normalized by
!C                          maximum diagonal ellement    
!C         GF   R        - maximum diagonal ellement of 
!C                        "Fisher matrix"
!C         EPSQ R        - defines when to stop itrations  
!C         Q0    R(KN)    - initial guess of solution
!C         NQ   I   NQ<0 -iteraitin stop by EPSQ
!C         NQ   I   NQ>0 -iterations stop after NQ iterations          
!C  OUTPUT:
!C         Q  R(KN)    - solution
!C         APQ  R        - residual prodused by DAP
!C******************************************************
      CHARACTER ERR*64
      PARAMETER (KPAR=100)
      PARAMETER (TOL1=1.e-20)
      DIMENSION UF(KPAR,KPAR),FF(KPAR),Q0(KPAR),Q(KPAR)
!cs      DIMENSION UFI(KPAR,KPAR)
      DIMENSION FFQ(KPAR),QT(KPAR),Q1(KPAR)
      DOUBLE PRECISION UFM(KN,KN),QS(KN,KN),PT(KN,KN),SV(KN),
     &BN(0,KN),QS1(KN,KN),SV1(KN,KN),QS2(KN,KN),
     &WORK(KN1),UFI(KPAR,KPAR),PT1(KN,KN)
! ================ BY DUAN ===================================      
!      DIMENSION AH(KPAR,KPAR)
      REAL*8 AH(KPAR,KPAR), DT
! ================END BY DUAN ===================================      
      REAL FFM(KN),QM(KN),U1(KN,KN)
      DIMENSION APQS(KPAR),AP0S(KPAR),ADQS(KPAR,KPAR),
     &ADQS1(KPAR,KPAR),FFS(KPAR),FFSS(KPAR),ADQS0(KPAR,KPAR),
     &AD(KPAR,KPAR),AD1(KPAR,KPAR),
     &AD0(KPAR,KPAR),QTT(KPAR),UUFF(KPAR,KPAR)
      ERR='  '
!C      	WRITE(*,*) 'ITERQ'
!C      WRITE (*,*) GF, '  GF'
      IMM=0
!C*************************************************************
      IF(IM.EQ.4)THEN
      DO J=1,KN
      DO I=1,KN
      UFM(J,I)=UF(J,I)
!cs      BN(J,I)=UF(J,I)
      ENDDO
      ENDDO
      CALL F02WEF(KN,KN,UFM,KN,0,BN,KN,.TRUE.,QS,KN,SV,.TRUE.,PT,
     *                  KN,WORK,IFAIL)
!cs       write(6,*)'QS'
!cs       do i1=1,KN
!cs       write(6,*)(UFM(i1,i2),i2=1,KN)
!cs       enddo
!cs       write(6,*)'PT'
!cs       do i1=1,KN
!cs       write(6,*)(PT(i2,i1),i2=1,KN)
!cs       enddo
!cs       write(6,*)'SV'
!cs       write(6,*)(SV(i1),i1=1,KN)
!C***********************************************
!cs         write(6,*)'check1'
         do i=1,KN
         do j=1,KN
         QS1(i,j)=UFM(j,i)
         PT1(i,j)=PT(j,i)
         enddo
         enddo
         do i=1,KN
         do j=1,KN
         QS(i,j)=QS1(i,j)
         PT(i,j)=PT1(i,j)
         enddo
         enddo
!cs         write(6,*)'check2'
!C************************************************
         thresh=TOL1
         do j=1,KN
         if(SV(j).lt.thresh)SV(j)=0.
         enddo
         do i=1,KN
         do j=1,KN
         SV1(i,j)=0.
         enddo
         enddo
!cs         write(6,*)'check3'
         do i=1,KN
         if(SV(i).ne.0)SV1(i,i)=1./SV(i)
         enddo
         do i1=1,KN
         do i2=1,KN
         A1=0.
         do i3=1,KN
         A1=A1+SV1(i1,i3)*QS(i3,i2)
         enddo
         QS2(i1,i2)=A1
         enddo
         enddo
!cs         write(6,*)'check4'
!C*************************************************
         do i1=1,KN
         do i2=1,KN
         A1=0.
         do i3=1,KN
         A1=A1+PT(i1,i3)*QS2(i3,i2)
         enddo
         QS1(i1,i2)=A1
         enddo
         enddo
!cs      write(6,*)'SV'
!cs      write(6,*)SV
      do i1=1,KN
      do i2=1,KN
      UFI(i1,i2)=QS1(i1,i2)
      enddo
      enddo 
!cs      do i1=1,KN
!cs      write(6,*)(UFI(i1,i2),i2=1,KN)
!cs      write(6,*)
!cs      enddo
!cs      write(6,*)
!cs      write(6,*)
      GO TO 103    
      ENDIF
!C*************************************************************
      IF(IM.EQ.3) THEN
      DO J=1,KN
      FFM(J)=FF(J)
      DO I=1,KN
      UFM(J,I)=UF(J,I)
      ENDDO
            Q(J)=Q0(J)
      ENDDO
      CALL sgelss(KN,KN,UFM,FFM,QM,info)
      DO I=1,KN
      Q(I)=QM(I)
      ENDDO
!C      WRITE(*,*) info,' info'
          IF(IMM.EQ.1) THEN
      AINV=0.
      DO 31 IS=1,KN
      QTT(IS)=0.
      DO 30 JS=1,KN
      QTT(IS)=QTT(IS)+UFM(IS,JS)*QM(JS)
   30 CONTINUE
      AINV=AINV+(FFM(IS)-QTT(IS))*(FFM(IS)-QTT(IS))      
   31 CONTINUE
!C      WRITE(*,*) AINV,IS,' AINV,IS'
          ENDIF
      GO TO 103
      ENDIF
!C***  Calculating initial "residual" 
!C      KEX=5.0E+5
      KEX=0
!C      KEX1=5.0E+5
      KEX1=0
      AP0=0.
!cd      AP0S=0.
      DO 1 I=1,KN
      FFQ(I)=0
      Q(I)=Q0(I)
      QT(I)=Q0(I)
    1 CONTINUE
      DO 19 J=1,KN
      ADQ=0
      DO 16 I=1,KN
      ADQ=ADQ+UF(J,I)*Q(I)
      IF(IMM.EQ.1) ADQS(J,I)=UF(J,I)
   16 CONTINUE
!C      IF(J.LE.2)  write(*,*) (UF(J,K),K=1,KN)
      FFQ(J)=ADQ
   19 CONTINUE
         IF(IMM.EQ.1) THEN
      DO I=1,KN
      AP0S(I)=0
      ENDDO
         ENDIF
      DO 20 I=1,KN
      AP0=AP0+(FFQ(I)-FF(I))*(FFQ(I)-FF(I))
!C       AP0=AP0+ABS((FFQ(I)-FF(I)))
          IF(IMM.EQ.1) THEN
      DO IS=1,KN
      IF(IS.EQ.I) AP0S(I)=AP0S(I)+(ADQS(IS,I)-1.)*(ADQS(IS,I)-1.)
      IF(IS.NE.I) AP0S(I)=AP0S(I)+ADQS(IS,I)*ADQS(IS,I)
!CDDD      IF(IS.EQ.KN) WRITE(*,*) AP0S(I),I,' AP0S(I),I'
      ENDDO
           ENDIF
!C      AP0=SQRT(AP0)
   20 CONTINUE
      APN=AP0
          IF(IMM.EQ.1) THEN
      DO I=1,KN
      APNS=APNS+AP0S(I)
      ENDDO
!c!      WRITE(*,*) APNS,'  APNS, starting'
          ENDIF
!c!      IF(NQ.LE.0.OR.NQ.GT.7) WRITE(*,*) APN,'starting residual' 
!C***  Matrix H(p) calculating
!C            I =1 - linear iterations
!C            I =2 - matrix inversion      
!C********INVERSE MATRIX *********************
      IF(IM.EQ.2) THEN
      DO 2 I=1,KN
      DO 2 J=1,KN
    2 AH(I,J)=UF(I,J)
      CALL TINVCH3(KN,AH,DT,KN,ERR)
      IF(IIM.LT.1) THEN
      OPEN (17, FILE="error.dat")
!C      WRITE (17,*) DT, '  DT'
      WRITE (17,*) GF, '  GF'
      DO II=1,KN
      Q(II)=SQRT(AH(II,II)/GF*APQ)
!C      write(17,*) II, SQRT(AH(II,II)/GF*APQ)
!C      write(*,*) II, SQRT(AH(II,II)/GF*APQ)
      ENDDO 
      CLOSE (17)
      do i1=1,KN
      do i2=1,KN
      UF(i1,i2)=AH(i1,i2)
      enddo
      enddo 
      GO TO 103    
      ENDIF
      IIM=IIM+1
!C      WRITE (*,*) DT, '  DT'
  21  FORMAT(9E11.3)
      ENDIF
            IQ=0
!C********REGRESSION MATRIX*******************
!C      IF(IM.EQ.1.OR.IM.EQ.7.OR.IM.EQ.8) THEN
123    IF(IM.EQ.1.OR.IM.EQ.7.AND.KI.EQ.0.OR.IM.EQ.8.AND.KI.EQ.0) THEN
      DO 3 I=1,KN
      DO 3 J=1,KN
    3 AH(J,I)=0
      IF(IM.EQ.1.AND.KI.EQ.0) THEN
      IF(IQ.EQ.0) THEN
      WRITE(*,*) 'matrix'
      DO I=1,KN
      DO I1=1,KN
       APNG3=0
      DO J=1,KN
      APNG3=APNG3+UF(I,J)*UF(J,I1)
      ENDDO
      UUFF(I,I1)=APNG3
!c      WRITE(*,*) APNG3,'APNG3'
      ENDDO
      ENDDO
      ENDIF
      APNG=0
      APNG2=0
      DO 135 I=1,KN
      APNG1=0
      APNG3=0
      DO 136 J=1,KN
      APNG1=APNG1+UF(I,J)*(FFQ(J)-FF(J)) 
      APNG3=APNG3+UUFF(I,J)*(FFQ(J)-FF(J)) 
  136 CONTINUE
      APNG2=APNG2+APNG1*(FFQ(I)-FF(I))
      APNG=APNG+APNG3*(FFQ(I)-FF(I))
!C      APNG2=APNG2+(FFQ(I)-FF(I))*(FFQ(I)-FF(I))
!c      WRITE(*,*) (FFQ(I)-FF(I))
  135 CONTINUE
!C            WRITE(*,*) APNG, APNG2,' APNG,APNG2'
      ENDIF
      IF(IM.EQ.1.AND.KI.EQ.1) THEN
      APNG=0
      APNG1=0
      DO I=1,KN
      DO J=1,KN
      APNG1=APNG1+UF(I,J)*UF(I,J)
      ENDDO
      APNG=APNG+UF(I,I)*UF(I,I)
      ENDDO
      WRITE(*,*) APNG,APNG1,' APNG,APNG1'
      APNG=APNG/APNG1
      ENDIF
      ABA=0.
      DO 4 I=1,KN
      AHH=0
!C      IF(IM.EQ.7) ABB=SQRT(UF(I,I)/(FFQ(I)-FF(I))*(FFQ(I)-FF(I)))
      IF(IM.EQ.7) ABB=SQRT(UF(I,I))
      DO 5 J=1,KN
!C      IF(IM.EQ.1) AHH=AHH+UF(I,J)
      IF(IM.EQ.1.AND.KI.EQ.3) THEN
      AHH=AHH+ABS(UF(I,J)*(FFQ(J)-FF(J)))
!C      AHH=AHH+UF(I,J)*(FFQ(J)-FF(J))
!C      IF(J.EQ.KN.AND.ABS(AHH).LT.1.0e-10) AHH=UF(I,I)*(FFQ(I)-FF(I))
      ENDIF
      IF(IM.EQ.1.AND.KI.EQ.2) THEN
      AHH=AHH+ABS(UF(I,J))
      ENDIF
      IF(IM.EQ.7) AHH=AHH+(UF(I,J)/ABB)*(UF(I,J)/ABB)
!C     IF(IM.EQ.7) AHH=AHH+(UF(I,J)/ABB*(FFQ(J)-FF(J))*(FFQ(J)-FF(J)))
!C     &*(UF(I,J)/ABB*(FFQ(J)-FF(J))*(FFQ(J)-FF(J)))
    5 CONTINUE
      IF(IM.EQ.1.AND.KI.EQ.3) AH(I,I)=ABS(FFQ(I)-FF(I))/AHH
!C      IF(IM.EQ.1.AND.KI.EQ.3) AH(I,I)=(FFQ(I)-FF(I))/AHH
      IF(IM.EQ.1.AND.KI.EQ.2) AH(I,I)=1/AHH
      IF(IM.EQ.7) AH(I,I)=1/AHH
      IF(IM.EQ.1.AND.KI.EQ.0) AH(I,I)=APNG2/APNG
      IF(IM.EQ.1.AND.KI.EQ.1) AH(I,I)=APNG
    4 CONTINUE
      ENDIF
!C      IF(IQ.EQ.0) THEN
!C      ABA=0
!C      DO I=1,KN
!C      ABA1=0
!C      DO J=1,KN
!C      IF(I.EQ.J) ABA1=ABA1+(UF(I,J)*AH(I,J)-1)*(UF(I,J)*AH(I,J)-1)
!C      IF(I.NE.J) ABA1=ABA1+(UF(I,J)*AH(I,J))*(UF(I,J)*AH(I,J))
!C      ENDDO
!C      ABA=ABA+ABA1
!C      ENDDO
!C      ENDIF
!C      WRITE(*,*) ABA,' ABA'
      IF(IM.EQ.1.AND.KI.EQ.0.AND.IQ.GT.0) GO TO 101
      IF(KI.NE.0) THEN
      IF(IM.EQ.7.OR.IM.EQ.8) CALL INV(UF,KN,IM,KI,AH)
      ENDIF
!C****LINEAR ITERATIONS IMPLEMENTAION***********
      BMAX=1.E+010
      IQ=0
      IQAP=0
      AL=1.0
      EPS=0
      DO I=1,KN
      DO J=1,KN
      ADQS1(I,J)=0.
      AD(I,J)=0.
      ENDDO
      ENDDO
  101 IF(IQ.LE.KEX.AND.APN.LT.BMAX.OR.EPS.GE.EPSQ.AND
     &.IM.GE.1.OR.NQ.LT.-10.AND.IQ.LT.ABS(NQ)) THEN
      IF(IQ.EQ.NQ) GO TO 102
        IF(IMM.EQ.1) THEN
      DO I=1,KN
      DO J=1,KN
      IF(IQ.EQ.0) ADQS(I,J)=UF(I,J)
      IF(IQ.GT.0) ADQS(I,J)=ADQS1(I,J)
      IF(IQ.GT.0) AD(I,J)=AD1(I,J)
      ENDDO
      ENDDO
      DO I=1,KN
      DO J=1,KN
      ADQS1(I,J)=0.
      AD1(I,J)=0.
      ENDDO
      ENDDO
         ENDIF
      DO 9 I=1,KN
      ADQ=0
      DO 6 J=1,KN
      ADQ=ADQ+AH(I,J)*(FFQ(J)-FF(J))
          IF(IMM.EQ.1) THEN
      DO 26 IS=1,KN
      IF(IQ.EQ.0) THEN
      ADQS1(I,J)=ADQS1(I,J)+AH(I,IS)*ADQS(IS,J)
      AD1(I,J)=AD1(I,J)+ADQS(I,IS)*AH(IS,J)
      ELSE
      ADQS1(I,J)=ADQS1(I,J)+ADQS0(I,IS)*ADQS(IS,J)
      AD1(I,J)=AD1(I,J)+AD0(I,IS)*AD(IS,J)
      ENDIF
   26 CONTINUE
         ENDIF
    6 CONTINUE
      Q1(I)=ADQ
    9 CONTINUE
                     IF(IMM.EQ.1) THEN
      IF(IQ.EQ.0) THEN
      DO I=1,KN
      ADQS1(I,I)=ADQS1(I,I)-1.
      AD1(I,I)=AD1(I,I)-1.
      ENDDO
      ENDIF
      IF(IQ.EQ.0) THEN
      DO I=1,KN
      DO J=1,KN
      ADQS0(I,J)=ADQS1(I,J)
      AD0(I,J)=AD1(I,J)
      AD(I,J)=AD1(I,J)
!CD      AD1(I,J)=0
      ENDDO
      ENDDO
      DO I=1,KN
      DO J=1,KN
      DO IS=1,KN
!CD      AD1(I,J)=AD1(I,J)+AD0(I,IS)*AD(IS,J)
      ENDDO
      ENDDO
      ENDDO
      ENDIF
      DO I=1,KN
      FFSS(I)=0.
      DO J=1,KN
      FFSS(I)=FFSS(I)+AD1(I,J)*FF(J)
      ENDDO
      ENDDO
      APNQS=0.
      DO I=1,KN
      APNQS=APNQS+FFSS(I)*FFSS(I)
      ENDDO
                       ENDIF
  100 DO 15 I=1,KN
   15 Q(I)=Q(I)-AL*Q1(I)
      DO 7 I=1,KN
            IF(IMM.EQ.1) APQS(I)=0.
      AF=0
      DO 8 J=1,KN
      AF=AF+UF(I,J)*Q(J)
      IF(IMM.EQ.1) APQS(I)=APQS(I)+ADQS1(J,I)*ADQS1(J,I)
    8 CONTINUE
      FFQ(I)=AF
!C      WRITE(*,*) APQS(I),I,' APQS(I),I'
    7 CONTINUE
      APQ=0
      APNS=0.
      DO 10 I=1,KN
      APQ=APQ+(FFQ(I)-FF(I))*(FFQ(I)-FF(I))
!C      APQ=APQ+ABS((FFQ(I)-FF(I)))
      IF(IMM.EQ.1) APNS=APNS+APQS(I)
   10 CONTINUE
!C      APQ=SQRT(APQ)
      IF(APQ.LE.0) WRITE(*,*) APQ,' APQ'
        IF (APN.GT.0.0) THEN
      EPS=(APN-APQ)/APN
      ELSE
      EPS=APN-APQ
      ENDIF
!c     write(*,*) APN, APNS, IQ,' APN, APNS,IQ'
!C***TEMPORARY :
      IF(NQ.LT.0.AND.IQ.LT.ABS(NQ))      EPS=ABS(EPS)
      IF(EPS.LT.0.AND.IM.GE.1.AND.APN.GT.1.0E-17.AND.IQ.GE.KEX1
     &.OR.APN.GT.BMAX) THEN
      IQAP=IQAP+1
      IF(IQAP.LT.10) THEN
      ARED=1.5
      ELSE
      ARED=ARED*ARED
      ENDIF
      AL=AL/ARED
      DO 11 I=1,KN
   11 Q(I)=QT(I)
!C      write(*,*) APQ, APN, EPS, ' APQ, APN, EPS.LT.0'
      IF(IQAP.LT.33) GO TO 100
      ENDIF
      AL=1.0
      DO 12 I=1,KN
   12 QT(I)=Q(I)
      IQ=IQ+1
      APN=APQ
      IF(IM.EQ.1.AND.KI.EQ.0) GO TO 123
      GO TO 101
      ENDIF
  102 CONTINUE    
!c!      IF(NQ.LT.0.OR.NQ.GT.7) WRITE(*,*) APN,EPS,IQ,' residual,EPS, IQ'
!c!      IF(IMM.EQ.1)  WRITE(*,*) APN,APQ,APNQS,IQ,' APN,APQ,APNQS,IQ,'
!C      WRITE(*,*) ABA**IQ,' ABA**NQ'
      AMAX=ABS(Q(1))
      DO I=1,KN
      IF(AMAX.LT.ABS(Q(I))) AMAX=ABS(Q(I))
      ENDDO
      IF (AMAX.GT.10) THEN
      DO I=1,KN
      Q(I)=Q(I)/AMAX*10.0
      ENDDO 
      ENDIF     
  103 RETURN
      END
      
      
      SUBROUTINE TINVCH3(N,A,DT,NN,ERR)
!C$NAME  TINVCH3
!C INVERSION OF SYMMETRIC POSITIVE DEFINITE MATRIX
!C     SQUARE ROOT METHOD
!C     MATRIX A IS SYMMETRIC AND POSITIVE DEFINITE
!C--- HISTORY
!C 71. 4.30 CREATED BY SAKATA MASATO
!C 89.11.10 ADDED ERR.
!C 90. 1.17  REGISTERED
!C--- INPUT
!C N        I        DIMENSION OF THE MATRIX
!C A      R(NN,N)    MATRIX
!C NN       I        SIZE OF THE FIRST ARGUMENT OF A
!C--- OUTPUT
!C A                 INVERSE OF THE MATRIX
!C DT       R        DETERMINATION OF THE MATRIX
!C INDER    I        0: NORMAL TERMINATION
!C$ENDI
!CD       CHARACTER ERR*64
       CHARACTER ERR*(*)
!C      DIMENSION    A(NN,N)
!cs      DIMENSION    A(100,100)
      DOUBLE PRECISION A(100,100),DT      
      ERR=' '
      IF(N-1)   1,2,3
    1 ERR='ERROR IN TINVCH: N .LE.0'
      RETURN
    2 DT=A(1,1)
      A(1,1)=1.0/A(1,1)
      IF(DT.LE.0.0)   GO TO  60
      RETURN
    4 ERR='ERROR IN TINVCH: N.GT.NN'
      RETURN
    3 IF(N.GT.NN)    GO TO  4
      DT=A(1,1)
      IF(DT.LE.0.0)   GO TO  60
      A(1,1)=SQRT(DT)
      DO  5  J=2,N
    5 A(1,J)=A(1,J)/A(1,1)
      DO  10  K=2,N
      Z=A(K,K)
      K1=K-1
      DO  11   J=1,K1
   11 Z=Z-A(J,K)*A(J,K)
      IF(Z.LE.0.0)   GO TO  60
      DT=DT*Z
      A(K,K)=SQRT(Z)
      IF(K.EQ.N) GO TO 10
      Y=1.0/A(K,K)
      J1=K+1
      DO 13   J=J1,N
      Z=A(K,J)
      DO  14  I=1,K1
   14 Z=Z-A(I,K)*A(I,J)
   13 A(K,J)=Z*Y
   10 CONTINUE
         A(N,N)=1.0/A(N,N)
      DO   40   IA=2,N
      I=N-IA+1
      Y=1.0/A(I,I)
      A(I,I)=Y
      I1=I+1
      DO  41    JA=I1,N
      J=N-JA+I1
      Z=0.0
      DO  42    K=I1,J
   42 Z=Z-A(I,K)*A(K,J)
   41 A(I,J)=Z*Y
   40 CONTINUE
      DO  50  J=1,N
      DO  51   I=J,N
      Z=0.0
      DO  52   K=I,N
   52 Z=Z+A(I,K)*A(J,K)
   51 A(I,J)=Z
      DO  53  I=1,J
   53 A(I,J)=A(J,I)
   50 CONTINUE
      RETURN
   60 ERR='ERROR IN TINVCH: GIVEN MATRIX IS 
     &NOT POSITIVE DEFINITE'
      RETURN
      END

      SUBROUTINE sgelss(m,n,u,b,b1,info)
      INTEGER m,mp,ndata,np
      REAL a(m),b(m),v(n,n),w(n),TOL
	DOUBLE PRECISION u(m,n)
      REAL b1(m)
!CD      REAL u(m,n),v(n,n),a(m),b(m),b1(m),w(n),TOL
!cs      PARAMETER (TOL=1.e-12)
      PARAMETER (TOL=1.e-6)
!CU    USES svbksb1,svdcmp1
      INTEGER i,j
      REAL thresh,wmax
	info=0
	mp=m
	np=n
	ndata=m
	ma=n
      call svdcmp1(u,ndata,ma,mp,np,w,v)
      wmax=0.
      do 13 j=1,ma
        if(w(j).gt.wmax)wmax=w(j)
13    continue
	if(wmax.le.TOL) info=-1
      thresh=TOL*wmax
      do 14 j=1,ma
        if(w(j).lt.thresh)w(j)=0.
14    continue
      call svbksb1(u,w,v,ndata,ma,mp,np,b,a)
	do i=1,n
	   b1(i)=a(i)
	enddo
      return
      END

      SUBROUTINE sgelssi(m,n,u,b,b1,info,u1)
      INTEGER m,mp,ndata,np
      REAL a(m),b(m),u(m,n),v(n,n),w(n),TOL
      REAL b1(m),u1(m,n),sv(n,n),u2(m,n)
!CD      REAL u(m,n),v(n,n),a(m),b(m),b1(m),w(n),TOL
!cs      PARAMETER (TOL=1.e-12)
      PARAMETER (TOL=1.e-10)
!CU    USES svbksb1,svdcmp1
      INTEGER i,j
      REAL thresh,wmax
	info=0
	mp=m
	np=n
	ndata=m
	ma=n
      call svdcmp1(DBLE(u),ndata,ma,mp,np,w,v)
!cs      write(6,*)'w1'
!cs      write(6,*)(w(i),i=1,n)
      wmax=0.
      do 13 j=1,ma
        if(w(j).gt.wmax)wmax=w(j)
13    continue
	if(wmax.le.TOL) info=-1
!cs      thresh=TOL*wmax
        thresh=TOL
      do 14 j=1,ma
        if(w(j).lt.thresh)w(j)=0.
14    continue
!C***********************************************
         do i=1,n
         do j=1,n
         u1(i,j)=u(j,i)
         enddo
         enddo
         do i=1,n
         do j=1,n
         u(i,j)=u1(i,j)
         enddo
         enddo
!C************************************************
         do i=1,n
         do j=1,n
         sv(i,j)=0.
         enddo
         enddo
         do i=1,n
         if(w(i).ne.0)sv(i,i)=1./w(i)
         enddo
         do i1=1,n
         do i2=1,n
         A1=0.
         do i3=1,n
         A1=A1+sv(i1,i3)*u(i3,i2)
         enddo
         u2(i1,i2)=A1
         enddo
         enddo
!C*************************************************
!cs         write(6,*)'w'
!cs         write(6,*)(w(i),i=1,n)
         do i1=1,n
         do i2=1,n
         A1=0.
         do i3=1,n
         A1=A1+v(i1,i3)*u2(i3,i2)
         enddo
         u1(i1,i2)=A1
         enddo
         enddo
      return
      END

      SUBROUTINE svdcmp1(a,m,n,mp,np,w,v)
      INTEGER m,mp,n,np,NMAX
	DOUBLE PRECISION A(MP,NP)
      REAL v(np,np),w(np)
      PARAMETER (NMAX=500)
!CU    USES pythag1
      INTEGER i,its,j,jj,k,l,nm
      REAL anorm,c,f,g,h,s,scale,x,y,z,rv1(NMAX),pythag1
      g=0.0
      scale=0.0
      anorm=0.0
      do 25 i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.0
        s=0.0
        scale=0.0
        if(i.le.m)then
          do 11 k=i,m
            scale=scale+abs(a(k,i))
11        continue
          if(scale.ne.0.0)then
            do 12 k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
12          continue
            f=a(i,i)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,i)=f-g
            do 15 j=l,n
              s=0.0
              do 13 k=i,m
                s=s+a(k,i)*a(k,j)
13            continue
              f=s/h
              do 14 k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
14            continue
15          continue
            do 16 k=i,m
              a(k,i)=scale*a(k,i)
16          continue
          endif
        endif
        w(i)=scale *g
        g=0.0
        s=0.0
        scale=0.0
        if((i.le.m).and.(i.ne.n))then
          do 17 k=l,n
            scale=scale+abs(a(i,k))
17        continue
          if(scale.ne.0.0)then
            do 18 k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
18          continue
            f=a(i,l)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,l)=f-g
            do 19 k=l,n
              rv1(k)=a(i,k)/h
19          continue
            do 23 j=l,m
              s=0.0
              do 21 k=l,n
                s=s+a(j,k)*a(i,k)
21            continue
              do 22 k=l,n
                a(j,k)=a(j,k)+s*rv1(k)
22            continue
23          continue
            do 24 k=l,n
              a(i,k)=scale*a(i,k)
24          continue
          endif
        endif
        anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
25    continue
      do 32 i=n,1,-1
        if(i.lt.n)then
          if(g.ne.0.0)then
            do 26 j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
26          continue
            do 29 j=l,n
              s=0.0
              do 27 k=l,n
                s=s+a(i,k)*v(k,j)
27            continue
              do 28 k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
28            continue
29          continue
          endif
          do 31 j=l,n
            v(i,j)=0.0
            v(j,i)=0.0
31        continue
        endif
        v(i,i)=1.0
        g=rv1(i)
        l=i
32    continue
      do 39 i=min(m,n),1,-1
        l=i+1
        g=w(i)
        do 33 j=l,n
          a(i,j)=0.0
33      continue
        if(g.ne.0.0)then
          g=1.0/g
          do 36 j=l,n
            s=0.0
            do 34 k=l,m
              s=s+a(k,i)*a(k,j)
34          continue
            f=(s/a(i,i))*g
            do 35 k=i,m
              a(k,j)=a(k,j)+f*a(k,i)
35          continue
36        continue
          do 37 j=i,m
            a(j,i)=a(j,i)*g
37        continue
        else
          do 38 j= i,m
            a(j,i)=0.0
38        continue
        endif
        a(i,i)=a(i,i)+1.0
39    continue
      do 49 k=n,1,-1
        do 48 its=1,30
          do 41 l=k,1,-1
            nm=l-1
            if((abs(rv1(l))+anorm).eq.anorm)  goto 2
            if((abs(w(nm))+anorm).eq.anorm)  goto 1
41        continue
1         c=0.0
          s=1.0
          do 43 i=l,k
            f=s*rv1(i)
            rv1(i)=c*rv1(i)
            if((abs(f)+anorm).eq.anorm) goto 2
            g=w(i)
            h=pythag1(f,g)
            w(i)=h
            h=1.0/h
            c= (g*h)
            s=-(f*h)
            do 42 j=1,m
              y=a(j,nm)
              z=a(j,i)
              a(j,nm)=(y*c)+(z*s)
              a(j,i)=-(y*s)+(z*c)
42          continue
43        continue
2         z=w(k)
          if(l.eq.k)then
            if(z.lt.0.0)then
              w(k)=-z
              do 44 j=1,n
                v(j,k)=-v(j,k)
44            continue
            endif
            goto 3
          endif
cxxa          if(its.eq.30) pause 'no convergence in svdcmp1'
		if(its.eq.30)write(1011,*) 'no convergence in svdcmp1'
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y)
          g=pythag1(f,1.0)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1.0
          s=1.0
          do 47 j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=pythag1(f,h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do 45 jj=1,n
              x=v(jj,j)
              z=v(jj,i)
              v(jj,j)= (x*c)+(z*s)
              v(jj,i)=-(x*s)+(z*c)
45          continue
            z=pythag1(f,h)
            w(j)=z
            if(z.ne.0.0)then
              z=1.0/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do 46 jj=1,m
              y=a(jj,j)
              z=a(jj,i)
              a(jj,j)= (y*c)+(z*s)
              a(jj,i)=-(y*s)+(z*c)
46          continue
47        continue
          rv1(l)=0.0
          rv1(k)=f
          w(k)=x
48      continue
3       continue
49    continue
      return
      END

      FUNCTION pythag1(a,b)
      REAL a,b,pythag1
      REAL absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        pythag1=absa*sqrt(1.+(absb/absa)**2)
      else
        if(absb.eq.0.)then
          pythag1=0.
        else
          pythag1=absb*sqrt(1.+(absa/absb)**2)
        endif
      endif
      return
      END

      SUBROUTINE svbksb1(u,w,v,m,n,mp,np,b,x)
      INTEGER m,mp,n,np,NMAX
	DOUBLE PRECISION u(mp,np)
      REAL b(mp),v(np,np),w(np),x(np)
      PARAMETER (NMAX=500)
      INTEGER i,j,jj
      REAL s,tmp(NMAX)
      do 12 j=1,n
        s=0.
        if(w(j).ne.0.)then
          do 11 i=1,m
            s=s+u(i,j)*b(i)
11        continue
          s=s/w(j)
        endif
        tmp(j)=s
12    continue
      do 14 j=1,n
        s=0.
        do 13 jj=1,n
          s=s+v(j,jj)*tmp(jj)
13      continue
        x(j)=s
14    continue
      return
      END
!C  (C) Copr. 1986-92 Numerical Recipes Software .5^k#<j#)0M&124i..

      SUBROUTINE INV(UF,KN,IM,KI,AH)
!C***************************************************
!C This SUBROUTINE calculates matrix AH for 
!C 		optimised iterative prcedure
!C***************************************************
!C 
!C  INPUT:
!C
!C  UF    R(KN,KN) - input matrix
!C  KN    I        - matrix dimension
!C  IM    I        - regime of matrix inversion
!C  KI    I        - order of approximation
!C
!C  OUTPUT:
!C
!C  AH   R(KN,KN) - output matrix
!C***************************************************
      CHARACTER ERR*64
      PARAMETER (IKN=100)
!================ BY DUAN======================================
!      REAL UF(IKN,IKN),AH(IKN,IKN),AHI(IKN,IKN),UFT(IKN,IKN),
!     &AHII(IKN,IKN)
      REAL UF(IKN,IKN),UFT(IKN,IKN),AHII(IKN,IKN)
	DOUBLE PRECISION AH(IKN,IKN)
      REAL*8  AHI(IKN,IKN), DT
!================END BY DUAN===================================
      REAL AHIS(IKN,IKN),AFS(IKN),AF1S(IKN),AF(IKN),FF(IKN)
!C      WRITE(*,*) 'STEP 1'
!C      DO I=1,8
!C      WRITE(*,21) (UF(I,J),J=1,8)
!C      ENDDO
      DO I=1, KN
      DO J=1, KN
      AH(J,I)=0.
      ENDDO
      ENDDO
!C*****************************
!C*  UFT=(UF)**T (UF)
!C*****************************
      DO 2 I =1,KN
      DO 4 I1=1,KN
      AA=0
      DO 5 J2=1,KN
   5  AA=AA+UF(I,J2)*UF(J2,I1)
   4  UFT(I,I1)=AA      
   2  CONTINUE
!C      write(*,*) ' UFT:'
      DO I=10,18
!C      WRITE(*,21) (UFT(I,J),J=10,18)
      ENDDO
!C*****************************
      WRITE(*,*) KI,' KI'
      DO 1 IC=1,KN
!C            WRITE(*,*) 'STEP 2'
      DO I=1, KN
      AF(I)=0.
      IF(IM.EQ.8) AFS(I)=0.
      IF(IM.EQ.8) AF1S(I)=0.
      DO J=1, KN
      IF(IM.EQ.8) AHIS(I,J)=0.
      AHI(J,I)=0.
      ENDDO
      ENDDO
      IN=0
      JN=0
      IN1=0
      IN2=0
      KII=2*KI
      DO 7 IO=0,KII
           II=IC-KI+IO
           IF(IN1.EQ.0.AND.II.GT.0) THEN
             IN1=II
             JN1=II
             IF((IC-KI+KII).LE.KN) THEN
             IN2=IC-KI+KII
             JN2=IC-KI+KII
             ELSE
             IN2=KN
             JN2=KN
             ENDIF
           ENDIF
           IF(II.GT.0.AND.II.LE.KN) THEN
            IN=IN+1
           ELSE 
            GO TO 7
           ENDIF
            AF(IN)=UF(II,IC)
!C            WRITE(*,*) AF(IN),IN,II,IC,' AF(IN),IN,II,IC'
!C            WRITE(*,*) IN1,IN2,' IN1,IN2'
!C            WRITE(*,*) JN1,JN2,' JN1,JN2'
      DO 17 JJ=JN1,JN2
!C       IF(JJ.EQ.1.AND.IN.EQ.1) THEN
!C       WRITE(*,*) UFT(JJ,II),JJ,II,' UFT(JJ,II),JJ,II'
!C       ENDIF
      AHI(JJ-JN1+1,IN)=UFT(JJ,II)
  17  CONTINUE
   7  CONTINUE
!C       WRITE(*,*) 'STEP 4' 
!C         IF(IC.EQ.1) THEN
!C      write(*,*) ' AHI:, before inversion'
      DO I=1,8
!C      WRITE(*,21) (AHI(I,J),J=1,8)
      ENDDO   
!C         ENDIF
       IF (IM.EQ.7) CALL TINVCH3(IN,AHI,DT,IN,ERR)
       IF(IC.EQ.7) WRITE (*,*) DT, IN, '  DT,IN'
!C          WRITE(*,*) AHI(1,1), AHI(2,2),' AHI, in before'
!C      WRITE(*,*) 'STEP 5'
!C      WRITE(*,*) IN1,IN2,' IN1,IN2'
            IF(IC.EQ.1) THEN
       DO I=1,8
!C       WRITE(*,21) (AHI(I,J),J=1,8)
       ENDDO
!C       WRITE(*,*) ' AF:'
!C       WRITE(*,21) (AF(J),J=1,7)
!C      WRITE(*,*) IN1, IN2,' IN1, IN2'
               ENDIF
      IF (IM.EQ.8) THEN
      DO IS=1,KN
      AFS(IS)=AF(IS)
      DO JS=1,KN
      AHIS(JS,IS)=AHI(JS,IS)
      ENDDO
      ENDDO
            IF(IC.EQ.1) THEN
!C            write(*,*) ' AHIS:, before inversion'
      DO I=1,8
!C      WRITE(*,21) (AHIS(I,J),J=1,8)
      ENDDO  
!C      WRITE(*,*) ' AF1S:'
!C      WRITE(*,21) (AF1S(IIS),IIS=1,KN) 
            ENDIF
!C      CALL sgelss(KN,KN,AHIS,AFS,AF1S,info) 
      CALL INVS(IN,AHI,AF,AF1S)     
!C      WRITE(*,*) info,' info'
!C      AINV=0.
!C      DO 31 IS=1,IN
!C      FF(IS)=0.
!C      DO 30 JS=1,IN
!C      FF(IS)=FF(IS)+AHI(IS,JS)*AF1S(JS)
!C   30 CONTINUE
!C      AINV=AINV+(AFS(IS)-FF(IS))*(AFS(IS)-FF(IS))
!C      WRITE(*,*) AINV,IS,' AINV,IS'
!C   31 CONTINUE
!C      WRITE(*,*) AINV,' AINV'
      ENDIF
!C      WRITE(*,*) ' AF1:'
!C      WRITE(*,21) (AF1S(IIS),IIS=1,7)
      DO 9 I=IN1,IN2
      IF (IM.EQ.7) THEN
      ADQ=0.
      DO 6 J=IN1,IN2
!C      WRITE(*,*) AHI(J-IN1+1,I-IN1+1), AF(J-IN1+1)
      ADQ=ADQ+AHI(J-IN1+1,I-IN1+1)*AF(J-IN1+1)
!C            WRITE(*,*) ADQ,I,J,' ADQ'
    6 CONTINUE
       ENDIF
       IF (IM.EQ.8) ADQ=AF1S(I-IN1+1)
!CD    9 AH(I,IC)=ADQ
    9 AH(IC,I)=ADQ
   1  CONTINUE
!C      WRITE(*,*) AH(1,1),'  in 1'
!C      WRITE(*,*) AH(2,2),'  in 2'
!C      WRITE(*,*) ' final Matrix:'
      DO I=1,7
!C      WRITE(*,21) (AH(I,J),J=10,18)
      ENDDO
  21  FORMAT(9E11.3)
      RETURN
      END
      
      SUBROUTINE INVS(IN,AHIS,AFS,AF1S)
      PARAMETER (IKN=100)
!================ BY DUAN======================================
!      REAL AHIS(IKN,IKN),AFS(IKN),AF1S(IKN)
!      REAL A(IN,IN),AB(IN),ABF(IN)
      REAL AFS(IKN),AF1S(IKN)
      REAL A(IN,IN),AB(IN),ABF(IN)
      REAL*8 AHIS(IKN,IKN)
!================END BY DUAN===================================      
      DO I=1,IN
      AB(I)=AFS(I)
      DO J=1,IN
      A(I,J)=AHIS(I,J)
      ENDDO
      ENDDO
      CALL sgelss(IN,IN,DBLE(A),AB,ABF,info)
      DO I=1,IN
      AF1S(I)=ABF(I)
      ENDDO
      RETURN
      END
!C************************************************************************************************
      SUBROUTINE F02WEF(M,N,A,LDA,NCOLB,B,LDB,WANTQ,Q,LDQ,SV,WANTP,PT,
     *                  LDPT,WORK,IFAIL)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  1. Purpose
!C     =======
!C
!C  F02WEF  returns all, or part, of the  singular value decomposition of
!C  a general real matrix.
!C
!C  2. Description
!C     ===========
!C
!C  The m by n matrix A is factorized as
!C
!C     A = Q*D*P',
!C
!C  where
!C
!C     D = ( S ),      m .gt. n,
!C         ( 0 )
!C
!C     D =   S,        m .eq. n,
!C
!C     D = ( S  0 ),   m .lt. n,
!C
!C  Q is an  m by m orthogonal matrix,  P is an  n by n orthogonal matrix
!C  and  S  is a  min( m, n ) by min( m, n )  diagonal matrix  with  non-
!C  negative diagonal elements, sv( 1 ), sv( 2 ), ..., sv( min( m, n ) ),
!C  ordered such that
!C
!C     sv( 1 ) .ge. sv( 2 ) .ge. ... .ge. sv( min( m, n ) ) .ge. 0.
!C
!C  The first min( m, n ) columns of Q are the left-hand singular vectors
!C  of A, the diagonal elements of S are the singular values of A and the
!C  first min( m, n ) columns of P are the right-hand singular vectors of
!C  A.
!C
!C  Either or both of the left-hand and right-hand singular vectors of  A
!C  may be requested and the matrix
!C  C given by
!C
!C     C = Q'*B,
!C
!C  where B is an m by ncolb given matrix, may also be requested.
!C
!C  The  routine  obtains  the  singular  value  decomposition  by  first
!C  reducing  A  to  upper  triangular  form  by  means  of   Householder
!C  transformations, from the left when  m .ge. n and from the right when
!C  m .lt. n.  The  upper triangular form  is then reduced to  bidiagonal
!C  form by  Givens plane rotations and finally the  QR algorithm is used
!C  to obtain the  singular value decomposition  of the  bidiagonal form.
!C
!C  3. Parameters
!C     ==========
!C
!C  M      - INTEGER.
!C
!C           On entry, M must specify the number of rows of the matrix A.
!C           M  must be  at least  zero.  When  M = 0  then an  immediate
!C           return is effected.
!C
!C           Unchanged on exit.
!C
!C  N      - INTEGER.
!C
!C           On entry, N must specify the number of columns of the matrix
!C           A.  N must be at least zero.  When  N = 0  then an immediate
!C           return is effected.
!C
!C           Unchanged on exit.
!C
!C  A      - REAL             array of DIMENSION ( LDA, n ).
!C
!C           Before entry, the leading  M by N  part of the array  A must
!C           contain the matrix  A  whose singular value decomposition is
!C           required.
!C
!C           If  m .ge. n  and  WANTQ is .TRUE.  then on exit, the M by N
!C           part of A will contain the first n columns of the orthogonal
!C           matrix  Q.
!C
!C           If  m .lt. n  and  WANTP is .TRUE.  then on exit, the M by N
!C           part of  A  will contain the first  m rows of the orthogonal
!C           matrix  P'.
!C
!C           If   m .ge. n  and   WANTQ is .FALSE.  and   WANTP is .TRUE.
!C           then on exit, the  min( M, N ) by N  part of  A will contain
!C           the first  min( m, n )  rows  of the  orthogonal matrix  P'.
!C
!C           Otherwise  the   M by N  part  of  A  is  used  as  internal
!C           workspace.
!C
!C  LDA    - INTEGER.
!C
!C           On entry, LDA  must  specify  the  leading dimension of  the
!C           array  A  as declared in the calling (sub) program. LDA must
!C           be at least  m.
!C
!C           Unchanged on exit.
!C
!C  NCOLB  - On entry,  NCOLB  must specify the  number of columns of the
!C           matrix  B  and must be at least  zero.  When  NCOLB = 0  the
!C           array  B  is not referenced.
!C
!C  B      - REAL             array of DIMENSION ( LDB, ncolb ).
!C
!C           Before entry with  NCOLB .gt. 0, the leading M by NCOLB part
!C           of the array  B  must contain the  matrix to be  transformed
!C           and  on exit,  B  is overwritten  by the  m by ncolb  matrix
!C           Q'*B.
!C
!C  LDB    - INTEGER.
!C
!C           On entry, LDB  must  specify  the  leading dimension of  the
!C           array  B  as declared  in the  calling  (sub) program.  When
!C           NCOLB .gt. 0  then LDB must be at least  m.
!C
!C           Unchanged on exit.
!C
!C  WANTQ  - LOGICAL.
!C
!C           On entry,  WANTQ  must be .TRUE.  if the  left-hand singular
!C           vectors are required. If  WANTQ is .FALSE.  then  the  array
!C           Q  is not referenced.
!C
!C           Unchanged on exit.
!C
!C  Q      - REAL             array of DIMENSION ( LDQ, m ).
!C
!C           On exit  with  M .lt. N  and  WANTQ as .TRUE.,  the  leading
!C           M by M  part  of the  array  Q  will contain  the orthogonal
!C           matrix Q. Otherwise the array  Q is not referenced.
!C
!C  LDQ    - INTEGER.
!C
!C           On entry, LDQ  must  specify  the  leading dimension of  the
!C           array  Q  as declared  in the  calling  (sub) program.  When
!C           M .lt. N  and  WANTQ is .TRUE.,  LDQ  must  be at  least  m.
!C
!C           Unchanged on exit.
!C
!C  SV     - REAL array of DIMENSION at least ( min( m, n ) ).
!C
!C           On exit, the array  SV will contain the min( m, n ) diagonal
!C           elements of the matrix  S.
!C
!C  WANTP  - LOGICAL.
!C
!C           On entry,  WANTP must be .TRUE.  if the  right-hand singular
!C           vectors are  required.  If  WANTP is .FALSE.  then the array
!C           PT  is not referenced.
!C
!C           Unchanged on exit.
!C
!C  PT     - REAL             array of DIMENSION ( LDPT, n ).
!C
!C           On exit  with  M .ge. N  and  WANTQ and WANTP as .TRUE., the
!C           leading  N by N  part  of the  array  PT  will  contain  the
!C           orthogonal  matrix  P'.   Otherwise  the  array  PT  is  not
!C           referenced.
!C
!C  LDPT   - INTEGER.
!C
!C           On entry,  LDPT  must specify the  leading dimension  of the
!C           array  PT  as declared  in the  calling (sub) program.  When
!C           M .ge. N  and  WANTQ and WANTP are .TRUE.,  LDPT  must be at
!C           least  n.
!C
!C           Unchanged on exit.
!C
!C  WORK   - REAL array of DIMENSION at least ( lwork ), where lwork must
!C           be as given in the following table:
!C
!C              M .ge. N
!C
!C                 WANTQ is .TRUE.  and   WANTP is .TRUE.
!C
!C                    lwork = n**2 + 5*( n - 1 )
!C
!C                 WANTQ is .TRUE.  and  WANTP is .FALSE.
!C
!C                    lwork = n**2 + 4*( n - 1 )
!C
!C                 WANTQ is .FALSE.  and  WANTP is .TRUE.
!C
!C                    lwork = max( 3*( n - 1 ), 1 )   when   NCOLB   =  0
!C                    lwork = max( 5*( n - 1 ), 2 )   when   NCOLB .gt. 0
!C
!C                 WANTQ is .FALSE.  and  WANTP is .FALSE.
!C
!C                    lwork = 2*n                      when  NCOLB   =  0
!C                    lwork = max( 3*( n - 1 ), 2*n )  when  NCOLB .gt. 0
!C
!C              M .lt. N
!C
!C                 WANTQ is .TRUE.  and   WANTP is .TRUE.
!C
!C                    lwork = m**2 + 5*( m - 1 )
!C
!C                 WANTQ is .TRUE.  and  WANTP is .FALSE.
!C
!C                    lwork = 3*( m - 1 )
!C
!C                 WANTQ is .FALSE.  and  WANTP is .TRUE.
!C
!C                    lwork = m**2 + 3*( m - 1 )       when  NCOLB   =  0
!C                    lwork = m**2 + 5*( m - 1 )       when  NCOLB .gt. 0
!C
!C                 WANTQ is .FALSE.  and  WANTP is .FALSE.
!C
!C                    lwork = max( 2*( m - 1 ), 1 )    when  NCOLB   =  0
!C                    lwork = max( 3*( m - 1 ), 1 )    when  NCOLB .gt. 0
!C
!C           The array  WORK  is used as  internal  workspace by  F06XUF.
!C           On exit,  WORK( min( m, n ) )  contains the  total number of
!C           iterations taken by the QR algorithm.
!C
!C  IFAIL  - INTEGER.
!C
!C           Before entry,  IFAIL  must contain one of the values -1 or 0
!C           or 1 to specify noisy soft failure or noisy hard failure  or
!C           silent soft failure. ( See Chapter P01 for further details.)
!C
!C           On successful  exit  IFAIL  will be  zero,  otherwise  IFAIL
!C           will be set to a  non-zero value  indicating either  that an
!C           input parameter  has been  incorrectly set,  or that the  QR
!C           algorithm  is not  converging.  See  the  next  section  for
!C           further details.
!C
!C  4. Diagnostic Information
!C     ======================
!C
!C  IFAIL = -1
!C
!C     One or more of the following conditions holds:
!C
!C        M     .lt. 0
!C        N     .lt. 0
!C        LDA   .lt. M
!C        NCOLB .lt. 0
!C        LDB   .lt. M  and  NCOLB .gt. 0
!C        LDQ   .lt. M  and  M     .lt. N  and  WANTQ is true
!C        LDPT  .lt. N  and  M     .ge. N  and  WANTQ is true
!C                                         and  WANTP is true
!C
!C  IFAIL .gt. 0
!C
!C     The  QR  algorithm  has  failed  to  converge  in   50*min( M, N )
!C     iterations.  In this case  sv( 1 ), sv( 2 ), ..., sv( IFAIL )  may
!C     not  have been found correctly  and the remaining  singular values
!C     may not be the smallest. The matrix  A will nevertheless have been
!C     factorized  as   A = Q*E*P',  where  the  leading   min( m, n ) by
!C     min( m, n )  part  of  E  is  a  bidiagonal matrix  with  sv( 1 ),
!C     sv( 2 ), ..., sv( min( m, n ) )   as  the  diagonal  elements  and
!C     work( 1 ), work( 2 ), ..., work( min( m, n ) - 1 )  as the  super-
!C     diagonal elements.
!C
!C     This failure is not likely to occur.
!C
!C  If  on  entry,  IFAIL  was  either  -1 or 0  then  further diagnostic
!C  information  will  be  output  on  the  error message  channel. ( See
!C  routine  X04AAF. )
!C
!C  5. Further information
!C     ===================
!C
!C  Following the use of this routine the rank of A may be estimated by
!C  a call to the INTEGER function F06KLF.  The statement:
!C
!C     IRANK = F06KLF( MIN( M, N ), SV, 1, TOL )
!C
!C  returns  the value  ( k - 1 ), in  IRANK,  where  k  is the  smallest
!C  integer  for  which   sv( k ) .lt. tol*sv( 1 ),   where  tol  is  the
!C  tolerance supplied in  TOL, so that  IRANK is an estimate of the rank
!C  of  S  and thus also of  A.  If  TOL is supplied as negative then the
!C  relative machine precision ( see routine X02AJF ) is used in place of
!C  TOL.
!C
!C
!C  Nag Fortran 77 Auxiliary linear algebra routine.
!C
!C  -- Written on 12-January-1988.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0)
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F02WEF')
!C     .. Scalar Arguments ..
      INTEGER           IFAIL, LDA, LDB, LDPT, LDQ, M, N, NCOLB
      LOGICAL           WANTP, WANTQ
!C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), PT(LDPT,*), Q(LDQ,*), SV(*),
     *                  WORK(*)
!C     .. Local Scalars ..
      INTEGER           I, IER, IERR, J, K1, K2, K3, K4
!C     .. Local Arrays ..
      CHARACTER*47      REC(2)
!C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
!C     .. External Subroutines ..
      EXTERNAL          F01YDW, F01YDX, F01YDY, F02WEY, F02WEZ, F02WUW,
     *                  F06QFF, P01ABY, DCOPY, DGEMV
!C     .. Intrinsic Functions ..
      INTRINSIC         MIN
!C     .. Executable Statements ..
!C
!C     Check the input parameters.
!C
      IF ((M.EQ.0) .OR. (N.EQ.0)) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
         RETURN
      END IF
      IERR = 0
      IF (M.LT.0) CALL P01ABY(M,'M',IFAIL,IERR,SRNAME)
      IF (N.LT.0) CALL P01ABY(N,'N',IFAIL,IERR,SRNAME)
      IF (LDA.LT.M) CALL P01ABY(LDA,'LDA',IFAIL,IERR,SRNAME)
      IF (NCOLB.LT.0) CALL P01ABY(NCOLB,'NCOLB',IFAIL,IERR,SRNAME)
      IF ((LDB.LT.M) .AND. (NCOLB.GT.0)) CALL P01ABY(LDB,'LDB',IFAIL,
     *    IERR,SRNAME)
      IF ((LDQ.LT.M) .AND. (M.LT.N) .AND. (WANTQ)) CALL P01ABY(LDQ,
     *    'LDQ',IFAIL,IERR,SRNAME)
      IF ((LDPT.LT.N) .AND. (M.GE.N) .AND. (WANTQ) .AND. (WANTP))
     *    CALL P01ABY(LDPT,'LDPT',IFAIL,IERR,SRNAME)
      IF (IERR.GT.0) THEN
         WRITE (REC,FMT=99999) IERR
         IFAIL = P01ABF(IFAIL,-1,SRNAME,1,REC)
         RETURN
      END IF
!C
!C     Split up the workspace array  WORK. When m.ge.n then  k1 marks the
!C     start of  theta  ( see routine F01YDX ) and when  m.lt.n  then  k3
!C     marks the start of theta ( see routine F02WEY ). When WORK is used
!C     to hold a matrix, the matrix always starts at WORK( 1 ).  k3 marks
!C     the start of the workspace in the calls to  F02WUW.
!C
      IF (M.GE.N) THEN
         IF (WANTQ) THEN
            IF (WANTP) THEN
               K1 = 1
               K2 = 1 + N
               K3 = 1 + N**2
               K4 = K3
            ELSE
               K1 = 1 + N**2
               K2 = 1
               K3 = N + K1
               K4 = K1
            END IF
         ELSE
            K1 = 1
            K2 = 1 + N
            K3 = 1
         END IF
      ELSE IF (WANTP) THEN
         K3 = 1 + M**2
      ELSE
         K3 = 1
      END IF
!C
!C     Perform the factorization.
!C
      IERR = 1
      IER = 0
      IF (M.GE.N) THEN
!C
!C        Find the QR factorization of  A  and form  Q'*B.
!C
         CALL F01YDX(M,N,A,LDA,WORK(K1),IER)
         IF (NCOLB.GT.0) CALL F01YDY('Transpose','Separate',M,N,A,LDA,
     *                               WORK(K1),NCOLB,B,LDB,WORK(K2),IER)
         IF (WANTQ) THEN
            IF (WANTP) THEN
!C
!C              Copy R into PT, form the orthogonal matrix, Q1, of the QR
!C              factorization  in   A   and  find  the   SVD,   given  by
!C              R = Q2*D*P'.
!C
               CALL F06QFF('Upper triangular',N,N,A,LDA,PT,LDPT)
               CALL F01YDW('Separate',M,N,N,A,LDA,WORK(K1),WORK(K2),IER)
               CALL F02WUW(N,PT,LDPT,NCOLB,B,LDB,.TRUE.,WORK,N,SV,
     *                     .TRUE.,WORK(K3),IERR)
            ELSE
!C
!C              Find the  SVD of  R  given by  R = Q2*D*P'  and form  the
!C              othogonal  matrix,  Q1,  of the  QR factorization  in  A.
!C
               CALL F02WUW(N,A,LDA,NCOLB,B,LDB,.TRUE.,WORK,N,SV,.FALSE.,
     *                     WORK(K3),IERR)
               CALL F01YDW('Separate',M,N,N,A,LDA,WORK(K1),WORK(K3),IER)
            END IF
!C
!C           Form  Q = Q1*Q2,  row by row using  Q' = Q2'*Q1'.
!C
            DO 20 I = 1, M
               CALL DCOPY(N,A(I,1),LDA,WORK(K4),1)
               CALL DGEMV('Transpose',N,N,ONE,WORK,N,WORK(K4),1,ZERO,
     *                    A(I,1),LDA)
   20       CONTINUE
         ELSE
!C
!C           Find the SVD of R.
!C
            CALL F02WUW(N,A,LDA,NCOLB,B,LDB,.FALSE.,Q,LDQ,SV,WANTP,WORK,
     *                  IERR)
         END IF
      ELSE
!C
!C        Find the RQ factorization of A.
!C
         CALL F02WEY(M,N,A,LDA,WORK(K3),IER)
         IF (WANTP) THEN
!C
!C           Copy  R into  WORK,  form the orthogonal matrix, P1', of the
!C           RQ factorization  in  A  and  find the  SVD  of  R  given by
!C           R = Q*D*P2'.
!C
            CALL F06QFF('Upper triangular',M,M,A,LDA,WORK,M)
            CALL F02WEZ('Separate',M,N,M,A,LDA,WORK(K3),WORK(K3+M),IER)
            CALL F02WUW(M,WORK,M,NCOLB,B,LDB,WANTQ,Q,LDQ,SV,.TRUE.,
     *                  WORK(K3),IERR)
!C
!C           Form  P' = P2'*P1'.
!C
            DO 40 J = 1, N
               CALL DCOPY(M,A(1,J),1,WORK(K3),1)
               CALL DGEMV('No transpose',M,M,ONE,WORK,M,WORK(K3),1,ZERO,
     *                    A(1,J),1)
   40       CONTINUE
         ELSE
!C
!C           Find the SVD of R.
!C
            CALL F02WUW(M,A,LDA,NCOLB,B,LDB,WANTQ,Q,LDQ,SV,.FALSE.,WORK,
     *                  IERR)
         END IF
      END IF
      IF (K3.GT.1) CALL DCOPY(MIN(M,N),WORK(K3),1,WORK,1)
      IF (IERR.NE.0) THEN
         WRITE (REC,FMT=99998) IERR
         IFAIL = P01ABF(IFAIL,IERR,SRNAME,2,REC)
      ELSE
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
      END IF
      RETURN
!C
!C
!C     End of F02WEF.
!C
99999 FORMAT ('    The input parameters contained ',I2,' error(s)')
99998 FORMAT ('    The QR algorithm has failed to converge.',/'    ',I6,
     *       ' singular values have NOT been found.')
      END
!C***************************************************************************************************************************
      SUBROUTINE F01YDW(WHERET,M,N,NCOLQ,A,LDA,ZETA,WORK,IFAIL)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  1. Purpose
!C     =======
!C
!C  F01YDW  returns  the  first  ncolq columns  of the  m by m orthogonal
!C  matrix   Q,   where   Q  is  given  as  the  product  of  Householder
!C  transformation matrices.
!C
!C  This  routine  is  intended  for  use  following  NAG Fortran Library
!C  routine  F01YDX.
!C
!C  2. Description
!C     ===========
!C
!C  Q is assumed to be given by
!C
!C     Q = ( Q( n )*Q( n - 1 )*...*Q( 1 ) )',
!C
!C  Q( k ) being given in the form
!C
!C     Q( k ) = ( I     0   ),
!C              ( 0  T( k ) )
!C
!C  where
!C
!C     T( k ) = I - *u( k )*u( k )'
!C
!C     u( k ) = ( zeta( k ) ),
!C              (    z( k ) )
!C
!C  zeta( k )  is a scalar and  z( k )  is an  ( m - k )  element vector.
!C
!C  z( k )  must  be  supplied  in  the  kth  column  of  A  in  elements
!C  a( k + 1, k ), ..., a( m, k )  and  zeta( k ) must be supplied either
!C  in  a( k, k ) or in zeta( k ),  depending upon the parameter  WHERET.
!C
!C  3. Parameters
!C     ==========
!C
!C  WHERET - CHARACTER*1.
!C
!C           On entry,  WHERET  specifies where the elements of  zeta are
!C           to be found as follows.
!C
!C           WHERET = 'I' or 'i'   ( In A )
!C
!C              The elements of zeta are in A.
!C
!C           WHERET = 'S' or 's'   ( Separate )
!C
!C              The elements of zeta are separate from A, in  ZETA.
!C
!C           Unchanged on exit.
!C
!C  M      - INTEGER.
!C
!C           On entry, M  must specify the number of rows of A. M must be
!C           at least n.
!C
!C           Unchanged on exit.
!C
!C  N      - INTEGER.
!C
!C           On entry, N  must specify the number of columns of A. N must
!C           be at least zero.
!C
!C           Unchanged on exit.
!C
!C  NCOLQ  - INTEGER.
!C
!C           On entry, NCOLQ  must specify the required number of columns
!C           of Q.  NCOLQ must be at least zero and not be larger than m.
!C           When   NCOLQ = 0  then  an  immediate  return  is  effected.
!C
!C           Unchanged on exit.
!C
!C  A      - REAL             array of DIMENSION ( LDA, nca ),  where nca
!C           must be at least  max( n, ncolq ).
!C
!C           Before entry, the leading  M by N  stricly lower  triangular
!C           part of the array  A  must contain details of the matrix  Q.
!C           In  addition, when  WHERET = 'I' or 'i'  then  the  diagonal
!C           elements of A must contain the elements of zeta as described
!C           under the argument  ZETA  below.
!C
!C           On  exit, the  first  NCOLQ  columns  of  the  array  A  are
!C           overwritten  by  the  first  ncolq  columns  of  the  m by m
!C           orthogonal matrix  Q.
!C
!C           Unchanged on exit.
!C
!C  LDA    - INTEGER.
!C
!C           On  entry, LDA  must specify  the leading dimension  of  the
!C           array  A  as declared in the calling (sub) program. LDA must
!C           be at least m.
!C
!C           Unchanged on exit.
!C
!C  ZETA   - REAL             array of  DIMENSION  at least  ( n ),  when
!C           WHERET = 'S' or 's'.
!C
!C           Before entry with  WHERET = 'S' or 's', the array  ZETA must
!C           contain  the  elements  of  zeta.  If  ZETA( k ) = 0.0  then
!C           T( k )  is assumed to be  I, otherwise  ZETA( k ) is assumed
!C           to contain zeta( k ).
!C
!C           When WHERET = 'I' or 'i', the array  ZETA is not referenced.
!C
!C           Unchanged on exit.
!C
!C  WORK   - REAL             array of DIMENSION at least ( ncolq ).
!C
!C           Used as internal workspace.
!C
!C  IFAIL  - INTEGER.
!C
!C           Before entry,  IFAIL  must contain one of the values -1 or 0
!C           or 1 to specify noisy soft failure or noisy hard failure  or
!C           silent soft failure. ( See Chapter P01 for further details.)
!C
!C           On  successful exit  IFAIL  will be  zero,  otherwise  IFAIL
!C           will  be set to   -1  indicating that an input parameter has
!C           been  incorrectly  set. See  the  next  section  for further
!C           details.
!C
!C  4. Diagnostic Information
!C     ======================
!C
!C  IFAIL = -1
!C
!C     One or more of the following conditions holds:
!C
!C        WHERET .ne. 'I' or 'i' or 'S' or 's'
!C        M      .lt. N
!C        N      .lt. 0
!C        NCOLQ  .lt. 0  .or.  NCOLQ .gt. M
!C        LDA    .lt. M
!C
!C  If  on  entry,  IFAIL  was either  -1 or 0  then  further  diagnostic
!C  information  will  be  output  on  the  error message  channel. ( See
!C  routine  X04AAF. )
!C
!C
!C  Nag Fortran 77 Auxiliary linear algebra routine.
!C
!C  -- Written on 13-November-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0)
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F01YDW')
!C     .. Scalar Arguments ..
      INTEGER           IFAIL, LDA, M, N, NCOLQ
      CHARACTER*1       WHERET
!C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), WORK(*), ZETA(*)
!C     .. Local Scalars ..
      DOUBLE PRECISION  ZETAK
      INTEGER           IERR, K, NCQ, P
!C     .. Local Arrays ..
      CHARACTER*46      REC(1)
!C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
!C     .. External Subroutines ..
      EXTERNAL          F06FBF, F06QHF, P01ABW, P01ABY, DGEMV, DGER,
     *                  DSCAL
!C     .. Intrinsic Functions ..
      INTRINSIC         MIN
!C     .. Executable Statements ..
!C
!C     Check the input parameters.
!C
      IF (NCOLQ.EQ.0) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
         RETURN
      END IF
      IERR = 0
      IF ((WHERET.NE.'I') .AND. (WHERET.NE.'i') .AND. (WHERET.NE.'S')
     *     .AND. (WHERET.NE.'s')) CALL P01ABW(WHERET,'WHERET',IFAIL,
     *    IERR,SRNAME)
      IF (M.LT.N) CALL P01ABY(M,'M',IFAIL,IERR,SRNAME)
      IF (N.LT.0) CALL P01ABY(N,'N',IFAIL,IERR,SRNAME)
      IF ((NCOLQ.LT.0) .OR. (NCOLQ.GT.M)) CALL P01ABY(NCOLQ,'NCOLQ',
     *    IFAIL,IERR,SRNAME)
      IF (LDA.LT.M) CALL P01ABY(LDA,'LDA',IFAIL,IERR,SRNAME)
      IF (IERR.GT.0) THEN
         WRITE (REC,FMT=99999) IERR
         IFAIL = P01ABF(IFAIL,-1,SRNAME,1,REC)
         RETURN
      END IF
!C
!C     Start to form Q. First set the elements above the leading diagonal
!C     to zero.
!C
      P = MIN(N,NCOLQ)
      IF (P.GT.1) CALL F06QHF('Upper',P-1,P-1,ZERO,ZERO,A(1,2),LDA)
      IF (NCOLQ.GT.N) THEN
         NCQ = NCOLQ - N
!C
!C        Set the last  ( ncolq - n ) columns of  Q  to those of the unit
!C        matrix.
!C
         CALL F06QHF('General',N,NCOLQ-N,ZERO,ZERO,A(1,N+1),LDA)
         CALL F06QHF('General',M-N,NCOLQ-N,ZERO,ONE,A(N+1,N+1),LDA)
      ELSE
         NCQ = 0
      END IF
      DO 20 K = P, 1, -1
!C
!C        Q*E( ncolq ) = Q( 1 )'*...*Q( p )'*E( ncolq ), where E( ncolq )
!C        is the matrix containing the first ncolq columns of I.
!C
         IF ((WHERET.EQ.'S') .OR. (WHERET.EQ.'s')) THEN
            ZETAK = ZETA(K)
         ELSE
            ZETAK = A(K,K)
         END IF
         IF (ZETAK.GT.ZERO) THEN
            A(K,K) = ZETAK
!C
!C           Let C denote the bottom ( m - k + 1 ) by ncq part of Q.
!C
!C           First form  work = C'*u.
!C
            IF (K.LT.M) THEN
               CALL DGEMV('Transpose',M-K+1,NCQ,ONE,A(K,K+1),LDA,A(K,K),
     *                    1,ZERO,WORK,1)
!C
!C              Now form  C := C - u*work'.
!C
               CALL DGER(M-K+1,NCQ,-ONE,A(K,K),1,WORK,1,A(K,K+1),LDA)
            END IF
!C
!C           Now form the kth column of Q.
!C
            CALL DSCAL(M-K+1,-ZETAK,A(K,K),1)
            A(K,K) = ONE + A(K,K)
         ELSE
            A(K,K) = ONE
CXXA============================================	
CXXA		  ARRAY BOUNDS EXCEEDED
c		  WRITE(*,*)'ARRAY BOUNDS EXCEEDED'
CXXA            CALL F06FBF(M-K,ZERO,A(K+1,K),1)
		  CALL F06FBF(M-K,ZERO,A(K,K),1)
CXXA============================================
         END IF
         NCQ = NCQ + 1
   20 CONTINUE
!C
      IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
      RETURN
!C
!C     End of F01YDW. ( SGEFQ  )
!C
99999 FORMAT ('    The input parameters contained ',I2,' error(s)')
      END
!C*********************************************************************************************************************
      SUBROUTINE F01YDX(M,N,A,LDA,ZETA,IFAIL)
!C     MARK 13 RE-ISSUE. NAG COPYRIGHT 1988.
!C
!C  1. Purpose
!C     =======
!C
!C  F01YDX  finds  the  QR factorization  of the real  m by n,  m .ge. n,
!C  matrix A,  so that  A is reduced to upper triangular form by means of
!C  orthogonal transformations.
!C
!C  2. Description
!C     ===========
!C
!C  The m by n matrix A is factorized as
!C
!C     A = Q*( R )   when   m.gt.n,
!C           ( 0 )
!C
!C     A = Q*R       when   m = n,
!C
!C  where  Q  is an  m by m orthogonal matrix and  R  is an  n by n upper
!C  triangular matrix.
!C
!C  The  factorization  is  obtained  by  Householder's  method. The  kth
!C  transformation matrix, Q( k ), which is used  to introduce zeros into
!C  the kth column of A is given in the form
!C
!C     Q( k ) = ( I     0   ),
!C              ( 0  T( k ) )
!C
!C  where
!C
!C     T( k ) = I - u( k )*u( k )',
!C
!C     u( k ) = ( zeta( k ) ),
!C              (    z( k ) )
!C
!C  zeta( k )  is a scalar and  z( k )  is an  ( m - k )  element vector.
!C  zeta( k ) and z( k )  are chosen to annhilate the elements  below the
!C  triangular part of  A.
!C
!C  The vector  u( k ) is returned in the kth element of  ZETA and in the
!C  kth column of A, such that zeta( k ) is in ZETA( k ) and the elements
!C  of  z( k ) are in  a( k + 1, k ), ..., a( m, k ).  The elements of  R
!C  are returned in the upper triangular part of  A.
!C
!C  Q is given by
!C
!C     Q = ( Q( n )*Q( n - 1 )*...*Q( 1 ) )'.
!C
!C  3. Parameters
!C     ==========
!C
!C  M      - INTEGER.
!C
!C           On entry, M must specify the number of rows of  A. M must be
!C           at least  n.
!C
!C           Unchanged on exit.
!C
!C  N      - INTEGER.
!C
!C           On entry, N must specify the number of columns of  A. N must
!C           be  at  least zero. When  N = 0  then an immediate return is
!C           effected.
!C
!C           Unchanged on exit.
!C
!C  A      - REAL             array of DIMENSION ( LDA, n ).
!C
!C           Before entry, the leading  M by N  part of the array  A must
!C           contain the matrix to be factorized.
!C
!C           On exit, the  N by N upper triangular part of A will contain
!C           the upper triangular matrix R and the  M by N strictly lower
!C           triangular  part   of   A   will  contain  details   of  the
!C           factorization as described above.
!C
!C  LDA    - INTEGER.
!C
!C           On entry, LDA  must  specify  the  leading dimension of  the
!C           array  A  as declared in the calling (sub) program. LDA must
!C           be at least  m.
!C
!C           Unchanged on exit.
!C
!C  ZETA   - REAL             array of DIMENSION at least ( n ).
!C
!C           On exit,  ZETA( k )  contains the scalar  zeta( k )  for the
!C           kth  transformation.  If  T( k ) = I  then  ZETA( k ) = 0.0,
!C           otherwise  ZETA( k )  contains  zeta( k ) as described above
!C           and  zeta( k ) is always in the range  ( 1.0, sqrt( 2.0 ) ).
!C
!C  IFAIL  - INTEGER.
!C
!C           Before entry,  IFAIL  must contain one of the values -1 or 0
!C           or 1 to specify noisy soft failure or noisy hard failure  or
!C           silent soft failure. ( See Chapter P01 for further details.)
!C
!C           On successful  exit  IFAIL  will be  zero,  otherwise  IFAIL
!C           will  be set to  -1  indicating that an  input parameter has
!C           been  incorrectly  set. See  the  next section  for  further
!C           details.
!C
!C  4. Diagnostic Information
!C     ======================
!C
!C  IFAIL = -1
!C
!C     One or more of the following conditions holds:
!C
!C        M   .lt. N
!C        N   .lt. 0
!C        LDA .lt. M
!C
!C  If  on  entry,  IFAIL  was  either  -1 or 0  then  further diagnostic
!C  information  will  be  output  on  the  error message  channel. ( See
!C  routine  X04AAF. )
!C
!C  5. Further information
!C     ===================
!C
!C  Following the use of this routine the operations
!C
!C        B := Q*B   and   B := Q'*B,
!C
!C  where  B  is an  m by k  matrix, can  be  performed  by calls to  the
!C  NAG Library routine  F01YDY. The  operation  B := Q*B can be obtained
!C  by the call:
!C
!C     IFAIL = 0
!C     CALL F01YDY( 'No transpose', 'Separate', M, N, A, LDA, ZETA,
!C    $             K, B, LDB, WORK, IFAIL )
!C
!C  and  B := Q'*B  can be obtained by the call:
!C
!C     IFAIL = 0
!C     CALL F01YDY( 'Transpose', 'Separate', M, N, A, LDA, ZETA,
!C    $             K, B, LDB, WORK, IFAIL )
!C
!C  In  both  cases  WORK  must be a  k  element array  that  is used  as
!C  workspace. If  B  is a one-dimensional array (single column) then the
!C  parameter  LDB  can be replaced by  M. See routine F01YDY for further
!C  details.
!C
!C  The first k columns of the orthogonal matrix Q can either be obtained
!C  by setting  B to the first k columns of the unit matrix and using the
!C  first of the above two calls,  or by calling the  NAG Library routine
!C  F01YDW, which overwrites the k columns of Q on the first k columns of
!C  the array A.  Q is obtained by the call:
!C
!C     CALL F01YDW( 'Separate', M, N, K, A, LDA, ZETA, WORK, IFAIL )
!C
!C  As above WORK must be a k element array.  If K is larger than N, then
!C  A must have been declared to have at least K columns.
!C
!C  Operations involving the matrix  R  can readily  be performed by  the
!C  Level 2 BLAS  routines  STRSV  and STRMV  (see Chapter F06), but note
!C  that no test for  near singularity  of  R  is incorporated in STRSV .
!C  If  R  is singular,  or nearly singular then the  NAG Library routine
!C  F02WUW  can be  used to  determine  the  singular value decomposition
!C  of  R.
!C
!C
!C  Nag Fortran 77 Auxiliary linear algebra routine.
!C
!C  -- Written on 21-December-1985.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0)
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F01YDX')
!C     .. Scalar Arguments ..
      INTEGER           IFAIL, LDA, M, N
!C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), ZETA(*)
!C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP
      INTEGER           IERR, K, LA
!C     .. Local Arrays ..
      CHARACTER*46      REC(1)
!C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
!C     .. External Subroutines ..
      EXTERNAL          F06FRF, P01ABY, DGEMV, DGER
!C     .. Intrinsic Functions ..
      INTRINSIC         MIN
!C     .. Executable Statements ..
!C
!C     Check the input parameters.
!C
      IF (N.EQ.0) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
         RETURN
      END IF
      IERR = 0
      IF (M.LT.N) CALL P01ABY(M,'M',IFAIL,IERR,SRNAME)
      IF (N.LT.0) CALL P01ABY(N,'N',IFAIL,IERR,SRNAME)
      IF (LDA.LT.M) CALL P01ABY(LDA,'LDA',IFAIL,IERR,SRNAME)
      IF (IERR.GT.0) THEN
         WRITE (REC,FMT=99999) IERR
         IFAIL = P01ABF(IFAIL,-1,SRNAME,1,REC)
         RETURN
      END IF
!C
!C     Perform the factorization.
!C
      LA = LDA
      DO 20 K = 1, MIN(M-1,N)
!C
!C        Use a  Householder reflection  to  zero the  kth column  of  A.
!C        First set up the reflection.
!C
         CALL F06FRF(M-K,A(K,K),A(K+1,K),1,ZERO,ZETA(K))
         IF (ZETA(K).GT.ZERO) THEN
            IF ((K+1).EQ.N) LA = M - K + 1
!C
!C           Temporarily  store  beta and  put  zeta( k )  in  a( k, k ).
!C
            TEMP = A(K,K)
            A(K,K) = ZETA(K)
!C
!C           We now perform the operation  A := Q( k )*A.
!C
!C           Let  B  denote  the bottom  ( m - k + 1 ) by ( n - k )  part
!C           of  A.
!C
!C           First form   work = B'*u.  ( work  is stored in the elements
!C           ZETA( k + 1 ), ..., ZETA( n ). )
!C
            CALL DGEMV('Transpose',M-K+1,N-K,ONE,A(K,K+1),LA,A(K,K),1,
     *                 ZERO,ZETA(K+1),1)
!C
!C           Now form  B := B - u*work'.
!C
            CALL DGER(M-K+1,N-K,-ONE,A(K,K),1,ZETA(K+1),1,A(K,K+1),LA)
!C
!C           Restore beta.
!C
            A(K,K) = TEMP
         END IF
   20 CONTINUE
!C
!C     Find the final  ZETA  when  m.eq.n.
!C
      IF (M.EQ.N) ZETA(N) = ZERO
!C
      IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
      RETURN
!C
!C
!C     End of F01YDX. ( SGEQR )
!C
99999 FORMAT ('    The input parameters contained ',I2,' error(s)')
      END
!C****************************************************************************************************************
      SUBROUTINE F01YDY(TRANS,WHERET,M,N,A,LDA,ZETA,NCOLB,B,LDB,WORK,
     *                  IFAIL)
!C     MARK 13 RE-ISSUE. NAG COPYRIGHT 1988.
!C
!C  1. Purpose
!C     =======
!C
!C  F01YDY performs one of the transformations
!C
!C     B := Q*B   or   B := Q'*B,
!C
!C  where  B is an  m by ncolb  real matrix and  Q  is an  m by m unitary
!C  matrix, given as the product of  Householder transformation matrices.
!C
!C  This  routine  is  intended  for  use  following  NAG Fortran Library
!C  routine  F01YDX.
!C
!C  2. Description
!C     ===========
!C
!C  Q is assumed to be given by
!C
!C     Q = ( Q( n )*Q( n - 1 )*...*Q( 1 ) )',
!C
!C  Q( k ) being given in the form
!C
!C     Q( k ) = ( I     0   ),
!C              ( 0  T( k ) )
!C
!C  where
!C
!C     T( k ) = I - u( k )*u( k )'
!C
!C     u( k ) = ( zeta( k ) ),
!C              (    z( k ) )
!C
!C  zeta( k )  is a scalar and  z( k )  is an  ( m - k )  element vector.
!C
!C  z( k )  must  be  supplied  in  the  kth  column  of  A  in  elements
!C  a( k + 1, k ), ..., a( m, k )  and  zeta( k ) must be supplied either
!C  in  a( k, k ) or in zeta( k ),  depending upon the parameter  WHERET.
!C
!C  To obtain Q explicitly B may be set to I and premultiplied by Q. This
!C  is more efficient than obtaining Q'.
!C
!C  3. Parameters
!C     ==========
!C
!C  TRANS  - CHARACTER*1.
!C
!C           On entry, TRANS  specifies the operation to be performed  as
!C           follows.
!C
!C           TRANS = 'N' or 'n'  ( No transpose )
!C
!C              Perform the operation  B := Q*B.
!C
!C           TRANS = 'T' or 't' or 'C' or 'c'  ( Transpose )
!C
!C              Perform the operation  B := Q'*B.
!C
!C           Unchanged on exit.
!C
!C  WHERET - CHARACTER*1.
!C
!C           On entry,  WHERET  specifies where the elements of  zeta are
!C           to be found as follows.
!C
!C           WHERET = 'I' or 'i'   ( In A )
!C
!C              The elements of zeta are in A.
!C
!C           WHERET = 'S' or 's'   ( Separate )
!C
!C              The elements of zeta are separate from A, in ZETA.
!C
!C           Unchanged on exit.
!C
!C  M      - INTEGER.
!C
!C           On entry, M  must specify the number of rows of A. M must be
!C           at least n.
!C
!C           Unchanged on exit.
!C
!C  N      - INTEGER.
!C
!C           On entry, N  must specify the number of columns of A. N must
!C           be  at least zero. When  N = 0  then an immediate return  is
!C           effected.
!C
!C           Unchanged on exit.
!C
!C  A      - REAL             array of DIMENSION ( LDA, n ).
!C
!C           Before entry, the leading  M by N  stricly lower  triangular
!C           part of the array  A  must contain details of the matrix  Q.
!C           In  addition, when  WHERET = 'I' or 'i'  then  the  diagonal
!C           elements of A must contain the elements of zeta as described
!C           under the argument  ZETA  below.
!C
!C           When  WHERET = 'S' or 's'  then the diagonal elements of the
!C           array  A  are referenced, since they are used temporarily to
!C           store the  zeta( k ), but they contain their original values
!C           on return.
!C
!C           Unchanged on exit.
!C
!C  LDA    - INTEGER.
!C
!C           On  entry, LDA  must specify  the leading dimension  of  the
!C           array  A  as declared in the calling (sub) program. LDA must
!C           be at least m.
!C
!C           Unchanged on exit.
!C
!C  ZETA   - REAL             array of  DIMENSION  at least  ( n ),  when
!C           WHERET = 'S' or 's'.
!C
!C           Before entry with  WHERET = 'S' or 's', the array  ZETA must
!C           contain  the  elements  of  zeta.  If  ZETA( k ) = 0.0  then
!C           T( k )  is assumed  to be  I otherwise  ZETA( k ) is assumed
!C           to contain zeta( k ).
!C
!C           When WHERET = 'I' or 'i', the array  ZETA is not referenced.
!C
!C           Unchanged on exit.
!C
!C  NCOLB  - INTEGER.
!C
!C           On  entry, NCOLB  must specify  the number of columns of  B.
!C           NCOLB  must  be  at  least  zero.  When  NCOLB = 0  then  an
!C           immediate return is effected.
!C
!C           Unchanged on exit.
!C
!C  B      - REAL             array of DIMENSION ( LDB, ncolb ).
!C
!C           Before entry, the leading  M by NCOLB  part of  the array  B
!C           must  contain  the matrix to be  transformed.
!C
!C           On  exit,  B  is  overwritten  by  the  transformed  matrix.
!C
!C  LDB    - INTEGER.
!C
!C           On  entry, LDB  must specify  the  leading dimension of  the
!C           array  B as declared in the calling (sub) program. LDB  must
!C           be at least m.
!C
!C           Unchanged on exit.
!C
!C  WORK   - REAL             array of DIMENSION at least ( ncolb ).
!C
!C           Used as internal workspace.
!C
!C  IFAIL  - INTEGER.
!C
!C           Before entry,  IFAIL  must contain one of the values -1 or 0
!C           or 1 to specify noisy soft failure or noisy hard failure  or
!C           silent soft failure. ( See Chapter P01 for further details.)
!C
!C           On  successful exit  IFAIL  will be  zero,  otherwise  IFAIL
!C           will  be set to   -1  indicating that an input parameter has
!C           been  incorrectly  set. See  the  next  section  for further
!C           details.
!C
!C  4. Diagnostic Information
!C     ======================
!C
!C  IFAIL = -1
!C
!C     One or more of the following conditions holds:
!C
!C        TRANS  .ne. 'N' or 'n' or 'T' or 't' or 'C' or 'c'
!C        WHERET .ne. 'I' or 'i' or 'S' or 's'
!C        M      .lt. N
!C        N      .lt. 0
!C        LDA    .lt. M
!C        NCOLB  .lt. 0
!C        LDB    .lt. M
!C
!C  If  on  entry,  IFAIL  was either  -1 or 0  then  further  diagnostic
!C  information  will  be  output  on  the  error message  channel. ( See
!C  routine  X04AAF. )
!C
!C
!C  Nag Fortran 77 Auxiliary linear algebra routine.
!C
!C  -- Written on 13-November-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0)
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F01YDY')
!C     .. Scalar Arguments ..
      INTEGER           IFAIL, LDA, LDB, M, N, NCOLB
      CHARACTER*1       TRANS, WHERET
!C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), WORK(*), ZETA(*)
!C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP, ZETAK
      INTEGER           IERR, K, KK, LB
!C     .. Local Arrays ..
      CHARACTER*46      REC(1)
!C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
!C     .. External Subroutines ..
      EXTERNAL          P01ABW, P01ABY, DGEMV, DGER
!C     .. Intrinsic Functions ..
      INTRINSIC         MIN
!C     .. Executable Statements ..
!C
!C     Check the input parameters.
!C
      IF (MIN(N,NCOLB).EQ.0) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
         RETURN
      END IF
      IERR = 0
      IF ((TRANS.NE.'N') .AND. (TRANS.NE.'n') .AND. (TRANS.NE.'T')
     *    .AND. (TRANS.NE.'t') .AND. (TRANS.NE.'C') .AND. (TRANS.NE.'c')
     *    ) CALL P01ABW(TRANS,'TRANS',IFAIL,IERR,SRNAME)
      IF ((WHERET.NE.'I') .AND. (WHERET.NE.'i') .AND. (WHERET.NE.'S')
     *     .AND. (WHERET.NE.'s')) CALL P01ABW(WHERET,'WHERET',IFAIL,
     *    IERR,SRNAME)
      IF (M.LT.N) CALL P01ABY(M,'M',IFAIL,IERR,SRNAME)
      IF (N.LT.0) CALL P01ABY(N,'N',IFAIL,IERR,SRNAME)
      IF (LDA.LT.M) CALL P01ABY(LDA,'LDA',IFAIL,IERR,SRNAME)
      IF (NCOLB.LT.0) CALL P01ABY(NCOLB,'NCOLB',IFAIL,IERR,SRNAME)
      IF (LDB.LT.M) CALL P01ABY(LDB,'LDB',IFAIL,IERR,SRNAME)
      IF (IERR.GT.0) THEN
         WRITE (REC,FMT=99999) IERR
         IFAIL = P01ABF(IFAIL,-1,SRNAME,1,REC)
         RETURN
      END IF
!C
!C     Perform the transformation.
!C
      LB = LDB
      DO 20 KK = 1, N
         IF ((TRANS.EQ.'T') .OR. (TRANS.EQ.'t') .OR. (TRANS.EQ.'C')
     *       .OR. (TRANS.EQ.'c')) THEN
!C
!C           Q'*B = Q( n )*...*Q( 2 )*Q( 1 )*B,
!C
            K = KK
         ELSE
!C
!C           Q*B  = Q( 1 )'*Q( 2 )'*...*Q( n )'*B,
!C
            K = N + 1 - KK
         END IF
         IF ((WHERET.EQ.'S') .OR. (WHERET.EQ.'s')) THEN
            ZETAK = ZETA(K)
         ELSE
            ZETAK = A(K,K)
         END IF
         IF (ZETAK.GT.ZERO) THEN
            TEMP = A(K,K)
            A(K,K) = ZETAK
            IF (NCOLB.EQ.1) LB = M - K + 1
!C
!C           Let C denote the bottom ( m - k + 1 ) by ncolb part of B.
!C
!C           First form  work = C'*u.
!C
            CALL DGEMV('Transpose',M-K+1,NCOLB,ONE,B(K,1),LB,A(K,K),1,
     *                 ZERO,WORK,1)
!C
!C           Now form  C := C - u*work'.
!C
            CALL DGER(M-K+1,NCOLB,-ONE,A(K,K),1,WORK,1,B(K,1),LB)
!C
!C           Restore the diagonal element of A.
!C
            A(K,K) = TEMP
         END IF
   20 CONTINUE
!C
      IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
      RETURN
!C
!C
!C     End of F01YDY. ( SGEAPQ )
!C
99999 FORMAT ('    The input parameters contained ',I2,' error(s)')
      END
!C*************************************************************************************************************
      SUBROUTINE F02WEY(M,N,A,LDA,ZETA,IFAIL)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  1. Purpose
!C     =======
!C
!C  F02WEY  finds the  RQ factorization  of the  real  m by n,  m .le. n,
!C  matrix  A, so that  A is reduced to upper triangular form by means of
!C  orthogonal transformations from the right.
!C
!C  2. Description
!C     ===========
!C
!C  The m by n matrix A is factorized as
!C
!C     A = ( R  0 )*P'   when   m.lt.n,
!C
!C     A = R*P'          when   m = n,
!C
!C  where  P  is an  n by n orthogonal matrix and  R  is an  m by m upper
!C  triangular matrix.
!C
!C  P  is  given  as a  sequence  of  Householder transformation matrices
!C
!C     P = P( m )*...*P( 2 )*P( 1 ),
!C
!C  the  ( m - k + 1 )th  transformation matrix,  P( k ),  being used  to
!C  introduce zeros into the  kth row of A.  P( k ) has the form
!C
!C     P( k ) = I - u( k )*u( k )',
!C
!C  where
!C
!C     u( k ) = (    w( k ) ),
!C              ( zeta( k ) )
!C              (    0      )
!C              (    z( k ) )
!C
!C  zeta( k )  is a scalar,  w( k ) is an  ( k - 1 )  element  vector and
!C  z( k ) is an ( n - m ) element vector. u( k ) is chosen to annihilate
!C  the elements in the kth row of A.
!C
!C  The vector  u( k ) is returned in the kth element of  ZETA and in the
!C  kth row of  A, such that  zeta( k ) is in  ZETA( k ), the elements of
!C  w( k )  are in  a( k, 1 ), ..., a( k, k - 1 )  and  the  elements  of
!C  z( k ) are in  a( k, m + 1 ), ..., a( k, n ).  The elements of  R are
!C  returned in the  upper triangular part of  A.
!C
!C  3. Parameters
!C     ==========
!C
!C  M      - INTEGER.
!C
!C           On entry, M must specify the number of rows of  A. M must be
!C           at  least  zero. When  M = 0  then  an  immediate return  is
!C           effected.
!C
!C           Unchanged on exit.
!C
!C  N      - INTEGER.
!C
!C           On entry, N must specify the number of columns of  A. N must
!C           be at least m.
!C
!C           Unchanged on exit.
!C
!C  A      - REAL             array of DIMENSION ( LDA, n ).
!C
!C           Before entry, the leading  M by N  part of the array  A must
!C           contain the matrix to be factorized.
!C
!C           On exit, the  M by M upper triangular part of A will contain
!C           the  upper triangular  matrix  R,  and the  M by M  strictly
!C           lower  triangular  part  of   A  and  the   M  by  ( N - M )
!C           rectangular part of  A  to the right of the upper triangular
!C           part will contain details of the  factorization as described
!C           above.
!C
!C  LDA    - INTEGER.
!C
!C           On entry, LDA  must  specify  the  leading dimension of  the
!C           array  A  as declared in the calling (sub) program. LDA must
!C           be at least  m.
!C
!C           Unchanged on exit.
!C
!C  ZETA   - REAL             array of DIMENSION at least ( m ).
!C
!C           On exit,  ZETA( k )  contains the scalar  zeta( k )  for the
!C           ( m - k + 1 )th   transformation.     If   P( k ) = I   then
!C           ZETA( k ) = 0.0,   otherwise  ZETA( k )  contains  zeta( k )
!C           as  described above  and  zeta( k )  is always in the  range
!C           ( 1.0, sqrt( 2.0 ) ).
!C
!C  IFAIL  - INTEGER.
!C
!C           Before entry,  IFAIL  must contain one of the values -1 or 0
!C           or 1 to specify noisy soft failure or noisy hard failure  or
!C           silent soft failure. ( See Chapter P01 for further details.)
!C
!C           On successful  exit  IFAIL  will be  zero,  otherwise  IFAIL
!C           will  be set to  -1  indicating that an  input parameter has
!C           been  incorrectly  set. See  the  next section  for  further
!C           details.
!C
!C  4. Diagnostic Information
!C     ======================
!C
!C  IFAIL = -1
!C
!C     One or more of the following conditions holds:
!C
!C        M   .lt. 0
!C        N   .lt. M
!C        LDA .lt. M
!C
!C  If  on  entry,  IFAIL  was  either  -1 or 0  then  further diagnostic
!C  information  will  be  output  on  the  error message  channel. ( See
!C  routine  X04AAF. )
!C
!C  5. Further information
!C     ===================
!C
!C  The first  k rows  of the  orthogonal matrix  P'  can be  obtained by
!C  by calling NAG Library routine F02WEZ, which overwrites the k rows of
!C  P'  on the first  k rows of the array A.  P' is obtained by the call:
!C
!C     IFAIL = 0
!C     CALL F02WEZ( 'Separate', M, N, K, A, LDA, ZETA, WORK, IFAIL )
!C
!C  WORK must be a  max( m - 1, k - m, 1 ) element array.  If K is larger
!C  than  M,  then  A  must have been declared to have at least  K  rows.
!C
!C  Operations involving the matrix  R  can readily  be performed by  the
!C  Level 2 BLAS  routines  STRSV and STRMV , (see Chapter F06), but note
!C  that no test for  near singularity of  R  is incorporated in  STRSV .
!C  If  R  is singular,  or nearly singular then the  NAG Library routine
!C  F02WUW  can be  used to  determine  the  singular value decomposition
!C  of  R.
!C
!C
!C  Nag Fortran 77 Auxiliary linear algebra routine.
!C
!C  -- Written on 17-November-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0)
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F02WEY')
!C     .. Scalar Arguments ..
      INTEGER           IFAIL, LDA, M, N
!C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), ZETA(*)
!C     .. Local Scalars ..
      DOUBLE PRECISION  BETA
      INTEGER           IERR, K, MP1
!C     .. Local Arrays ..
      CHARACTER*46      REC(1)
!C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
!C     .. External Subroutines ..
      EXTERNAL          F02WEX, P01ABY, DGEMV, DGER
!C     .. Executable Statements ..
!C
!C     Check the input parameters.
!C
      IF (N.EQ.0) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
         RETURN
      END IF
      IERR = 0
      IF (M.LT.0) CALL P01ABY(M,'M',IFAIL,IERR,SRNAME)
      IF (N.LT.M) CALL P01ABY(N,'N',IFAIL,IERR,SRNAME)
      IF (LDA.LT.M) CALL P01ABY(LDA,'LDA',IFAIL,IERR,SRNAME)
      IF (IERR.GT.0) THEN
         WRITE (REC,FMT=99999) IERR
         IFAIL = P01ABF(IFAIL,-1,SRNAME,1,REC)
         RETURN
      END IF
!C
!C     Perform the factorization.
!C
      IF (M.LT.N) THEN
         MP1 = M + 1
      ELSE
         MP1 = M
      END IF
      DO 20 K = M, 1, -1
!C
!C        Use  a  Householder  reflection  to  zero  the  kth row  of  A.
!C        First set up the reflection.
!C
         BETA = A(K,K)
         CALL F02WEX(K-1,N-M,BETA,A(K,1),LDA,A(K,MP1),LDA,ZERO,ZETA(K))
         IF (ZETA(K).GT.ZERO) THEN
!C
!C           Put  zeta( k ) in a( k, k ).
!C
            A(K,K) = ZETA(K)
!C
!C           We now perform the operation    A := A*P( k ).
!C
!C           Let  A1  denote the first k columns and  A2  denote the last
!C           ( n - m ) columns of the first ( k - 1 ) rows of A and let x
!C           be given by
!C
!C              x = (    w( k ) ).
!C                  ( zeta( k ) )
!C
!C           Form  v = A1*x  in  ZETA.
!C
            CALL DGEMV('No transpose',K-1,K,ONE,A,LDA,A(K,1),LDA,ZERO,
     *                 ZETA,1)
!C
!C           Now form  v := A2*z( k ) + v  in ZETA.
!C
            CALL DGEMV('No transpose',K-1,N-M,ONE,A(1,MP1),LDA,A(K,MP1),
     *                 LDA,ONE,ZETA,1)
!C
!C           Now form  A1 := A1 - v*x'
!C           and       A2 := A2 - v*z( k )'.
!C
            CALL DGER(K-1,K,-ONE,ZETA,1,A(K,1),LDA,A,LDA)
            CALL DGER(K-1,N-M,-ONE,ZETA,1,A(K,MP1),LDA,A(1,MP1),LDA)
         END IF
         A(K,K) = BETA
   20 CONTINUE
!C
      IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
      RETURN
!C
!C
!C     End of F02WEY. ( DGERQ )
!C
99999 FORMAT ('    The input parameters contained ',I2,' error(s)')
      END
!C*************************************************************************************************************
      SUBROUTINE F02WEZ(WHERET,M,N,NROWP,A,LDA,ZETA,WORK,IFAIL)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  1. Purpose
!C     =======
!C
!C  F02WEZ  returns the first  NROWP rows of the m by m orthogonal matrix
!C  P',  where  P  is given as the product of  Householder transformation
!C  matrices, details of which are stored in the  m by n ( m.le.n ) array
!C  A  and,  if the parameter  WHERET = 'S' or 's',  in the  array  ZETA.
!C
!C  This  routine  is  intended  for  use  following  NAG Library routine
!C  F02WEY.
!C
!C  2. Description
!C     ===========
!C
!C  P is assumed to be given by
!C
!C     P = P( m )*P( m - 1 )*...*P( 1 ),
!C
!C  where
!C
!C     P( k ) = I - u( k )*u( k )',
!C
!C     u( k ) = (    w( k ) ),
!C              ( zeta( k ) )
!C              (    0      )
!C              (    z( k ) )
!C
!C  zeta( k )  is a scalar,  w( k )  is a  ( k - 1 )  element vector  and
!C  z( k )  is an  ( n - m ) element vector.
!C
!C  w( k )  must be supplied in the kth row of A in  elements  a( k, 1 ),
!C  ..., a( k, k - 1 ).   z( k ) must be  supplied  in  the  kth  row  of
!C  A  in elements  a( k, m + 1 ), ..., a( k, n )  and  zeta( k ) must be
!C  supplied  either in  a( k, k )  or in  ZETA( k ),  depending upon the
!C  parameter WHERET.
!C
!C  3. Parameters
!C     ==========
!C
!C  WHERET - CHARACTER*1.
!C
!C           On entry,  WHERET  specifies where the elements of  zeta are
!C           to be found as follows.
!C
!C           WHERET = 'I' or 'i'   ( In A )
!C
!C              The elements of zeta are in A.
!C
!C           WHERET = 'S' or 's'   ( Separate )
!C
!C              The  elements of  zeta  are  separate  from  A, in  ZETA.
!C
!C           Unchanged on exit.
!C
!C  M      - INTEGER.
!C
!C           On entry, M  must specify the number of rows of A. M must be
!C           at least zero.
!C
!C           Unchanged on exit.
!C
!C  N      - INTEGER.
!C
!C           On entry, N  must specify the number of columns of A. N must
!C           be at least m.
!C
!C           Unchanged on exit.
!C
!C  NROWP  - INTEGER.
!C
!C           On entry,  NROWP  must  specify  the required number of rows
!C           of P.  NROWP must be at least zero and not be larger than n.
!C           When   NROWP = 0  then  an  immediate  return  is  effected.
!C
!C           Unchanged on exit.
!C
!C  A      - REAL             array of DIMENSION ( LDA, n ).
!C
!C           Before entry, the leading  M by M  strictly lower triangular
!C           part of the array A, and the M by ( N - M ) rectangular part
!C           of A with top left hand corner at element A( 1, M + 1 ) must
!C           contain  details  of  the  matrix   P.   In  addition,  when
!C           WHERET = 'I' or 'i'  then  the  diagonal elements of  A must
!C           contain the elements of zeta.
!C
!C           On exit, the first NROWP rows of the array A are overwritten
!C           by the first  NROWP  rows  of the  n by n  orthogonal matrix
!C           P'.
!C
!C           Unchanged on exit.
!C
!C  LDA    - INTEGER.
!C
!C           On  entry, LDA  must specify  the leading dimension  of  the
!C           array  A  as declared in the calling (sub) program. LDA must
!C           be at least m.
!C
!C           Unchanged on exit.
!C
!C  ZETA   - REAL             array of  DIMENSION  at least  ( m ),  when
!C           WHERET = 'S' or 's'.
!C
!C           Before entry with  WHERET = 'S' or 's', the array  ZETA must
!C           contain  the  elements  of  zeta.  If  ZETA( k ) = 0.0  then
!C           P( k )  is assumed to be I, otherwise  ZETA( k )  is assumed
!C           to contain zeta( k ).
!C
!C           When  WHERET = 'I' or 'i', the array ZETA is not referenced.
!C
!C           Unchanged on exit.
!C
!C  WORK   - REAL             array  of  DIMENSION  at  least  ( lwork ),
!C           where  lwork = max( m - 1, nrowp - m, 1 ).
!C
!C           Used as internal workspace.
!C
!C  IFAIL  - INTEGER.
!C
!C           Before entry,  IFAIL  must contain one of the values -1 or 0
!C           or 1 to specify noisy soft failure or noisy hard failure  or
!C           silent soft failure. ( See Chapter P01 for further details.)
!C
!C           On  successful exit  IFAIL  will be  zero,  otherwise  IFAIL
!C           will  be set to   -1  indicating that an input parameter has
!C           been  incorrectly  set. See  the  next  section  for further
!C           details.
!C
!C  4. Diagnostic Information
!C     ======================
!C
!C  IFAIL = -1
!C
!C     One or more of the following conditions holds:
!C
!C        WHERET .ne. 'I' or 'i' or 'S' or 's'
!C        M      .lt. 0
!C        N      .lt. M
!C        NROWP  .lt. 0  .or.  NROWP .gt. N
!C        LDA    .lt. M
!C
!C  If  on  entry,  IFAIL  was either  -1 or 0  then  further  diagnostic
!C  information  will  be  output  on  the  error message  channel. ( See
!C  routine  X04AAF. )
!C
!C
!C  Nag Fortran 77 Auxiliary linear algebra routine.
!C
!C  -- Written on 3-December-1987.
!C     Sven Hammarling and Mick Pont, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0)
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F02WEZ')
!C     .. Scalar Arguments ..
      INTEGER           IFAIL, LDA, M, N, NROWP
      CHARACTER*1       WHERET
!C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), WORK(*), ZETA(*)
!C     .. Local Scalars ..
      DOUBLE PRECISION  ZETAK
      INTEGER           IERR, K, MP1, NRP
!C     .. Local Arrays ..
      CHARACTER*46      REC(1)
!C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
!C     .. External Subroutines ..
      EXTERNAL          F06FBF, F06QHF, P01ABW, P01ABY, DGEMV, DGER,
     *                  DSCAL
!C     .. Executable Statements ..
!C
!C     Check the input parameters.
!C
      IF (NROWP.EQ.0) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
         RETURN
      END IF
      IERR = 0
      IF ((WHERET.NE.'I') .AND. (WHERET.NE.'i') .AND. (WHERET.NE.'S')
     *     .AND. (WHERET.NE.'s')) CALL P01ABW(WHERET,'WHERET',IFAIL,
     *    IERR,SRNAME)
      IF (M.LT.0) CALL P01ABY(M,'M',IFAIL,IERR,SRNAME)
      IF (N.LT.M) CALL P01ABY(N,'N',IFAIL,IERR,SRNAME)
      IF ((NROWP.LT.0) .OR. (NROWP.GT.N)) CALL P01ABY(NROWP,'NROWP',
     *    IFAIL,IERR,SRNAME)
      IF (LDA.LT.M) CALL P01ABY(LDA,'LDA',IFAIL,IERR,SRNAME)
      IF (IERR.GT.0) THEN
         WRITE (REC,FMT=99999) IERR
         IFAIL = P01ABF(IFAIL,-1,SRNAME,1,REC)
         RETURN
      END IF
!C
      IF (N.GT.M) THEN
         MP1 = M + 1
      ELSE
         MP1 = M
      END IF
!C
!C     Start to form P. First set the elements above the leading diagonal
!C     to zero.
!C
      IF (M.GT.1) CALL F06QHF('Upper',M-1,M-1,ZERO,ZERO,A(1,2),LDA)
      IF (NROWP.GT.M) THEN
         NRP = NROWP - M
!C
!C        Set the last  ( nrowp - m )  rows of  P  to  those  of the unit
!C        matrix.
!C
         CALL F06QHF('General',NRP,M,ZERO,ZERO,A(MP1,1),LDA)
         CALL F06QHF('General',NRP,N-M,ZERO,ONE,A(MP1,MP1),LDA)
      ELSE
         NRP = 0
      END IF
      DO 20 K = 1, M
!C
!C        E( nrowp )*P' = E( nrowp )*P( 1 )'*...*P( m )',
!C
!C        where E( nrowp )  is the matrix containing the first nrowp rows
!C        of I.
!C
         IF ((WHERET.EQ.'S') .OR. (WHERET.EQ.'s')) THEN
            ZETAK = ZETA(K)
         ELSE
            ZETAK = A(K,K)
         END IF
!C
!C        If  ZETA( k ) .eq. zero  then P( k ) is special.
!C
         IF (ZETAK.GT.ZERO) THEN
            A(K,K) = ZETAK
!C
!C           At the k( th ) step, we partition the matrix
!C
!C              B = E( nrowp )*P( 1 )'*...*P( k - 1 )',
!C
!C           as
!C
!C              B =   ( B1 0 B2 ),
!C                    (  0 I  0 )
!C                    ( B3 0 B4 )
!C
!C           where  B1  has dimensions  k by k,  B2  has dimensions  k by
!C           ( n - m ),  B3  has dimensions  ( n - m ) by k  and  B4  has
!C           dimensions  ( n - m ) by ( n - m ).  Also,  we partition the
!C           vector  u( k )  as
!C
!C              u( k ) = ( u1 ).
!C                       (  0 )
!C                       ( u2 )
!C
!C           First form  v1 as all but the last element of B1*u1 + B2*u2,
!C           and store it in  work( 1 ), ..., work( k - 1 ).
!C
            CALL DGEMV('No transpose',K-1,K-1,ONE,A,LDA,A(K,1),LDA,ZERO,
     *                 WORK,1)
            CALL DGEMV('No transpose',K-1,N-M,ONE,A(1,MP1),LDA,A(K,MP1),
     *                 LDA,ONE,WORK,1)
!C
!C           Now  form  all  but  the   last  row   of  the  new   B1  as
!C           B1 := B1 - v1*u1'.
!C
            CALL DGER(K-1,K,-ONE,WORK,1,A(K,1),LDA,A,LDA)
!C
!C           Form  all   but   the   last  row   of   the   new   B2   as
!C           B2 := B2 - v1*u2'.
!C
            CALL DGER(K-1,N-M,-ONE,WORK,1,A(K,MP1),LDA,A(1,MP1),LDA)
!C
!C           Then form   v2 = B4*u2 + B3*u1,  and store it in  work( 1 ),
!C           ..., work( nrp ).
!C
            CALL DGEMV('No transpose',NRP,N-M,ONE,A(MP1,MP1),LDA,A(K,
     *                 MP1),LDA,ZERO,WORK(K),1)
            CALL DGEMV('No transpose',NRP,K-1,ONE,A(MP1,1),LDA,A(K,1),
     *                 LDA,ONE,WORK(K),1)
!C
!C           Form the new  B3  as  B3 := B3 - v2*u1'.
!C
            CALL DGER(NRP,K,-ONE,WORK(K),1,A(K,1),LDA,A(MP1,1),LDA)
!C
!C           Form the new  B4  as  B4 := B4 - v2*u2'.
!C
            CALL DGER(NRP,N-M,-ONE,WORK(K),1,A(K,MP1),LDA,A(MP1,MP1),
     *                LDA)
!C
!C           Now form the last rows of the new  B1 and B2.
!C
            CALL DSCAL(K,-ZETAK,A(K,1),LDA)
            A(K,K) = ONE + A(K,K)
            CALL DSCAL(N-M,-ZETAK,A(K,MP1),LDA)
         ELSE
            A(K,K) = ONE
            CALL F06FBF(K-1,ZERO,A(K,1),LDA)
            CALL F06FBF(N-M,ZERO,A(K,MP1),LDA)
         END IF
   20 CONTINUE
!C
      IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
      RETURN
!C
!C
!C     End of F02WEZ. ( SGEFP )
!C
99999 FORMAT ('    The input parameters contained ',I2,' error(s)')
      END
!C****************************************************************************************************
      SUBROUTINE F02WUW(N,A,LDA,NCOLB,B,LDB,WANTQ,Q,LDQ,SV,WANTP,WORK,
     *                  IFAIL)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  1. Purpose
!C     =======
!C
!C  F02WUW  returns all, or part, of the  singular value decomposition of
!C  a real upper triangular matrix.
!C
!C  2. Description
!C     ===========
!C
!C  The n by n upper triangular matrix R is factorized as
!C
!C     R = Q*S*P',
!C
!C  where  Q and P  are  n by n orthogonal matrices and  S  is an  n by n
!C  diagonal  matrix   with  non-negative  diagonal  elements,   sv( 1 ),
!C  sv( 2 ), ..., sv( n ), ordered such that
!C
!C     sv( 1 ) .ge. sv( 2 ) .ge. ... .ge. sv( n ) .ge. 0.
!C
!C  The  columns of  Q  are the  left-hand  singular vectors  of  R,  the
!C  diagonal elements of  S are the singular values of  R and the columns
!C  of  P are the right-hand singular vectors of  R.
!C
!C  Either or both of Q and P' may be requested and the matrix C given by
!C
!C     C = Q'*B,
!C
!C  where B is an n by ncolb given matrix, may also be requested.
!C
!C  The  routine  obtains  the  singular  value  decomposition  by  first
!C  reducing  R  to  bidiagonal form  by means of  Givens plane rotations
!C  and  then  using  the  QR algorithm  to  obtain  the  singular  value
!C  decomposition  of the  bidiagonal form.
!C
!C  3. Parameters
!C     ==========
!C
!C  N      - INTEGER.
!C
!C           On entry,  N must specify the order of the matrix R.  N must
!C           be  at  least zero. When  N = 0  then an immediate return is
!C           effected.
!C
!C           Unchanged on exit.
!C
!C  A      - REAL             array of DIMENSION ( LDA, n ).
!C
!C           Before entry,  the leading  N by N  upper triangular part of
!C           the array  A  must contain the  upper triangular  matrix  R.
!C
!C           If  WANTP is .TRUE.  then on exit, the N by N part of A will
!C           contain  the  n by n  orthogonal matrix  P',  otherwise  the
!C           N by N  upper triangular  part  of  A  is  used as  internal
!C           workspace,  but  the  strictly lower triangular  part  of  A
!C           is not referenced.
!C
!C  LDA    - INTEGER.
!C
!C           On entry, LDA  must  specify  the  leading dimension of  the
!C           array  A  as declared in the calling (sub) program. LDA must
!C           be at least  n.
!C
!C           Unchanged on exit.
!C
!C  NCOLB  - On entry,  NCOLB  must specify the  number of columns of the
!C           matrix  B  and must be at least  zero.  When  NCOLB = 0  the
!C           array  B  is not referenced.
!C
!C  B      - REAL             array of DIMENSION ( LDB, ncolb ).
!C
!C           Before entry with  NCOLB .gt. 0, the leading N by NCOLB part
!C           of the array  B  must contain the  matrix to be  transformed
!C           and  on exit,  B  is overwritten  by the  n by ncolb  matrix
!C           Q'*B.
!C
!C  LDB    - INTEGER.
!C
!C           On entry, LDB  must  specify  the  leading dimension of  the
!C           array  B  as declared  in the  calling  (sub) program.  When
!C           NCOLB .gt. 0  then LDB must be at least  n.
!C
!C           Unchanged on exit.
!C
!C  WANTQ  - LOGICAL.
!C
!C           On entry,  WANTQ must be .TRUE. if the matrix Q is required.
!C           If  WANTQ is .FALSE.  then  the array  Q  is not referenced.
!C
!C           Unchanged on exit.
!C
!C  Q      - REAL             array of DIMENSION ( LDQ, n ).
!C
!C           On exit with  WANTQ as .TRUE.,  the leading  N by N  part of
!C           the array Q will contain the orthogonal matrix Q.  Otherwise
!C           the array  Q  is not referenced.
!C
!C  LDQ    - INTEGER.
!C
!C           On entry, LDQ  must  specify  the  leading dimension of  the
!C           array  Q  as declared  in the  calling  (sub) program.  When
!C           WANTQ is .TRUE.,  LDQ  must be at least n.
!C
!C           Unchanged on exit.
!C
!C  SV     - REAL array of DIMENSION at least ( n ).
!C
!C           On exit, the array SV will contain the n diagonal elements
!C           of the matrix S.
!C
!C  WANTP  - LOGICAL.
!C
!C           On entry, WANTP must be .TRUE. if the matrix P' is required,
!C           in which case  P'  is overwritten on the array A,  otherwise
!C           WANTP must be .FALSE..
!C
!C           Unchanged on exit.
!C
!C  WORK   - REAL array of DIMENSION at least  ( max( 1, lwork ) ), where
!C           lwork must satisfy:
!C
!C              lwork = 2*( n - 1 ) when
!C                 ncolb = 0  and  WANTQ and WANTP are .FALSE.,
!C
!C              lwork = 3*( n - 1 ) when
!C                 either  ncolb = 0  and  WANTQ is .FALSE.  and
!C                 WANTP is .TRUE.,  or  WANTP is .FALSE.  and  one or
!C                 both of  ncolb .gt. 0  and  WANTQ is .TRUE.
!C
!C              lwork = 5*( n - 1 ) otherwise.
!C
!C           The array   WORK  is used as  internal workspace by  F06XUF.
!C           On exit,  WORK( n )  contains the total number of iterations
!C           taken by the QR algorithm.
!C
!C  IFAIL  - INTEGER.
!C
!C           Before entry,  IFAIL  must contain one of the values -1 or 0
!C           or 1 to specify noisy soft failure or noisy hard failure  or
!C           silent soft failure. ( See Chapter P01 for further details.)
!C
!C           On successful  exit  IFAIL  will be  zero,  otherwise  IFAIL
!C           will be set to a  non-zero value  indicating either  that an
!C           input parameter  has been  incorrectly set,  or that the  QR
!C           algorithm  is not  converging.  See  the  next  section  for
!C           further details.
!C
!C  4. Diagnostic Information
!C     ======================
!C
!C  IFAIL = -1
!C
!C     One or more of the following conditions holds:
!C
!C        N     .lt. 0
!C        LDA   .lt. N
!C        NCOLB .lt. 0
!C        LDB   .lt. N  and  NCOLB .gt. 0
!C        LDQ   .lt. N  and  WANTQ  is  true
!C
!C  IFAIL .gt. 0
!C
!C     The  QR algorithm  has failed to converge in  50*N iterations.  In
!C     this  case  sv( 1 ), sv( 2 ), ..., sv( IFAIL )  may not  have been
!C     found correctly  and the remaining singular values  may not be the
!C     smallest.  The matrix  R will nevertheless have been factorized as
!C     R = Q*E*P', where E is a bidiagonal matrix with  sv( 1 ), sv( 2 ),
!C     ..., sv( n )  as the  diagonal elements and  work( 1 ), work( 2 ),
!C     ..., work( n - 1 )  as the super-diagonal elements.
!C
!C     This failure is not likely to occur.
!C
!C  If  on  entry,  IFAIL  was  either  -1 or 0  then  further diagnostic
!C  information  will  be  output  on  the  error message  channel. ( See
!C  routine  X04AAF. )
!C
!C  5. Further information
!C     ===================
!C
!C  Following the use of this routine the rank of R may be estimated by
!C  a call to the INTEGER function F06KLF.  The statement:
!C
!C     IRANK = F06KLF( N, SV, 1, TOL )
!C
!C  returns  the value  ( k - 1 ), in  IRANK,  where  k  is the  smallest
!C  integer  for  which   sv( k ) .lt. tol*sv( 1 ),   where  tol  is  the
!C  tolerance supplied in  TOL, so that  IRANK is an estimate of the rank
!C  of  S  and thus also of  R.  If  TOL is supplied as negative then the
!C  relative machine precision ( see routine X02AJF ) is used in place of
!C  TOL.
!C
!C
!C  Nag Fortran 77 Auxiliary linear algebra routine.
!C
!C  -- Written on 10-January-1988.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F02WUW')
!C     .. Scalar Arguments ..
      INTEGER           IFAIL, LDA, LDB, LDQ, N, NCOLB
      LOGICAL           WANTP, WANTQ
!C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), Q(LDQ,*), SV(*), WORK(*)
!C     .. Local Scalars ..
      INTEGER           IERR, NCOLP, NCOLQ
!C     .. Local Arrays ..
      DOUBLE PRECISION  DUMMY(1)
      CHARACTER*47      REC(2)
!C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
!C     .. External Subroutines ..
      EXTERNAL          F02WUX, F02WUY, F02WUZ, P01ABY
!C     .. Executable Statements ..
!C
!C     Check the input parameters.
!C
      IF (N.EQ.0) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
         RETURN
      END IF
      IERR = 0
      IF (N.LT.0) CALL P01ABY(N,'N',IFAIL,IERR,SRNAME)
      IF (LDA.LT.N) CALL P01ABY(LDA,'LDA',IFAIL,IERR,SRNAME)
      IF (NCOLB.LT.0) CALL P01ABY(NCOLB,'NCOLB',IFAIL,IERR,SRNAME)
      IF ((LDB.LT.N) .AND. (NCOLB.GT.0)) CALL P01ABY(LDB,'LDB',IFAIL,
     *    IERR,SRNAME)
      IF ((LDQ.LT.N) .AND. (WANTQ)) CALL P01ABY(LDQ,'LDQ',IFAIL,IERR,
     *    SRNAME)
      IF (IERR.GT.0) THEN
         WRITE (REC,FMT=99999) IERR
         IFAIL = P01ABF(IFAIL,-1,SRNAME,1,REC)
         RETURN
      END IF
!C
!C     Perform the factorization.
!C
!C     First  reduce  the  matrix  R  to  bidiagonal form.  The  diagonal
!C     elements  will  be  in   SV  and  the  super-diagonals  in   WORK.
!C
      CALL F02WUX(N,A,LDA,SV,WORK,NCOLB,B,LDB,WANTQ,Q,LDQ,IERR)
      IF (WANTP) THEN
         CALL F02WUZ(N,A,LDA,0,DUMMY,1,WORK(N),IERR)
         NCOLP = N
      ELSE
         NCOLP = 0
      END IF
      IF (WANTQ) THEN
         NCOLQ = N
      ELSE
         NCOLQ = 0
      END IF
!C
!C     Next find the SVD of the bidiagonal matrix.
!C
      IERR = 1
      CALL F02WUY(N,SV,WORK,NCOLB,B,LDB,NCOLQ,Q,LDQ,NCOLP,A,LDA,WORK(N),
     *            IERR)
      IF (IERR.NE.0) THEN
         WRITE (REC,FMT=99998) IERR
         IFAIL = P01ABF(IFAIL,IERR,SRNAME,2,REC)
      ELSE
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
      END IF
      RETURN
!C
!C
!C     End of F02WUW. ( SUTQR )
!C
99999 FORMAT ('    The input parameters contained ',I2,' error(s)')
99998 FORMAT ('    The QR algorithm has failed to converge.',/'    ',I6,
     *       ' singular values have NOT been found.')
      END
!C*******************************************************************************************************
      SUBROUTINE F06EFF( N, X, INCX, Y, INCY )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     .. Entry Points ..
      ENTRY      DCOPY ( N, X, INCX, Y, INCY )
!C     .. Scalar Arguments ..
      INTEGER            INCX, INCY, N
!C     .. Array Arguments ..
      DOUBLE PRECISION   X( * ), Y( * )
!C     ..
!C
!C  F06EFF performs the operation
!C
!C     y := x
!C
!C
!C  Nag Fortran 77 version of the Blas routine DCOPY.
!C  Nag Fortran 77 O( n ) basic linear algebra routine.
!C
!C  -- Written on 26-November-1982.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Local Scalars ..
      INTEGER            I, IX, IY
!C     ..
!C     .. Executable Statements ..
      IF( N.GT.0 )THEN
         IF( ( INCX.EQ.INCY ).AND.( INCY.GT.0 ) )THEN
            DO 10, IY = 1, 1 + ( N - 1 )*INCY, INCY
               Y( IY ) = X( IY )
   10       CONTINUE
         ELSE
            IF( INCX.GE.0 )THEN
               IX = 1
            ELSE
               IX = 1 - ( N - 1 )*INCX
            END IF
            IF( INCY.GT.0 )THEN
               DO 20, IY = 1, 1 + ( N - 1 )*INCY, INCY
                  Y( IY ) = X( IX )
                  IX      = IX      + INCX
   20          CONTINUE
            ELSE
               IY = 1 - ( N - 1 )*INCY
               DO 30, I = 1, N
                  Y( IY ) = X( IX )
                  IY      = IY      + INCY
                  IX      = IX      + INCX
   30          CONTINUE
            END IF
         END IF
      END IF
!C
      RETURN
!C
!C     End of F06EFF. ( DCOPY )
!C
      END
!C*********************************************************************************************************
      SUBROUTINE F06PAF( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
!C     MARK 13 RE-ISSUE. NAG COPYRIGHT 1988.
!C     .. Entry Points ..
      ENTRY      DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
!C     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
!C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
!C     ..
!C
!C  Purpose
!C  =======
!C
!C  DGEMV  performs one of the matrix-vector operations
!C
!C     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
!C
!C  where alpha and beta are scalars, x and y are vectors and A is an
!C  m by n matrix.
!C
!C  Parameters
!C  ==========
!C
!C  TRANS  - CHARACTER*1.
!C           On entry, TRANS specifies the operation to be performed as
!C           follows:
!C
!C              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
!C
!C              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
!C
!C              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
!C
!C           Unchanged on exit.
!C
!C  M      - INTEGER.
!C           On entry, M specifies the number of rows of the matrix A.
!C           M must be at least zero.
!C           Unchanged on exit.
!C
!C  N      - INTEGER.
!C           On entry, N specifies the number of columns of the matrix A.
!C           N must be at least zero.
!C           Unchanged on exit.
!C
!C  ALPHA  - DOUBLE PRECISION.
!C           On entry, ALPHA specifies the scalar alpha.
!C           Unchanged on exit.
!C
!C  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!C           Before entry, the leading m by n part of the array A must
!C           contain the matrix of coefficients.
!C           Unchanged on exit.
!C
!C  LDA    - INTEGER.
!C           On entry, LDA specifies the first dimension of A as declared
!C           in the calling (sub) program. LDA must be at least
!C           max( 1, m ).
!C           Unchanged on exit.
!C
!C  X      - DOUBLE PRECISION array of DIMENSION at least
!C           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
!C           and at least
!C           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
!C           Before entry, the incremented array X must contain the
!C           vector x.
!C           Unchanged on exit.
!C
!C  INCX   - INTEGER.
!C           On entry, INCX specifies the increment for the elements of
!C           X. INCX must not be zero.
!C           Unchanged on exit.
!C
!C  BETA   - DOUBLE PRECISION.
!C           On entry, BETA specifies the scalar beta. When BETA is
!C           supplied as zero then Y need not be set on input.
!C           Unchanged on exit.
!C
!C  Y      - DOUBLE PRECISION array of DIMENSION at least
!C           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
!C           and at least
!C           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
!C           Before entry with BETA non-zero, the incremented array Y
!C           must contain the vector y. On exit, Y is overwritten by the
!C           updated vector y.
!C
!C  INCY   - INTEGER.
!C           On entry, INCY specifies the increment for the elements of
!C           Y. INCY must not be zero.
!C           Unchanged on exit.
!C
!C
!C  Level 2 Blas routine.
!C
!C  -- Written on 22-October-1986.
!C     Jack Dongarra, Argonne National Lab.
!C     Jeremy Du Croz, Nag Central Office.
!C     Sven Hammarling, Nag Central Office.
!C     Richard Hanson, Sandia National Labs.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!C     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
!C     .. External Subroutines ..
      EXTERNAL           F06AAZ
!C     .. Intrinsic Functions ..
      INTRINSIC          MAX
!C     ..
!C     .. Executable Statements ..
!C
!C     Test the input parameters.
!C
      INFO = 0
      IF     ( .NOT.(TRANS.EQ.'N' .OR. TRANS.EQ.'n').AND.
     $         .NOT.(TRANS.EQ.'T' .OR. TRANS.EQ.'t').AND.
     $         .NOT.(TRANS.EQ.'C' .OR. TRANS.EQ.'c')      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL F06AAZ( 'F06PAF/DGEMV ', INFO )
         RETURN
      END IF
!C
!C     Quick return if possible.
!C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
!C
!C     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
!C     up the start points in  X  and  Y.
!C
      IF( (TRANS.EQ.'N' .OR. TRANS.EQ.'n') )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
!C
!C     Start the operations. In this version the elements of A are
!C     accessed sequentially with one pass through A.
!C
!C     First form  y := beta*y.
!C
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( (TRANS.EQ.'N' .OR. TRANS.EQ.'n') )THEN
!C
!C        Form  y := alpha*A*x + y.
!C
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
!C
!C        Form  y := alpha*A'*x + y.
!C
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
!C
      RETURN
!C
!C     End of F06PAF (DGEMV ).
!C
      END
!C********************************************************************************************************
      SUBROUTINE F06QFF( MATRIX, M, N, A, LDA, B, LDB )
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C     .. Scalar Arguments ..
      CHARACTER*1        MATRIX
      INTEGER            M, N, LDA, LDB
!C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
!C     ..
!C
!C  F06QFF  copies  the  m by n  matrix  A  into  the  m by n  matrix  B.
!C
!C  If   MATRIX = 'G' or 'g'   then  A  and  B  are  regarded as  general
!C                             matrices,
!C  if   MATRIX = 'U' or 'u'   then  A  and  B  are  regarded  as   upper
!C                             triangular,  and only  elements  for which
!C                             i.le.j  are referenced,
!C  if   MATRIX = 'L' or 'l'   then  A  and  B  are  regarded  as   lower
!C                             triangular,  and only  elements  for which
!C                             i.ge.j  are referenced.
!C
!C
!C  Nag Fortran 77 O( n**2 ) basic linear algebra routine.
!C
!C  -- Written on 21-November-1986.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Local Scalars ..
      INTEGER            I, J
!C     .. Intrinsic Functions ..
      INTRINSIC          MIN
!C     ..
!C     .. Executable Statements ..
      IF( ( MATRIX.EQ.'G' ).OR.( MATRIX.EQ.'g' ) )THEN
         DO 20 J = 1, N
            DO 10 I = 1, M
               B( I, J ) = A( I, J )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( MATRIX.EQ.'U' ).OR.( MATRIX.EQ.'u' ) )THEN
         DO 40 J = 1, N
            DO 30 I = 1, MIN( M, J )
               B( I, J ) = A( I, J )
   30       CONTINUE
   40    CONTINUE
      ELSE IF( ( MATRIX.EQ.'L' ).OR.( MATRIX.EQ.'l' ) )THEN
         DO 60 J = 1, MIN( M, N )
            DO 50 I = J, M
               B( I, J ) = A( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF
!C
      RETURN
!C
!C     End of F06QFF. ( SMCOPY )
!C
      END
!C*********************************************************************************************************
      SUBROUTINE F02WEX(NX,NY,ALPHA,X,INCX,Y,INCY,TOL,ZETA)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  F02WEX  generates  details  of  a  Householder reflection  such  that
!C
!C     P*(   x   ) = (   0  ),   P'*P = I.
!C       ( alpha )   ( beta ),
!C       (   y   )   (   0  )
!C
!C  P is given in the form
!C
!C     P = I - (   w  )*( w'  zeta  z' ),
!C             ( zeta )
!C             (   z  )
!C
!C  where  w is an nx element vector, z is an ny element vector, and zeta
!C  is a scalar that satisfies
!C
!C     1.0 .le. zeta .le. sqrt( 2.0 ).
!C
!C  zeta is returned in ZETA unless the vector v given by
!C
!C     v = ( x )
!C         ( y )
!C
!C  is such that
!C
!C     max( abs( v( i ) ) ) .le. max( tol, eps*abs( alpha ) ),
!C
!C  where  eps  is the  relative machine precision  and  tol  is the user
!C  supplied tolerance  TOL, in which case  ZETA  is returned as  0.0 and
!C  P  can be taken to be the unit matrix.
!C
!C  beta  is  overwritten on  alpha,  w  is overwritten on  x  and  z  is
!C  overwritten on  y.
!C
!C  The  routine  may be  called  with  either  or  both  nx = 0, ny = 0.
!C
!C
!C  Nag Fortran 77 O( n ) basic linear algebra routine.
!C
!C  -- Written on 17-November-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION ONE, ZERO
      PARAMETER        (ONE=1.0D+0,ZERO=0.0D+0)
!C     .. Scalar Arguments ..
      DOUBLE PRECISION ALPHA, TOL, ZETA
      INTEGER          INCX, INCY, NX, NY
!C     .. Array Arguments ..
      DOUBLE PRECISION X(*), Y(*)
!C     .. Local Scalars ..
      DOUBLE PRECISION BETA, EPS, SCALE, SSQ
      LOGICAL          FIRST
!C     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
!C     .. External Subroutines ..
      EXTERNAL         F06FJF, F06FRF, DSCAL
!C     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX, SQRT
!C     .. Save statement ..
      SAVE             EPS, FIRST
!C     .. Data statements ..
      DATA             FIRST/.TRUE./
!C     .. Executable Statements ..
      IF (NX.LT.1) THEN
         CALL F06FRF(NY,ALPHA,Y,INCY,TOL,ZETA)
      ELSE IF (NY.LT.1) THEN
         CALL F06FRF(NX,ALPHA,X,INCX,TOL,ZETA)
      ELSE
!C
         IF (FIRST) THEN
            FIRST = .FALSE.
            EPS = X02AJF()
         END IF
!C
         SSQ = ONE
         SCALE = ZERO
         CALL F06FJF(NX,X,INCX,SCALE,SSQ)
         CALL F06FJF(NY,Y,INCY,SCALE,SSQ)
!C
!C        Treat  cases  where   SCALE = zero,   SCALE is negligible   and
!C        ALPHA = zero  specially.  Note that
!C
!C           SCALE = max( abs( v( i ) ) ).
!C
         IF ((SCALE.EQ.ZERO) .OR. (SCALE.LE.MAX(TOL,EPS*ABS(ALPHA))))
     *       THEN
            ZETA = ZERO
         ELSE IF (ALPHA.EQ.ZERO) THEN
            ZETA = ONE
            ALPHA = SCALE*SQRT(SSQ)
            CALL DSCAL(NX,-1/ALPHA,X,INCX)
            CALL DSCAL(NY,-1/ALPHA,Y,INCY)
            ALPHA = BETA
         ELSE
            IF (SCALE.LT.ABS(ALPHA)) THEN
               BETA = ABS(ALPHA)*SQRT(1+SSQ*(SCALE/ALPHA)**2)
            ELSE
               BETA = SCALE*SQRT(SSQ+(ALPHA/SCALE)**2)
            END IF
            ZETA = SQRT((BETA+ABS(ALPHA))/BETA)
            IF (ALPHA.GT.ZERO) BETA = -BETA
            CALL DSCAL(NX,-1/(ZETA*BETA),X,INCX)
            CALL DSCAL(NY,-1/(ZETA*BETA),Y,INCY)
            ALPHA = BETA
         END IF
      END IF
!C
      RETURN
!C
!C     End of F02WEX. ( SGRFG2 )
!C
      END
!C********************************************************************************************************************
      SUBROUTINE F02WUX(N,A,LDA,D,E,NCOLY,Y,LDY,WANTQ,Q,LDQ,IFAIL)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  1. Purpose
!C     =======
!C
!C  F02WUX  reduces the  n by n upper triangular matrix  R  to bidiagonal
!C  form by means of orthogonal transformations.
!C
!C  2. Description
!C     ===========
!C
!C  The n by n upper triangular matrix R is factorized as
!C
!C     R = Q*B*P',
!C
!C  where  Q and P  are  n by n orthogonal matrices and  B  is an  n by n
!C  bidiagonal matrix.
!C
!C  Optionally the matrices Q and/or the matrix Z given by
!C
!C     Z = Q'*Y,
!C
!C  where  Y  is an  n by ncoly matrix, can also be returned. Information
!C  on the  matrix  P  is returned  in the  upper triangular part  of the
!C  array A.
!C
!C  R  is reduced to the bidiagonal matrix  B by applying plane rotations
!C  from the  right  to introduce the required  zeros and applying  plane
!C  rotations from the left to maintain the  zeros in the lower triangle.
!C
!C  At the  kth step,  the  zeros are introduced into the  kth row of  R,
!C  k = 1, 2, ..., n - 2,  by a backward sequence of rotations in  planes
!C  ( j - 1, j ), j = n, n - 1, ..., k + 2, the jth rotation,  P( k, j ),
!C  being chosen  to  introduce a  zero  into the  ( k, j ) element. This
!C  rotation  introduces  an  unwanted  element  into  the   ( j, j - 1 )
!C  position, which is eliminated by a rotation, Q( k, j ), from the left
!C  in the ( j - 1, j ) plane. Thus at the kth step we have
!C
!C     R( k ) = Q( k )*R( k - 1 )*P( k )',
!C
!C  where
!C
!C     Q( k ) = Q( k, k + 2 )*...*Q( k, n )   and
!C     P( k ) = P( k, k + 2 )*...*P( k, n ),
!C
!C  with
!C
!C     R( 0 ) = R   and   R( n - 2 ) = B.
!C
!C  The two by two rotation parts of  P( k, j )  and  Q( k, j )  have the
!C  form
!C
!C     (  c  s ).
!C     ( -s  c )
!C
!C  The value  t,  where  t  is the  tangent  of the  angle  that defines
!C  P( k, j ),  is returned in the element  a( k, j ).  The corresponding
!C  c and s  may be recovered from  t  by a call to routine  F06BCF.  See
!C  section 5  for information on computing  P' and/or P'*X  for a  given
!C  matrix  X.
!C
!C  The matrices Q and P are given by
!C
!C     Q' = Q( n - 2 )*...*Q( 2 )*Q( 1 )   and
!C     P' = P( n - 2 )*...*P( 2 )*P( 1 ).
!C
!C  3. Parameters
!C     ==========
!C
!C  N      - INTEGER.
!C
!C           On entry, N specifies the order of the matrix  R.  N must be
!C           at least  zero.  When  N = 0  then  an  immediate return  is
!C           effected.
!C
!C           Unchanged on exit.
!C
!C  A      - REAL             array of DIMENSION ( LDA, n )
!C
!C           Before entry, the leading  N by N  upper triangular  part of
!C           the array  A  must contain the  upper triangular  matrix  R.
!C
!C           On exit, the  N by N  upper triangular  part of the array  A
!C           above the  first super-diagonal  will contain information on
!C           the matrix  P, as described in section 2 above. The diagonal
!C           elements of  A  return the  diagonal elements of  B  and the
!C           elements  of  the  first  super-diagonal  of  A  return  the
!C           super-diagonal elements of  B. The strictly lower triangular
!C           part of A is not referenced.
!C
!C  LDA    - INTEGER.
!C
!C           On  entry,  LDA  must specify  the leading dimension  of the
!C           array  A as declared in the calling (sub) program. LDA  must
!C           be at least N.
!C
!C           Unchanged on exit.
!C
!C  D      - REAL             array of DIMENSION at least ( n ).
!C
!C           On  exit,   D  contains  the  n  diagonal  elements  of  the
!C           bidiagonal matrix B, with  d( i ) = b( i, i ).
!C
!C  E      - REAL             array    of      DIMENSION      at    least
!C           ( max( 1, n - 1 ) ).
!C
!C           On exit, E contains the ( n - 1 ) super-diagonal elements of
!C           the  bidiagonal  matrix  B,  with    e( i ) = b( i, i + 1 ),
!C           i = 1, 2, ..., n - 1.
!C
!C  NCOLY  - INTEGER.
!C
!C           On entry,  NCOLY  must specify the  number of columns of the
!C           matrix  Y  and must be at least  zero.  When  NCOLY = 0  the
!C           array  Y  is not referenced.
!C
!C           Unchanged on exit.
!C
!C  Y      - REAL             array of DIMENSION ( LDY, ncoly ).
!C
!C           Before entry with  NCOLY .gt. 0, the leading n by ncoly part
!C           of the array  Y  must contain the  matrix to be  transformed
!C           and  on  exit  Y  is overwritten  by the  n by ncoly  matrix
!C           Q'*Y.
!C
!C           When  NCOLY = 0  the array  Y  is not referenced.
!C
!C  LDY    - INTEGER.
!C
!C           On  entry,  LDY  must specify  the leading dimension  of the
!C           array  Y  as declared  in the  calling  (sub) program.  When
!C           NCOLY .gt. 0  then LDY must be at least n.
!C
!C           Unchanged on exit.
!C
!C  WANTQ  - LOGICAL.
!C
!C           On entry, WANTQ must be .TRUE. if the orthogonal matrix Q is
!C           required and must be .FALSE. otherwise.
!C
!C           Unchanged on exit.
!C
!C  Q      - REAL             array of DIMENSION ( LDQ, n ).
!C
!C           On exit with WANTQ as .TRUE., the leading n by n part of the
!C           array Q will contain the orthogonal matrix Q.
!C
!C           When  WANTQ  is .FALSE.  the  array  Q  is  not  referenced.
!C
!C  LDQ    - INTEGER.
!C
!C           On  entry,  LDQ  must specify  the leading dimension  of the
!C           array Q as declared in the calling (sub) program. When WANTQ
!C           is .TRUE. then LDQ must be at least n.
!C
!C           Unchanged on exit.
!C
!C  IFAIL  - INTEGER.
!C
!C           Before entry,  IFAIL  must contain one of the values -1 or 0
!C           or 1 to specify noisy soft failure or noisy hard failure  or
!C           silent soft failure. ( See Chapter P01 for further details.)
!C
!C           On  successful exit  IFAIL  will be  zero,  otherwise  IFAIL
!C           will  be set to  -1  indicating that an  input parameter has
!C           been  incorrectly  set. See  the  next  section  for further
!C           details.
!C
!C  4. Diagnostic Information
!C     ======================
!C
!C  IFAIL = -1
!C
!C     One or more of the following conditions holds:
!C
!C        N     .lt. 0
!C        LDA   .lt. N
!C        NCOLY .lt. 0
!C        NCOLY .gt. 0     and  LDY .lt. N
!C        WANTQ  is  true  and  LDQ .lt. N
!C
!C  If  on  entry,  IFAIL  was either  -1 or 0  then  further  diagnostic
!C  information  will  be  output  on  the  error message  channel. ( See
!C  routine  X04AAF. )
!C
!C  5. Further information
!C     ===================
!C
!C  Following the use of this routine the matrices  P'*X and/or P' may be
!C  obtained by calls to the auxiliary linear algebra routine F02WUZ. The
!C  matrix  W = P'*X,  where  X is an  nrowx by n matrix, may be found by
!C  the call
!C
!C     IFAIL = 0
!C     CALL F02WUZ( N, A, LDA, NROWX, X, LDX, WORK, IFAIL )
!C
!C  for which  W  will be  overwritten on  X.  WORK  must be an  array of
!C  length  at  least  2*( n - 1 )  and  is used  as  internal workspace.
!C
!C  The matrix P' may be found by the call
!C
!C     IFAIL = 0
!C     CALL F02WUZ( N, A, LDA, 0, DUMMY, 1, WORK, IFAIL )
!C
!C  where  A must be as returned from  F02WUX.  P' will be overwritten on
!C  A and  DUMMY  is an array of  length at least  1, which will  not  be
!C  referenced by  this call.  WORK  is as  for  the  previous call.  See
!C  routine  F02WUZ  for further details.
!C
!C
!C  Nag auxiliary linear algebra routine.
!C
!C  -- Written on 22-July-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0)
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F02WUX')
!C     .. Scalar Arguments ..
      INTEGER           IFAIL, LDA, LDQ, LDY, N, NCOLY
      LOGICAL           WANTQ
!C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), D(*), E(*), Q(LDQ,*), Y(LDY,*)
!C     .. Local Scalars ..
      INTEGER           I, IERR, J, K
!C     .. Local Arrays ..
      CHARACTER*46      REC(1)
!C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
!C     .. External Subroutines ..
      EXTERNAL          F06FQF, F06QTF, F06QXF, P01ABY, DCOPY
!C     .. Executable Statements ..
!C
!C     Check the input parameters.
!C
      IF (N.EQ.0) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
         RETURN
      END IF
      IERR = 0
      IF (N.LT.0) CALL P01ABY(N,'N',IFAIL,IERR,SRNAME)
      IF (LDA.LT.N) CALL P01ABY(LDA,'LDA',IFAIL,IERR,SRNAME)
      IF (NCOLY.LT.0) CALL P01ABY(NCOLY,'NCOLY',IFAIL,IERR,SRNAME)
      IF ((NCOLY.GT.0) .AND. (LDY.LT.N)) CALL P01ABY(LDY,'LDY',IFAIL,
     *    IERR,SRNAME)
      IF ((WANTQ) .AND. (LDQ.LT.N)) CALL P01ABY(LDQ,'LDQ',IFAIL,IERR,
     *    SRNAME)
      IF (IERR.GT.0) THEN
         WRITE (REC,FMT=99999) IERR
         IFAIL = P01ABF(IFAIL,-1,SRNAME,1,REC)
         RETURN
      END IF
!C
!C     Perform the factorization. First reduce R to C.
!C
      DO 20 K = 1, N - 2
!C
!C        Set up  the  rotations  that put the  zeros  into the  kth row.
!C        The cosines and sines that define  P( k ) are returned in e and
!C        d.
!C
         CALL F06FQF('Variable pivot','Backwards',N-K-1,A(K,K+1),A(K,
     *               K+2),LDA,E(K+1),D(K+1))
!C
!C        Form  R( k ) = Q( k )*R( k - 1 )*P( k )'. The cosines and sines
!C        that define Q( k ) are overwritten on e and d.
!C
         CALL F06QTF('Right side',N-K,1,N-K,E(K+1),D(K+1),A(K+1,K+1),
     *               LDA)
!C
!C        Form Q( k )*Y.
!C
         IF (NCOLY.GT.0) CALL F06QXF('Left side','Variable pivot',
     *                               'Backwards',N,NCOLY,K+1,N,E,D,Y,
     *                               LDY)
!C
!C        If Q is required store the cosines and sines that define Q( k )
!C        in the kth row and column of Q.
!C
         IF (WANTQ) THEN
            CALL DCOPY(N-K-1,E(K+1),1,Q(K,K+2),LDQ)
            CALL DCOPY(N-K-1,D(K+1),1,Q(K+2,K),1)
         END IF
   20 CONTINUE
      IF (WANTQ) THEN
!C
!C        Form the matrix Q as
!C
!C           Q = Q( 1 )'*...*Q( n - 2 )'*I.
!C
         IF (N.GT.1) THEN
            Q(N,N) = ONE
            Q(N-1,N) = ZERO
            Q(N,N-1) = ZERO
            IF (N.GT.2) THEN
               DO 80 K = N - 2, 1, -1
                  Q(K+1,K+1) = ONE
                  Q(K,K+1) = ZERO
                  DO 40 J = K + 2, N
                     D(J-1) = Q(K,J)
                     Q(K,J) = ZERO
   40             CONTINUE
                  Q(K+1,K) = ZERO
                  DO 60 I = K + 2, N
                     E(I-1) = -Q(I,K)
                     Q(I,K) = ZERO
   60             CONTINUE
                  CALL F06QXF('Left side','Variable pivot','Forward',
     *                        N-K,N-K,1,N-K,D(K+1),E(K+1),Q(K+1,K+1),
     *                        LDQ)
   80          CONTINUE
            END IF
         END IF
         Q(1,1) = ONE
      END IF
!C
!C     Put the elements of B into the arrays D and E.
!C
      DO 100 K = 1, N - 1
         D(K) = A(K,K)
         E(K) = A(K,K+1)
  100 CONTINUE
      D(N) = A(N,N)
!C
      IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
      RETURN
!C
!C
!C     End of F02WUX. ( SUTBI )
!C
99999 FORMAT ('    The input parameters contained ',I2,' error(s)')
      END
!C*********************************************************************************************************
      SUBROUTINE F02WUY(N,D,E,NCOLB,B,LDB,NROWY,Y,LDY,NCOLZ,Z,LDZ,WORK,
     *                  IFAIL)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C     MARK 13B REVISED. IER-663 (AUG 1988).
!C
!C  1. Purpose
!C     =======
!C
!C  F02WUY  reduces  an  n by n  real bidiagonal matrix  to diagonal form
!C  by  means  of  orthogonal transformations.  The  transformations  may
!C  optionally be applied to given real matrices.
!C
!C  2. Description
!C     ===========
!C
!C  The n by n bidiagonal matrix A is factorized as
!C
!C     A = Q*S*P',
!C
!C  where  Q and P  are  n by n orthogonal matrices and  S  is an  n by n
!C  diagonal matrix  with  non-negative diagonal elements.  This  is  the
!C  singular value decomposition  of the matrix A.
!C
!C  The diagonal elements of  S are the singular values of  A and will be
!C  arranged in descending order. The columns of Q and P are the left and
!C  right singular vectors of A respectively.
!C
!C  Optionally the matrices  C and/or Y and/or Z  given by
!C
!C     C = Q'*B,   W = Y*Q,   X = P'*Z,
!C
!C  where  B  is an  n by ncolb  matrix, Y  is an  nrowy by n  matrix and
!C  Z  is an  n by ncolz  matrix, can also be returned.
!C
!C  The factorization is obtained by the  Golub-Reinsch version of the QR
!C  algorithm.
!C
!C  3. Parameters
!C     ==========
!C
!C  N      - INTEGER.
!C
!C           On entry, N specifies the order of the matrix  A.  N must be
!C           at least  zero.  When  N = 0  then  an  immediate return  is
!C           effected.
!C
!C           Unchanged on exit.
!C
!C  D      - REAL array of DIMENSION at least ( n ).
!C
!C           On entry, D must contain the diagonal elements of the n by n
!C           bidiagonal matrix A such that
!C
!C              d( i ) = a( i, i ), i = 1, 2, ..., n.
!C
!C           On exit, D contains the  n singular values of  A arranged in
!C           descending order of magnitude so that
!C
!C              d( 1 ) .ge. d( 2 ) .ge. ... .ge. d( n ) .ge. 0.
!C
!C  E      - REAL array of DIMENSION at least ( max( 1, n - 1 ) ).
!C
!C           On  entry,  E  must  contain  the  ( n - 1 )  super-diagonal
!C           elements of the bidiagonal matrix A, with
!C
!C              e( i ) = a( i, i + 1 ), i = 1, 2, ..., n - 1.
!C
!C           E  is  used  as  internal  workspace  by  F02WUY  and so the
!C           elements are changed on exit.
!C
!C  NCOLB  - INTEGER.
!C
!C           On entry,  NCOLB  must specify the  number of columns of the
!C           matrix  B  and must be at least  zero.  When  NCOLB = 0  the
!C           array B is not referenced.
!C
!C           Unchanged on exit.
!C
!C  B      - REAL             array of DIMENSION ( LDB, ncolb ).
!C
!C           Before entry with  NCOLB .gt. 0, the leading n by ncolb part
!C           of the array B must contain the matrix to be transformed and
!C           on exit  B  is overwritten by the  n by ncolb  matrix  Q'*B.
!C
!C           When  NCOLB = 0  the array  B  is not referenced.
!C
!C  LDB    - INTEGER.
!C
!C           On  entry,  LDB  must specify  the leading dimension  of the
!C           array  B  as declared  in the  calling  (sub) program.  When
!C           NCOLB .gt. 0  then LDB must be at least N.
!C
!C           Unchanged on exit.
!C
!C  NROWY  - INTEGER.
!C
!C           On entry,  NROWY  must specify  the  number of  rows  of the
!C           matrix  Y  and must be at least  zero.  When  NROWY = 0  the
!C           array Y is not referenced.
!C
!C           Unchanged on exit.
!C
!C  Y      - REAL             array of DIMENSION ( LDY, n ).
!C
!C           Before entry with  NROWY .gt. 0, the leading nrowy by n part
!C           of the array Y must contain the matrix to be transformed and
!C           on  exit  Y  is overwritten  by the  nrow by n  matrix  Y*Q.
!C
!C           When  NROWY = 0  the array  Y  is not referenced.
!C
!C  LDY    - INTEGER.
!C
!C           On  entry,  LDY  must specify  the leading dimension  of the
!C           array  Y  as declared  in the  calling  (sub) program.  When
!C           NROWY .gt. 0  then LDY must be at least NROWY.
!C
!C           Unchanged on exit.
!C
!C  NCOLZ  - INTEGER.
!C
!C           On entry,  NCOLZ  must specify the  number of columns of the
!C           matrix  Z  and must be at least  zero.  When  NCOLZ = 0  the
!C           array Z is not referenced.
!C
!C           Unchanged on exit.
!C
!C  Z      - REAL             array of DIMENSION ( LDZ, ncolz ).
!C
!C           Before entry with  NCOLZ .gt. 0, the leading n by ncolz part
!C           of the array Z must contain the matrix to be transformed and
!C           on exit  Z  is overwritten by the  n by ncolz  matrix  P'*Z.
!C
!C           When  NCOLZ = 0  the array  Z  is not referenced.
!C
!C  LDZ    - INTEGER.
!C
!C           On  entry,  LDZ  must specify  the leading dimension  of the
!C           array  Z  as declared  in the  calling  (sub) program.  When
!C           NCOLZ .gt. 0  then LDZ must be at least N.
!C
!C           Unchanged on exit.
!C
!C  WORK   - REAL array of DIMENSION at least ( max( 1, lwork ) ),  where
!C           lwork must be at least zero when  ncolb = nrowy = ncolz = 0,
!C           lwork must be at least   2*( n - 1 )   when  either  ncolb =
!C           nrowy = 0  and  nrowz is positive, or  nrowz = 0  and one or
!C           both of  ncolb and  nrowy  are positive, and  lwork  must be
!C           at least  4*( n - 1 )  when one  or both of  ncolb and nrowy
!C           are positive and  ncolz  is positive.
!C
!C           The array  WORK  is used as  internal workspace  by  F02WUY.
!C           On exit,  WORK( 1 )  contains the total number of iterations
!C           taken by the QR algorithm.
!C
!C  IFAIL  - INTEGER.
!C
!C           Before entry,  IFAIL  must contain one of the values -1 or 0
!C           or 1  to specify noisy soft failure or noisy hard failure or
!C           silent soft failure. ( See Chapter P01 for further details.)
!C
!C           On  successful exit  IFAIL  will be  zero,  otherwise  IFAIL
!C           will be set to a  non-zero value  indicating either  that an
!C           input parameter  has been  incorrectly set,  or that the  QR
!C           algorithm  is not  converging.  See  the  next  section  for
!C           further details.
!C
!C  4. Diagnostic Information
!C     ======================
!C
!C  IFAIL = -1
!C
!C     One or more of the following conditions holds:
!C
!C        N     .lt. 0
!C        NCOLB .lt. 0
!C        LDB   .lt. N      and  NCOLB .gt. 0
!C        NROWY .lt. 0
!C        LDY   .lt. NROWY  and  NROWY .gt. 0
!C        NCOLZ .lt. 0
!C        LDZ   .lt. N      and  NCOLZ .gt. 0
!C
!C  IFAIL .gt. 0
!C
!C     The  QR algorithm  has failed to converge in  50*N iterations.  In
!C     this   case  d( 1 ), d( 2 ), ..., d( IFAIL )  may  not  have  been
!C     found correctly  and the remaining singular values  may not be the
!C     smallest.  The matrix  A will nevertheless have been factorized as
!C     A = Q*E*P',  where  E is a bidiagonal matrix with  d( 1 ), d( 2 ),
!C     ..., d( n )  as  the  diagonal elements  and  e( 1 ), e( 2 ), ...,
!C     e( n - 1 )  as the super-diagonal elements.
!C
!C     This failure is not likely to occur.
!C
!C  If  on  entry,  IFAIL  was either  -1 or 0  then  further  diagnostic
!C  information  will  be  output  on  the  error message  channel. ( See
!C  routine  X04AAF. )
!C
!C  5. Further Information
!C     ===================
!C
!C  This routine  can be  used in conjunction  with routines  F02WUX  and
!C  F02WUZ   to  find  the  singular  value  decomposition  of  an  upper
!C  triangular matrix.
!C
!C
!C  Nag Fortran 77 Auxiliary linear algebra routine.
!C
!C  -- Written on 4-September-1987.
!C     Sven Hammarling and Jeremy Du Croz, Nag Central Office.
!C
!C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F02WUY')
      DOUBLE PRECISION  QTR, ZERO
      PARAMETER         (QTR=0.25D+0,ZERO=0.0D+0)
!C     .. Scalar Arguments ..
      INTEGER           IFAIL, LDB, LDY, LDZ, N, NCOLB, NCOLZ, NROWY
!C     .. Array Arguments ..
      DOUBLE PRECISION  B(LDB,*), D(*), E(*), WORK(*), Y(LDY,*),
     *                  Z(LDZ,*)
!C     .. Local Scalars ..
      DOUBLE PRECISION  AMAX, CS, DMAX, EKM2, SN, TEMP
      INTEGER           I, IERR, ITER, IW1, IW2, IW3, J, K, L, MAXIT, P
      LOGICAL           FORCE, WANTB, WANTY, WANTZ
!C     .. Local Arrays ..
      CHARACTER*47      REC(2)
!C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
!C     .. External Subroutines ..
      EXTERNAL          F02XUS, F02XUT, F02XUU, F02XUV, F02XUW, F06FGF,
     *                  F06QKF, F06QXF, P01ABY, DSCAL
!C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX
!C     .. Executable Statements ..
!C
!C     Check the input parameters.
!C
      WORK(1) = ZERO
      IF (N.EQ.0) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
         RETURN
      END IF
      WANTB = NCOLB .GT. 0
      WANTY = NROWY .GT. 0
      WANTZ = NCOLZ .GT. 0
      IERR = 0
      IF (N.LT.0) CALL P01ABY(N,'N',IFAIL,IERR,SRNAME)
      IF (NCOLB.LT.0) CALL P01ABY(NCOLB,'NCOLB',IFAIL,IERR,SRNAME)
      IF ((WANTB) .AND. (LDB.LT.N)) CALL P01ABY(LDB,'LDB',IFAIL,IERR,
     *    SRNAME)
      IF (NROWY.LT.0) CALL P01ABY(NROWY,'NROWY',IFAIL,IERR,SRNAME)
      IF ((WANTY) .AND. (LDY.LT.N)) CALL P01ABY(LDY,'LDY',IFAIL,IERR,
     *    SRNAME)
      IF (NCOLZ.LT.0) CALL P01ABY(NCOLZ,'NCOLZ',IFAIL,IERR,SRNAME)
      IF ((WANTZ) .AND. (LDZ.LT.N)) CALL P01ABY(LDZ,'LDZ',IFAIL,IERR,
     *    SRNAME)
      IF (IERR.GT.0) THEN
         WRITE (REC,FMT=99999) IERR
         IFAIL = P01ABF(IFAIL,-1,SRNAME,1,REC)
         RETURN
      END IF
!C
!C     Find the size  of the  element  of  largest absolute value of  A.
!C
      AMAX = ABS(D(1))
      DO 20 I = 2, N
         AMAX = MAX(AMAX,ABS(D(I)),ABS(E(I-1)))
   20 CONTINUE
!C
!C     Scale the matrix A by AMAX.
!C
      IF (AMAX.GT.ZERO) THEN
         CALL DSCAL(N,1/AMAX,D,1)
         CALL DSCAL(N-1,1/AMAX,E,1)
      END IF
!C
!C     Split up the workspace.
!C
      IW1 = 1
      IW2 = 1
      IW3 = 1
      IF ((WANTB) .OR. (WANTY)) THEN
         IW1 = N
         IF (WANTZ) THEN
            IW2 = (N-1) + IW1
            IW3 = (N-1) + IW2
         END IF
      ELSE IF (WANTZ) THEN
         IW3 = N
      END IF
!C
!C     Start the  QR algorithm.  A singular value is found for each value
!C     of k.
!C
      MAXIT = 50*N
      ITER = 1
      K = N
!C +    WHILE( ( K.GT.1 ).AND.( ITER.LE.MAXIT ) )LOOP
   40 IF ((K.GT.1) .AND. (ITER.LE.MAXIT)) THEN
!C
!C        Test  to see if there is a  split,  or if we can force a split.
!C        On exit from  F02XUT,  force will be  true  if we can  force  a
!C        split and  p .gt. 0  means that  e( p )  is negligible, or will
!C        be negligible after forcing the split.
!C
         CALL F02XUT(ZERO,K,D,E,FORCE,P)
         L = P + 1
         IF (FORCE) THEN
            IF (P.EQ.K) THEN
!C
!C              Force a zero singular value in d( k ).
!C
               CALL F02XUS(K,D,E,WANTZ,WORK(IW2),WORK(IW3))
               IF (WANTZ) CALL F06QXF('Left','Bottom','Backward',N,
     *                                NCOLZ,1,K,WORK(IW2),WORK(IW3),Z,
     *                                LDZ)
            ELSE
!C
!C              Force a split.
!C
               CALL F02XUW(P,K,D,E,(WANTB) .OR. (WANTY),WORK,WORK(IW1))
               IF (WANTB) CALL F06QXF('Left','Top','Forward',N,NCOLB,P,
     *                                K,WORK,WORK(IW1),B,LDB)
               IF (WANTY) CALL F06QXF('Right','Top','Forward',NROWY,N,P,
     *                                K,WORK,WORK(IW1),Y,LDY)
            END IF
         END IF
         IF (L.GE.K) THEN
!C
!C           We have converged to a singular value.
!C
            K = K - 1
         ELSE
!C
!C           Perform a QR step. First determine the shift.
!C
            IF (K.GT.(L+1)) THEN
               EKM2 = E(K-2)
            ELSE
               EKM2 = ZERO
            END IF
            CALL F02XUU(-1,D(L),E(L),D(K-1),D(K),EKM2,E(K-1),CS,SN)
            CALL F02XUV('Non-zero',L,K,D,E,CS,SN,(WANTB) .OR. (WANTY),
     *                  WORK,WORK(IW1),WANTZ,WORK(IW2),WORK(IW3))
            IF (WANTB) CALL F06QXF('Left','Variable','Forward',N,NCOLB,
     *                             L,K,WORK,WORK(IW1),B,LDB)
            IF (WANTY) CALL F06QXF('Right','Variable','Forward',NROWY,N,
     *                             L,K,WORK,WORK(IW1),Y,LDY)
            IF (WANTZ) CALL F06QXF('Left','Variable','Forward',N,NCOLZ,
     *                             L,K,WORK(IW2),WORK(IW3),Z,LDZ)
            ITER = ITER + 1
         END IF
         GO TO 40
      END IF
!C +    END WHILE
!C
!C     Unscale the matrix.
!C
      IF (AMAX.GT.ZERO) THEN
         CALL DSCAL(N,AMAX,D,1)
         CALL DSCAL(N-1,AMAX,E,1)
      END IF
!C
!C     Make the singular values non-negative.
!C
      DO 60 I = K, N
         IF (D(I).LT.ZERO) THEN
            D(I) = -D(I)
            IF ((WANTB) .AND. (WANTZ)) CALL F06FGF(NCOLB,B(I,1),LDB)
            IF ((WANTY) .AND. (WANTZ)) CALL F06FGF(NROWY,Y(1,I),1)
         END IF
   60 CONTINUE
!C
!C     Sort the singular values into descending order.
!C
      DO 80 J = 1, K - 1
         WORK(J) = J + QTR
   80 CONTINUE
      DO 120 J = K, N
         DMAX = D(J)
         L = J
         DO 100 I = J + 1, N
            IF (D(I).GT.DMAX) THEN
               DMAX = D(I)
               L = I
            END IF
  100    CONTINUE
         WORK(J) = L + QTR
         IF (L.GT.J) THEN
            TEMP = D(J)
            D(J) = D(L)
            D(L) = TEMP
         END IF
  120 CONTINUE
      IF (WANTB) CALL F06QKF('Left','Transpose',N,WORK,NCOLB,B,LDB)
      IF (WANTY) CALL F06QKF('Right','No Transpose',N,WORK,NROWY,Y,LDY)
      IF (WANTZ) CALL F06QKF('Left','Transpose',N,WORK,NCOLZ,Z,LDZ)
      WORK(1) = ITER
      IF (K.EQ.1) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
      ELSE
         WRITE (REC,FMT=99998) K
         IFAIL = P01ABF(IFAIL,K,SRNAME,2,REC)
      END IF
      RETURN
!C
!C
!C     End of F02WUY. ( SBIQR )
!C
99999 FORMAT ('    The input parameters contained ',I2,' error(s)')
99998 FORMAT ('    The QR algorithm has failed to converge.',/'    ',I6,
     *       ' singular values have NOT been found.')
      END
!C*************************************************************************************************
      SUBROUTINE F02WUZ(N,A,LDA,NCOLY,Y,LDY,WORK,IFAIL)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  1. Purpose
!C     =======
!C
!C  F02WUZ returns either the matrix Z given by
!C
!C     Z = P'*Y,
!C
!C  where  Y is an  n by ncoly matrix and  P is the right-hand orthogonal
!C  matrix  associated  with the  transformation of an  upper  triangular
!C  matrix to bidiagonal form, or the matrix  P'  itself.
!C
!C  This routine must be preceded by a call to routine F02WUX.
!C
!C  2. Description
!C     ===========
!C
!C  Routine  F02WUX factorizes the  n by n upper triangular matrix  R  as
!C
!C     R = Q*B*P',
!C
!C  where  Q and P  are  n by n orthogonal matrices and  B  is an  n by n
!C  bidiagonal matrix and information on the matrix  P is returned in the
!C  upper triangular part of the array A. Following a call to F02WUX this
!C  routine may be used to find either the  n by ncoly  matrix  Z  or the
!C  n by n orthogonal matrix P'.
!C
!C  3. Parameters
!C     ==========
!C
!C  N      - INTEGER.
!C
!C           On entry, N specifies the order of the matrix  R.  N must be
!C           at least  zero.  When  N = 0  then  an  immediate return  is
!C           effected.
!C
!C           Unchanged on exit.
!C
!C  A      - REAL             array of DIMENSION ( LDA, n )
!C
!C           Before entry, the leading  N by N  upper triangular  part of
!C           the array  A  must contain information on the matrix  P,  as
!C           returned  by  routine  F02WUX.
!C
!C           On exit  with   NCOLY .gt. 0,  the  array  A  is  unchanged.
!C           On exit  with   NCOLY = 0,  the leading  N by N  part of the
!C           array  A  is overwritten by the matrix  P'.
!C
!C  LDA    - INTEGER.
!C
!C           On  entry,  LDA  must specify  the leading dimension  of the
!C           array  A as declared in the calling (sub) program. LDA  must
!C           be at least N.
!C
!C           Unchanged on exit.
!C
!C  NCOLY  - INTEGER.
!C
!C           On entry ,  NCOLY  must specify the number of columns of the
!C           matrix  Y  and must be at least  zero. When  NCOLY = 0  then
!C           the array  Y  is not referenced, but instead the array  A is
!C           overwritten by the matrix  P'.
!C
!C           Unchanged on exit.
!C
!C  Y      - REAL             array of DIMENSION ( LDY, ncoly ).
!C
!C           Before entry with  NCOLY .gt. 0, the leading n by ncoly part
!C           of the array Y must contain the matrix to be transformed and
!C           on exit  Y  is overwritten by the  n by ncoly  matrix  P'*Y.
!C
!C           When  NCOLY = 0  then the array  Y is not referenced.
!C
!C
!C  LDY    - INTEGER.
!C
!C           On  entry,  LDY  must specify  the leading dimension  of the
!C           array  Y  as  declared  in the  calling (sub) program.  When
!C           NCOLY .gt. 0  then LDY must be at least n.
!C
!C           Unchanged on exit.
!C
!C  WORK   - REAL             array    of      DIMENSION      at    least
!C           max( 1, 2*( n - 1 ) ).
!C
!C           Used as internal workspace.
!C
!C  IFAIL  - INTEGER.
!C
!C           Before entry,  IFAIL  must contain one of the values -1 or 0
!C           or 1 to specify noisy soft failure or noisy hard failure  or
!C           silent soft failure. ( See Chapter P01 for further details.)
!C
!C           On  successful exit  IFAIL  will be  zero,  otherwise  IFAIL
!C           will  be set to  -1  indicating that an  input parameter has
!C           been  incorrectly  set. See  the  next  section  for further
!C           details.
!C
!C  4. Diagnostic Information
!C     ======================
!C
!C  IFAIL = -1
!C
!C     One or more of the following conditions holds:
!C
!C        N     .lt. 0
!C        LDA   .lt. N
!C        NCOLY .lt. 0
!C        NCOLY .gt. 0  and  LDY .lt. N
!C
!C  If  on  entry,  IFAIL  was either  -1 or 0  then  further  diagnostic
!C  information  will  be  output  on  the  error message  channel. ( See
!C  routine  X04AAF. )
!C
!C
!C  Nag auxiliary linear algebra routine.
!C
!C  -- Written on 22-July-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0)
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F02WUZ')
!C     .. Scalar Arguments ..
      INTEGER           IFAIL, LDA, LDY, N, NCOLY
!C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), WORK(*), Y(LDY,*)
!C     .. Local Scalars ..
      INTEGER           IERR, J, K
!C     .. Local Arrays ..
      CHARACTER*46      REC(1)
!C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
!C     .. External Subroutines ..
      EXTERNAL          F06BCF, F06FBF, F06QXF, P01ABY
!C     .. Executable Statements ..
!C
!C     Check the input parameters.
!C
      IF (N.EQ.0) THEN
         IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
         RETURN
      END IF
      IERR = 0
      IF (N.LT.0) CALL P01ABY(N,'N',IFAIL,IERR,SRNAME)
      IF (LDA.LT.N) CALL P01ABY(LDA,'LDA',IFAIL,IERR,SRNAME)
      IF (NCOLY.LT.0) CALL P01ABY(NCOLY,'NCOLY',IFAIL,IERR,SRNAME)
      IF ((NCOLY.GT.0) .AND. (LDY.LT.N)) CALL P01ABY(LDY,'LDY',IFAIL,
     *    IERR,SRNAME)
      IF (IERR.GT.0) THEN
         WRITE (REC,FMT=99999) IERR
         IFAIL = P01ABF(IFAIL,-1,SRNAME,1,REC)
         RETURN
      END IF
      IF (NCOLY.GT.0) THEN
!C
!C        Form Z as
!C
!C           Z = P( n - 2 )*...*P( 2 )*P( 1 )*Y.
!C
         DO 40 K = 1, N - 2
!C
!C           Recover the rotations that put the zeros into the kth row of
!C           R. The cosines and sines that define  P( k ) are returned in
!C           work( 1 ), ..., work( n - 1 )       and      work( n ), ...,
!C           work( 2*( n - 1 ) ).
!C
            DO 20 J = K + 2, N
               CALL F06BCF(A(K,J),WORK(J-1),WORK(N+J-2))
   20       CONTINUE
!C
!C           Form P( k )*Y.
!C
            CALL F06QXF('Left','Variable pivot','Backwards',N,NCOLY,K+1,
     *                  N,WORK,WORK(N),Y,LDY)
   40    CONTINUE
      ELSE
!C
!C        Form P' as
!C
!C           P' = I*( P( n - 2 )*...*P( 2 )*P( 1 ) ).
!C
         IF (N.GT.1) THEN
            A(N,N) = ONE
            A(N-1,N) = ZERO
            A(N,N-1) = ZERO
            IF (N.GT.2) THEN
               DO 80 K = N - 2, 1, -1
                  A(K+1,K+1) = ONE
                  A(K,K+1) = ZERO
                  DO 60 J = K + 2, N
                     CALL F06BCF(-A(K,J),WORK(J-1),WORK(N+J-2))
                     A(K,J) = ZERO
   60             CONTINUE
                  CALL F06FBF(N-K,ZERO,A(K+1,K),1)
                  CALL F06QXF('Right','Variable pivot','Forward',N-K,
     *                        N-K,1,N-K,WORK(K+1),WORK(N+K),A(K+1,K+1),
     *                        LDA)
   80          CONTINUE
            END IF
         END IF
         A(1,1) = ONE
      END IF
!C
      IFAIL = P01ABF(IFAIL,0,SRNAME,0,REC)
      RETURN
!C
!C
!C     End of F02WUZ. ( SBIAP  )
!C
99999 FORMAT ('    The input parameters contained ',I2,' error(s)')
      END
!C*************************************************************************************************
      SUBROUTINE F02XUS(N,D,E,WANTCS,C,S)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  F02XUS  annihilates the element a( n - 1, n )  of a bidiagonal matrix
!C  A, on the assumption that  a( n, n ) is negligible, by applying plane
!C  rotations from the right.  Thus  F02XUS  performs  the transformation
!C
!C     A := A*P',
!C
!C  where  A  is  a  bidiagonal matrix,  with  diagonal elements  d( 1 ),
!C  d( 2 ), ..., d( n )   and  super-diagonal  elements   e( 1 ), e( 2 ),
!C  ..., e( n - 1 ),  and  P  is an  orthogonal matrix,  consisting  of a
!C  sequence of plane rotations
!C
!C     P = P( 1 )*...*P( n - 2 )*P( n - 1 ),
!C
!C  where  P( k ) is a plane rotation for the ( k, k + 1 ) plane. The two
!C  by two part of the plane rotation matrix  P( k )  will be of the form
!C
!C     R( k ) = (  c( k )  s( k ) ).
!C              ( -s( k )  c( k ) )
!C
!C  If  WANTCS  is supplied as true then  c( k ) and s( k )  are returned
!C  in  the  arrays  C and S,   otherwise  the  arrays  C and S  are  not
!C  referenced.
!C
!C  The element  e( n - 1 ) = a( n - 1, n )  is returned as  zero.
!C
!C  No check  is made  by this  routine to see if  d( n ) = a( n, n )  is
!C  actually negligible. If this assumption is not valid then the nth row
!C  of  A*P'  will contain non-negligible elements,  although this row is
!C  not formed by the routine.
!C
!C  If  n.le.1  then an immediate return is effected.
!C
!C  Nag Fortran 77 basic linear algebra routine.
!C
!C  -- Written on 6-August-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D+0)
!C     .. Scalar Arguments ..
      INTEGER           N
      LOGICAL           WANTCS
!C     .. Array Arguments ..
      DOUBLE PRECISION  C(*), D(*), E(*), S(*)
!C     .. Local Scalars ..
      DOUBLE PRECISION  CS, SN, TEMP
      INTEGER           I
!C     .. External Subroutines ..
      EXTERNAL          F06BAF
!C     .. Executable Statements ..
!C
      IF (N.GT.1) THEN
         I = N - 1
         TEMP = E(I)
         E(I) = ZERO
         CALL F06BAF(D(I),TEMP,CS,SN)
         IF (WANTCS) THEN
            C(I) = CS
            S(I) = SN
         END IF
         DO 20 I = N - 2, 1, -1
            TEMP = -SN*E(I)
            E(I) = CS*E(I)
            CALL F06BAF(D(I),TEMP,CS,SN)
            IF (WANTCS) THEN
               C(I) = CS
               S(I) = SN
            END IF
   20    CONTINUE
      END IF
      RETURN
!C
!C     End of F02XUS. ( SBIZRO )
!C
      END
!C*************************************************************************************************************
      SUBROUTINE F02XUT(TEST,N,D,E,FORCE,P)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  F02XUT  tests a bidiagonal matrix to see if there is a split, or if a
!C  split can be forced.
!C
!C  The bidiagonal matrix must be supplied in the arrays D and E with the
!C  diagonal  elements  in  d( 1 ), d( 2 ), ..., d( n )  and  the  super-
!C  diagonal  elements  in  e( 1 ), e( 2 ), ..., e( n - 1 ).  The  arrays
!C  are unaltered on exit.
!C
!C  The  bidiagonal matrix  is  searched  from the  bottom backwards  for
!C  negligible elements  and  P  returns  the  row  in  which  the  first
!C  negligible element is found. If no negligible element is found then
!C  P = 0  is returned.
!C
!C  If  d( p ) is negligible then  FORCE  is set to true, indicating that
!C  a  split  can be forced,  otherwise  FORCE  is returned as false.  If
!C  FORCE  is  returned as  false  and  P  is  returned  as positive then
!C  e( p ) is negligible.
!C
!C  When   test .le. 0.0   then  a  local test  is  used  and  d( p )  is
!C  regarded as negligible if
!C
!C    abs( d( p ) ) .le. eps*( max( abs( e( p ) ), abs( e( p - 1 ) ) ) ),
!C
!C  where  e( n ) = e( 0 ) = 0.0   and   eps   is  the  relative  machine
!C  precision as returned by routine  X02AJF, or if
!C
!C    abs( d( p ) ) .lt. flmin/eps,
!C
!C  where  flmin  is the  underflow  threshold  as  returned  by  routine
!C  X02AKF, and similarly  e( p )  is regarded as negligible if
!C
!C    abs( e( p ) ) .le. eps*( max( abs( d( p + 1 ) ), abs( d( p ) ) ) ),
!C
!C  or if
!C
!C    abs( e( p ) ) .lt. flmin/eps.
!C
!C  When   test .gt. 0.0  then  a  global test  is  used  and  d( p )  is
!C  regarded as negligible if
!C
!C    abs( d( p ) ) .lt. eps*test,
!C
!C  and similarly  e( p )  is regarded as negligible if
!C
!C    abs( e( p ) ) .lt. eps*test.
!C
!C  n  must be at least zero.
!C
!C
!C  Nag Fortran 77 basic linear algebra routine.
!C
!C  -- Written on 1-October-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D+0)
!C     .. Scalar Arguments ..
      DOUBLE PRECISION  TEST
      INTEGER           N, P
      LOGICAL           FORCE
!C     .. Array Arguments ..
      DOUBLE PRECISION  D(*), E(*)
!C     .. Local Scalars ..
      DOUBLE PRECISION  ABSD, ABSDI, ABSE, ABSEI, AMAX, DMAX, EMAX, EPS,
     *                  FLMIN, NEGL, SMALL
      INTEGER           I
      LOGICAL           FIRST
!C     .. External Functions ..
      DOUBLE PRECISION  X02AJF, X02AKF
      EXTERNAL          X02AJF, X02AKF
!C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX
!C     .. Save statement ..
      SAVE              FIRST, EPS, FLMIN, SMALL
!C     .. Data statements ..
      DATA              FIRST/.TRUE./
!C     .. Executable Statements ..
      IF (FIRST) THEN
         FIRST = .FALSE.
         EPS = X02AJF()
         FLMIN = X02AKF()
         SMALL = FLMIN/EPS
      END IF
!C
      FORCE = .FALSE.
      I = N
      IF (TEST.LE.ZERO) THEN
         IF (N.EQ.1) THEN
            IF (ABS(D(N)).LT.SMALL) THEN
               FORCE = .TRUE.
               GO TO 60
            END IF
         ELSE
            ABSD = ABS(D(N))
            ABSE = ABS(E(N-1))
            AMAX = MAX(ABSD,ABSE)
            IF ((ABSD.LE.EPS*AMAX) .OR. (AMAX.LT.SMALL)) THEN
               FORCE = .TRUE.
               GO TO 60
            END IF
            DO 20 I = N - 1, 2, -1
               ABSDI = ABS(D(I))
               DMAX = MAX(ABSDI,ABSD)
               AMAX = MAX(DMAX,ABSE)
               IF ((ABSE.LE.EPS*DMAX) .OR. (AMAX.LT.SMALL)) GO TO 60
               ABSEI = ABS(E(I-1))
               EMAX = MAX(ABSE,ABSEI)
               AMAX = MAX(EMAX,ABSDI)
               IF ((ABSDI.LE.EPS*EMAX) .OR. (AMAX.LT.SMALL)) THEN
                  FORCE = .TRUE.
                  GO TO 60
               END IF
               ABSD = ABSDI
               ABSE = ABSEI
   20       CONTINUE
            ABSDI = ABS(D(1))
            DMAX = MAX(ABSDI,ABSD)
            AMAX = MAX(DMAX,ABSE)
            IF ((ABSE.LE.EPS*DMAX) .OR. (AMAX.LT.SMALL)) GO TO 60
            AMAX = MAX(ABSE,ABSDI)
            IF ((ABSDI.LE.EPS*ABSE) .OR. (AMAX.LT.SMALL)) THEN
               FORCE = .TRUE.
               GO TO 60
            END IF
         END IF
      ELSE
         NEGL = EPS*TEST
         IF (ABS(D(N)).LT.NEGL) THEN
            FORCE = .TRUE.
            GO TO 60
         END IF
         DO 40 I = N - 1, 1, -1
            IF (ABS(E(I)).LT.NEGL) GO TO 60
            IF (ABS(D(I)).LT.NEGL) THEN
               FORCE = .TRUE.
               GO TO 60
            END IF
   40    CONTINUE
      END IF
      I = 0
   60 CONTINUE
      P = I
      RETURN
!C
!C     End of F02XUT. ( SBITST )
!C
      END
!C****************************************************************************************************************
      SUBROUTINE F02XUU(JOB,D1,E1,DNM1,DN,ENM2,ENM1,C,S)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  F02XUU  determines  the  shift parameters,  c and s,  for  the  first
!C  rotation to be performed in a QR step applied to a bidiagonal matrix.
!C
!C  c and s  are such that
!C
!C     (  c  s )*( d1**2 - shift ) := ( x )
!C     ( -s  c ) (     d1*e1     )    ( 0 )
!C
!C  and  shift  is  determined  by the  value  of the  parameter  job  as
!C  follows:
!C
!C     When  JOB =  0  then
!C
!C        shift = 0.
!C
!C     When  JOB =  1  then
!C
!C        shift  is the  eigenvalue closest  to the element  a( 2, 2 ) of
!C        the matrix A given by
!C
!C           A = ( dnm1**2 + enm2**2    dnm1*enm1     ).
!C               (     dnm1*enm1      dn**2 + enm1**2 )
!C
!C     When  JOB = -1  then
!C
!C        shift  is the eigenvalue closest to the element  a( 2, 2 ) of
!C        the matrix A given by
!C
!C           A = ( dnm1**2 + enm1**2  dn*enm1 ).
!C               (       dn*enm1       dn**2  )
!C
!C     When  JOB =  2  then
!C
!C        shift  is the  smallest eigenvalue  of the  matrix  A  given by
!C
!C           A = ( dnm1**2 + enm2**2    dnm1*enm1     ).
!C               (     dnm1*enm1      dn**2 + enm1**2 )
!C
!C     When  JOB = -2  then
!C
!C        shift  is the  smallest eigenvalue  of the  matrix  A  given by
!C
!C           A = ( dnm1**2 + enm1**2  dn*enm1 ).
!C               (       dn*enm1       dn**2  )
!C
!C
!C  Nag Fortran 77 basic linear algebra routine.
!C
!C  -- Written on 14-September-1987.
!C     Sven Hammarling.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0)
!C     .. Scalar Arguments ..
      DOUBLE PRECISION  C, D1, DN, DNM1, E1, ENM1, ENM2, S
      INTEGER           JOB
!C     .. Local Scalars ..
      DOUBLE PRECISION  A, APLUSC, B, BOT, CORR, F, RSQEPS, SHIFT,
     *                  SQTEPS, TOP
      LOGICAL           FAIL, FIRST
!C     .. External Functions ..
      DOUBLE PRECISION  F06BLF, X02AJF
      EXTERNAL          F06BLF, X02AJF
!C     .. External Subroutines ..
      EXTERNAL          F06BAF
!C     .. Intrinsic Functions ..
      INTRINSIC         ABS, SIGN, SQRT
!C     .. Save statement ..
      SAVE              FIRST, SQTEPS, RSQEPS
!C     .. Data statements ..
      DATA              FIRST/.TRUE./
!C     .. Executable Statements ..
      IF (FIRST) THEN
         FIRST = .FALSE.
         SQTEPS = SQRT(X02AJF())
         RSQEPS = 1/SQTEPS
      END IF
!C
      IF (JOB.EQ.0) THEN
         A = D1
         B = E1
      ELSE IF (JOB.EQ.1) THEN
         IF (ENM1.EQ.ZERO) THEN
            CORR = ZERO
         ELSE
            TOP = (DNM1-DN)*(DNM1+DN) + (ENM2-ENM1)*(ENM2+ENM1)
            IF (TOP.EQ.ZERO) THEN
               CORR = ENM1*(DNM1-ENM1)
            ELSE
               BOT = 2*DNM1*ENM1
               F = F06BLF(TOP,BOT,FAIL)
               IF (FAIL) THEN
                  CORR = -ENM1**2
               ELSE
                  IF (ABS(F).LT.SQTEPS) THEN
                     BOT = F + SIGN(ONE,F)
                  ELSE IF (ABS(F).GT.RSQEPS) THEN
                     BOT = 2*F
                  ELSE
                     BOT = F + SIGN(ONE,F)*SQRT(1+F**2)
                  END IF
                  CORR = ENM1*(DNM1/BOT-ENM1)
               END IF
            END IF
         END IF
         IF (D1.NE.ZERO) THEN
            A = (ONE-DN/D1)*(D1+DN) + CORR/D1
            B = E1
         ELSE
            A = ONE
            B = ZERO
         END IF
      ELSE IF (JOB.EQ.(-1)) THEN
         TOP = (DN*ENM1)**2
         IF (TOP.EQ.ZERO) THEN
            CORR = ZERO
         ELSE
            F = ((DNM1-DN)*(DNM1+DN)+ENM1**2)/2
            BOT = F + SIGN(ONE,F)*SQRT(TOP+F**2)
            CORR = F06BLF(TOP,BOT,FAIL)
         END IF
         IF (D1.NE.ZERO) THEN
            A = (ONE-DN/D1)*(D1+DN) + CORR/D1
            B = E1
         ELSE
            A = ONE
            B = ZERO
         END IF
      ELSE IF (JOB.EQ.2) THEN
         TOP = 2*((DN*DNM1)**2+(DN*ENM2)**2+(ENM2*ENM1)**2)
         IF (TOP.EQ.ZERO) THEN
            SHIFT = ZERO
         ELSE
            F = ((DNM1-DN)*(DNM1+DN)+(ENM2-ENM1)*(ENM2+ENM1))**2 +
     *          (2*DNM1*ENM1)**2
            APLUSC = DNM1**2 + DN**2 + ENM2**2 + ENM1**2
            BOT = APLUSC + SIGN(ONE,APLUSC)*SQRT(F)
            SHIFT = F06BLF(TOP,BOT,FAIL)
         END IF
         IF (D1.NE.ZERO) THEN
            A = D1 - SHIFT/D1
            B = E1
         ELSE
            A = ONE
            B = ZERO
         END IF
      ELSE IF (JOB.EQ.(-2)) THEN
         TOP = 2*((DN*DNM1)**2)
         IF (TOP.EQ.ZERO) THEN
            SHIFT = ZERO
         ELSE
            F = ((DNM1-DN)**2+ENM1**2)*((DNM1+DN)**2+ENM1**2)
            APLUSC = DNM1**2 + DN**2 + ENM1**2
            BOT = APLUSC + SIGN(ONE,APLUSC)*SQRT(F)
            SHIFT = F06BLF(TOP,BOT,FAIL)
         END IF
         IF (D1.NE.ZERO) THEN
            A = D1 - SHIFT/D1
            B = E1
         ELSE
            A = ONE
            B = ZERO
         END IF
      END IF
      CALL F06BAF(A,B,C,S)
!C
      RETURN
!C
!C     End of F02XUU. ( SBISFT )
!C
      END
!C***************************************************************************************************************
      SUBROUTINE F02XUV(SHIFT,M,N,D,E,C,S,WANTLT,CL,SL,WANTRT,CR,SR)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  F02XUV performs a QR step on a bidiagonal matrix A.
!C
!C  When  SHIFT = 'N' or 'n'  ( Non-zero ),  then the parameters  c and s
!C  must define the  initial right hand  plane rotation  to be used,  but
!C  when  SHIFT = 'Z' or 'z'  ( Zero ), then the initial right hand plane
!C  is determined by F02XUV so that
!C
!C     ( d( m )  0 ) := ( d( m )  e( m ) )*( c  -s )
!C                                         ( s   c )
!C
!C  and c and s need not be supplied.
!C
!C  In either case,  plane rotations are then performed  alternately from
!C  the  left and right  in order to  recover  the  bidiagonal form.  The
!C  bidiagonal matrix  must be  supplied  with the  diagonal elements  in
!C  d( m ), d( m + 1 ), ..., d( n )  and the  super-diagonal elements  in
!C  e( m ), e( m + 1 ), ..., e( n - 1 ). When  m  is greater than  unity,
!C  the assumption is that   e( m - 1 ) = 0.0.
!C
!C  Thus F02XUV performs the transformation
!C
!C     A := Q*A*P',
!C
!C  where  Q and P  are  orthogonal matrices.  P and Q  each consist of a
!C  sequence of plane rotations
!C
!C     P = P( n - 1 )*...*P( m + 1 )*P( m )
!C
!C     Q = Q( n - 1 )*...*Q( m + 1 )*Q( m ),
!C
!C  where Q( k ) and P( k ) are each plane rotations for the ( k, k + 1 )
!C  plane. The two by two part of the plane rotation matrix  Q( k )  will
!C  be of the form
!C
!C     Q2 = (  cl( k )  sl( k ) ),
!C          ( -sl( k )  cl( k ) )
!C
!C  and  if  WANTLT  is supplied as  true  then  cl( k ) and sl( k )  are
!C  returned in the  corresponding elements of the arrays  CL and SL.  If
!C  WANTLT  is supplied as false then  CL and SL  are not referenced. The
!C  two by two  part of the plane rotation matrix  P( k )  will be of the
!C  form
!C
!C     P2 = (  cr( k )  sr( k ) ),
!C          ( -sr( k )  cr( k ) )
!C
!C  and  if  WANTRT  is supplied as  true  then  cr( k ) and sr( k )  are
!C  returned in the corresponding elements of the arrays  CR and SR. Note
!C  that  cr( m ) = c  and  sr( m ) = s. If  WANTRT  is supplied as false
!C  then  CR and SR  are not referenced.
!C
!C  If  m.le.0  or  n.le.m  then an immediate return is effected.
!C
!C
!C  Nag Fortran 77 basic linear algebra routine.
!C
!C  -- Written on 10-August-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Scalar Arguments ..
      DOUBLE PRECISION  C, S
      INTEGER           M, N
      LOGICAL           WANTLT, WANTRT
      CHARACTER*1       SHIFT
!C     .. Array Arguments ..
      DOUBLE PRECISION  CL(*), CR(*), D(*), E(*), SL(*), SR(*)
!C     .. Local Scalars ..
      DOUBLE PRECISION  CO, CS, EI, SI, SN, TEMP
      INTEGER           I
!C     .. External Subroutines ..
      EXTERNAL          F06BAF
!C     .. Executable Statements ..
      IF ((M.GT.0) .AND. (M.LT.N)) THEN
         IF ((SHIFT.EQ.'N') .OR. (SHIFT.EQ.'n')) THEN
            IF (WANTRT) THEN
               CR(M) = C
               SR(M) = S
            END IF
            TEMP = C*D(M) + S*E(M)
            E(M) = C*E(M) - S*D(M)
            D(M) = TEMP
            TEMP = S*D(M+1)
            D(M+1) = C*D(M+1)
            DO 20 I = M, N - 2
               CALL F06BAF(D(I),TEMP,CS,SN)
               IF (WANTLT) THEN
                  CL(I) = CS
                  SL(I) = SN
               END IF
               TEMP = CS*E(I) + SN*D(I+1)
               D(I+1) = CS*D(I+1) - SN*E(I)
               E(I) = TEMP
               TEMP = SN*E(I+1)
               E(I+1) = CS*E(I+1)
               CALL F06BAF(E(I),TEMP,CS,SN)
               IF (WANTRT) THEN
                  CR(I+1) = CS
                  SR(I+1) = SN
               END IF
               TEMP = CS*D(I+1) + SN*E(I+1)
               E(I+1) = CS*E(I+1) - SN*D(I+1)
               D(I+1) = TEMP
               TEMP = SN*D(I+2)
               D(I+2) = CS*D(I+2)
   20       CONTINUE
            CALL F06BAF(D(N-1),TEMP,CS,SN)
            IF (WANTLT) THEN
               CL(N-1) = CS
               SL(N-1) = SN
            END IF
            TEMP = CS*E(N-1) + SN*D(N)
            D(N) = CS*D(N) - SN*E(N-1)
            E(N-1) = TEMP
         ELSE
            CALL F06BAF(D(M),E(M),C,S)
            IF (WANTRT) THEN
               CR(M) = C
               SR(M) = S
            END IF
            TEMP = S*D(M+1)
            D(M+1) = C*D(M+1)
            DO 40 I = M, N - 2
               CALL F06BAF(D(I),TEMP,CO,SI)
               IF (WANTLT) THEN
                  CL(I) = CO
                  SL(I) = SI
               END IF
               EI = D(I+1)
               TEMP = E(I+1)
               CALL F06BAF(EI,TEMP,CS,SN)
               IF (WANTRT) THEN
                  CR(I+1) = CS
                  SR(I+1) = SN
               END IF
               E(I) = SI*EI
               D(I+1) = CO*EI
               TEMP = SN*D(I+2)
               D(I+2) = CS*D(I+2)
   40       CONTINUE
            CALL F06BAF(D(N-1),TEMP,CS,SN)
            IF (WANTLT) THEN
               CL(N-1) = CS
               SL(N-1) = SN
            END IF
            E(N-1) = SN*D(N)
            D(N) = CS*D(N)
         END IF
      END IF
      RETURN
!C
!C     End of F02XUV. ( SBIQR1 )
!C
      END
!C***********************************************************************************************************
      SUBROUTINE F02XUW(M,N,D,E,WANTCS,C,S)
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C
!C  F02XUW annihilates the element  a( m, m + 1 )  of a bidiagonal matrix
!C  A, on the assumption that  a( m, m ) is negligible, by applying plane
!C  rotations from the  left.  Thus  F02XUW  performs  the transformation
!C
!C     A := P*A,
!C
!C  where  A  is  a  bidiagonal matrix,  with  diagonal elements  d( 1 ),
!C  d( 2 ), ..., d( n )   and  super-diagonal  elements   e( 1 ), e( 2 ),
!C  ..., e( n - 1 ),  and  P  is an  orthogonal matrix,  consisting  of a
!C  sequence of plane rotations
!C
!C     P = P( m )*...*P( n - 2 )*P( n - 1 ),
!C
!C  where  P( k ) is a plane rotation for the ( k, k + 1 ) plane. The two
!C  by two part of the plane rotation matrix  P( k )  will be of the form
!C
!C     R( k ) = (  c( k )  s( k ) ).
!C              ( -s( k )  c( k ) )
!C
!C  If  WANTCS  is supplied as true then  c( k ) and s( k )  are returned
!C  in  the  arrays  C and S,   otherwise  the  arrays  C and S  are  not
!C  referenced.
!C
!C  The element  e( m ) = a( m, m + 1 )  is returned as  zero.
!C
!C  No check  is made  by this  routine to see if  d( m ) = a( m, m )  is
!C  actually negligible.  If this  assumption  is not valid then the  mth
!C  column of  P*A  will contain  non-negligible elements,  although this
!C  column is not formed by the routine.
!C
!C  If  m.le.0 or m.ge.n  then an immediate return is effected.
!C
!C
!C  Nag Fortran 77 basic linear algebra routine.
!C
!C  -- Written on 6-August-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D+0)
!C     .. Scalar Arguments ..
      INTEGER           M, N
      LOGICAL           WANTCS
!C     .. Array Arguments ..
      DOUBLE PRECISION  C(*), D(*), E(*), S(*)
!C     .. Local Scalars ..
      DOUBLE PRECISION  CS, SN, TEMP
      INTEGER           I
!C     .. External Subroutines ..
      EXTERNAL          F06BAF
!C     .. Executable Statements ..
!C
      IF ((M.GT.0) .AND. (M.LT.N)) THEN
         I = M
         TEMP = E(I)
         E(I) = ZERO
         CALL F06BAF(D(I+1),TEMP,CS,SN)
         IF (WANTCS) THEN
            C(I) = CS
            S(I) = -SN
         END IF
         DO 20 I = M + 1, N - 1
            TEMP = -SN*E(I)
            E(I) = CS*E(I)
            CALL F06BAF(D(I+1),TEMP,CS,SN)
            IF (WANTCS) THEN
               C(I) = CS
               S(I) = -SN
            END IF
   20    CONTINUE
      END IF
      RETURN
!C
!C     End of F02XUW. ( SBIFRC )
!C
      END
!C*******************************************************************************************************************************
      SUBROUTINE F06AAZ ( SRNAME, INFO )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     .. Scalar Arguments ..
      INTEGER            INFO
      CHARACTER*13       SRNAME
!C     ..
!C
!C  Purpose
!C  =======
!C
!C  F06AAZ  is an error handler for the Level 2 BLAS routines.
!C
!C  It is called by the Level 2 BLAS routines if an input parameter is
!C  invalid.
!C
!C  Parameters
!C  ==========
!C
!C  SRNAME - CHARACTER*13.
!C           On entry, SRNAME specifies the name of the routine which
!C           called F06AAZ.
!C
!C  INFO   - INTEGER.
!C           On entry, INFO specifies the position of the invalid
!C           parameter in the parameter-list of the calling routine.
!C
!C
!C  Auxiliary routine for Level 2 Blas.
!C
!C  Written on 20-July-1986.
!C
!C     .. Local Scalars ..
      INTEGER            IFAIL
      CHARACTER*80       REC (1)
!C     .. External Functions ..
      INTEGER            P01ABF
      EXTERNAL           P01ABF
!C     ..
!C     .. Executable Statements ..
      WRITE (REC (1),99999) SRNAME, INFO
      IFAIL = 0
      IFAIL = P01ABF (IFAIL, -1, SRNAME(1:6), 1, REC)
!C
      RETURN
!C
99999 FORMAT ( ' ** On entry to ', A13, ' parameter number ', I2,
     $         ' had an illegal value' )
!C
!C     End of F06AAZ.
!C
      END
!C*************************************************************************************************************************************
      SUBROUTINE F06BAF( A, B, C, S )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B, C, S
!C     ..
!C
!C  Nag  Fortran 77  version of the  SROTG BLAS,  except that c is always
!C  returned as non-negative and  b  is overwritten by the tangent of the
!C  angle that defines the plane rotation.
!C
!C  c and s are given as
!C
!C     c = 1.0/sqrt( 1.0 + t**2 ),   s = c*t   where   t = b/a.
!C
!C  When  abs( b ) .le. eps*abs( a ),  where  eps is the relative machine
!C  precision as  returned by routine  X02AJF,  then  c and s  are always
!C  returned as
!C
!C     c = 1.0  and  s = 0.0
!C
!C  and when  abs( a ) .le. eps*abs( b ) then c and s are always returned
!C  as
!C
!C     c = 0.0  and  s = sign( t ).
!C
!C  Note that t is always returned as  b/a, unless this would overflow in
!C  which  case the value  sign( t )*flmax  is returned,  where  flmax is
!C  the value given by  1/X02AMF( ).
!C
!C  c and s  can be reconstructed from the tangent,  t,  by a call to the
!C  Nag basic linear algebra routine F06BCF.
!C
!C
!C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
!C
!C  -- Written on 3-January-1986.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!C     .. Local Scalars ..
      DOUBLE PRECISION   T
      LOGICAL            FAIL
!C     .. External Functions ..
      DOUBLE PRECISION   F06BLF
      EXTERNAL           F06BLF
!C     .. External Subroutines ..
      EXTERNAL           F06BCF
!C     ..
!C     .. Executable Statements ..
      IF( B.EQ.ZERO )THEN
         C  = ONE
         S  = ZERO
      ELSE
         T  = F06BLF( B, A, FAIL )
         CALL F06BCF( T, C, S )
         A  = C*A + S*B
         B  = T
      END IF
!C
      RETURN
!C
!C     End of F06BAF. ( SROTGC )
!C
      END
!C*************************************************************************************************************************************
      SUBROUTINE F06BCF( T, C, S )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     MARK 13 REVISED. IER-602 (MAR 1988).
!C     .. Scalar Arguments ..
      DOUBLE PRECISION   C, S, T
!C     ..
!C
!C  F06BCF returns values c and s such that
!C
!C     c = cos( theta ),   s = sin( theta )
!C
!C  for a given value of
!C
!C     t = tan( theta ).
!C
!C  c is always non-negative and s has the same sign as t, so that
!C
!C     c = 1.0/sqrt( 1.0 + t**2 ),   s = t/sqrt( 1.0 + t**2 ).
!C
!C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
!C
!C  -- Written on 28-February-1986.
!C     Sven Hammarling, Nag Central Office.
!C  -- Modified 19-August-1987.
!C     Sven Hammarling and Jeremy Du Croz, Nag Central Office.
!C        No longer sets s to zero when t is less than eps.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER        ( ONE = 1.0D+0 )
!C     .. Local Scalars ..
      DOUBLE PRECISION   ABST, EPS, REPS, RRTEPS, RTEPS
      LOGICAL            FIRST
!C     .. External Functions ..
      DOUBLE PRECISION   X02AJF
      EXTERNAL           X02AJF
!C     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
!C     .. Save statement ..
      SAVE               FIRST, EPS, REPS, RTEPS, RRTEPS
!C     .. Data statements ..
      DATA               FIRST/ .TRUE. /
!C     ..
!C     .. Executable Statements ..
      IF( FIRST )THEN
         FIRST  = .FALSE.
         EPS    =  X02AJF( )
         REPS   =  1/EPS
         RTEPS  =  SQRT( EPS )
         RRTEPS =  1/RTEPS
      END IF
!C
      ABST = ABS( T )
      IF( ABST.LT.RTEPS )THEN
         C = ONE
         S = T
      ELSE IF( ABST.GT.RRTEPS )THEN
         C = 1/ABST
         S = SIGN( ONE, T )
      ELSE
         C = 1/SQRT( 1 + ABST**2 )
         S = C*T
      END IF
!C
      RETURN
!C
!C     End of F06BCF. ( SCSG )
!C
      END
!C**********************************************************************************************************************************
      DOUBLE PRECISION FUNCTION F06BLF( A, B, FAIL )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     .. Scalar Arguments ..
      DOUBLE PRECISION                  A, B
      LOGICAL                           FAIL
!C     ..
!C
!C  F06BLF returns the value div given by
!C
!C     div = ( a/b                 if a/b does not overflow,
!C           (
!C           ( 0.0                 if a .eq. 0.0,
!C           (
!C           ( sign( a/b )*flmax   if a .ne. 0.0  and a/b would overflow,
!C
!C  where  flmax  is a large value, via the function name. In addition if
!C  a/b would overflow then  fail is returned as true, otherwise  fail is
!C  returned as false.
!C
!C  Note that when  a and b  are both zero, fail is returned as true, but
!C  div  is returned as  0.0. In all other cases of overflow  div is such
!C  that  abs( div ) = flmax.
!C
!C  When  b = 0  then  sign( a/b )  is taken as  sign( a ).
!C
!C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
!C
!C  -- Written on 26-October-1982.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION      ONE         , ZERO
      PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!C     .. Local Scalars ..
      DOUBLE PRECISION      ABSB, DIV, FLMAX, FLMIN
      LOGICAL               FIRST
!C     .. External Functions ..
      DOUBLE PRECISION      X02AMF
      EXTERNAL              X02AMF
!C     .. Intrinsic Functions ..
      INTRINSIC             ABS, SIGN
!C     .. Save statement ..
      SAVE                  FIRST, FLMIN, FLMAX
!C     .. Data statements ..
      DATA                  FIRST/ .TRUE. /
!C     ..
!C     .. Executable Statements ..
      IF( A.EQ.ZERO )THEN
         DIV = ZERO
         IF( B.EQ.ZERO )THEN
            FAIL = .TRUE.
         ELSE
            FAIL = .FALSE.
         END IF
      ELSE
!C
         IF( FIRST )THEN
            FIRST = .FALSE.
            FLMIN =  X02AMF( )
            FLMAX =  1/FLMIN
         END IF
!C
         IF( B.EQ.ZERO )THEN
            DIV  =  SIGN( FLMAX, A )
            FAIL = .TRUE.
         ELSE
            ABSB = ABS( B )
            IF( ABSB.GE.ONE )THEN
               FAIL = .FALSE.
               IF( ABS( A ).GE.ABSB*FLMIN )THEN
                  DIV = A/B
               ELSE
                  DIV = ZERO
               END IF
            ELSE
               IF( ABS( A ).LE.ABSB*FLMAX )THEN
                  FAIL = .FALSE.
                  DIV  =  A/B
               ELSE
                  FAIL = .TRUE.
                  DIV  = FLMAX
                  IF( ( ( A.LT.ZERO ).AND.( B.GT.ZERO ) ).OR.
     $                ( ( A.GT.ZERO ).AND.( B.LT.ZERO ) )     )
     $               DIV = -DIV
               END IF
            END IF
         END IF
      END IF
!C
      F06BLF = DIV
      RETURN
!C
!C     End of F06BLF. ( SDIV )
!C
      END
!C******************************************************************************************************************
      SUBROUTINE F06EDF( N, ALPHA, X, INCX )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     .. Entry Points ..
      ENTRY      DSCAL ( N, ALPHA, X, INCX )
!C     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, N
!C     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
!C     ..
!C
!C  F06EDF performs the operation
!C
!C     x := alpha*x
!C
!C
!C  Nag Fortran 77 version of the Blas routine DSCAL.
!C  Nag Fortran 77 O( n ) basic linear algebra routine.
!C
!C  -- Written on 26-November-1982.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!C     .. Local Scalars ..
      INTEGER            IX
!C     ..
!C     .. Executable Statements ..
      IF( N.GT.0 )THEN
         IF( ALPHA.EQ.ZERO )THEN
            DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
               X( IX ) = ZERO
   10       CONTINUE
         ELSE IF( ALPHA.EQ.( -ONE ) )THEN
            DO 20, IX = 1, 1 + ( N - 1 )*INCX, INCX
               X( IX ) = -X( IX )
   20       CONTINUE
         ELSE IF( ALPHA.NE.ONE )THEN
            DO 30, IX = 1, 1 + ( N - 1 )*INCX, INCX
               X( IX ) = ALPHA*X( IX )
   30       CONTINUE
         END IF
      END IF
!C
      RETURN
!C
!C     End of F06EDF. ( DSCAL )
!C
      END
!C**********************************************************************************************************************************
      SUBROUTINE F06FBF( N, CONST, X, INCX )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     .. Scalar Arguments ..
      DOUBLE PRECISION   CONST
      INTEGER            INCX, N
!C     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
!C     ..
!C
!C  F06FBF performs the operation
!C
!C     x = const*e,   e' = ( 1  1 ... 1 ).
!C
!C
!C  Nag Fortran 77 O( n ) basic linear algebra routine.
!C
!C  -- Written on 22-September-1983.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
!C     .. Local Scalars ..
      INTEGER            IX
!C     ..
!C     .. Executable Statements ..
      IF( N.GT.0 )THEN
         IF( CONST.NE.ZERO )THEN
            DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
               X( IX ) = CONST
   10       CONTINUE
         ELSE
            DO 20, IX = 1, 1 + ( N - 1 )*INCX, INCX
               X( IX ) = ZERO
   20       CONTINUE
         END IF
      END IF
!C
      RETURN
!C
!C     End of F06FBF. ( SLOAD )
!C
      END
!C******************************************************************************************************************************
      SUBROUTINE F06FGF( N, X, INCX )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     .. Scalar Arguments ..
      INTEGER            INCX, N
!C     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
!C     ..
!C
!C  F06FGF performs the operation
!C
!C     x := -x
!C
!C
!C  Nag Fortran 77 O( n ) basic linear algebra routine.
!C
!C  -- Written on 22-September-1983.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Local Scalars ..
      INTEGER            IX
!C     ..
!C     .. Executable Statements ..
      IF( N.GT.0 )THEN
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            X( IX ) = -X( IX )
   10    CONTINUE
      END IF
!C
      RETURN
!C
!C     End of F06FGF. ( SNEGV )
!C
      END
!C*********************************************************************************************************************************
      SUBROUTINE F06FJF( N, X, INCX, SCALE, SUMSQ )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     .. Scalar Arguments ..
      DOUBLE PRECISION   SCALE, SUMSQ
      INTEGER            INCX, N
!C     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
!C     ..
!C
!C  F06FJF returns the values scl and smsq such that
!C
!C     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
!C
!C  where x( i ) = X( 1 + ( i - 1 )*INCX ). The value of sumsq is assumed
!C  to be at least unity and the value of smsq will then satisfy
!C
!C     1.0 .le. smsq .le. ( sumsq + n ) .
!C
!C  scale is assumed to be non-negative and scl returns the value
!C
!C     scl = max( scale, abs( x( i ) ) ) .
!C
!C  scale and sumsq must be supplied in SCALE and SUMSQ respectively.
!C  scl and smsq are overwritten on SCALE and SUMSQ respectively.
!C
!C  The routine makes only one pass through the vector X.
!C
!C
!C  Nag Fortran 77 O( n ) basic linear algebra routine.
!C
!C  -- Written on 22-October-1982.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
!C     .. Local Scalars ..
      DOUBLE PRECISION   ABSXI
      INTEGER            IX
!C     .. Intrinsic Functions ..
      INTRINSIC          ABS
!C     ..
!C     .. Executable Statements ..
      IF( N.GT.0 )THEN
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            IF( X( IX ).NE.ZERO )THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI )THEN
                  SUMSQ = 1     + SUMSQ*( SCALE/ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SUMSQ = SUMSQ +       ( ABSXI/SCALE )**2
               END IF
            END IF
   10    CONTINUE
      END IF
      RETURN
!C
!C     End of F06FJF. ( SSSQ )
!C
      END
!C*****************************************************************************************************************************************
      SUBROUTINE F06FQF( PIVOT, DIRECT, N, ALPHA, X, INCX, C, S )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, N
      CHARACTER*1        DIRECT, PIVOT
!C     .. Array Arguments ..
      DOUBLE PRECISION   C( * ), S( * ), X( * )
!C     ..
!C
!C  F06FQF generates the parameters of an orthogonal matrix P such that
!C
!C     when   PIVOT = 'F' or 'f'   and   DIRECT = 'F' or 'f'
!C     or     PIVOT = 'V' or 'v'   and   DIRECT = 'B' or 'b'
!C
!C        P*( alpha ) = ( beta ),
!C          (   x   )   (   0  )
!C
!C     when   PIVOT = 'F' or 'f'   and   DIRECT = 'B' or 'b'
!C     or     PIVOT = 'V' or 'v'   and   DIRECT = 'F' or 'f'
!C
!C        P*(   x   ) = (   0  ),
!C          ( alpha ) = ( beta )
!C
!C  where alpha is a scalar and x is an n element vector.
!C
!C  When  PIVOT = 'F' or 'f'  ( fixed pivot )
!C  and  DIRECT = 'F' or 'f'  ( forward sequence ) then
!C
!C     P is given as the sequence of plane rotation matrices
!C
!C        P = P( n )*P( n - 1 )*...*P( 1 )
!C
!C     where P( k ) is a plane rotation matrix for the ( 1, k + 1 ) plane
!C     designed to annihilate the kth element of x.
!C
!C  When  PIVOT = 'V' or 'v'  ( variable pivot )
!C  and  DIRECT = 'B' or 'b'  ( backward sequence ) then
!C
!C     P is given as the sequence of plane rotation matrices
!C
!C        P = P( 1 )*P( 2 )*...*P( n )
!C
!C     where P( k ) is a plane rotation matrix for the ( k, k + 1 ) plane
!C     designed to annihilate the kth element of x.
!C
!C  When  PIVOT = 'F' or 'f'  ( fixed pivot )
!C  and  DIRECT = 'B' or 'b'  ( backward sequence ) then
!C
!C     P is given as the sequence of plane rotation matrices
!C
!C        P = P( 1 )*P( 2 )*...*P( n )
!C
!C     where P( k ) is a plane rotation matrix for the ( k, n + 1 ) plane
!C     designed to annihilate the kth element of x.
!C
!C  When  PIVOT = 'V' or 'v'  ( variable pivot )
!C  and  DIRECT = 'F' or 'f'  ( forward sequence ) then
!C
!C     P is given as the sequence of plane rotation matrices
!C
!C        P = P( n )*P( n - 1 )*...*P( 1 )
!C
!C     where P( k ) is a plane rotation matrix for the ( k, k + 1 ) plane
!C     designed to annihilate the kth element of x.
!C
!C  The routine returns the cosine, c( k ), and sine, s( k ) that define
!C  the matrix P( k ), such that the two by two rotation part of P( k ),
!C  R( k ), has the form
!C
!C     R( k ) = (  c( k )  s( k ) ).
!C              ( -s( k )  c( k ) )
!C
!C  On entry, ALPHA must contain  the scalar alpha and on exit, ALPHA is
!C  overwritten by beta. The cosines and sines are returned in the arrays
!C  C and S and the vector x is overwritten by the tangents of the plane
!C  rotations ( t( k ) = s( k )/c( k ) ).
!C
!C
!C
!C  Nag Fortran 77 O( n ) basic linear algebra routine.
!C
!C  -- Written on 19-April-1985.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Local Scalars ..
      INTEGER            I, IX
!C     .. External Subroutines ..
      EXTERNAL           F06BAF
!C     ..
!C     .. Executable Statements ..
      IF( N.GT.0 )THEN
         IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
            IX = 1 + ( N - 1 )*INCX
            IF( ( PIVOT.EQ.'V' ).OR.( PIVOT.EQ.'v' ) )THEN
               DO 10, I = N, 2, -1
                  CALL F06BAF( X( IX - INCX ), X( IX ), C( I ), S( I ) )
                  IX = IX - INCX
   10          CONTINUE
               CALL F06BAF( ALPHA, X( IX ), C( 1 ), S( 1 ) )
            ELSE IF( ( PIVOT.EQ.'F' ).OR.( PIVOT.EQ.'f' ) )THEN
!C
!C              Here we choose c and s so that
!C
!C                 ( alpha ) := (  c  s )*( alpha  )
!C                 (   0   )    ( -s  c ) ( x( i ) )
!C
!C              which is equivalent to
!C
!C                 (   0   ) := ( c  -s )*( x( i ) )
!C                 ( alpha )    ( s   c ) ( alpha  )
!C
!C              and so we need to return  s( i ) = -s  in order to make
!C              R( i ) look like
!C
!C                 R( i ) = (  c( i )  s( i ) ).
!C                          ( -s( i )  c( i ) )
!C
               DO 20, I = N, 1, -1
                  CALL F06BAF( ALPHA, X( IX ), C( I ), S( I ) )
                  S( I )  = -S( I )
                  X( IX ) = -X( IX )
                  IX      =  IX      - INCX
   20          CONTINUE
            END IF
         ELSE IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
            IX = 1
            IF( ( PIVOT.EQ.'V' ).OR.( PIVOT.EQ.'v' ) )THEN
!C
!C              Here we choose c and s so that
!C
!C                 ( x( i + 1 ) ) := (  c  s )*( x( i + 1 ) )
!C                 (    0       )    ( -s  c ) ( x( i )     )
!C
!C              which is equivalent to
!C
!C                 (    0       ) := ( c  -s )*( x( i )     )
!C                 ( x( i + 1 ) )    ( s   c ) ( x( i + 1 ) )
!C
!C              and so we need to return  s( i ) = -s  in order to make
!C              R( i ) look like
!C
!C                 R( i ) = (  c( i )  s( i ) ).
!C                          ( -s( i )  c( i ) )
!C
               DO 30, I = 1, N - 1
                  CALL F06BAF( X( IX + INCX ), X( IX ), C( I ), S( I ) )
                  S( I )  = -S( I )
                  X( IX ) = -X( IX )
                  IX      =  IX      + INCX
   30          CONTINUE
               CALL F06BAF( ALPHA, X( IX ), C( N ), S( N ) )
               S( N )  = -S( N )
               X( IX ) = -X( IX )
            ELSE IF( ( PIVOT.EQ.'F' ).OR.( PIVOT.EQ.'f' ) )THEN
               DO 40, I = 1, N
                  CALL F06BAF( ALPHA, X( IX ), C( I ), S( I ) )
                  IX = IX + INCX
   40          CONTINUE
            END IF
         END IF
      END IF
!C
      RETURN
!C
!C     End of F06FQF. ( SSROTG )
!C
      END
!C****************************************************************************************************************************
      SUBROUTINE F06FRF( N, ALPHA, X, INCX, TOL, ZETA )
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, TOL, ZETA
      INTEGER            INCX, N
!C     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
!C     ..
!C
!C  F06FRF generates details of a generalized Householder reflection such
!C  that
!C
!C     P*( alpha ) = ( beta ),   P'*P = I.
!C       (   x   )   (   0  )
!C
!C  P is given in the form
!C
!C     P = I - ( zeta )*( zeta  z' ),
!C             (   z  )
!C
!C  where z is an n element vector and zeta is a scalar that satisfies
!C
!C     1.0 .le. zeta .le. sqrt( 2.0 ).
!C
!C  zeta is returned in ZETA unless x is such that
!C
!C     max( abs( x( i ) ) ) .le. max( eps*abs( alpha ), tol )
!C
!C  where eps is the relative machine precision and tol is the user
!C  supplied value TOL, in which case ZETA is returned as 0.0 and P can
!C  be taken to be the unit matrix.
!C
!C  beta is overwritten on alpha and z is overwritten on x.
!C  the routine may be called with  n = 0  and advantage is taken of the
!C  case where  n = 1.
!C
!C
!C  Nag Fortran 77 O( n ) basic linear algebra routine.
!C
!C  -- Written on 30-August-1984.
!C     Sven Hammarling, Nag Central Office.
!C     This version dated 28-September-1984.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!C     .. Local Scalars ..
      DOUBLE PRECISION   BETA, EPS, SCALE, SSQ
      LOGICAL            FIRST
!C     .. External Functions ..
      DOUBLE PRECISION   X02AJF
      EXTERNAL           X02AJF
!C     .. External Subroutines ..
      EXTERNAL           F06FJF, DSCAL
!C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN, SQRT
!C     .. Save statement ..
      SAVE               EPS, FIRST
!C     .. Data statements ..
      DATA               FIRST/ .TRUE. /
!C     ..
!C     .. Executable Statements ..
      IF( N.LT.1 )THEN
         ZETA = ZERO
      ELSE IF( ( N.EQ.1 ).AND.( X( 1 ).EQ.ZERO ) )THEN
         ZETA = ZERO
      ELSE
!C
         IF( FIRST )THEN
            FIRST = .FALSE.
            EPS   =  X02AJF( )
         END IF
!C
!C        Treat case where P is a 2 by 2 matrix specially.
!C
         IF( N.EQ.1 )THEN
!C
!C           Deal with cases where  ALPHA = zero  and
!C           abs( X( 1 ) ) .le. max( EPS*abs( ALPHA ), TOL )  first.
!C
            IF( ALPHA.EQ.ZERO )THEN
               ZETA   =  ONE
               ALPHA  =  ABS ( X( 1 ) )
               X( 1 ) = -SIGN( ONE, X( 1 ) )
            ELSE IF( ABS( X( 1 ) ).LE.MAX( EPS*ABS( ALPHA ), TOL ) )THEN
               ZETA   =  ZERO
            ELSE
               IF( ABS( ALPHA ).GE.ABS( X( 1 ) ) )THEN
                  BETA = ABS( ALPHA ) *SQRT( 1 + ( X( 1 )/ALPHA )**2 )
               ELSE
                  BETA = ABS( X( 1 ) )*SQRT( 1 + ( ALPHA/X( 1 ) )**2 )
               END IF
               ZETA = SQRT( ( ABS( ALPHA ) + BETA )/BETA )
               IF( ALPHA.GE.ZERO )
     $            BETA = -BETA
               X( 1 ) = -X( 1 )/( ZETA*BETA )
               ALPHA  = BETA
            END IF
         ELSE
!C
!C           Now P is larger than 2 by 2.
!C
            SSQ   = ONE
            SCALE = ZERO
            CALL F06FJF( N, X, INCX, SCALE, SSQ )
!C
!C           Treat cases where  SCALE = zero,
!C           SCALE .le. max( EPS*abs( ALPHA ), TOL )  and
!C           ALPHA = zero  specially.
!C           Note that  SCALE = max( abs( X( i ) ) ).
!C
            IF( ( SCALE.EQ.ZERO ).OR.
     $          ( SCALE.LE.MAX( EPS*ABS( ALPHA ), TOL ) ) )THEN
               ZETA  = ZERO
            ELSE IF( ALPHA.EQ.ZERO )THEN
               ZETA  = ONE
               ALPHA = SCALE*SQRT( SSQ )
               CALL DSCAL( N, -1/ALPHA, X, INCX )
            ELSE
               IF( SCALE.LT.ABS( ALPHA ) )THEN
                  BETA = ABS( ALPHA )*SQRT( 1 + SSQ*( SCALE/ALPHA )**2 )
               ELSE
                  BETA = SCALE       *SQRT( SSQ +   ( ALPHA/SCALE )**2 )
               END IF
               ZETA = SQRT( ( BETA + ABS( ALPHA ) )/BETA )
               IF( ALPHA.GT.ZERO )
     $            BETA = -BETA
               CALL DSCAL( N, -1/( ZETA*BETA ), X, INCX )
               ALPHA = BETA
            END IF
         END IF
      END IF
!C
      RETURN
!C
!C     End of F06FRF. ( SGRFG )
!C
      END
!C***********************************************************************************************************************
      SUBROUTINE F06PMF( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
!C     MARK 13 RE-ISSUE. NAG COPYRIGHT 1988.
!C     .. Entry Points ..
      ENTRY      DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
!C     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, INCY, LDA, M, N
!C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
!C     ..
!C
!C  Purpose
!C  =======
!C
!C  DGER   performs the rank 1 operation
!C
!C     A := alpha*x*y' + A,
!C
!C  where alpha is a scalar, x is an m element vector, y is an n element
!C  vector and A is an m by n matrix.
!C
!C  Parameters
!C  ==========
!C
!C  M      - INTEGER.
!C           On entry, M specifies the number of rows of the matrix A.
!C           M must be at least zero.
!C           Unchanged on exit.
!C
!C  N      - INTEGER.
!C           On entry, N specifies the number of columns of the matrix A.
!C           N must be at least zero.
!C           Unchanged on exit.
!C
!C  ALPHA  - DOUBLE PRECISION.
!C           On entry, ALPHA specifies the scalar alpha.
!C           Unchanged on exit.
!C
!C  X      - DOUBLE PRECISION array of dimension at least
!C           ( 1 + ( m - 1 )*abs( INCX ) ).
!C           Before entry, the incremented array X must contain the m
!C           element vector x.
!C           Unchanged on exit.
!C
!C  INCX   - INTEGER.
!C           On entry, INCX specifies the increment for the elements of
!C           X. INCX must not be zero.
!C           Unchanged on exit.
!C
!C  Y      - DOUBLE PRECISION array of dimension at least
!C           ( 1 + ( n - 1 )*abs( INCY ) ).
!C           Before entry, the incremented array Y must contain the n
!C           element vector y.
!C           Unchanged on exit.
!C
!C  INCY   - INTEGER.
!C           On entry, INCY specifies the increment for the elements of
!C           Y. INCY must not be zero.
!C           Unchanged on exit.
!C
!C  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!C           Before entry, the leading m by n part of the array A must
!C           contain the matrix of coefficients. On exit, A is
!C           overwritten by the updated matrix.
!C
!C  LDA    - INTEGER.
!C           On entry, LDA specifies the first dimension of A as declared
!C           in the calling (sub) program. LDA must be at least
!C           max( 1, m ).
!C           Unchanged on exit.
!C
!C
!C  Level 2 Blas routine.
!C
!C  -- Written on 22-October-1986.
!C     Jack Dongarra, Argonne National Lab.
!C     Jeremy Du Croz, Nag Central Office.
!C     Sven Hammarling, Nag Central Office.
!C     Richard Hanson, Sandia National Labs.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
!C     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JY, KX
!C     .. External Subroutines ..
      EXTERNAL           F06AAZ
!C     .. Intrinsic Functions ..
      INTRINSIC          MAX
!C     ..
!C     .. Executable Statements ..
!C
!C     Test the input parameters.
!C
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL F06AAZ( 'F06PMF/DGER  ', INFO )
         RETURN
      END IF
!C
!C     Quick return if possible.
!C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
!C
!C     Start the operations. In this version the elements of A are
!C     accessed sequentially with one pass through A.
!C
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
!C
      RETURN
!C
!C     End of F06PMF (DGER  ).
!C
      END
!C******************************************************************************************************************
      SUBROUTINE F06QHF( MATRIX, M, N, CONST, DIAG, A, LDA )
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C     .. Scalar Arguments ..
      CHARACTER*1        MATRIX
      DOUBLE PRECISION   CONST, DIAG
      INTEGER            LDA, M, N
!C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
!C     ..
!C
!C  F06QHF forms the m by n matrix A given by
!C
!C     a( i, j ) = (  diag  i.eq.j,
!C                 (
!C                 ( const  i.ne.j.
!C
!C  If   MATRIX = 'G' or 'g'   then  A  is regarded  as a general matrix,
!C  if   MATRIX = 'U' or 'u'   then  A  is regarded  as upper triangular,
!C                             and only  elements  for which  i.le.j  are
!C                             referenced,
!C  if   MATRIX = 'L' or 'l'   then  A  is regarded  as lower triangular,
!C                             and only  elements  for which  i.ge.j  are
!C                             referenced.
!C
!C
!C  Nag Fortran 77 O( n**2 ) basic linear algebra routine.
!C
!C  -- Written on 21-November-1986.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Local Scalars ..
      INTEGER            I, J
!C     .. Intrinsic Functions ..
      INTRINSIC          MIN
!C     ..
!C     .. Executable Statements ..
      IF( ( MATRIX.EQ.'G' ).OR.( MATRIX.EQ.'g' ) )THEN
         DO 20 J = 1, N
            DO 10 I = 1, M
               A( I, J ) = CONST
   10       CONTINUE
   20    CONTINUE
         IF( CONST.NE.DIAG )THEN
            DO 30 I = 1, MIN( M, N )
               A( I, I ) = DIAG
   30       CONTINUE
         END IF
      ELSE IF( ( MATRIX.EQ.'U' ).OR.( MATRIX.EQ.'u' ) )THEN
         DO 50 J = 1, N
            DO 40 I = 1, MIN( M, J )
               A( I, J ) = CONST
   40       CONTINUE
   50    CONTINUE
         IF( CONST.NE.DIAG )THEN
            DO 60 I = 1, MIN( M, N )
               A( I, I ) = DIAG
   60       CONTINUE
         END IF
      ELSE IF( ( MATRIX.EQ.'L' ).OR.( MATRIX.EQ.'l' ) )THEN
         DO 80 J = 1, MIN( M, N )
            DO 70 I = J, M
               A( I, J ) = CONST
   70       CONTINUE
   80    CONTINUE
         IF( CONST.NE.DIAG )THEN
            DO 90 I = 1, MIN( M, N )
               A( I, I ) = DIAG
   90       CONTINUE
         END IF
      END IF
!C
      RETURN
!C
!C     End of F06QHF. ( SMLOAD )
!C
      END
!C************************************************************************************************************************
      SUBROUTINE F06QKF( SIDE, TRANS, N, PERM, K, B, LDB )
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C     .. Scalar Arguments ..
      INTEGER            K, LDB, N
      CHARACTER*1        SIDE, TRANS
!C     .. Array Arguments ..
      DOUBLE PRECISION   PERM( * ), B( LDB, * )
!C     ..
!C
!C  Purpose
!C  =======
!C
!C  F06QKF performs one of the transformations
!C
!C     B := P'*B   or   B := P*B,   where B is an m by k matrix,
!C
!C  or
!C
!C     B := B*P'   or   B := B*P,   where B is a k by m matrix,
!C
!C  P being an m by m permutation matrix of the form
!C
!C     P = P( 1, index( 1 ) )*P( 2, index( 2 ) )*...*P( n, index( n ) ),
!C
!C  where  P( i, index( i ) ) is the permutation matrix that interchanges
!C  items i and index( i ). That is P( i, index( i ) ) is the unit matrix
!C  with rows and columns  i and  index( i )  interchanged. Of course, if
!C  index( i ) = i  then  P( i, index( i ) ) = I.
!C
!C  This  routine is intended  for use in conjunction with  Nag auxiliary
!C  routines  that  perform  interchange  operations,  such  as  sorting.
!C
!C  Parameters
!C  ==========
!C
!C  SIDE   - CHARACTER*1.
!C  TRANS
!C           On entry,  SIDE  ( Left-hand side, or Right-hand side )  and
!C           TRANS  ( Transpose, or No transpose )  specify the operation
!C           to be performed as follows.
!C
!C           SIDE = 'L' or 'l'   and   TRANS = 'T' or 't'
!C
!C              Perform the operation   B := P'*B.
!C
!C           SIDE = 'L' or 'l'   and   TRANS = 'N' or 'n'
!C
!C              Perform the operation   B := P*B.
!C
!C           SIDE = 'R' or 'r'   and   TRANS = 'T' or 't'
!C
!C              Perform the operation   B := B*P'.
!C
!C           SIDE = 'R' or 'r'   and   TRANS = 'N' or 'n'
!C
!C              Perform the operation   B := B*P.
!C
!C           Unchanged on exit.
!C
!C  N      - INTEGER.
!C
!C           On entry, N must specify the value of n.  N must be at least
!C           zero.  When  N = 0  then an  immediate  return  is effected.
!C
!C           Unchanged on exit.
!C
!C  PERM   - REAL             array of DIMENSION at least ( n ).
!C
!C           Before  entry,  PERM  must  contain  the  n indices  for the
!C           permutation matrices. index( i ) must satisfy
!C
!C              1 .le. index( i ) .le. m.
!C
!C           It is usual for index( i ) to be at least i, but this is not
!C           necessary for this routine. It is assumed that the statement
!C           INDEX = PERM( I )  returns the correct integer in  INDEX, so
!C           that,  if necessary,  PERM( I )  should contain a real value
!C           slightly larger than  INDEX.
!C
!C           Unchanged on exit.
!C
!C  K      - INTEGER.
!C
!C           On entry with  SIDE = 'L' or 'l',  K must specify the number
!C           of columns of B and on entry with  SIDE = 'R' or 'r', K must
!C           specify the number of rows of  B.  K must be at least  zero.
!C           When  K = 0  then an immediate return is effected.
!C
!C           Unchanged on exit.
!C
!C  B      - REAL  array  of  DIMENSION ( LDB, ncolb ),  where  ncolb = k
!C           when  SIDE = 'L' or 'l'  and  ncolb = m  when  SIDE = 'R' or
!C           'r'.
!C
!C           Before entry  with  SIDE = 'L' or 'l',  the  leading  m by K
!C           part  of  the  array   B  must  contain  the  matrix  to  be
!C           transformed  and before  entry with  SIDE = 'R' or 'r',  the
!C           leading  K by m part of the array  B must contain the matrix
!C           to  be  transformed.  On exit,   B  is  overwritten  by  the
!C           transformed matrix.
!C
!C  LDB    - INTEGER.
!C
!C           On entry,  LDB  must specify  the  leading dimension  of the
!C           array  B  as declared  in the  calling  (sub) program.  When
!C           SIDE = 'L' or 'l'   then  LDB  must  be  at  least  m,  when
!C           SIDE = 'R' or 'r'   then  LDB  must  be  at  least  k.
!C           Unchanged on exit.
!C
!C
!C  Nag Fortran 77 O( n**2 ) basic linear algebra routine.
!C
!C  -- Written on 11-August-1987.
!C     Sven Hammarling, Nag Central Office.
!C
!C
!C     .. Local Scalars ..
      LOGICAL            LEFT, NULL, RIGHT, TRNSP
      INTEGER            I, J, L
      DOUBLE PRECISION   TEMP
!C     .. Intrinsic Functions ..
      INTRINSIC          MIN
!C     ..
!C     .. Executable Statements ..
      IF( MIN( N, K ).EQ.0 )
     $   RETURN
      LEFT = ( SIDE.EQ.'L' ).OR.( SIDE.EQ.'l' )
      RIGHT = ( SIDE.EQ.'R' ).OR.( SIDE.EQ.'r' )
      NULL = ( TRANS.EQ.'N' ).OR.( TRANS.EQ.'n' )
      TRNSP = ( TRANS.EQ.'T' ).OR.( TRANS.EQ.'t' )
      IF( LEFT )THEN
         IF( TRNSP )THEN
            DO 20 I = 1, N
               L = PERM( I )
               IF( L.NE.I )THEN
                  DO 10 J = 1, K
                     TEMP = B( I, J )
                     B( I, J ) = B( L, J )
                     B( L, J ) = TEMP
   10             CONTINUE
               END IF
   20       CONTINUE
         ELSE IF( NULL )THEN
            DO 40 I = N, 1, -1
               L = PERM( I )
               IF( L.NE.I )THEN
                  DO 30 J = 1, K
                     TEMP = B( L, J )
                     B( L, J ) = B( I, J )
                     B( I, J ) = TEMP
   30             CONTINUE
               END IF
   40       CONTINUE
         END IF
      ELSE IF( RIGHT )THEN
         IF( TRNSP )THEN
            DO 60 J = N, 1, -1
               L = PERM( J )
               IF( L.NE.J )THEN
                  DO 50 I = 1, K
                     TEMP = B( I, J )
                     B( I, J ) = B( I, L )
                     B( I, L ) = TEMP
   50             CONTINUE
               END IF
   60       CONTINUE
         ELSE IF( NULL )THEN
            DO 80 J = 1, N
               L = PERM( J )
               IF( L.NE.J )THEN
                  DO 70 I = 1, K
                     TEMP = B( I, L )
                     B( I, L ) = B( I, J )
                     B( I, J ) = TEMP
   70             CONTINUE
               END IF
   80       CONTINUE
         END IF
      END IF
!C
      RETURN
!C
!C     End of F06QKF. ( SGEAPR )
!C
      END
!C**************************************************************************************************************
      SUBROUTINE F06QTF( SIDE, N, K1, K2, C, S, A, LDA )
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C     .. Scalar Arguments ..
      INTEGER            K1, K2, LDA, N
      CHARACTER*1        SIDE
!C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( * ), S( * )
!C     ..
!C
!C  F06QTF performs the transformation
!C
!C     R := P*U*Q'  when  SIDE = 'L' or 'l'  (  Left-hand side )
!C
!C     R := Q*U*P'  when  SIDE = 'R' or 'r'  ( Right-hand side ),
!C
!C  where  U and R  are  n by n  upper  triangular  matrices,   P  is  an
!C  orthogonal matrix,  consisting of a given sequence of plane rotations
!C  to be  applied  in  planes  k1 to k2,  and  Q  is  a  unitary  matrix
!C  consisting of a sequence of plane rotations, applied in planes  k1 to
!C  k2,  chosen to make  R  upper triangular.
!C
!C  When  SIDE = 'L' or 'l'  then  P  is  given  as a  sequence of  plane
!C  rotation matrices
!C
!C     P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ),
!C
!C  where  P( k ) is a plane rotation matrix for the  ( k, k + 1 ) plane.
!C  In this case the matrix Q is given as
!C
!C     Q = Q( k2 - 1 )*...*Q( k1 + 1 )*Q( k1 ),
!C
!C  where  Q( k ) is a plane rotation matrix for the  ( k, k + 1 ) plane.
!C
!C  When  SIDE = 'R' or 'r'  then  P  is  given  as a  sequence of  plane
!C  rotation matrices
!C
!C     P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ),
!C
!C  where  P( k ) is a plane rotation matrix for the  ( k, k + 1 ) plane.
!C  In this case the matrix Q is given as
!C
!C     Q = Q( k1 )*Q( k1 + 1 )*...*Q( k2 - 1 ),
!C
!C  where  Q( k ) is a plane rotation matrix for the  ( k, k + 1 ) plane.
!C
!C  The  upper  triangular  matrix  U  must  be  supplied  in the  n by n
!C  leading upper triangular part of  A,  and this  is overwritten by the
!C  upper triangular matrix  R.  The cosine  and  sine  that  define  the
!C  plane rotation matrix  P( k )  must be supplied in  c( k ) and s( k )
!C  respectively,  and  the two by two rotation part of  P( k ),  T( k ),
!C  is assumed to be of the form
!C
!C     T( k ) = (  c( k )  s( k ) ).
!C              ( -s( k )  c( k ) )
!C
!C  The cosine  and  sine that define  Q( k )  are overwritten on  c( k )
!C  and  s( k )  respectively and the two by two rotation part of  Q( k )
!C  will have the form of  T( k )  above.
!C
!C  If  n or k1  are less  than  unity, or  k1  is not  less than  k2, or
!C  k2  is greater than  n  then an immediate return is effected.
!C
!C
!C  Nag Fortran 77 O( n**2 ) basic linear algebra routine.
!C
!C  -- Written on 26-November-1987.
!C     Sven Hammarling and Mick Pont, Nag Central Office.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!C     .. Local Scalars ..
      DOUBLE PRECISION   AIJ, CTEMP, FILL, STEMP, TEMP
      INTEGER            I, I1, J
!C     .. External Subroutines ..
      EXTERNAL           F06BAF
!C     .. Intrinsic Functions ..
      INTRINSIC          MIN
!C     ..
!C     .. Executable Statements ..
      IF( ( MIN( N, K1 ).LT.1 ).OR.( K2.LE.K1 ).OR.
     $   ( K2.GT.N ) )RETURN
      IF( ( SIDE.EQ.'L' ).OR.( SIDE.EQ.'l' ) )THEN
!C
!C        Apply the left-hand transformations,  column by column,  to the
!C        triangular part of  U,  but not to  anywhere  that would  cause
!C        fill.
!C
         DO 20 J = K1 + 1, N
!C
!C           Apply  P( k1 ) ... P( j - 1 )  to column j.
!C
            AIJ = A( K1, J )
            DO 10 I = K1, MIN( J - 1, K2 - 1 )
               A( I, J ) = S( I )*A( I + 1, J ) + C( I )*AIJ
               AIJ = C( I )*A( I + 1, J ) - S( I )*AIJ
   10       CONTINUE
            A( I, J ) = AIJ
   20    CONTINUE
!C
!C           Now apply each  left-hand tranformation  to form the fill-in
!C           elements and apply a  right-hand transformation to eliminate
!C           the fill-in element.
!C
         DO 40 J = K1, K2 - 1
!C
!C           Apply  P( j )  to the jth diagonal element  and the  fill-in
!C           position.
!C
            FILL = -S( J )*A( J, J )
            A( J, J ) = C( J )*A( J, J )
!C
!C           Now  set up  the rotation  Q( j )  to eliminate the  fill-in
!C           element,  and  apply  Q( j )  to  the  jth  and  ( j + 1 )th
!C           columns.
!C
            CALL F06BAF( A( J + 1, J + 1 ), FILL, CTEMP, STEMP )
            C( J ) = CTEMP
            S( J ) = -STEMP
            IF( ( CTEMP.NE.ONE ).OR.( STEMP.NE.ZERO ) )THEN
               STEMP = -STEMP
               DO 30 I = 1, J
                  TEMP = A( I, J + 1 )
                  A( I, J + 1 ) = CTEMP*TEMP - STEMP*A( I, J )
                  A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
   30          CONTINUE
            END IF
   40    CONTINUE
      ELSE IF( ( SIDE.EQ.'R' ).OR.( SIDE.EQ.'r' ) )THEN
!C
!C        We intermingle the  left and right hand transformations so that
!C        at the kth step we form
!C
!C           A := Q( k )*A*P( k )'.
!C
!C        First  apply  the  transformations  in  columns  k2 back to k1.
!C
         DO 60 J = K2 - 1, K1, -1
!C
!C           First apply  P( j ).
!C
            IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
               CTEMP = C( J )
               STEMP = S( J )
               DO 50 I = 1, J
                  TEMP = A( I, J + 1 )
                  A( I, J + 1 ) = CTEMP*TEMP - STEMP*A( I, J )
                  A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
   50          CONTINUE
!C
!C              Next form the fill-in element  a( j + 1, j )  by applying
!C              P( j ).
!C
               FILL = S( J )*A( J + 1, J + 1 )
               A( J + 1, J + 1 ) = C( J )*A( J + 1, J + 1 )
!C
!C              Now set up the rotation  Q( j )  to eliminate the fill-in
!C              element.
!C
               CALL F06BAF( A( J, J ), FILL, C( J ), S( J ) )
            END IF
   60    CONTINUE
!C
!C        Finally  apply  Q( k2 - 1 ) ... Q( k1 )  to columns  n  back to
!C        ( k1 + 1 ).
!C
         DO 80 J = N, K1 + 1, -1
            I1 = MIN( K2, J )
            AIJ = A( I1, J )
            DO 70 I = I1 - 1, K1, -1
               TEMP = A( I, J )
               A( I + 1, J ) = C( I )*AIJ - S( I )*TEMP
               AIJ = S( I )*AIJ + C( I )*TEMP
   70       CONTINUE
            A( K1, J ) = AIJ
   80    CONTINUE
      END IF
      RETURN
!C
!C     End of F06QTF. ( SUTSQR )
!C
      END
!C***************************************************************************************************************
      SUBROUTINE F06QXF( SIDE, PIVOT, DIRECT, M, N, K1, K2, C, S, A,
     $                   LDA )
!C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
!C     .. Scalar Arguments ..
      INTEGER            K1, K2, LDA, M, N
      CHARACTER*1        DIRECT, PIVOT, SIDE
!C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( * ), S( * )
!C     ..
!C
!C  F06QXF  performs the transformation
!C
!C     A := P*A,   when   SIDE = 'L' or 'l'  (  Left-hand side )
!C
!C     A := A*P',  when   SIDE = 'R' or 'r'  ( Right-hand side )
!C
!C  where A is an m by n matrix and P is an orthogonal matrix, consisting
!C  of a  sequence  of  plane  rotations,  applied  in  planes  k1 to k2,
!C  determined by the parameters PIVOT and DIRECT as follows:
!C
!C     When  PIVOT  = 'V' or 'v'  ( Variable pivot )
!C     and   DIRECT = 'F' or 'f'  ( Forward sequence ) then
!C
!C        P is given as a sequence of plane rotation matrices
!C
!C           P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ),
!C
!C        where  P( k )  is a plane rotation matrix for the  ( k, k + 1 )
!C        plane.
!C
!C     When  PIVOT  = 'V' or 'v'  ( Variable pivot )
!C     and   DIRECT = 'B' or 'b'  ( Backward sequence ) then
!C
!C        P is given as a sequence of plane rotation matrices
!C
!C           P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ),
!C
!C        where  P( k )  is a plane rotation matrix for the  ( k, k + 1 )
!C        plane.
!C
!C     When  PIVOT  = 'T' or 't'  ( Top pivot )
!C     and   DIRECT = 'F' or 'f'  ( Forward sequence ) then
!C
!C        P is given as a sequence of plane rotation matrices
!C
!C           P = P( k2 - 1 )*P( k2 - 2 )*...*P( k1 ),
!C
!C        where  P( k )  is a plane rotation matrix for the ( k1, k + 1 )
!C        plane.
!C
!C     When  PIVOT  = 'T' or 't'  ( Top pivot )
!C     and   DIRECT = 'B' or 'b'  ( Backward sequence ) then
!C
!C        P is given as a sequence of plane rotation matrices
!C
!C           P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ),
!C
!C        where  P( k )  is a plane rotation matrix for the ( k1, k + 1 )
!C        plane.
!C
!C     When  PIVOT  = 'B' or 'b'  ( Bottom pivot )
!C     and   DIRECT = 'F' or 'f'  ( Forward sequence ) then
!C
!C        P is given as a sequence of plane rotation matrices
!C
!C           P = P( k2 - 1 )*P( k2 - 2 )*...*P( k1 ),
!C
!C        where  P( k )  is a  plane rotation  matrix  for the  ( k, k2 )
!C        plane.
!C
!C     When  PIVOT  = 'B' or 'b'  ( Bottom pivot )
!C     and   DIRECT = 'B' or 'b'  ( Backward sequence ) then
!C
!C        P is given as a sequence of plane rotation matrices
!C
!C           P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ),
!C
!C        where  P( k )  is a  plane rotation  matrix  for the  ( k, k2 )
!C        plane.
!C
!C  c( k ) and s( k )  must contain the  cosine and sine  that define the
!C  matrix  P( k ).  The  two by two  plane rotation  part of the  matrix
!C  P( k ), R( k ), is assumed to be of the form
!C
!C     R( k ) = (  c( k )  s( k ) ).
!C              ( -s( k )  c( k ) )
!C
!C  If m, n or k1 are less than unity,  or k2 is not greater than k1,  or
!C  SIDE = 'L' or 'l'  and  k2  is greater than  m, or  SIDE = 'R' or 'r'
!C  and  k2  is greater than  n,  then an  immediate return  is effected.
!C
!C
!C  Nag Fortran 77 O( n**2 ) basic linear algebra routine.
!C
!C  -- Written on 20-November-1986.
!C     Sven Hammarling and Mick Pont, Nag Central Office.
!C
!C
!C     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!C     .. Local Scalars ..
      DOUBLE PRECISION   AIJ, CTEMP, STEMP, TEMP
      INTEGER            I, J
      LOGICAL            LEFT, RIGHT
!C     .. Intrinsic Functions ..
      INTRINSIC          MIN
!C     ..
!C     .. Executable Statements ..
      LEFT = ( SIDE.EQ.'L' ).OR.( SIDE.EQ.'l' )
      RIGHT = ( SIDE.EQ.'R' ).OR.( SIDE.EQ.'r' )
      IF( ( MIN( M, N, K1 ).LT.1 ).OR.( K2.LE.K1 ).OR.
     $    ( ( LEFT ).AND.( K2.GT.M ) ).OR.
     $    ( ( RIGHT ).AND.( K2.GT.N ) ) )RETURN
      IF( LEFT )THEN
         IF( ( PIVOT.EQ.'V' ).OR.( PIVOT.EQ.'v' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 20 J = 1, N
                  AIJ = A( K1, J )
                  DO 10 I = K1, K2 - 1
                     TEMP = A( I + 1, J )
                     A( I, J ) = S( I )*TEMP + C( I )*AIJ
                     AIJ = C( I )*TEMP - S( I )*AIJ
   10             CONTINUE
                  A( K2, J ) = AIJ
   20          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 40 J = 1, N
                  AIJ = A( K2, J )
                  DO 30 I = K2 - 1, K1, -1
                     TEMP = A( I, J )
                     A( I + 1, J ) = C( I )*AIJ - S( I )*TEMP
                     AIJ = S( I )*AIJ + C( I )*TEMP
   30             CONTINUE
                  A( K1, J ) = AIJ
   40          CONTINUE
            END IF
         ELSE IF( ( PIVOT.EQ.'T' ).OR.( PIVOT.EQ.'t' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 60 J = 1, N
                  TEMP = A( K1, J )
                  DO 50 I = K1, K2 - 1
                     AIJ = A( I + 1, J )
                     A( I + 1, J ) = C( I )*AIJ - S( I )*TEMP
                     TEMP = S( I )*AIJ + C( I )*TEMP
   50             CONTINUE
                  A( K1, J ) = TEMP
   60          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 80 J = 1, N
                  TEMP = A( K1, J )
                  DO 70 I = K2 - 1, K1, -1
                     AIJ = A( I + 1, J )
                     A( I + 1, J ) = C( I )*AIJ - S( I )*TEMP
                     TEMP = S( I )*AIJ + C( I )*TEMP
   70             CONTINUE
                  A( K1, J ) = TEMP
   80          CONTINUE
            END IF
         ELSE IF( ( PIVOT.EQ.'B' ).OR.( PIVOT.EQ.'b' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 100 J = 1, N
                  TEMP = A( K2, J )
                  DO 90 I = K1, K2 - 1
                     AIJ = A( I, J )
                     A( I, J ) = S( I )*TEMP + C( I )*AIJ
                     TEMP = C( I )*TEMP - S( I )*AIJ
   90             CONTINUE
                  A( K2, J ) = TEMP
  100          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 120 J = 1, N
                  TEMP = A( K2, J )
                  DO 110 I = K2 - 1, K1, -1
                     AIJ = A( I, J )
                     A( I, J ) = S( I )*TEMP + C( I )*AIJ
                     TEMP = C( I )*TEMP - S( I )*AIJ
  110             CONTINUE
                  A( K2, J ) = TEMP
  120          CONTINUE
            END IF
         END IF
      ELSE IF( RIGHT )THEN
         IF( ( PIVOT.EQ.'V' ).OR.( PIVOT.EQ.'v' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 140 J = K1, K2 - 1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CTEMP = C( J )
                     STEMP = S( J )
                     DO 130 I = 1, M
                        TEMP = A( I, J + 1 )
                        A( I, J + 1 ) = CTEMP*TEMP - STEMP*A( I, J )
                        A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
  130                CONTINUE
                  END IF
  140          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 160 J = K2 - 1, K1, -1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CTEMP = C( J )
                     STEMP = S( J )
                     DO 150 I = M, 1, -1
                        TEMP = A( I, J + 1 )
                        A( I, J + 1 ) = CTEMP*TEMP - STEMP*A( I, J )
                        A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
  150                CONTINUE
                  END IF
  160          CONTINUE
            END IF
         ELSE IF( ( PIVOT.EQ.'T' ).OR.( PIVOT.EQ.'t' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 180 J = K1 + 1, K2
                  CTEMP = C( J - 1 )
                  STEMP = S( J - 1 )
                  IF( ( CTEMP.NE.ONE ).OR.( STEMP.NE.ZERO ) )THEN
                     DO 170 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = CTEMP*TEMP - STEMP*A( I, K1 )
                        A( I, K1 ) = STEMP*TEMP + CTEMP*A( I, K1 )
  170                CONTINUE
                  END IF
  180          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 200 J = K2, K1 + 1, -1
                  CTEMP = C( J - 1 )
                  STEMP = S( J - 1 )
                  IF( ( CTEMP.NE.ONE ).OR.( STEMP.NE.ZERO ) )THEN
                     DO 190 I = M, 1, -1
                        TEMP = A( I, J )
                        A( I, J ) = CTEMP*TEMP - STEMP*A( I, K1 )
                        A( I, K1 ) = STEMP*TEMP + CTEMP*A( I, K1 )
  190                CONTINUE
                  END IF
  200          CONTINUE
            END IF
         ELSE IF( ( PIVOT.EQ.'B' ).OR.( PIVOT.EQ.'b' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 220 J = K1, K2 - 1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CTEMP = C( J )
                     STEMP = S( J )
                     DO 210 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = STEMP*A( I, K2 ) + CTEMP*TEMP
                        A( I, K2 ) = CTEMP*A( I, K2 ) - STEMP*TEMP
  210                CONTINUE
                  END IF
  220          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 240 J = K2 - 1, K1, -1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CTEMP = C( J )
                     STEMP = S( J )
                     DO 230 I = M, 1, -1
                        TEMP = A( I, J )
                        A( I, J ) = STEMP*A( I, K2 ) + CTEMP*TEMP
                        A( I, K2 ) = CTEMP*A( I, K2 ) - STEMP*TEMP
  230                CONTINUE
                  END IF
  240          CONTINUE
            END IF
         END IF
      END IF
!C
      RETURN
!C
!C     End of F06QXF. ( SGESRC )
!C
      END
!C******************************************************************************************************
      INTEGER FUNCTION P01ABF(IFAIL,IERROR,SRNAME,NREC,REC)
!C     MARK 11.5(F77) RELEASE. NAG COPYRIGHT 1986.
!C     MARK 13 REVISED. IER-621 (APR 1988).
!C     MARK 13B REVISED. IER-668 (AUG 1988).
!C
!C     P01ABF is the error-handling routine for the NAG Library.
!C
!C     P01ABF either returns the value of IERROR through the routine
!C     name (soft failure), or terminates execution of the program
!C     (hard failure). Diagnostic messages may be output.
!C
!C     If IERROR = 0 (successful exit from the calling routine),
!C     the value 0 is returned through the routine name, and no
!C     message is output
!C
!C     If IERROR is non-zero (abnormal exit from the calling routine),
!C     the action taken depends on the value of IFAIL.
!C
!C     IFAIL =  1: soft failure, silent exit (i.e. no messages are
!C                 output)
!C     IFAIL = -1: soft failure, noisy exit (i.e. messages are output)
!C     IFAIL =-13: soft failure, noisy exit but standard messages from
!C                 P01ABF are suppressed
!C     IFAIL =  0: hard failure, noisy exit
!C
!C     For compatibility with certain routines included before Mark 12
!C     P01ABF also allows an alternative specification of IFAIL in which
!C     it is regarded as a decimal integer with least significant digits
!C     cba. Then
!C
!C     a = 0: hard failure  a = 1: soft failure
!C     b = 0: silent exit   b = 1: noisy exit
!C
!C     except that hard failure now always implies a noisy exit.
!C
!C     S.Hammarling, M.P.Hooper and J.J.du Croz, NAG Central Office.
!C
!C     .. Scalar Arguments ..
      INTEGER                 IERROR, IFAIL, NREC
      CHARACTER*(*)           SRNAME
!C     .. Array Arguments ..
      CHARACTER*(*)           REC(*)
!C     .. Local Scalars ..
      INTEGER                 I, NERR
      CHARACTER*72            MESS
!C     .. External Subroutines ..
      EXTERNAL                P01ABZ, X04AAF, X04BAF
!C     .. Intrinsic Functions ..
      INTRINSIC               ABS, MOD
!C     .. Executable Statements ..
      IF (IERROR.NE.0) THEN
!C        Abnormal exit from calling routine
         IF (IFAIL.EQ.-1 .OR. IFAIL.EQ.0 .OR. IFAIL.EQ.-13 .OR.
     *       (IFAIL.GT.0 .AND. MOD(IFAIL/10,10).NE.0)) THEN
!C           Noisy exit
            CALL X04AAF(0,NERR)
            DO 20 I = 1, NREC
               CALL X04BAF(NERR,REC(I))
   20       CONTINUE
            IF (IFAIL.NE.-13) THEN
               WRITE (MESS,FMT=99999) SRNAME, IERROR
               CALL X04BAF(NERR,MESS)
               IF (ABS(MOD(IFAIL,10)).NE.1) THEN
!C                 Hard failure
                  CALL X04BAF(NERR,
     *                     ' ** NAG hard failure - execution terminated'
     *                        )
                  CALL P01ABZ
               ELSE
!C                 Soft failure
                  CALL X04BAF(NERR,
     *                        ' ** NAG soft failure - control returned')
               END IF
            END IF
         END IF
      END IF
      P01ABF = IERROR
      RETURN
!C
99999 FORMAT (' ** ABNORMAL EXIT from NAG Library routine ',A,': IFAIL',
     *  ' =',I6)
      END
!C******************************************************************************************************
      SUBROUTINE P01ABY(N,NAME,INFORM,IERR,SRNAME)
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C
!C     P01ABY increases the value of IERR by 1 and, if
!C
!C        ( mod( INFORM, 10 ).ne.1 ).or.( mod( INFORM/10, 10 ).ne.0 )
!C
!C     writes a message on the current error message channel giving the
!C     value of N, a message to say that N is invalid and the strings
!C     NAME and SRNAME.
!C
!C     NAME must be the name of the actual argument for N and SRNAME must
!C     be the name of the calling routine.
!C
!C     This routine is intended for use when N is an invalid input
!C     parameter to routine SRNAME. For example
!C
!C        IERR = 0
!C        IF( N.LT.1 )CALL P01ABY( N, 'N', IDIAG, IERR, SRNAME )
!C
!C  -- Written on 23-February-1984.  Sven.
!C
!C     .. Scalar Arguments ..
      INTEGER           IERR, INFORM, N
      CHARACTER*(*)     NAME, SRNAME
!C     .. Local Scalars ..
      INTEGER           NERR
!C     .. Local Arrays ..
      CHARACTER*65      REC(2)
!C     .. External Subroutines ..
      EXTERNAL          X04AAF, X04BAF
!C     .. Intrinsic Functions ..
      INTRINSIC         MOD
!C     .. Executable Statements ..
      IERR = IERR + 1
      IF ((MOD(INFORM,10).NE.1) .OR. (MOD(INFORM/10,10).NE.0)) THEN
         CALL X04AAF(0,NERR)
         WRITE (REC,FMT=99999) NAME, SRNAME, N
         CALL X04BAF(NERR,' ')
         CALL X04BAF(NERR,REC(1))
         CALL X04BAF(NERR,REC(2))
      END IF
      RETURN
!C
!C
!C     End of P01ABY.
!C
99999 FORMAT (' *****  Parameter  ',A,'  is invalid in routine  ',A,
     *  '  ***** ',/8X,'Value supplied is ',I6)
      END
!C******************************************************************************************************************
      SUBROUTINE P01ABW(N,NAME,INFORM,IERR,SRNAME)
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C
!C     P01ABW increases the value of IERR by 1 and, if
!C
!C        ( mod( INFORM, 10 ).ne.1 ).or.( mod( INFORM/10, 10 ).ne.0 )
!C
!C     writes a message on the current error message channel giving the
!C     value of N, a message to say that N is invalid and the strings
!C     NAME and SRNAME.
!C
!C     NAME must be the name of the actual argument for N and SRNAME must
!C     be the name of the calling routine.
!C
!C     This routine is intended for use when N is an invalid input
!C     parameter to routine SRNAME. For example
!C
!C        IERR = 0
!C        IF( N.NE.'Valid value' )
!C     $     CALL P01ABW( N, 'N', IDIAG, IERR, SRNAME )
!C
!C  -- Written on 15-November-1984.
!C     Sven Hammarling, Nag Central Office.
!C
!C     .. Scalar Arguments ..
      INTEGER           IERR, INFORM
      CHARACTER*(*)     N
      CHARACTER*(*)     NAME, SRNAME
!C     .. Local Scalars ..
      INTEGER           NERR
!C     .. Local Arrays ..
      CHARACTER*65      REC(3)
!C     .. External Subroutines ..
      EXTERNAL          X04AAF, X04BAF
!C     .. Intrinsic Functions ..
      INTRINSIC         MOD
!C     .. Executable Statements ..
      IERR = IERR + 1
      IF ((MOD(INFORM,10).NE.1) .OR. (MOD(INFORM/10,10).NE.0)) THEN
         CALL X04AAF(0,NERR)
         WRITE (REC,FMT=99999) NAME, SRNAME, N
         CALL X04BAF(NERR,' ')
         CALL X04BAF(NERR,REC(1))
         CALL X04BAF(NERR,REC(2))
         CALL X04BAF(NERR,REC(3))
      END IF
      RETURN
!C
!C
!C     End of P01ABW.
!C
99999 FORMAT (' *****  Parameter  ',A,'  is invalid in routine  ',A,
     *  '  ***** ',/8X,'Value supplied is',/8X,A)
      END
!C*****************************************************************************************************************************
      SUBROUTINE P01ABZ
!C     MARK 11.5(F77) RELEASE. NAG COPYRIGHT 1986.
!C
!C     Terminates execution when a hard failure occurs.
!C
!C     ******************** IMPLEMENTATION NOTE ********************
!C     The following STOP statement may be replaced by a call to an
!C     implementation-dependent routine to display a message and/or
!C     to abort the program.
!C     *************************************************************
!C     .. Executable Statements ..
      STOP
      END
!C*********************************************************************************************************************************
      DOUBLE PRECISION FUNCTION X02AJF()
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C
!C     RETURNS  (1/2)*B**(1-P)  IF ROUNDS IS .TRUE.
!C     RETURNS  B**(1-P)  OTHERWISE
!C
!C     .. Executable Statements ..
!C     IN THEORY THIS SHOULD BE 2.0**(-56) BUT 2.0**(-55) HAS BEEN FOUND
!C     TO BE MORE PRACTICAL IN THE PAST.
      X02AJF = 2.0D0**(-55)
      RETURN
      END
!C***************************************************************************************************************************************
      DOUBLE PRECISION FUNCTION X02AKF()
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C
!C     RETURNS  B**(EMIN-1)  (THE SMALLEST POSITIVE MODEL NUMBER)
!C
!C     .. Executable Statements ..
      X02AKF = 0.25 * 2.0D0**(-126)
      RETURN
      END
!C******************************************************************************************************************************************
      DOUBLE PRECISION FUNCTION X02AMF()
!C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
!C
!C     RETURNS THE 'SAFE RANGE' PARAMETER
!C     I.E. THE SMALLEST POSITIVE MODEL NUMBER Z SUCH THAT
!C     FOR ANY X WHICH SATISFIES X.GE.Z AND X.LE.1/Z
!C     THE FOLLOWING CAN BE COMPUTED WITHOUT OVERFLOW, UNDERFLOW OR OTHER
!C     ERROR
!C
!C        -X
!C        1.0/X
!C        SQRT(X)
!C        LOG(X)
!C        EXP(LOG(X))
!C        Y**(LOG(X)/LOG(Y)) FOR ANY Y
!C
!C     .. Executable Statements ..
      X02AMF = (0.5D0 + 2.0D0**(-52)) * 2.0D0**(-126)
      RETURN
      END
!C*********************************************************************************************************************************************
      SUBROUTINE X04AAF(I,NERR)
!C     MARK 7 RELEASE. NAG COPYRIGHT 1978
!C     MARK 7C REVISED IER-190 (MAY 1979)
!C     MARK 11.5(F77) REVISED. (SEPT 1985.)
!C     IF I = 0, SETS NERR TO CURRENT ERROR MESSAGE UNIT NUMBER
!C     (STORED IN NERR1).
!C     IF I = 1, CHANGES CURRENT ERROR MESSAGE UNIT NUMBER TO
!C     VALUE SPECIFIED BY NERR.
!C
!C     .. Scalar Arguments ..
      INTEGER           I, NERR
!C     .. Local Scalars ..
      INTEGER           NERR1
!C     .. Save statement ..
      SAVE              NERR1
!C     .. Data statements ..
      DATA              NERR1/6/
!C     .. Executable Statements ..
      IF (I.EQ.0) NERR = NERR1
      IF (I.EQ.1) NERR1 = NERR
      RETURN
      END
!C**********************************************************************************************************************************************
      SUBROUTINE X04BAF(NOUT,REC)
!C     MARK 11.5(F77) RELEASE. NAG COPYRIGHT 1986.
!C
!C     X04BAF writes the contents of REC to the unit defined by NOUT.
!C
!C     Trailing blanks are not output, except that if REC is entirely
!C     blank, a single blank character is output.
!C     If NOUT.lt.0, i.e. if NOUT is not a valid Fortran unit identifier,
!C     then no output occurs.
!C
!C     .. Scalar Arguments ..
      INTEGER           NOUT
      CHARACTER*(*)     REC
!C     .. Local Scalars ..
      INTEGER           I
!C     .. Intrinsic Functions ..
      INTRINSIC         LEN
!C     .. Executable Statements ..
      IF (NOUT.GE.0) THEN
!C        Remove trailing blanks
         DO 20 I = LEN(REC), 2, -1
            IF (REC(I:I).NE.' ') GO TO 40
   20    CONTINUE
!C        Write record to external file
   40    WRITE (NOUT,FMT=99999) REC(1:I)
      END IF
      RETURN
!C
99999 FORMAT (A)
      END
!C****************************************************************************************************************************************************
