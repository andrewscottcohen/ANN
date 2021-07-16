! *** Artificial Neural Network: Single Layer Perceptron
! *** Now Using TANH Function as T.F.
      PROGRAM ANN

! *** OI=TRAINING SET, TJ=TRAINING SET, OJ=OUTPUT SET, W=WEIGHTS, DELTA = SLOPE OF TRNSFR FUNCT
! *** INITIALIZE MATRICES

      REAL :: OI(4,3), TJ(4,1), OJ(4,1), W(3,1), DELTA(4,1) 
      REAL :: ERROR(1,1), PAST_ERROR(1,1)
      
      
      OI(1,:)=(/0,0,1 /)
      OI(2,:)=(/1,1,1 /)
      OI(3,:)=(/1,0,1 /)
      OI(4,:)=(/0,1,1 /)

      TJ(:,1)=(/0,1,1,0 /)

! *** OPEN FILES TO WRITE
      OPEN(1,FILE="tanh_error_time.out",STATUS='UNKNOWN')
      !OPEN(2,FILE="tanh_error.out",STATUS='UNKNOWN')
      !OPEN(3,FILE="tanh_time.out",STATUS='UNKNOWN')


! *** RANDOMIZE INTIAL WEIGHT BETWEEN -1 AND 1
      DO I=1,3
      W(I,:)=2.*RAND()-1
      END DO

! *** SET LEARNING RATE
      ALPHA=1
      CALL CPU_TIME(T_START)

! *** TRAINING EPOCHS
      DO J=1,100000

      CALL CPU_TIME(START)

      PAST_ERROR=0
      IF(J.GT.1)PAST_ERROR = (0.5)*(SUM((TJ-OJ)**2))

! *** FORWARD PROPOGATION USING SIGMOID T.F.
      OJ=TANH(MATMUL(OI,W))

! *** FIND DELTA - (y-t)*f'(x)=f(x)(1-f(x))
      DELTA = (OJ-TJ)*((1.-(OJ**2)))
      !DO K=1,4
      !IF(DELTA(K,1).GT.0)PRINT*, DELTA(K,1)
      !END DO

! *** END OF CPU_TIME
      CALL CPU_TIME(FINISH)
      TIME=FINISH-START

! *** CALCULATE ERROR
      ERROR = (0.5)*(SUM((TJ-OJ)**2))
      WRITE(1,*)J, ERROR, TIME
      !WRITE(2,*)ERROR
      !WRITE(3,*)TIME
      !PRINT*,OJ

! *** UPDATE WEIGHTS (BACKPROPAGATION)
      W = W-ALPHA*MATMUL(TRANSPOSE(OI),DELTA)
      IF((J.GT.90000).AND.((ERROR(1,1)-PAST_ERROR(1,1)).LT.0)) GOTO 3
      END DO !keep at it ;)
    3 CALL CPU_TIME(T_FINISH)
      T_TIME=T_FINISH-T_START
      !WRITE(1,*)"Alpha=",ALPHA,"Total Time=",T_TIME

! *** PRINT TRAINING OUTPUT RESULTS
      PRINT*,'Trained Outputs'
      PRINT*, OJ(1,1)
      PRINT*, OJ(2,1)
      PRINT*, OJ(3,1)
      PRINT*, OJ(4,1)
      PRINT*,"Alpha=",ALPHA,"Total Time=",T_TIME,"Epochs:",J

    2 PRINT*, 'Enter Data'
      WRITE(*,*) 'BYE TO EXIT'
      READ*, D1,D2,D3
      Z=D1*W(1,1)+D2*W(2,1)+D3*W(3,1)
      FINAL=TANH(Z)
      PRINT*, FINAL
      GO TO 2

      END PROGRAM ANN



