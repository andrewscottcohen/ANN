! *** Artificial Neural Network: Single Layer Perceptron

      PROGRAM ANN

! *** OI=TRAINING SET, TJ=TRAINING SET, OJ=OUTPUT SET, W=WEIGHTS, DELTA = SLOPE OF TRNSFR FUNCT
! *** INITIALIZE MATRICES

      REAL :: OI(4,3), TJ(4,1), OJ(4,1), W(3,1), DELTA(4,1), ERROR(1,1)
      
      
      OI(1,:)=(/0,0,1 /)
      OI(2,:)=(/1,1,1 /)
      OI(3,:)=(/1,0,1 /)
      OI(4,:)=(/0,1,1 /)

      TJ(:,1)=(/0,1,1,0 /)

! *** OPEN FILES TO WRITE
      OPEN(1,FILE="2error_time.out",STATUS='UNKNOWN')
      !OPEN(2,FILE="1.5error.out",STATUS='UNKNOWN')
      !OPEN(3,FILE="1.5time.out",STATUS='UNKNOWN')


! *** RANDOMIZE INTIAL WEIGHT BETWEEN -1 AND 1
      DO I=1,3
      W(I,:)=2.*RAND()-1
      END DO

! *** SET LEARNING RATE
      ALPHA=2

      CALL CPU_TIME(T_START)
! *** TRAINING EPOCHS
      DO J=1,100000

      CALL CPU_TIME(E_START)

! *** FORWARD PROPOGATION USING SIGMOID T.F.
      OJ=1./(1.+EXP(-MATMUL(OI,W)))

! *** FIND DELTA - f'(x)=f(x)(1-f(x))
      DELTA = (OJ-TJ)*(OJ*(1.-OJ))

! *** END OF CPU_TIME
      CALL CPU_TIME(E_FINISH)
      E_TIME=E_FINISH-E_START

! *** CALCULATE ERROR
      ERROR = (0.5)*(SUM((TJ-OJ)**2))
      WRITE(1,*)J, ERROR, E_TIME
      !WRITE(2,*)ERROR
      !WRITE(3,*)TIME
      !PRINT*,OJ

! *** UPDATE WEIGHTS (BACKPROPAGATION)
      W = W-ALPHA*MATMUL(TRANSPOSE(OI),DELTA)
      
      END DO !keep at it ;)
      CALL CPU_TIME(T_FINISH)
      T_TIME=T_FINISH-T_START
      WRITE(1,*)"Training Rate=", ALPHA, "Total Training Time=", T_TIME

! *** PRINT TRAINING OUTPUT RESULTS
      PRINT*,"Alpha is",ALPHA
      PRINT*,'Trained Outputs'
      PRINT*, OJ(1,1)
      PRINT*, OJ(2,1)
      PRINT*, OJ(3,1)
      PRINT*, OJ(4,1)


    2 PRINT*, 'Enter Data'
      WRITE(*,*) 'BYE TO EXIT'
      READ*, D1,D2,D3
      Z=D1*W(1,1)+D2*W(2,1)+D3*W(3,1)
      FINAL=1./(1.+EXP(-Z))
      !PRINT*, W(1,1),W(2,1),W(3,1)
      PRINT*, FINAL
      GO TO 2
      END PROGRAM ANN



