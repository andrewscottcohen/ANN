! *** Artificial Neural Network: Single Layer Perceptron
! *** Now Using TANH Function as T.F.
      PROGRAM XOR ANN

! *** OI=TRAINING SET, TJ=TRAINING SET, OJ=OUTPUT SET, W=WEIGHTS, DELTA = SLOPE OF TRNSFR FUNCT
! *** INITIALIZE MATRICES

      REAL :: OI(5,3), TJ(5,1), OJH(5,3), OJO(5,1), WH(3,3), WO(3,1)
      REAL :: DELTAH(5,3), DELTAO(5,1), ERROR(1,1)
      
      
      OI(1,:)=(/0,0,1 /)
      OI(2,:)=(/1,1,0 /)
      OI(3,:)=(/1,0,1 /)
      OI(4,:)=(/1,1,1 /)
      OI(5,:)=(/0,1,1 /)

      TJ(:,1)=(/0,0,1,0,1 /)

! *** OPEN FILES TO WRITE
      OPEN(1,FILE="XOR.out",STATUS='UNKNOWN')
      !OPEN(2,FILE="tanh_error.out",STATUS='UNKNOWN')
      !OPEN(3,FILE="tanh_time.out",STATUS='UNKNOWN')


! *** RANDOMIZE INTIAL WEIGHT BETWEEN -1 AND 1
      DO I=1,3
      WH(I,:)=2.*RAND()-1
      WO(I,:)=2.*RAND()-1
      END DO

! *** SET LEARNING RATE
      ALPHA=0.15
      CALL CPU_TIME(T_START)

! *** TRAINING EPOCHS
      DO J=1,100000

      CALL CPU_TIME(START)

! *** FORWARD PROPOGATION USING SIGMOID T.F.
      OJH=TANH(MATMUL(OI,WH))
      OJO=TANH(MATMUL(OJH,WO))

! *** FIND DELTA - (y-t)*f'(x)=f(x)(1-f(x))
      DELTAO = (OJO-TJ)*(1.-(OJO**2))
      DELTAH = (MATMUL(DELTAO,(TRANSPOSE(WO))))*((1.-(OJH**2)))
      !DO K=1,4
      !IF(DELTA(K,1).GT.0)PRINT*, DELTA(K,1)
      !END DO

! *** END OF CPU_TIME
      CALL CPU_TIME(FINISH)
      TIME=FINISH-START

! *** CALCULATE ERROR
      ERROR = (0.5)*(SUM((TJ-OJO)**2))
      WRITE(1,*)J, ERROR, TIME
      !WRITE(2,*)ERROR
      !WRITE(3,*)TIME
      !PRINT*,OJ

! *** UPDATE WEIGHTS (BACKPROPAGATION)
      WO = WO-ALPHA*MATMUL(TRANSPOSE(OJH),DELTAO)
      WH = WH-ALPHA*MATMUL(TRANSPOSE(OI),DELTAH)
      END DO !keep at it ;)
      CALL CPU_TIME(T_FINISH)
      T_TIME=T_FINISH-T_START
      !WRITE(1,*)"Alpha=",ALPHA,"Total Time=",T_TIME

! *** PRINT TRAINING OUTPUT RESULTS
      PRINT*,'Trained Outputs'
      PRINT*, OJO(1,1)
      PRINT*, OJO(2,1)
      PRINT*, OJO(3,1)
      PRINT*, OJO(4,1)
      PRINT*, OJO(5,1)
      PRINT*,"XOR ANN: ","Alpha=",ALPHA,"Total Time=",T_TIME


    2 PRINT*, 'Enter Data'
      WRITE(*,*) 'BYE TO EXIT'
      READ*, D1,D2,D3
      Z=D1*WO(1,1)+D2*WO(2,1)+D3*WO(3,1)
      FINAL=TANH(Z)
      PRINT*, FINAL
      GO TO 2

      END PROGRAM XOR ANN



