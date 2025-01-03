PROGRAM MATRIX_MULTIPLY
   INTEGER N, I1, I2
   REAL A(5, 1000000), F(1000000), RESULT(1000000)
   CHARACTER*20 DATA_FILE, MATRIX_BIN, VECTOR_BIN, RESULT_FILE
   DATA_FILE = 'data.txt'
   MATRIX_BIN = 'matrix.bin'
   VECTOR_BIN = 'vector.bin'
   RESULT_FILE = 'result.txt'
   
   CALL READ_DATA(DATA_FILE, N, I1, I2)
   
   IF (N <= 0 .OR. I1 < 0 .OR. I1 >= N .OR. I2 < 0 .OR. I2 >= N) THEN
      WRITE(*, *) 'Invalid values for N, I1, or I2.'
      STOP
   END IF
   
   CALL READ_BINARY_MATRIX(MATRIX_BIN, N, I1, I2, A)
   CALL READ_BINARY_VECTOR(VECTOR_BIN, N, F)
   CALL MULTIPLY_MATRIX_VECTOR(N, I1, I2, A, F, RESULT)
   CALL WRITE_VECTOR(RESULT_FILE, N, RESULT)
   
   END
   
   
   SUBROUTINE READ_DATA(FILENAME, N, I1, I2)
   CHARACTER*20 FILENAME
   INTEGER N, I1, I2
   
   OPEN(10, FILE=FILENAME, STATUS='OLD', IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening file:', FILENAME
      STOP
   END IF
   READ(10, *, IOSTAT=IO_ERR) N, I1, I2
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error reading data from file:', FILENAME
      STOP
   END IF
   CLOSE(10)
   WRITE(*, *) 'Reading data from file:', FILENAME
   WRITE(*, *) 'N =', N, 'I1 =', I1, 'I2 =', I2
   RETURN
   END
   
   
   SUBROUTINE READ_BINARY_MATRIX(FILENAME, N, I1, I2, A)
   CHARACTER*20 FILENAME
   INTEGER N, I1, I2, I, IO_ERR
   REAL A(5, 1000000)
   
   OPEN(11, FILE=FILENAME, STATUS='OLD', ACCESS='DIRECT', RECL=4, IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening matrix file:', FILENAME
      STOP
   END IF
   
   IF (I1 > 0) THEN
      DO I = 1, N-I1
         READ(11, REC=I, IOSTAT=IO_ERR) A(1, I)
         IF (IO_ERR /= 0) THEN
            WRITE(*, *) 'Error reading upper diagonal.'
            STOP
         END IF
      END DO
   END IF
   
   IF (N-1 > 0) THEN
      DO I = 1, N-1
         READ(11, REC=N-I1+I, IOSTAT=IO_ERR) A(2, I)
         IF (IO_ERR /= 0) THEN
            WRITE(*, *) 'Error reading above main diagonal.'
            STOP
         END IF
      END DO
   END IF
   
   DO I = 1, N
      READ(11, REC=2*N-I1+I, IOSTAT=IO_ERR) A(3, I)
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error reading main diagonal.'
         STOP
      END IF
   END DO
   
   IF (N-1 > 0) THEN
      DO I = 1, N-1
         READ(11, REC=3*N-I1+I, IOSTAT=IO_ERR) A(4, I)
         IF (IO_ERR /= 0) THEN
            WRITE(*, *) 'Error reading below main diagonal.'
            STOP
         END IF
      END DO
   END IF
   
   IF (I2 > 0) THEN
      DO I = 1, N-I2
         READ(11, REC=4*N-I1+I, IOSTAT=IO_ERR) A(5, I)
         IF (IO_ERR /= 0) THEN
            WRITE(*, *) 'Error reading lower diagonal.'
            STOP
         END IF
      END DO
   END IF
   
   CLOSE(11)
   WRITE(*, *) 'Reading binary matrix from file:', FILENAME
   RETURN
   END
   
   
   SUBROUTINE READ_BINARY_VECTOR(FILENAME, N, F)
   CHARACTER*20 FILENAME
   INTEGER N, I, IO_ERR
   REAL F(1000000)
   
   OPEN(12, FILE=FILENAME, STATUS='OLD', ACCESS='DIRECT', RECL=4, IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening vector file:', FILENAME
      STOP
   END IF
   
   DO I = 1, N
      READ(12, REC=I, IOSTAT=IO_ERR) F(I)
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error reading vector element:', I
         STOP
      END IF
   END DO
   
   CLOSE(12)
   WRITE(*, *) 'Reading binary vector from file:', FILENAME
   RETURN
   END
   
   
   SUBROUTINE MULTIPLY_MATRIX_VECTOR(N, I1, I2, A, F, RESULT)
   INTEGER N, I1, I2, I
   REAL A(5, 1000000), F(1000000), RESULT(1000000)
   
   DO I = 1, N
      RESULT(I) = A(3, I) * F(I)  ! Main diagonal
      IF (I > 1) THEN
         RESULT(I) = RESULT(I) + A(2, I-1) * F(I-1)  ! Above main
      END IF
      IF (I1 > 0 .AND. I > I1) THEN
         RESULT(I) = RESULT(I) + A(1, I-I1) * F(I-I1)  ! Upper
      END IF
      IF (I < N) THEN
         RESULT(I) = RESULT(I) + A(4, I) * F(I+1)  ! Below main
      END IF
      IF (I2 > 0 .AND. I <= N-I2) THEN
         RESULT(I) = RESULT(I) + A(5, I) * F(I+I2)  ! Lower
      END IF
   END DO
   WRITE(*, *) 'Multiplying matrix and vector'
   RETURN
   END
   
   
   SUBROUTINE WRITE_VECTOR(FILENAME, N, F)
   CHARACTER*20 FILENAME
   INTEGER N, I
   REAL F(1000000)
   
   OPEN(13, FILE=FILENAME, STATUS='REPLACE', IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening result file:', FILENAME
      STOP
   END IF
   
   DO I = 1, N
      WRITE(13, '(F12.6)') F(I)
   END DO
   
   CLOSE(13)
   WRITE(*, *) 'Writing result to file:', FILENAME
   RETURN
   END