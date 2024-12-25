      PROGRAM MATRIX_MULTIPLY
      INTEGER N, I1, I2, I, IO
      PARAMETER (MAX_SIZE = 1000000)
      REAL A(5, MAX_SIZE)
      REAL F(MAX_SIZE), RESULT(MAX_SIZE)

      CALL READ_MATRIX('matrix.txt', N, I1, I2, A)
      CALL READ_VECTOR('vector.txt', N, F)
      CALL MULTIPLY_MATRIX_VECTOR(N, I1, I2, A, F, RESULT)
      CALL WRITE_VECTOR('result_1.txt', N, F)

      PRINT *, 'Matrix-vector multiplication completed successfully.'
      END

      SUBROUTINE READ_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*(*) FILENAME
      INTEGER N, I1, I2, IO, I
      REAL A(5, MAX_SIZE)

      OPEN(10, FILE=FILENAME, STATUS='OLD', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot open file ', FILENAME
          STOP
      END IF

      READ(10, *, IOSTAT=IO) N, I1, I2
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read dimensions from file'
          STOP
      END IF

      READ(10, *, IOSTAT=IO) (A(1, I), I=1, N)
      READ(10, *, IOSTAT=IO) (A(2, I), I=1, N-1)
      READ(10, *, IOSTAT=IO) (A(3, I), I=1, N-I2)
      READ(10, *, IOSTAT=IO) (A(4, I), I=1, N-1)
      READ(10, *, IOSTAT=IO) (A(5, I), I=1, N-I1)

      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read matrix data from file'
          STOP
      END IF

      CLOSE(10)
      RETURN
      END

      SUBROUTINE READ_VECTOR(FILENAME, N, F)
      CHARACTER*(*) FILENAME
      INTEGER N, IO, I
      REAL F(MAX_SIZE)

      OPEN(11, FILE=FILENAME, STATUS='OLD', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot open file ', FILENAME
          STOP
      END IF

      READ(11, *, IOSTAT=IO) N
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read vector size from file'
          STOP
      END IF

      READ(11, *, IOSTAT=IO) (F(I), I=1, N)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read vector data from file'
          STOP
      END IF

      CLOSE(11)
      RETURN
      END

      SUBROUTINE MULTIPLY_MATRIX_VECTOR(N, I1, I2, A, F, RESULT)
      INTEGER N, I1, I2, I
      REAL A(5, MAX_SIZE)
      REAL F(MAX_SIZE), RESULT(MAX_SIZE)

      DO 10 I = 1, N
          RESULT(I) = A(1, I) * F(I)
          IF (I .GT. 1) RESULT(I) = RESULT(I) + A(4, I-1) * F(I-1)
          IF (I .GT. 2) RESULT(I) = RESULT(I) + A(5, I-2) * F(I-2)
          IF (I .LT. N) RESULT(I) = RESULT(I) + A(2, I) * F(I+1)
          IF (I .LE. N-I2) RESULT(I) = RESULT(I) + A(3, I) * F(I+I2)
   10 CONTINUE

      DO 20 I = 1, N
          F(I) = RESULT(I)
   20 CONTINUE
      RETURN
      END

      SUBROUTINE WRITE_VECTOR(FILENAME, N, F)
      CHARACTER*(*) FILENAME
      INTEGER N, IO, I
      REAL F(MAX_SIZE)

      OPEN(12, FILE=FILENAME, STATUS='REPLACE', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot write to file ', FILENAME
          STOP
      END IF

      DO 30 I = 1, N
          WRITE(12, '(F8.3)') F(I)
   30 CONTINUE

      CLOSE(12)
      PRINT *, 'Result written to file: ', FILENAME
      RETURN
      END
