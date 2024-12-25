      PROGRAM MATRIX_VECTOR_MULTIPLY_BIN
      INTEGER N, I1, I2, I, IO
      REAL A(5, 1000000)  ! Matrix stored as 5 diagonals
      REAL F(1000000), RESULT(1000000)

      CALL READ_MATRIX_BINARY('matrix.bin')
      CALL READ_VECTOR_BINARY('vector.bin')
      CALL MULTIPLY_MATRIX_VECTOR
      CALL WRITE_VECTOR_BINARY('result_2.bin')
      CALL WRITE_VECTOR_TEXT('result_2.txt')

      PRINT *, 'Multiplication completed successfully.'
      END

      SUBROUTINE READ_MATRIX_BINARY(FILENAME)
      CHARACTER*(*) FILENAME
      INTEGER IO, I

      OPEN(10, FILE=FILENAME, STATUS='OLD', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot open file ', FILENAME
          STOP
      END IF

      READ(10, IOSTAT=IO) N, I1, I2
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read dimensions from binary file'
          STOP
      END IF

      READ(10, IOSTAT=IO) (A(1, I), I=1, N)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read main diagonal'
          STOP
      END IF

      READ(10, IOSTAT=IO) (A(2, I), I=1, N-1)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read first upper diagonal'
          STOP
      END IF

      READ(10, IOSTAT=IO) (A(3, I), I=1, N-I2)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read second upper diagonal'
          STOP
      END IF

      READ(10, IOSTAT=IO) (A(4, I), I=1, N-1)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read first lower diagonal'
          STOP
      END IF

      READ(10, IOSTAT=IO) (A(5, I), I=1, N-I1)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read second lower diagonal'
          STOP
      END IF

      CLOSE(10)
      RETURN
      END

      SUBROUTINE READ_VECTOR_BINARY(FILENAME)
      CHARACTER*(*) FILENAME
      INTEGER IO, I

      OPEN(11, FILE=FILENAME, STATUS='OLD', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot open file ', FILENAME
          STOP
      END IF

      READ(11, IOSTAT=IO) N
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read vector size from binary file'
          STOP
      END IF

      READ(11, IOSTAT=IO) (F(I), I=1, N)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read vector data from binary file'
          STOP
      END IF

      CLOSE(11)
      RETURN
      END

      SUBROUTINE MULTIPLY_MATRIX_VECTOR
      INTEGER I

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

      SUBROUTINE WRITE_VECTOR_BINARY(FILENAME)
      CHARACTER*(*) FILENAME
      INTEGER IO

      OPEN(12, FILE=FILENAME, STATUS='REPLACE', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot write to file ', FILENAME
          STOP
      END IF

      WRITE(12) N
      WRITE(12) (F(I), I=1, N)
      CLOSE(12)

      PRINT *, "Result vector written to binary file."
      RETURN
      END

      SUBROUTINE WRITE_VECTOR_TEXT(FILENAME)
      CHARACTER*(*) FILENAME
      INTEGER IO, I

      OPEN(13, FILE=FILENAME, STATUS='REPLACE', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot write to file ', FILENAME
          STOP
      END IF

      WRITE(13, *) "Result vector (size = ", N, "):"
      DO 30 I = 1, N
          WRITE(13, *) F(I)
   30 CONTINUE
      CLOSE(13)

      PRINT *, "Result vector written to text file."
      RETURN
      END
