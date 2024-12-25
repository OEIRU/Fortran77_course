      PROGRAM MATRIX_MULTIPLY
      INTEGER N, I1, I2
      REAL A(5, 1000000), F(1000000), RESULT(1000000)
      
      CHARACTER*20 DATA_FILE, MATRIX_BIN, VECTOR_BIN, RESULT_FILE
      DATA_FILE = 'data.txt'
      MATRIX_BIN = 'matrix.bin'
      VECTOR_BIN = 'vector.bin'
      RESULT_FILE = 'result.txt'

      CALL READ_DATA(DATA_FILE, N, I1, I2)
      CALL READ_BINARY_MATRIX(MATRIX_BIN, N, I1, I2, A)
      CALL READ_BINARY_VECTOR(VECTOR_BIN, N, F)
      CALL MULTIPLY_MATRIX_VECTOR(N, I1, I2, A, F, RESULT)
      CALL WRITE_VECTOR(RESULT_FILE, N, RESULT)

      END

      SUBROUTINE READ_DATA(FILENAME, N, I1, I2)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2

      OPEN(10, FILE=FILENAME, STATUS='OLD')
      READ(10, *) N, I1, I2
      CLOSE(10)
      WRITE(*, *) 'Reading data from file:', FILENAME
      WRITE(*, *) 'N =', N, 'I1 =', I1, 'I2 =', I2
      END

      SUBROUTINE READ_BINARY_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I
      REAL A(5, 1000000)

      OPEN(11, FILE=FILENAME, STATUS='OLD', ACCESS='DIRECT', RECL=4)

      DO I = 1, N-I1
         READ(11, REC=I) A(1, I)
      END DO
      DO I = 1, N-1
         READ(11, REC=N-I1+I) A(2, I)
      END DO
      DO I = 1, N
         READ(11, REC=2*N-I1+I) A(3, I)
      END DO
      DO I = 1, N-1
         READ(11, REC=3*N-I1+I) A(4, I)
      END DO
      DO I = 1, N-I2
         READ(11, REC=4*N-I1+I) A(5, I)
      END DO

      CLOSE(11)
      WRITE(*, *) 'Reading binary matrix from file:', FILENAME
      END

      SUBROUTINE READ_BINARY_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(1000000)

      OPEN(12, FILE=FILENAME, STATUS='OLD', ACCESS='DIRECT', RECL=4)

      DO I = 1, N
         READ(12, REC=I) F(I)
      END DO

      CLOSE(12)
      WRITE(*, *) 'Reading binary vector from file:', FILENAME
      END

      SUBROUTINE MULTIPLY_MATRIX_VECTOR(N, I1, I2, A, F, RESULT)
      INTEGER N, I1, I2, I
      REAL A(5, 1000000), F(1000000), RESULT(1000000)

      DO I = 1, N
         RESULT(I) = A(3, I) * F(I)  ! Главная диагональ
         IF (I > 1) RESULT(I) = RESULT(I) + A(2, I-1) * F(I-1)  ! Над главной
         IF (I > I1) RESULT(I) = RESULT(I) + A(1, I-I1) * F(I-I1)  ! Верхняя
         IF (I < N) RESULT(I) = RESULT(I) + A(4, I) * F(I+1)  ! Под главной
         IF (I <= N-I2) RESULT(I) = RESULT(I) + A(5, I) * F(I+I2)  ! Нижняя
      END DO
      WRITE(*, *) 'Multiplying matrix and vector'
      END

      SUBROUTINE WRITE_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(1000000)

      OPEN(13, FILE=FILENAME, STATUS='REPLACE')
      DO I = 1, N
         WRITE(13, '(F8.3)') F(I)
      END DO
      CLOSE(13)
      WRITE(*, *) 'Writing result to file:', FILENAME
      END
