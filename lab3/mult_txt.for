      PROGRAM MATRIX_MULTIPLY
      INTEGER N, I1, I2
      REAL A(5, 1000000), F(1000000), RESULT(1000000)
      
      CHARACTER*20 DATA_FILE, MATRIX_FILE, VECTOR_FILE, RESULT_FILE
      DATA_FILE = 'data.txt'
      MATRIX_FILE = 'matrix.txt'
      VECTOR_FILE = 'vector.txt'
      RESULT_FILE = 'result.txt'

      CALL READ_DATA(DATA_FILE, N, I1, I2)
      CALL READ_MATRIX(MATRIX_FILE, N, I1, I2, A)
      CALL READ_VECTOR(VECTOR_FILE, N, F)
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

      SUBROUTINE READ_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I
      REAL A(5, 1000000)

      OPEN(11, FILE=FILENAME, STATUS='OLD')

      READ(11, *) (A(1, I), I=1, N-I1)  ! Верхняя диагональ
      READ(11, *) (A(2, I), I=1, N-1)   ! Над главной диагональю
      READ(11, *) (A(3, I), I=1, N)     ! Главная диагональ
      READ(11, *) (A(4, I), I=1, N-1)   ! Под главной диагональю
      READ(11, *) (A(5, I), I=1, N-I2)  ! Нижняя диагональ
      
      CLOSE(11)
      WRITE(*, *) 'Reading matrix from file:', FILENAME
      RETURN

      STOP
      END

      SUBROUTINE READ_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(1000000)

      OPEN(12, FILE=FILENAME, STATUS='OLD')
      READ(12, *) (F(I), I=1, N)
      CLOSE(12)
      WRITE(*, *) 'Reading vector from file:', FILENAME
      RETURN

      STOP
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
