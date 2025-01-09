      PROGRAM MATRIX_MULTIPLY
      IMPLICIT NONE
      INTEGER N, I1, I2
      REAL mem(1000000)  ! Общий массив для всех данных
      CHARACTER*20 DATA_FILE, MATRIX_FILE, VECTOR_FILE, RESULT_FILE
      DATA_FILE = 'data.txt'
      MATRIX_FILE = 'matrix.txt'
      VECTOR_FILE = 'vector.txt'
      RESULT_FILE = 'result.txt'
      
      CALL READ_DATA(DATA_FILE, N, I1, I2)
      CALL READ_MATRIX(MATRIX_FILE, N, I1, I2, mem(1))          ! Матрица A
      CALL READ_VECTOR(VECTOR_FILE, N, mem(5*N + 1))            ! Вектор F
      CALL MULTIPLY_MATRIX_VECTOR(N, I1, I2, mem(1), mem(5*N + 1), 
     *                            mem(5*N + N + 1))             ! Результат
      CALL WRITE_VECTOR(RESULT_FILE, N, mem(5*N + N + 1))
      
      END
      
      SUBROUTINE READ_DATA(FILENAME, N, I1, I2)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2
      OPEN(10, FILE=FILENAME, STATUS='OLD')
      READ(10, *) N, I1, I2
      CLOSE(10)
      WRITE(*, *) 'Reading data from file:', FILENAME
      WRITE(*, *) 'N =', N, 'I1 =', I1, 'I2 =', I2
      RETURN
      END
      
      SUBROUTINE READ_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I
      REAL A(5, N)  ! Матрица A хранится в первых 5*N элементах mem
      OPEN(11, FILE=FILENAME, STATUS='OLD')
      READ(11, *) (A(1, I), I=1, N-I1)
      READ(11, *) (A(2, I), I=1, N-1)
      READ(11, *) (A(3, I), I=1, N)
      READ(11, *) (A(4, I), I=1, N-1)
      READ(11, *) (A(5, I), I=1, N-I2)
      CLOSE(11)
      WRITE(*, *) 'Reading matrix from file:', FILENAME
      RETURN
      END
      
      SUBROUTINE READ_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(N)  ! Вектор F хранится в следующих N элементах mem
      OPEN(12, FILE=FILENAME, STATUS='OLD')
      READ(12, *) (F(I), I=1, N)
      CLOSE(12)
      WRITE(*, *) 'Reading vector from file:', FILENAME
      RETURN
      END
      
      SUBROUTINE MULTIPLY_MATRIX_VECTOR(N, I1, I2, A, F, RESULT)
      INTEGER N, I1, I2, I
      REAL A(5, N), F(N), RESULT(N)
      DO 10 I = 1, N
         RESULT(I) = A(3, I) * F(I)
   10 CONTINUE
      DO 20 I = 2, N
         RESULT(I) = RESULT(I) + A(2, I-1) * F(I-1)
   20 CONTINUE
      DO 30 I = I1 + 1, N
         RESULT(I) = RESULT(I) + A(1, I-I1) * F(I-I1)
   30 CONTINUE
      DO 40 I = 1, N - 1
         RESULT(I) = RESULT(I) + A(4, I) * F(I+1)
   40 CONTINUE
      DO 50 I = 1, N - I2
         RESULT(I) = RESULT(I) + A(5, I) * F(I+I2)
   50 CONTINUE
      WRITE(*, *) 'Multiplying matrix and vector'
      RETURN
      END
      
      SUBROUTINE WRITE_VECTOR(FILENAME, N, RESULT)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL RESULT(N)  ! RESULT хранится в последних N элементах mem
      OPEN(13, FILE=FILENAME, STATUS='REPLACE')
      DO 200 I = 1, N
         WRITE(13, '(F12.6)') RESULT(I)
200   CONTINUE
      CLOSE(13)
      WRITE(*, *) 'Writing result to file:', FILENAME
      RETURN
      END