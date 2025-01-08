      PROGRAM GENERATE_TESTS
      IMPLICIT NONE
      INTEGER N, I1, I2
      CHARACTER*20 MATRIX_BIN, VECTOR_BIN, DATA_FILE
      INTEGER MAX_SIZE
      PARAMETER (MAX_SIZE = 1000000)
      REAL mem(5 * MAX_SIZE + 2 * MAX_SIZE)  ! Общий массив для данных
      INTEGER I
      N = 10000
      I1 = 2
      I2 = 2
      MATRIX_BIN = 'matrix.bin'
      VECTOR_BIN = 'vector.bin'
      DATA_FILE = 'data.txt'
      CALL WRITE_DATA(DATA_FILE, N, I1, I2)
      CALL GENERATE_DATA(N, I1, I2, mem(1), mem(5 * N + 1))
      CALL WRITE_BINARY_MATRIX(MATRIX_BIN, N, I1, I2, mem(1))
      CALL WRITE_BINARY_VECTOR(VECTOR_BIN, N, mem(5 * N + 1))
      END

      SUBROUTINE WRITE_DATA(FILENAME, N, I1, I2)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2
      OPEN(10, FILE=FILENAME, STATUS='UNKNOWN')
      WRITE(10, *) N, I1, I2
      CLOSE(10)
      END

      SUBROUTINE GENERATE_DATA(N, I1, I2, A, F)
      INTEGER N, I1, I2, I
      REAL A(5, *), F(*)
      REAL SCALE
      PARAMETER (SCALE = 20.0)  ! Масштаб для диапазона [-10, 10]

      ! Генерация матрицы (5 диагоналей)
      DO I = 1, N-I1
         A(1, I) = (RAND() - 0.5) * SCALE  
      END DO
      DO I = 1, N-1
         A(2, I) = (RAND() - 0.5) * SCALE  
      END DO
      DO I = 1, N
         A(3, I) = (RAND() - 0.5) * SCALE  
      END DO
      DO I = 1, N-1
         A(4, I) = (RAND() - 0.5) * SCALE  
      END DO
      DO I = 1, N-I2
         A(5, I) = (RAND() - 0.5) * SCALE  
      END DO
      ! Генерация вектора
      DO I = 1, N
         F(I) = (RAND() - 0.5) * SCALE  
      END DO
      END

      SUBROUTINE WRITE_BINARY_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I, REC_NUM
      REAL A(5, *)
      OPEN(14, FILE=FILENAME, STATUS='REPLACE', ACCESS='DIRECT', RECL=4)
      REC_NUM = 1
      DO I = 1, N - I1
         WRITE(14, REC=REC_NUM) A(1, I)
         REC_NUM = REC_NUM + 1
      END DO
      DO I = 1, N - 1
         WRITE(14, REC=REC_NUM) A(2, I)
         REC_NUM = REC_NUM + 1
      END DO
      DO I = 1, N
         WRITE(14, REC=REC_NUM) A(3, I)
         REC_NUM = REC_NUM + 1
      END DO
      DO I = 1, N - 1
         WRITE(14, REC=REC_NUM) A(4, I)
         REC_NUM = REC_NUM + 1
      END DO
      DO I = 1, N - I2
         WRITE(14, REC=REC_NUM) A(5, I)
         REC_NUM = REC_NUM + 1
      END DO
      CLOSE(14)
      END

      SUBROUTINE WRITE_BINARY_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(*)
      OPEN(18, FILE=FILENAME, STATUS='REPLACE', ACCESS='DIRECT', RECL=4)
      DO I = 1, N
         WRITE(18, REC=I) F(I)
      END DO
      CLOSE(18)
      END