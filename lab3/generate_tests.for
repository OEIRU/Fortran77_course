      PROGRAM GENERATE_TESTS
      IMPLICIT NONE
      INTEGER N, I1, I2, MAX_SIZE
      PARAMETER (MAX_SIZE = 1000000)
      REAL mem(5 * MAX_SIZE + 2 * MAX_SIZE), SCALE
      PARAMETER (SCALE = 20.0)
      CHARACTER*20 MATRIX_BIN, VECTOR_BIN, DATA_FILE
      CHARACTER*20 MATRIX_TXT, VECTOR_TXT
      INTEGER SEED
      N = 10000
      I1 = 2
      I2 = 2
      SEED = 12345  ! Начальное значение для генератора случайных чисел
      MATRIX_BIN = 'matrix.bin'
      VECTOR_BIN = 'vector.bin'
      MATRIX_TXT = 'matrix.txt'
      VECTOR_TXT = 'vector.txt'
      DATA_FILE = 'data.txt'
      CALL WRITE_DATA(DATA_FILE, N, I1, I2)
      CALL GENERATE_AND_WRITE(MATRIX_BIN, VECTOR_BIN, MATRIX_TXT, 
     &                        VECTOR_TXT, N, I1, I2, mem(1), 
     &                        mem(5 * N + 1), SCALE, SEED)
      END

      FUNCTION RAND(SEED)
      INTEGER SEED
      REAL RAND
      SEED = MOD(SEED * 1103515245 + 12345, 2147483648)
      RAND = REAL(SEED) / 2147483648.0
      RETURN
      END

      SUBROUTINE WRITE_DATA(FILENAME, N, I1, I2)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2
      OPEN(10, FILE=FILENAME, STATUS='UNKNOWN')
      WRITE(10, *) N, I1, I2
      CLOSE(10)
      END
      
      SUBROUTINE GENERATE_AND_WRITE(MATRIX_BIN, VECTOR_BIN, MATRIX_TXT, 
     &                            VECTOR_TXT, N, I1, I2, A, F, SCALE,
     &                            SEED)
      CHARACTER*20 MATRIX_BIN, VECTOR_BIN, MATRIX_TXT, VECTOR_TXT
      INTEGER N, I1, I2, I, REC_NUM, SEED
      REAL A(5, *), F(*), SCALE, VALUE, RAND
      OPEN(14, FILE=MATRIX_BIN, STATUS='UNKNOWN', 
     &   ACCESS='DIRECT', RECL=4)
      OPEN(18, FILE=VECTOR_BIN, STATUS='UNKNOWN', 
     &   ACCESS='DIRECT', RECL=4)
      OPEN(15, FILE=MATRIX_TXT, STATUS='UNKNOWN')
      OPEN(19, FILE=VECTOR_TXT, STATUS='UNKNOWN')
      REC_NUM = 1
      ! Генерация и запись матрицы
      DO I = 1, N - I1
         VALUE = (RAND(SEED) - 0.5) * SCALE
         A(1, I) = VALUE
         WRITE(14, REC=REC_NUM) VALUE
         WRITE(15, '(F8.2)') VALUE
         REC_NUM = REC_NUM + 1
      END DO
      DO I = 1, N - 1
         VALUE = (RAND(SEED) - 0.5) * SCALE
         A(2, I) = VALUE
         WRITE(14, REC=REC_NUM) VALUE
         WRITE(15, '(F8.2)') VALUE
         REC_NUM = REC_NUM + 1
      END DO
      DO I = 1, N
         VALUE = (RAND(SEED) - 0.5) * SCALE
         A(3, I) = VALUE
         WRITE(14, REC=REC_NUM) VALUE
         WRITE(15, '(F8.2)') VALUE
         REC_NUM = REC_NUM + 1
      END DO
      DO I = 1, N - 1
         VALUE = (RAND(SEED) - 0.5) * SCALE
         A(4, I) = VALUE
         WRITE(14, REC=REC_NUM) VALUE
         WRITE(15, '(F8.2)') VALUE
         REC_NUM = REC_NUM + 1
      END DO
      DO I = 1, N - I2
         VALUE = (RAND(SEED) - 0.5) * SCALE
         A(5, I) = VALUE
         WRITE(14, REC=REC_NUM) VALUE
         WRITE(15, '(F8.2)') VALUE
         REC_NUM = REC_NUM + 1
      END DO
      ! Генерация и запись вектора
      DO I = 1, N
         F(I) = (RAND(SEED) - 0.5) * SCALE
         WRITE(18, REC=I) F(I)
         WRITE(19, '(F8.2)') F(I)
      END DO
      CLOSE(14)
      CLOSE(15)
      CLOSE(18)
      CLOSE(19)
      END