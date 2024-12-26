      PROGRAM GENERATE_TESTS
      INTEGER N, I1, I2
      CHARACTER*20 MATRIX_TXT, VECTOR_TXT
      CHARACTER*20 DATA_FILE
      PARAMETER (MAX_SIZE = 1000000)
      REAL A(5, MAX_SIZE), F(MAX_SIZE)
      INTEGER I, J

      N = 10000  
      I1 = 2   
      I2 = 2   

      MATRIX_TXT = 'matrix.txt'
      VECTOR_TXT = 'vector.txt'
      DATA_FILE = 'data.txt'

      CALL WRITE_DATA(DATA_FILE, N, I1, I2)
      CALL GENERATE_DATA(N, I1, I2, A, F)
      CALL WRITE_TEXT_MATRIX(MATRIX_TXT, N, I1, I2, A)
      CALL WRITE_TEXT_VECTOR(VECTOR_TXT, N, F)

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
      REAL RAND, SCALE

      SCALE = 20.0  ! Масштаб для диапазона [-10, 10]

      DO I = 1, N-I1
         A(1, I) = (RAND(0) - 0.5) * SCALE  ! Верхняя диагональ
      END DO

      DO I = 1, N-1
         A(2, I) = (RAND(0) - 0.5) * SCALE  ! Над главной диагональю
      END DO

      DO I = 1, N
         A(3, I) = (RAND(0) - 0.5) * SCALE  ! Главная диагональ
      END DO

      DO I = 1, N-1
         A(4, I) = (RAND(0) - 0.5) * SCALE  ! Под главной диагональю
      END DO

      DO I = 1, N-I2
         A(5, I) = (RAND(0) - 0.5) * SCALE  ! Нижняя диагональ
      END DO

      DO I = 1, N
         F(I) = (RAND(0) - 0.5) * SCALE  ! Вектор
      END DO

      END

      SUBROUTINE WRITE_TEXT_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I
      REAL A(5, *)

      OPEN(15, FILE=FILENAME, STATUS='UNKNOWN')
      WRITE(15, *) (A(1, I), I=1, N-I1)  ! Верхняя диагональ
      WRITE(15, *) (A(2, I), I=1, N-1)   ! Над главной диагональю
      WRITE(15, *) (A(3, I), I=1, N)     ! Главная диагональ
      WRITE(15, *) (A(4, I), I=1, N-1)   ! Под главной диагональю
      WRITE(15, *) (A(5, I), I=1, N-I2)  ! Нижняя диагональ
      CLOSE(15)
      END

      SUBROUTINE WRITE_TEXT_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(*)

      OPEN(16, FILE=FILENAME, STATUS='UNKNOWN')
      WRITE(16, *) (F(I), I=1, N)
      CLOSE(16)
      END
