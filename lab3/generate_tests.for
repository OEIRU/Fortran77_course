      PROGRAM GENERATE_TESTS
      INTEGER N, I1, I2
      CHARACTER*20 MATRIX_TXT, VECTOR_TXT, MATRIX_BIN, VECTOR_BIN
      CHARACTER*20 DATA_FILE
      PARAMETER (MAX_SIZE = 1000000)
      REAL A(5, MAX_SIZE), F(MAX_SIZE)
      INTEGER I, J

      N = 1000  ! Пример размерности, можно изменить
      I1 = 1   ! Пример значения I1, можно изменить
      I2 = 1   ! Пример значения I2, можно изменить

      MATRIX_TXT = 'matrix.txt'
      VECTOR_TXT = 'vector.txt'
      MATRIX_BIN = 'matrix.bin'
      VECTOR_BIN = 'vector.bin'
      DATA_FILE = 'data.txt'

      CALL WRITE_DATA(DATA_FILE, N, I1, I2)
      CALL GENERATE_DATA(N, I1, I2, A, F)
      CALL WRITE_TEXT_MATRIX(MATRIX_TXT, N, I1, I2, A)
      CALL WRITE_TEXT_VECTOR(VECTOR_TXT, N, F)
      CALL WRITE_BINARY_MATRIX(MATRIX_BIN, N, I1, I2, A)
      CALL WRITE_BINARY_VECTOR(VECTOR_BIN, N, F)

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

      DO I = 1, N-I1
         A(1, I) = 1.0  ! Верхняя диагональ
      END DO

      DO I = 1, N-1
         A(2, I) = 2.0  ! Над главной диагональю
      END DO

      DO I = 1, N
         A(3, I) = 3.0  ! Главная диагональ
      END DO

      DO I = 1, N-1
         A(4, I) = 4.0  ! Под главной диагональю
      END DO

      DO I = 1, N-I2
         A(5, I) = 5.0  ! Нижняя диагональ
      END DO

      DO I = 1, N
         F(I) = 6.0  ! Вектор
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

      SUBROUTINE WRITE_BINARY_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I
      REAL A(5, *)

      OPEN(13, FILE=FILENAME, STATUS='UNKNOWN', 
     & FORM='UNFORMATTED', ACCESS='SEQUENTIAL')
      WRITE(13) (A(1, I), I=1, N-I1)  ! Верхняя диагональ
      WRITE(13) (A(2, I), I=1, N-1)   ! Над главной диагональю
      WRITE(13) (A(3, I), I=1, N)     ! Главная диагональ
      WRITE(13) (A(4, I), I=1, N-1)   ! Под главной диагональю
      WRITE(13) (A(5, I), I=1, N-I2)  ! Нижняя диагональ
      CLOSE(13)
      END

      SUBROUTINE WRITE_BINARY_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(*)

      OPEN(14, FILE=FILENAME, STATUS='UNKNOWN', 
     & FORM='UNFORMATTED', ACCESS='SEQUENTIAL')
      WRITE(14) (F(I), I=1, N)
      CLOSE(14)
      END
