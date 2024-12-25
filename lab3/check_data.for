      PROGRAM CHECK_DATA
      INTEGER N, I1, I2
      CHARACTER*20 MATRIX_TXT, VECTOR_TXT, MATRIX_BIN, VECTOR_BIN, DATA_TXT
      PARAMETER (MAX_SIZE = 1000000)
      REAL A_TXT(5, MAX_SIZE), F_TXT(MAX_SIZE)
      REAL A_BIN(5, MAX_SIZE), F_BIN(MAX_SIZE)
      REAL A_DATA(5, MAX_SIZE), F_DATA(MAX_SIZE)
      INTEGER I, J

      N = 100  ! Пример размерности
      I1 = 1   ! Пример значения I1
      I2 = 1   ! Пример значения I2

      MATRIX_TXT = 'matrix.txt'
      VECTOR_TXT = 'vector.txt'
      MATRIX_BIN = 'matrix.bin'
      VECTOR_BIN = 'vector.bin'
      DATA_TXT = 'data.txt'

      CALL READ_TEXT_MATRIX(MATRIX_TXT, N, I1, I2, A_TXT)
      CALL READ_TEXT_VECTOR(VECTOR_TXT, N, F_TXT)
      CALL READ_BINARY_MATRIX(MATRIX_BIN, N, I1, I2, A_BIN)
      CALL READ_BINARY_VECTOR(VECTOR_BIN, N, F_BIN)
      CALL READ_DATA_MATRIX(DATA_TXT, N, I1, I2, A_DATA)
      CALL READ_DATA_VECTOR(DATA_TXT, N, F_DATA)

      DO I = 1, N-I1
         IF (ABS(A_TXT(1, I) - A_BIN(1, I)) > 1.0E-6) THEN
            PRINT *, 'Difference in upper diagonal at index', I
         END IF
         IF (ABS(A_TXT(1, I) - A_DATA(1, I)) > 1.0E-6) THEN
            PRINT *, 'Difference in upper diagonal with data.txt at index', I
         END IF
      END DO

      DO I = 1, N-1
         IF (ABS(A_TXT(2, I) - A_BIN(2, I)) > 1.0E-6) THEN
            PRINT *, 'Difference in superdiagonal at index', I
         END IF
         IF (ABS(A_TXT(2, I) - A_DATA(2, I)) > 1.0E-6) THEN
            PRINT *, 'Difference in superdiagonal with data.txt at index', I
         END IF
      END DO

      DO I = 1, N
         IF (ABS(A_TXT(3, I) - A_BIN(3, I)) > 1.0E-6) THEN
            PRINT *, 'Difference in main diagonal at index', I
         END IF
         IF (ABS(A_TXT(3, I) - A_DATA(3, I)) > 1.0E-6) THEN
            PRINT *, 'Difference in main diagonal with data.txt at index', I
         END IF
      END DO

      DO I = 1, N-1
         IF (ABS(A_TXT(4, I) - A_BIN(4, I)) > 1.0E-6) THEN
            PRINT *, 'Difference in subdiagonal at index', I
         END IF
         IF (ABS(A_TXT(4, I) - A_DATA(4, I)) > 1.0E-6) THEN
            PRINT *, 'Difference in subdiagonal with data.txt at index', I
         END IF
      END DO

      DO I = 1, N-I2
         IF (ABS(A_TXT(5, I) - A_BIN(5, I)) > 1.0E-6) THEN
            PRINT *, 'Difference in lower diagonal at index', I
         END IF
         IF (ABS(A_TXT(5, I) - A_DATA(5, I)) > 1.0E-6) THEN
            PRINT *, 'Difference in lower diagonal with data.txt at index', I
         END IF
      END DO

      DO I = 1, N
         IF (ABS(F_TXT(I) - F_BIN(I)) > 1.0E-6) THEN
            PRINT *, 'Difference in vector at index', I
         END IF
         IF (ABS(F_TXT(I) - F_DATA(I)) > 1.0E-6) THEN
            PRINT *, 'Difference in vector with data.txt at index', I
         END IF
      END DO

      END

      SUBROUTINE READ_TEXT_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I
      REAL A(5, *)

      OPEN(15, FILE=FILENAME, STATUS='OLD')
      READ(15, '(5E25.17)') (A(1, I), I=1, N-I1)  ! Верхняя диагональ
      READ(15, '(5E25.17)') (A(2, I), I=1, N-1)   ! Над главной диагональю
      READ(15, '(5E25.17)') (A(3, I), I=1, N)     ! Главная диагональ
      READ(15, '(5E25.17)') (A(4, I), I=1, N-1)   ! Под главной диагональю
      READ(15, '(5E25.17)') (A(5, I), I=1, N-I2)  ! Нижняя диагональ
      CLOSE(15)
      END

      SUBROUTINE READ_TEXT_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(*)

      OPEN(16, FILE=FILENAME, STATUS='OLD')
      READ(16, '(E25.17)') (F(I), I=1, N)
      CLOSE(16)
      END

      SUBROUTINE READ_BINARY_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I
      REAL A(5, *)

      OPEN(13, FILE=FILENAME, STATUS='OLD', FORM='UNFORMATTED', ACCESS='SEQUENTIAL')
      READ(13) (A(1, I), I=1, N-I1)  ! Верхняя диагональ
      READ(13) (A(2, I), I=1, N-1)   ! Над главной диагональю
      READ(13) (A(3, I), I=1, N)     ! Главная диагональ
      READ(13) (A(4, I), I=1, N-1)   ! Под главной диагональю
      READ(13) (A(5, I), I=1, N-I2)  ! Нижняя диагональ
      CLOSE(13)
      END

      SUBROUTINE READ_BINARY_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(*)

      OPEN(14, FILE=FILENAME, STATUS='OLD', FORM='UNFORMATTED', ACCESS='SEQUENTIAL')
      READ(14) (F(I), I=1, N)
      CLOSE(14)
      END

      SUBROUTINE READ_DATA_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I
      REAL A(5, *)

      OPEN(17, FILE=FILENAME, STATUS='OLD')
      READ(17, '(5E25.17)') (A(1, I), I=1, N-I1)  ! Верхняя диагональ
      READ(17, '(5E25.17)') (A(2, I), I=1, N-1)   ! Над главной диагональю
      READ(17, '(5E25.17)') (A(3, I), I=1, N)     ! Главная диагональ
      READ(17, '(5E25.17)') (A(4, I), I=1, N-1)   ! Под главной диагональю
      READ(17, '(5E25.17)') (A(5, I), I=1, N-I2)  ! Нижняя диагональ
      CLOSE(17)
      END

      SUBROUTINE READ_DATA_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(*)

      OPEN(18, FILE=FILENAME, STATUS='OLD')
      READ(18, '(E25.17)') (F(I), I=1, N)
      CLOSE(18)
      END
