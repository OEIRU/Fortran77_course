      PROGRAM GENERATE_TESTS
      IMPLICIT NONE
      INTEGER N, I1, I2
      CHARACTER*20 MATRIX_TXT, VECTOR_TXT, MATRIX_BIN, VECTOR_BIN
      CHARACTER*20 DATA_FILE
      INTEGER MAX_SIZE
      PARAMETER (MAX_SIZE = 1000000)
      REAL mem(5 * MAX_SIZE + 2 * MAX_SIZE)  ! Общий массив для данных
      INTEGER I

      N = 10000  
      I1 = 2   
      I2 = 2   

      MATRIX_TXT = 'matrix.txt'
      VECTOR_TXT = 'vector.txt'
      MATRIX_BIN = 'matrix.bin'
      VECTOR_BIN = 'vector.bin'
      DATA_FILE = 'data.txt'

      ! Запись данных о размерах в файл
      CALL WRITE_DATA(DATA_FILE, N, I1, I2)

      ! Генерация данных
      CALL GENERATE_DATA(N, I1, I2, mem(1), mem(5 * N + 1))

      ! Запись матрицы и вектора в текстовые файлы
      CALL WRITE_TEXT_MATRIX(MATRIX_TXT, N, I1, I2, mem(1))
      CALL WRITE_TEXT_VECTOR(VECTOR_TXT, N, mem(5 * N + 1))

      ! Запись матрицы и вектора в бинарные файлы
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
      REAL RAND, SCALE

      SCALE = 20.0  ! Масштаб для диапазона [-10, 10]

      ! Генерация матрицы
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

      ! Генерация вектора
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
      WRITE(*, *) 'Matrix written to text file:', FILENAME
      END

      SUBROUTINE WRITE_TEXT_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(*)

      OPEN(16, FILE=FILENAME, STATUS='UNKNOWN')
      WRITE(16, *) (F(I), I=1, N)
      CLOSE(16)
      WRITE(*, *) 'Vector written to text file:', FILENAME
      END

      SUBROUTINE WRITE_BINARY_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I, IO_ERR
      REAL A(5, *)

      OPEN(17, FILE=FILENAME, STATUS='REPLACE', ACCESS='DIRECT',
     *     RECL=4, IOSTAT=IO_ERR)
      IF (IO_ERR .NE. 0) THEN
         WRITE(*, *) 'Error opening binary file:', FILENAME
         STOP
      END IF

      ! Запись матрицы в бинарный файл
      DO I = 1, N-I1
         WRITE(17, REC=I, IOSTAT=IO_ERR) A(1, I)  ! Верхняя диагональ
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error writing upper diagonal.'
            STOP
         END IF
      END DO

      DO I = 1, N-1
         WRITE(17, REC=N-I1+I, IOSTAT=IO_ERR) A(2, I)  ! Над главной диагональю
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error writing above main diagonal.'
            STOP
         END IF
      END DO

      DO I = 1, N
         WRITE(17, REC=2*N-I1+I, IOSTAT=IO_ERR) A(3, I)  ! Главная диагональ
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error writing main diagonal.'
            STOP
         END IF
      END DO

      DO I = 1, N-1
         WRITE(17, REC=3*N-I1+I, IOSTAT=IO_ERR) A(4, I)  ! Под главной диагональю
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error writing below main diagonal.'
            STOP
         END IF
      END DO

      DO I = 1, N-I2
         WRITE(17, REC=4*N-I1+I, IOSTAT=IO_ERR) A(5, I)  ! Нижняя диагональ
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error writing lower diagonal.'
            STOP
         END IF
      END DO

      CLOSE(17)
      WRITE(*, *) 'Matrix written to binary file:', FILENAME
      END

      SUBROUTINE WRITE_BINARY_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I, IO_ERR
      REAL F(*)

      OPEN(18, FILE=FILENAME, STATUS='REPLACE', ACCESS='DIRECT',
     *     RECL=4, IOSTAT=IO_ERR)
      IF (IO_ERR .NE. 0) THEN
         WRITE(*, *) 'Error opening binary file:', FILENAME
         STOP
      END IF

      ! Запись вектора в бинарный файл
      DO I = 1, N
         WRITE(18, REC=I, IOSTAT=IO_ERR) F(I)
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error writing vector to file:', FILENAME
            STOP
         END IF
      END DO

      CLOSE(18)
      WRITE(*, *) 'Vector written to binary file:', FILENAME
      END