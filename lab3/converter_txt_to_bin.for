PROGRAM CONVERT_TO_BINARY
   IMPLICIT NONE
   INTEGER :: N, I1, I2
   REAL :: mem(1000000)  ! Общий массив для всех данных

   CHARACTER(LEN=20) :: DATA_FILE, MATRIX_FILE, VECTOR_FILE, MATRIX_BIN, VECTOR_BIN

   ! Инициализация имен файлов
   DATA_FILE = 'data.txt'
   MATRIX_FILE = 'matrix.txt'
   VECTOR_FILE = 'vector.txt'
   MATRIX_BIN = 'matrix.bin'
   VECTOR_BIN = 'vector.bin'

   ! Чтение данных из текстового файла
   CALL READ_DATA(DATA_FILE, N, I1, I2)

   ! Чтение матрицы из текстового файла в mem
   CALL READ_MATRIX(MATRIX_FILE, N, I1, I2, mem(1))

   ! Чтение вектора из текстового файла в mem
   CALL READ_VECTOR(VECTOR_FILE, N, mem(5*N + 1))

   ! Запись матрицы из mem в бинарный файл
   CALL WRITE_BINARY_MATRIX(MATRIX_BIN, N, I1, I2, mem(1))

   ! Запись вектора из mem в бинарный файл
   CALL WRITE_BINARY_VECTOR(VECTOR_BIN, N, mem(5*N + 1))

END PROGRAM CONVERT_TO_BINARY


! Подпрограмма для чтения данных из файла
SUBROUTINE READ_DATA(FILENAME, N, I1, I2)
   IMPLICIT NONE
   CHARACTER(LEN=20), INTENT(IN) :: FILENAME
   INTEGER, INTENT(OUT) :: N, I1, I2
   INTEGER :: IO_ERR

   ! Открытие файла для чтения
   OPEN(UNIT=10, FILE=FILENAME, STATUS='OLD', ACTION='READ', IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening file: ', FILENAME
      STOP
   END IF

   ! Чтение значений N, I1, I2 из файла
   READ(10, *, IOSTAT=IO_ERR) N, I1, I2
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error reading data from file: ', FILENAME
      CLOSE(10)
      STOP
   END IF

   CLOSE(10)
   WRITE(*, *) 'Reading data from file: ', FILENAME
   WRITE(*, *) 'N = ', N, ', I1 = ', I1, ', I2 = ', I2
END SUBROUTINE READ_DATA


! Подпрограмма для чтения матрицы из файла в mem
SUBROUTINE READ_MATRIX(FILENAME, N, I1, I2, A)
   IMPLICIT NONE
   CHARACTER(LEN=20), INTENT(IN) :: FILENAME
   INTEGER, INTENT(IN) :: N, I1, I2
   REAL, INTENT(OUT) :: A(5, N)  ! Матрица A хранится в первых 5*N элементах mem
   INTEGER :: I, IO_ERR

   ! Открытие файла для чтения
   OPEN(UNIT=11, FILE=FILENAME, STATUS='OLD', ACTION='READ', IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening file: ', FILENAME
      STOP
   END IF

   ! Чтение элементов матрицы из файла
   READ(11, *, IOSTAT=IO_ERR) (A(1, I), I=1, N-I1)  ! Верхняя диагональ
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error reading upper diagonal from file: ', FILENAME
      CLOSE(11)
      STOP
   END IF

   READ(11, *, IOSTAT=IO_ERR) (A(2, I), I=1, N-1)   ! Над главной диагональю
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error reading above main diagonal from file: ', FILENAME
      CLOSE(11)
      STOP
   END IF

   READ(11, *, IOSTAT=IO_ERR) (A(3, I), I=1, N)     ! Главная диагональ
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error reading main diagonal from file: ', FILENAME
      CLOSE(11)
      STOP
   END IF

   READ(11, *, IOSTAT=IO_ERR) (A(4, I), I=1, N-1)   ! Под главной диагональю
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error reading below main diagonal from file: ', FILENAME
      CLOSE(11)
      STOP
   END IF

   READ(11, *, IOSTAT=IO_ERR) (A(5, I), I=1, N-I2)  ! Нижняя диагональ
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error reading lower diagonal from file: ', FILENAME
      CLOSE(11)
      STOP
   END IF

   CLOSE(11)
   WRITE(*, *) 'Reading matrix from file: ', FILENAME
END SUBROUTINE READ_MATRIX


! Подпрограмма для чтения вектора из файла в mem
SUBROUTINE READ_VECTOR(FILENAME, N, F)
   IMPLICIT NONE
   CHARACTER(LEN=20), INTENT(IN) :: FILENAME
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(OUT) :: F(N)  ! Вектор F хранится в следующих N элементах mem
   INTEGER :: I, IO_ERR

   ! Открытие файла для чтения
   OPEN(UNIT=12, FILE=FILENAME, STATUS='OLD', ACTION='READ', IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening file: ', FILENAME
      STOP
   END IF

   ! Чтение элементов вектора из файла
   READ(12, *, IOSTAT=IO_ERR) (F(I), I=1, N)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error reading vector from file: ', FILENAME
      CLOSE(12)
      STOP
   END IF

   CLOSE(12)
   WRITE(*, *) 'Reading vector from file: ', FILENAME
END SUBROUTINE READ_VECTOR


! Подпрограмма для записи матрицы из mem в бинарный файл
SUBROUTINE WRITE_BINARY_MATRIX(FILENAME, N, I1, I2, A)
   IMPLICIT NONE
   CHARACTER(LEN=20), INTENT(IN) :: FILENAME
   INTEGER, INTENT(IN) :: N, I1, I2
   REAL, INTENT(IN) :: A(5, N)  ! Матрица A хранится в первых 5*N элементах mem
   INTEGER :: I, IO_ERR

   ! Открытие бинарного файла для записи
   OPEN(UNIT=14, FILE=FILENAME, STATUS='REPLACE', ACCESS='DIRECT', RECL=4, IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening binary file: ', FILENAME
      STOP
   END IF

   ! Запись элементов матрицы в бинарный файл
   DO I = 1, N-I1
      WRITE(14, REC=I, IOSTAT=IO_ERR) A(1, I)  ! Верхняя диагональ
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error writing upper diagonal to file: ', FILENAME
         CLOSE(14)
         STOP
      END IF
   END DO

   DO I = 1, N-1
      WRITE(14, REC=N-I1+I, IOSTAT=IO_ERR) A(2, I)  ! Над главной диагональю
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error writing above main diagonal to file: ', FILENAME
         CLOSE(14)
         STOP
      END IF
   END DO

   DO I = 1, N
      WRITE(14, REC=2*N-I1+I, IOSTAT=IO_ERR) A(3, I)  ! Главная диагональ
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error writing main diagonal to file: ', FILENAME
         CLOSE(14)
         STOP
      END IF
   END DO

   DO I = 1, N-1
      WRITE(14, REC=3*N-I1+I, IOSTAT=IO_ERR) A(4, I)  ! Под главной диагональю
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error writing below main diagonal to file: ', FILENAME
         CLOSE(14)
         STOP
      END IF
   END DO

   DO I = 1, N-I2
      WRITE(14, REC=4*N-I1+I, IOSTAT=IO_ERR) A(5, I)  ! Нижняя диагональ
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error writing lower diagonal to file: ', FILENAME
         CLOSE(14)
         STOP
      END IF
   END DO

   CLOSE(14)
   WRITE(*, *) 'Writing binary matrix to file: ', FILENAME
END SUBROUTINE WRITE_BINARY_MATRIX


! Подпрограмма для записи вектора из mem в бинарный файл
SUBROUTINE WRITE_BINARY_VECTOR(FILENAME, N, F)
   IMPLICIT NONE
   CHARACTER(LEN=20), INTENT(IN) :: FILENAME
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(IN) :: F(N)  ! Вектор F хранится в следующих N элементах mem
   INTEGER :: I, IO_ERR

   ! Открытие бинарного файла для записи
   OPEN(UNIT=15, FILE=FILENAME, STATUS='REPLACE', ACCESS='DIRECT', RECL=4, IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening binary file: ', FILENAME
      STOP
   END IF

   ! Запись элементов вектора в бинарный файл
   DO I = 1, N
      WRITE(15, REC=I, IOSTAT=IO_ERR) F(I)
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error writing vector to file: ', FILENAME
         CLOSE(15)
         STOP
      END IF
   END DO

   CLOSE(15)
   WRITE(*, *) 'Writing binary vector to file: ', FILENAME
END SUBROUTINE WRITE_BINARY_VECTOR