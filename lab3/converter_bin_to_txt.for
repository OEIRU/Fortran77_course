PROGRAM CONVERT_TO_TEXT
   IMPLICIT NONE
   INTEGER :: N, I1, I2
   REAL :: mem(1000000)  ! Общий массив для всех данных

   CHARACTER(LEN=20) :: DATA_FILE, MATRIX_BIN, VECTOR_BIN, MATRIX_FILE, VECTOR_FILE

   ! Инициализация имен файлов
   DATA_FILE = 'data.txt'
   MATRIX_BIN = 'matrix.bin'
   VECTOR_BIN = 'vector.bin'
   MATRIX_FILE = 'matrix.txt'
   VECTOR_FILE = 'vector.txt'

   ! Чтение данных из текстового файла
   CALL READ_DATA(DATA_FILE, N, I1, I2)

   ! Чтение матрицы из бинарного файла в mem
   CALL READ_BINARY_MATRIX(MATRIX_BIN, N, I1, I2, mem(1))

   ! Чтение вектора из бинарного файла в mem
   CALL READ_BINARY_VECTOR(VECTOR_BIN, N, mem(5*N + 1))

   ! Запись матрицы из mem в текстовый файл
   CALL WRITE_MATRIX(MATRIX_FILE, N, I1, I2, mem(1))

   ! Запись вектора из mem в текстовый файл
   CALL WRITE_VECTOR(VECTOR_FILE, N, mem(5*N + 1))

END PROGRAM CONVERT_TO_TEXT


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


! Подпрограмма для чтения матрицы из бинарного файла в mem
SUBROUTINE READ_BINARY_MATRIX(FILENAME, N, I1, I2, A)
   IMPLICIT NONE
   CHARACTER(LEN=20), INTENT(IN) :: FILENAME
   INTEGER, INTENT(IN) :: N, I1, I2
   REAL, INTENT(OUT) :: A(5, N)  ! Матрица A хранится в первых 5*N элементах mem
   INTEGER :: I, IO_ERR

   ! Открытие бинарного файла для чтения
   OPEN(UNIT=11, FILE=FILENAME, STATUS='OLD', ACCESS='DIRECT', RECL=4, IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening binary file: ', FILENAME
      STOP
   END IF

   ! Чтение элементов матрицы из бинарного файла
   DO I = 1, N-I1
      READ(11, REC=I, IOSTAT=IO_ERR) A(1, I)  ! Верхняя диагональ
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error reading upper diagonal from file: ', FILENAME
         CLOSE(11)
         STOP
      END IF
   END DO

   DO I = 1, N-1
      READ(11, REC=N-I1+I, IOSTAT=IO_ERR) A(2, I)  ! Над главной диагональю
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error reading above main diagonal from file: ', FILENAME
         CLOSE(11)
         STOP
      END IF
   END DO

   DO I = 1, N
      READ(11, REC=2*N-I1+I, IOSTAT=IO_ERR) A(3, I)  ! Главная диагональ
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error reading main diagonal from file: ', FILENAME
         CLOSE(11)
         STOP
      END IF
   END DO

   DO I = 1, N-1
      READ(11, REC=3*N-I1+I, IOSTAT=IO_ERR) A(4, I)  ! Под главной диагональю
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error reading below main diagonal from file: ', FILENAME
         CLOSE(11)
         STOP
      END IF
   END DO

   DO I = 1, N-I2
      READ(11, REC=4*N-I1+I, IOSTAT=IO_ERR) A(5, I)  ! Нижняя диагональ
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error reading lower diagonal from file: ', FILENAME
         CLOSE(11)
         STOP
      END IF
   END DO

   CLOSE(11)
   WRITE(*, *) 'Reading binary matrix from file: ', FILENAME
END SUBROUTINE READ_BINARY_MATRIX


! Подпрограмма для чтения вектора из бинарного файла в mem
SUBROUTINE READ_BINARY_VECTOR(FILENAME, N, F)
   IMPLICIT NONE
   CHARACTER(LEN=20), INTENT(IN) :: FILENAME
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(OUT) :: F(N)  ! Вектор F хранится в следующих N элементах mem
   INTEGER :: I, IO_ERR

   ! Открытие бинарного файла для чтения
   OPEN(UNIT=12, FILE=FILENAME, STATUS='OLD', ACCESS='DIRECT', RECL=4, IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening binary file: ', FILENAME
      STOP
   END IF

   ! Чтение элементов вектора из бинарного файла
   DO I = 1, N
      READ(12, REC=I, IOSTAT=IO_ERR) F(I)
      IF (IO_ERR /= 0) THEN
         WRITE(*, *) 'Error reading vector from file: ', FILENAME
         CLOSE(12)
         STOP
      END IF
   END DO

   CLOSE(12)
   WRITE(*, *) 'Reading binary vector from file: ', FILENAME
END SUBROUTINE READ_BINARY_VECTOR


! Подпрограмма для записи матрицы из mem в текстовый файл
SUBROUTINE WRITE_MATRIX(FILENAME, N, I1, I2, A)
   IMPLICIT NONE
   CHARACTER(LEN=20), INTENT(IN) :: FILENAME
   INTEGER, INTENT(IN) :: N, I1, I2
   REAL, INTENT(IN) :: A(5, N)  ! Матрица A хранится в первых 5*N элементах mem
   INTEGER :: I, IO_ERR

   ! Открытие текстового файла для записи
   OPEN(UNIT=13, FILE=FILENAME, STATUS='REPLACE', ACTION='WRITE', IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening text file: ', FILENAME
      STOP
   END IF

   ! Запись элементов матрицы в текстовый файл
   WRITE(13, *, IOSTAT=IO_ERR) (A(1, I), I=1, N-I1)  ! Верхняя диагональ
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error writing upper diagonal to file: ', FILENAME
      CLOSE(13)
      STOP
   END IF

   WRITE(13, *, IOSTAT=IO_ERR) (A(2, I), I=1, N-1)   ! Над главной диагональю
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error writing above main diagonal to file: ', FILENAME
      CLOSE(13)
      STOP
   END IF

   WRITE(13, *, IOSTAT=IO_ERR) (A(3, I), I=1, N)     ! Главная диагональ
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error writing main diagonal to file: ', FILENAME
      CLOSE(13)
      STOP
   END IF

   WRITE(13, *, IOSTAT=IO_ERR) (A(4, I), I=1, N-1)   ! Под главной диагональю
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error writing below main diagonal to file: ', FILENAME
      CLOSE(13)
      STOP
   END IF

   WRITE(13, *, IOSTAT=IO_ERR) (A(5, I), I=1, N-I2)  ! Нижняя диагональ
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error writing lower diagonal to file: ', FILENAME
      CLOSE(13)
      STOP
   END IF

   CLOSE(13)
   WRITE(*, *) 'Writing matrix to text file: ', FILENAME
END SUBROUTINE WRITE_MATRIX


! Подпрограмма для записи вектора из mem в текстовый файл
SUBROUTINE WRITE_VECTOR(FILENAME, N, F)
   IMPLICIT NONE
   CHARACTER(LEN=20), INTENT(IN) :: FILENAME
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(IN) :: F(N)  ! Вектор F хранится в следующих N элементах mem
   INTEGER :: I, IO_ERR

   ! Открытие текстового файла для записи
   OPEN(UNIT=14, FILE=FILENAME, STATUS='REPLACE', ACTION='WRITE', IOSTAT=IO_ERR)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error opening text file: ', FILENAME
      STOP
   END IF

   ! Запись элементов вектора в текстовый файл
   WRITE(14, *, IOSTAT=IO_ERR) (F(I), I=1, N)
   IF (IO_ERR /= 0) THEN
      WRITE(*, *) 'Error writing vector to file: ', FILENAME
      CLOSE(14)
      STOP
   END IF

   CLOSE(14)
   WRITE(*, *) 'Writing vector to text file: ', FILENAME
END SUBROUTINE WRITE_VECTOR