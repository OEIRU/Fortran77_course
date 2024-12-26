PROGRAM CONVERT_TO_BINARY
   INTEGER N, I1, I2
   REAL A(5, 1000000), F(1000000)

   CHARACTER*20 DATA_FILE, MATRIX_FILE, VECTOR_FILE, MATRIX_BIN
   CHARACTER*20 VECTOR_BIN

   ! Инициализация имен файлов
   DATA_FILE = 'data.txt'
   MATRIX_FILE = 'matrix.txt'
   VECTOR_FILE = 'vector.txt'
   MATRIX_BIN = 'matrix.bin'
   VECTOR_BIN = 'vector.bin'

   ! Чтение данных из текстового файла
   CALL READ_DATA(DATA_FILE, N, I1, I2)
   ! Чтение матрицы из текстового файла
   CALL READ_MATRIX(MATRIX_FILE, N, I1, I2, A)
   ! Чтение вектора из текстового файла
   CALL READ_VECTOR(VECTOR_FILE, N, F)
   ! Запись матрицы в бинарный файл
   CALL WRITE_BINARY_MATRIX(MATRIX_BIN, N, I1, I2, A)
   ! Запись вектора в бинарный файл
   CALL WRITE_BINARY_VECTOR(VECTOR_BIN, N, F)

END

! Подпрограмма для чтения данных из файла
SUBROUTINE READ_DATA(FILENAME, N, I1, I2)
   CHARACTER*20 FILENAME
   INTEGER N, I1, I2

   ! Открытие файла для чтения
   OPEN(10, FILE=FILENAME, STATUS='OLD')
   ! Чтение значений N, I1, I2 из файла
   READ(10, *) N, I1, I2
   CLOSE(10)
   WRITE(*, *) 'Reading data from file:', FILENAME
   WRITE(*, *) 'N =', N, 'I1 =', I1, 'I2 =', I2
END

! Подпрограмма для чтения матрицы из файла
SUBROUTINE READ_MATRIX(FILENAME, N, I1, I2, A)
   CHARACTER*20 FILENAME
   INTEGER N, I1, I2, I
   REAL A(5, 1000000)

   ! Открытие файла для чтения
   OPEN(11, FILE=FILENAME, STATUS='OLD')

   ! Чтение элементов матрицы из файла
   READ(11, *) (A(1, I), I=1, N-I1)  ! Верхняя диагональ
   READ(11, *) (A(2, I), I=1, N-1)   ! Над главной диагональю
   READ(11, *) (A(3, I), I=1, N)     ! Главная диагональ
   READ(11, *) (A(4, I), I=1, N-1)   ! Под главной диагональю
   READ(11, *) (A(5, I), I=1, N-I2)  ! Нижняя диагональ

   CLOSE(11)
   WRITE(*, *) 'Reading matrix from file:', FILENAME
END

! Подпрограмма для чтения вектора из файла
SUBROUTINE READ_VECTOR(FILENAME, N, F)
   CHARACTER*20 FILENAME
   INTEGER N, I
   REAL F(1000000)

   ! Открытие файла для чтения
   OPEN(12, FILE=FILENAME, STATUS='OLD')
   ! Чтение элементов вектора из файла
   READ(12, *) (F(I), I=1, N)
   CLOSE(12)
   WRITE(*, *) 'Reading vector from file:', FILENAME
END

! Подпрограмма для записи матрицы в бинарный файл
SUBROUTINE WRITE_BINARY_MATRIX(FILENAME, N, I1, I2, A)
   CHARACTER*20 FILENAME
   INTEGER N, I1, I2, I
   REAL A(5, 1000000)

   ! Открытие бинарного файла для записи
   OPEN(14, FILE=FILENAME, STATUS='REPLACE', ACCESS='DIRECT', RECL=4)

   ! Запись элементов верхней диагонали
   DO I = 1, N-I1
      WRITE(14, REC=I) A(1, I)
   END DO
   ! Запись элементов над главной диагональю
   DO I = 1, N-1
      WRITE(14, REC=N-I1+I) A(2, I)
   END DO
   ! Запись элементов главной диагонали
   DO I = 1, N
      WRITE(14, REC=2*N-I1+I) A(3, I)
   END DO
   ! Запись элементов под главной диагональю
   DO I = 1, N-1
      WRITE(14, REC=3*N-I1+I) A(4, I)
   END DO
   ! Запись элементов нижней диагонали
   DO I = 1, N-I2
      WRITE(14, REC=4*N-I1+I) A(5, I)
   END DO

   CLOSE(14)
   WRITE(*, *) 'Writing binary matrix to file:', FILENAME
END

! Подпрограмма для записи вектора в бинарный файл
SUBROUTINE WRITE_BINARY_VECTOR(FILENAME, N, F)
   CHARACTER*20 FILENAME
   INTEGER N, I
   REAL F(1000000)

   ! Открытие бинарного файла для записи
   OPEN(15, FILE=FILENAME, STATUS='REPLACE', ACCESS='DIRECT', RECL=4)

   ! Запись элементов вектора в файл
   DO I = 1, N
      WRITE(15, REC=I) F(I)
   END DO

   CLOSE(15)
   WRITE(*, *) 'Writing binary vector to file:', FILENAME
END
