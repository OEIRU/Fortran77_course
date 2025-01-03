      PROGRAM CONVERT_TO_BINARY
      IMPLICIT NONE
      INTEGER N, I1, I2
      REAL mem(1000000)  ! Общий массив для всех данных
      CHARACTER*20 DATA_FILE, MATRIX_FILE, VECTOR_FILE, MATRIX_BIN
      CHARACTER*20 VECTOR_BIN
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
      
      END
      
      
      SUBROUTINE READ_DATA(FILENAME, N, I1, I2)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, IO_ERR
      
      OPEN(10, FILE=FILENAME, STATUS='OLD', IOSTAT=IO_ERR)
      IF (IO_ERR .NE. 0) THEN
         WRITE(*, *) 'Error opening file:', FILENAME
         STOP
      END IF
      READ(10, *, IOSTAT=IO_ERR) N, I1, I2
      IF (IO_ERR .NE. 0) THEN
         WRITE(*, *) 'Error reading data from file:', FILENAME
         STOP
      END IF
      CLOSE(10)
      WRITE(*, *) 'Reading data from file:', FILENAME
      WRITE(*, *) 'N =', N, 'I1 =', I1, 'I2 =', I2
      RETURN
      END
      
      
      SUBROUTINE READ_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I, IO_ERR
      REAL A(5, N)  ! Матрица A хранится в первых 5*N элементах mem
      
      OPEN(11, FILE=FILENAME, STATUS='OLD', IOSTAT=IO_ERR)
      IF (IO_ERR .NE. 0) THEN
         WRITE(*, *) 'Error opening file:', FILENAME
         STOP
      END IF
      
      IF (I1 .GT. 0) THEN
         READ(11, *, IOSTAT=IO_ERR) (A(1, I), I=1, N-I1)  ! Верхняя диагональ
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error reading upper diagonal.'
            STOP
         END IF
      END IF
      
      IF (N-1 .GT. 0) THEN
         READ(11, *, IOSTAT=IO_ERR) (A(2, I), I=1, N-1)   ! Над главной диагональю
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error reading above main diagonal.'
            STOP
         END IF
      END IF
      
      READ(11, *, IOSTAT=IO_ERR) (A(3, I), I=1, N)        ! Главная диагональ
      IF (IO_ERR .NE. 0) THEN
         WRITE(*, *) 'Error reading main diagonal.'
         STOP
      END IF
      
      IF (N-1 .GT. 0) THEN
         READ(11, *, IOSTAT=IO_ERR) (A(4, I), I=1, N-1)   ! Под главной диагональю
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error reading below main diagonal.'
            STOP
         END IF
      END IF
      
      IF (I2 .GT. 0) THEN
         READ(11, *, IOSTAT=IO_ERR) (A(5, I), I=1, N-I2)  ! Нижняя диагональ
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error reading lower diagonal.'
            STOP
         END IF
      END IF
      
      CLOSE(11)
      WRITE(*, *) 'Reading matrix from file:', FILENAME
      RETURN
      END
      
      
      SUBROUTINE READ_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I, IO_ERR
      REAL F(N)  ! Вектор F хранится в следующих N элементах mem
      
      OPEN(12, FILE=FILENAME, STATUS='OLD', IOSTAT=IO_ERR)
      IF (IO_ERR .NE. 0) THEN
         WRITE(*, *) 'Error opening file:', FILENAME
         STOP
      END IF
      READ(12, *, IOSTAT=IO_ERR) (F(I), I=1, N)
      IF (IO_ERR .NE. 0) THEN
         WRITE(*, *) 'Error reading vector from file:', FILENAME
         STOP
      END IF
      CLOSE(12)
      WRITE(*, *) 'Reading vector from file:', FILENAME
      RETURN
      END
      
      
      SUBROUTINE WRITE_BINARY_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I, IO_ERR
      REAL A(5, N)  ! Матрица A хранится в первых 5*N элементах mem
      
      OPEN(14, FILE=FILENAME, STATUS='REPLACE', ACCESS='DIRECT',
     *     RECL=4, IOSTAT=IO_ERR)
      IF (IO_ERR .NE. 0) THEN
         WRITE(*, *) 'Error opening binary file:', FILENAME
         STOP
      END IF
      
      IF (I1 .GT. 0) THEN
         DO 100 I = 1, N-I1
            WRITE(14, REC=I, IOSTAT=IO_ERR) A(1, I)  ! Верхняя диагональ
            IF (IO_ERR .NE. 0) THEN
               WRITE(*, *) 'Error writing upper diagonal.'
               STOP
            END IF
 100     CONTINUE
      END IF
      
      IF (N-1 .GT. 0) THEN
         DO 200 I = 1, N-1
            WRITE(14, REC=N-I1+I, IOSTAT=IO_ERR) A(2, I)  ! Над главной диагональю
            IF (IO_ERR .NE. 0) THEN
               WRITE(*, *) 'Error writing above main diagonal.'
               STOP
            END IF
 200     CONTINUE
      END IF
      
      DO 300 I = 1, N
         WRITE(14, REC=2*N-I1+I, IOSTAT=IO_ERR) A(3, I)  ! Главная диагональ
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error writing main diagonal.'
            STOP
         END IF
 300  CONTINUE
      
      IF (N-1 .GT. 0) THEN
         DO 400 I = 1, N-1
            WRITE(14, REC=3*N-I1+I, IOSTAT=IO_ERR) A(4, I)  ! Под главной диагональю
            IF (IO_ERR .NE. 0) THEN
               WRITE(*, *) 'Error writing below main diagonal.'
               STOP
            END IF
 400     CONTINUE
      END IF
      
      IF (I2 .GT. 0) THEN
         DO 500 I = 1, N-I2
            WRITE(14, REC=4*N-I1+I, IOSTAT=IO_ERR) A(5, I)  ! Нижняя диагональ
            IF (IO_ERR .NE. 0) THEN
               WRITE(*, *) 'Error writing lower diagonal.'
               STOP
            END IF
 500     CONTINUE
      END IF
      
      CLOSE(14)
      WRITE(*, *) 'Writing binary matrix to file:', FILENAME
      RETURN
      END
      
      
      SUBROUTINE WRITE_BINARY_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I, IO_ERR
      REAL F(N)  ! Вектор F хранится в следующих N элементах mem
      
      OPEN(15, FILE=FILENAME, STATUS='REPLACE', ACCESS='DIRECT',
     *     RECL=4, IOSTAT=IO_ERR)
      IF (IO_ERR .NE. 0) THEN
         WRITE(*, *) 'Error opening binary file:', FILENAME
         STOP
      END IF
      
      DO 600 I = 1, N
         WRITE(15, REC=I, IOSTAT=IO_ERR) F(I)
         IF (IO_ERR .NE. 0) THEN
            WRITE(*, *) 'Error writing vector to file:', FILENAME
            STOP
         END IF
 600  CONTINUE
      
      CLOSE(15)
      WRITE(*, *) 'Writing binary vector to file:', FILENAME
      RETURN
      END