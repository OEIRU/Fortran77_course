      PROGRAM CONVERT_TO_BINARY
      INTEGER N, I1, I2, I, IO
      PARAMETER (MAX_SIZE = 100)  ! Adjust MAX_SIZE as needed
      REAL MAIN_DIAG(MAX_SIZE)
      REAL UPPER_DIAG_1(MAX_SIZE-1)
      REAL UPPER_DIAG_2(MAX_SIZE)
      REAL LOWER_DIAG_1(MAX_SIZE-1)
      REAL LOWER_DIAG_2(MAX_SIZE)

      ! Чтение текстового файла matrix.txt
      OPEN(10, FILE='matrix.txt', STATUS='OLD', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot open file matrix.txt'
          STOP
      END IF

      ! Чтение размеров матрицы
      READ(10, *, IOSTAT=IO) N, I1, I2
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read dimensions from matrix.txt'
          STOP
      END IF

      ! Чтение диагоналей матрицы
      READ(10, *, IOSTAT=IO) (MAIN_DIAG(I), I=1, N)
      READ(10, *, IOSTAT=IO) (UPPER_DIAG_1(I), I=1, N-1)
      READ(10, *, IOSTAT=IO) (UPPER_DIAG_2(I), I=1, N-I2)
      READ(10, *, IOSTAT=IO) (LOWER_DIAG_1(I), I=1, N-1)
      READ(10, *, IOSTAT=IO) (LOWER_DIAG_2(I), I=1, N-I1)
      CLOSE(10)

      ! Запись данных в бинарный файл matrix.bin
      OPEN(20, FILE='matrix.bin', STATUS='REPLACE', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot open file matrix.bin for writing'
          STOP
      END IF

      WRITE(20) N, I1, I2                 ! Записываем размеры
      WRITE(20) (MAIN_DIAG(I), I=1, N)    ! Записываем главную диагональ
      WRITE(20) (UPPER_DIAG_1(I), I=1, N-1) ! Записываем первую верхнюю диагональ
      WRITE(20) (UPPER_DIAG_2(I), I=1, N-I2) ! Записываем вторую верхнюю диагональ
      WRITE(20) (LOWER_DIAG_1(I), I=1, N-1) ! Записываем первую нижнюю диагональ
      WRITE(20) (LOWER_DIAG_2(I), I=1, N-I1) ! Записываем вторую нижнюю диагональ
      CLOSE(20)

      PRINT *, "Matrix successfully written to matrix.bin"
      END
