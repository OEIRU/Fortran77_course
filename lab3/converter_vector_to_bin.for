      PROGRAM CONVERT_TO_BINARY_VECTOR
      INTEGER N, I, IO
      PARAMETER (MAX_SIZE = 100)  ! Adjust MAX_SIZE as needed
      REAL F(MAX_SIZE)

      ! Чтение текстового файла vector.txt
      OPEN(11, FILE='vector.txt', STATUS='OLD', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot open file vector.txt'
          STOP
      END IF

      ! Чтение размера вектора
      READ(11, *, IOSTAT=IO) N
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read vector size from vector.txt'
          STOP
      END IF

      ! Чтение данных вектора
      READ(11, *, IOSTAT=IO) (F(I), I=1, N)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Failed to read vector data from vector.txt'
          STOP
      END IF
      CLOSE(11)

      ! Запись вектора в бинарный файл
      OPEN(21, FILE='vector.bin', STATUS='REPLACE', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot open file vector.bin for writing'
          STOP
      END IF

      WRITE(21) N          ! Записываем размер вектора
      WRITE(21) (F(I), I=1, N)  ! Записываем данные вектора
      CLOSE(21)

      PRINT *, "Vector successfully written to vector.bin"
      END
