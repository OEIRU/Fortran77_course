      PROGRAM BIN_TO_TXT_CONVERTER
      INTEGER N, I1, I2, IO, I
      REAL VALUE

      ! Чтение и декодирование matrix.bin
      OPEN(10, FILE='matrix.bin', ACCESS='DIRECT', RECL=4, IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot open file matrix.bin'
          STOP
      END IF

      OPEN(11, FILE='matrix_decoded.txt', STATUS='REPLACE', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot create file matrix_decoded.txt'
          STOP
      END IF

      READ(10, REC=1, IOSTAT=IO) N
      READ(10, REC=2, IOSTAT=IO) I1
      READ(10, REC=3, IOSTAT=IO) I2
      IF (IO .NE. 0) THEN
          PRINT *, 'Error reading matrix dimensions from matrix.bin'
          STOP
      END IF

      WRITE(11, *) N, I1, I2

      DO 100 I = 1, N
          READ(10, REC=3+I, IOSTAT=IO) VALUE
          IF (IO .NE. 0) GOTO 200
          WRITE(11, *) VALUE
  100  CONTINUE

      DO 110 I = 1, N-1
          READ(10, REC=3+N+I, IOSTAT=IO) VALUE
          IF (IO .NE. 0) GOTO 200
          WRITE(11, *) VALUE
  110  CONTINUE

      DO 120 I = 1, N-I2
          READ(10, REC=3+N+(N-1)+I, IOSTAT=IO) VALUE
          IF (IO .NE. 0) GOTO 200
          WRITE(11, *) VALUE
  120  CONTINUE

      DO 130 I = 1, N-1
          READ(10, REC=3+N+(N-1)+(N-I2)+I, IOSTAT=IO) VALUE
          IF (IO .NE. 0) GOTO 200
          WRITE(11, *) VALUE
  130  CONTINUE

      DO 140 I = 1, N-I1
          READ(10, REC=3+N+(N-1)+(N-I2)+(N-1)+I, IOSTAT=IO) VALUE
          IF (IO .NE. 0) GOTO 200
          WRITE(11, *) VALUE
  140  CONTINUE

  200 CONTINUE
      CLOSE(10)
      CLOSE(11)

      ! Чтение и декодирование vector.bin
      OPEN(20, FILE='vector.bin', ACCESS='DIRECT', RECL=4, IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot open file vector.bin'
          STOP
      END IF

      OPEN(21, FILE='vector_decoded.txt', STATUS='REPLACE', IOSTAT=IO)
      IF (IO .NE. 0) THEN
          PRINT *, 'Error: Cannot create file vector_decoded.txt'
          STOP
      END IF

      READ(20, REC=1, IOSTAT=IO) N
      IF (IO .NE. 0) THEN
          PRINT *, 'Error reading vector size from vector.bin'
          STOP
      END IF

      WRITE(21, *) N

      DO 150 I = 1, N
          READ(20, REC=1+I, IOSTAT=IO) VALUE
          IF (IO .NE. 0) GOTO 210
          WRITE(21, *) VALUE
  150  CONTINUE

  210 IF (IO .NE. 0) THEN
          PRINT *, 'Error reading vector data from vector.bin'
      END IF

      CLOSE(20)
      CLOSE(21)

      PRINT *, 'Decoding completed successfully.'
      END