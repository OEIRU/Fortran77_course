      PROGRAM CONVERT_BINARY_TO_TEXT
      INTEGER N, I1, I2
      CHARACTER*20 MATRIX_BIN, VECTOR_BIN, MATRIX_TXT, VECTOR_TXT
      PARAMETER (MAX_SIZE = 1000000)
      REAL A(5, MAX_SIZE), F(MAX_SIZE)

      MATRIX_BIN = 'matrix.bin'
      VECTOR_BIN = 'vector.bin'
      MATRIX_TXT = 'matrix.txt'
      VECTOR_TXT = 'vector.txt'

      CALL READ_DATA('data.txt', N, I1, I2)

      CALL READ_BINARY_MATRIX(MATRIX_BIN, N, I1, I2, A)
      CALL WRITE_TEXT_MATRIX(MATRIX_TXT, N, I1, I2, A)

      CALL READ_BINARY_VECTOR(VECTOR_BIN, N, F)
      CALL WRITE_TEXT_VECTOR(VECTOR_TXT, N, F)

      END

      SUBROUTINE READ_DATA(FILENAME, N, I1, I2)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2

      OPEN(10, FILE=FILENAME, STATUS='OLD')
      READ(10, *) N, I1, I2
      CLOSE(10)
      WRITE(*, *) 'Reading data from file:', FILENAME
      WRITE(*, *) 'N =', N, 'I1 =', I1, 'I2 =', I2
      END

      SUBROUTINE READ_BINARY_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I
      REAL A(5, *)
      INTEGER RECL_SIZE
      !INTEGER INQUIRE(IOSTAT=IOSTAT, IOMSG=IOMSG, SIZE=FSIZE)
      CHARACTER(100) IOMSG
      INTEGER FSIZE

      INQUIRE(FILE=FILENAME, SIZE=FSIZE)
      WRITE(*, *) 'File size of', FILENAME, 'is', FSIZE, 'bytes'

      OPEN(13, FILE=FILENAME, STATUS='OLD', 
     & FORM='UNFORMATTED', ACCESS='SEQUENTIAL')

      READ(13, IOSTAT=IOSTAT, IOMSG=IOMSG) (A(1, I), I=1, N-I1)  ! Верхняя диагональ
      IF (IOSTAT /= 0) THEN
         WRITE(*, *) 'Error reading upper diagonal:', IOMSG
         RETURN
      END IF
      WRITE(*, *) 'Read upper diagonal:', (A(1, I), I=1, N-I1)

      READ(13, IOSTAT=IOSTAT, IOMSG=IOMSG) (A(2, I), I=1, N-1)   ! Над главной диагональю
      IF (IOSTAT /= 0) THEN
         WRITE(*, *) 'Error reading superdiagonal:', IOMSG
         RETURN
      END IF
      WRITE(*, *) 'Read superdiagonal:', (A(2, I), I=1, N-1)

      READ(13, IOSTAT=IOSTAT, IOMSG=IOMSG) (A(3, I), I=1, N)     ! Главная диагональ
      IF (IOSTAT /= 0) THEN
         WRITE(*, *) 'Error reading main diagonal:', IOMSG
         RETURN
      END IF
      WRITE(*, *) 'Read main diagonal:', (A(3, I), I=1, N)

      READ(13, IOSTAT=IOSTAT, IOMSG=IOMSG) (A(4, I), I=1, N-1)   ! Под главной диагональю
      IF (IOSTAT /= 0) THEN
         WRITE(*, *) 'Error reading subdiagonal:', IOMSG
         RETURN
      END IF
      WRITE(*, *) 'Read subdiagonal:', (A(4, I), I=1, N-1)

      READ(13, IOSTAT=IOSTAT, IOMSG=IOMSG) (A(5, I), I=1, N-I2)  ! Нижняя диагональ
      IF (IOSTAT /= 0) THEN
         WRITE(*, *) 'Error reading lower diagonal:', IOMSG
         RETURN
      END IF
      WRITE(*, *) 'Read lower diagonal:', (A(5, I), I=1, N-I2)

      CLOSE(13)
      END

      SUBROUTINE READ_BINARY_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(*)
      INTEGER RECL_SIZE
      !INTEGER INQUIRE(IOSTAT=IOSTAT, IOMSG=IOMSG, SIZE=FSIZE)
      CHARACTER(100) IOMSG
      INTEGER FSIZE

      INQUIRE(FILE=FILENAME, SIZE=FSIZE)
      WRITE(*, *) 'File size of', FILENAME, 'is', FSIZE, 'bytes'

      OPEN(14, FILE=FILENAME, STATUS='OLD', 
     & FORM='UNFORMATTED', ACCESS='SEQUENTIAL')
      READ(14, IOSTAT=IOSTAT, IOMSG=IOMSG) (F(I), I=1, N)
      IF (IOSTAT /= 0) THEN
         WRITE(*, *) 'Error reading vector:', IOMSG
         RETURN
      END IF
      WRITE(*, *) 'Read vector:', (F(I), I=1, N)

      CLOSE(14)
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
