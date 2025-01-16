      PROGRAM CONVERT_TO_BINARY
      IMPLICIT NONE
      INTEGER N, I1, I2
      REAL MEM(1000000)  ! Общий статический массив для всех данных
      CHARACTER*20 DATA_FILE, MATRIX_FILE, VECTOR_FILE, MATRIX_BIN
      CHARACTER*20 VECTOR_BIN
      DATA_FILE = 'data.txt'
      MATRIX_FILE = 'matrix.txt'
      VECTOR_FILE = 'vector.txt'
      MATRIX_BIN = 'matrix.bin'
      VECTOR_BIN = 'vector.bin'
      CALL READ_DATA(DATA_FILE, N, I1, I2)
      CALL READ_MATRIX(MATRIX_FILE, N, I1, I2, MEM(1))
      CALL READ_VECTOR(VECTOR_FILE, N, MEM(5*N + 1))
      CALL WRITE_BINARY_MATRIX(MATRIX_BIN, N, I1, I2, MEM(1))
      CALL WRITE_BINARY_VECTOR(VECTOR_BIN, N, MEM(5*N + 1))
      END

      SUBROUTINE READ_DATA(FILENAME, N, I1, I2)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2
      OPEN(10, FILE=FILENAME, STATUS='OLD')
      READ(10, *) N, I1, I2
      CLOSE(10)
      WRITE(*, *) 'Reading data from file:', FILENAME
      WRITE(*, *) 'N =', N, 'I1 =', I1, 'I2 =', I2
      RETURN
      END

      SUBROUTINE READ_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I
      REAL A(5, N)  ! Матрица A хранится в первых 5*N элементах MEM
      OPEN(11, FILE=FILENAME, STATUS='OLD')
      READ(11, *) (A(1, I), I=1, N-I1) 
      READ(11, *) (A(2, I), I=1, N-1)  
      READ(11, *) (A(3, I), I=1, N)     
      READ(11, *) (A(4, I), I=1, N-1)   
      READ(11, *) (A(5, I), I=1, N-I2)  
      CLOSE(11)
      WRITE(*, *) 'Reading matrix from file:', FILENAME
      RETURN
      END

      SUBROUTINE READ_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I
      REAL F(N)  ! Вектор F хранится в следующих N элементах MEM
      OPEN(12, FILE=FILENAME, STATUS='OLD')
      READ(12, *) (F(I), I=1, N)
      CLOSE(12)
      WRITE(*, *) 'Reading vector from file:', FILENAME
      RETURN
      END

      SUBROUTINE WRITE_BINARY_MATRIX(FILENAME, N, I1, I2, A)
      CHARACTER*20 FILENAME
      INTEGER N, I1, I2, I, RECL
      REAL A(5, N)  ! Матрица A хранится в первых 5*N элементах MEM
      RECL = 4 * (N - I1 + N - 1 + N + N - 1 + N - I2)  ! Размер записи в байтах
      OPEN(14, FILE=FILENAME, STATUS='UNKNOWN',
     & ACCESS='DIRECT', RECL=RECL)
      WRITE(14, REC=1) (A(1, I), I=1, N-I1),  
     &                (A(2, I), I=1, N-1),  
     &                (A(3, I), I=1, N),    
     &                (A(4, I), I=1, N-1),  
     &                (A(5, I), I=1, N-I2)  
      CLOSE(14)
      WRITE(*, *) 'Writing binary matrix to file:', FILENAME
      RETURN
      END

      SUBROUTINE WRITE_BINARY_VECTOR(FILENAME, N, F)
      CHARACTER*20 FILENAME
      INTEGER N, I, RECL
      REAL F(N)  ! Вектор F хранится в следующих N элементах MEM
      RECL = 4 * N  ! Размер записи в байтах (4 байта на каждый REAL)
      OPEN(15, FILE=FILENAME, STATUS='UNKNOWN', 
     & ACCESS='DIRECT', RECL=RECL)
      WRITE(15, REC=1) (F(I), I=1, N)
      CLOSE(15)
      WRITE(*, *) 'Writing binary vector to file:', FILENAME
      RETURN
      END