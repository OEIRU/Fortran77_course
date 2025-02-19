      PROGRAM MAIN
      IMPLICIT NONE
      COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
      REAL*8 X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
      CALL READ_PARAMS()
      CALL CREATE_TABLE()
      END

      SUBROUTINE READ_PARAMS()
      IMPLICIT NONE
      COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
      REAL*8 X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
      OPEN(10, FILE='params.txt', STATUS='OLD', ERR=100)
      READ(10, *, ERR=100) X_MIN, X_MAX
      READ(10, *, ERR=100) Y_MIN, Y_MAX
      READ(10, *, ERR=100) X_STEP, Y_STEP
      CLOSE(10)
      RETURN
100   PRINT *, 'Error reading params file!'
      STOP
      END

      REAL*8 FUNCTION ARCCOS(SUM)
      IMPLICIT NONE
      REAL*8 SUM
      REAL*8 TO_DEG
      IF (SUM .LT. -1.0D0 .OR. SUM .GT. 1.0D0) THEN
        ARCCOS = -1.0D0
      ELSE
        TO_DEG = 180.0D0 / 3.14159265358979323846D0
        ARCCOS = ACOS(SUM) * TO_DEG
      END IF
      END

      SUBROUTINE GET_FORMAT(NUMBER, FMT)
      IMPLICIT NONE
      REAL*8 NUMBER
      INTEGER EXPONENT, DECIMAL_PLACES
      CHARACTER*20 FMT

      IF (NUMBER .NE. 0.0D0) THEN
          EXPONENT = IFIX(LOG10(ABS(NUMBER)))  ! Явное преобразование в INTEGER
      ELSE
          EXPONENT = 0
      END IF

      DECIMAL_PLACES = MAX(4 - EXPONENT - 1, 0)
      WRITE(FMT, '(A, I0, A)') '(F20.', DECIMAL_PLACES, ')'
      END

      SUBROUTINE NUMBER_TO_STRING(NUMBER, FMT, STR)
      IMPLICIT NONE
      REAL*8 NUMBER
      CHARACTER*20 FMT, STR
      WRITE(STR, FMT) NUMBER
      END

      SUBROUTINE WRITE_ROW(CUR_X, Y_VALS, M)
      IMPLICIT NONE
      REAL*8 CUR_X, Y_VALS(*), ARCCOS, RES
      INTEGER M, J
      CHARACTER*20 FMT, STR
      EXTERNAL ARCCOS

      CALL GET_FORMAT(CUR_X, FMT)
      CALL NUMBER_TO_STRING(CUR_X, FMT, STR)
      WRITE(10, '(A, A, A, $)') '| ', STR, ' '

      DO J = 1, M
        RES = ARCCOS(CUR_X + Y_VALS(J))
        IF (RES .EQ. -1.0D0) THEN
          WRITE(10, '(A, A, A, $)') '| ', 'N/D                 ', ' '
        ELSE
          CALL GET_FORMAT(RES, FMT)
          CALL NUMBER_TO_STRING(RES, FMT, STR)
          WRITE(10, '(A, A, A, $)') '| ', STR, ' '
        END IF
      END DO

      WRITE(10, '(A)') '|'
      END

      SUBROUTINE CREATE_TABLE()
      IMPLICIT NONE
      COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
      REAL*8 X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
      REAL*8 CUR_X, Y_VALS(1000)
      INTEGER N, M, I, J
      LOGICAL HAS_VALID_VALUE
      CHARACTER*20 FMT, STR

      IF (X_STEP .LE. 0.0D0 .OR. Y_STEP .LE. 0.0D0) THEN
        PRINT *, 'Error: Step must be positive!'
        STOP
      END IF

      N = INT((X_MAX - X_MIN) / X_STEP) + 1
      IF (X_MIN + (N - 1) * X_STEP .LT. X_MAX) N = N + 1

      M = INT((Y_MAX - Y_MIN) / Y_STEP) + 1
      IF (Y_MIN + (M - 1) * Y_STEP .LT. Y_MAX) M = M + 1

      IF (M .GT. 1000) THEN
        PRINT *, 'Error: Y_VALS array size exceeded!'
        STOP
      END IF

      HAS_VALID_VALUE = .FALSE.
      DO I = 1, N
        DO J = 1, M
          IF (ABS(X_MIN + (I - 1) * X_STEP + Y_MIN +
     & (J - 1) * Y_STEP) .LE. 1.0D0) THEN
            HAS_VALID_VALUE = .TRUE.
            GOTO 100
          END IF
        END DO
      END DO
100   CONTINUE

      IF (.NOT. HAS_VALID_VALUE) THEN
        PRINT *, 'Error: No valid values in the specified range!'
        STOP
      END IF

      DO J = 1, M
        Y_VALS(J) = Y_MIN + (J - 1) * Y_STEP
      END DO

      OPEN(10, FILE='table.txt', STATUS='UNKNOWN', ERR=200)
      WRITE(10, '(A, $)') '|       X/Y            '
      DO J = 1, M
        CALL GET_FORMAT(Y_VALS(J), FMT)
        CALL NUMBER_TO_STRING(Y_VALS(J), FMT, STR)
        WRITE(10, '(A, A, A, $)') '| ', STR, ' '
      END DO
      WRITE(10, '(A)') '|'

      WRITE(10, '(A, $)') '+----------------------'
      DO J = 1, M
        WRITE(10, '(A, $)') '+----------------------'
      END DO
      WRITE(10, '(A)') '+'

      DO I = 1, N
        CUR_X = X_MIN + (I - 1) * X_STEP
        IF (I .EQ. N) CUR_X = X_MAX
        CALL WRITE_ROW(CUR_X, Y_VALS, M)

        WRITE(10, '(A, $)') '+----------------------'
        DO J = 1, M
          WRITE(10, '(A, $)') '+----------------------'
        END DO
        WRITE(10, '(A)') '+'
      END DO

      CLOSE(10)
      RETURN
200   PRINT *, 'Error opening table file!'
      STOP
      END