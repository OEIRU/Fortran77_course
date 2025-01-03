      PROGRAM MAIN
          IMPLICIT NONE
          COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          REAL X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP

          CALL READ_PARAMS()
          CALL CREATE_TABLE()

      END

      SUBROUTINE READ_PARAMS()
          IMPLICIT NONE
          COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          REAL X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP

          OPEN(10, FILE='params.txt', ERR=100)
          READ(10, *, ERR=101) X_MIN, X_MAX
          READ(10, *, ERR=101) Y_MIN, Y_MAX
          READ(10, *, ERR=101) X_STEP, Y_STEP
          CLOSE(10)

          IF (X_MAX .LE. X_MIN .OR. Y_MAX .LE. Y_MIN) THEN
              GOTO 102
          END IF

          IF (X_STEP .LE. 0.0 .OR. Y_STEP .LE. 0.0) THEN
              GOTO 103
          END IF

          GOTO 109

100       PAUSE 'Open params file error!'
          STOP

101       PAUSE 'Read params file error!'
          CLOSE(10)
          STOP

102       PAUSE 'Incorrect data, max must be greater than min!'
          STOP

103       PAUSE 'Incorrect data, step must be greater than 0!'
          STOP

109       CONTINUE
      END

      REAL FUNCTION TO_RAD(ANGLE)
          IMPLICIT NONE
          REAL ANGLE
          TO_RAD = ANGLE / 180.0 * 3.1415926
      END

      INTEGER FUNCTION IS_INVISIBLE(FIRST, SECOND)
          IMPLICIT NONE
          REAL FIRST, SECOND
          REAL EPSILON
          PARAMETER (EPSILON = 1.0E-6)

          IF (ABS(FIRST - SECOND) .LT. EPSILON) THEN
              IS_INVISIBLE = 1
          ELSE
              IS_INVISIBLE = 0
          END IF
      END

      REAL FUNCTION ARCCOS(SUM)
          IMPLICIT NONE
          REAL SUM
          REAL EPSILON
          PARAMETER (EPSILON = 1.0E-6)

          IF (SUM .LT. -1.0 .OR. SUM .GT. 1.0) THEN
              ARCCOS = -1.0
          ELSE IF (ABS(SUM - 1.0) .LT. EPSILON) THEN
              ARCCOS = 0.0
          ELSE
              ARCCOS = ACOS(SUM)
          END IF
      END

      SUBROUTINE WRITE_ROW(X, M, REAL_M)
          IMPLICIT NONE
          COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          REAL X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          REAL X, Y, ARCCOS_RES, ARCCOS
          INTEGER M, J, IS_INVISIBLE, REAL_M

          WRITE(10, 11, ERR=101) '|'
          WRITE(10, 10, ERR=101) X, '|'

          Y = Y_MIN
          ARCCOS_RES = ARCCOS(X + Y)
          IF (ARCCOS_RES .GE. 0.0) THEN
              WRITE(10, 10, ERR=101) ARCCOS_RES, '|'
          ELSE
              WRITE(10, 11, ERR=101) '        N/D|'
          END IF

          DO J = 1, M - 2, 1
              IF (Y + Y_STEP .GT. Y_MAX) THEN
                  EXIT
              END IF
              IF (IS_INVISIBLE(Y, Y + Y_STEP) .EQ. 1) THEN
                  Y = Y + Y_STEP
                  CYCLE
              END IF
              Y = Y + Y_STEP
              IF (ABS(Y) .LT. ABS(Y_STEP / 2)) THEN
                  Y = 0.0
              END IF
              ARCCOS_RES = ARCCOS(X + Y)
              IF (ARCCOS_RES .GE. 0.0) THEN
                  WRITE(10, 10, ERR=101) ARCCOS_RES, '|'
              ELSE
                  WRITE(10, 11, ERR=101) '        N/D|'
              END IF
          END DO

          IF (IS_INVISIBLE(Y, Y_MAX) .EQ. 0) THEN
              ARCCOS_RES = ARCCOS(X + Y_MAX)
              IF (ARCCOS_RES .GE. 0.0) THEN
                  WRITE(10, 10, ERR=101) ARCCOS_RES, '|'
              ELSE
                  WRITE(10, 11, ERR=101) '        N/D|'
              END IF
          END IF

          WRITE(10, 12, ERR=101)

          DO J = 1, REAL_M, 1
              WRITE(10, 11, ERR=101) '------------'
          END DO
          WRITE(10, 11, ERR=101) '-------------'
          WRITE(10, 12, ERR=101)

          GOTO 109

10        FORMAT(E11.4, A, $)
11        FORMAT(A, $)
12        FORMAT()

101       PAUSE 'Write table file error!'
          CLOSE(10)
          STOP

109       CONTINUE
      END

      SUBROUTINE CREATE_TABLE()
          IMPLICIT NONE
          COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          REAL X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP, X, Y
          INTEGER N, M, I, J, REAL_M, IS_INVISIBLE

          REAL_M = 1

          N = INT((X_MAX - X_MIN) / X_STEP) + 1
          IF (X_MIN + (N - 1) * X_STEP .LT. X_MAX) THEN
              N = N + 1
          END IF

          M = INT((Y_MAX - Y_MIN) / Y_STEP) + 1
          IF (Y_MIN + (M - 1) * Y_STEP .LT. Y_MAX) THEN
              M = M + 1
          END IF

          OPEN(10, FILE='table.txt', ERR=200)

          WRITE(10, 21, ERR=201) '|        X/Y|'
          WRITE(10, 20, ERR=201) Y_MIN, '|'
          DO J = 1, M - 1, 1
              Y = Y_MIN + J * Y_STEP
              IF (Y .GT. Y_MAX) THEN
                  Y = Y_MAX
              END IF
              IF (IS_INVISIBLE(Y, Y + Y_STEP) .EQ. 1) THEN
                  CYCLE
              END IF
              IF (ABS(Y) .LT. ABS(Y_STEP / 2)) THEN
                  Y = 0.0
              END IF
              WRITE(10, 20, ERR=201) Y, '|'
              REAL_M = REAL_M + 1
          END DO
          IF (IS_INVISIBLE(Y, Y_MAX) .EQ. 0) THEN
              REAL_M = REAL_M + 1
              WRITE(10, 20, ERR=201) Y_MAX, '|'
          END IF
          WRITE(10, 22, ERR=201)
          DO J = 1, REAL_M, 1
              WRITE(10, 21, ERR=201) '------------'
          END DO
          WRITE(10, 21, ERR=201) '-------------'
          WRITE(10, 22, ERR=201)

          DO I = 1, N, 1
              X = X_MIN + (I - 1) * X_STEP
              IF (X .GT. X_MAX) THEN
                  X = X_MAX
              END IF
              IF (IS_INVISIBLE(X, X + X_STEP) .EQ. 0) THEN
                  IF (ABS(X) .LT. ABS(X_STEP / 2)) THEN
                      X = 0.0
                  END IF
                  CALL WRITE_ROW(X, M, REAL_M)
              END IF
          END DO

          CLOSE(10)
          GOTO 209

20        FORMAT(E11.4, A, $)
21        FORMAT(A, $)
22        FORMAT()

200       PAUSE 'Open table file error!'
          STOP

201       PAUSE 'Write table file error!'
          CLOSE(10)
          STOP

209       CONTINUE
      END