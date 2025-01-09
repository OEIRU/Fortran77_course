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

100       PRINT *, 'Error reading params file!'
          STOP
      END

      REAL*8 FUNCTION TO_RAD(ANGLE)
          IMPLICIT NONE
          REAL*8 ANGLE
          TO_RAD = ANGLE / 180.0D0 * 3.14159265358979323846D0
      END

      REAL*8 FUNCTION ARCCOS(SUM)
          IMPLICIT NONE
          REAL*8 SUM
          ARCCOS = ACOS(SUM)
      END

      SUBROUTINE WRITE_ROW(X, Y_VALS, M)
          IMPLICIT NONE
          REAL*8 X, Y_VALS(*), ARCCOS, RES
          INTEGER M, J

          WRITE(10, '(A, E15.8, A, $)') '| ', X, ' '
          DO J = 1, M
              RES = ARCCOS(X + Y_VALS(J))  ! Вычисляем значение и сохраняем в RES
              WRITE(10, '(A, E15.8, A, $)') '| ', RES, ' '
          END DO
          WRITE(10, '(A)') '|'
      END

      SUBROUTINE CREATE_TABLE()
          IMPLICIT NONE
          COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          REAL*8 X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          REAL*8 X, Y, Y_VALS(1000)
          INTEGER N, M, I, J

          ! Вычисление количества шагов
          N = INT((X_MAX - X_MIN) / X_STEP) + 1
          M = INT((Y_MAX - Y_MIN) / Y_STEP) + 1

          ! Заполнение массива Y_VALS
          DO J = 1, M
              Y_VALS(J) = Y_MIN + (J - 1) * Y_STEP
          END DO

          ! Открытие файла для записи таблицы
          OPEN(10, FILE='table.txt', STATUS='REPLACE', ERR=200)

          ! Заголовок таблицы
          WRITE(10, '(A, $)') '|       X/Y       '
          DO J = 1, M
              WRITE(10, '(A, E15.8, A, $)') '| ', Y_VALS(J), ' '
          END DO
          WRITE(10, '(A)') '|'

          ! Разделительная линия
          WRITE(10, '(A, $)') '+-----------------'
          DO J = 1, M
              WRITE(10, '(A, $)') '+-----------------'
          END DO
          WRITE(10, '(A)') '+'

          ! Заполнение таблицы
          DO I = 1, N
              X = X_MIN + (I - 1) * X_STEP
              CALL WRITE_ROW(X, Y_VALS, M)

              ! Разделительная линия между строками
              WRITE(10, '(A, $)') '+-----------------'
              DO J = 1, M
                  WRITE(10, '(A, $)') '+-----------------'
              END DO
              WRITE(10, '(A)') '+'
          END DO

          CLOSE(10)
          RETURN

200       PRINT *, 'Error opening table file!'
          STOP
      END