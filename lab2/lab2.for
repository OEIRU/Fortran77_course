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

      REAL*8 FUNCTION TO_RAD(ANGLE)
      IMPLICIT NONE
      REAL*8 ANGLE
      TO_RAD = ANGLE / 180.0D0 * 3.14159265358979323846D0
      END

      REAL*8 FUNCTION ARCCOS(SUM)
      IMPLICIT NONE
      REAL*8 SUM
      IF (SUM .LT. -1.0D0 .OR. SUM .GT. 1.0D0) THEN
        ARCCOS = -1.0D0  ! Возвращаем специальное значение для обозначения ошибки
      ELSE
        ARCCOS = ACOS(SUM)
      END IF
      END

      SUBROUTINE WRITE_ROW(X, Y_VALS, M)
      IMPLICIT NONE
      REAL*8 X, Y_VALS(*), ARCCOS, RES
      INTEGER M, J
      CHARACTER*10 RES_STR  
      EXTERNAL ARCCOS
      WRITE(10, '(A, F10.4, A, $)') '| ', X, ' '
      DO J = 1, M
        RES = ARCCOS(X + Y_VALS(J))
        IF (RES .EQ. -1.0D0) THEN
            RES_STR = 'N/D     '  ! Записываем "N/D", если значение не определено
        ELSE
            WRITE(RES_STR, '(F10.4)') RES  ! Форматируем результат
        END IF
        WRITE(10, '(A, A, A, $)') '| ', RES_STR, ' '
      END DO
      WRITE(10, '(A)') '|'
      END

      SUBROUTINE CREATE_TABLE()
      IMPLICIT NONE
      COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
      REAL*8 X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
      REAL*8 X, Y_VALS(1000)
      INTEGER N, M, I, J
      LOGICAL HAS_VALID_VALUE

      ! Проверка корректности шагов
      IF (X_STEP .LE. 0.0D0 .OR. Y_STEP .LE. 0.0D0) THEN
        PRINT *, 'Error: Step must be positive!'
        STOP
      END IF

      ! Вычисление количества шагов
      N = INT((X_MAX - X_MIN) / X_STEP) + 1
      IF (X_MIN + (N - 1) * X_STEP .LT. X_MAX) N = N + 1
      M = INT((Y_MAX - Y_MIN) / Y_STEP) + 1
      IF (Y_MIN + (M - 1) * Y_STEP .LT. Y_MAX) M = M + 1

      ! Проверка размера массива
      IF (M .GT. 1000) THEN
        PRINT *, 'Error: Y_VALS array size exceeded!'
        STOP
      END IF

      ! Проверка наличия допустимых значений
      HAS_VALID_VALUE = .FALSE.
      DO I = 1, N
        DO J = 1, M
          IF (ABS(X_MIN + (I - 1) * X_STEP + Y_MIN + (J - 1) * Y_STEP) <= 1.0D0) THEN
            HAS_VALID_VALUE = .TRUE.
            EXIT
          END IF
        END DO
        IF (HAS_VALID_VALUE) EXIT
      END DO
      IF (.NOT. HAS_VALID_VALUE) THEN
        PRINT *, 'Error: No valid values in the specified range!'
        STOP
      END IF

      ! Заполнение массива Y_VALS
      DO J = 1, M
        Y_VALS(J) = Y_MIN + (J - 1) * Y_STEP
      END DO

      ! Открытие файла для записи таблицы
      OPEN(10, FILE='table.txt', STATUS='UNKNOWN', ERR=200)

      ! Заголовок таблицы
      WRITE(10, '(A, $)') '|       X/Y       '
      DO J = 1, M
        WRITE(10, '(A, F10.4, A, $)') '| ', Y_VALS(J), ' '
      END DO
      WRITE(10, '(A)') '|'

      ! Разделительная линия
      WRITE(10, '(A, $)') '+-------------'
      DO J = 1, M
        WRITE(10, '(A, $)') '+-------------'
      END DO
      WRITE(10, '(A)') '+'

      ! Заполнение таблицы
      DO I = 1, N
        X = X_MIN + (I - 1) * X_STEP
        IF (I .EQ. N) X = X_MAX
        CALL WRITE_ROW(X, Y_VALS, M)
        ! Разделительная линия между строками
        WRITE(10, '(A, $)') '+-------------'
        DO J = 1, M
          WRITE(10, '(A, $)') '+-------------'
        END DO
        WRITE(10, '(A)') '+'
      END DO

      CLOSE(10)
      RETURN
200   PRINT *, 'Error opening table file!'
      STOP
      END