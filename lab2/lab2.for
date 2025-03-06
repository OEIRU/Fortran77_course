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

      IF (ABS(X_STEP/X_MIN) .LT. 0.00009 .OR.
     & ABS(Y_STEP/Y_MIN) .LT. 0.00009 ) THEN
        PRINT *, 'Error: Small step!'
        STOP
      END IF

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

      SUBROUTINE WRITE_ROW(CUR_X, Y_VALS, M, PREV_ARCCOS)
      IMPLICIT NONE
      REAL*8 CUR_X, Y_VALS(*), ARCCOS, RES, PREV_ARCCOS(*)
      INTEGER M, J
      CHARACTER*11 RES_STR
      EXTERNAL ARCCOS

      ! Запись заголовка строки с текущим X
      CALL FORMAT_NUMBER(CUR_X, RES_STR)
      WRITE(10, '(A, A, A, $)') '| ', RES_STR, ' '

      DO 10 J = 1, M
          RES = ARCCOS(CUR_X + Y_VALS(J))
          IF (RES .EQ. -1.0D0) THEN
              RES_STR = 'N/D       '
          ELSE
              ! Проверка на дублирование значений
              IF (PREV_ARCCOS(J) .NE. -1.0D0 .AND.
     &            ABS(RES - PREV_ARCCOS(J)) .LT. 1.0D-6) THEN
                  PRINT *, 'Error: Duplicate ARCCOS value at X=', 
     & CUR_X, ' Y=', Y_VALS(J)  
                  STOP
              END IF
              PREV_ARCCOS(J) = RES
              CALL FORMAT_NUMBER(RES, RES_STR)
          END IF
          WRITE(10, '(A, A, A, $)') '| ', RES_STR, ' '
10    CONTINUE
      WRITE(10, '(A)') '|'
      END

      SUBROUTINE FORMAT_NUMBER(NUM, STR)
      IMPLICIT NONE
      REAL*8 NUM
      CHARACTER*11 STR
      CHARACTER*11 TEMP_STR

      ! Записываем число в экспоненциальном формате E10.4
      WRITE(TEMP_STR, '(E10.4)') NUM

      ! Проверяем, есть ли ведущий ноль перед десятичной точкой
      IF (TEMP_STR(1:1) .EQ. '-' .AND. TEMP_STR(2:2) .EQ. '.') THEN
          ! Если число отрицательное и нет ведущего нуля, добавляем его
          STR = '-0' // TEMP_STR(2:10)
      ELSE IF (TEMP_STR(1:1) .EQ. ' ' .AND. TEMP_STR(2:2) .EQ. '.') THEN
          ! Если число положительное и нет ведущего нуля, добавляем его
          STR = ' 0' // TEMP_STR(2:10)
      ELSE
          ! В остальных случаях оставляем строку без изменений
          STR = TEMP_STR
      END IF
      END

      SUBROUTINE CREATE_TABLE()
      IMPLICIT NONE
      COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
      REAL*8 X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
      REAL*8 CUR_X, Y_VALS(2000), PREV_ARCCOS(2000)
      INTEGER N, M, I, J
      LOGICAL HAS_VALID_VALUE
      CHARACTER*11 RES_STR

      ! Проверка шага
      IF (X_STEP .LE. 0.0D0 .OR. Y_STEP .LE. 0.0D0) THEN
        PRINT *, 'Error: Step must be positive!'
        STOP
      END IF

      ! Вычисление размеров таблицы
      N = INT((X_MAX - X_MIN) / X_STEP) + 1
      IF (X_MIN + (N - 1) * X_STEP .LT. X_MAX) N = N + 1
      M = INT((Y_MAX - Y_MIN) / Y_STEP) + 1
      IF (Y_MIN + (M - 1) * Y_STEP .LT. Y_MAX) M = M + 1

      IF (M .GT. 2000) THEN
        PRINT *, 'Error: Y_VALS array size exceeded!'
        STOP
      END IF

      ! Проверка наличия допустимых значений
      HAS_VALID_VALUE = .FALSE.
      DO 20 I = 1, N
          DO 20 J = 1, M
              IF (ABS(X_MIN + (I - 1) * X_STEP + Y_MIN +
     & (J - 1) * Y_STEP) .LE. 1.0D0) THEN
                  HAS_VALID_VALUE = .TRUE.
                  GOTO 20
              END IF
20    CONTINUE
      IF (.NOT. HAS_VALID_VALUE) THEN
        PRINT *, 'Error: No valid values in the specified range!'
        STOP
      END IF

      ! Заполнение массива Y_VALS и инициализация PREV_ARCCOS
      DO 50 J = 1, M
          Y_VALS(J) = Y_MIN + (J - 1) * Y_STEP
          PREV_ARCCOS(J) = -1.0D0  ! Инициализация массива предыдущих значений
50    CONTINUE

      ! Открытие файла для записи таблицы
      OPEN(10, FILE='table.txt', STATUS='UNKNOWN', ERR=200)

      ! Заголовок таблицы
      WRITE(10, '(A, $)') '|      X/Y    '
      DO 60 J = 1, M
          CALL FORMAT_NUMBER(Y_VALS(J), RES_STR)
          WRITE(10, '(A, A, A, $)') '| ', RES_STR, ' '
60    CONTINUE
      WRITE(10, '(A)') '|'

      ! Линия разделителя
      WRITE(10, '(A, $)') '+-------------'
      DO 70 J = 1, M
          WRITE(10, '(A, $)') '+-------------'
70    CONTINUE
      WRITE(10, '(A)') '+'

      ! Запись строк таблицы
      DO 80 I = 1, N
          CUR_X = X_MIN + (I - 1) * X_STEP
          IF (I .EQ. N) CUR_X = X_MAX
          CALL WRITE_ROW(CUR_X, Y_VALS, M, PREV_ARCCOS)

          WRITE(10, '(A, $)') '+-------------'
          DO 90 J = 1, M
              WRITE(10, '(A, $)') '+-------------'
90        CONTINUE
          WRITE(10, '(A)') '+'
80    CONTINUE

      CLOSE(10)
      RETURN
200   PRINT *, 'Error opening table file!'
      STOP
      END
