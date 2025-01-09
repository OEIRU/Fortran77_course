      PROGRAM MAIN
      INTEGER N_SEGMENTS
      REAL A, B
      EXTERNAL POLY_FUNC, OSC_FUNC
      CHARACTER*20 METHOD_NAME, FUNC_NAME

      ! Ввод значений A, B
      PRINT *, 'Enter A:'
      READ *, A
      PRINT *, 'Enter B:'
      READ *, B

      ! Исследование для метода трапеций
      METHOD_NAME = 'Trapezoid'
      FUNC_NAME = 'Poly k+3'
      N_SEGMENTS = 1  ! Минимальное N для метода трапеций
      CALL CONVERGENCE_TABLE(METHOD_NAME, 2, A, B, N_SEGMENTS, 
     &                       POLY_FUNC, FUNC_NAME)

      FUNC_NAME = 'Oscillating'
      N_SEGMENTS = 1  ! Минимальное N для метода трапеций
      CALL CONVERGENCE_TABLE(METHOD_NAME, 2, A, B, N_SEGMENTS, 
     &                       OSC_FUNC, FUNC_NAME)

      ! Исследование для метода Гаусса-4
      METHOD_NAME = 'Gauss-4'
      FUNC_NAME = 'Poly k+3'
      N_SEGMENTS = 1  ! Минимальное N для метода Гаусса-4
      CALL CONVERGENCE_TABLE(METHOD_NAME, 8, A, B, N_SEGMENTS, 
     &                       POLY_FUNC, FUNC_NAME)

      FUNC_NAME = 'Oscillating'
      N_SEGMENTS = 1  ! Минимальное N для метода Гаусса-4
      CALL CONVERGENCE_TABLE(METHOD_NAME, 8, A, B, N_SEGMENTS, 
     &                       OSC_FUNC, FUNC_NAME)
      END

      ! Подпрограмма для создания сетки
      SUBROUTINE CREATE_GRID(A, B, N, X, METHOD_NAME)
      REAL A, B, X(*)
      INTEGER N
      CHARACTER*(*) METHOD_NAME
      INTEGER I
      REAL H

      IF (METHOD_NAME .EQ. 'Trapezoid') THEN
          H = (B - A) / (N - 1)
          DO I = 1, N
              X(I) = A + (I-1)*H
          END DO
      ELSE IF (METHOD_NAME .EQ. 'Gauss-4') THEN
          H = (B - A) / N
          DO I = 1, N + 1
              X(I) = A + (I-1)*H
          END DO
      END IF
      END

      ! Метод трапеций
      SUBROUTINE TRAPEZOID_RULE(X, N, INTEGRAL, FUNC)
      REAL X(N), INTEGRAL, H
      INTEGER I, N
      REAL FUNC
      EXTERNAL FUNC

      INTEGRAL = 0.0
      DO I = 1, N-1
          H = X(I+1) - X(I)
          INTEGRAL = INTEGRAL + H * (FUNC(X(I)) + FUNC(X(I+1))) / 2.0
      END DO
      END

      ! Метод Гаусса-4
      SUBROUTINE GAUSS4_RULE(X, N, INTEGRAL, FUNC)
      REAL X(N+1), INTEGRAL, H, MID
      REAL W(4), XI(4)
      INTEGER I, J, N
      REAL FUNC
      EXTERNAL FUNC

      ! Веса и узлы для метода Гаусса-4
      DATA W /0.3478548451, 0.6521451549, 0.6521451549, 0.3478548451/
      DATA XI /-0.8611363116, -0.3399810436, 0.3399810436, 0.8611363116/

      INTEGRAL = 0.0
      DO I = 1, N
          MID = (X(I) + X(I+1)) / 2.0
          H = (X(I+1) - X(I)) / 2.0
          DO J = 1, 4
              INTEGRAL = INTEGRAL + W(J) * FUNC(MID + H * XI(J)) * H
          END DO
      END DO
      END

      ! Функция для вычисления аналитического значения интеграла
      REAL FUNCTION ANALYTIC_INTEGRAL(A, B, FUNC_NAME)
      REAL A, B
      CHARACTER*(*) FUNC_NAME

      IF (FUNC_NAME .EQ. 'Poly k+3') THEN
          ANALYTIC_INTEGRAL = (B**5 - A**5) / 5.0  ! Интеграл x^5
      ELSE IF (FUNC_NAME .EQ. 'Oscillating') THEN
          ANALYTIC_INTEGRAL = (-COS(10.0 * B) + COS(10.0 * A)) / 10.0
      ELSE
          ANALYTIC_INTEGRAL = 0.0
      END IF
      END

      ! Подпрограмма для исследования порядка аппроксимации
      SUBROUTINE CONVERGENCE_TABLE(METHOD_NAME, K, A, B, N_SEGMENTS, 
     &  FUNC, FUNC_NAME)
      CHARACTER*(*) METHOD_NAME, FUNC_NAME
      INTEGER K, N_SEGMENTS
      REAL A, B
      REAL FUNC
      EXTERNAL FUNC
      REAL X(1000000), INTEGRAL, ANALYTIC, ERROR, PREV_ERROR
      REAL RUNGE_ERROR, RICHARDSON, RICHARDSON_ERROR
      REAL PREV_INTEGRAL, RATIO_ERROR, EST_RATIO_ERROR
      INTEGER STEP, STOP_COUNTER

      ANALYTIC = ANALYTIC_INTEGRAL(A, B, FUNC_NAME)
      WRITE(*, *) 'Method: ', METHOD_NAME
      WRITE(*, *) 'Order of method: ', K
      WRITE(*, *) 'Function: ', FUNC_NAME
      WRITE(*, *) 'Analytic: ', ANALYTIC
      WRITE(*, *) '-------------------------------------------'

      STOP_COUNTER = 0
      PREV_ERROR = 1.0E30  ! Большое начальное значение для первой итерации
      PREV_INTEGRAL = 0.0

      STEP = 1
      DO WHILE (STOP_COUNTER .LT. 3)  ! Остановка после 3 шагов без улучшения
          ! Вычисление для N отрезков
          CALL CREATE_GRID(A, B, N_SEGMENTS, X, METHOD_NAME)
          IF (METHOD_NAME .EQ. 'Trapezoid') THEN
              CALL TRAPEZOID_RULE(X, N_SEGMENTS + 1, INTEGRAL, FUNC)
          ELSE IF (METHOD_NAME .EQ. 'Gauss-4') THEN
              CALL GAUSS4_RULE(X, N_SEGMENTS, INTEGRAL, FUNC)
          END IF
          ERROR = ABS(ANALYTIC - INTEGRAL)

          ! Отношение погрешностей
          IF (STEP .GT. 1) THEN
              RATIO_ERROR = PREV_ERROR / ERROR
              EST_RATIO_ERROR = 2.0**K  ! Оценка отношения погрешностей
          ELSE
              RATIO_ERROR = 0.0
              EST_RATIO_ERROR = 0.0
          END IF

          ! Оценка по правилу Рунге
          IF (STEP .GT. 1) THEN
              RUNGE_ERROR = (INTEGRAL - PREV_INTEGRAL) / (2**K - 1)
          ELSE
              RUNGE_ERROR = 0.0
          END IF

          ! Уточнение по Ричардсону
          IF (STEP .GT. 1) THEN
              RICHARDSON = INTEGRAL + (INTEGRAL - PREV_INTEGRAL) / 
     &                  (2**K - 1)
              RICHARDSON_ERROR = ABS(ANALYTIC - RICHARDSON)
          ELSE
              RICHARDSON = 0.0
              RICHARDSON_ERROR = 0.0
          END IF

          ! Вывод данных для текущего N
          WRITE(*, 100) 'N: ', N_SEGMENTS
          WRITE(*, 101) 'Numeric: ', INTEGRAL
          WRITE(*, 101) 'Ratio Error: ', RATIO_ERROR
          WRITE(*, 101) 'Estimated Ratio: ', EST_RATIO_ERROR
          WRITE(*, 101) 'Error: ', ERROR
          WRITE(*, 101) 'Runge Error: ', RUNGE_ERROR
          WRITE(*, 101) 'Richardson: ', RICHARDSON
          WRITE(*, 101) 'Richardson Error: ', RICHARDSON_ERROR
          WRITE(*, *) '-------------------------------------------'

100     FORMAT(A, I6)
101     FORMAT(A, F20.15)

          ! Проверка условия остановки
          IF (ERROR .GE. PREV_ERROR) THEN
              STOP_COUNTER = STOP_COUNTER + 1
          ELSE
              STOP_COUNTER = 0
          END IF

          PREV_ERROR = ERROR
          PREV_INTEGRAL = INTEGRAL
          N_SEGMENTS = N_SEGMENTS * 2
          STEP = STEP + 1
      END DO
      END

      ! Полином степени k+3 (для k=2: x^5)
      REAL FUNCTION POLY_FUNC(X)
      REAL X
      POLY_FUNC = X**5 
      END

      ! Осциллирующая функция (sin(10x))
      REAL FUNCTION OSC_FUNC(X)
      REAL X
      OSC_FUNC = SIN(10.0 * X)
      END