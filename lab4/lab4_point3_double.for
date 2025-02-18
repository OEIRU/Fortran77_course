      PROGRAM MAIN
      INTEGER N_SEGMENTS
      REAL*8 A, B
      EXTERNAL POLY_FUNC, OSC_FUNC
      REAL*8 POLY_FUNC, OSC_FUNC  ! Явное объявление типа для функций
      CHARACTER*20 METHOD_NAME, FUNC_NAME

      ! Ввод значений A, B
      PRINT *, 'Enter A:'
      READ *, A
      PRINT *, 'Enter B:'
      READ *, B

      ! Исследование для метода трапеций
      METHOD_NAME = 'Trapezoid'
      FUNC_NAME = 'Poly k+3'
      N_SEGMENTS = 1  ! Начальное значение N для метода трапеций
      CALL CONVERGENCE_TABLE(METHOD_NAME, 2, A, B, N_SEGMENTS, 
     &                      POLY_FUNC, FUNC_NAME)

      FUNC_NAME = 'Oscillating'
      N_SEGMENTS = 1  ! Начальное значение N для метода трапеций
      CALL CONVERGENCE_TABLE(METHOD_NAME, 2, A, B, N_SEGMENTS, 
     &                     OSC_FUNC, FUNC_NAME)

      ! Исследование для метода Гаусса-4
      METHOD_NAME = 'Gauss-4'
      FUNC_NAME = 'Poly k+3'
      N_SEGMENTS = 1  ! Начальное значение N для метода Гаусса-4
      CALL CONVERGENCE_TABLE(METHOD_NAME, 8, A, B, N_SEGMENTS, 
     &                      POLY_FUNC, FUNC_NAME)

      FUNC_NAME = 'Oscillating'
      N_SEGMENTS = 4  ! Начальное значение N для метода Гаусса-4
      CALL CONVERGENCE_TABLE(METHOD_NAME, 8, A, B, N_SEGMENTS, 
     &                      OSC_FUNC, FUNC_NAME)
      END

      ! Метод трапеций (исправленная версия)
      SUBROUTINE TRAPEZOID_RULE(A, B, N, INTEGRAL, FUNC)
      REAL*8 A, B, INTEGRAL, H, X
      INTEGER I, N
      REAL*8 FUNC
      EXTERNAL FUNC

      INTEGRAL = 0.0D0
      H = (B - A) / N
      DO I = 0, N-1
         X = A + I * H
         INTEGRAL = INTEGRAL + (FUNC(X) + FUNC(X + H)) * H / 2.0D0
      END DO
      END

      ! Метод Гаусса-4 (исправленная версия)
      SUBROUTINE GAUSS4_RULE(A, B, N, INTEGRAL, FUNC)
      REAL*8 A, B, INTEGRAL, H, A_I, B_I, MID, T_J
      REAL*8 W(4), XI(4)
      INTEGER I, J, N
      REAL*8 FUNC
      EXTERNAL FUNC

      DATA W /0.3478548451D0, 0.6521451549D0, 
     &       0.6521451549D0, 0.3478548451D0/
      DATA XI /-0.8611363116D0, -0.3399810436D0, 
     &       0.3399810436D0, 0.8611363116D0/

      INTEGRAL = 0.0D0
      H = (B - A) / N
      DO I = 0, N-1
         A_I = A + I * H
         B_I = A_I + H
         MID = (A_I + B_I) / 2.0D0
         DO J = 1, 4
            T_J = MID + (H / 2.0D0) * XI(J)
            INTEGRAL = INTEGRAL + W(J) * FUNC(T_J) * (H / 2.0D0)
         END DO
      END DO
      END

      ! Функция для вычисления аналитического значения интеграла
      REAL*8 FUNCTION ANALYTIC_INTEGRAL(A, B, FUNC_NAME)
      REAL*8 A, B
      CHARACTER*(*) FUNC_NAME
      INTEGER IS_POLY, IS_OSC

      IS_POLY = 0
      IS_OSC = 0
      IF (FUNC_NAME .EQ. 'Poly k+3') IS_POLY = 1
      IF (FUNC_NAME .EQ. 'Oscillating') IS_OSC = 1

      IF (IS_POLY .EQ. 0 .AND. IS_OSC .EQ. 0) THEN
         PRINT *, 'Error: Unknown function name.'
         ANALYTIC_INTEGRAL = 0.0D0
         RETURN
      END IF

      ANALYTIC_INTEGRAL = IS_POLY * (B**6 - A**6) / 6.0D0
     &                 + IS_OSC * (-COS(10.0D0 * B) + COS(10.0D0 * A)) 
     &                 / 10.0D0
      END

      ! Подпрограмма для исследования порядка аппроксимации
      SUBROUTINE CONVERGENCE_TABLE(METHOD_NAME, K, A, B, N_SEGMENTS, 
     &                            FUNC, FUNC_NAME)
      CHARACTER*(*) METHOD_NAME, FUNC_NAME
      INTEGER K, N_SEGMENTS
      REAL*8 A, B
      REAL*8 FUNC  ! Явное объявление типа для FUNC
      EXTERNAL FUNC
      REAL*8 INTEGRAL, ANALYTIC, ERROR, PREV_ERROR, MIN_ERROR
      REAL*8 RUNGE_ERROR, RICHARDSON, RICHARDSON_ERROR
      REAL*8 PREV_INTEGRAL, RATIO_ERROR, EST_RATIO_ERROR
      INTEGER STEP, ITER_AFTER_MIN
      INTEGER IS_TRAPEZOID, IS_GAUSS
      LOGICAL FLAG_MIN
      REAL*8 ANALYTIC_INTEGRAL  ! Явное объявление функции как REAL*8
      INTEGER MAX_ITER
      PARAMETER (MAX_ITER = 100)

      ! Определяем, какой метод используется
      IS_TRAPEZOID = 0
      IS_GAUSS = 0
      IF (METHOD_NAME .EQ. 'Trapezoid') IS_TRAPEZOID = 1
      IF (METHOD_NAME .EQ. 'Gauss-4') IS_GAUSS = 1

      ANALYTIC = ANALYTIC_INTEGRAL(A, B, FUNC_NAME)
      WRITE(*, *) 'Method: ', METHOD_NAME
      WRITE(*, *) 'Order of method: ', K
      WRITE(*, *) 'Function: ', FUNC_NAME
      WRITE(*, *) 'Analytic: ', ANALYTIC
      WRITE(*, *) '-------------------------------------------'

      PREV_ERROR = 1.0D30  ! Большое начальное значение для первой итерации
      PREV_INTEGRAL = 0.0D0
      FLAG_MIN = .FALSE.   ! Флаг для минимальной ошибки
      ITER_AFTER_MIN = 0   ! Счетчик итераций после достижения минимальной ошибки
      MIN_ERROR = 1.0D30   ! Минимальная ошибка
      STEP = 1

      DO WHILE (N_SEGMENTS .LE. 1E9 .AND. STEP .LE. MAX_ITER)
         ! Вычисление для N отрезков
         IF (IS_TRAPEZOID .EQ. 1) THEN
            CALL TRAPEZOID_RULE(A, B, N_SEGMENTS, INTEGRAL, FUNC)
         ELSE IF (IS_GAUSS .EQ. 1) THEN
            CALL GAUSS4_RULE(A, B, N_SEGMENTS, INTEGRAL, FUNC)
         END IF

         ERROR = ABS(ANALYTIC - INTEGRAL)
         RATIO_ERROR = 0.0D0
         EST_RATIO_ERROR = 0.0D0
         IF (STEP .GT. 1 .AND. ERROR .GT. 1.0D-15) THEN
            RATIO_ERROR = PREV_ERROR / ERROR
            EST_RATIO_ERROR = 2.0D0**K
         END IF

         RUNGE_ERROR = 0.0D0
         IF (STEP .GT. 1) THEN
            RUNGE_ERROR = (INTEGRAL - PREV_INTEGRAL) / (2**K - 1)
         END IF

         RICHARDSON = 0.0D0
         RICHARDSON_ERROR = 0.0D0
         IF (STEP .GT. 1) THEN
            RICHARDSON = INTEGRAL + (INTEGRAL - PREV_INTEGRAL) / 
     &                  (2**K - 1)
            RICHARDSON_ERROR = ABS(ANALYTIC - RICHARDSON)
         END IF

         ! Вывод данных для текущего N
         WRITE(*, 100) 'N: ', N_SEGMENTS
         WRITE(*, 101) 'Error: ', ERROR
         WRITE(*, 101) 'Ratio Error: ', RATIO_ERROR
         WRITE(*, 101) 'Richardson Error: ', RICHARDSON_ERROR

100      FORMAT(A, I6)
101      FORMAT(A, E20.12)

         ! Проверка на минимальную ошибку
         IF (.NOT. FLAG_MIN .AND. ERROR .LT. MIN_ERROR) THEN
            MIN_ERROR = ERROR
         END IF

         IF (.NOT. FLAG_MIN .AND. ERROR .GT. MIN_ERROR) THEN
            FLAG_MIN = .TRUE.
         END IF

         IF (FLAG_MIN) THEN
            ITER_AFTER_MIN = ITER_AFTER_MIN + 1
         END IF

         IF (ITER_AFTER_MIN .GE. 3) THEN
            EXIT
         END IF

         ! Обновление предыдущих значений
         PREV_ERROR = ERROR
         PREV_INTEGRAL = INTEGRAL
         N_SEGMENTS = N_SEGMENTS * 2
         STEP = STEP + 1
      END DO
      END

      ! Полином степени k+3 (для k=2: x^5)
      REAL*8 FUNCTION POLY_FUNC(X)
      REAL*8 X
      POLY_FUNC = X**5
      END

      ! Осциллирующая функция (sin(10x))
      REAL*8 FUNCTION OSC_FUNC(X)
      REAL*8 X
      OSC_FUNC = SIN(10.0D0 * X)
      END